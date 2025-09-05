;;; context-navigator-add-from-minibuffer-mask-test.el --- ERT tests for add-from-minibuffer (mask mode) -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(require 'context-navigator-core)
(require 'context-navigator-path-add)

(defun cn-mb--with-temp-dir (fn)
  "Create a temporary directory, call FN with it, then delete recursively."
  (let* ((dir (make-temp-file "cn-mb-" t)))
    (unwind-protect
        (funcall fn dir)
      (ignore-errors (delete-directory dir t)))))

(defun cn-mb--touch (file &optional content)
  "Create FILE and optionally write CONTENT."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (when content (insert content)))
  file)

(defun cn-mb--reset-state ()
  (let ((st (context-navigator--state-make)))
    (funcall #'context-navigator--set-state st)
    st))

(ert-deftest cn-mb/mixed-input-rejected ()
  "Mixed input (names + mask) should be rejected without adding items."
  (cn-mb--reset-state)
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (&rest _args) "foo.txt *.el"))
            ((symbol-function 'message)
             (lambda (&rest _args) nil))) ;; silence echo area
    (context-navigator-add-from-minibuffer)
    (let* ((st (context-navigator--state-get)))
      (should (= (length (context-navigator-state-items st)) 0)))))

(ert-deftest cn-mb/one-mask-basic-adds ()
  "Single mask adds matching files; when >limit, preview asks for confirmation."
  (cn-mb--with-temp-dir
   (lambda (dir)
     (let* ((default-directory (file-name-as-directory dir))
            (context-navigator-path-add-limit 2))
       (cn-mb--reset-state)
       ;; Prepare 3 files to exceed the limit
       (cn-mb--touch (expand-file-name "a.el" dir) "(a)")
       (cn-mb--touch (expand-file-name "b.el" dir) "(b)")
       (cn-mb--touch (expand-file-name "c.el" dir) "(c)")
       ;; Inject a minimal project-root detector so base='project' maps to dir
       (cl-letf (((symbol-function 'context-navigator--state-get)
                  (lambda ()
                    (let ((st (and (boundp 'context-navigator--state) context-navigator--state)))
                      (unless (and st (context-navigator-state-p st))
                        (setq st (context-navigator--state-make))
                        (funcall #'context-navigator--set-state st))
                      (setf (context-navigator-state-last-project-root st) dir)
                      st)))
                 ((symbol-function 'read-from-minibuffer)
                  (lambda (&rest _args) "*.el"))
                 ;; Auto-confirm preview, don't pop windows during tests
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'display-buffer) (lambda (&rest _) nil)))
         (context-navigator-add-from-minibuffer)
         (let* ((st (context-navigator--state-get))
                (its (and st (context-navigator-state-items st)))
                (names (sort (mapcar #'context-navigator-item-name its) #'string-lessp)))
           (should (= (length its) 3))
           (should (equal names '("a.el" "b.el" "c.el")))))))))

(ert-deftest cn-mb/tramp-unsupported-mask ()
  "When pattern scan-root is remote (TRAMP), mask expansion is denied by default."
  (cn-mb--reset-state)
  (let ((context-navigator-mask-enable-remote nil))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _args) "/ssh:host:/tmp/**/*.el"))
              ;; Don't rely on actual TRAMP; :remote is detected by file-remote-p on the path
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (context-navigator-add-from-minibuffer)
      (let* ((st (context-navigator--state-get)))
        (should (= (length (context-navigator-state-items st)) 0))))))

(ert-deftest cn-mb/dotfiles-default-hidden ()
  "Dotfiles should be hidden unless mask explicitly starts component with a dot."
  (cn-mb--with-temp-dir
   (lambda (dir)
     (let ((default-directory (file-name-as-directory dir)))
       (cn-mb--reset-state)
       (cn-mb--touch (expand-file-name "a.el" dir) "(ok)")
       (cn-mb--touch (expand-file-name ".hidden.el" dir) "(ok)")
       ;; Without explicit dot in the component
       (cl-letf (((symbol-function 'context-navigator--state-get)
                  (lambda ()
                    (let ((st (and (boundp 'context-navigator--state) context-navigator--state)))
                      (unless (and st (context-navigator-state-p st))
                        (setq st (context-navigator--state-make))
                        (funcall #'context-navigator--set-state st))
                      (setf (context-navigator-state-last-project-root st) dir)
                      st)))
                 ((symbol-function 'read-from-minibuffer)
                  (lambda (&rest _args) "*.el"))
                 ;; No preview (limit high), no windows
                 ((symbol-function 'display-buffer) (lambda (&rest _) nil)))
         (let ((context-navigator-path-add-limit 50))
           (context-navigator-add-from-minibuffer)
           (let* ((st (context-navigator--state-get))
                  (its (context-navigator-state-items st))
                  (names (mapcar #'context-navigator-item-name its)))
             (should (= (length its) 1))
             (should (member "a.el" names))
             (should-not (member ".hidden.el" names)))))))))

(provide 'context-navigator-add-from-minibuffer-mask-test)
;;; context-navigator-add-from-minibuffer-mask-test.el ends here
