;;; context-navigator-groups-test.el --- ERT tests for groups  -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)

(require 'context-navigator-core)
(require 'context-navigator-persist)

(defun ctxnav-test--tmpdir ()
  "Create and return a fresh temporary directory path (absolute)."
  (directory-file-name (make-temp-file "ctxnav-test-" t)))

(defun ctxnav-test--with-tmp-root (fn)
  "Create temp root and call FN with it. Delete directory afterwards."
  (let* ((root (ctxnav-test--tmpdir)))
    (unwind-protect
        (funcall fn root)
      (ignore-errors (delete-directory root t)))))

(ert-deftest ctxnav-persist-list-groups-filters-nonregular ()
  "list-groups returns only regular .el files, ignoring state.el/context.el and non-regulars."
  (ctxnav-test--with-tmp-root
   (lambda (root)
     (let* ((ctxdir (expand-file-name ".context" root)))
       (make-directory ctxdir t)
       ;; Create files
       (with-temp-file (expand-file-name "state.el" ctxdir) (insert "(:version 1 :current \"default\")\n"))
       (with-temp-file (expand-file-name "context.el" ctxdir) (insert "()\n")) ;; legacy, must be ignored
       (with-temp-file (expand-file-name "default.el" ctxdir) (insert "(:version 3 :items ())\n"))
       ;; Non-regular file: symlink (if supported)
       (let ((dst (expand-file-name "link.el" ctxdir)))
         (ignore-errors (make-symbolic-link "default.el" dst t)))
       (let* ((groups (context-navigator-persist-list-groups root))
              (slugs  (mapcar (lambda (pl) (plist-get pl :slug)) groups)))
         (should (listp groups))
         ;; Only "default" must be included
         (should (= (length groups) 1))
         (should (member "default" slugs)))))))

(ert-deftest ctxnav-core-context-save-guard-when-no-slug ()
  "Manual save must not write legacy context.el when no active group."
  (ctxnav-test--with-tmp-root
   (lambda (root)
     ;; Install state with nil slug and set root
     (let* ((st (context-navigator--state-get))
            (new (context-navigator--state-copy st)))
       (setf (context-navigator-state-last-project-root new) root)
       (setf (context-navigator-state-current-group-slug new) nil)
       (setf (context-navigator-state-items new) '())
       (context-navigator--set-state new))
     (let* ((ctxdir (expand-file-name ".context" root))
            (legacy (expand-file-name "context.el" ctxdir)))
       (when (file-exists-p legacy) (delete-file legacy))
       (context-navigator-context-save)
       (should-not (file-exists-p legacy))))))

(ert-deftest ctxnav-core-project-switch-does-not-clear-when-autoload-nil ()
  "When autoload is nil, project switch must not clear model/gptel."
  (ctxnav-test--with-tmp-root
   (lambda (root1)
     (ctxnav-test--with-tmp-root
      (lambda (root2)
        (let ((context-navigator-autoload nil))
          ;; seed some items
          (let* ((it (context-navigator-item-create :type 'file :name "f" :path (expand-file-name "f" root1) :enabled t)))
            (context-navigator-set-items (list it)))
          ;; switch projects
          (funcall #'context-navigator--on-project-switch root2)
          ;; items should remain (no clearing)
          (let* ((st (context-navigator--state-get))
                 (items (context-navigator-state-items st)))
            (should (= (length items) 1))
            (should (equal (context-navigator-item-name (car items)) "f")))))))))

(ert-deftest ctxnav-core-groups-crud ()
  "Create, rename, duplicate and delete groups affect filesystem, :current only when needed."
  (ctxnav-test--with-tmp-root
   (lambda (root)
     ;; point core to our root
     (let* ((st (context-navigator--state-get))
            (new (context-navigator--state-copy st)))
       (setf (context-navigator-state-last-project-root new) root)
       (context-navigator--set-state new))
     ;; Create
     (let* ((slug (context-navigator-group-create "foo"))
            (f (context-navigator-persist-context-file root slug)))
       (should (file-exists-p f))
       ;; Rename foo -> bar
       (let* ((new-slug (context-navigator-group-rename slug "bar"))
              (f-old (context-navigator-persist-context-file root slug))
              (f-new (context-navigator-persist-context-file root new-slug)))
         (should (not (file-exists-p f-old)))
         (should (file-exists-p f-new))
         ;; Duplicate bar -> baz
         (let* ((dup (context-navigator-group-duplicate new-slug "baz"))
                (f-dup (context-navigator-persist-context-file root dup)))
           (should (file-exists-p f-dup))
           ;; Delete baz
           (should (context-navigator-group-delete dup))
           (should (not (file-exists-p f-dup))))
         ;; Delete bar
         (should (context-navigator-group-delete new-slug))
         (should (not (file-exists-p f-new))))))))

(provide 'context-navigator-groups-test)
;;; context-navigator-groups-test.el ends here
