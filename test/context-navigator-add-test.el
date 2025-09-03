;;; context-navigator-add-test.el --- Tests for universal add (Dired, recursion, limits) -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dired)
(require 'context-navigator-test-helpers)
(require 'context-navigator-core)
(require 'context-navigator-transient)
(require 'context-navigator-model)

(defun ctxnav-add-test--reset-state (&optional items)
  (let ((st (context-navigator--state-make)))
    (when items
      (setq st (context-navigator--state-with-items st items)))
    (funcall #'context-navigator--set-state st)
    st))

(defun ctxnav-add-test--file (dir name size)
  "Create NAME in DIR with SIZE bytes, return absolute path."
  (let ((p (expand-file-name name dir)))
    (with-temp-file p
      (insert (make-string size ?x)))
    p))

(ert-deftest ctxnav-add/dired-recursive-with-size-limit-and-preview ()
  "Dired selection including directories should recurse, filter by size, and confirm."
  (context-navigator-test-with-temp-dir root
    (let* ((sub (expand-file-name "sub" root)))
      (make-directory sub t)
      (let* ((small1 (ctxnav-add-test--file root "a.txt" 5))
             (small2 (ctxnav-add-test--file sub  "b.txt" 7))
             (big1   (ctxnav-add-test--file root "big.bin" 4096))
             ;; Set a low limit to filter out big1
             (context-navigator-max-file-size 16))
        (ctxnav-add-test--reset-state)
        ;; Open Dired and mark root and subdir
        (let ((buf (dired root)))
          (with-current-buffer buf
            ;; Mark the directory 'sub' and small1; big1 will be skipped by size
            (dired-unmark-all-marks)
            (dired-goto-file small1)
            (dired-mark 1)
            (dired-goto-file sub)
            (dired-mark 1)
            ;; Stub yes-or-no-p to auto-approve, avoid UI windows during preview
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                      ((symbol-function 'display-buffer) (lambda (&rest _) nil)))
              (context-navigator-add-universal))))
        ;; Verify only small files were added (a.txt and sub/b.txt)
        (let* ((st (context-navigator--state-get))
               (its (context-navigator-state-items st))
               (names (sort (mapcar #'context-navigator-item-name its) #'string<)))
          (should (= (length its) 2))
          (should (equal names '("a.txt" "b.txt"))))))))

(provide 'context-navigator-add-test)
;;; context-navigator-add-test.el ends here
