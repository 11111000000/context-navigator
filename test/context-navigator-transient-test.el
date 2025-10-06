;;; context-navigator-transient-test.el --- Tests for transient and universal add -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dired)
(require 'context-navigator-test-helpers)
(require 'context-navigator-core)
(require 'context-navigator-transient)
(require 'context-navigator-model)

(defun ctxnav-test--reset-state (&optional items)
  (let ((st (context-navigator--state-make)))
    (when items
      (setq st (context-navigator--state-with-items st items)))
    (funcall #'context-navigator--set-state st)
    st))

(ert-deftest ctxnav-transient/add-universal-region-and-file ()
  "Universal add should add selection when region is active, and file when no region."
  (context-navigator-test-with-temp-dir root
                                        (ctxnav-test--reset-state)
                                        ;; Create a temp file and visit it
                                        (let* ((f (expand-file-name "x.txt" root)))
                                          (with-temp-file f (insert "hello world"))
                                          (find-file f)
                                          (unwind-protect
                                              (progn
                                                ;; Region add
                                                (goto-char (point-min))
                                                (set-mark (point))
                                                (forward-word 1) ;; select "hello"
                                                (context-navigator-add-universal)
                                                (let* ((st (context-navigator--state-get))
                                                       (its (context-navigator-state-items st)))
                                                  (should (= (length its) 1))
                                                  (should (eq (context-navigator-item-type (car its)) 'selection))
                                                  (should (context-navigator-item-enabled (car its))))
                                                ;; Clear region and add file
                                                (deactivate-mark)
                                                ;; Reset state for the second step
                                                (ctxnav-test--reset-state)
                                                (context-navigator-add-universal)
                                                (let* ((st2 (context-navigator--state-get))
                                                       (its2 (context-navigator-state-items st2)))
                                                  (should (= (length its2) 1))
                                                  (should (eq (context-navigator-item-type (car its2)) 'file))
                                                  (should (context-navigator-item-enabled (car its2)))))
                                            (when (get-buffer (file-name-nondirectory f))
                                              (kill-buffer (file-name-nondirectory f)))))))

(provide 'context-navigator-transient-test)
;;; context-navigator-transient-test.el ends here
