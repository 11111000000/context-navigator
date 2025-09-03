;;; context-navigator-transient-tramp-test.el --- Tests for TRAMP warning (i18n) in universal add -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-core)
(require 'context-navigator-transient)
(require 'context-navigator-i18n)

(defun ctxnav-tramp-test--reset-state ()
  (let ((st (context-navigator--state-make)))
    (funcall #'context-navigator--set-state st)
    st))

(ert-deftest ctxnav-transient/remote-current-file-prompt-i18n ()
  "When current buffer is remote, universal add must prompt with i18n string and abort if answered NO."
  (let ((context-navigator-language 'en)) ;; ensure deterministic prompt
    (let ((buf (get-buffer-create "ctxnav-tramp-test")))
      (unwind-protect
          (with-current-buffer buf
            (setq buffer-file-name "/ssh:host:/tmp/zzz.txt")
            (ctxnav-tramp-test--reset-state)
            (let ((prompt ""))
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (p) (setq prompt p) nil))) ;; decline
                (context-navigator-add-universal)
                ;; Prompt must be localized
                (should (string= prompt (context-navigator-i18n :warn-remote-current)))
                ;; No items added
                (let* ((st (context-navigator--state-get)))
                  (should (= (length (context-navigator-state-items st)) 0))))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(provide 'context-navigator-transient-tramp-test)
;;; context-navigator-transient-tramp-test.el ends here
