;;; context-navigator-headerline-test.el --- Tests for headerline integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-headerline)

(ert-deftest ctxnav-headerline/format-uses-controls-segments ()
  "headerline-format should concat controls segments returned by controls module."
  (with-temp-buffer
    (delay-mode-hooks (context-navigator-view-mode))
    ;; Stub controls to return two segments " A" " B"
    (cl-letf (((symbol-function 'context-navigator-view-controls-segments)
               (lambda () '(" A" " B"))))
      (let ((out (context-navigator-headerline-format)))
        (should (string= out " A B"))))))

(ert-deftest ctxnav-headerline/apply-installs-eval-format ()
  "headerline--apply should install (:eval ... ) format when enabled."
  (with-temp-buffer
    (delay-mode-hooks (context-navigator-view-mode))
    (let ((context-navigator-view-headerline-enable t))
      (context-navigator-headerline--apply (current-buffer))
      (should (equal header-line-format '((:eval (context-navigator-headerline-format))))))))

(provide 'context-navigator-headerline-test)
;;; context-navigator-headerline-test.el ends here
