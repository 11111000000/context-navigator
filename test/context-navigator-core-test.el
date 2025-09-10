;;; context-navigator-core-test.el --- Tests for core sync/merge/gating -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-events)

(defun context-navigator-core-test--reset-state (&optional items)
  "Reset global state to a fresh one, optionally with ITEMS installed."
  (let* ((st (context-navigator--state-make)))
    (when items
      (setq st (context-navigator--state-with-items st items)))
    (funcall #'context-navigator--set-state st)
    st))

(provide 'context-navigator-core-test)
;;; context-navigator-core-test.el ends here
