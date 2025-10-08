;;; context-navigator-core-push-toggle-test.el --- Tests for push-to-gptel gating -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-persist)

(defun ctxnav-test--install-root (root)
  "Set core state last-project-root to ROOT."
  (let* ((st (context-navigator--state-get))
         (new (context-navigator--state-copy st)))
    (setf (context-navigator-state-last-project-root new) root)
    (context-navigator--set-state new)))

(provide 'context-navigator-core-push-toggle-test)
;;; context-navigator-core-push-toggle-test.el ends here
