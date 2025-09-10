;;; context-navigator-gptel-apply-fallback-test.el --- Tests for apply reset fallback -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-model)

(ert-deftest context-navigator-gptel/apply-reset-when-pull-empty-raw-nonempty ()
  "If pull returns empty but raw gptel context is non-empty, apply should fallback to reset."
  (context-navigator-test-with-mocked-gptel
   ;; Seed raw context (seen by gptel-context-list) with a file
   (setq gptel--context (list (list :type 'file :path "/tmp/existing")))
   ;; Simulate converter failure: pull returns empty list, forcing reset path
   (cl-letf (((symbol-function 'context-navigator-gptel-pull)
              (lambda () '())))
     (let ((res (context-navigator-gptel-apply '())))
       (should (plist-get res :applied))
       (should (eq (plist-get res :method) 'reset))))))

(provide 'context-navigator-gptel-apply-fallback-test)
;;; context-navigator-gptel-apply-fallback-test.el ends here
