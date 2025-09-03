;;; run-tests.el --- Simple test runner for context-navigator -*- lexical-binding: t; -*-

;; Load path
(add-to-list 'load-path (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Load modules that tests may need
(require 'context-navigator)
(require 'context-navigator-core)
(require 'context-navigator-events)
(require 'context-navigator-fp)
(require 'context-navigator-model)
(require 'context-navigator-persist)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-project)
(require 'context-navigator-render)
(require 'context-navigator-icons)
(require 'context-navigator-sidebar)

;; Load tests
(require 'context-navigator-test-helpers)
(require 'context-navigator-events-test)
(require 'context-navigator-fp-test)
(require 'context-navigator-model-test)
(require 'context-navigator-persist-test)
(require 'context-navigator-gptel-bridge-test)
(require 'context-navigator-core-test)
(require 'context-navigator-core-api-test)
(require 'context-navigator-core-push-toggle-test)
(require 'context-navigator-core-project-toggle-test)
(require 'context-navigator-groups-create-switch-test)

(defun context-navigator-run-tests ()
  "Run all ERT tests for context-navigator."
  (interactive)
  (ert t))

(provide 'run-tests)
;;; run-tests.el ends here
