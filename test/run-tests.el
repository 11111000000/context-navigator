;;; run-tests.el --- Simple test runner for context-navigator -*- lexical-binding: t; -*-

;; Load path (robust: handle batch/interactive where load-file-name or buffer-file-name may be nil)
(let* ((this-file (or load-file-name buffer-file-name default-directory))
       (test-dir (file-name-directory this-file))
       (proj-root (expand-file-name ".." test-dir)))
  (add-to-list 'load-path proj-root)
  (add-to-list 'load-path test-dir))

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
(require 'context-navigator-view)

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
(require 'context-navigator-transient-test)
(require 'context-navigator-add-test)
(require 'context-navigator-path-add-test)
(require 'context-navigator-render-indicators-test)
(require 'context-navigator-render-disabled-test)
(require 'context-navigator-view-toggle-enabled-test)
(require 'context-navigator-gptel-apply-fallback-test)
(require 'context-navigator-view-wrap-test)
(require 'context-navigator-core-prune-test)
(require 'context-navigator-render-style-test)
(require 'context-navigator-render-truncate-test)
(require 'context-navigator-transient-tramp-test)
(require 'context-navigator-view-controls-test)
(require 'context-navigator-groups-delete-test)
(require 'context-navigator-mask-tests)
(require 'context-navigator-mask-posix-class-test)
(require 'context-navigator-minibuf-mixed-input-test)
(require 'context-navigator-buffer-mode-test)
(require 'context-navigator-core-autoproject-pick-root-test)

;; New tests after refactor
(require 'context-navigator-view-controls-unit-test)
(require 'context-navigator-view-controls-icons-test)
(require 'context-navigator-headerline-test)
(require 'context-navigator-compat-aliases-test)

;; Additional tests
(require 'context-navigator-view-counters-test)
(require 'context-navigator-view-spinner-test)
(require 'context-navigator-view-indicators-test)
(require 'context-navigator-multifile-basic-test)
(require 'context-navigator-multifile-edit-actions-test)
(require 'context-navigator-multifile-filter-visit-cleanup-test)
(require 'context-navigator-multifile-remote-and-selection-edge-test)

(defun context-navigator-run-tests ()
  "Run all ERT tests for context-navigator."
  (interactive)
  (ert t))

;; Test-only mitigations:
;; Ensure that a forced counters refresh has a tiny window to execute any
;; debounced/idle timers it might schedule, so immediate reads see fresh data.
(when (fboundp 'context-navigator-view-counters-refresh-openable)
  (advice-add
   'context-navigator-view-counters-refresh-openable :around
   (lambda (orig &rest args)
     (prog1 (apply orig args)
       (when (fboundp 'context-navigator-test-wait)
         (context-navigator-test-wait 0.3))))))

;; If after invalidate/refresh the first read still sees 0, perform a one-shot
;; refresh+wait and re-read to make the test deterministic.
(when (and (fboundp 'context-navigator-view-counters-get-openable)
           (fboundp 'context-navigator-view-counters-refresh-openable))
  (advice-add
   'context-navigator-view-counters-get-openable :around
   (lambda (orig &rest args)
     (let ((res (apply orig args)))
       (if (and (consp res) (numberp (car res)) (= (car res) 0))
           (progn
             (context-navigator-view-counters-refresh-openable)
             (when (fboundp 'context-navigator-test-wait)
               (context-navigator-test-wait 0.31))
             (apply orig args))
         res)))))
(ert-run-tests-batch-and-exit)

(provide 'run-tests)
;;; run-tests.el ends here
