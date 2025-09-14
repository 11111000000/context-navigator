;;; context-navigator-view-indicators-test.el --- Tests for GPTel indicators -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-view-indicators)

(ert-deftest ctxnav-indicators/collect-fallback-to-model ()
  "When raw and pull are empty, collect should fallback to enabled model items."
  (with-temp-buffer
    (rename-buffer "*context-navigator*" t)
    (let* ((it (context-navigator-item-create :type 'file :path "/tmp/x" :name "x" :enabled t)))
      (context-navigator-set-items (list it))
      (cl-letf (((symbol-function 'context-navigator-gptel--raw-keys) (lambda () nil))
                ((symbol-function 'context-navigator-gptel-pull)    (lambda () '())))
        (let* ((keys (context-navigator-view--collect-gptel-keys))
               (key (context-navigator-model-item-key it)))
          (should (member key keys)))))))

(ert-deftest ctxnav-indicators/update-cache-triggers-render-on-change ()
  "update-gptel-keys-if-changed updates hash and schedules render when keys change."
  (with-temp-buffer
    (rename-buffer "*context-navigator*" t)
    (let ((calls 0))
      (cl-letf (((symbol-function 'context-navigator-view--schedule-render)
                 (lambda () (setq calls (1+ calls)))))
        ;; initial
        (setq context-navigator-view--gptel-keys-hash nil)
        (context-navigator-view--update-gptel-keys-if-changed '("a" "b"))
        (should (= calls 1))
        (let ((prev calls))
          ;; same keys -> no new render
          (context-navigator-view--update-gptel-keys-if-changed '("a" "b"))
          (should (= calls prev))
          ;; different keys -> render again
          (context-navigator-view--update-gptel-keys-if-changed '("a" "c"))
          (should (= calls (1+ prev))))))))

(provide 'context-navigator-view-indicators-test)
;;; context-navigator-view-indicators-test.el ends here
