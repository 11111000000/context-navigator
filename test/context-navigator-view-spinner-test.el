;;; context-navigator-view-spinner-test.el --- Tests for spinner helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'context-navigator-view-spinner)

(ert-deftest ctxnav-spinner/start-stop-basic ()
  "Spinner should start a timer and stop/reset cleanly."
  (with-temp-buffer
    ;; Use the sidebar buffer name for consistency
    (rename-buffer "*context-navigator*" t)
    (context-navigator-view-spinner-start)
    (should (timerp context-navigator-view--spinner-timer))
    (should (numberp context-navigator-view--spinner-index))
    (context-navigator-view-spinner-stop)
    (should (null context-navigator-view--spinner-timer))
    (should (= context-navigator-view--spinner-index 0))
    (should (numberp context-navigator-view--spinner-last-time))
    (should (null context-navigator-view--spinner-degraded))))

(provide 'context-navigator-view-spinner-test)
;;; context-navigator-view-spinner-test.el ends here
