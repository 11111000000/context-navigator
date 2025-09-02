;;; context-navigator-events-test.el --- Tests for event bus/debouncer -*- lexical-binding: t; -*-

(require 'ert)
(require 'context-navigator-test-helpers)
(require 'context-navigator-events)

(ert-deftest context-navigator-events/subscribe-unsubscribe ()
  (context-navigator-test-with-clean-events
   (let ((hits 0))
     (let ((tok (context-navigator-events-subscribe :ping (lambda (&rest _) (cl-incf hits)))))
       (context-navigator-events-publish :ping 1)
       (should (= hits 1))
       (context-navigator-events-unsubscribe tok)
       (context-navigator-events-publish :ping 2)
       (should (= hits 1))))))

(ert-deftest context-navigator-events/debounce-last-wins ()
  (context-navigator-test-with-clean-events
   (let ((x 0))
     (context-navigator-events-debounce :k 0.02 (lambda () (setq x 1)))
     (context-navigator-events-debounce :k 0.02 (lambda () (setq x 2)))
     (context-navigator-events-debounce :k 0.02 (lambda () (setq x 3)))
     (context-navigator-test-wait 0.08)
     (should (= x 3)))))

(provide 'context-navigator-events-test)
;;; context-navigator-events-test.el ends here
