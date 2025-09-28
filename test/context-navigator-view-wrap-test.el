;;; context-navigator-view-wrap-test.el --- Tests for footer/header wrap -*- lexical-binding: t; -*-

(require 'ert)
(require 'context-navigator-view)

(ert-deftest ctxnav-sidebar/wrap-segments-basic ()
  "Wrap helper should split segments to fit within width."
  (let* ((segs '(" AAA" " BBB" " CCC" " DDD"))
         (lines (context-navigator-view-controls--wrap-segments segs 10)))
    ;; Each line width must be <= 10 and preserve order
    (dolist (ln lines)
      (should (<= (string-width ln) 10)))
    ;; Join back and compare ignoring line breaks
    (should (equal (mapconcat #'identity lines "")
                   (mapconcat #'identity segs "")))))

(provide 'context-navigator-view-wrap-test)
;;; context-navigator-view-wrap-test.el ends here
