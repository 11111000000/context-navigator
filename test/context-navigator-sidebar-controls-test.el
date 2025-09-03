;;; context-navigator-sidebar-controls-test.el --- Tests for footer controls visibility -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-sidebar)
(require 'context-navigator-model)

(ert-deftest ctxnav-sidebar/footer-controls-push-off-no-gptel ()
  "When push is OFF and gptel is empty, show [Push now] and [Clear gptel] (disabled)."
  (let ((context-navigator-controls-style 'text)
        (context-navigator-language 'en))
    (cl-letf (((symbol-value 'context-navigator--push-to-gptel) nil)
              ((symbol-function 'context-navigator-gptel-pull) (lambda () '())))
      (let* ((segs (context-navigator-sidebar--footer-control-segments))
             (joined (mapconcat #'identity segs "")))
        (should (string-match-p "\\[Push now\\]" joined))
        (should (string-match-p "\\[Clear gptel\\]" joined))))))

(ert-deftest ctxnav-sidebar/footer-controls-push-off-has-gptel ()
  "When push is OFF and gptel has entries, show both [Push now] and [Clear gptel]."
  (let ((context-navigator-controls-style 'text)
        (context-navigator-language 'en))
    (cl-letf* (((symbol-value 'context-navigator--push-to-gptel) nil)
               ((symbol-function 'context-navigator-gptel-pull)
                (lambda () (list (context-navigator-item-create :type 'file :path "/tmp/a" :name "a" :enabled t)))))
      (let* ((segs (context-navigator-sidebar--footer-control-segments))
             (joined (mapconcat #'identity segs "")))
        (should (string-match-p "\\[Push now\\]" joined))
        (should (string-match-p "\\[Clear gptel\\]" joined))))))

(ert-deftest ctxnav-sidebar/footer-controls-push-on-has-gptel ()
  "When push is ON and gptel has entries, [Push now] is present (disabled), [Clear gptel] is present."
  (let ((context-navigator-controls-style 'text)
        (context-navigator-language 'en))
    (cl-letf* (((symbol-value 'context-navigator--push-to-gptel) t)
               ((symbol-function 'context-navigator-gptel-pull)
                (lambda () (list (context-navigator-item-create :type 'file :path "/tmp/a" :name "a" :enabled t)))))
      (let* ((segs (context-navigator-sidebar--footer-control-segments))
             (joined (mapconcat #'identity segs "")))
        (should (string-match-p "\\[Push now\\]" joined))
        (should (string-match-p "\\[Clear gptel\\]" joined))))))

(provide 'context-navigator-sidebar-controls-test)
;;; context-navigator-sidebar-controls-test.el ends here
