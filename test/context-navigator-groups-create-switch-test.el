;;; context-navigator-groups-create-switch-test.el --- Tests for create+switch group flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-core)
(require 'context-navigator-persist)

(defun ctxnav-test--set-root (root)
  (let* ((st (context-navigator--state-get))
         (new (context-navigator--state-copy st)))
    (setf (context-navigator-state-last-project-root new) root)
    (context-navigator--set-state new)))

(defun ctxnav-test--current-slug ()
  (let ((st (context-navigator--state-get)))
    (and st (context-navigator-state-current-group-slug st))))

(ert-deftest ctxnav-groups-create-switch/push-off ()
  "Creating a group should immediately switch to it with empty model, no gptel ops when push is off."
  (context-navigator-test-with-temp-dir root
                                        (context-navigator-test-with-mocked-gptel
                                         (let ((context-navigator--push-to-gptel nil))
                                           (ctxnav-test--set-root root)
                                           (setq gptel--calls nil)
                                           (let ((slug (context-navigator-group-create "Foo Bar")))
                                             (should (stringp slug))
                                             ;; Allow async load to complete
                                             (context-navigator-test-wait 0.08)
                                             (should (equal (ctxnav-test--current-slug) slug))
                                             (let* ((st (context-navigator--state-get)))
                                               (should (equal (length (context-navigator-state-items st)) 0)))
                                             (should (equal gptel--calls nil)))))))

(ert-deftest ctxnav-groups-create-switch/push-on ()
  "With push on, creating a new empty group should reset gptel to empty."
  (context-navigator-test-with-temp-dir root
                                        (context-navigator-test-with-mocked-gptel
                                         (let ((context-navigator--push-to-gptel t))
                                           (ctxnav-test--set-root root)
                                           (setq gptel--calls nil)
                                           (let ((slug (context-navigator-group-create "Baz")))
                                             (should (stringp slug))
                                             (context-navigator-test-wait 0.08)
                                             ;; Expect a reset to empty context
                                             (should (cl-find 'gptel-context-remove-all gptel--calls :key #'car))
                                             ;; And no add-file because new group is empty
                                             (should-not (cl-find 'gptel-context-add-file gptel--calls :key #'car)))))))

(provide 'context-navigator-groups-create-switch-test)
;;; context-navigator-groups-create-switch-test.el ends here
