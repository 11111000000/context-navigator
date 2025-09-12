;;; context-navigator-core-project-toggle-test.el --- Tests for auto-project switch toggle -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-core)
(require 'context-navigator-project)

(defun ctxnav-test--get-last-root ()
  (let ((st (context-navigator--state-get)))
    (and st (context-navigator-state-last-project-root st))))

(ert-deftest ctxnav-core-project-toggle/handler-updates-last-root-when-flag-off ()
  "Handler should update last-project-root even when auto-project-switch is off (for UI/header)."
  (context-navigator-test-with-temp-dir rootA
                                        (context-navigator-test-with-temp-dir rootB
                                                                              (let ((context-navigator--auto-project-switch nil))
                                                                                ;; seed state.last-project-root = rootA
                                                                                (let* ((st (context-navigator--state-get))
                                                                                       (new (context-navigator--state-copy st)))
                                                                                  (setf (context-navigator-state-last-project-root new) rootA)
                                                                                  (context-navigator--set-state new))
                                                                                ;; call handler -> update last-project-root for UI purposes
                                                                                (context-navigator--on-project-switch rootB)
                                                                                (should (equal (ctxnav-test--get-last-root) rootB))))))

(ert-deftest ctxnav-core-project-toggle/manual-switch-command-works ()
  "Manual command should switch root even when auto-project-switch is off."
  (context-navigator-test-with-temp-dir root
                                        (let ((context-navigator--auto-project-switch nil))
                                          ;; stub project-current-root to our temp root
                                          (cl-letf (((symbol-function 'context-navigator-project-current-root) (lambda (&optional _b) root)))
                                            (context-navigator-switch-to-current-buffer-project)
                                            (should (equal (ctxnav-test--get-last-root) root))))))

(ert-deftest ctxnav-core-project-toggle/handler-updates-when-flag-on ()
  "With auto-project-switch on, handler should update last-project-root."
  (context-navigator-test-with-temp-dir rootA
                                        (context-navigator-test-with-temp-dir rootB
                                                                              (let ((context-navigator--auto-project-switch t)
                                                                                    (context-navigator-autoload nil)) ;; do not load contexts in this test
                                                                                (let* ((st (context-navigator--state-get))
                                                                                       (new (context-navigator--state-copy st)))
                                                                                  (setf (context-navigator-state-last-project-root new) rootA)
                                                                                  (context-navigator--set-state new))
                                                                                (context-navigator--on-project-switch rootB)
                                                                                (should (equal (ctxnav-test--get-last-root) rootB))))))

(provide 'context-navigator-core-project-toggle-test)
;;; context-navigator-core-project-toggle-test.el ends here
