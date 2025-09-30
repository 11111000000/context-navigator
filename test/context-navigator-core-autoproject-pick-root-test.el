;;; context-navigator-core-autoproject-pick-root-test.el --- Tests for autoproj root pick (frame-first) -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-core)
(require 'context-navigator-project)
(require 'context-navigator-test-helpers)

(defun ctxnav-test--get-last-root ()
  (let ((st (context-navigator--state-get)))
    (and st (context-navigator-state-last-project-root st))))

(ert-deftest ctxnav-core-autoproject/pick-root-prefers-frame-file-window ()
  "Enabling auto-project should use frame's first file-visiting window root when available."
  (context-navigator-test-with-temp-dir rootA
                                        (let ((context-navigator--auto-project-switch nil)
                                              (context-navigator-autoload nil))
                                          (cl-letf (((symbol-function 'context-navigator-project--frame-file-project-root)
                                                     (lambda () rootA))
                                                    ;; Ensure buffer-based fallbacks don't override frame-root in this test
                                                    ((symbol-function 'context-navigator-project-root)
                                                     (lambda (&optional _b) nil)))
                                            (context-navigator-toggle-auto-project-switch)
                                            (should (equal (ctxnav-test--get-last-root) rootA))))))

(provide 'context-navigator-core-autoproject-pick-root-test)
;;; context-navigator-core-autoproject-pick-root-test.el ends here
