;;; context-navigator-groups-delete-test.el --- Tests for group delete confirmation in batch -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-core)
(require 'context-navigator-persist)
(require 'context-navigator-test-helpers)

(ert-deftest ctxnav-groups/delete-no-confirm-in-batch ()
  "In batch (noninteractive) mode, deleting a group should not prompt for confirmation."
  (context-navigator-test-with-temp-dir dir
                                        (let* ((root dir)
                                               ;; create a real group file "g.el"
                                               (slug "g")
                                               (_file (context-navigator-persist-save '() root slug))
                                               (prompt-calls 0))
                                          ;; Point core to our temp root so group-delete resolves the correct path
                                          (let* ((st (context-navigator--state-get))
                                                 (new (context-navigator--state-copy st)))
                                            (setf (context-navigator-state-last-project-root new) root)
                                            (funcall #'context-navigator--set-state new))
                                          ;; If code tried to prompt, this stub would be called and increment counter.
                                          (cl-letf (((symbol-function 'yes-or-no-p)
                                                     (lambda (&rest _args) (cl-incf prompt-calls) t)))
                                            (context-navigator-group-delete slug)
                                            (should (= prompt-calls 0))
                                            ;; file must be gone
                                            (let ((p (context-navigator-persist-context-file root slug)))
                                              (should (not (file-exists-p p))))))))

(provide 'context-navigator-groups-delete-test)
;;; context-navigator-groups-delete-test.el ends here
