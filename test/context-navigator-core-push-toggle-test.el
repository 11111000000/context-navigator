;;; context-navigator-core-push-toggle-test.el --- Tests for push-to-gptel gating -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-persist)

(defun ctxnav-test--install-root (root)
  "Set core state last-project-root to ROOT."
  (let* ((st (context-navigator--state-get))
         (new (context-navigator--state-copy st)))
    (setf (context-navigator-state-last-project-root new) root)
    (context-navigator--set-state new)))

(ert-deftest ctxnav-core-push-toggle/gating-when-off ()
  "When push-to-gptel is off, group switches and unload do not touch gptel."
  (context-navigator-test-with-temp-dir root
                                        (context-navigator-test-with-mocked-gptel
                                         (let ((context-navigator--push-to-gptel nil))
                                           (ctxnav-test--install-root root)
                                           ;; Prepare group file with one enabled file item
                                           (let* ((f (expand-file-name "a.txt" root))
                                                  (_ (with-temp-file f (insert "x")))
                                                  (it (context-navigator-item-create :type 'file :path f :name "a.txt" :enabled t))
                                                  (slug "g"))
                                             (ignore-errors (context-navigator-persist-save (list it) root slug))
                                             ;; Load the group (async)
                                             (ignore-errors (funcall #'context-navigator--load-group-for-root root slug))
                                             (context-navigator-test-wait 0.06)
                                             (should (equal gptel--calls nil))
                                             ;; Manual unload should also avoid gptel
                                             (context-navigator-context-unload)
                                             (context-navigator-test-wait 0.02)
                                             (should (equal gptel--calls nil))
                                             ;; Deleting active group should not clear gptel when push is off
                                             (ctxnav-test--install-root root)
                                             ;; Switch to group first to make it active
                                             (ignore-errors (funcall #'context-navigator--load-group-for-root root slug))
                                             (context-navigator-test-wait 0.06)
                                             (setq gptel--calls nil)
                                             (context-navigator-group-delete slug)
                                             (context-navigator-test-wait 0.05)
                                             (should (equal gptel--calls nil)))))))

(ert-deftest ctxnav-core-push-toggle/reset-on-group-switch-when-on ()
  "When push-to-gptel is on, switching a group produces reset + adds."
  (context-navigator-test-with-temp-dir root
                                        (context-navigator-test-with-mocked-gptel
                                         (let ((context-navigator--push-to-gptel t))
                                           (ctxnav-test--install-root root)
                                           (let* ((f (expand-file-name "b.txt" root))
                                                  (_ (with-temp-file f (insert "y")))
                                                  (it (context-navigator-item-create :type 'file :path f :name "b.txt" :enabled t))
                                                  (slug "g2"))
                                             (ignore-errors (context-navigator-persist-save (list it) root slug))
                                             (setq gptel--calls nil)
                                             (ignore-errors (funcall #'context-navigator--load-group-for-root root slug))
                                             (context-navigator-test-wait 0.08)
                                             ;; We expect remove-all then add-file call(s)
                                             (should (cl-find 'gptel-context-remove-all gptel--calls :key #'car))
                                             (should (cl-find 'gptel-context-add-file gptel--calls :key #'car)))))))

(ert-deftest ctxnav-core-push-toggle/manual-push-and-clear-commands ()
  "Manual commands push-to-gptel-now and clear-gptel-now operate regardless of auto-push flag."
  (context-navigator-test-with-temp-dir root
                                        (context-navigator-test-with-mocked-gptel
                                         (let ((context-navigator--push-to-gptel nil)) ;; auto-push irrelevant for manual ops
                                           (ctxnav-test--install-root root)
                                           ;; Seed model with one enabled file
                                           (let* ((f (expand-file-name "c.txt" root)))
                                             (with-temp-file f (insert "z"))
                                             (let ((it (context-navigator-item-create :type 'file :path f :name "c.txt" :enabled t)))
                                               (context-navigator-set-items (list it))))
                                           ;; Manual push now: expect reset + add
                                           (setq gptel--calls nil)
                                           (context-navigator-push-to-gptel-now)
                                           (should (cl-find 'gptel-context-remove-all gptel--calls :key #'car))
                                           (should (cl-find 'gptel-context-add-file gptel--calls :key #'car))
                                           ;; Manual clear: expect remove-all
                                           (setq gptel--calls nil)
                                           (context-navigator-clear-gptel-now)
                                           (should (cl-find 'gptel-context-remove-all gptel--calls :key #'car))))))

(provide 'context-navigator-core-push-toggle-test)
;;; context-navigator-core-push-toggle-test.el ends here
