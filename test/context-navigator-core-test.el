;;; context-navigator-core-test.el --- Tests for core sync/merge/gating -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-events)

(defun context-navigator-core-test--reset-state (&optional items)
  "Reset global state to a fresh one, optionally with ITEMS installed."
  (let* ((st (context-navigator--state-make)))
    (when items
      (setq st (context-navigator--state-with-items st items)))
    (funcall #'context-navigator--set-state st)
    st))

(ert-deftest context-navigator-core/merge-disabled-on-sync ()
  "Disabled items in the model are preserved across sync-from-gptel."
  (context-navigator-test-with-mocked-gptel
   ;; Prepare: one disabled 'old' not present in gptel, gptel has one enabled
   (let* ((old-disabled (context-navigator-item-create
                         :type 'file :path "/tmp/old.txt" :name "old.txt" :enabled nil))
          (new-enabled-path "/tmp/new.txt"))
     (setq gptel--context (list (list :type 'file :path new-enabled-path)))
     (context-navigator-core-test--reset-state (list old-disabled))
     (let ((state (context-navigator--sync-from-gptel)))
       (should (context-navigator-state-p state))
       (let* ((items (context-navigator-state-items state))
              (keys (mapcar #'context-navigator-model-item-key items)))
         (should (= (length items) 2))
         (should (member (context-navigator-model-item-key old-disabled) keys))
         (should (member (format "file:%s" new-enabled-path) keys))
         ;; check flags
         (let ((old* (cl-find (context-navigator-model-item-key old-disabled) items
                              :key #'context-navigator-model-item-key :test #'equal))
               (new* (cl-find (format "file:%s" new-enabled-path) items
                              :key #'context-navigator-model-item-key :test #'equal)))
           (should (null (context-navigator-item-enabled old*)))
           (should (context-navigator-item-enabled new*))))))))

(ert-deftest context-navigator-core/on-gptel-change-gated-by-inhibit-refresh ()
  "on-gptel-change should not sync when inhibit-refresh is set."
  (context-navigator-test-with-mocked-gptel
   (let ((hits 0))
     ;; ensure gptel is 'available' and empty
     (setq gptel--context '())
     (context-navigator-events-reset)
     (context-navigator-core-test--reset-state)
     (context-navigator-events-subscribe :model-refreshed (lambda (&rest _) (cl-incf hits)))
     ;; set inhibit-refresh and call handler → expected 0 hits
     (let ((st (context-navigator--state-get)))
       (setf (context-navigator-state-inhibit-refresh st) t)
       (funcall #'context-navigator--set-state (cl-copy-struct st)))
     (context-navigator--on-gptel-change)
     (context-navigator-test-wait 0.08)
     (should (= hits 0))
     ;; clear inhibit-refresh and call again → expected >=1 hit
     (let ((st2 (context-navigator--state-get)))
       (setf (context-navigator-state-inhibit-refresh st2) nil)
       (funcall #'context-navigator--set-state (cl-copy-struct st2)))
     (context-navigator--on-gptel-change)
     (context-navigator-test-wait 0.08)
     (should (>= hits 1)))))

(provide 'context-navigator-core-test)
;;; context-navigator-core-test.el ends here
