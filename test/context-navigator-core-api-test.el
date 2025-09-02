;;; context-navigator-core-api-test.el --- Tests for core API (set/add/remove/toggle) -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-events)

(defun context-navigator-core-api-test--reset-state (&optional items)
  "Reset global state to a fresh struct, optionally with ITEMS."
  (let ((st (context-navigator--state-make)))
    (when items
      (setq st (context-navigator--state-with-items st items)))
    (funcall #'context-navigator--set-state st)
    st))

(ert-deftest context-navigator-core-api/set-items-publishes-and-builds-index ()
  (context-navigator-test-with-clean-events
   (context-navigator-core-api-test--reset-state)
   (let* ((hits 0))
     (context-navigator-events-subscribe
      :model-refreshed (lambda (&rest _) (cl-incf hits)))
     (let* ((a (context-navigator-item-create :type 'file :path "/tmp/a.txt" :name "a.txt" :enabled t))
            (b (context-navigator-item-create :type 'selection :path "/tmp/b.txt" :beg 1 :end 5 :name "b.txt:1-5" :enabled nil))
            (st0 (context-navigator--state-get)))
       (should (= (context-navigator-state-generation st0) 0))
       (let ((new (context-navigator-set-items (list a b))))
         (should (context-navigator-state-p new))
         (should (= (length (context-navigator-state-items new)) 2))
         (let ((idx (context-navigator-state-index new)))
           (should (gethash (context-navigator-model-item-key a) idx))
           (should (gethash (context-navigator-model-item-key b) idx)))
         (should (= hits 1))
         (should (= (context-navigator-state-generation new) 1)))))))

(ert-deftest context-navigator-core-api/add-remove-toggle-roundtrip ()
  (context-navigator-test-with-clean-events
   (context-navigator-core-api-test--reset-state)
   (let* ((key (context-navigator-model-item-key
                (context-navigator-item-create :type 'file :path "/tmp/a.txt"))))
     ;; add enabled
     (let* ((it-en (context-navigator-item-create :type 'file :path "/tmp/a.txt" :name "a" :enabled t))
            (st1 (context-navigator-add-item it-en)))
       (should (context-navigator-state-p st1))
       (should (= (length (context-navigator-state-items st1)) 1))
       (let ((idx (context-navigator-state-index st1)))
         (should (gethash key idx))
         (should (context-navigator-item-enabled (gethash key idx)))))
     ;; add same key disabled -> last wins, still 1 item but enabled=nil
     (let* ((it-dis (context-navigator-item-create :type 'file :path "/tmp/a.txt" :name "a" :enabled nil))
            (st2 (context-navigator-add-item it-dis)))
       (should (= (length (context-navigator-state-items st2)) 1))
       (let ((idx (context-navigator-state-index st2)))
         (should (not (context-navigator-item-enabled (gethash key idx))))))
     ;; toggle -> enabled becomes t
     (let* ((st3 (context-navigator-toggle-item key)))
       (let ((idx (context-navigator-state-index st3)))
         (should (context-navigator-item-enabled (gethash key idx)))))
     ;; explicit toggle to nil
     (let* ((st4 (context-navigator-toggle-item key nil)))
       (let ((idx (context-navigator-state-index st4)))
         (should (not (context-navigator-item-enabled (gethash key idx))))))
     ;; remove
     (let* ((st5 (context-navigator-remove-item-by-key key)))
       (should (= (length (context-navigator-state-items st5)) 0))
       (let ((idx (context-navigator-state-index st5)))
         (should (null (gethash key idx))))))))

(ert-deftest context-navigator-core-api/toggle-nonexistent-returns-same ()
  (context-navigator-test-with-clean-events
   (context-navigator-core-api-test--reset-state)
   (let* ((st0 (context-navigator--state-get))
          (st1 (context-navigator-toggle-item "file:/does/not/exist.txt")))
     (should (eq st0 st1)))))

(provide 'context-navigator-core-api-test)
;;; context-navigator-core-api-test.el ends here
