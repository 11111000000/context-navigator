;;; context-navigator-core-sync-test.el --- Tests for sync-from-gptel -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-events)

(defun cnt--reset-state-for-tests ()
  "Reset events and core state to a clean baseline."
  (context-navigator-events-reset)
  (let* ((cur (context-navigator--state-get))
         (new (context-navigator--state-with-items
               (context-navigator--state-copy cur) nil)))
    (context-navigator--set-state new)))

(ert-deftest context-navigator-sync-disabled-on-gptel-clear ()
  "Previously enabled items become disabled if gptel returns empty."
  (cnt--reset-state-for-tests)
  (let* ((it-enabled (context-navigator-item-create
                      :type 'file :name "a.txt" :path "/tmp/a.txt" :enabled t))
         (it-disabled (context-navigator-item-create
                       :type 'file :name "b.txt" :path "/tmp/b.txt" :enabled nil)))
    ;; Seed state with one enabled and one disabled
    (let* ((cur (context-navigator--state-get))
           (st  (context-navigator--state-with-items (context-navigator--state-copy cur)
                                                     (list it-enabled it-disabled))))
      (context-navigator--set-state st))
    ;; Simulate gptel available but empty context
    (cl-letf (((symbol-function 'context-navigator-gptel-available-p) (lambda () t))
              ((symbol-function 'context-navigator-gptel-pull) (lambda () nil)))
      (let* ((new (context-navigator--sync-from-gptel))
             (items (and (context-navigator-state-p new)
                         (context-navigator-state-items new)))
             (idx (context-navigator-model-build-index items)))
        (should (equal (length items) 2))
        (let* ((k1 (context-navigator-model-item-key it-enabled))
               (k2 (context-navigator-model-item-key it-disabled)))
          (should (eq (context-navigator-item-enabled (gethash k1 idx)) nil))
          (should (eq (context-navigator-item-enabled (gethash k2 idx)) nil)))))))

(ert-deftest context-navigator-sync-incoming-enables ()
  "Incoming items from gptel force enabled state even if previously disabled."
  (cnt--reset-state-for-tests)
  (let* ((same-path "/tmp/c.txt")
         (old-disabled (context-navigator-item-create
                        :type 'file :name "c.txt" :path same-path :enabled nil))
         (incoming-enabled (context-navigator-item-create
                            :type 'file :name "c.txt" :path same-path :enabled t)))
    ;; Seed old state with disabled item
    (let* ((cur (context-navigator--state-get))
           (st  (context-navigator--state-with-items (context-navigator--state-copy cur)
                                                     (list old-disabled))))
      (context-navigator--set-state st))
    ;; Simulate gptel reporting that item as present/enabled
    (cl-letf (((symbol-function 'context-navigator-gptel-available-p) (lambda () t))
              ((symbol-function 'context-navigator-gptel-pull) (lambda () (list incoming-enabled))))
      (let* ((new (context-navigator--sync-from-gptel))
             (items (and (context-navigator-state-p new)
                         (context-navigator-state-items new)))
             (idx (context-navigator-model-build-index items))
             (k (context-navigator-model-item-key incoming-enabled)))
        (should (equal (length items) 1))
        (should (eq (context-navigator-item-enabled (gethash k idx)) t))))))
