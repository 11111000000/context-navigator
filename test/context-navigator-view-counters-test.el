;;; context-navigator-view-counters-test.el --- Tests for view counters -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-core)
(require 'context-navigator-view-counters)

(defun ctxnav-counters--with-sidebar (&rest body)
  "Execute BODY with a live Navigator sidebar buffer."
  (let ((buf (get-buffer-create "*context-navigator*")))
    (unwind-protect
        (with-current-buffer buf
          ;; make buffer-local variables be created in this buffer
          (eval (cons 'progn body)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest ctxnav-counters/refresh-and-get-basic ()
  "Refresh openable counters and read them back."
  (context-navigator-test-with-temp-dir root
                                        (let* ((f1 (expand-file-name "a.txt" root))
                                               (f2 (expand-file-name "b.txt" root)))
                                          (with-temp-file f1 (insert "x"))
                                          (with-temp-file f2 (insert "y"))
                                          (let* ((it1 (context-navigator-item-create :type 'file :name "a" :path f1 :enabled t))
                                                 (it2 (context-navigator-item-create :type 'file :name "b" :path f2 :enabled t)))
                                            (context-navigator-set-items (list it1 it2))
                                            (ctxnav-counters--with-sidebar
                                             ;; Force a fresh compute
                                             (context-navigator-view-counters-invalidate)
                                             (context-navigator-view-counters-refresh-openable)
                                             (let ((res (context-navigator-view-counters-get-openable)))
                                               (should (consp res))
                                               (should (= (car res) 2))
                                               (should (not (cdr res)))))))))

(ert-deftest ctxnav-counters/get-triggers-debounced-refresh ()
  "When cache is stale or empty, get-openable schedules a refresh."
  (context-navigator-test-with-temp-dir root
                                        (let* ((f1 (expand-file-name "a.txt" root)))
                                          (with-temp-file f1 (insert "x"))
                                          (let* ((it1 (context-navigator-item-create :type 'file :name "a" :path f1 :enabled t)))
                                            (context-navigator-set-items (list it1))
                                            (ctxnav-counters--with-sidebar
                                             ;; invalidate -> cache empty
                                             (context-navigator-view-counters-invalidate)
                                             ;; First read may be 0 but should schedule refresh
                                             (let ((res1 (context-navigator-view-counters-get-openable)))
                                               (should (consp res1)))
                                             ;; Wait a bit for debounced refresh to run
                                             (context-navigator-test-wait 0.3)
                                             (let ((res2 (context-navigator-view-counters-get-openable)))
                                               (should (= (car res2) 1))))))))

(ert-deftest ctxnav-counters/collect-closable-buffers ()
  "Collect live buffers referenced by model items."
  (context-navigator-test-with-temp-dir root
                                        (let* ((f1 (expand-file-name "a.txt" root))
                                               (f2 (expand-file-name "b.txt" root)))
                                          (with-temp-file f1 (insert "x"))
                                          (with-temp-file f2 (insert "y"))
                                          (let* ((b1 (find-file-noselect f1))
                                                 (b2 (find-file-noselect f2))
                                                 (it1 (context-navigator-item-create :type 'file :name "a" :path f1 :enabled t))
                                                 (it2 (context-navigator-item-create :type 'selection :name "sel" :path f2 :beg 1 :end 1 :enabled t)))
                                            (unwind-protect
                                                (progn
                                                  (context-navigator-set-items (list it1 it2))
                                                  (let ((lst (context-navigator-view-counters-collect-closable)))
                                                    (should (listp lst))
                                                    (should (>= (length lst) 2))
                                                    (should (memq b1 lst))
                                                    (should (memq b2 lst))))
                                              (when (buffer-live-p b1) (kill-buffer b1))
                                              (when (buffer-live-p b2) (kill-buffer b2)))))))

(provide 'context-navigator-view-counters-test)
;;; context-navigator-view-counters-test.el ends here
