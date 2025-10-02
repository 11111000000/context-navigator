;;; context-navigator-view-push-now-mg-test.el --- Tests for Push Now behavior in MG -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'context-navigator-view-actions)
(require 'context-navigator-core)
(require 'context-navigator-persist)

(defun cn-test--set-core-state (root)
  (context-navigator--set-state
   (context-navigator--state-make
    :last-project-root root
    :current-group-slug "g1"
    :items '())))

(ert-deftest context-navigator-push-now-mg-aggregates ()
  "When MG is ON and selection non-empty, push-now must call aggregate apply."
  (let* ((root "/tmp/mg-agg")
         (called nil))
    (cn-test--set-core-state root)
    (cl-letf (((symbol-function 'context-navigator-persist-state-load)
               (lambda (_root) (list :version 1 :multi t :selected '("g1" "g2"))))
              ((symbol-function 'context-navigator-apply-groups-now)
               (lambda (r sl)
                 (setq called (list :agg r sl))
                 t))
              ((symbol-function 'context-navigator-push-to-gptel-now)
               (lambda ()
                 (setq called (list :single))
                 t)))
      (context-navigator-view-push-now)
      (should (consp called))
      (should (eq (car called) :agg))
      (should (equal (cadr called) root))
      (should (equal (caddr called) '("g1" "g2"))))))

(ert-deftest context-navigator-push-now-single-when-not-mg ()
  "When MG is OFF or selection empty, push-now must call single-group push."
  (let* ((root "/tmp/mg-single")
         (called nil))
    (cn-test--set-core-state root)
    (cl-letf (((symbol-function 'context-navigator-persist-state-load)
               (lambda (_root) (list :version 1 :multi nil :selected '("g1" "g2"))))
              ((symbol-function 'context-navigator-apply-groups-now)
               (lambda (&rest _) (setq called (list :agg)) t))
              ((symbol-function 'context-navigator-push-to-gptel-now)
               (lambda () (setq called (list :single)) t)))
      (context-navigator-view-push-now)
      (should (equal called '(:single))))
    ;; selection empty even when MG on -> single
    (let ((called nil))
      (cl-letf (((symbol-function 'context-navigator-persist-state-load)
                 (lambda (_root) (list :version 1 :multi t :selected '())))
                ((symbol-function 'context-navigator-apply-groups-now)
                 (lambda (&rest _) (setq called (list :agg)) t))
                ((symbol-function 'context-navigator-push-to-gptel-now)
                 (lambda () (setq called (list :single)) t)))
        (context-navigator-view-push-now)
        (should (equal called '(:single)))))))

(provide 'context-navigator-view-push-now-mg-test)
;;; context-navigator-view-push-now-mg-test.el ends here
