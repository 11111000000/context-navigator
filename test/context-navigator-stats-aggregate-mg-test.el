;;; context-navigator-stats-aggregate-mg-test.el --- Tests for Stats MG aggregate header -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'context-navigator-stats)
(require 'context-navigator-core)
(require 'context-navigator-persist)

(defun cn-test--set-core-items (&optional root)
  (context-navigator--set-state
   (context-navigator--state-make
    :last-project-root (or root "/tmp")
    :current-group-slug "g1"
    :items
    (list
     (context-navigator-item-create :type 'file :name "a.txt" :path "/tmp/a.txt" :enabled t)
     (context-navigator-item-create :type 'file :name "b.txt" :path "/tmp/b.txt" :enabled nil)))))

(ert-deftest context-navigator-stats-header-shows-mg-groups-count ()
  "Stats footer header should show “Stats (MG, N)” when MG is ON with selection."
  (cn-test--set-core-items "/tmp/mg-root")
  (cl-letf (((symbol-function 'context-navigator-persist-state-load)
             (lambda (_root) (list :version 1 :multi t :selected '("g1" "g2"))))
            ;; Avoid async complications; let the fallback path set :mg/:groups
            ((symbol-function 'context-navigator-collect-items-for-groups-async)
             (lambda (_root _slugs _cb) nil)))
    (let* ((lines (context-navigator-stats-footer-lines 80))
           (hdr (car lines)))
      (should (stringp hdr))
      (should (string-match-p "Stats (MG, 2)" hdr)))))

(provide 'context-navigator-stats-aggregate-mg-test)
;;; context-navigator-stats-aggregate-mg-test.el ends here
