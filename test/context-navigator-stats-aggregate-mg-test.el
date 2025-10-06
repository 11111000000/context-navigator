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



(provide 'context-navigator-stats-aggregate-mg-test)
;;; context-navigator-stats-aggregate-mg-test.el ends here
