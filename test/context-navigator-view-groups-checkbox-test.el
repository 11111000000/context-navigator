;;; context-navigator-view-groups-checkbox-test.el --- Tests for MG checkboxes in groups view -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'context-navigator-view)
(require 'context-navigator-view-groups)
(require 'context-navigator-core)
(require 'context-navigator-persist)

(defun cn-test--make-state (&optional root slug)
  (context-navigator--state-make
   :last-project-root (or root "/tmp")
   :current-group-slug (or slug "g1")))



(provide 'context-navigator-view-groups-checkbox-test)
;;; context-navigator-view-groups-checkbox-test.el ends here
