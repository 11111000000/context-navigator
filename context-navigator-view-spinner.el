;;; context-navigator-view-spinner.el --- Spinner helpers for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Expose a small API for starting/stopping the lightweight loading spinner used
;; by the sidebar. Implementation currently delegates to functions defined in
;; context-navigator-view.el; later the spinner will be moved here completely.

;;; Code:

(require 'subr-x)

;; Declarations for existing implementations in view.el
(declare-function context-navigator-view--spinner-start "context-navigator-view" ())
(declare-function context-navigator-view--spinner-stop "context-navigator-view" ())

(defun context-navigator-view-spinner-start ()
  "Start or restart the sidebar loading spinner."
  (interactive)
  (ignore-errors (context-navigator-view--spinner-start)))

(defun context-navigator-view-spinner-stop ()
  "Stop the sidebar loading spinner and reset its state."
  (interactive)
  (ignore-errors (context-navigator-view--spinner-stop)))

(provide 'context-navigator-view-spinner)
;;; context-navigator-view-spinner.el ends here
