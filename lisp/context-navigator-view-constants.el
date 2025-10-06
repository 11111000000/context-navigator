;;; context-navigator-view-constants.el --- Constants for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Small, dependency-light constants used across Navigator view modules to avoid
;; duplicate defvar/defconst and load order issues.

;;; Code:

(defconst context-navigator-view--buffer-name "*context-navigator*"
  "Canonical buffer name for Context Navigator view and buffer-mode.")

(defvar context-navigator-view-window-params
  '((side . left) (slot . -1))
  "Default parameters for the sidebar window.")


(provide 'context-navigator-view-constants)
;;; context-navigator-view-constants.el ends here
