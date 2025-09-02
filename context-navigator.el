;;; context-navigator.el --- Umbrella entry for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; This is a small, clear entry point (umbrella) that exposes public commands
;; and defers loading to the respective modules via autoload.
;;
;; Main modules:
;; - context-navigator-core      — core state, commands, wiring
;; - context-navigator-events    — event bus and debouncer
;; - context-navigator-fp        — functional helpers
;; - context-navigator-model     — pure model (items, diff)
;; - context-navigator-persist   — v3 persistence layer (save/load async)
;; - context-navigator-gptel-bridge — gptel adapter (pull/apply, change advices)
;; - context-navigator-project   — project detection and :project-switch events
;; - context-navigator-render    — pure render helpers
;; - context-navigator-icons     — optional icon provider
;; - context-navigator-sidebar   — sidebar UI (side window)

;;; Code:

;; Public commands from core (autoloaded)
;;;###autoload
(autoload 'context-navigator-mode "context-navigator-core" "Global minor mode for Context Navigator." t)
;;;###autoload
(autoload 'context-navigator-refresh "context-navigator-core" "Recompute indices and publish a refresh event." t)
;;;###autoload
(autoload 'context-navigator-context-load "context-navigator-core" "Load context for project/global (async)." t)
;;;###autoload
(autoload 'context-navigator-context-save "context-navigator-core" "Save current context to file." t)
;;;###autoload
(autoload 'context-navigator-context-unload "context-navigator-core" "Unload/clear context and switch to global." t)

;; Sidebar entry points (autoloaded)
;;;###autoload
(autoload 'context-navigator-sidebar-open "context-navigator-sidebar" "Open the sidebar window." t)
;;;###autoload
(autoload 'context-navigator-sidebar-close "context-navigator-sidebar" "Close the sidebar window." t)
;;;###autoload
(autoload 'context-navigator-sidebar-toggle "context-navigator-sidebar" "Toggle the sidebar window." t)

;;;###autoload
(defun context-navigator-version ()
  "Return version string for Context Navigator."
  (interactive)
  (message "context-navigator — 0.2.1")
  "0.2.1")

;;;###autoload
(defun context-navigator-start ()
  "Enable the mode, refresh model, and open the sidebar."
  (interactive)
  (context-navigator-mode 1)     ;; autoloads core
  (context-navigator-refresh)    ;; autoloads core if needed
  (ignore-errors (context-navigator-sidebar-open))) ;; autoloads sidebar

(provide 'context-navigator)
;;; context-navigator.el ends here
