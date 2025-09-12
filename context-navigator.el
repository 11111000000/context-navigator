;;; context-navigator.el --- Umbrella entry for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT
;; Author: Peter Kosov <11111000000@email.com>
;; Maintainer: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/context-navigator
;; Keywords: convenience, tools
;; Version: 1.1.9
;; Package-Requires: ((emacs "29.1") (transient "0.3.0"))

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
;; - context-navigator-view   — sidebar UI (side window)

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
;;;###autoload
(autoload 'context-navigator-groups-open "context-navigator-core" "Open groups list (publish event for sidebar)." t)
;;;###autoload
(autoload 'context-navigator-group-switch "context-navigator-core" "Switch active group." t)
;;;###autoload
(autoload 'context-navigator-group-create "context-navigator-core" "Create a new group." t)
;;;###autoload
(autoload 'context-navigator-group-rename "context-navigator-core" "Rename a group." t)
;;;###autoload
(autoload 'context-navigator-group-delete "context-navigator-core" "Delete a group." t)
;;;###autoload
(autoload 'context-navigator-group-duplicate "context-navigator-core" "Duplicate a group." t)

;; Sidebar entry points (autoloaded)
;;;###autoload
(autoload 'context-navigator-view-open "context-navigator-view" "Open the sidebar window." t)
;;;###autoload
(autoload 'context-navigator-view-close "context-navigator-view" "Close the sidebar window." t)
;;;###autoload
(autoload 'context-navigator-view-toggle "context-navigator-view" "Toggle the sidebar window." t)
;;;###autoload
(autoload 'context-navigator-view-show-groups "context-navigator-view" "Open the sidebar and show groups list." t)
;;;###autoload
(autoload 'context-navigator-transient "context-navigator-transient" "Open Context Navigator transient." t)
;;;###autoload
(autoload 'context-navigator-open "context-navigator-core" "Open Navigator in current display mode." t)
;;;###autoload
(autoload 'context-navigator-close "context-navigator-core" "Close Navigator in current display mode." t)
;;;###autoload
(autoload 'context-navigator-toggle "context-navigator-core" "Toggle Navigator in current display mode." t)
;;;###autoload
(autoload 'context-navigator-display-mode-toggle "context-navigator-core" "Toggle display mode (buffer/sidebar) and reopen." t)

;;;###autoload
(defun context-navigator-version ()
  "Return version string for Context Navigator."
  (interactive)
  (message "context-navigator — 1.1.9")
  "1.1.9")

;;;###autoload
(defun context-navigator-start ()
  "Enable the mode, refresh model, and open Navigator (buffer or sidebar) per current display mode."
  (interactive)
  (context-navigator-mode 1)     ;; autoloads core
  (context-navigator-refresh)    ;; autoloads core if needed
  (ignore-errors (context-navigator-open)))

(provide 'context-navigator)
;;; context-navigator.el ends here
