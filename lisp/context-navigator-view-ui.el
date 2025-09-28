;;; context-navigator-view-ui.el --- Small UI helpers (keymaps, segments) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Tiny, dependency-light helpers for building consistent interactive
;; segments in Navigator views. Kept separate to avoid require cycles.
;;
;; Exposed API:
;; - context-navigator-view-ui-make-keymap (COMMAND &optional PARENT)
;;   Build a sparse keymap for clickable segments that handles mouse-1,
;;   RET/C-m and TAB variants, inheriting from PARENT when provided.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun context-navigator-view-ui-make-keymap (command &optional parent)
  "Return a sparse keymap for interactive segment bound to COMMAND.
When PARENT is a keymap, inherit it so navigation keys (n/p, j/k, arrows)
continue to work within the segment."
  (let ((m (make-sparse-keymap)))
    (when (keymapp parent)
      (set-keymap-parent m parent))
    ;; Mouse clicks (both regular and header-line areas)
    (define-key m [mouse-1] command)
    (define-key m [header-line mouse-1] command)
    ;; RET variants
    (define-key m (kbd "RET")      command)
    (define-key m (kbd "C-m")      command)
    (define-key m [return]         command)
    (define-key m (kbd "<return>") command)
    ;; TAB variants (handy for toggles)
    (define-key m (kbd "TAB")   command)
    (define-key m (kbd "<tab>") command)
    (define-key m [tab]         command)
    (define-key m (kbd "C-i")   command)
    m))

(provide 'context-navigator-view-ui)
;;; context-navigator-view-ui.el ends here
