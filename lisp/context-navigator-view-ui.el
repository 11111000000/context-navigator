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

(defvar context-navigator-view-ui--fallback-parent nil
  "Fallback parent keymap for Navigator interactive segments when the major mode map is not yet available.")

(defun context-navigator-view-ui-parent-keymap ()
  "Return a reliable parent keymap for Navigator UI segments.
Prefers `context-navigator-view-mode-map' when it is bound and a keymap.
Falls back to a lazily created sparse keymap cached in
`context-navigator-view-ui--fallback-parent'."
  (or (and (boundp 'context-navigator-view-mode-map)
           (keymapp context-navigator-view-mode-map)
           context-navigator-view-mode-map)
      (progn
        (unless (keymapp context-navigator-view-ui--fallback-parent)
          (setq context-navigator-view-ui--fallback-parent (make-sparse-keymap)))
        context-navigator-view-ui--fallback-parent)))

(defun context-navigator-view-ui-make-keymap (command &optional parent)
  "Return a sparse keymap for interactive segment bound to COMMAND.
When PARENT is a keymap, inherit it so navigation keys (n/p, j/k, arrows)
continue to work within the segment. If PARENT is nil or not a keymap,
inherit from a reliable Navigator parent keymap."
  (let* ((m (make-sparse-keymap))
         (p (or (and (keymapp parent) parent)
                (context-navigator-view-ui-parent-keymap))))
    (when (keymapp p)
      (set-keymap-parent m p))
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
