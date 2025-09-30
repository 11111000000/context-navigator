;;; context-navigator-view-title.el --- Title-line shim for refactor -*- lexical-binding: t; -*-
;;; Commentary:
;; Compatibility shim: some callsites still invoke
;; `context-navigator-view--title-line' after the refactor. This file
;; provides a small, well-documented implementation that other modules
;; can rely on until callers are migrated to the new API.
;;
;; The implementation is intentionally minimal: it accepts a HEADER value
;; (string, plist, or alist-ish) and returns a single-line rendering.
;; It avoids heavy dependencies on view internals to reduce coupling.
;;; Code:

(require 'context-navigator-i18n)
(require 'context-navigator-view-ui)

(defvar context-navigator-view--title-line-keymap nil
  "Keymap used by title-line segments (kept for compatibility).")

(defun context-navigator-view--title-line (header)
  "Return a one-line, clickable title rendering for HEADER with collapse/expand behavior.

- Adds an icon and a ▸/▾ indicator that reflects current collapse state.
- Installs a local keymap so mouse-1/TAB/RET toggle collapse.
- Marks the line with 'context-navigator-title so Navigator navigation recognizes it."
  (let* ((text
          (cond
           ((stringp header) header)
           ((and (consp header) (plistp header))
            (or (plist-get header :title)
                (plist-get header 'title)
                ""))
           ((and (consp header) (not (plistp header)))
            (let ((v (or (cdr (assoc 'title header))
                         (cdr (assoc :title header)))))
              (if (stringp v) v (format "%s" v))))
           (t
            (format "%s" header))))
         (arrow (if (and (boundp 'context-navigator-view--collapsed-p)
                         context-navigator-view--collapsed-p)
                    "▸" "▾"))
         (icon "🧭")
         (line (format "%s %s %s" arrow icon text))
         (s (copy-sequence line))
         ;; Prefer the shared keymap from view.el; if not yet bound, create one that inherits Navigator parent.
         (km (or context-navigator-view--title-line-keymap
                 (context-navigator-view-ui-make-keymap
                  #'context-navigator-view-toggle-collapse
                  (context-navigator-view-ui-parent-keymap)))))
    (add-text-properties 0 (length s)
                         (list 'context-navigator-title t
                               'context-navigator-header t
                               'context-navigator-interactive t
                               'mouse-face 'highlight
                               'help-echo (context-navigator-i18n :title-toggle-hint)
                               'keymap km
                               'local-map km)
                         s)
    s))

(defun context-navigator-view-toggle-collapse ()
  "Toggle collapse/expand for the Navigator buffer body and refresh."
  (interactive)
  (when (boundp 'context-navigator-view--collapsed-p)
    (setq context-navigator-view--collapsed-p (not context-navigator-view--collapsed-p)))
  ;; Ensure next render is not skipped by cache and schedule it.
  (when (boundp 'context-navigator-view--last-render-key)
    (setq context-navigator-view--last-render-key nil))
  (when (fboundp 'context-navigator-view--schedule-render)
    (context-navigator-view--schedule-render)))

(provide 'context-navigator-view-title)
;;; context-navigator-view-title.el ends here
