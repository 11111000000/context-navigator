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

(defvar context-navigator-view--title-line-keymap nil
  "Keymap used by title-line segments (kept for compatibility).")

(defun context-navigator-view--title-line (header)
  "Return a one-line title rendering for HEADER.
HEADER can be:
- a string: returned as-is;
- a plist: tries :title (or 'title);
- an alist or other object: converted to string.

This shim is deliberately conservative: it should not error when
called from legacy callsites, and it provides a reasonable fallback
rendering until the full title rendering is centralized."
  (cond
   ((stringp header)
    header)
   ((and (consp header) (plistp header))
    (or (plist-get header :title)
        (plist-get header 'title)
        ""))
   ((and (consp header) (not (plistp header)))
    ;; alist-like: try assoc 'title or :title
    (let ((v (or (cdr (assoc 'title header))
                 (cdr (assoc :title header)))))
      (if (stringp v) v (format "%s" v))))
   (t
    (format "%s" header))))

(provide 'context-navigator-view-title)
;;; context-navigator-view-title.el ends here
