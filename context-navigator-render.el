;;; context-navigator-render.el --- Pure render helpers (vtree/lines) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Functional rendering utilities:
;; - Build list of propertized lines from items and a header
;; - Apply lines to a buffer preserving point and window-start best-effort
;;
;; This module is pure wrt data processing; IO is limited to buffer writes
;; performed in a single function that receives the target buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)

(defgroup context-navigator-render nil
  "Rendering settings for context-navigator."
  :group 'context-navigator)

(defcustom context-navigator-render-show-path t
  "Show item path on the right side when non-nil."
  :type 'boolean :group 'context-navigator-render)

(defcustom context-navigator-render-truncate-name 64
  "Max display length for item names."
  :type 'integer :group 'context-navigator-render)

(defvar-local context-navigator-render--last-hash nil)

(defun context-navigator-render--truncate (s n)
  "Return S truncated to N chars with ellipsis."
  (if (and (stringp s) (> (length s) (max 4 n)))
      (concat (substring s 0 (- n 3)) "…")
    s))

(defun context-navigator-render--format-line (item icon-fn &optional left-width)
  "Format a single ITEM into a propertized line string, using ICON-FN if non-nil.
The returned string carries the text property 'context-navigator-key and
'context-navigator-item for later retrieval at point.

Render a circular state icon (green for enabled, gray for disabled) so the
state marker is more visible than the small checkbox glyph previously used.

When LEFT-WIDTH is non-nil, align the left column to that width."
  (let* ((key (context-navigator-model-item-key item))
         (enabled (and (context-navigator-item-enabled item) t))
         (name (context-navigator-render--truncate
                (or (context-navigator-item-name item) "") context-navigator-render-truncate-name))
         (path (or (context-navigator-item-path item) ""))
         (raw-icon "●")
         (color (if enabled "green4" "gray"))
         (state-icon (propertize raw-icon 'face (list :foreground color :height 1.15)))
         (icon (and (functionp icon-fn) (or (funcall icon-fn item) "")))
         (col-left (string-trim (mapconcat #'identity
                                           (delq nil (list state-icon
                                                           (and (stringp icon) icon)
                                                           name))
                                           " ")))
         (col-right (and context-navigator-render-show-path
                         (stringp path) (not (string-empty-p path))
                         (abbreviate-file-name path)))
         (lw (or left-width 48))
         (line (if col-right
                   (format (format "%%-%ds  %%s" lw) col-left col-right)
                 col-left)))
    ;; Ensure we return a copy so adding text properties doesn't mutate shared strings
    (let ((s (copy-sequence line)))
      (add-text-properties 0 (length s)
                           (list 'context-navigator-key key
                                 'context-navigator-item item)
                           s)
      s)))

(defun context-navigator-render-build-lines (items header &optional icon-fn left-width)
  "Return list of propertized lines for ITEMS with HEADER line first.
ICON-FN is a function (item -> string|nil) to decorate items.
When LEFT-WIDTH is non-nil, align left column to that width."
  (let* ((title (or header "Context"))
         (hl (propertize (format " %s" title) 'face 'mode-line-emphasis))
         (sep (make-string (max 8 (min 120 (length hl))) ?-)))
    (append (list hl sep)
            (mapcar (lambda (it) (context-navigator-render--format-line it icon-fn left-width))
                    items))))

(defun context-navigator-render-apply-to-buffer (buffer lines)
  "Replace BUFFER text with LINES when different, preserving point and window start best-effort."
  (with-current-buffer buffer
    (let* ((inhibit-read-only t)
           (old-point (point))
           (old-line (line-number-at-pos))
           (win (get-buffer-window buffer 'visible))
           (old-start (and win (window-start win)))
           (new-text (concat (mapconcat #'identity lines "\n") "\n"))
           (new-hash (sxhash-equal new-text))
           (changed (not (equal context-navigator-render--last-hash new-hash))))
      (when changed
        (erase-buffer)
        (insert new-text)
        (setq context-navigator-render--last-hash new-hash)
        (goto-char (point-min))
        ;; Restore position: prefer same line number
        (forward-line (1- (max 1 old-line)))
        (when (and win old-start)
          (set-window-start win old-start t))))))

(provide 'context-navigator-render)
;;; context-navigator-render.el ends here
