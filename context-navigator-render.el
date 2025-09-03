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

(defcustom context-navigator-render-show-path nil
  "Show item path on the right side when non-nil."
  :type 'boolean :group 'context-navigator-render)

(defcustom context-navigator-render-truncate-name 64
  "Max display length for item names."
  :type 'integer :group 'context-navigator-render)

(defcustom context-navigator-render-indicator-style 'text
  "Indicator style for item activity lamps:
- auto  : use icons when available, otherwise text bullets
- icons : force icon glyphs (requires all-the-icons); fallback to text if unavailable
- text  : use plain text bullets (●/○) with colors
- off   : hide indicators entirely"
  :type '(choice (const auto) (const icons) (const text) (const off))
  :group 'context-navigator-render)

(defvar-local context-navigator-render--last-hash nil)
(defvar context-navigator-render--gptel-keys nil
  "Optional list of stable keys of items currently present in gptel.
When non-nil, enables tri-state indicator in the sidebar:
- green  ● : item enabled and present in gptel
- gray   ○ : item disabled and absent in gptel
- yellow ● : mismatch between model and gptel (present when disabled, or absent when enabled).")

(defun context-navigator-render--truncate (s n)
  "Return S truncated to N chars with ellipsis."
  (if (and (stringp s) (> (length s) (max 4 n)))
      (concat (substring s 0 (- n 3)) "…")
    s))

(defun context-navigator-render--indicator (present enabled)
  "Return indicator string (or nil) for PRESENT/ENABLED, honoring style settings.
When style is 'off, return nil. When style is 'icons or 'auto with icon
provider available, return icon glyph. Otherwise return colored text bullet.

This function robustly falls back to a text bullet when an icon provider
is present but returns nil for the requested state.

The visual size and vertical alignment of text bullets are adjusted so the
indicator sits centered and appears smaller (half-size) relative to item text."
  (let ((style (or context-navigator-render-indicator-style 'auto)))
    (cond
     ((eq style 'off) nil)
     ((and (memq style '(icons auto))
           (fboundp 'context-navigator-icons-for-indicator))
      ;; Try to get icon; if provider returns nil or non-string, fall back to text bullet.
      (let ((icon (ignore-errors
                    (context-navigator-icons-for-indicator
                     (cond
                      ((and present enabled) 'ok)
                      ((not (eq present enabled)) 'mismatch)
                      (t 'absent))))))
        (if (and (stringp icon) (> (length icon) 0))
            icon
          ;; Fallback to a smaller text bullet, slightly raised to better vertically center.
          (let* ((raw (if (and (not present) (not enabled)) "○" "●"))
                 (color (cond
                         ((and present enabled) "green4")
                         ((not (eq present enabled)) "goldenrod2")
                         (t "gray"))))
            (propertize raw
                        'face (list :foreground color :height 0.55)
                        ;; Small raise to visually center the glyph relative to surrounding text.
                        'display '(raise 0.12))))))
     (t
      (let* ((raw (if (and (not present) (not enabled)) "○" "●"))
             (color (cond
                     ((and present enabled) "green4")
                     ((not (eq present enabled)) "goldenrod2")
                     (t "gray"))))
        (propertize raw
                    'face (list :foreground color :height 0.55)
                    'display '(raise 0.12)))))))

(defun context-navigator-render--left-column (state-icon icon name)
  "Build left column string from STATE-ICON, ICON and NAME."
  (string-trim
   (mapconcat #'identity
              (delq nil (list state-icon icon name))
              " ")))

(defun context-navigator-render--right-column (path)
  "Return right column string for PATH or nil when not shown."
  (when (and context-navigator-render-show-path
             (stringp path) (not (string-empty-p path)))
    (abbreviate-file-name path)))

(defun context-navigator-render--compose-line (left right left-width)
  "Compose final line from LEFT and RIGHT using LEFT-WIDTH."
  (if right
      (format (format "%%-%ds  %%s" (or left-width 48)) left right)
    left))

(defun context-navigator-render--propertize-line (line key item)
  "Return a copy of LINE with text properties for KEY and ITEM."
  (let ((s (copy-sequence line)))
    (add-text-properties 0 (length s)
                         (list 'context-navigator-key key
                               'context-navigator-item item)
                         s)
    s))

(defun context-navigator-render--format-line (item icon-fn &optional left-width)
  "Format a single ITEM into a propertized line string, using ICON-FN if non-nil.
When LEFT-WIDTH is non-nil, align the left column to that width.

Tri-state indicator (when gptel keys list is provided):
- green  ● : item enabled and present in gptel
- gray   ○ : item disabled and absent in gptel
- yellow ● : mismatch between model and gptel."
  (let* ((key (context-navigator-model-item-key item))
         (enabled (and (context-navigator-item-enabled item) t))
         (name (context-navigator-render--truncate
                (or (context-navigator-item-name item) "")
                context-navigator-render-truncate-name))
         (path (or (context-navigator-item-path item) ""))
         (have-keys (listp context-navigator-render--gptel-keys))
         (present (and have-keys (member key context-navigator-render--gptel-keys)))
         (state-icon (and have-keys
                          (context-navigator-render--indicator present enabled)))
         (icon (and (functionp icon-fn) (or (funcall icon-fn item) "")))
         (left (context-navigator-render--left-column
                state-icon
                (and (stringp icon) icon)
                name))
         (right (context-navigator-render--right-column path))
         (line (context-navigator-render--compose-line left right left-width)))
    (context-navigator-render--propertize-line line key item)))

(defun context-navigator-render-build-lines (items header &optional icon-fn left-width)
  "Return list of propertized lines for ITEMS with HEADER line first.
ICON-FN is a function (item -> string|nil) to decorate items.
When LEFT-WIDTH is non-nil, align left column to that width."
  (let* ((title (or header "Context"))
         (hl (propertize (format " %s" title) 'face 'mode-line-emphasis))
         ;; Use textual/full-width separator (box-drawing) for header separation. Make it unobtrusive.
         (sep (propertize (make-string (max 8 (min 120 (length hl))) ?─) 'face 'shadow)))
    (append (list hl sep)
            (mapcar (lambda (it)
                      (context-navigator-render--format-line it icon-fn left-width))
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
