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
(require 'context-navigator-log)

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

(defface context-navigator-disabled-face
  '((t :foreground "gray55"))
  "Face used to render disabled items in the sidebar."
  :group 'context-navigator-render)

(defvar-local context-navigator-render--last-hash nil)
(defvar context-navigator-render--gptel-keys nil
  "Optional list of stable keys of items currently present in gptel.
When non-nil, shows a binary indicator in the sidebar:
- green  ● : item present in gptel
- gray   ○ : item absent in gptel")

(defun context-navigator-render--truncate (s n)
  "Return S truncated to N chars with ellipsis."
  (if (and (stringp s) (> (length s) (max 4 n)))
      (concat (substring s 0 (- n 3)) "…")
    s))

(defun context-navigator-render--indicator (present)
  "Return indicator string (or nil) based only on PRESENT in gptel.
- present → green ●
- absent  → gray  ○

When style is 'off, return nil. When style is 'icons or 'auto with icon
provider available, return icon glyph; otherwise return colored text bullet.

The visual size and vertical alignment of text bullets are adjusted so the
indicator sits centered and appears moderately large relative to item text."
  (let ((style (or context-navigator-render-indicator-style 'auto)))
    (cond
     ((eq style 'off) nil)
     ((and (memq style '(icons auto))
           (fboundp 'context-navigator-icons-for-indicator))
      (let* ((state (if present 'ok 'absent))
             (icon (ignore-errors (context-navigator-icons-for-indicator state))))
        (if (and (stringp icon) (> (length icon) 0))
            icon
          (let* ((raw (if present "●" "○"))
                 (color (if present "green4" "gray")))
            (propertize raw
                        'face (list :foreground color :height 0.75)
                        'display '(raise 0.08))))))
     (t
      (let* ((raw (if present "●" "○"))
             (color (if present "green4" "gray")))
        (propertize raw
                    'face (list :foreground color :height 0.75)
                    'display '(raise 0.08)))))))

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

Binary indicator:
- green  ● : item present in gptel (when keys snapshot says so)
- gray   ○ : item absent in gptel or when the keys snapshot is unknown.
Disabled items are rendered with a subdued face (only the name). Indicator reflects actual gptel presence (green when present, gray otherwise)."
  (let* ((key (context-navigator-model-item-key item))
         (name (context-navigator-render--truncate
                (or (context-navigator-item-name item) "")
                context-navigator-render-truncate-name))
         (path (or (context-navigator-item-path item) ""))
         (enabled (not (null (context-navigator-item-enabled item))))
         (keys-list context-navigator-render--gptel-keys)
         ;; Reflect actual presence in gptel regardless of enabled flag.
         (present (and keys-list (member key keys-list)))
         ;; Always render an indicator; when PRESENT is nil it will be gray
         (state-icon (context-navigator-render--indicator present))
         ;; Trace why indicator is green/gray in this render row
         (ignore-errors
           (context-navigator-debug :trace :render
                                    "indicator key=%s enabled=%s present=%s keys=%d"
                                    key enabled (and present t)
                                    (length (or keys-list '()))))
         (icon (and (functionp icon-fn) (or (funcall icon-fn item) "")))
         ;; Dim only the name for disabled items; do not touch indicator/icon.
         (name-prop (if enabled
                        name
                      (propertize name 'face 'context-navigator-disabled-face)))
         (left (context-navigator-render--left-column
                state-icon
                (and (stringp icon) icon)
                name-prop))
         (right (context-navigator-render--right-column path))
         ;; Add a small left padding to each item line
         (line (context-navigator-render--compose-line (concat " " left) right left-width)))
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
  "Replace BUFFER text with LINES when different, preserving point/column and window start best-effort.

This function now:
- preserves the exact column within the restored line
- restores the point for the window showing BUFFER (window-point), not only the buffer's point
so that re-renders triggered by timers do not cause the cursor to jump to BOL or to the top."
  (with-current-buffer buffer
    (let* ((inhibit-read-only t)
           (win (get-buffer-window buffer 'visible))
           ;; Capture point/line/column relative to the window that shows the buffer
           (old-point (if (window-live-p win) (window-point win) (point)))
           (old-line  (save-excursion (goto-char old-point) (line-number-at-pos)))
           (old-col   (save-excursion (goto-char old-point) (current-column)))
           (old-start (and (window-live-p win) (window-start win)))
           ;; Compute plain text for hashing (strip properties) to avoid false misses
           (text-plain (concat
                        (mapconcat (lambda (s)
                                     (if (stringp s) (substring-no-properties s) ""))
                                   lines
                                   "\n")
                        "\n"))
           (new-hash  (sxhash-equal text-plain))
           (changed   (not (equal context-navigator-render--last-hash new-hash))))
      (when changed
        (erase-buffer)
        ;; Insert each line separately to preserve text properties (keymaps, mouse-face, etc.)
        (dolist (ln lines)
          (when (stringp ln) (insert ln))
          (insert "\n"))
        (setq context-navigator-render--last-hash new-hash)
        ;; Compute target position from saved line/column
        (let ((target
               (save-excursion
                 (goto-char (point-min))
                 (forward-line (1- (max 1 old-line)))
                 (move-to-column (max 0 old-col))
                 (point))))
          ;; Restore buffer point (for the selected window) and the sidebar window's point
          (goto-char target)
          (when (window-live-p win)
            (set-window-point win target))
          ;; Restore window-start for smooth scrolling restoration
          (when (and (window-live-p win) old-start)
            (set-window-start win old-start t)))))))

(provide 'context-navigator-render)
;;; context-navigator-render.el ends here
