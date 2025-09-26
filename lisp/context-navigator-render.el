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

(defcustom context-navigator-render-path-prefix-mode 'short
  "Prefix mode before item names:
- off      : do not show path prefix
- short    : show abbreviated path per directory (minimal unique prefixes); dotdirs start with 2 chars
- relative : full relative directory path to project root
- full     : full absolute directory path"
  :type '(choice (const off) (const short) (const relative) (const full))
  :group 'context-navigator-render)

(defcustom context-navigator-render-short-prefix-cutoff 800
  "When the ITEMS count exceeds this number and path prefix mode is 'short,
automatically degrade to 'relative to avoid expensive unique-prefix computation.
Set to nil to disable auto-degrade."
  :type 'integer :group 'context-navigator-render)

(defcustom context-navigator-render-prefix-dim-face 'shadow
  "Face used to render path prefix before item names."
  :type 'face :group 'context-navigator-render)

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

(defvar-local context-navigator-render--prefix-getter nil
  "Buffer-local function (path -> prefix-string) used during a single render pass.")

(defvar-local context-navigator-render--short-prefix-cache nil
  "Buffer-local cache for short prefix map across renders.
Plist: (:key KEY :map HASH), where
  KEY = (ROOT . ITEMS-HASH)
  ROOT is expanded project root (or nil),
  ITEMS-HASH is an sxhash over the list of absolute item paths.")

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
                               'context-navigator-item item
                               'context-navigator-interactive t)
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
         (raw-name (or (context-navigator-item-name item) ""))
         (name (context-navigator-render--truncate raw-name
                                                   context-navigator-render-truncate-name))
         (path (or (context-navigator-item-path item) ""))
         (enabled (not (null (context-navigator-item-enabled item))))
         (keys-list context-navigator-render--gptel-keys)
         ;; Reflect actual presence in gptel regardless of enabled flag.
         (present (and keys-list (member key keys-list)))
         ;; Always render an indicator; when PRESENT is nil it will be gray
         (state-icon (context-navigator-render--indicator present))
         (icon (and (functionp icon-fn) (or (funcall icon-fn item) "")))
         ;; Build prefix via buffer-local getter when available
         (prefix (cond
                  ((and (stringp path) (not (string-empty-p path))
                        (functionp context-navigator-render--prefix-getter))
                   (funcall context-navigator-render--prefix-getter path))
                  (t "")))
         (prefix-prop (if (and (stringp prefix) (> (length prefix) 0))
                          (propertize prefix 'face context-navigator-render-prefix-dim-face)
                        ""))
         ;; Dim only the name for disabled items; do not touch indicator/icon or prefix.
         (name-prop (if enabled
                        name
                      (propertize name 'face 'context-navigator-disabled-face)))
         (visible-name (concat prefix-prop name-prop))
         (left (context-navigator-render--left-column
                state-icon
                (and (stringp icon) icon)
                visible-name))
         (right (context-navigator-render--right-column path))
         ;; Add a small left padding to each item line
         (line (context-navigator-render--compose-line (concat " " left) right left-width)))
    (context-navigator-render--propertize-line line key item)))

(defun context-navigator-render--header-lines (header)
  "Build header title and separator lines."
  (let* ((title (or header "Context"))
         (hl (propertize (format " %s" title) 'face 'mode-line-emphasis))
         (sep (propertize (make-string (max 8 (min 120 (length hl))) ?─) 'face 'shadow)))
    (list hl sep)))

(defun context-navigator-render--state-root ()
  "Return last known project root from state, or nil."
  (let ((st (and (fboundp 'context-navigator--state-get)
                 (ignore-errors (context-navigator--state-get)))))
    (and st
         (fboundp 'context-navigator-state-last-project-root)
         (ignore-errors (context-navigator-state-last-project-root st)))))

(defun context-navigator-render--relpath (p root)
  "Return file-relative-name of P to ROOT, or P on failure."
  (condition-case _err
      (if (and (stringp root) (stringp p))
          (file-relative-name (expand-file-name p)
                              (file-name-as-directory (expand-file-name root)))
        p)
    (error p)))

(defun context-navigator-render--split-relpath (rel)
  "Split REL into (DIRS . BASENAME). DIRS is a list of directories."
  (if (and (stringp rel) (not (string-empty-p rel)))
      (let* ((dir (file-name-directory rel))
             (base (file-name-nondirectory rel))
             (dirs (and dir
                        (split-string (directory-file-name dir) "/" t))))
        (cons dirs base))
    (cons nil (or rel ""))))

(defun context-navigator-render--minimal-prefix-map (names)
  "Compute minimal unique prefixes among sibling NAMES.
Return an alist of (name . minimal-prefix)."
  ;; Fast algorithm: sort, then use LCP with neighbors to decide minimal unique length.
  (let* ((uniq (delete-dups (cl-copy-list names)))
         (n (length uniq)))
    (cond
     ((= n 0) nil)
     ((= n 1)
      (let* ((s (car uniq))
             (need (if (and (stringp s) (> (length s) 0)
                            (eq (aref s 0) ?.))
                       (min 2 (length s))
                     (min 1 (length s)))))
        (list (cons s (substring s 0 need)))))
     (t
      (let* ((vec (vconcat (cl-sort uniq #'string-lessp)))
             (res nil))
        (cl-labels
            ((lcp (a b)
               (let* ((la (length a)) (lb (length b))
                      (lim (min la lb))
                      (i 0))
                 (while (and (< i lim)
                             (eq (aref a i) (aref b i)))
                   (setq i (1+ i)))
                 i))
             (min-start (s)
               (if (and (stringp s) (> (length s) 0)
                        (eq (aref s 0) ?.))
                   (min 2 (length s))
                 (min 1 (length s)))))
          (let ((len (length vec)))
            (dotimes (i len)
              (let* ((s (aref vec i))
                     (lprev (if (> i 0) (lcp s (aref vec (1- i))) 0))
                     (lnext (if (< i (1- len)) (lcp s (aref vec (1+ i))) 0))
                     (need (1+ (max lprev lnext)))
                     (need (max need (min-start s)))
                     (need (min need (length s))))
                (push (cons s (substring s 0 need)) res))))
          res))))))

(defun context-navigator-render--build-short-prefix-map (items root)
  "Return hash: abs-path -> short directory prefix \"a/b/\" relative to ROOT.
Items with paths outside ROOT get an empty prefix.

Optimized: results are cached buffer-locally across renders keyed by (ROOT . ITEMS-HASH)."
  (let* ((root-key (and (stringp root) (not (string-empty-p root))
                        (expand-file-name root)))
         (paths (delq nil
                      (mapcar (lambda (it)
                                (let ((p (context-navigator-item-path it)))
                                  (and (stringp p) (not (string-empty-p p)) p)))
                              items)))
         (items-key (sxhash-equal paths))
         (key (cons root-key items-key))
         (cached (and (listp context-navigator-render--short-prefix-cache)
                      (equal (plist-get context-navigator-render--short-prefix-cache :key) key)
                      (plist-get context-navigator-render--short-prefix-cache :map))))
    (or cached
        (let* ((children-by-parent (make-hash-table :test 'equal)) ; parent (list dirs) -> child-set (hash)
               (acc  (make-hash-table :test 'equal)))
          ;; Collect unique children per parent-dir-list key
          (dolist (it items)
            (let ((p (context-navigator-item-path it)))
              (when (and (stringp p) (not (string-empty-p p)))
                (let ((rel (context-navigator-render--relpath p root)))
                  (when (not (string-prefix-p ".." rel))
                    (let* ((dirs+base (context-navigator-render--split-relpath rel))
                           (dirs (car dirs+base)))
                      (when dirs
                        (let ((i 0) (len (length dirs)))
                          (while (< i len)
                            (let* ((parent (cl-subseq dirs 0 i))
                                   (child  (nth i dirs))
                                   (set    (gethash parent children-by-parent)))
                              (unless set
                                (setq set (make-hash-table :test 'equal))
                                (puthash parent set children-by-parent))
                              (puthash child t set))
                            (setq i (1+ i)))))))))))
          ;; For each parent, compute minimal prefixes map
          (let ((prefix-per-parent (make-hash-table :test 'equal)))
            (maphash
             (lambda (parent child-set)
               (let (children)
                 (maphash (lambda (k _v) (push k children)) child-set)
                 (puthash parent (context-navigator-render--minimal-prefix-map children)
                          prefix-per-parent)))
             children-by-parent)
            ;; Compute prefix for each item path
            (dolist (it items)
              (let ((p (context-navigator-item-path it)))
                (when (and (stringp p) (not (string-empty-p p)))
                  (let ((rel (context-navigator-render--relpath p root)))
                    (if (string-prefix-p ".." rel)
                        (puthash p "" acc)
                      (let* ((dirs (car (context-navigator-render--split-relpath rel)))
                             (parts nil)
                             (i 0)
                             (len (length (or dirs '()))))
                        (while (< i len)
                          (let* ((parent (cl-subseq dirs 0 i))
                                 (child  (nth i dirs))
                                 (mp     (gethash parent prefix-per-parent))
                                 (pref   (or (cdr (assoc child mp)) child)))
                            (push pref parts))
                          (setq i (1+ i)))
                        (puthash p (if parts
                                       (concat (mapconcat #'identity (nreverse parts) "/") "/")
                                     "")
                                 acc)))))))
            ;; Save cache
            (setq context-navigator-render--short-prefix-cache
                  (list :key key :map acc))
            acc)))))

(defun context-navigator-render--relative-prefix (p root)
  "Return directory prefix relative to ROOT for path P, or empty string."
  (let* ((rel (context-navigator-render--relpath p root))
         (dir (file-name-directory rel)))
    (if (and dir (> (length dir) 0))
        dir
      "")))

(defun context-navigator-render--full-prefix (p)
  "Return absolute directory for P (with trailing slash trimmed by caller)."
  (let ((dir (file-name-directory (expand-file-name p))))
    (or dir "")))

(defun context-navigator-render--make-prefix-getter (mode root items)
  "Return a function that maps absolute path -> prefix string for MODE."
  (let ((short-map (when (eq mode 'short)
                     (context-navigator-render--build-short-prefix-map items root))))
    (pcase mode
      ('off (lambda (_p) ""))
      ('short (lambda (p) (or (and short-map (gethash p short-map)) "")))
      ('relative (lambda (p) (context-navigator-render--relative-prefix p root)))
      ('full (lambda (p)
               (let ((d (context-navigator-render--full-prefix p)))
                 (if (and d (> (length d) 0))
                     (file-name-as-directory d)
                   ""))))
      (_ (lambda (_p) "")))))

(defun context-navigator-render-build-lines (items header &optional icon-fn left-width)
  "Return list of propertized lines for ITEMS with HEADER line first.
ICON-FN is a function (item -> string|nil) to decorate items.
When LEFT-WIDTH is non-nil, align left column to that width."
  (let* ((header-lines (context-navigator-render--header-lines header))
         (root (context-navigator-render--state-root))
         (n (length (or items '())))
         (configured (or context-navigator-render-path-prefix-mode 'short))
         (mode (if (and (eq configured 'short)
                        (integerp context-navigator-render-short-prefix-cutoff)
                        (> n context-navigator-render-short-prefix-cutoff))
                   'relative
                 configured))
         ;; Install getter for this render pass
         (context-navigator-render--prefix-getter
          (context-navigator-render--make-prefix-getter mode root items)))
    (append header-lines
            (mapcar (lambda (it)
                      (context-navigator-render--format-line it icon-fn left-width))
                    items))))

(defun context-navigator-render-build-item-lines (items &optional icon-fn left-width)
  "Return list of propertized lines for ITEMS only (no header/separator).
ICON-FN is a function (item -> string|nil) to decorate items.
When LEFT-WIDTH is non-nil, align left column to that width.

This is identical to `context-navigator-render-build-lines' minus the header
construction, so callers that render their own titles can avoid building and
discarding header/separator lines."
  (let* ((root (context-navigator-render--state-root))
         (n (length (or items '())))
         (configured (or context-navigator-render-path-prefix-mode 'short))
         (mode (if (and (eq configured 'short)
                        (integerp context-navigator-render-short-prefix-cutoff)
                        (> n context-navigator-render-short-prefix-cutoff))
                   'relative
                 configured))
         ;; Install getter for this render pass
         (context-navigator-render--prefix-getter
          (context-navigator-render--make-prefix-getter mode root items)))
    (mapcar (lambda (it)
              (context-navigator-render--format-line it icon-fn left-width))
            items)))

(defun context-navigator-render-apply-to-buffer (buffer lines)
  "Replace BUFFER text with LINES when different, preserving point/column, mark and window start best-effort.

This function now:
- preserves the exact column within the restored line
- restores the point for the window showing BUFFER (window-point), not only the buffer's point
- preserves the active region (mark position and mark-active) across re-renders
so that re-renders triggered by timers do not cause the cursor/selection to jump."
  (with-current-buffer buffer
    (let* ((inhibit-read-only t)
           (win (get-buffer-window buffer 'visible))
           ;; Capture point/line/column relative to the window that shows the buffer
           (old-point (if (window-live-p win) (window-point win) (point)))
           (old-line  (save-excursion (goto-char old-point) (line-number-at-pos)))
           (old-col   (save-excursion (goto-char old-point) (current-column)))
           (old-start (and (window-live-p win) (window-start win)))
           ;; Also capture mark/region so selection works normally across renders
           (old-mark (mark t))
           (old-mark-line (when old-mark (save-excursion (goto-char old-mark) (line-number-at-pos))))
           (old-mark-col  (when old-mark (save-excursion (goto-char old-mark) (current-column))))
           (old-mark-active mark-active)
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
        (let* ((target
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- (max 1 old-line)))
                  (move-to-column (max 0 old-col))
                  (point)))
               ;; Compute restored mark position (line/col) if previously set
               (mark-target
                (when old-mark
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- (max 1 (or old-mark-line 1))))
                    (move-to-column (max 0 (or old-mark-col 0)))
                    (point)))))
          ;; Restore buffer point (for the selected window) and the sidebar window's point
          (goto-char target)
          (when (window-live-p win)
            (set-window-point win target))
          ;; Restore mark (and active state) to keep user's selection intact
          (when old-mark
            (set-marker (mark-marker) (or mark-target (point-min)) (current-buffer))
            (setq mark-active old-mark-active))
          ;; Restore window-start for smooth scrolling restoration
          (when (and (window-live-p win) old-start)
            (set-window-start win old-start t)))))))

(provide 'context-navigator-render)
;;; context-navigator-render.el ends here
