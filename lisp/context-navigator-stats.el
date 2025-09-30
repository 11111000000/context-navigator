;;; context-navigator-stats.el --- Stats for current context (footer block) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Collapsible "Stats" block for the items view:
;; - Counts per type (files/buffers/selections), total and enabled
;; - Size/cost: bytes and rough tokens for enabled and total
;; - Lazy remote mode by default (do not stat TRAMP)
;; - Small TTL cache; buffer-local expand state for the Navigator buffer
;; - Optional icons via all-the-icons (auto-detect)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)
(require 'context-navigator-icons)
(require 'context-navigator-i18n)
(require 'context-navigator-util)
(require 'context-navigator-view-segments)

;; Forward decls to avoid cycles
(declare-function context-navigator--state-get "context-navigator-core" ())
(declare-function context-navigator-state-items "context-navigator-core" (state))
(declare-function context-navigator-view--schedule-render "context-navigator-view" ())

(defgroup context-navigator-stats nil
  "Statistics footer for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-stats-enable t
  "When non-nil, show a collapsible Stats block under items."
  :type 'boolean :group 'context-navigator-stats)

(defcustom context-navigator-stats-bytes-per-token 4.0
  "Approximate bytes-per-token ratio for rough token cost estimation."
  :type 'number :group 'context-navigator-stats)

(defcustom context-navigator-stats-remote-mode 'lazy
  "How to treat remote/TRAMP paths when computing sizes:
- lazy   : do not stat TRAMP files; use live buffer sizes if available
- strict : stat TRAMP paths with file-attributes (may be slow)
- off    : ignore remote files when summing sizes"
  :type '(choice (const lazy) (const strict) (const off))
  :group 'context-navigator-stats)

(defcustom context-navigator-stats-cache-ttl 3.0
  "TTL (seconds) for cached stats computation."
  :type 'number :group 'context-navigator-stats)

(defcustom context-navigator-stats-icons-enable 'auto
  "Enable icons in the Stats block:
- auto : follow global icon setting and availability
- t/nil: force on/off"
  :type '(choice (const auto) (const t) (const nil))
  :group 'context-navigator-stats)

(defcustom context-navigator-stats-icon-height 0.9
  "Relative height for icons in the Stats footer."
  :type 'number :group 'context-navigator-stats)

(defvar-local context-navigator-stats--expanded-p nil
  "When non-nil, Stats block is expanded in the Navigator buffer.")

(defvar-local context-navigator-stats--cache nil
  "Cached stats: plist (:stamp float :data PLIST).
PLIST fields:
  :items :items-en
  :files :files-en
  :buffers :buffers-en
  :selections :selections-en
  :bytes :bytes-en
  :tokens :tokens-en")

;; Helpers --------------------------------------------------------------------



(defun context-navigator-stats--icons-enabled-p ()
  (pcase context-navigator-stats-icons-enable
    ('t t)
    ('nil nil)
    (_ (and (boundp 'context-navigator-enable-icons)
            context-navigator-enable-icons
            (fboundp 'all-the-icons-icon-for-file)))))

(defun context-navigator-stats--propertize-icon (str color)
  (when (stringp str)
    (propertize str
                'face (list :foreground color :height context-navigator-stats-icon-height)
                'display '(raise 0.11))))

(defvar context-navigator-stats--icon-cache (make-hash-table :test 'equal)
  "Cache for small icons used in the Stats block. Keyed by (KEY HEIGHT).")

(defun context-navigator-stats-clear-icon-cache ()
  "Clear cached Stats icons and schedule a view refresh."
  (interactive)
  (clrhash context-navigator-stats--icon-cache)
  (when (fboundp 'context-navigator-view--schedule-render)
    (context-navigator-view--schedule-render)))

;; Refresh Stats icons when theme changes (colors/fonts may differ)
(when (boundp 'after-enable-theme-functions)
  (add-hook 'after-enable-theme-functions
            (lambda (&rest _)
              (context-navigator-stats-clear-icon-cache))))

(defun context-navigator-stats--icon (key)
  "Return small icon string for a logical KEY or nil (cached).
All icon calls are wrapped in `ignore-errors' to avoid timer crashes
when an icon name is unavailable in the installed icon set."
  (when (context-navigator-stats--icons-enabled-p)
    (let* ((ck (list key context-navigator-stats-icon-height))
           (cached (gethash ck context-navigator-stats--icon-cache)))
      (or cached
          (let ((res
                 (pcase key
                   (:header
                    (or (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-material "bar_chart") "SteelBlue4"))
                        (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-material "assessment") "SteelBlue4"))
                        (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-faicon "bar-chart") "SteelBlue4"))))
                   (:counts
                    (or (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-material "view_list") "SlateGray"))
                        (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-octicon "list-unordered") "SlateGray"))))
                   (:size
                    (or (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-material "data_usage") "DarkGoldenrod4"))
                        (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-faicon "database") "DarkGoldenrod4"))))
                   (:tokens
                    (or (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-material "calculate") "DarkMagenta"))
                        (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-faicon "calculator") "DarkMagenta"))))
                   (:file
                    (or (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-material "insert_drive_file") "gray40"))
                        (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-faicon "file-o") "gray40"))))
                   (:buffer
                    (or (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-material "description") "SlateGray"))
                        (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-faicon "file-text-o") "SlateGray"))))
                   (:selection
                    (or (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-material "content_copy") "DarkSeaGreen4"))
                        (ignore-errors
                          (context-navigator-stats--propertize-icon
                           (all-the-icons-faicon "files-o") "DarkSeaGreen4"))))
                   (_ nil))))
            (when (stringp res)
              (puthash ck res context-navigator-stats--icon-cache))
            res)))))

(defun context-navigator-stats--item-bytes (it)
  "Best-effort byte size for item IT, respecting remote mode."
  (pcase (context-navigator-item-type it)
    ('selection
     (let ((b (context-navigator-item-beg it))
           (e (context-navigator-item-end it)))
       (if (and (integerp b) (integerp e)) (abs (- e b)) 0)))
    ('buffer
     (let ((buf (context-navigator-item-buffer it))
           (p   (context-navigator-item-path it)))
       (cond
        ((and (bufferp buf) (buffer-live-p buf))
         (with-current-buffer buf (buffer-size)))
        ((and (stringp p) (not (file-remote-p p)) (file-exists-p p))
         (let ((a (ignore-errors (file-attributes p 'string))))
           (or (and a (file-attribute-size a)) 0)))
        ((and (stringp p) (file-remote-p p))
         (pcase context-navigator-stats-remote-mode
           ('off 0)
           ('lazy 0)
           ('strict
            (let ((a (ignore-errors (file-attributes p 'string))))
              (or (and a (file-attribute-size a)) 0)))
           (_ 0)))
        (t 0))))
    ('file
     (let ((p (context-navigator-item-path it)))
       (cond
        ((and (stringp p) (file-remote-p p))
         (pcase context-navigator-stats-remote-mode
           ('off 0)
           ('lazy 0)
           ('strict
            (let ((a (ignore-errors (file-attributes p 'string))))
              (or (and a (file-attribute-size a)) 0)))
           (_ 0)))
        ((and (stringp p) (file-exists-p p))
         (let ((a (ignore-errors (file-attributes p 'string))))
           (or (and a (file-attribute-size a)) 0)))
        (t 0))))
    (_ 0)))

(defun context-navigator-stats--compute-now ()
  "Compute stats now; return plist."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (ignore-errors (context-navigator-state-items st))))
         (files 0) (buffers 0) (sels 0)
         (files-en 0) (buffers-en 0) (sels-en 0)
         (count 0) (count-en 0)
         (bytes 0) (bytes-en 0))
    (dolist (it (or items '()))
      (setq count (1+ count))
      (pcase (context-navigator-item-type it)
        ('file (setq files (1+ files)))
        ('buffer (setq buffers (1+ buffers)))
        ('selection (setq sels (1+ sels))))
      (when (context-navigator-item-enabled it)
        (setq count-en (1+ count-en))
        (pcase (context-navigator-item-type it)
          ('file (setq files-en (1+ files-en)))
          ('buffer (setq buffers-en (1+ buffers-en)))
          ('selection (setq sels-en (1+ sels-en)))))
      (let ((b (context-navigator-stats--item-bytes it)))
        (setq bytes (+ bytes (or b 0)))
        (when (context-navigator-item-enabled it)
          (setq bytes-en (+ bytes-en (or b 0))))))
    (let* ((k (max 1e-6 (or context-navigator-stats-bytes-per-token 4.0)))
           (tok (floor (/ (float bytes) k)))
           (tok-en (floor (/ (float bytes-en) k))))
      (list :items count :items-en count-en
            :files files :files-en files-en
            :buffers buffers :buffers-en buffers-en
            :selections sels :selections-en sels-en
            :bytes bytes :bytes-en bytes-en
            :tokens tok :tokens-en tok-en))))

(defun context-navigator-stats--get ()
  "Return cached stats or recompute when stale."
  (let* ((now (float-time))
         (ttl (or context-navigator-stats-cache-ttl 3.0))
         (cell context-navigator-stats--cache)
         (stamp (plist-get cell :stamp))
         (old (plist-get cell :data)))
    (if (and (numberp stamp) old (< (- now stamp) (max 0 ttl)))
        old
      (let ((pl (context-navigator-stats--compute-now)))
        (setq context-navigator-stats--cache (list :stamp now :data pl))
        pl))))

;; UI helpers ------------------------------------------------------------------



;; Public API -----------------------------------------------------------------

(defun context-navigator-stats-invalidate ()
  "Drop cached stats so they are recomputed on the next render."
  (interactive)
  (setq context-navigator-stats--cache nil)
  (when (fboundp 'context-navigator-view--schedule-render)
    (context-navigator-view--schedule-render)))

(defun context-navigator-stats-toggle ()
  "Toggle expand/collapse of the Stats block under items."
  (interactive)
  (setq context-navigator-stats--expanded-p (not context-navigator-stats--expanded-p))
  (when (fboundp 'context-navigator-view--schedule-render)
    (context-navigator-view--schedule-render)))

(defun context-navigator-stats-footer-lines (_total-width)
  "Return Stats footer lines for items view or nil."
  (when context-navigator-stats-enable
    (let* ((pl (context-navigator-stats--get))
           (en (plist-get pl :items-en))
           (b-en (plist-get pl :bytes-en))
           (t-en (plist-get pl :tokens-en))
           (b-all (plist-get pl :bytes))
           (t-all (plist-get pl :tokens))
           (files (plist-get pl :files))
           (buffers (plist-get pl :buffers))
           (sels (plist-get pl :selections))
           (files-en (plist-get pl :files-en))
           (buffers-en (plist-get pl :buffers-en))
           (sels-en (plist-get pl :selections-en))
           (arrow (if context-navigator-stats--expanded-p "▾" "▸"))
           (hdr-ico (or (context-navigator-stats--icon :header) ""))
           (lbl (context-navigator-i18n :stats))
           (hdr (format "%s %s %s: %d  ~%s  (~%d %s)"
                        arrow
                        hdr-ico
                        lbl
                        (or en 0)
                        (context-navigator-human-size b-en)
                        (or t-en 0)
                        (context-navigator-i18n :stats-tokens)))
           (s (copy-sequence hdr))
           (km (let ((m (make-sparse-keymap)))
                 (define-key m [mouse-1] #'context-navigator-stats-toggle)
                 ;; Make TAB on the stats header toggle expand/collapse (like the items title)
                 (define-key m (kbd "TAB")       #'context-navigator-stats-toggle)
                 (define-key m (kbd "<tab>")     #'context-navigator-stats-toggle)
                 (define-key m [tab]             #'context-navigator-stats-toggle)
                 (define-key m (kbd "C-i")       #'context-navigator-stats-toggle)
                 ;; Also support RET directly on the header segment
                 (define-key m (kbd "RET")       #'context-navigator-stats-toggle)
                 (define-key m (kbd "C-m")       #'context-navigator-stats-toggle)
                 (define-key m [return]          #'context-navigator-stats-toggle)
                 (define-key m (kbd "<return>")  #'context-navigator-stats-toggle)
                 m)))
      ;; Make header clickable
      (add-text-properties 0 (length s)
                           (list 'mouse-face 'highlight
                                 'help-echo (context-navigator-i18n :stats-toggle-hint)
                                 'keymap km 'local-map km
                                 'context-navigator-stats-toggle t
                                 'face 'mode-line-emphasis)
                           s)
      (if (not context-navigator-stats--expanded-p)
          (list s)
        (let* ((cnt-ico (or (context-navigator-stats--icon :counts) ""))
               (siz-ico (or (context-navigator-stats--icon :size) ""))
               (tok-ico (or (context-navigator-stats--icon :tokens) ""))
               (ico-file (or (context-navigator-stats--icon :file) ""))
               (ico-buf (or (context-navigator-stats--icon :buffer) ""))
               (ico-sel (or (context-navigator-stats--icon :selection) ""))
               (row1 (format "   %s %s: %s %d (%d), %s %d (%d), %s %d (%d)"
                             cnt-ico
                             (context-navigator-i18n :stats-counts)
                             ico-file files files-en
                             ico-buf buffers buffers-en
                             ico-sel sels sels-en))
               (row2 (format "   %s %s: %s ~%s  /  %s ~%s"
                             siz-ico
                             (context-navigator-i18n :stats-size)
                             (context-navigator-i18n :enabled) (context-navigator-human-size b-en)
                             (context-navigator-i18n :total) (context-navigator-human-size b-all)))
               (row3 (format "   %s %s: %s %d  /  %s %d"
                             tok-ico
                             (context-navigator-i18n :stats-tokens)
                             (context-navigator-i18n :enabled) (or t-en 0)
                             (context-navigator-i18n :total) (or t-all 0))))
          (list s row1 row2 row3))))))

(defun context-navigator-stats-interactive-lines (total-width &optional parent-map)
  "Return Stats block lines wrapped with interactive props/keymaps.
This is a thin wrapper around `context-navigator-stats-footer-lines' that:
- marks the header as interactive with a unified keymap (mouse/TAB/RET),
- marks content lines with 'context-navigator-stats-line,
- inherits key bindings from PARENT-MAP when provided.

TOTAL-WIDTH is forwarded to `context-navigator-stats-footer-lines'."
  (let ((raw (condition-case nil
                 (context-navigator-stats-footer-lines total-width)
               (error nil))))
    (when (and (listp raw) raw)
      (let ((first t))
        (mapcar
         (lambda (s)
           (let ((str (copy-sequence s)))
             (when first
               (setq first nil)
               (let ((km (ignore-errors
                           (context-navigator-view-ui-make-keymap
                            #'context-navigator-stats-toggle parent-map))))
                 (when (keymapp km)
                   (add-text-properties
                    0 (length str)
                    (list
                     'mouse-face 'highlight
                     'help-echo (context-navigator-i18n :stats-toggle-hint)
                     'context-navigator-stats-toggle t
                     'context-navigator-header t
                     'context-navigator-interactive t
                     'keymap km
                     'local-map km)
                    str))))
             (add-text-properties 0 (length str)
                                  (list 'context-navigator-stats-line t)
                                  str)
             str))
         raw)))))

(provide 'context-navigator-stats)
;;; context-navigator-stats.el ends here
