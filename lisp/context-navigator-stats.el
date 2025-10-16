;;; context-navigator-stats.el --- Stats for current context (split-only) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Split-only Stats:
;; - Compact 5-line summary in a dedicated split below the Navigator
;; - Items and Multi-group (aggregate) supported
;; - Inline footer removed; this module only provides compute + icons
;; - Lazy remote mode by default (do not stat TRAMP) and small TTL cache

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)
(require 'context-navigator-icons)
(require 'context-navigator-i18n)
(require 'context-navigator-util)

;; Forward decls to avoid cycles
(declare-function context-navigator--state-get "context-navigator-core" ())
(declare-function context-navigator-state-items "context-navigator-core" (state))
(declare-function context-navigator-view--schedule-render "context-navigator-view" ())

(defgroup context-navigator-stats nil
  "Statistics footer for Context Navigator."
  :group 'context-navigator)



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
    ('t (display-graphic-p))
    ('nil nil)
    (_ (and (display-graphic-p)
            (boundp 'context-navigator-enable-icons)
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

(defun context-navigator-stats--compute-from-items (items)
  "Compute stats from ITEMS list; return plist."
  (let ((files 0) (buffers 0) (sels 0)
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

(defun context-navigator-stats--compute-now ()
  "Compute stats now (current state items); return plist."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (ignore-errors (context-navigator-state-items st)))))
    (context-navigator-stats--compute-from-items items)))

(defun context-navigator-stats--get ()
  "Return cached stats (respecting MG) or recompute/launch async aggregate when needed.

In Multi-group mode with a non-empty selection, compute aggregated stats across
selected groups asynchronously and cache them. While the computation is pending,
return the previous cache (or a fallback based on current group's items)."
  (let* ((now (float-time))
         (ttl (or context-navigator-stats-cache-ttl 3.0))
         (st   (ignore-errors (context-navigator--state-get)))
         (root (and st (ignore-errors (context-navigator-state-last-project-root st))))
         (ps   (and root (ignore-errors (context-navigator-persist-state-load root))))
         (mg   (and (listp ps) (plist-member ps :multi) (plist-get ps :multi)))
         (sel  (and mg (plist-get ps :selected)))
         (cell context-navigator-stats--cache)
         (stamp (plist-get cell :stamp))
         (cache-mode (plist-get cell :mode))
         (cache-sel  (plist-get cell :sel))
         (cache-root (plist-get cell :root))
         (old (plist-get cell :data)))
    (cond
     ;; MG aggregate path
     ((and mg (listp sel) (> (length sel) 0))
      (let* ((fresh (and (numberp stamp)
                         old
                         (eq cache-mode 'mg)
                         (equal cache-root root)
                         (equal cache-sel sel)
                         (< (- now stamp) (max 0 ttl)))))
        (unless fresh
          (when (fboundp 'context-navigator-collect-items-for-groups-async)
            (context-navigator-collect-items-for-groups-async
             root sel
             (lambda (items)
               (let ((pl (context-navigator-stats--compute-from-items items)))
                 ;; enrich with MG meta for UI
                 (let ((pl* (append pl (list :mg t :groups (length sel)))))
                   (setq context-navigator-stats--cache
                         (list :stamp (float-time) :data pl* :mode 'mg :sel sel :root root))
                   (when (fboundp 'context-navigator-view--schedule-render)
                     (context-navigator-view--schedule-render))))))))
        (or (and old old)
            (let ((pl (context-navigator-stats--compute-now)))
              (setq context-navigator-stats--cache
                    (list :stamp now
                          :data (append pl (list :mg t :groups (length sel)))
                          :mode 'mg :sel sel :root root))
              (plist-get context-navigator-stats--cache :data)))))
     ;; Non-MG: use current group items
     (t
      (if (and (numberp stamp) old (< (- now stamp) (max 0 ttl)) (not (eq cache-mode 'mg)))
          old
        (let* ((pl (context-navigator-stats--compute-now))
               (pl* (append pl (list :mg nil :groups 1))))
          (setq context-navigator-stats--cache (list :stamp now :data pl* :mode 'single :root root))
          pl*))))))

;; UI helpers ------------------------------------------------------------------



;; Public API -----------------------------------------------------------------

(defun context-navigator-stats-invalidate ()
  "Drop cached stats so they are recomputed on the next render."
  (interactive)
  (setq context-navigator-stats--cache nil)
  (when (fboundp 'context-navigator-view--schedule-render)
    (context-navigator-view--schedule-render)))







;; Compatibility: inline Stats footer is removed; keep no-op stubs/redirects.

(defun context-navigator-stats-footer-lines (_total-width)
  "Inline Stats footer removed; return empty list."
  '())

(defun context-navigator-stats-interactive-lines (_total-width &optional _parent-map)
  "Inline Stats footer removed; return empty list."
  '())

(defun context-navigator-stats-toggle ()
  "Open/close the 5-line Stats split below the Navigator."
  (interactive)
  (when (fboundp 'context-navigator-stats-split-toggle)
    (context-navigator-stats-split-toggle)))

(provide 'context-navigator-stats)
;;; context-navigator-stats.el ends here
