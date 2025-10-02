;;; context-navigator-view-pinned-title.el --- Pinned title under headerline -*- lexical-binding: t; -*-

;; Optional posframe-based pinned title under the header-line.
;; Fallback: inline first line (scrolls) when posframe is unavailable.

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-core)
(require 'context-navigator-i18n)
(require 'context-navigator-icons)

(defgroup context-navigator-pinned-title nil
  "Pinned title (project/group) under header-line for Navigator."
  :group 'context-navigator)

(defcustom context-navigator-pinned-title-enable t
  "When non-nil, show a pinned title under the header-line."
  :type 'boolean :group 'context-navigator-pinned-title)

(defcustom context-navigator-pinned-title-use-posframe 'auto
  "Use posframe for the pinned title:
- auto: use when posframe is available; otherwise fallback inline
- t   : force posframe (error when missing)
- nil : never use posframe, always fallback inline"
  :type '(choice (const auto) (const t) (const nil))
  :group 'context-navigator-pinned-title)

(defcustom context-navigator-pinned-title-y-offset 0
  "Pixel Y offset for the posframe (tweak to sit below the header-line)."
  :type 'integer :group 'context-navigator-pinned-title)

(defface context-navigator-pinned-title-face
  '((t :inherit mode-line-emphasis))
  "Face for pinned title text."
  :group 'context-navigator-pinned-title)

(defvar-local context-navigator--pintitle-on nil)
(defvar-local context-navigator--pintitle-frame nil)

(defcustom context-navigator-pinned-title-left-padding 2
  "Number of space characters to prefix to the pinned title text."
  :type 'integer :group 'context-navigator-pinned-title)

;; Custom poshandler: place at the window text area's top-left (below header-line).
(defun context-navigator-pinned-title--poshandler-below-headerline (info)
  "Return pixel position cons (X . Y) below header-line of parent window."
  (let* ((win (plist-get info :parent-window))
         ;; Inside pixel edges start at the top-left of the text area (below header-line/tab-line)
         (edges (window-inside-pixel-edges win))
         (x (nth 0 edges))
         (y (nth 1 edges)))
    (cons x y)))

(defun context-navigator-pinned-title--want-posframe-p ()
  (and context-navigator-pinned-title-enable
       (not (eq context-navigator-pinned-title-use-posframe nil))
       (require 'posframe nil t)
       (or (eq context-navigator-pinned-title-use-posframe 'auto)
           (eq context-navigator-pinned-title-use-posframe t))))

(defun context-navigator-pinned-title--mode ()
  (and (boundp 'context-navigator-view--mode) context-navigator-view--mode))

(defun context-navigator-pinned-title--compute ()
  "Return a propertized title string with icons: [project[: group]]."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (slug (and st (context-navigator-state-current-group-slug st)))
         (proj (if (and (stringp root) (not (string-empty-p root)))
                   (file-name-nondirectory (directory-file-name root))
                 (context-navigator-i18n :global-context)))
         (mode (context-navigator-pinned-title--mode))
         (icon-p (fboundp 'all-the-icons-material))
         (ico-proj (if icon-p
                       (ignore-errors
                         (propertize (all-the-icons-material "layers")
                                     'face '(:foreground "DodgerBlue3" :height 0.9 :raise -0.2)
                                     'display '(raise 0.08)))
                     "📁"))
         (ico-gr (if icon-p
                     (ignore-errors
                       (propertize (all-the-icons-material "folder")
                                   'face '(:foreground "MediumOrchid3" :height 0.9 :raise -0.2)
                                   'display '(raise 0.08)))
                   "🏷"))
         (base (cond
                ((eq mode 'groups) (format "%s %s" ico-proj proj))
                (slug (format "%s %s  %s %s" ico-proj proj ico-gr slug))
                (t (format "%s %s" ico-proj proj))))
         (txt (concat (make-string (max 0 context-navigator-pinned-title-left-padding) ?\s)
                      base))
         (s (copy-sequence txt)))
    (add-text-properties 0 (length s)
                         (list 'face 'context-navigator-pinned-title-face
                               'context-navigator-header t)
                         s)
    s))

(defun context-navigator-pinned-title--nav-window ()
  (catch 'hit
    (let ((buf (and (boundp 'context-navigator-view--buffer-name)
                    (get-buffer context-navigator-view--buffer-name))))
      (dolist (w (window-list nil 'no-mini))
        (when (and (window-live-p w)
                   (eq (window-buffer w) buf))
          (throw 'hit w))))
    nil))

(defun context-navigator-pinned-title-enable ()
  "Enable pinned title (posframe when available)."
  (setq context-navigator--pintitle-on t)
  (when (context-navigator-pinned-title--want-posframe-p)
    (context-navigator-pinned-title-refresh)))

(defun context-navigator-pinned-title-refresh ()
  "Refresh pinned title (posframe), if enabled."
  (when (and context-navigator-pinned-title-enable
             (context-navigator-pinned-title--want-posframe-p))
    (let* ((win (context-navigator-pinned-title--nav-window))
           (buf (and (window-live-p win) (window-buffer win))))
      (when (and (window-live-p win) (buffer-live-p buf))
        (let ((str (context-navigator-pinned-title--compute)))
          (setq context-navigator--pintitle-frame
                (posframe-show (get-buffer-create " *cn-pinned-title*")
                               :string str
                               ;; Наш poshandler ставит фрейм ровно в левый верхний угол текстовой области окна,
                               ;; то есть строго под header-line и над первой строкой буфера навигатора.
                               :poshandler #'context-navigator-pinned-title--poshandler-below-headerline
                               :position (window-start win)
                               :respect-header-line nil
                               :respect-tab-line t
                               :accept-focus nil
                               :background-color (face-background 'header-line nil t)
                               :foreground-color (face-foreground 'header-line nil t)
                               :border-width 0
                               :parent-window win
                               :x-pixel-offset 0
                               :y-pixel-offset 0)))))))

(defun context-navigator-pinned-title-disable ()
  "Disable pinned title (hide posframe)."
  (setq context-navigator--pintitle-on nil)
  (when (and (featurep 'posframe) (frame-live-p context-navigator--pintitle-frame))
    (ignore-errors (posframe-hide (frame-parameter context-navigator--pintitle-frame 'posframe-buffer)))
    (ignore-errors (posframe-delete context-navigator--pintitle-frame))
    (setq context-navigator--pintitle-frame nil)))

(defun context-navigator-pinned-title-fallback-line (&optional _mode)
  "Return inline fallback title (first line), or empty string when posframe is in use."
  (if (context-navigator-pinned-title--want-posframe-p)
      ""
    (context-navigator-pinned-title--compute)))

(provide 'context-navigator-view-pinned-title)
;;; context-navigator-view-pinned-title.el ends here
