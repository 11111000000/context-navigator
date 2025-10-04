;;; context-navigator-view-title.el --- Title builders and placement helpers -*- lexical-binding: t; -*-

;; Title string builders (project/group) and simple placement helpers.

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-core)
(require 'context-navigator-i18n)
(require 'context-navigator-icons)

(defgroup context-navigator-title nil
  "Title (project/group) for Navigator."
  :group 'context-navigator)

(defcustom context-navigator-title-enable nil
  "Deprecated: no-op. Title is shown in the header-line by default."
  :type 'boolean :group 'context-navigator-title)

(defface context-navigator-title-face
  '((t :inherit mode-line-emphasis))
  "Face for pinned title text."
  :group 'context-navigator-title)

(defvar-local context-navigator--pintitle-on nil)


(defcustom context-navigator-title-left-padding 2
  "Number of space characters to prefix to the pinned title text."
  :type 'integer :group 'context-navigator-title)


(defun context-navigator-title--mode ()
  (and (boundp 'context-navigator-view--mode) context-navigator-view--mode))

(defun context-navigator-title--compute ()
  "Return a propertized title string with icons: [project[: group]]."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (slug (and st (context-navigator-state-current-group-slug st)))
         (proj (if (and (stringp root) (not (string-empty-p root)))
                   (file-name-nondirectory (directory-file-name root))
                 (context-navigator-i18n :global-context)))
         (mode (context-navigator-title--mode))
         (icon-p (fboundp 'all-the-icons-material))
         (ico-proj (if icon-p
                       (ignore-errors
                         (propertize (all-the-icons-material "layers")
                                     'face '(:foreground "DodgerBlue3" :height 0.9)
                                     'display '(raise -0.1)))
                     "📁"))
         (ico-gr (if icon-p
                     (ignore-errors
                       (propertize (all-the-icons-material "folder")
                                   'face '(:foreground "MediumOrchid3" :height 0.9)
                                   'display '(raise -0.1)))
                   "🏷"))
         (base (cond
                ((eq mode 'groups) (format "%s %s" ico-proj proj))
                (slug (format "%s %s  %s %s" ico-proj proj ico-gr slug))
                (t (format "%s %s" ico-proj proj))))
         (txt (concat (make-string (max 0 context-navigator-title-left-padding) ?\s)
                      base))
         (s (copy-sequence txt)))
    (add-text-properties 0 (length s)
                         (list 'font-lock-face 'context-navigator-title-face
                               'context-navigator-title t
                               'context-navigator-header t)
                         s)
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :trace :ui
                                 "pinned-title inline: mode=%s slug=%s"
                                 (context-navigator-title--mode)
                                 slug)))
    s))

(defun context-navigator-title--nav-window ()
  (catch 'hit
    (let ((buf (and (boundp 'context-navigator-view--buffer-name)
                    (get-buffer context-navigator-view--buffer-name))))
      (dolist (w (window-list nil 'no-mini))
        (when (and (window-live-p w)
                   (eq (window-buffer w) buf))
          (throw 'hit w))))
    nil))

(defun context-navigator-title-enable ()
  "Enable inline pinned title (no posframe)."
  (setq context-navigator--pintitle-on t))


(defun context-navigator-title-refresh ()
  "No-op: inline title renders together with the buffer."
  nil)

(defun context-navigator-title-disable ()
  "Disable inline pinned title."
  (setq context-navigator--pintitle-on nil))

(defun context-navigator-title-fallback-line (&optional _mode)
  "Return inline title (posframe removed)."
  (context-navigator-title--compute))

(provide 'context-navigator-view-title)
;;; context-navigator-view-title.el ends here
