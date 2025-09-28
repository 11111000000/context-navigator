;;; context-navigator-modeline.el --- Minimal modeline for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Minimal, Navigator-focused modeline for the sidebar/buffer:
;; - Items view: show full absolute path of the selected item (if any)
;; - Groups view: show current group display name and its description (if any)
;;
;; Enabled by default, configurable via:
;; - context-navigator-view-modeline-enable
;; - context-navigator-view-modeline-face

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)
(require 'context-navigator-core)
(require 'context-navigator-groups)

(declare-function context-navigator-view--status-text-at-point "context-navigator-view-items" ())
(declare-function context-navigator--groups-candidates "context-navigator-groups" (&optional root))
(declare-function context-navigator--state-read "context-navigator-groups" (root))

(defgroup context-navigator-modeline nil
  "Modeline settings for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-view-modeline-enable t
  "When non-nil, use a minimal modeline for Context Navigator buffers showing the status hint."
  :type 'boolean :group 'context-navigator-modeline)

(defcustom context-navigator-view-modeline-face 'shadow
  "Face to render the status text in the Navigator modeline."
  :type 'face :group 'context-navigator-modeline)

(defvar-local context-navigator-modeline--desc-cache nil
  "Cache for group descriptions per root.
Plist: (:root ROOT :stamp TIME :alist ALIST), where ALIST is slug->desc.")

(defun context-navigator-modeline--desc-alist (root)
  "Return description alist for ROOT, reloading rarely to avoid IO on every tick."
  (let* ((now (float-time))
         (ttl 2.0)
         (cached (and (plist-get context-navigator-modeline--desc-cache :root)
                      (equal (plist-get context-navigator-modeline--desc-cache :root) root)
                      (plist-get context-navigator-modeline--desc-cache :alist)))
         (stamp (and (plist-get context-navigator-modeline--desc-cache :root)
                     (plist-get context-navigator-modeline--desc-cache :stamp))))
    (if (and cached stamp (< (- now stamp) ttl))
        cached
      (let* ((st (ignore-errors (context-navigator--state-read root)))
             (alist (and (plist-member st :descriptions)
                         (plist-get st :descriptions))))
        (setq context-navigator-modeline--desc-cache
              (list :root root :stamp now :alist alist))
        alist))))

(defun context-navigator-modeline--item-fullpath-at-point ()
  "Return absolute path for the item at point (if any), else nil."
  (let ((it (get-text-property (point) 'context-navigator-item)))
    (when (and it (context-navigator-item-p it))
      (let ((p (context-navigator-item-path it)))
        (when (and (stringp p) (not (string-empty-p p)))
          ;; Expand to absolute, then abbreviate with ~ for readability
          (abbreviate-file-name (expand-file-name p)))))))

(defun context-navigator-modeline--group-summary ()
  "Return \"<Display> — <Desc>\" for the current group when available."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (slug (and st (context-navigator-state-current-group-slug st))))
    (when slug
      (let* ((cand (ignore-errors (context-navigator--groups-candidates root)))
             (disp (or (car (rassoc slug cand)) slug))
             (desc (let* ((alist (context-navigator-modeline--desc-alist root)))
                     (cdr (assoc slug alist)))))
        (string-trim
         (if (and (stringp desc) (not (string-empty-p (string-trim desc))))
             (format "%s — %s" disp desc)
           (format "%s" disp)))))))

(defun context-navigator-modeline--text ()
  "Compute Navigator-specific modeline text depending on view mode."
  (cond
   ;; Only for Navigator view buffers
   ((not (eq major-mode 'context-navigator-view-mode))
    "")
   ;; Groups view: group summary
   ((and (boundp 'context-navigator-view--mode)
         (eq context-navigator-view--mode 'groups))
    (or (context-navigator-modeline--group-summary) ""))
   ;; Items view: full path of item at point (if any)
   (t
    (or (context-navigator-modeline--item-fullpath-at-point)
        ""))))

(defun context-navigator-modeline-string ()
  "Return minimal modeline string for Context Navigator buffers."
  (let* ((txt (context-navigator-modeline--text)))
    (propertize (concat " " (or txt "")) 'face context-navigator-view-modeline-face)))

(defun context-navigator-modeline--apply (buffer)
  "Apply or remove modeline in BUFFER based on the feature flag.
If the global default `mode-line-format' is nil (user disabled global modeline),
install the status string into `header-line-format' so the Navigator still shows it.

Additionally, enforce the modeline via window parameter `mode-line-format' on
all windows showing BUFFER to out-prioritize global modeline providers."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq major-mode 'context-navigator-view-mode)
        (let ((fmt (and context-navigator-view-modeline-enable
                        '((:eval (context-navigator-modeline-string)))))
              (global-mode-line (default-value 'mode-line-format)))
          ;; Prefer buffer-local mode-line when the global/default mode-line exists.
          ;; If the user has globally disabled the mode-line (default nil), use
          ;; header-line only when Navigator header-line controls are disabled,
          ;; to avoid collisions.
          (if global-mode-line
              (progn
                (setq mode-line-format fmt
                      header-line-format nil))
            (progn
              (setq mode-line-format nil)
              (unless (and (boundp 'context-navigator-view-headerline-enable)
                           context-navigator-view-headerline-enable)
                (setq header-line-format fmt)))))
        ;; Also set window-parameter 'mode-line-format so external modelines cannot override.
        (dolist (w (get-buffer-window-list (current-buffer) nil t))
          (when (window-live-p w)
            (set-window-parameter w 'mode-line-format
                                  (and context-navigator-view-modeline-enable
                                       '((:eval (context-navigator-modeline-string)))))))
        (force-mode-line-update t)))))

;; React to runtime toggling
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher
   'context-navigator-view-modeline-enable
   (lambda (_sym _newval _op _where)
     (dolist (buf (buffer-list))
       (context-navigator-modeline--apply buf)))))

(provide 'context-navigator-modeline)
;;; context-navigator-modeline.el ends here
