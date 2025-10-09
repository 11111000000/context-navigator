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
(require 'context-navigator-persist)
(require 'context-navigator-view-controls)

(declare-function context-navigator-view--status-text-at-point "context-navigator-view-items" ())
(declare-function context-navigator--groups-candidates "context-navigator-groups" (&optional root))
(declare-function context-navigator--state-get "context-navigator-core" ())

(defgroup context-navigator-modeline nil
  "Modeline settings for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-view-modeline-enable t
  "When non-nil, use a minimal modeline for Context Navigator buffers showing the status hint."
  :type 'boolean :group 'context-navigator-modeline)

(defcustom context-navigator-view-modeline-face 'shadow
  "Face to render the status text in the Navigator modeline."
  :type 'face :group 'context-navigator-modeline)

;; Face-remap cookies to make modeline background equal to header-line and remove border.
(defvar-local context-navigator--modeline-face-cookie nil)
(defvar-local context-navigator--modeline-inactive-face-cookie nil)
(defvar-local context-navigator--modeline-active-face-cookie nil)

(defvar-local context-navigator-modeline--cached-menu nil
  "Cached concatenated controls string for Navigator modeline.")

(defun context-navigator-modeline--rebuild-menu-cache ()
  "Rebuild and cache the controls menu string for the Navigator modeline.
This function may call heavy control renderers; call it only on explicit events."
  (let* ((segs (and (fboundp 'context-navigator-view-controls-segments)
                    (context-navigator-view-controls-segments)))
         (txt (mapconcat (lambda (s) (or s "")) (or segs '()) "")))
    (setq context-navigator-modeline--cached-menu txt)
    txt))

(defun context-navigator-modeline--ensure-face ()
  "Ensure Navigator modeline uses header-line-like background with no border."
  (when (eq major-mode 'context-navigator-view-mode)
    ;; Reset previous remaps to keep the operation idempotent.
    (when context-navigator--modeline-face-cookie
      (ignore-errors (face-remap-remove-relative context-navigator--modeline-face-cookie))
      (setq context-navigator--modeline-face-cookie nil))
    (when context-navigator--modeline-inactive-face-cookie
      (ignore-errors (face-remap-remove-relative context-navigator--modeline-inactive-face-cookie))
      (setq context-navigator--modeline-inactive-face-cookie nil))
    (when context-navigator--modeline-active-face-cookie
      (ignore-errors (face-remap-remove-relative context-navigator--modeline-active-face-cookie))
      (setq context-navigator--modeline-active-face-cookie nil))
    ;; Inherit header-line, but explicitly drop :box to remove thick border.
    (setq context-navigator--modeline-face-cookie
          (face-remap-add-relative 'mode-line 'context-navigator-toolbar '(:box nil)))
    (setq context-navigator--modeline-inactive-face-cookie
          (face-remap-add-relative 'mode-line-inactive 'context-navigator-toolbar '(:box nil)))
    (setq context-navigator--modeline-active-face-cookie
          (face-remap-add-relative 'mode-line-active 'context-navigator-toolbar '(:box nil)))))

(defun context-navigator-modeline--remove-face ()
  "Remove modeline face remaps previously applied by Navigator."
  (when context-navigator--modeline-face-cookie
    (ignore-errors (face-remap-remove-relative context-navigator--modeline-face-cookie))
    (setq context-navigator--modeline-face-cookie nil))
  (when context-navigator--modeline-inactive-face-cookie
    (ignore-errors (face-remap-remove-relative context-navigator--modeline-inactive-face-cookie))
    (setq context-navigator--modeline-inactive-face-cookie nil))
  (when context-navigator--modeline-active-face-cookie
    (ignore-errors (face-remap-remove-relative context-navigator--modeline-active-face-cookie))
    (setq context-navigator--modeline-active-face-cookie nil)))

(defvar-local context-navigator-modeline--desc-cache nil
  "Cache for group descriptions per root.
Plist: (:root ROOT :stamp TIME :alist ALIST), where ALIST is slug->desc.")

(defun context-navigator-modeline--desc-alist (root)
  "Return description alist for ROOT from state.el with a small TTL cache.

Reads :descriptions from persist state (state.el) instead of core struct
(state is a cl-struct, not a plist). Avoids disk IO on every tick by caching
for a short period."
  (let* ((now (float-time))
         (ttl 2.0)
         (cached (and (plist-get context-navigator-modeline--desc-cache :root)
                      (equal (plist-get context-navigator-modeline--desc-cache :root) root)
                      (plist-get context-navigator-modeline--desc-cache :alist)))
         (stamp (and (plist-get context-navigator-modeline--desc-cache :root)
                     (plist-get context-navigator-modeline--desc-cache :stamp))))
    (if (and cached stamp (< (- now stamp) ttl))
        cached
      (let* ((ps (and (stringp root)
                      (ignore-errors (context-navigator-persist-state-load root))))
             (alist (and (listp ps)
                         (plist-member ps :descriptions)
                         (plist-get ps :descriptions))))
        (setq context-navigator-modeline--desc-cache
              (list :root root :stamp now :alist alist))
        alist))))

(defun context-navigator-modeline--item-fullpath-at-point ()
  "Return path for the item at point (if any), relative to project root when possible.
If the item is outside the current project root, show an abbreviated absolute path."
  (let ((it (get-text-property (point) 'context-navigator-item)))
    (when (and it (context-navigator-item-p it))
      (let ((p (context-navigator-item-path it)))
        (when (and (stringp p) (not (string-empty-p p)))
          (let* ((pabs (expand-file-name p))
                 (st (ignore-errors (context-navigator--state-get)))
                 (root (and st (context-navigator-state-last-project-root st))))
            (if (and (stringp root) (not (string-empty-p root)))
                (let ((rel (file-relative-name pabs root)))
                  (abbreviate-file-name (if (string-prefix-p ".." rel) pabs rel)))
              (abbreviate-file-name pabs))))))))

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
  "Return toolbar controls for Navigator modeline from cache (no IO on redisplay)."
  (let* ((pad (propertize " " 'face context-navigator-view-modeline-face))
         (body (or context-navigator-modeline--cached-menu
                   (and (fboundp 'context-navigator-modeline--rebuild-menu-cache)
                        (context-navigator-modeline--rebuild-menu-cache))
                   "")))
    (concat pad body)))

(defun context-navigator-modeline--apply (buffer)
  "Apply or remove modeline in BUFFER based on the feature flag.
Ensure the Navigator modeline has no border and uses the header-line background.

Additionally, enforce the modeline via window parameter `mode-line-format' on
all windows showing BUFFER to out-prioritize global modeline providers."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq major-mode 'context-navigator-view-mode)
        (let* ((enabled context-navigator-view-modeline-enable)
               (fmt (and enabled '((:eval (context-navigator-modeline-string))))))
          (setq mode-line-format fmt)
          ;; Apply/clear local face remaps so the modeline background matches header-line and has no :box.
          (if enabled
              (context-navigator-modeline--ensure-face)
            (context-navigator-modeline--remove-face)))
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


;;; context-navigator-modeline.el ends here

(provide 'context-navigator-view-modeline)
