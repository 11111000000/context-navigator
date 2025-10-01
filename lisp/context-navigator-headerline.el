;;; context-navigator-headerline.el --- Header-line controls for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Puts Navigator control toggles and actions into the header-line of the
;; Navigator buffer. Enabled by default and configurable via:
;;   - context-navigator-view-headerline-enable
;;
;; The header-line shows only interactive control segments (toggles + actions)
;; produced by the controls module; the project/group title is rendered inside
;; the buffer itself (above the \"..\" line). Per-point status is displayed in
;; the modeline.
;;
;; This module avoids hard requires on the view module to prevent load cycles:
;; it declares the helper functions used and calls them when available.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup context-navigator-headerline nil
  "Header-line controls for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-view-headerline-enable t
  "When non-nil, show Navigator controls and toggles in the header-line."
  :type 'boolean :group 'context-navigator-headerline)

;; Declarations for byte-compiler friendliness (provided by context-navigator-view).
(declare-function context-navigator-view-controls-segments "context-navigator-view-controls" ())
(declare-function context-navigator-view--header "context-navigator-view" (state))
(declare-function context-navigator--state-get "context-navigator-core" ())
(declare-function context-navigator-state-last-project-root "context-navigator-core" (state))
(declare-function context-navigator-persist-state-load "context-navigator-persist" (root))

(defvar-local context-navigator-headerline--cache-key nil)
(defvar-local context-navigator-headerline--cache-str nil)

(defun context-navigator-headerline-format ()
  "Return header-line content for Navigator buffers (with light caching).

Shows only control toggles and action segments; the project/group title is
rendered inside the buffer itself (above the \"..\" line).

Cache key includes:
- controls style and icons availability,
- session toggles (push/auto),
- Occam spinner state/index (when available),
- current view mode (items/groups),
- selection size and multi-group flag (per-project),
so that frequent redisplays reuse the same string unless a relevant bit changes."
  (when (eq major-mode 'context-navigator-view-mode)
    (let* ((style (and (boundp 'context-navigator-controls-style)
                       context-navigator-controls-style))
           (icons-on (and (fboundp 'context-navigator-controls-icons-available-p)
                          (context-navigator-controls-icons-available-p)))
           (push-on (and (boundp 'context-navigator--push-to-gptel)
                         context-navigator--push-to-gptel))
           (auto-on (and (boundp 'context-navigator--auto-project-switch)
                         context-navigator--auto-project-switch))
           ;; Occam spinner state (optional; present only when razor module is loaded)
           (razor-run (and (boundp 'context-navigator-razor--running)
                           context-navigator-razor--running))
           (razor-idx (and (boundp 'context-navigator-razor--spinner-index)
                           context-navigator-razor--spinner-index))
           ;; Additional bits to make headerline cache sensitive to selection/mode.
           (mode-sym (and (boundp 'context-navigator-view--mode)
                          context-navigator-view--mode))
           (sel-count
            (ignore-errors
              (let* ((st (context-navigator--state-get))
                     (root (and st (context-navigator-state-last-project-root st)))
                     (ps (and (stringp root)
                              (context-navigator-persist-state-load root)))
                     (sel (and (listp ps) (plist-member ps :selected) (plist-get ps :selected))))
                (if (listp sel) (length sel) 0))))
           (mg-flag
            (ignore-errors
              (let* ((st (context-navigator--state-get))
                     (root (and st (context-navigator-state-last-project-root st)))
                     (ps (and (stringp root)
                              (context-navigator-persist-state-load root))))
                (and (listp ps) (plist-member ps :multi) (plist-get ps :multi)))))
           (key (list style icons-on push-on auto-on razor-run razor-idx mode-sym sel-count mg-flag)))
      (if (equal key context-navigator-headerline--cache-key)
          ;; Reuse cached string
          context-navigator-headerline--cache-str
        ;; Build headerline segments using the controls module.
        ;; Apply a small downward shift for graphic icons in the header-line so
        ;; they visually have a tiny top offset (space above the icons).
        (let ((controls (ignore-errors
                          (when (fboundp 'context-navigator-view-controls-segments)
                            (let ((context-navigator-controls-icon-raise -0.08))
                              (context-navigator-view-controls-segments))))))
          (when (and (listp controls) controls)
            ;; Preserve exact spacing from segments (tests rely on it).
            (let ((s (mapconcat #'identity controls "")))
              (setq context-navigator-headerline--cache-key key
                    context-navigator-headerline--cache-str s)
              s)))))))

(defun context-navigator-headerline--apply (buffer)
  "Apply or remove header-line controls in BUFFER based on the feature flag."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq major-mode 'context-navigator-view-mode)
        (if context-navigator-view-headerline-enable
            (setq header-line-format '((:eval (context-navigator-headerline-format))))
          ;; Remove only if we had previously installed our own format.
          (when (equal header-line-format '((:eval (context-navigator-headerline-format))))
            (setq header-line-format nil)))
        (ignore-errors
          (context-navigator-debug :debug :ui "headerline applied: enabled=%s, format=%S"
                                   context-navigator-view-headerline-enable header-line-format))
        (force-mode-line-update t)))))

;; React to runtime toggling of the header-line feature.
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher
   'context-navigator-view-headerline-enable
   (lambda (_sym _newval _op _where)
     (dolist (buf (buffer-list))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (eq major-mode 'context-navigator-view-mode)
             (context-navigator-headerline--apply (current-buffer)))))))))

(provide 'context-navigator-headerline)
;;; context-navigator-headerline.el ends here
