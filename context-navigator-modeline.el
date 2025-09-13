;;; context-navigator-modeline.el --- Minimal modeline for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides a minimal modeline for Context Navigator buffers that mirrors the
;; inline status hint (current control help or item path).
;;
;; Enabled by default, configurable via:
;; - context-navigator-view-modeline-enable
;; - context-navigator-view-modeline-face

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function context-navigator-view--status-text-at-point "context-navigator-view" ())

(defgroup context-navigator-modeline nil
  "Modeline settings for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-view-modeline-enable t
  "When non-nil, use a minimal modeline for Context Navigator buffers showing the status hint."
  :type 'boolean :group 'context-navigator-modeline)

(defcustom context-navigator-view-modeline-face 'shadow
  "Face to render the status text in the Navigator modeline."
  :type 'face :group 'context-navigator-modeline)

(defun context-navigator-modeline-string ()
  "Return minimal modeline string for Context Navigator buffers."
  (let* ((txt (when (fboundp 'context-navigator-view--status-text-at-point)
                (ignore-errors (context-navigator-view--status-text-at-point)))))
    (propertize (concat " " (or txt "")) 'face context-navigator-view-modeline-face)))

(defun context-navigator-modeline--apply (buffer)
  "Apply or remove modeline in BUFFER based on the feature flag.
If the global default `mode-line-format' is nil (user disabled global modeline),
install the status string into `header-line-format' so the Navigator still shows it."
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
