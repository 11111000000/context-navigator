;;; context-navigator-icons.el --- Optional icon provider -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Optional icons provider using all-the-icons when available.
;; Returns small, cached strings; safe to disable on remote/TRAMP.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)

(defgroup context-navigator-icons nil
  "Icon settings for context-navigator."
  :group 'context-navigator)

(defcustom context-navigator-enable-icons t
  "Whether to render icons in the sidebar."
  :type 'boolean :group 'context-navigator-icons)

(defcustom context-navigator-icons-disable-on-remote t
  "Disable icons on remote/TRAMP buffers/paths."
  :type 'boolean :group 'context-navigator-icons)

(defvar context-navigator-icons--cache (make-hash-table :test 'equal))

(defun context-navigator-icons-clear-cache ()
  "Clear cached icons."
  (clrhash context-navigator-icons--cache))

(defun context-navigator-icons--remote-p (item)
  "Return non-nil if ITEM path is remote."
  (let ((p (context-navigator-item-path item)))
    (and (stringp p) (file-remote-p p))))

(defun context-navigator-icons--ensure ()
  "Return non-nil if icons can be provided."
  (and context-navigator-enable-icons
       (fboundp 'all-the-icons-icon-for-file)))

(defun context-navigator-icons-for-item (item)
  "Return an icon string for ITEM or nil."
  (when (and (context-navigator-icons--ensure)
             (not (and context-navigator-icons-disable-on-remote
                       (context-navigator-icons--remote-p item))))
    (let* ((key (list (context-navigator-item-type item)
                      (or (context-navigator-item-path item) "")
                      (or (context-navigator-item-name item) "")))
           (cached (gethash key context-navigator-icons--cache)))
      (or cached
          (let* ((type (context-navigator-item-type item))
                 (icon
                  (pcase type
                    ('file (when-let* ((p (context-navigator-item-path item))
                                       (ext (file-name-extension p)))
                             (ignore-errors (all-the-icons-icon-for-file p))))
                    ('buffer (ignore-errors (all-the-icons-octicon "file-text")))
                    ('selection (ignore-errors (all-the-icons-material "content_copy")))
                    (_ nil))))
            (when (stringp icon)
              (puthash key icon context-navigator-icons--cache)
              icon))))))

(defun context-navigator-icons-for-indicator (state)
  "Return a small icon string for indicator STATE or nil.
STATE is one of: 'ok, 'mismatch, 'absent."
  (let* ((color (pcase state
                  ('ok "green4")
                  ('mismatch "goldenrod2")
                  (_ "gray")))
         (icon
          (cond
           ;; Prefer Font Awesome if available
           ((fboundp 'all-the-icons-faicon)
            (pcase state
              ('ok (all-the-icons-faicon "circle"))
              ('absent (all-the-icons-faicon "circle-o"))
              ('mismatch (all-the-icons-faicon "dot-circle-o"))))
           ;; Material fallback if faicon missing
           ((fboundp 'all-the-icons-material)
            (pcase state
              ('ok (all-the-icons-material "lens"))
              ('absent (all-the-icons-material "panorama_fish_eye"))
              ('mismatch (all-the-icons-material "brightness_1"))))
           (t nil))))
    (when (stringp icon)
      (propertize icon 'face (list :foreground color :height 1.0)))))

(provide 'context-navigator-icons)
;;; context-navigator-icons.el ends here
