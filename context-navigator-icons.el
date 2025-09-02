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

(provide 'context-navigator-icons)
;;; context-navigator-icons.el ends here
