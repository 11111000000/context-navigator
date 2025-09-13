;;; context-navigator-headerline.el --- Header-line controls for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Puts Navigator control toggles and actions into the header-line of the
;; Navigator buffer. Enabled by default and configurable via:
;;   - context-navigator-view-headerline-enable
;;
;; The header-line shows:
;;   [context-navigator: default]  [A] [→] [O] [∅] [✖] [⇪] [⌦]
;; The project/group title is rendered at the left (mode-line-emphasis), followed
;; by the interactive control segments produced by the view. The header-line no
;; longer contains the per-point status (it is shown in the modeline).
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
(declare-function context-navigator-view--footer-control-segments "context-navigator-view" ())
(declare-function context-navigator-view--header "context-navigator-view" (state))
(declare-function context-navigator--state-get "context-navigator-core" ())

(defun context-navigator-headerline-format ()
  "Return header-line content for Navigator buffers.

Format:
 - project/group title (from view helper) in `mode-line-emphasis' face
 - two spaces
 - controls segments produced by `context-navigator-view--footer-control-segments'

The per-point status is intentionally omitted from the header-line (it's shown
in the dedicated modeline)."
  (when (eq major-mode 'context-navigator-view-mode)
    (let* ((st (and (fboundp 'context-navigator--state-get)
                    (ignore-errors (context-navigator--state-get))))
           (title (and (fboundp 'context-navigator-view--header)
                       (ignore-errors (context-navigator-view--header st))))
           (title-str (when (and (stringp title) (> (length (string-trim title)) 0))
                        (propertize (concat " " title) 'face 'mode-line-emphasis)))
           (controls (and (fboundp 'context-navigator-view--footer-control-segments)
                          (ignore-errors (context-navigator-view--footer-control-segments)))))
      ;; Compose parts into a single propertized string so header-line preserves
      ;; text properties (keymaps/local-maps) reliably and mouse clicks work.
      (let ((parts (append (if title-str (list title-str "  ") (list ""))
                           (or controls '()))))
        (apply #'concat parts)))))

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
