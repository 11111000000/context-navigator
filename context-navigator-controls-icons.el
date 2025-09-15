;;; context-navigator-controls-icons.el --- Graphic icons for controls  -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Context Navigator
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; This module provides a small API to render uniform-sized control icons
;; using all-the-icons, with caching and graceful fallbacks.
;;
;; Integration points:
;; - Call (context-navigator-controls-icons-available-p) to decide whether
;;   to render icon-only controls (without brackets).
;; - Use (context-navigator-controls-icon KEY &optional STATE) to get a propertized
;;   icon string (or nil if unavailable).
;;
;; Fallback policy:
;; - If not in a GUI, or all-the-icons is missing, returns nil.
;; - Callers should fallback to the legacy compact/text controls (with brackets).

;;; Code:

(require 'subr-x)

(defgroup context-navigator-controls-icons nil
  "Graphic icons for Context Navigator controls (header-line, etc.)."
  :group 'context-navigator
  :prefix "context-navigator-controls-")

(defcustom context-navigator-controls-use-graphic-icons t
  "When non-nil, Context Navigator will prefer graphic icons (all-the-icons) for controls.
If icons are unavailable (TTY or package/fonts missing), callers must fallback
to the textual/compact style (with brackets)."
  :type 'boolean
  :group 'context-navigator-controls-icons)

(defcustom context-navigator-controls-icon-height 0.9
  "Uniform height for control icons."
  :type 'number
  :group 'context-navigator-controls-icons)

(defcustom context-navigator-controls-icon-raise 0.11
  "Vertical raise for control icons (applied via `display' property)."
  :type 'number
  :group 'context-navigator-controls-icons)

(defface context-navigator-controls-icon-on
  '((t :inherit success))
  "Face for ON-state control icons."
  :group 'context-navigator-controls-icons)

(defface context-navigator-controls-icon-off
  '((t :inherit shadow))
  "Face for OFF-state control icons."
  :group 'context-navigator-controls-icons)

;; Default icon map. Keys are control identifiers; values are either:
;; - A cons (PROVIDER . NAME)
;; - An alist of states ((on . (PROVIDER . NAME)) (off . (PROVIDER . NAME)))
;; Providers map to all-the-icons backends, e.g. faicon, material, octicon.
(defcustom context-navigator-controls-icon-map
  '((push . ((on  . (faicon   . "toggle-on"))
             (off . (faicon   . "toggle-off"))))
    (auto-project . ((on  . (material . "autorenew"))
                     (off . (material . "autorenew"))))
    (undo . (faicon . "undo"))
    (redo . (faicon . "repeat"))
    (push-now . (faicon . "paper-plane"))
    (open-buffers . (faicon . "folder-open"))
    (close-buffers . (material . "close"))
    (clear-gptel . (faicon . "trash"))
    (toggle-all-gptel . (faicon . "check-square-o")))
  "Default mapping of control keys to all-the-icons specs."
  :type '(alist :key-type symbol
                :value-type (choice
                             (cons :tag "Single icon"
                                   (symbol :tag "Provider")
                                   (string :tag "Name"))
                             (alist :tag "Stateful toggle"
                                    :key-type (choice (const on) (const off))
                                    :value-type (cons (symbol :tag "Provider")
                                                      (string :tag "Name")))))
  :group 'context-navigator-controls-icons)

(defcustom context-navigator-controls-icon-face-map
  '((undo . (:foreground "SteelBlue4"))
    (redo . (:foreground "SteelBlue4"))
    (close-buffers . (:foreground "IndianRed3"))
    (clear-gptel . (:foreground "orange3")))
  "Optional per-key face overrides for control icons.
Each entry is either a face symbol or a plist like (:foreground \"...\" [:height N])."
  :type '(alist :key-type symbol
                :value-type (choice face (plist :key-type symbol :value-type sexp)))
  :group 'context-navigator-controls-icons)

(defvar context-navigator-controls-icons--cache (make-hash-table :test 'equal)
  "Cache for rendered control icons.
Key is a list (KEY STATE HEIGHT RAISE FACE-SYM).")

(defun context-navigator-controls-icons--provider-fn (provider)
  "Return all-the-icons rendering function for PROVIDER symbol."
  (let* ((name (format "all-the-icons-%s" provider))
         (fn (intern-soft name)))
    (when (and fn (fboundp fn)) fn)))

(defun context-navigator-controls-icons--spec-for (key state)
  "Get icon spec for KEY and optional STATE.
Returns cons (PROVIDER . NAME) or nil."
  (let ((spec (alist-get key context-navigator-controls-icon-map)))
    (cond
     ;; Single icon form: (provider . name)
     ((and (consp spec) (symbolp (car spec)) (stringp (cdr spec)))
      spec)
     ;; Stateful form: ((on . (provider . name)) (off . (provider . name)))
     ((and (listp spec) (memq state '(on off)))
      (alist-get state spec))
     (t nil))))

(defun context-navigator-controls-icons-available-p ()
  "Return non-nil if graphic icons can be used in the current context."
  (and context-navigator-controls-use-graphic-icons
       (display-graphic-p)
       (require 'all-the-icons nil t)))

(defun context-navigator-controls-icon (key &optional state)
  "Return a propertized icon string for control KEY and optional STATE.
Returns nil if icons are not available or spec cannot be resolved.

Known KEYS by default:
  push (stateful: on/off)
  auto-project (stateful: on/off)
  undo redo push-now open-buffers close-buffers clear-gptel toggle-all-gptel

STATE is used for stateful controls: 'on or 'off."
  (when (context-navigator-controls-icons-available-p)
    (let* ((spec (context-navigator-controls-icons--spec-for key state))
           (provider (car-safe spec))
           (name (cdr-safe spec))
           ;; Default state-based face only for true toggles; otherwise keep nil (provider default),
           ;; then apply per-key override below when configured.
           (base-face (when (memq state '(on off))
                        (if (eq state 'on)
                            'context-navigator-controls-icon-on
                          'context-navigator-controls-icon-off)))
           (override (alist-get key context-navigator-controls-icon-face-map))
           (final-face (or override base-face))
           (cache-key (list key state
                            context-navigator-controls-icon-height
                            context-navigator-controls-icon-raise
                            final-face)))
      (or (gethash cache-key context-navigator-controls-icons--cache)
          (when (and provider name)
            (let* ((fn (context-navigator-controls-icons--provider-fn provider))
                   (icon
                    (when fn
                      (ignore-errors
                        (funcall fn name
                                 :face (or (and (symbolp final-face) final-face)
                                           (and (listp final-face) final-face))
                                 :height context-navigator-controls-icon-height
                                 ;; keep v-adjust neutral; we apply raise via display:
                                 :v-adjust 0.0)))))
              (when (and (stringp icon) (not (string-empty-p icon)))
                ;; Apply uniform raise on top of backend result; also enforce height override
                ;; when face is a plist (add :height if missing).
                (let* ((s (propertize icon
                                      'display
                                      (list 'raise context-navigator-controls-icon-raise))))
                  (puthash cache-key s context-navigator-controls-icons--cache)
                  s))))))))

(defun context-navigator-controls-icons-clear-cache ()
  "Clear the internal cache of rendered control icons."
  (clrhash context-navigator-controls-icons--cache))

(provide 'context-navigator-controls-icons)

;;; context-navigator-controls-icons.el ends here
