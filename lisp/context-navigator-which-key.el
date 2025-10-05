;;; context-navigator-which-key.el --- which-key integration (middle path) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Optional which-key integration sourced from the centralized keyspec.
;; - Applies human-readable labels (from i18n/:desc-key) to keys present in the mode maps
;; - Covers Navigator sidebar (items/groups) and Multifile view
;; - Reacts to keyspec changes (variable watcher)
;;
;; Safe when which-key is absent: this file becomes a no-op.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-keyspec)
(require 'context-navigator-i18n)

;; which-key is optional; guard all calls
(defvar which-key--dummy nil)

(defun context-navigator-which-key--normalize-key (k)
  "Return a which-key-friendly representation for K."
  (pcase k
    ((or "RET" "<return>" "<kp-enter>") "RET")
    ("SPC" "SPC")
    ((or "TAB" "<tab>" "C-i") "TAB")
    ((or "<backtab>" "S-<tab>") "S-TAB")
    (_ k)))

(defun context-navigator-which-key--desc (pl)
  "Resolve a human description for a keyspec entry PL."
  (let ((k (plist-get pl :desc-key)))
    (cond
     ((and k (fboundp 'context-navigator-i18n))
      (context-navigator-i18n k))
     (t (symbol-name (or (plist-get pl :id) (plist-get pl :cmd)))))))

(defun context-navigator-which-key--replacements-for (context)
  "Build (key . label) pairs for CONTEXT from keyspec."
  (let ((entries (cl-remove-if-not
                  (lambda (pl) (memq context (plist-get pl :contexts)))
                  context-navigator-keyspec))
        (acc '()))
    (dolist (pl entries (nreverse acc))
      (let* ((label (context-navigator-which-key--desc pl))
             (keys  (or (plist-get pl :keys) '())))
        (dolist (k keys)
          (push (cons (context-navigator-which-key--normalize-key k) label) acc))))))

(defun context-navigator-which-key--apply-to-map (map-symbol context)
  "Apply which-key replacements for MAP-SYMBOL using CONTEXT from keyspec."
  (when (and (featurep 'which-key)
             (fboundp 'which-key-add-keymap-based-replacements)
             (boundp map-symbol)
             (keymapp (symbol-value map-symbol)))
    (let ((map (symbol-value map-symbol))
          (pairs (context-navigator-which-key--replacements-for context)))
      ;; which-key-add-keymap-based-replacements accepts pairs (key desc) variadically,
      ;; but we call it per pair for clarity and idempotency.
      (dolist (cell pairs)
        (condition-case _err
            (which-key-add-keymap-based-replacements map (car cell) (cdr cell))
          (error nil))))
    t))

;;;###autoload
(defun context-navigator-which-key-apply! ()
  "Apply which-key replacements for Navigator maps from keyspec (idempotent, safe)."
  (interactive)
  (when (featurep 'which-key)
    ;; Sidebar (items/groups/global live in the same mode-map)
    (ignore-errors
      (context-navigator-which-key--apply-to-map 'context-navigator-view-mode-map 'global)
      (context-navigator-which-key--apply-to-map 'context-navigator-view-mode-map 'items)
      (context-navigator-which-key--apply-to-map 'context-navigator-view-mode-map 'groups))
    ;; Multifile
    (ignore-errors
      (context-navigator-which-key--apply-to-map 'context-navigator-multifile-mode-map 'multifile))))

;; Auto-apply on load when which-key is present
(when (featurep 'which-key)
  (ignore-errors (context-navigator-which-key-apply!)))

;; Re-apply when keyspec changes (middle-path watcher)
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher
   'context-navigator-keyspec
   (lambda (&rest _)
     (when (featurep 'which-key)
       (ignore-errors (context-navigator-which-key-apply!))))))

(provide 'context-navigator-which-key)
;;; context-navigator-which-key.el ends here
