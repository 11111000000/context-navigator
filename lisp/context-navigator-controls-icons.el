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

(require 'cl-lib)
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



;; Default icon map. Keys are control identifiers; values are either:
;; - A cons (PROVIDER . NAME)
;; - An alist of states ((on . (PROVIDER . NAME)) (off . (PROVIDER . NAME)))
;; Providers map to all-the-icons backends, e.g. faicon, material, octicon.
(defcustom context-navigator-controls-icon-map
  '((push . ((on  . (faicon   . "toggle-on"))
             (off . (faicon   . "toggle-off"))))
    (auto-project . ((on  . (material . "autorenew"))
                     (off . (material . "autorenew"))))
    (multi-group . ((on . (material . "group_work"))
                    (off . (material . "group_work"))))
    (undo . (faicon . "undo"))
    (redo . (faicon . "repeat"))
    (push-now . (faicon . "paper-plane"))
    (razor . (faicon . "filter"))
    (stats . (material . "assessment"))
    (multifile . (faicon . "list"))
    ;; Multifile local controls (used in Multifile buffer header-line)
    (mf-collapse . ((on  . (faicon . "chevron-up"))   ;; on = expanded (click to collapse)
                    (off . (faicon . "chevron-down"))))
    (mf-filter . ((on . (faicon . "filter"))
                  (off . (faicon . "filter"))))
    ;; Use a valid icon backend for "edit" (material has it; faicon "edit" may be absent)
    (mf-edit-all . (material . "edit"))
    (mf-close . (faicon . "times"))
    (open-buffers . (faicon . "folder-open"))
    (close-buffers . (material . "cancel"))
    (clear-gptel . (material . "delete"))
    (clear-group . (faicon . "trash"))
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
  ;; Provide explicit on/off colors for toggles so they visually reflect state.
  '((push . ((on  . (:foreground "green4"))
             (off . (:foreground "gray60"))))
    (auto-project . ((on  . (:foreground "green4" ))
                     (off . (:foreground "gray60" ))))
    (multi-group . ((on . (:foreground "green4"))
                    (off . (:foreground "gray60"))))
    ;; Multifile local controls (match mf-collapse/mf-filter to toggle colors)
    (mf-collapse . ((on  . (:foreground "green4"))
                    (off . (:foreground "gray60"))))
    (mf-filter . ((on . (:foreground "green4"))
                  (off . (:foreground "gray60"))))
    ;; Action keys / defaults
    (undo . (:foreground "SteelBlue4"))
    (redo . (:foreground "SteelBlue4"))
    (push-now . (:foreground "magenta3"))
    (stats . (:foreground "purple"))
    (toggle-all-gptel . (:foreground "SteelBlue4"))
    (razor . (:foreground "magenta3"))
    (multifile . (:foreground "purple"))
    (open-buffers . (:foreground "orange3"))
    (close-buffers . (:foreground "orange3"))
    (clear-gptel . (:foreground "gray"))
    (clear-group . (:foreground "tomato")))
  "Optional per-key faces/attributes for control icons.

Each value may be:
- a face symbol;
- a plist of face attributes, e.g. (:foreground \"...\" :weight bold),
  and optionally :raise and :height for this key/state;
- a stateful alist for toggles: ((on . FACE-OR-PLIST) (off . FACE-OR-PLIST)).

When :raise is provided, it overrides `context-navigator-controls-icon-raise'
for that key/state. When :height is provided, it overrides
`context-navigator-controls-icon-height' for that key/state."
  :type '(alist :key-type symbol
                :value-type (choice
                             face
                             (plist :key-type symbol :value-type sexp)
                             (alist :tag "Stateful toggle faces"
                                    :key-type (choice (const on) (const off))
                                    :value-type (choice
                                                 face
                                                 (plist :key-type symbol :value-type sexp)))))
  :group 'context-navigator-controls-icons)

(defcustom context-navigator-controls-icon-local-prefixes
  '("mf-")
  "List of key name prefixes treated as \"local-only\" icon keys (e.g. Multifile header controls).
Audit and consistency checks can treat keys with these prefixes as local UI-only and not
flag them as unused in the global header-line registry."
  :type '(repeat string)
  :group 'context-navigator-controls-icons)

(defvar context-navigator-controls-icons--cache (make-hash-table :test 'equal)
  "Cache for rendered control icons.
Key is a list (KEY STATE HEIGHT RAISE FACE).")

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

(defun context-navigator-controls-icons--resolve-attrs (key state)
  "Resolve face/height/raise attributes for KEY and STATE from face map.
Returns plist (:face FACE-OR-PLIST :height H :raise R)."
  (let* ((entry (alist-get key context-navigator-controls-icon-face-map))
         (val (if (and (listp entry)
                       (or (alist-get 'on entry) (alist-get 'off entry)))
                  (alist-get state entry)
                entry))
         (height (if (and (listp val) (plist-member val :height))
                     (plist-get val :height)
                   context-navigator-controls-icon-height))
         (raise (if (and (listp val) (plist-member val :raise))
                    (plist-get val :raise)
                  context-navigator-controls-icon-raise))
         (face (cond
                ((symbolp val) val)
                ((listp val)
                 ;; Sanitize: drop non-face attributes from plist before passing as `face'
                 (let ((plist val)
                       (res nil))
                   (while plist
                     (let ((k (pop plist))
                           (v (pop plist)))
                       (unless (memq k '(:raise :height))
                         (setq res (plist-put res k v)))))
                   (and res res)))
                (t nil))))
    (list :face face :height height :raise raise)))

(defun context-navigator-controls-icons-available-p ()
  "Return non-nil if graphic icons can be used in the current context.

Avoid calling `require' in hot paths; rely on `featurep' and refresh when
all-the-icons loads later (see with-eval-after-load below)."
  (and context-navigator-controls-use-graphic-icons
       (display-graphic-p)
       (featurep 'all-the-icons)))

(defun context-navigator-controls-icon (key &optional state)
  "Return a propertized icon string for control KEY and optional STATE.
Returns nil if icons are not available or the icon spec cannot be resolved.

Selection is strict: the icon provider/name and the face/attrs are taken only
from the user-configured maps without any hardcoded fallbacks."
  (when (context-navigator-controls-icons-available-p)
    (let* ((spec (context-navigator-controls-icons--spec-for key state))
           (provider (car-safe spec))
           (name (cdr-safe spec)))
      (when (and provider name)
        (let* ((attrs (context-navigator-controls-icons--resolve-attrs key state))
               (face (plist-get attrs :face))
               (height (plist-get attrs :height))
               (raise (plist-get attrs :raise))
               (cache-key (list key state height raise face)))
          (or (gethash cache-key context-navigator-controls-icons--cache)
              (let* ((fn (context-navigator-controls-icons--provider-fn provider))
                     (icon (and fn
                                (ignore-errors
                                  (funcall fn name
                                           :face (or (and (symbolp face) face)
                                                     (and (listp face) face))
                                           :height height
                                           ;; keep v-adjust neutral; we apply raise via `display'
                                           :v-adjust 0.0)))))
                (when (and (stringp icon) (not (string-empty-p icon)))
                  (let ((s (propertize icon 'display (list 'raise raise))))
                    (puthash cache-key s context-navigator-controls-icons--cache)
                    s)))))))))

(defun context-navigator-controls-icons-clear-cache ()
  "Clear the internal cache of rendered control icons."
  (clrhash context-navigator-controls-icons--cache))

;; Auto-refresh UI (header-line controls) when icons become available or settings/theme change.
(defun context-navigator-controls-icons--refresh-ui ()
  "Force header-line controls to rebuild after icon settings change."
  (ignore-errors (context-navigator-controls-icons-clear-cache))
  ;; Prefer a local sidebar re-render over full model refresh to avoid generation spam.
  (ignore-errors
    (let ((buf (get-buffer "*context-navigator*")))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          ;; Invalidate view and headerline caches
          (setq-local context-navigator-render--last-hash nil)
          (setq-local context-navigator-view--last-render-key nil)
          (setq-local context-navigator-headerline--cache-key nil)
          (setq-local context-navigator-headerline--cache-str nil)
          (when (fboundp 'context-navigator-view--render-if-visible)
            (context-navigator-view--render-if-visible))))))
  ;; Also refresh Multifile header-line if it's open.
  (ignore-errors
    (let ((buf (get-buffer "*Context Multifile View*")))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (force-mode-line-update t)))))
  ;; As a fallback, force header-line/modeline refresh.
  (force-mode-line-update t))

;; When all-the-icons loads later in the session, rebuild controls with icons.
(with-eval-after-load 'all-the-icons
  (context-navigator-controls-icons--refresh-ui))

;; React to variable changes (customize/setq): enable/disable icons, map/face, sizes.
(when (fboundp 'add-variable-watcher)
  (dolist (sym '(context-navigator-controls-use-graphic-icons
                 context-navigator-controls-icon-map
                 context-navigator-controls-icon-face-map
                 context-navigator-controls-icon-height
                 context-navigator-controls-icon-raise))
    (add-variable-watcher
     sym
     (lambda (_sym _newval _op _where)
       (context-navigator-controls-icons--refresh-ui)))))

;; Refresh after theme changes so colors/sizes re-render properly.
(when (boundp 'after-enable-theme-functions)
  (add-hook 'after-enable-theme-functions
            (lambda (&rest _)
              (context-navigator-controls-icons--refresh-ui))))

(provide 'context-navigator-controls-icons)

;;; context-navigator-controls-icons.el ends here
