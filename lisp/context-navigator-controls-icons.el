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
    (razor . (faicon . "filter"))
    (stats . (material . "assessment"))
    (multifile . (faicon . "list"))
    (multi-group . ((on . (material . "group_work"))
                    (off . (material . "group_work"))))
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
  '((undo . (:foreground "SteelBlue4"))
    (redo . (:foreground "SteelBlue4"))
    (multi-group . (:foreground "green4"))
    (push-now . (:foreground "green4"))
    (stats . (:foreground "green4"))
    (toggle-all-gptel . (:foreground "green4"))
    (razor . (:foreground "magenta3"))
    (multifile . (:foreground "orange3"))
    (open-buffers . (:foreground "orange3"))
    (close-buffers . (:foreground "orange3"))
    (clear-gptel . (:foreground "gray"))
    (clear-group . (:foreground "tomato"))
    (auto-project . (:raise -0.2)))
  "Optional per-key face overrides for control icons.
Each entry is either a face symbol or a plist like (:foreground \"...\" [:height N] [:raise N]).
For stateful toggles (push, auto-project) use `context-navigator-controls-toggle-on-face` and
`context-navigator-controls-toggle-off-face` variables to set on/off colors."
  :type '(alist :key-type symbol
                :value-type (choice face (plist :key-type symbol :value-type sexp)))
  :group 'context-navigator-controls-icons)

(defcustom context-navigator-controls-icon-local-prefixes
  '("mf-")
  "List of key name prefixes treated as \"local-only\" icon keys (e.g. Multifile header controls).
Audit and consistency checks can treat keys with these prefixes as local UI-only and not
flag them as unused in the global header-line registry."
  :type '(repeat string)
  :group 'context-navigator-controls-icons)

(defcustom context-navigator-controls-toggle-on-face
  '(:foreground "gray85")
  "Face attributes or symbol for toggle icons when enabled (ON).
Takes precedence for `push` and `auto-project` controls when STATE is 'on'."
  :type '(choice face (plist :key-type symbol :value-type sexp))
  :group 'context-navigator-controls-icons)

(defcustom context-navigator-controls-toggle-off-face
  '(:foreground "gray60")
  "Face attributes or symbol for toggle icons when disabled (OFF).
Takes precedence for `push` and `auto-project` controls when STATE is 'off'."
  :type '(choice face (plist :key-type symbol :value-type sexp))
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
  "Return non-nil if graphic icons can be used in the current context.

Avoid calling `require' in hot paths; rely on `featurep' and refresh when
all-the-icons loads later (see with-eval-after-load below)."
  (and context-navigator-controls-use-graphic-icons
       (display-graphic-p)
       (featurep 'all-the-icons)))

(defun context-navigator-controls-icon (key &optional state)
  "Return a propertized icon string for control KEY and optional STATE.
Returns nil if icons are not available or spec cannot be resolved.

Known KEYS by default:
  push (stateful: on/off)
  auto-project (stateful: on/off)
  undo redo push-now open-buffers close-buffers clear-group clear-gptel toggle-all-gptel

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
           ;; Use custom face for toggles, from defcustom, if present
           (final-face
            (cond
             ((eq key 'push)
              (cond
               ((eq state 'on)  context-navigator-controls-toggle-on-face)
               ((eq state 'off) context-navigator-controls-toggle-off-face)
               (t nil)))
             ((eq key 'auto-project)
              (cond
               ((eq state 'on)  context-navigator-controls-toggle-on-face)
               ((eq state 'off) context-navigator-controls-toggle-off-face)
               (t nil)))
             (override override)
             (t base-face)))
           (raise (or (and (listp override) (plist-get override :raise))
                      context-navigator-controls-icon-raise))
           (cache-key (list key state
                            context-navigator-controls-icon-height
                            raise
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
                                 :v-adjust 0.0))))
                   ;; Fallbacks for tricky keys (e.g. razor, mf-edit-all) across providers
                   (icon
                    (or icon
                        ;; Razor: try several providers to increase chances
                        (when (eq key 'razor)
                          (let ((alts '((material . "content_cut")
                                        (octicon  . "scissors")
                                        (faicon   . "scissors"))))
                            (cl-loop for sp in alts
                                     for pf = (context-navigator-controls-icons--provider-fn (car sp))
                                     for nm = (cdr sp)
                                     for s = (and pf (ignore-errors
                                                       (funcall pf nm
                                                                :face (or (and (symbolp final-face) final-face)
                                                                          (and (listp final-face) final-face))
                                                                :height context-navigator-controls-icon-height
                                                                :v-adjust 0.0)))
                                     when (and (stringp s) (not (string-empty-p s)))
                                     return s)))
                        ;; Edit-all: prefer material "edit", but try octicon/faicon fallbacks too
                        (when (eq key 'mf-edit-all)
                          (let ((alts '((material . "edit")
                                        (octicon  . "pencil")
                                        (faicon   . "pencil")
                                        (faicon   . "pencil-square-o"))))
                            (cl-loop for sp in alts
                                     for pf = (context-navigator-controls-icons--provider-fn (car sp))
                                     for nm = (cdr sp)
                                     for s = (and pf (ignore-errors
                                                       (funcall pf nm
                                                                :face (or (and (symbolp final-face) final-face)
                                                                          (and (listp final-face) final-face))
                                                                :height context-navigator-controls-icon-height
                                                                :v-adjust 0.0)))
                                     when (and (stringp s) (not (string-empty-p s)))
                                     return s))))))
              (when (and (stringp icon) (not (string-empty-p icon)))
                ;; Apply per-icon raise if specified, otherwise use default.
                (let* ((s (propertize icon
                                      'display
                                      (list 'raise raise))))
                  (puthash cache-key s context-navigator-controls-icons--cache)
                  s))))))))

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
