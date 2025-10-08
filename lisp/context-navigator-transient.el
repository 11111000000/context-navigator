;;; context-navigator-transient.el --- Transient menu for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Global transient

;;; Code:

(require 'transient)
(require 'cl-lib)
(require 'subr-x)
(require 'dired)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-i18n)
(require 'context-navigator-add-paths)
(require 'context-navigator-add)

(require 'context-navigator-log)
(require 'context-navigator-razor)
(require 'context-navigator-util)
(require 'context-navigator-ui)
(require 'context-navigator-view-multifile)
(require 'context-navigator-transient-build)

(declare-function context-navigator-multifile-open "context-navigator-view-multifile" ())

;; Safe wrapper to avoid transient setup errors when the real command
;; isn't defined yet (e.g., during partial loads).
(defun context-navigator-view-activate-safe (&optional arg)
  "Safely activate the Navigator action at point.
Calls `context-navigator-view-activate' when available; otherwise shows a hint."
  (interactive "P")
  (if (fboundp 'context-navigator-view-activate)
      (call-interactively 'context-navigator-view-activate)
    (context-navigator-ui-info :activate-not-available)))



(defcustom context-navigator-transient-display 'auto
  "Preferred backend for displaying Context Navigator transients:
- auto     : use posframe when transient-posframe is available; otherwise a small window
- posframe : force posframe (requires the transient-posframe package)
- window   : always use a small fixed-height window below the selected window"
  :type '(choice (const auto) (const posframe) (const window))
  :group 'context-navigator)

;;;###autoload
(transient-define-prefix context-navigator-transient ()
  "Context Navigator"
  (interactive)
  (let* ((raw-groups (context-navigator-transient-build-global))
         (len        (and (listp raw-groups) (length raw-groups))))
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :info :transient
                                 "global-transient: raw-groups=%s len=%s"
                                 (and raw-groups t) len)))
    (when (or (null raw-groups) (= (or len 0) 0))
      (message "[context-navigator] global transient spec is empty (len=%S)" len))
    ;; Parse and install layout (columns), then setup without passing layout arg.
    ;; Build a raw columns group implicitly by passing a vector of subgroup vectors.
    (let* ((cols (apply #'vector raw-groups)))
      (transient--set-layout
       'context-navigator-transient
       (transient-parse-suffixes 'context-navigator-transient (list cols))))
    (transient-setup 'context-navigator-transient)))

;;;###autoload
(transient-define-prefix context-navigator-view-transient ()
  "Navigator menu"
  (interactive)
  (let* ((core (context-navigator-transient-build-view-items))
         ;; Help/Exit as a regular group vector
         (help ["Help/Exit"
                ("H" (lambda () (context-navigator-i18n :help-help)) context-navigator-view-help)
                ("q" (lambda () (context-navigator-i18n :help-quit)) context-navigator-view-quit)])
         (raw-groups (append core (list help)))
         (core-len   (and (listp core) (length core))))
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :info :transient
                                 "view-transient: mode=%s core-len(list)=%s"
                                 (and (boundp 'context-navigator-view--mode) context-navigator-view--mode)
                                 core-len)))
    (when (or (null core) (= (or core-len 0) 0))
      (message "[context-navigator] view transient core is empty (mode=%S len=%S)"
               (and (boundp 'context-navigator-view--mode) context-navigator-view--mode) core-len))
    ;; Parse and install layout (columns), then setup without passing layout arg.
    ;; Build a raw columns group implicitly by passing a vector of subgroup vectors.
    (let* ((cols (apply #'vector raw-groups)))
      (transient--set-layout
       'context-navigator-view-transient
       (transient-parse-suffixes 'context-navigator-view-transient (list cols))))
    (transient-setup 'context-navigator-view-transient)))

;;;###autoload
(transient-define-prefix context-navigator-groups-split-transient ()
  "Groups split menu"
  (interactive)
  (let* ((core (context-navigator-transient-build-groups-split))
         ;; Help/Exit as a regular group vector
         (help ["Help/Exit"
                ("H" (lambda () (context-navigator-i18n :help-help)) context-navigator-view-help)
                ("q" (lambda () (context-navigator-i18n :help-quit)) context-navigator-view-quit)])
         (raw-groups (append core (list help)))
         (core-len   (and (listp core) (length core))))
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :info :transient
                                 "groups-split-transient: core-len(list)=%s" core-len)))
    (when (or (null core) (= (or core-len 0) 0))
      (message "[context-navigator] groups-split transient core is empty (len=%S)" core-len))
    (let* ((cols (apply #'vector raw-groups)))
      (transient--set-layout
       'context-navigator-groups-split-transient
       (transient-parse-suffixes 'context-navigator-groups-split-transient (list cols))))
    (transient-setup 'context-navigator-groups-split-transient)))

;; Add logic moved to context-navigator-add.el

;; Always show our transients in a small pop-up window, regardless of previous pop-up sizes.
;; Prefer posframe (via transient-posframe) when available and allowed.
;; We locally override `transient-display-buffer-action' around our two entry commands.
(defun context-navigator--with-small-transient (orig-fun &rest args)
  "Call ORIG-FUN displaying transient via posframe when available/allowed,
otherwise use a small fixed-height window."
  (let* ((backend (or context-navigator-transient-display 'auto))
         (use-posframe (and (memq backend '(auto posframe))
                            (require 'transient-posframe nil t))))
    (if use-posframe
        (progn
          ;; Enable posframe backend for transient when package is available.
          (when (fboundp 'transient-posframe-mode)
            (funcall 'transient-posframe-mode 1))
          (apply orig-fun args))
      ;; Fallback: small fixed-height pop-up window at the bottom of the frame
      ;; (non-destructive to side windows like Navigator/Groups/Stats).
      (let ((transient-display-buffer-action
             '(display-buffer-at-bottom
               . ((window-height . 12)
                  (preserve-size . (t . nil))
                  (window-parameters . ((no-other-window . t)
                                        (no-delete-other-windows . t)))))))
        (apply orig-fun args)))))

(advice-add 'context-navigator-transient :around #'context-navigator--with-small-transient)
(advice-add 'context-navigator-view-transient :around #'context-navigator--with-small-transient)
(advice-add 'context-navigator-groups-split-transient :around #'context-navigator--with-small-transient)

(provide 'context-navigator-transient)
;;; context-navigator-transient.el ends here
