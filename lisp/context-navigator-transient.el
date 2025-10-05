;;; context-navigator-transient.el --- Transient menu for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Global transient on C-c n:
;;  n: toggle sidebar
;;  p: switch to current buffer's project
;;  a: add current file/region/buffer/dired selection (minimal)
;;  g: groups list
;;  s: save context
;;  l: load context
;;  u: unload context
;;  x: toggle push → gptel
;;  T: toggle auto-project
;;  P: push now
;;  C: clear gptel

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
  :group 'context-navigator-add)

;;;###autoload
(transient-define-prefix context-navigator-transient ()
  "Context Navigator"
  [["Panel/Project"
    ("n" (lambda () (context-navigator-i18n :tr-toggle-sidebar)) context-navigator-toggle)
    ("S" (lambda () (context-navigator-i18n :tr-display-mode)) context-navigator-display-mode-toggle)
    ("p" (lambda () (context-navigator-i18n :tr-switch-project)) context-navigator-switch-to-current-buffer-project)]
   ["Context/Groups"
    ("g" (lambda () (context-navigator-i18n :tr-groups-list)) context-navigator-view-show-groups)
    ("E" (lambda () (context-navigator-i18n :clear-group)) context-navigator-context-clear-current-group)]
   ["Actions"
    ("a" (lambda () (context-navigator-i18n :tr-add-universal)) context-navigator-add-universal)
    ("f" (lambda () (context-navigator-i18n :add-from-minibuf)) context-navigator-add-from-minibuffer)
    ("t" (lambda () (context-navigator-i18n :add-from-text)) context-navigator-add-from-text)
    ("b" (lambda () (or (ignore-errors (context-navigator-i18n :select-by-name)) "Select by name"))
     context-navigator-select-by-name)
    ("o" (lambda () (context-navigator-i18n :tr-open-buffers)) context-navigator-view-open-all-buffers)
    ("m" (lambda () (context-navigator-i18n :tr-multifile)) context-navigator-multifile-open)]
   ["Control"
    ("G" (lambda () (context-navigator-i18n :toggle-multi-group)) context-navigator-view-toggle-multi-group)
    ("V" (lambda () (context-navigator-i18n :tr-toggle-push)) context-navigator-toggle-push-to-gptel)
    ("A" (lambda () (context-navigator-i18n :tr-toggle-auto)) context-navigator-toggle-auto-project-switch)
    ("M" (lambda () (context-navigator-i18n :enable-all-gptel)) context-navigator-view-enable-all-gptel)
    ("U" (lambda () (context-navigator-i18n :disable-all-gptel)) context-navigator-view-disable-all-gptel)
    ("T" (lambda () (context-navigator-i18n :toggle-all-gptel)) context-navigator-view-toggle-all-gptel)
    ("P" (lambda () (context-navigator-i18n :tr-push-now)) context-navigator-push-to-gptel-now)
    ("C" (lambda () (context-navigator-i18n :clear-gptel)) context-navigator-clear-gptel-now)
    ("R" (lambda () (context-navigator-i18n :tr-razor)) context-navigator-razor-run
     :if (lambda () (derived-mode-p 'org-mode)))]
   [:description (lambda () (context-navigator-i18n :tr-logs))
                 ("D" (lambda () (context-navigator-i18n :tr-logs-toggle)) context-navigator-log-toggle)
                 ("L" (lambda () (context-navigator-i18n :tr-logs-open)) context-navigator-log-open)
                 ("K" (lambda () (context-navigator-i18n :tr-logs-clear)) context-navigator-log-clear)
                 ("V" (lambda () (context-navigator-i18n :tr-logs-set-level)) context-navigator-log-set-level)
                 ("F" (lambda () (context-navigator-i18n :tr-logs-toggle-file)) context-navigator-log-toggle-file-persistence)]])

;;;###autoload
(transient-define-prefix context-navigator-view-transient ()
  "Navigator menu"
  [[:description (lambda () (context-navigator-i18n :tr-navigate))
                 ("RET" (lambda () (context-navigator-i18n :help-activate)) context-navigator-view-activate-safe)
                 ("SPC" (lambda () (context-navigator-i18n :help-preview))  context-navigator-view-preview)
                 ("n"   (lambda () (context-navigator-i18n :help-next-item))     context-navigator-view-next-item)
                 ("p"   (lambda () (context-navigator-i18n :help-previous-item)) context-navigator-view-previous-item)
                 ("j"   (lambda () (context-navigator-i18n :help-next-item))     context-navigator-view-next-item)
                 ("k"   (lambda () (context-navigator-i18n :help-previous-item)) context-navigator-view-previous-item)
                 ("b"   (lambda () (or (ignore-errors (context-navigator-i18n :select-by-name)) "Select by name"))
                  context-navigator-select-by-name)
                 ("h"   (lambda ()
                          (if (eq context-navigator-view--mode 'groups)
                              (context-navigator-i18n :groups-help-back)
                            (context-navigator-i18n :items-help-view-groups)))
                  context-navigator-view-go-up)]
   [:description (lambda () (context-navigator-i18n :tr-items))
                 :if (lambda () (eq context-navigator-view--mode 'items))
                 ("t" (lambda () (context-navigator-i18n :help-toggle-gptel)) context-navigator-view-toggle-enabled)
                 ("m" (lambda () (context-navigator-i18n :help-toggle-gptel)) context-navigator-view-toggle-enabled)
                 ("d" (lambda () (context-navigator-i18n :help-delete))       context-navigator-view-delete-dispatch)
                 ("g" (lambda () (context-navigator-i18n :help-refresh))      context-navigator-view-refresh-dispatch)
                 ("o" (lambda ()
                        (cl-destructuring-bind (n . plus) (context-navigator-view--openable-count-get)
                          (format "%s (%d%s)" (context-navigator-i18n :open-buffers) n (if plus "+" ""))))
                  context-navigator-view-open-all-buffers)
                 ("K" (lambda ()
                        (let ((n (length (context-navigator-view--collect-closable-buffers))))
                          (format "%s (%d)" (context-navigator-i18n :close-buffers) n)))
                  context-navigator-view-close-all-buffers)
                 ("R" (lambda () (context-navigator-i18n :tr-razor))
                  context-navigator-view-razor-run
                  :if (lambda ()
                        (cl-some (lambda (b)
                                   (with-current-buffer b
                                     (derived-mode-p 'org-mode)))
                                 (buffer-list)))) ]
   [:description (lambda () (context-navigator-i18n :tr-groups))
                 :if (lambda () (eq context-navigator-view--mode 'groups))
                 ("RET" (lambda () (context-navigator-i18n :groups-help-open))   context-navigator-view-activate-safe)
                 ("+"   (lambda () (context-navigator-i18n :groups-help-add))    context-navigator-view-group-create)
                 ("R"   (lambda () (context-navigator-i18n :groups-help-rename)) context-navigator-view-group-rename)
                 ("E"   (lambda ()
                          (or (ignore-errors (context-navigator-i18n :groups-help-edit-description))
                              "Edit description"))
                  context-navigator-view-group-edit-description)
                 ("C"   (lambda () (context-navigator-i18n :groups-help-copy))   context-navigator-view-group-duplicate)
                 ("D"   (lambda () (context-navigator-i18n :groups-help-delete)) context-navigator-view-delete-dispatch)
                 ("g"   (lambda () (context-navigator-i18n :groups-help-refresh)) context-navigator-view-refresh-dispatch)]
   [:description (lambda () (context-navigator-i18n :tr-session))
                 ("G" (lambda ()
                        (format (context-navigator-i18n :push-state)
                                (context-navigator-i18n
                                 (if (and (boundp 'context-navigator--push-to-gptel)
                                          context-navigator--push-to-gptel) :on :off))))
                  context-navigator-view-toggle-push)
                 ("A" (lambda ()
                        (format (context-navigator-i18n :auto-project-state)
                                (context-navigator-i18n
                                 (if (and (boundp 'context-navigator--auto-project-switch)
                                          context-navigator--auto-project-switch) :on :off))))
                  context-navigator-view-toggle-auto-project)
                 ("P" (lambda () (context-navigator-i18n :push-now))     context-navigator-view-push-now)
                 ("E" (lambda () (context-navigator-i18n :clear-group)) context-navigator-view-clear-group)
                 ("X" (lambda () (context-navigator-i18n :tr-unload))   context-navigator-context-unload)]
   ["Help/Exit"
    ("H" (lambda () (context-navigator-i18n :help-help)) context-navigator-view-help)
    ("q" (lambda () (context-navigator-i18n :help-quit)) context-navigator-view-quit)]])

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
      ;; Fallback: small fixed-height pop-up window below the selected window.
      (let ((transient-display-buffer-action
             '(display-buffer-below-selected
               . ((side . bottom)
                  (slot . 0)
                  ;; Use a small fixed height (lines). Tweak to taste.
                  (window-height . 12)
                  ;; Keep it from being expanded by other commands.
                  (preserve-size . (t . nil))
                  (window-parameters . ((no-other-window . t)
                                        (no-delete-other-windows . t)))))))
        (apply orig-fun args)))))

(advice-add 'context-navigator-transient :around #'context-navigator--with-small-transient)
(advice-add 'context-navigator-view-transient :around #'context-navigator--with-small-transient)

(provide 'context-navigator-transient)
;;; context-navigator-transient.el ends here
