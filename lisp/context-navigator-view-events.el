;;; context-navigator-view-events.el --- View event subscriptions/refresh -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Buffer-local subscriptions and small helpers for the Navigator view:
;; - subscribe to model/context/project/group events
;; - manage lightweight loading indicator state
;; - expose a simple refresh entry-point for dispatchers
;;
;; Handlers operate on the Navigator buffer (when live) to avoid
;; mutating globals and to keep UI work localized.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-events)
(require 'context-navigator-core)
(require 'context-navigator-view-constants)

;; Buffer-local state from the main view (declared there)
(defvar context-navigator-view--subs nil)
(defvar context-navigator-view--load-progress nil)
(defvar context-navigator-view--mode 'items)
(defvar context-navigator-view--last-render-key nil)
(defvar context-navigator-view--groups nil)

;; Small helpers provided by the main view
(declare-function context-navigator-view--schedule-render "context-navigator-view" ())
(declare-function context-navigator-view--invalidate-openable "context-navigator-view" ())
(declare-function context-navigator-view--spinner-start "context-navigator-view" ())
(declare-function context-navigator-view--spinner-stop "context-navigator-view" ())

;;;###autoload
(defun context-navigator-view-refresh ()
  "Recompute core indices and schedule a sidebar render."
  (interactive)
  (ignore-errors (context-navigator-refresh))
  (context-navigator-view--schedule-render))

(defun context-navigator-view--subscribe-model-events ()
  "Subscribe to model refresh; update counters and schedule UI render."
  (push (context-navigator-events-subscribe
         :model-refreshed
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (ignore-errors (context-navigator-view--invalidate-openable))
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs))

(defun context-navigator-view--subscribe-load-events ()
  "Subscribe to context load lifecycle to show a lightweight preloader."
  ;; Start: mark progress, kick spinner, force a render
  (push (context-navigator-events-subscribe
         :context-load-start
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq context-navigator-view--load-progress t)
                 (ignore-errors (context-navigator-view--spinner-start))
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs)
  ;; Step: update (pos . total), keep spinner running, re-render
  (push (context-navigator-events-subscribe
         :context-load-step
         (lambda (_root pos total)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq context-navigator-view--load-progress
                       (and (numberp pos) (numberp total) (cons pos total)))
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs)
  ;; Done: clear progress, stop spinner, re-render
  (push (context-navigator-events-subscribe
         :context-load-done
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq context-navigator-view--load-progress nil)
                 (ignore-errors (context-navigator-view--spinner-stop))
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs))

(defun context-navigator-view--subscribe-groups-events ()
  "Subscribe to group switch lifecycle; keep view mode and render in sync."
  (push (context-navigator-events-subscribe
         :group-switch-start
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq context-navigator-view--mode 'items)
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs)
  (push (context-navigator-events-subscribe
         :group-switch-done
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs)
  ;; Keep the groups list in the view's buffer-local cache
  (push (context-navigator-events-subscribe
         :groups-list-updated
         (lambda (_root groups)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq context-navigator-view--groups (and (listp groups) groups))
                 (when (eq context-navigator-view--mode 'groups)
                   (context-navigator-view--schedule-render)))))))
        context-navigator-view--subs)
  ;; Re-render headerline/controls on selection change (affects push/MG gating)
  (push (context-navigator-events-subscribe
         :group-selection-changed
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs))

(defun context-navigator-view--subscribe-project-events ()
  "Subscribe to project switch; drop render cache and re-render."
  (push (context-navigator-events-subscribe
         :project-switch
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 ;; ensure the next render is not short-circuited
                 (setq context-navigator-view--last-render-key nil)
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs))

;; High-level installers (moved from view.el progressively)

;; Variables declared in the main view (buffer-local there)
(defvar context-navigator-view--subs)
(defvar context-navigator-view--status-post-cmd-fn)
(defvar context-navigator-view--gptel-poll-timer)
(defvar context-navigator-view--buflist-fn nil)
(defvar context-navigator-view--winselect-fn nil)

;; Small helpers now live here (moved from view.el)

(defun context-navigator-view--install-buffer-list-hook ()
  "Install buffer-list update hook (idempotent).

When the global buffer list changes, recompute openable counters
for the Navigator buffer. We install a global hook and filter inside
the handler for the live sidebar buffer; removal is handled in
`context-navigator-view--remove-subs'."
  (unless context-navigator-view--buflist-fn
    (setq context-navigator-view--buflist-fn
          (lambda ()
            (let ((buf (get-buffer context-navigator-view--buffer-name)))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (ignore-errors (context-navigator-view-counters-invalidate)))))))
    ;; Global hook (filter inside handler by the Navigator buffer)
    (add-hook 'buffer-list-update-hook context-navigator-view--buflist-fn)))

(defun context-navigator-view--install-window-select-hook ()
  "Install window-selection-change hook (idempotent).

When the selected window changes, schedule a render of the sidebar so
it can become responsive to focus changes."
  (unless context-navigator-view--winselect-fn
    (setq context-navigator-view--winselect-fn
          (lambda (_frame)
            (let ((buf (get-buffer context-navigator-view--buffer-name)))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (ignore-errors (context-navigator-view--schedule-render)))))))
    (add-hook 'window-selection-change-functions context-navigator-view--winselect-fn)))

(defun context-navigator-view--initial-compute-counters ()
  "Perform initial computation of openable counters (safe no-op when modules missing)."
  (ignore-errors (context-navigator-view-counters-refresh-openable)))

;; Keep declaration for spinner-stop (provided by the main view)
(declare-function context-navigator-view--spinner-stop "context-navigator-view" ())

;;;###autoload
(defun context-navigator-view-events-install ()
  "Install Navigator view subscriptions, hooks and timers (idempotent).
Sets up model/load/groups/project events, gptel cache/events, buffer/window
hooks and a post-command updater. Also starts optional gptel polling."
  (unless context-navigator-view--subs
    ;; Model and loading lifecycle
    (context-navigator-view--subscribe-model-events)
    (context-navigator-view--subscribe-load-events)
    (context-navigator-view--subscribe-groups-events)
    (context-navigator-view--subscribe-project-events)
    ;; gptel events + initial cache (advices are installed by gptel-bridge)
    (context-navigator-view--subscribe-gptel-events)
    (context-navigator-view--init-gptel-cache)
    ;; Hooks and initial compute
    (when (fboundp 'context-navigator-view--install-buffer-list-hook)
      (context-navigator-view--install-buffer-list-hook))
    (when (fboundp 'context-navigator-view--install-window-select-hook)
      (context-navigator-view--install-window-select-hook))
    ;; Update modeline status as point moves inside the buffer
    (setq context-navigator-view--status-post-cmd-fn
          (lambda ()
            (when (and (eq major-mode 'context-navigator-view-mode)
                       (boundp 'context-navigator-view-modeline-enable)
                       context-navigator-view-modeline-enable)
              (force-mode-line-update nil))))
    (add-hook 'post-command-hook context-navigator-view--status-post-cmd-fn nil t)
    (when (fboundp 'context-navigator-view--initial-compute-counters)
      (context-navigator-view--initial-compute-counters))
    ;; Optional polling
    (when (fboundp 'context-navigator-view--start-gptel-poll-timer)
      (context-navigator-view--start-gptel-poll-timer))
    ;; Ensure cleanup on buffer kill (buffer-local)
    (add-hook 'kill-buffer-hook #'context-navigator-view-events-remove nil t)))

;;;###autoload
(defun context-navigator-view-events-remove ()
  "Tear down Navigator view subscriptions, hooks and timers (idempotent)."
  (when context-navigator-view--subs
    (mapc #'context-navigator-events-unsubscribe context-navigator-view--subs)
    (setq context-navigator-view--subs nil))
  ;; Cancel timers and drop cached counters/spinner
  (when (fboundp 'context-navigator-view--spinner-stop)
    (context-navigator-view--spinner-stop))
  (when (boundp 'context-navigator-view--gptel-poll-timer)
    (when (timerp context-navigator-view--gptel-poll-timer)
      (cancel-timer context-navigator-view--gptel-poll-timer))
    (setq context-navigator-view--gptel-poll-timer nil))
  ;; Remove post-command status updater if installed (buffer-local)
  (when context-navigator-view--status-post-cmd-fn
    (remove-hook 'post-command-hook context-navigator-view--status-post-cmd-fn t)
    (setq context-navigator-view--status-post-cmd-fn nil))
  t)

(provide 'context-navigator-view-events)
;;; context-navigator-view-events.el ends here
