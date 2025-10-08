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
(require 'context-navigator-persist)
(require 'context-navigator-model)
(require 'context-navigator-log)

;; Buffer-local state from the main view (declared there)
(defvar context-navigator-view--subs nil)
(defvar context-navigator-view--load-progress nil)
(defvar context-navigator-view--mode 'items)
(defvar context-navigator-view--last-render-key nil)
(defvar context-navigator-view--groups nil)
(defvar context-navigator-view--gptel-batch-start-time nil
  "Buffer-local timestamp (float-time) when a gptel batch started; used to show batch duration in UI.")

;; Small helpers provided by the main view
(declare-function context-navigator-view--schedule-render "context-navigator-view" ())
(declare-function context-navigator-view--invalidate-openable "context-navigator-view" ())
(declare-function context-navigator-view--spinner-start "context-navigator-view" ())
(declare-function context-navigator-view--spinner-stop "context-navigator-view" ())

;; Navigation helpers used to find nearest itemish anchors
(declare-function context-navigator-view--find-prev-itemish-pos "context-navigator-view-navigation" (&optional start))
(declare-function context-navigator-view--find-next-itemish-pos "context-navigator-view-navigation" (&optional start))

(defun context-navigator-view--track-cursor-post-cmd ()
  "Buffer-local post-command hook: track last cursor anchor in items view.
Stores item key or \"..\" into `context-navigator-view--last-cursor-key'."
  (when (eq context-navigator-view--mode 'items)
    (let ((key nil))
      (cond
       ((get-text-property (point) 'context-navigator-item)
        (let ((it (get-text-property (point) 'context-navigator-item)))
          (setq key (and it (context-navigator-model-item-key it)))))
       ;; Up-line removed
       (t
        ;; find nearest itemish (prev/next) and prefer the closest by line distance
        (let* ((here (line-number-at-pos))
               (pp (context-navigator-view--find-prev-itemish-pos (point)))
               (np (context-navigator-view--find-next-itemish-pos (point)))
               (dprev (and pp (save-excursion (goto-char pp) (line-number-at-pos))))
               (dnext (and np (save-excursion (goto-char np) (line-number-at-pos))))
               (choose
                (cond
                 ((and dprev dnext)
                  (if (<= (abs (- here dprev)) (abs (- dnext here))) pp np))
                 (dprev pp)
                 (dnext np)
                 (t nil))))
          (when choose
            (save-excursion
              (goto-char choose)
              (cond
               ((get-text-property (point) 'context-navigator-item)
                (let ((it (get-text-property (point) 'context-navigator-item)))
                  (setq key (and it (context-navigator-model-item-key it)))))
               ((get-text-property (point) 'context-navigator-groups-up)
                (setq key ".."))))))))
      (when (and (stringp key) (not (string-empty-p key)))
        (setq context-navigator-view--last-cursor-key key)))))

(defun context-navigator-view--save-items-cursor-state ()
  "Capture current items cursor (key or \"..\") and persist per group in state.el."
  (when (and (eq major-mode 'context-navigator-view-mode)
             (eq context-navigator-view--mode 'items))
    (let* ((key (and (stringp context-navigator-view--last-cursor-key)
                     (not (string-empty-p context-navigator-view--last-cursor-key))
                     context-navigator-view--last-cursor-key)))
      (unless key
        (cond
         ((get-text-property (point) 'context-navigator-item)
          (let ((it (get-text-property (point) 'context-navigator-item)))
            (setq key (and it (context-navigator-model-item-key it)))))
         ;; Up-line removed
         (t
          (let* ((pp (context-navigator-view--find-prev-itemish-pos (point)))
                 (np (context-navigator-view--find-next-itemish-pos (point)))
                 (choose (or pp np)))
            (when choose
              (save-excursion
                (goto-char choose)
                (cond
                 ((get-text-property (point) 'context-navigator-item)
                  (let ((it (get-text-property (point) 'context-navigator-item)))
                    (setq key (and it (context-navigator-model-item-key it)))))
                 ((get-text-property (point) 'context-navigator-groups-up)
                  (setq key "..")))))))))
      ;; Up-line removed: if no key, leave nil to avoid jumping to a non-existent anchor
      (setq key (or key ""))
      (let* ((st (ignore-errors (context-navigator--state-get)))
             (root (and st (ignore-errors (context-navigator-state-last-project-root st))))
             (slug (and st (ignore-errors (context-navigator-state-current-group-slug st)))))
        (when (and (stringp slug) (not (string-empty-p slug)))
          (ignore-errors
            (context-navigator-persist-state-put-last-pos root slug (list :key key)))))
      key)))

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
                 ;; Predictive lamps: use enabled items keys immediately; gptel events will refine.
                 (ignore-errors
                   (let* ((st (context-navigator--state-get))
                          (items (and st (context-navigator-state-items st)))
                          (enabled (and (listp items)
                                        (cl-remove-if-not #'context-navigator-item-enabled items)))
                          (keys (mapcar #'context-navigator-model-item-key enabled)))
                     (setq-local context-navigator-view--gptel-keys keys
                                 context-navigator-view--gptel-keys-hash (sxhash-equal keys))
                     (when (fboundp 'context-navigator-debug)
                       (context-navigator-debug :debug :ui
                                                "predictive lamps: enabled=%d"
                                                (length (or keys '()))))))
                 (ignore-errors (context-navigator-view--invalidate-openable))
                 ;; Rebuild modeline toolbar cache so redisplay stays cheap
                 (when (fboundp 'context-navigator-modeline--rebuild-menu-cache)
                   (ignore-errors (context-navigator-modeline--rebuild-menu-cache)))
                 (force-mode-line-update t)
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs))

;; Handlers for load lifecycle and gptel batch events (small and focused)

(defun context-navigator-view--on-context-load-start (&rest _)
  "Handler: context load started."
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq context-navigator-view--load-progress t)
        (ignore-errors (context-navigator-view--spinner-start))
        (context-navigator-view--schedule-render)))))

(defun context-navigator-view--on-context-load-step (_root pos total)
  "Handler: context load progress step (debounced to avoid render storms)."
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq context-navigator-view--load-progress
              (and (numberp pos) (numberp total) (cons pos total)))
        (context-navigator-events-debounce
         :preloader-render 0.12
         (lambda ()
           (let ((b (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p b)
               (with-current-buffer b
                 (context-navigator-view--schedule-render))))))))))

(defun context-navigator-view--on-context-load-done (&rest _)
  "Handler: context load finished."
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq context-navigator-view--load-progress nil)
        (ignore-errors (context-navigator-view--spinner-stop))
        (context-navigator-view--schedule-render)))))

(defun context-navigator-view--on-gptel-change-events (subtype &rest args)
  "Handler: gptel batch lifecycle events."
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (pcase subtype
          (:batch-start
           (ignore-errors (context-navigator-view--spinner-start))
           (context-navigator-view--schedule-render)
           (when (and (integerp (car-safe args)))
             (let ((total (car args))
                   (start (cadr args)))
               (setq context-navigator-view--gptel-batch-start-time
                     (or (and (numberp start) start) (float-time)))
               (context-navigator-ui-info :push-state
                                          (format "Pushing %d items..." total)))))
          (:batch-done
           (ignore-errors (context-navigator-view--spinner-stop))
           (context-navigator-view--schedule-render)
           (let ((total (car-safe args))
                 (now (float-time))
                 (start (or context-navigator-view--gptel-batch-start-time 0.0)))
             (when (and (integerp total))
               (context-navigator-ui-info :pushed-items total))
             (when (and (numberp start) (> start 0.0))
               (let ((dur (format "%.2fs" (max 0.0 (- now start)))))
                 (context-navigator-ui-info :pushed-items-done (or total 0) dur)))
             (setq context-navigator-view--gptel-batch-start-time nil)))
          (:batch-cancel
           (ignore-errors (context-navigator-view--spinner-stop))
           (setq context-navigator-view--gptel-batch-start-time nil)
           (context-navigator-view--schedule-render))
          (_ nil))))))

(defun context-navigator-view--subscribe-load-events ()
  "Subscribe to context load lifecycle and gptel batch events."
  ;; Start: mark progress, kick spinner, force a render
  (push (context-navigator-events-subscribe
         :context-load-start #'context-navigator-view--on-context-load-start)
        context-navigator-view--subs)
  ;; Step: update (pos . total), keep spinner running, re-render
  (push (context-navigator-events-subscribe
         :context-load-step #'context-navigator-view--on-context-load-step)
        context-navigator-view--subs)
  ;; Done: clear progress, stop spinner, re-render
  (push (context-navigator-events-subscribe
         :context-load-done #'context-navigator-view--on-context-load-done)
        context-navigator-view--subs)
  ;; GPTel batch lifecycle
  (push (context-navigator-events-subscribe
         :gptel-change #'context-navigator-view--on-gptel-change-events)
        context-navigator-view--subs))

(defun context-navigator-view--subscribe-groups-events ()
  "Subscribe to group switch lifecycle; keep view mode and render in sync."
  (push (context-navigator-events-subscribe
         :group-switch-start
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (ignore-errors (context-navigator-view--save-items-cursor-state))
                 (setq context-navigator-view--mode 'items)
                 ;; one-shot restore on first full items render after load
                 (setq context-navigator-view--restore-once t)
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
                   (when (fboundp 'context-navigator-view--invalidate-render-caches)
                     (context-navigator-view--invalidate-render-caches t))
                   ;; Немедленная перерисовка (достаточно одной перерисовки без дополнительного дебаунса)
                   (context-navigator-view--render-if-visible)))))))
        context-navigator-view--subs)
  ;; Re-render headerline/controls on selection change (affects push/MG gating)
  (push (context-navigator-events-subscribe
         :group-selection-changed
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 ;; Gate auto-push softly when selection changes (no heavy IO)
                 (when (fboundp 'context-navigator-autopush-gate-on-selection)
                   (ignore-errors (context-navigator-autopush-gate-on-selection)))
                 ;; Если push→gptel включён и MG активен с непустым выбором — пушим агрегат сразу.
                 (when (and (boundp 'context-navigator--push-to-gptel)
                            context-navigator--push-to-gptel)
                   (let* ((st   (ignore-errors (context-navigator--state-get)))
                          (root (and st (ignore-errors (context-navigator-state-last-project-root st))))
                          (ps   (and root (ignore-errors (context-navigator-persist-state-load root))))
                          (mg   (and (listp ps) (plist-member ps :multi) (plist-get ps :multi)))
                          (sel  (ignore-errors (context-navigator--selected-group-slugs-for-root root))))
                     (cond
                      ((and mg (listp sel) (> (length sel) 0))
                       (ignore-errors (context-navigator-apply-groups-now root sel)))
                      (mg
                       (ignore-errors (context-navigator-gptel-clear-all-now))))))
                 ;; Invalidate render caches so selection lamps update immediately
                 (when (fboundp 'context-navigator-view--invalidate-render-caches)
                   (context-navigator-view--invalidate-render-caches t))
                 ;; Rebuild modeline toolbar cache (menu depends on selection/MG)
                 (when (fboundp 'context-navigator-modeline--rebuild-menu-cache)
                   (ignore-errors (context-navigator-modeline--rebuild-menu-cache)))
                 (force-mode-line-update t)
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
                 (ignore-errors (context-navigator-view--save-items-cursor-state))
                 ;; ensure the next render is not short-circuited
                 (setq context-navigator-view--last-render-key nil)
                 ;; force restore once when first items render comes after load
                 (setq context-navigator-view--restore-once t)
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs))

;; High-level installers (moved from view.el progressively)

;; Variables declared in the main view (buffer-local there)
(defvar context-navigator-view--subs)
(defvar context-navigator-view--status-post-cmd-fn)
(defvar context-navigator-view--gptel-poll-timer)
(defvar context-navigator-view--buflist-fn nil)
(defvar context-navigator-view--winselect-fn nil)
(defvar context-navigator-view--last-modeline-key nil)

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

Do a lightweight refresh only when the Navigator buffer becomes the selected
window. Не сбрасываем курсор: не инвалидируем кэши и не используем
schedule-render здесь."
  (unless context-navigator-view--winselect-fn
    (setq context-navigator-view--winselect-fn
          (lambda (_frame)
            (let ((buf (get-buffer context-navigator-view--buffer-name)))
              (when (and (buffer-live-p buf)
                         (eq (window-buffer (selected-window)) buf))
                (with-current-buffer buf
                  ;; Обновить статус/модельную строку без тяжёлого рендера
                  (ignore-errors (force-mode-line-update nil))
                  ;; Мягкая перерисовка, без сброса кэшей и позиции
                  (ignore-errors (context-navigator-view--render-if-visible)))))))
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
            ;; Обновлять модельную строку только при смене якоря (ключа элемента).
            (when (and (eq major-mode 'context-navigator-view-mode)
                       (boundp 'context-navigator-view-modeline-enable)
                       context-navigator-view-modeline-enable)
              (let* ((key (or (and (stringp context-navigator-view--last-cursor-key)
                                   (not (string-empty-p context-navigator-view--last-cursor-key))
                                   context-navigator-view--last-cursor-key)
                              (let ((it (get-text-property (point) 'context-navigator-item)))
                                (and it (context-navigator-model-item-key it))))))
                (unless (equal key context-navigator-view--last-modeline-key)
                  (setq context-navigator-view--last-modeline-key key)
                  (force-mode-line-update nil))))))
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
