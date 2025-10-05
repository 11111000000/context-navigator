;;; context-navigator-view-actions.el --- User actions for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Extracted user-facing actions and helpers from context-navigator-view.el.
;; This module centralizes:
;; - item activation/preview
;; - toggle enabled in model
;; - toggle in gptel (add/remove)
;; - delete from model
;; - open/close buffers in batch
;; - push/auto-project toggles and push-now
;; - clear-group / clear-gptel / enable-all / toggle-all
;; - razor entry point
;;
;; The main view should require this file and stop defining these functions
;; to avoid duplicate definitions. We avoid require-cycles by declaring
;; callbacks provided by the main view (schedule/render/cache helpers).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)
(require 'context-navigator-core)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-i18n)
(require 'context-navigator-log)

;; Optional: counters (for closable-buffers)
(require 'context-navigator-view-counters)
(require 'context-navigator-view-constants)

;; Declarations (callbacks provided by the view)
(declare-function context-navigator-view--schedule-render "context-navigator-view" ())
(declare-function context-navigator-view--render-if-visible "context-navigator-view" ())
(declare-function context-navigator-view--invalidate-render-caches "context-navigator-view" (&optional also-headerline))
(declare-function context-navigator-razor-run "context-navigator-razor" ())

;; Direct access to counters to avoid extra wrappers
(declare-function context-navigator-view-counters-collect-closable "context-navigator-view-counters" ())



(defcustom context-navigator-view-toggle-advance-next nil
  "When non-nil, move point to the next item after toggling with t/m/SPC.
Defaults to nil to avoid cursor jumps and keep interaction smooth."
  :type 'boolean :group 'context-navigator)

;; Internal helpers -----------------------------------------------------------

(defun context-navigator-view--window-on-sidebar-p (&optional win)
  "Return non-nil if WIN (or selected window) is the Navigator sidebar window."
  (let ((w (or win (selected-window))))
    (and (window-live-p w)
         (eq (window-parameter w 'context-navigator-view) 'sidebar))))

(defun context-navigator-view--at-item ()
  "Return item at point (from text properties) or nil."
  (let ((it (get-text-property (point) 'context-navigator-item)))
    it))

(defun context-navigator-view--visit (preview)
  "Open item at point. If PREVIEW non-nil, show in other window.

When the sidebar buffer/window is currently selected, prefer opening targets
in another window (never replace the sidebar buffer)."
  (when-let* ((item (context-navigator-view--at-item)))
    (let* ((selected-win (selected-window))
           (is-sidebar (and (window-live-p selected-win)
                            (window-parameter selected-win 'context-navigator-view)
                            (eq (window-buffer selected-win) (current-buffer))))
           (prefer-other (or preview is-sidebar)))
      (context-navigator-visit-item item prefer-other))))

;; Public actions -------------------------------------------------------------

;;;###autoload
(defun context-navigator-view-visit ()
  "Visit item at point."
  (interactive)
  (context-navigator-view--visit nil))

;;;###autoload
(defun context-navigator-view-preview ()
  "Preview item at point in other window."
  (interactive)
  (context-navigator-view--visit t))

;;;###autoload
(defun context-navigator-view-toggle-enabled ()
  "Toggle inclusion of the item at point in gptel sync (model :enabled).

Toggles the item's enabled flag and, when push→gptel is ON, applies the
updated set to gptel immediately. The new state is persisted via autosave."
  (interactive)
  (when-let* ((item (context-navigator-view--at-item)))
    (let* ((key (context-navigator-model-item-key item))
           (name (or (context-navigator-item-name item) key)))
      ;; In MG mode, disable per-item toggling entirely (requirements)
      (let* ((st   (ignore-errors (context-navigator--state-get)))
             (root (and st (ignore-errors (context-navigator-state-last-project-root st))))
             (ps   (and root (ignore-errors (context-navigator-persist-state-load root))))
             (mg   (and (listp ps) (plist-member ps :multi) (plist-get ps :multi))))
        (when mg
          (context-navigator-ui-info :mg-toggle-locked)
          (when (fboundp 'context-navigator-view--render-if-visible)
            (context-navigator-view--render-if-visible))
          (cl-return-from context-navigator-view-toggle-enabled)))
      ;; Stick to current item and scroll to avoid cursor jumps on soft re-render
      (setq-local context-navigator-view--sticky-item-key key)
      (let ((w (selected-window)))
        (when (window-live-p w)
          (setq-local context-navigator-view--sticky-window-start (window-start w))))
      ;; Push snapshot for Undo/Redo (global) before changing the model
      (when (fboundp 'context-navigator-snapshot-push)
        (ignore-errors (context-navigator-snapshot-push)))
      ;; Toggle model
      (ignore-errors (context-navigator-toggle-item key))
      ;; Predictive snapshot for lamps (always): reflect desired state immediately (enabled items).
      (let* ((st   (ignore-errors (context-navigator--state-get)))
             (items (and st (context-navigator-state-items st)))
             (enabled (and (listp items)
                           (cl-remove-if-not #'context-navigator-item-enabled items)))
             (keys (mapcar #'context-navigator-model-item-key enabled)))
        (with-current-buffer (get-buffer-create context-navigator-view--buffer-name)
          (setq-local context-navigator-view--gptel-keys keys)
          (setq-local context-navigator-view--gptel-keys-hash (sxhash-equal keys))
          ;; Сразу сбросить кэши рендера/хедерлайна, чтобы перерисовка прошла мгновенно
          (ignore-errors (context-navigator-view--invalidate-render-caches t)))
        (ignore-errors
          (context-navigator-debug :debug :ui
                                   "toggle:t predictive keys -> %d" (length keys))))
      ;; Apply to gptel immediately when auto-push is ON (logic unchanged)
      (when (and (boundp 'context-navigator--push-to-gptel)
                 context-navigator--push-to-gptel)
        (let* ((st   (ignore-errors (context-navigator--state-get)))
               (items (and st (context-navigator-state-items st)))
               (root (and st (ignore-errors (context-navigator-state-last-project-root st))))
               (slug (and st (ignore-errors (context-navigator-state-current-group-slug st))))
               (ps   (and root (ignore-errors (context-navigator-persist-state-load root))))
               (mg   (and (listp ps) (plist-member ps :multi) (plist-get ps :multi)))
               (sel  (ignore-errors (context-navigator--selected-group-slugs-for-root root))))
          (if (and mg (listp sel) (> (length sel) 0))
              ;; MG: если текущая группа входит в выбор — пушим АГРЕГАТ; иначе — не трогаем gptel.
              (when (and (stringp slug) (member slug sel))
                (ignore-errors (context-navigator-apply-groups-now root sel)))
            ;; Обычный режим: применяем текущую группу.
            (let ((res (ignore-errors (context-navigator-gptel-apply (or items '())))))
              (ignore-errors
                (context-navigator-debug :debug :ui "toggle:t apply -> %S" res)))))
        ;; Report new state briefly (use the same KEY/NAME from this scope)
        (let* ((st2 (ignore-errors (context-navigator--state-get)))
               (idx (and st2 (context-navigator-state-index st2)))
               (it2 (and idx (gethash key idx)))
               (en (and it2 (context-navigator-item-enabled it2))))
          (context-navigator-ui-info (if en :item-enabled :item-disabled) name))
        ;; Minimal UI refresh without forcing cache invalidation (prevents flicker).
        ;; Also render immediately so the indicator lamp updates right away.
        (if (fboundp 'context-navigator-view--schedule-render-soft)
            (context-navigator-view--schedule-render-soft)
          (context-navigator-view--schedule-render))
        (when (fboundp 'context-navigator-view--render-if-visible)
          (context-navigator-view--render-if-visible))
        ;; Optional advance to next item (disabled by default to avoid jumps)
        (when (and (boundp 'context-navigator-view-toggle-advance-next)
                   context-navigator-view-toggle-advance-next
                   (fboundp 'context-navigator-view-next-item))
          (ignore-errors (context-navigator-view-next-item)))))))

;;;###autoload
(defun context-navigator-view-toggle-gptel ()
  "Toggle gptel membership for the item at point (no mass apply)."
  (interactive)
  (when-let* ((item (context-navigator-view--at-item)))
    (let* ((res (ignore-errors (context-navigator-gptel-toggle-one item)))
           (key (context-navigator-model-item-key item)))
      ;; Force next render not to short-circuit
      (let ((buf (get-buffer context-navigator-view--buffer-name)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq-local context-navigator-render--last-hash nil))))
      ;; Refresh indicators; :gptel-change will also trigger a refresh
      (context-navigator-view--schedule-render)
      (context-navigator-view--render-if-visible)
      (when (fboundp 'context-navigator-view-next-item)
        (ignore-errors (context-navigator-view-next-item)))
      (pcase res
        (:added   (context-navigator-ui-info :gptel-added-one (or (context-navigator-item-name item) key)))
        (:removed (context-navigator-ui-info :gptel-removed-one (or (context-navigator-item-name item) key)))
        (_        (context-navigator-ui-info :gptel-no-change (or (context-navigator-item-name item) key)))))))

;;;###autoload
(defun context-navigator-view-delete-from-model ()
  "Delete the item at point from the model permanently and apply to gptel."
  (interactive)
  (when-let* ((item (context-navigator-view--at-item)))
    ;; Push snapshot for Undo/Redo prior to deletion
    (when (fboundp 'context-navigator-snapshot-push)
      (ignore-errors (context-navigator-snapshot-push)))
    (let* ((key (context-navigator-model-item-key item))
           (st (ignore-errors (context-navigator-remove-item-by-key key)))
           (items (and (context-navigator-state-p st) (context-navigator-state-items st))))
      (when (and items (boundp 'context-navigator--push-to-gptel) context-navigator--push-to-gptel)
        (ignore-errors (context-navigator-gptel-apply items)))
      (context-navigator-ui-info :deleted-from-model (or (context-navigator-item-name item) key)))))

;;;###autoload
(defun context-navigator-view-open-all-buffers ()
  "Open all file/buffer/selection items from current model in background."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: open-all-buffers (event=%S)" last-input-event))
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st)))
         (count 0))
    (dolist (it (or items '()))
      (pcase (context-navigator-item-type it)
        ('buffer
         (let ((buf (context-navigator-item-buffer it))
               (p (context-navigator-item-path it)))
           (when (and (stringp p) (file-exists-p p)
                      (not (and buf (buffer-live-p buf)))
                      (not (get-file-buffer p)))
             (ignore-errors (find-file-noselect p))
             (setq count (1+ count)))))
        ('selection
         (let ((p (context-navigator-item-path it)))
           (when (and (stringp p) (file-exists-p p)
                      (not (get-file-buffer p)))
             (ignore-errors (find-file-noselect p))
             (setq count (1+ count)))))
        ('file
         (let ((p (context-navigator-item-path it)))
           (when (and (stringp p) (file-exists-p p)
                      (not (get-file-buffer p)))
             (ignore-errors (find-file-noselect p))
             (setq count (1+ count)))))
        (_ nil)))
    (context-navigator-view--schedule-render)
    (context-navigator-ui-info :opened-context-buffers-bg count)))

;;;###autoload
(defun context-navigator-view-close-all-buffers ()
  "Close all live buffers that belong to items in the current model."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: close-all-buffers (event=%S)" last-input-event))
  (let* ((bufs (ignore-errors (context-navigator-view-counters-collect-closable)))
         (count 0))
    (dolist (b bufs)
      (when (buffer-live-p b)
        (ignore-errors (kill-buffer b))
        (setq count (1+ count))))
    (context-navigator-view--schedule-render)
    (context-navigator-ui-info :closed-context-buffers count)))

;;;###autoload
(defun context-navigator-view-clear-group ()
  "Clear current group's items and redraw immediately."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: clear-group (event=%S)" last-input-event))
  ;; Push snapshot for Undo/Redo before clearing the group
  (when (fboundp 'context-navigator-snapshot-push)
    (ignore-errors (context-navigator-snapshot-push)))
  (ignore-errors (context-navigator-context-clear-current-group))
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (context-navigator-view--invalidate-render-caches))))
  (context-navigator-view--render-if-visible))

;;;###autoload
(defun context-navigator-view-clear-gptel ()
  "Clear gptel context, disable all items in the model, and redraw immediately."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: clear-gptel (event=%S)" last-input-event))
  ;; Push snapshot for Undo/Redo before clearing gptel (disables items)
  (when (fboundp 'context-navigator-snapshot-push)
    (ignore-errors (context-navigator-snapshot-push)))
  (ignore-errors (context-navigator-clear-gptel-now))
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Force fresh render and reset cached indicators so UI reflects cleared gptel.
        (context-navigator-view--invalidate-render-caches)
        ;; Clear the cached gptel keys snapshot so lamps update immediately.
        (setq-local context-navigator-view--gptel-keys nil)
        (setq-local context-navigator-view--gptel-keys-hash (sxhash-equal '())))))
  (context-navigator-view--render-if-visible))

;;;###autoload
(defun context-navigator-view-enable-all-gptel ()
  "Enable all items in the model and push them to gptel immediately."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: enable-all-gptel (event=%S)" last-input-event))
  ;; Push snapshot for Undo/Redo before enabling all
  (when (fboundp 'context-navigator-snapshot-push)
    (ignore-errors (context-navigator-snapshot-push)))
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st))))
    (when (listp items)
      (let ((all-on
             (mapcar (lambda (it)
                       (context-navigator-item-create
                        :type (context-navigator-item-type it)
                        :name (context-navigator-item-name it)
                        :path (context-navigator-item-path it)
                        :buffer (context-navigator-item-buffer it)
                        :beg (context-navigator-item-beg it)
                        :end (context-navigator-item-end it)
                        :size (context-navigator-item-size it)
                        :enabled t
                        :meta (context-navigator-item-meta it)))
                     items)))
        (ignore-errors (context-navigator-set-items all-on))
        ;; Push now (reset + add), regardless of auto-push flag
        (ignore-errors (context-navigator-push-to-gptel-now)))))
  ;; Refresh cached indicators and UI
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local context-navigator-render--last-hash nil)
        (setq-local context-navigator-view--last-render-key nil))))
  (context-navigator-view--render-if-visible))

;;;###autoload
(defun context-navigator-view-disable-all-gptel ()
  "Disable all items in the current group and clear gptel immediately."
  (interactive)
  (context-navigator-view-clear-gptel))

;;;###autoload
(defun context-navigator-view-toggle-all-gptel ()
  "Disable all in gptel (and items) or enable all and push, depending on current state."
  (interactive)
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st)))
         (all-disabled
          (and (listp items)
               (or (= (length items) 0)
                   (cl-every (lambda (it) (not (context-navigator-item-enabled it))) items)))))
    (if all-disabled
        (context-navigator-view-enable-all-gptel)
      (context-navigator-view-clear-gptel))))

;;;###autoload
(defun context-navigator-view-toggle-push ()
  "Toggle push-to-gptel session flag and refresh header immediately."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: toggle-push (event=%S)" last-input-event))
  (ignore-errors (context-navigator-toggle-push-to-gptel))
  ;; Immediately refresh cached keys from gptel so indicators reflect actual presence
  (let* ((lst (ignore-errors (context-navigator-gptel-pull)))
         (keys (and (listp lst)
                    (mapcar #'context-navigator-model-item-key lst)))
         (h (sxhash-equal keys)))
    (with-current-buffer (get-buffer-create context-navigator-view--buffer-name)
      (setq-local context-navigator-view--gptel-keys keys)
      (setq-local context-navigator-view--gptel-keys-hash h)))
  ;; Force immediate redraw for visible sidebar
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (context-navigator-view--invalidate-render-caches))))
  (context-navigator-view--render-if-visible))

;;;###autoload
(defun context-navigator-view-toggle-auto-project ()
  "Toggle auto-project-switch session flag and refresh header immediately."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: toggle-auto (event=%S)" last-input-event))
  (ignore-errors (context-navigator-toggle-auto-project-switch))
  ;; Force immediate redraw for visible sidebar
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (context-navigator-view--invalidate-render-caches))))
  (context-navigator-view--render-if-visible))

;;;###autoload
(defun context-navigator-view-push-now ()
  "Manually push current items to gptel (reset + add) and redraw immediately.

In Multi-group mode with a non-empty selection, push the aggregated enabled items
from all selected groups; otherwise push current group's items."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: push-now (event=%S)" last-input-event))
  (let* ((st   (ignore-errors (context-navigator--state-get)))
         (root (and st (ignore-errors (context-navigator-state-last-project-root st))))
         (ps   (and root (ignore-errors (context-navigator-persist-state-load root))))
         (mg   (and (listp ps) (plist-member ps :multi) (plist-get ps :multi)))
         (sel  (ignore-errors (context-navigator--selected-group-slugs-for-root root))))
    (if (and mg (listp sel) (> (length sel) 0))
        (ignore-errors (context-navigator-apply-groups-now root sel))
      (ignore-errors (context-navigator-push-to-gptel-now))))
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (context-navigator-view--invalidate-render-caches))))
  (context-navigator-view--render-if-visible))

;;;###autoload
(defun context-navigator-view-razor-run ()
  "Run Occam filter against the most recent org-mode buffer."
  (interactive)
  (let* ((org-buf
          (cl-find-if (lambda (b)
                        (with-current-buffer b
                          (derived-mode-p 'org-mode)))
                      (buffer-list))))
    (if (and org-buf (fboundp 'context-navigator-razor-run))
        (with-current-buffer org-buf
          (call-interactively 'context-navigator-razor-run))
      (context-navigator-ui-error :razor-only-org-mode))))

(provide 'context-navigator-view-actions)
;;; context-navigator-view-actions.el ends here
