;;; context-navigator-view-dispatch.el --- Activation/dispatch commands for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Extracted activation and dispatch commands from context-navigator-view.el.
;; This module centralizes:
;; - context-navigator-view-activate
;; - refresh/delete dispatchers
;; - navigation up to groups and groups CRUD helpers
;;
;; The main view should require this file. We keep only declarations for
;; helpers from other modules to avoid load cycles.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-i18n)
(require 'context-navigator-ui)

;; Declarations from the main view and related modules
(declare-function context-navigator-view-toggle-collapse "context-navigator-view" ())
(declare-function context-navigator-view-stats-toggle "context-navigator-view" ())
(declare-function context-navigator-view--open-group-at-point "context-navigator-view" ())
(declare-function context-navigator-view--render-if-visible "context-navigator-view" ())
(declare-function context-navigator-view--schedule-render "context-navigator-view" ())
(declare-function context-navigator-view-refresh "context-navigator-view-events" ())
(declare-function context-navigator-view--save-items-cursor-state "context-navigator-view-events" ())

;; Actions (moved to view-actions)
(declare-function context-navigator-view-push-now "context-navigator-view-actions" ())
(declare-function context-navigator-view-open-all-buffers "context-navigator-view-actions" ())
(declare-function context-navigator-view-close-all-buffers "context-navigator-view-actions" ())
(declare-function context-navigator-view-clear-group "context-navigator-view-actions" ())
(declare-function context-navigator-view-clear-gptel "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-all-gptel "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-push "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-auto-project "context-navigator-view-actions" ())
(declare-function context-navigator-view-visit "context-navigator-view-actions" ())

;; Groups API
(declare-function context-navigator-groups-open "context-navigator-groups" ())
(declare-function context-navigator-group-create "context-navigator-groups" (&optional display-name))
(declare-function context-navigator-group-rename "context-navigator-groups" (&optional old-slug new-display))
(declare-function context-navigator-group-delete "context-navigator-groups" (&optional slug))
(declare-function context-navigator-group-duplicate "context-navigator-groups" (&optional src-slug new-display))
(declare-function context-navigator-group-edit-description "context-navigator-groups" (&optional slug new-desc))

;; Minor helpers
(declare-function context-navigator-debug "context-navigator-log" (&rest args))

;; Core helpers for multi-group apply
(declare-function context-navigator-apply-groups-now "context-navigator-core" (root slugs))
(declare-function context-navigator-collect-enabled-items-for-groups-async "context-navigator-core" (root slugs callback))
(declare-function context-navigator-collect-items-for-groups-async "context-navigator-core" (root slugs callback &optional enabled-only))
(declare-function context-navigator--gptel-defer-or-start "context-navigator-core" (items token))
(declare-function context-navigator--force-enable-items "context-navigator-core" (items))

;; Buffer-local mode flag defined by the main view
(defvar-local context-navigator-view--mode 'items)

;;;###autoload
(defun context-navigator-view-activate ()
  "RET action:
- On title line: toggle collapse/expand
- On Stats header line: toggle stats
- On toggle segments in header: toggle push/auto flags
- On footer action segments: invoke the assigned action (push/open buffers/close buffers/clear group/toggle-all-gptel)
- In groups mode: open group at point
- In items mode: \"..\" goes to groups; otherwise visit item."
  (interactive)
  ;; Diagnostics: log where activation happened (helps with RET issues)
  (ignore-errors
    (context-navigator-debug :debug :ui "activate: mode=%s title=%s stats=%s item=%s group=%s up=%s act=%s tgl=%s"
                             context-navigator-view--mode
                             (and (get-text-property (point) 'context-navigator-title) t)
                             (and (get-text-property (point) 'context-navigator-stats-toggle) t)
                             (and (get-text-property (point) 'context-navigator-item) t)
                             (get-text-property (point) 'context-navigator-group-slug)
                             (and (get-text-property (point) 'context-navigator-groups-up) t)
                             (get-text-property (point) 'context-navigator-action)
                             (get-text-property (point) 'context-navigator-toggle)))
  ;; Title line: collapse/expand (render immediately)
  (when (get-text-property (point) 'context-navigator-title)
    (context-navigator-view-toggle-collapse)
    (context-navigator-view--render-if-visible)
    (cl-return-from context-navigator-view-activate))
  ;; Stats header toggle (render immediately)
  (when (get-text-property (point) 'context-navigator-stats-toggle)
    (context-navigator-view-stats-toggle)
    (context-navigator-view--render-if-visible)
    (cl-return-from context-navigator-view-activate))
  (let ((act (get-text-property (point) 'context-navigator-action))
        (tgl (get-text-property (point) 'context-navigator-toggle)))
    (cond
     ;; Footer actions (explicit)
     ((eq act 'push-now) (context-navigator-view-push-now))
     ((eq act 'open-buffers) (context-navigator-view-open-all-buffers))
     ((eq act 'close-buffers) (context-navigator-view-close-all-buffers))
     ((eq act 'clear-group) (context-navigator-view-clear-group))
     ((eq act 'clear-gptel) (context-navigator-view-clear-gptel))
     ((eq act 'toggle-all-gptel) (context-navigator-view-toggle-all-gptel))
     ((eq act 'razor) (when (fboundp 'context-navigator-view-razor-run)
                        (call-interactively 'context-navigator-view-razor-run)))
     ;; Header toggles
     ((eq tgl 'push) (context-navigator-view-toggle-push))
     ((eq tgl 'auto) (context-navigator-view-toggle-auto-project))
     ((eq context-navigator-view--mode 'groups)
      (or (context-navigator-view--open-group-at-point)
          (context-navigator-ui-info :no-group-at-point)))
     (t
      (context-navigator-view-visit)))))

;;;###autoload
(defun context-navigator-view-refresh-dispatch ()
  "g action: refresh items or groups, depending on mode."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (ignore-errors (context-navigator-groups-open))
    (context-navigator-view-refresh)))

;;;###autoload
(defun context-navigator-view-delete-dispatch ()
  "d action: delete item (items mode) or group (groups mode)."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (if-let* ((slug (get-text-property (point) 'context-navigator-group-slug)))
          (progn
            (ignore-errors (context-navigator-group-delete slug))
            (context-navigator-view--schedule-render)
            (context-navigator-view--render-if-visible))
        (context-navigator-ui-info :no-group-at-point))
    (when (fboundp 'context-navigator-view-delete-from-model)
      (call-interactively 'context-navigator-view-delete-from-model))))

;;;###autoload
(defun context-navigator-view-go-up ()
  "Open/focus Groups split below Navigator; sidebar remains in items mode."
  (interactive)
  ;; Save current cursor position for this group (for later restore)
  (ignore-errors (context-navigator-view--save-items-cursor-state))
  ;; Open/focus groups split instead of switching sidebar mode
  (when (fboundp 'context-navigator-groups-split-open)
    (context-navigator-groups-split-open))
  ;; Keep sidebar in items mode; just refresh if visible
  (context-navigator-view--render-if-visible))

;;;###autoload
(defun context-navigator-view-group-create ()
  "Create a new group (groups mode)."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let ((slug (ignore-errors (context-navigator-group-create))))
        ;; After successful creation, switch to items view immediately.
        (when (and slug (stringp slug))
          (setq context-navigator-view--mode 'items)
          (context-navigator-view--schedule-render)))
    (context-navigator-ui-info :press-h-open-groups-first)))

;;;###autoload
(defun context-navigator-view-group-rename ()
  "Rename selected group (groups mode)."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let ((slug (get-text-property (point) 'context-navigator-group-slug)))
        (ignore-errors (context-navigator-group-rename slug))
        (context-navigator-view--schedule-render)
        (context-navigator-view--render-if-visible))
    (context-navigator-ui-info :press-h-open-groups-first)))

;;;###autoload
(defun context-navigator-view-group-duplicate ()
  "Duplicate selected group (groups mode)."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let ((slug (get-text-property (point) 'context-navigator-group-slug)))
        (ignore-errors (context-navigator-group-duplicate slug))
        (context-navigator-view--schedule-render)
        (context-navigator-view--render-if-visible))
    (context-navigator-ui-info :press-h-open-groups-first)))

;;;###autoload
(defun context-navigator-view-group-edit-description ()
  "Edit description for selected group (groups mode) or current group otherwise."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let ((slug (get-text-property (point) 'context-navigator-group-slug)))
        (ignore-errors (context-navigator-group-edit-description slug))
        (context-navigator-view--schedule-render)
        (context-navigator-view--render-if-visible))
    (progn
      (ignore-errors (context-navigator-group-edit-description))
      (context-navigator-view--schedule-render)
      (context-navigator-view--render-if-visible))))

;;;###autoload
(defun context-navigator-view-group-toggle-select ()
  "Toggle selection of the group at point.

Works in both the Navigator groups view and in the Groups split buffer.
Selected groups are stored per project in state.el under :selected.
When push→gptel is ON, auto-apply aggregated selection if under threshold."
  (interactive)
  (let* ((slug (get-text-property (point) 'context-navigator-group-slug)))
    (if (not (and (stringp slug) (not (string-empty-p slug))))
        (context-navigator-ui-info :no-group-at-point)
      (let* ((st   (ignore-errors (context-navigator--state-get)))
             (root (and st (context-navigator-state-last-project-root st)))
             (ps   (or (ignore-errors (context-navigator-persist-state-load root)) '()))
             (mg   (and (plist-member ps :multi) (plist-get ps :multi))))
        ;; MG OFF → no-op (не меняем selection)
        (when (not mg)
          (context-navigator-ui-info :toggle-multi-group)
          (cl-return-from context-navigator-view-group-toggle-select))
        ;; MG ON → меняем selection в pstate и применяем побочные эффекты
        (let* ((pstate (if (plist-member ps :version) (copy-sequence ps)
                         (plist-put (copy-sequence ps) :version 1)))
               (sel0 (and (plist-member pstate :selected) (plist-get pstate :selected)))
               (sel  (if (listp sel0) sel0 '()))
               (sel1 (if (member slug sel)
                         (cl-remove slug sel :test #'equal)
                       (append sel (list slug)))))
          (setq pstate (plist-put (copy-sequence pstate) :selected sel1))
          (ignore-errors (context-navigator-persist-state-save root pstate))
          ;; Оповестить слушателей (Stats и т.п.)
          (ignore-errors (context-navigator-events-publish :group-selection-changed root sel1))
          ;; Автопуш агрегата при включенном push→gptel
          (when (and (boundp 'context-navigator--push-to-gptel)
                     context-navigator--push-to-gptel
                     (listp sel1) (> (length sel1) 0)
                     (stringp root))
            (context-navigator-collect-items-for-groups-async
             root sel1
             (lambda (items)
               (let* ((n (length (or items '())))
                      (thr (or (and (boundp 'context-navigator-multigroup-autopush-threshold)
                                    context-navigator-multigroup-autopush-threshold)
                               100)))
                 (cond
                  ((= n 0)
                   (ignore-errors (context-navigator-gptel-clear-all-now)))
                  ((> n thr)
                   (setq context-navigator--push-to-gptel nil)
                   (context-navigator-ui-info :push-state (context-navigator-i18n :off))
                   (context-navigator-debug :info :core "auto-push disabled (selection size=%s > %s)" n thr))
                  (t
                   (ignore-errors (context-navigator-gptel-clear-all-now))
                   (let* ((st (ignore-errors (context-navigator--state-get)))
                          (token (and st (context-navigator-state-load-token st)))
                          (forced (context-navigator--force-enable-items items)))
                     (ignore-errors (context-navigator--gptel-defer-or-start forced token)))))))))
          ;; Сохранить фокус на переключённой группе в сплите (если он открыт)
          (when (boundp 'context-navigator-groups-split--buffer)
            (let ((gb context-navigator-groups-split--buffer))
              (when (buffer-live-p gb)
                (with-current-buffer gb
                  (setq-local context-navigator-groups-split--focus-once slug)))))
          ;; Перерисуем список групп вместо ручной подмены строки (чтобы обновилась лампочка)
          (setq-local context-navigator-render--last-hash nil)
          (setq-local context-navigator-view--last-render-key nil)
          (context-navigator-view--schedule-render))))))
;;;###autoload
(defun context-navigator-view-toggle-multi-group ()
  "Toggle per-project multi-group mode (:multi in state.el)."
  (interactive)
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (cur-slug (and st (ignore-errors (context-navigator-state-current-group-slug st))))
         (pstate (or (ignore-errors (context-navigator-persist-state-load root)) '()))
         (pstate (if (plist-member pstate :version) (copy-sequence pstate)
                   (plist-put (copy-sequence pstate) :version 1)))
         (cur (and (plist-member pstate :multi) (plist-get pstate :multi)))
         (new (not cur)))
    (setq pstate (plist-put (copy-sequence pstate) :multi new))
    ;; If turning MG ON and selection is empty → add current group by default
    (when new
      (let* ((sel0 (and (plist-member pstate :selected) (plist-get pstate :selected)))
             (sel  (if (listp sel0) sel0 '())))
        (when (and (stringp cur-slug) (not (string-empty-p cur-slug)) (not (member cur-slug sel)))
          (setq sel (append sel (list cur-slug))))
        (setq pstate (plist-put (copy-sequence pstate) :selected sel))))
    (ignore-errors (context-navigator-persist-state-save root pstate))
    ;; Notify listeners (Stats etc.)
    (ignore-errors (context-navigator-events-publish :group-selection-changed root
                                                     (and (plist-member pstate :selected)
                                                          (plist-get pstate :selected))))
    ;; If turning MG ON, ensure the Groups split is open for selection UI.
    (when new
      (when (fboundp 'context-navigator-groups-split-open)
        (ignore-errors (context-navigator-groups-split-open))))
    ;; If push→gptel is ON — apply immediately per new mode, without touching model enabled flags
    (when (and (boundp 'context-navigator--push-to-gptel)
               context-navigator--push-to-gptel)
      (if new
          (let* ((sel (and (plist-member pstate :selected)
                           (plist-get pstate :selected))))
            (if (and (listp sel) (> (length sel) 0))
                (ignore-errors (context-navigator-apply-groups-now root sel))
              ;; MG ON but no selection — only clear gptel without disabling items
              (ignore-errors (context-navigator-gptel-clear-all-now))))
        ;; MG OFF → push current group's enabled items
        (ignore-errors (context-navigator-push-to-gptel-now))))
    (context-navigator-view--schedule-render)))

;;;###autoload
(defun context-navigator-view-push-now-dispatch ()
  "Push now: in groups mode push aggregated selected groups; otherwise delegate."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let* ((st (ignore-errors (context-navigator--state-get)))
             (root (and st (context-navigator-state-last-project-root st)))
             (pstate (or (ignore-errors (context-navigator-persist-state-load root)) '()))
             (sel (and (plist-member pstate :selected) (plist-get pstate :selected))))
        (if (or (null (listp sel)) (= (length sel) 0))
            (context-navigator-ui-info :no-group-selected)
          (ignore-errors (context-navigator-apply-groups-now root sel))))
    ;; Items mode: keep original behavior
    (when (fboundp 'context-navigator-view-push-now)
      (call-interactively 'context-navigator-view-push-now))))

(provide 'context-navigator-view-dispatch)
;;; context-navigator-view-dispatch.el ends here
