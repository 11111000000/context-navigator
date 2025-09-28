;;; context-navigator-view.el --- Sidebar UI (side window) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Lightweight, event-driven sidebar:
;; - Opens in a left side window with configurable width
;; - Subscribes to :model-refreshed and :context-load-(start|done)
;; - Renders via context-navigator-render, optional icons
;; - Minimal keymap: RET to visit, d delete, g refresh, q quit
;;
;; Functional by design: no state mutation outside buffer-local vars for UI.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'context-navigator-events)
(require 'context-navigator-render)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-icons)
(require 'context-navigator-persist)
(require 'context-navigator-i18n)
(require 'context-navigator-log)
(require 'context-navigator-modeline)
(require 'context-navigator-headerline)
(require 'context-navigator-stats)
(require 'context-navigator-view-controls)
;; Controls/items/groups are split into dedicated modules to reduce coupling.
;; Require controls (toolbar) and the items/groups renderers so the view can
;; call their implementations directly without leaving thin wrappers behind.
(require 'context-navigator-view-items)
(require 'context-navigator-view-groups)

;; Split helpers (indicators / counters / spinner) — thin wrappers live in
;; separate modules to reduce coupling and make incremental extraction easier.
;; Implementations currently delegate to existing functions in context-navigator-view.el
;; and therefore are safe to require here.
(require 'context-navigator-view-indicators)
(require 'context-navigator-view-counters)
(require 'context-navigator-view-spinner)
(require 'context-navigator-view-windows)

(defcustom context-navigator-auto-open-groups-on-error t
  "When non-nil, automatically switch the sidebar to the groups list if a group fails to load."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-highlight-active-group t
  "When non-nil, highlight the active group in the groups list."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-controls-style 'icons
  "Style for sidebar controls (toggles and footer buttons):
- auto  : prefer compact icon-like labels when possible
- icons : force compact icon-like labels
- text  : verbose text labels"
  :type '(choice (const auto) (const icons) (const text))
  :group 'context-navigator)



(defcustom context-navigator-gptel-indicator-poll-interval 0
  "Polling interval (seconds) to refresh gptel indicators while the sidebar is visible.

Set to 0 or nil to disable polling (event-based refresh still works)."
  :type 'number :group 'context-navigator)

;; Forward declarations to avoid load cycle; core provides these.
(declare-function context-navigator--state-get "context-navigator-core")
(declare-function context-navigator-state-last-project-root "context-navigator-core" (state))
(declare-function context-navigator-state-loading-p "context-navigator-core" (state))
(declare-function context-navigator-state-items "context-navigator-core" (state))
(declare-function context-navigator-state-index "context-navigator-core" (state))
(declare-function context-navigator-state-current-group-slug "context-navigator-core" (state))
(declare-function context-navigator-state-p "context-navigator-core" (state))
(declare-function context-navigator-state-generation "context-navigator-core" (state))
(declare-function context-navigator-toggle-item "context-navigator-core" (key &optional enabled))
(declare-function context-navigator-remove-item-by-key "context-navigator-core" (key))
(declare-function context-navigator-context-clear-current-group "context-navigator-core" ())
(declare-function context-navigator-context-unload "context-navigator-core" ())
;; group commands (from groups module)
(declare-function context-navigator-groups-open "context-navigator-groups" ())
(declare-function context-navigator-group-switch "context-navigator-groups" (&optional slug))
(declare-function context-navigator-group-create "context-navigator-groups" (&optional display-name))
(declare-function context-navigator-group-rename "context-navigator-groups" (&optional old-slug new-display))
(declare-function context-navigator-group-delete "context-navigator-groups" (&optional slug))
(declare-function context-navigator-group-duplicate "context-navigator-groups" (&optional src-slug new-display))
(declare-function context-navigator-group-edit-description "context-navigator-groups" (&optional slug new-desc))

(defconst context-navigator-view--buffer-name "*context-navigator*")



(defvar-local context-navigator-view--subs nil)
(defvar-local context-navigator-view--header "Context")
(defvar-local context-navigator-view--mode 'items) ;; 'items | 'groups
(defvar-local context-navigator-view--groups nil)  ;; cached groups plists
(defvar-local context-navigator-view--last-lines nil)
(defvar context-navigator-view--group-line-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m [mouse-1] #'context-navigator-view-mouse-open-group)
    ;; Explicit keyboard bindings on group lines for reliability
    (define-key m (kbd "RET")       #'context-navigator-view-activate)
    (define-key m (kbd "C-m")       #'context-navigator-view-activate)
    (define-key m [return]          #'context-navigator-view-activate)
    (define-key m (kbd "<return>")  #'context-navigator-view-activate)
    (define-key m [kp-enter]        #'context-navigator-view-activate)
    (define-key m (kbd "l")         #'context-navigator-view-activate)
    m)
  "Keymap attached to group lines to support mouse and keyboard activation.")
(defvar-local context-navigator-view--load-progress nil) ;; cons (POS . TOTAL) | nil)
(defvar-local context-navigator-view--winselect-fn nil)  ;; function added to window-selection-change-functions
(defvar-local context-navigator-view--gptel-keys nil)    ;; cached stable keys from gptel (for indicators)
(defvar-local context-navigator-view--gptel-keys-hash nil) ;; reserved for future use
(defvar-local context-navigator-view--sorted-items nil)  ;; cached sorted items (list) for current generation
(defvar-local context-navigator-view--sorted-gen nil)    ;; generation number of the cached sorted items
(defvar-local context-navigator-view--openable-count nil)          ;; cached count (int) or nil
(defvar-local context-navigator-view--openable-plus nil)           ;; non-nil when soft-cap reached
(defvar-local context-navigator-view--openable-stamp 0.0)          ;; float-time of last compute
(defvar-local context-navigator-view--openable-timer nil)          ;; pending timer for recompute
(defvar-local context-navigator-view--buflist-fn nil)              ;; function added to buffer-list-update-hook
(defvar-local context-navigator-view--gptel-poll-timer nil)        ;; polling timer for gptel indicators (or nil)
(defvar-local context-navigator-view--status-post-cmd-fn nil)      ;; post-command hook to update inline status line
(defvar-local context-navigator-view--last-render-key nil)        ;; cached render key to skip redundant renders
(defvar-local context-navigator-view--last-active-group nil)      ;; last active group cached to avoid jumping cursor in groups view
(defvar-local context-navigator-view--last-mode nil)              ;; last rendered mode: 'items or 'groups
(defvar-local context-navigator-view--sorted-root nil)            ;; root used for cached items sort
(defvar-local context-navigator-view--relpaths-hash nil)          ;; cache: item-key -> relpath for current generation/root
(defvar-local context-navigator-view--collapsed-p nil)            ;; when non-nil, hide everything below the title (TAB toggles)

(defvar context-navigator-view--title-line-keymap
  (let ((m (make-sparse-keymap)))
    ;; Mouse click toggles collapse/expand
    (define-key m [mouse-1] #'context-navigator-view-toggle-collapse-immediate)
    ;; TAB on title behaves like in Magit: toggle collapse
    (define-key m (kbd "TAB")       #'context-navigator-view-toggle-collapse-immediate)
    (define-key m (kbd "<tab>")     #'context-navigator-view-toggle-collapse-immediate)
    (define-key m [tab]             #'context-navigator-view-toggle-collapse-immediate)
    (define-key m (kbd "C-i")       #'context-navigator-view-toggle-collapse-immediate)
    ;; RET on title also toggles collapse/expand
    (define-key m (kbd "RET")       #'context-navigator-view-toggle-collapse-immediate)
    (define-key m (kbd "C-m")       #'context-navigator-view-toggle-collapse-immediate)
    (define-key m [return]          #'context-navigator-view-toggle-collapse-immediate)
    (define-key m (kbd "<return>")  #'context-navigator-view-toggle-collapse-immediate)
    m)
  "Keymap attached to the title line to support mouse/TAB/RET collapse/expand.")

(defun context-navigator-view-toggle-collapse-immediate ()
  "Toggle collapse and render immediately."
  (interactive)
  (context-navigator-view-toggle-collapse)
  (context-navigator-view--render-if-visible))



(defvar-local context-navigator-view--spinner-timer nil)          ;; loading spinner timer
(defvar-local context-navigator-view--spinner-index 0)
(defvar-local context-navigator-view--spinner-last-time 0.0)      ;; last tick timestamp (float-time)
(defvar-local context-navigator-view--spinner-degraded nil)       ;; when non-nil, render static indicator

(defvar context-navigator-view-window-params
  '((side . left) (slot . -1))
  "Default parameters for the sidebar window.")



(defcustom context-navigator-view-header-props
  '(context-navigator-header)
  "List of text-properties that mark section headers in the sidebar."
  :type '(repeat symbol) :group 'context-navigator)

(defcustom context-navigator-view-element-props
  '(context-navigator-interactive)
  "List of text-properties that are considered section elements for j/n/k/p."
  :type '(repeat symbol) :group 'context-navigator)

(defun context-navigator-view--header (state)
  "Compute compact header title from STATE.

Rules:
- Items mode: [<project>: <group>] when group is active, otherwise [<project>]
- Groups mode: [<project>] only
- Global (no project): use ~ as project name → items: [~: <group>] / groups: [~]

Note: status toggles [→gptel:on/off] [auto-proj:on/off] are rendered in the header-line."
  (let* ((root (context-navigator-state-last-project-root state))
         (group (context-navigator-state-current-group-slug state))
         (proj-name (if root
                        (file-name-nondirectory (directory-file-name root))
                      "~")))
    (cond
     ((eq context-navigator-view--mode 'groups)
      (format "[%s]" proj-name))
     (t
      (if group
          (format "[%s: %s]" proj-name group)
        (format "[%s]" proj-name))))))

(defun context-navigator-view--state-items ()
  "Get items from core state."
  (let* ((st (ignore-errors (context-navigator--state-get))))
    (and st (context-navigator-state-items st))))

;; Helpers



;; Lightweight buffer/window hook installers used by the view buffer.
;; These are minimal, safe implementations that avoid void-function errors
;; during incremental extraction. They intentionally do small, local work:
;; install buffer-list/window-selection hooks that trigger counters/refresh.
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
                  (ignore-errors (context-navigator-view--invalidate-openable)))))))
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





;; Moved to context-navigator-view-controls.el (provides `context-navigator-view-controls-lines').
;; This placeholder avoids a duplicate definition in view.el.

;; Controls moved to context-navigator-view-controls.el
;; Keep compiler happy with declarations; actual definitions provided by the module.

(declare-function context-navigator-view-controls-segments "context-navigator-view-controls" ())
(declare-function context-navigator-view-controls-lines "context-navigator-view-controls" (total-width))

;; Naming cleanup wrappers (non-breaking).
;; Wrappers removed: implementations live in dedicated modules (view-items, view-groups).
;; The view.el declares these symbols via `declare-function' above so the byte-compiler is satisfied.

;; Compatibility: provide legacy aliases for old API names and mark them obsolete.
;; We alias old symbols to the new canonical implementations (old -> new),
;; then call `make-obsolete' so callers see a clear warning but code continues to work.



;; Items / Groups footer -> canonical names (items already provides alias)


;; Note: context-navigator-view--items-footer-lines is already aliased inside
;; context-navigator-view-items.el to context-navigator-view--items-extra-lines
;; and is marked obsolete there.

;; Openable/Closable counters moved to context-navigator-view-counters.el
;; Implementations live in that module; the view calls thin wrappers so the API
;; remains stable while the implementation is extracted.

(declare-function context-navigator-view-counters-get-openable "context-navigator-view-counters" ())
(declare-function context-navigator-view-counters-refresh-openable "context-navigator-view-counters" ())
(declare-function context-navigator-view-counters-collect-closable "context-navigator-view-counters" ())
(declare-function context-navigator-view-counters-invalidate "context-navigator-view-counters" ())

(defun context-navigator-view--invalidate-openable ()
  "Invalidate cached openable counters (delegates to counters module)."
  (ignore-errors (context-navigator-view-counters-invalidate)))

(defun context-navigator-view--openable-count-refresh ()
  "Delegate refresh to counters module."
  (ignore-errors (context-navigator-view-counters-refresh-openable)))

(defun context-navigator-view--openable-count-get ()
  "Return cached openable count from counters module (COUNT . PLUS)."
  (ignore-errors (context-navigator-view-counters-get-openable)))

(defun context-navigator-view--collect-closable-buffers ()
  "Delegate closable-buffers collection to counters module."
  (ignore-errors (context-navigator-view-counters-collect-closable)))

;; Loading spinner helpers ----------------------------------------------------

(defun context-navigator-view--spinner-start ()
  "Start or restart the lightweight loading spinner timer.
Degrades to a static indicator when timer slippage exceeds threshold."
  (ignore-errors (context-navigator-view-spinner-start)))

(defun context-navigator-view--spinner-stop ()
  "Stop the loading spinner timer and reset index."
  (ignore-errors (context-navigator-view-spinner-stop)))


;; Controls API declarations are defined earlier to avoid duplication.



;; Groups rendering helpers moved to context-navigator-view-groups.el
;; Keep declarations so byte-compiler & callers in other modules are happy.
(declare-function context-navigator-view--groups-header-lines "context-navigator-view-groups" (header total-width))
(declare-function context-navigator-view--groups-body-lines "context-navigator-view-groups" (state))
(declare-function context-navigator-view--groups-help-lines "context-navigator-view-groups" (total-width))
(declare-function context-navigator-view--render-groups "context-navigator-view-groups" (state header total-width))

;; Items rendering & status helpers moved to context-navigator-view-items.el
;; Keep declarations so byte-compiler & callers in other modules are happy.
(declare-function context-navigator-view--items-header-toggle-lines "context-navigator-view-items" (total-width))
(declare-function context-navigator-view--items-base-lines "context-navigator-view-items" (state header total-width))
(declare-function context-navigator-view--status-text-at-point "context-navigator-view-items" ())
(declare-function context-navigator-view--items-extra-lines "context-navigator-view-items" (total-width))
(declare-function context-navigator-view--render-items "context-navigator-view-items" (state header total-width))


;; Entry point

(defun context-navigator-view--render-loading (state header total-width)
  "Render a lightweight loading/preloader view into the sidebar buffer.

Header is displayed in the header-line now; do not render a long title or
separator at the top of the buffer. Keep a small centered spinner/loading line."
  (let* ((hl "") ;; no header in buffer
         (sep "") ;; no separator
         (pct (when (and context-navigator-view--load-progress
                         (numberp (car context-navigator-view--load-progress))
                         (numberp (cdr context-navigator-view--load-progress))
                         (> (cdr context-navigator-view--load-progress) 0))
                (floor (* 100.0 (/ (float (car context-navigator-view--load-progress))
                                   (max 1 (cdr context-navigator-view--load-progress)))))))
         (use-spinner (and (not context-navigator-view--spinner-degraded)
                           (timerp context-navigator-view--spinner-timer)
                           pct))
         (frames (or context-navigator-view-spinner-frames
                     '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")))
         (len (length frames))
         (idx (or context-navigator-view--spinner-index 0))
         (ch (if (and use-spinner (> len 0))
                 (nth (mod idx (max 1 len)) frames)
               ""))
         (label (if pct
                    (format "%s%d%%" (if use-spinner ch "") pct)
                  (context-navigator-i18n :loading)))
         (spin-w (max 0 (string-width label)))
         (left-pad (max 0 (floor (/ (max 0 (- total-width spin-w)) 2))))
         (spin-line (concat (make-string left-pad ? ) label))
         (sline (propertize (concat " " spin-line) 'face 'shadow))
         (lines (list "" "" "" sline "")))
    (setq context-navigator-view--last-lines lines
          context-navigator-view--header header)
    (context-navigator-render-apply-to-buffer (current-buffer) lines)
    lines))

(defun context-navigator-view--render ()
  "Render current view (items or groups) into the sidebar buffer.

Uses a composite render key to skip full rendering when nothing relevant changed.
Key components:
 - model generation
 - current view mode (items/groups)
 - sidebar width (total)
 - gptel keys hash
 - cached openable count / soft-plus marker
 - header string (display)

When the key equals `context-navigator-view--last-render-key' the function
returns without rebuilding buffer contents.

Optimization: if the core state indicates loading in progress we render a
very small, cheap preloader view immediately (no icons, no sorting, no file
checks) so project switching feels responsive while the data loads in the
background."
  (catch 'context-navigator-view--render
    (let* ((state (context-navigator--state-get))
           (header (context-navigator-view--header state))
           (win (get-buffer-window (current-buffer) 'visible))
           (total (or (and win (window-body-width win))
                      (and (boundp 'context-navigator-view-width)
                           (symbol-value 'context-navigator-view-width))
                      33))
           ;; Components for early-exit render key
           (gen (or (and (context-navigator-state-p state)
                         (context-navigator-state-generation state))
                    0))
           (mode context-navigator-view--mode)
           ;; Use sxhash-equal to produce a stable-ish fingerprint of gptel keys list
           (gptel-hash (sxhash-equal context-navigator-view--gptel-keys))
           ;; Use cached openable count (may be nil) — normalize to integer and plus marker.
           (openable (or context-navigator-view--openable-count 0))
           (plus (and context-navigator-view--openable-plus t))
           (push-on (and (boundp 'context-navigator--push-to-gptel)
                         context-navigator--push-to-gptel))
           (auto-on (and (boundp 'context-navigator--auto-project-switch)
                         context-navigator--auto-project-switch))
           ;; Compose key (include session flags so toggles force a refresh)
           (key (list gen mode total gptel-hash openable plus header push-on auto-on context-navigator-view--collapsed-p)))
      (unless (equal key context-navigator-view--last-render-key)
        (setq context-navigator-view--last-render-key key)
        ;; Fast path: show minimal preloader when loading or when progress is reported by events.
        (when (or (and (context-navigator-state-p state)
                       (context-navigator-state-loading-p state))
                  context-navigator-view--load-progress)
          (context-navigator-view--render-loading state header total)
          (throw 'context-navigator-view--render nil))
        (cond
         ((eq context-navigator-view--mode 'groups)
          (context-navigator-view--render-groups state header total))
         (t
          (context-navigator-view--render-items state header total)))))))
(defun context-navigator-view--render-if-visible ()
  "Render sidebar if its buffer is visible."
  (when-let* ((buf (get-buffer context-navigator-view--buffer-name))
              (win (get-buffer-window buf t)))
    (with-selected-window win
      (with-current-buffer buf
        (context-navigator-view--render)))))

(defun context-navigator-view--schedule-render ()
  "Debounced request to render the sidebar if visible. Reset render cache to force update."
  ;; Ensure next render is not short-circuited by the render hash cache.
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local context-navigator-render--last-hash nil)
        (setq-local context-navigator-view--last-render-key nil))))
  (context-navigator-events-debounce
   :sidebar-render 0.12
   #'context-navigator-view--render-if-visible))

(defun context-navigator-view--at-item ()
  "Return item at point (from text properties) or nil."
  (let ((it (get-text-property (point) 'context-navigator-item)))
    it))

(defun context-navigator-view--visit (preview)
  "Open item at point. If PREVIEW non-nil, show in other window.

When the sidebar buffer/window is currently selected, prefer opening targets
in another window (never replace the sidebar buffer). This avoids the
situation where visiting a file replaces the sidebar buffer and makes it
hard to restore the sidebar afterward."
  (when-let* ((item (context-navigator-view--at-item)))
    (let* ((selected-win (selected-window))
           (is-sidebar (and (window-live-p selected-win)
                            (window-parameter selected-win 'context-navigator-view)
                            (eq (window-buffer selected-win) (current-buffer))))
           (prefer-other (or preview is-sidebar)))
      (context-navigator-open-item item prefer-other))))

(defun context-navigator-view-visit ()
  "Visit item at point."
  (interactive)
  (context-navigator-view--visit nil))

(defun context-navigator-view-preview ()
  "Preview item at point in other window."
  (interactive)
  (context-navigator-view--visit t))

(defun context-navigator-view-next-item ()
  "Move to the next interactive element (wrap at end).

Ходит последовательно по всем сегментам с 'context-navigator-interactive,
включая заголовки, \"..\", элементы и т.п."
  (interactive)
  (let* ((here (point))
         (prop 'context-navigator-interactive)
         (cur-end (if (get-text-property here prop)
                      (or (next-single-property-change here prop nil (point-max))
                          (point-max))
                    (1+ here)))
         (pos (context-navigator-view--find-next-interactive-pos cur-end)))
    (unless pos
      (setq pos (context-navigator-view--find-next-interactive-pos (point-min))))
    (when pos (goto-char pos))))

(defun context-navigator-view-previous-item ()
  "Move to the previous interactive element (wrap to last).

Ходит последовательно по всем сегментам с 'context-navigator-interactive,
включая заголовки, \"..\", элементы и т.п."
  (interactive)
  (let* ((here (point))
         (prop 'context-navigator-interactive)
         (cur-beg (if (get-text-property here prop)
                      (or (previous-single-property-change here prop nil (point-min))
                          (point-min))
                    here))
         (pos (context-navigator-view--find-prev-interactive-pos cur-beg)))
    (unless pos
      (setq pos (context-navigator-view--find-prev-interactive-pos (point-max))))
    (when pos (goto-char pos))))

;; TAB navigation helpers ----------------------------------------------------

(defun context-navigator-view--find-next-interactive-pos (&optional start)
  "Return nearest position >= START with 'context-navigator-interactive."
  (let ((start (or start (point))))
    (text-property-not-all start (point-max) 'context-navigator-interactive nil)))

(defun context-navigator-view--find-prev-interactive-pos (&optional start)
  "Return the beginning of the previous 'context-navigator-interactive run before START.
If START is inside a run, return the beginning of the same run; if START is
exactly at a run start, return the beginning of the previous run. Returns nil
when none exists."
  (let* ((prop 'context-navigator-interactive)
         (pos (or start (point))))
    ;; Start strictly before START to search previous runs.
    (setq pos (max (1- pos) (point-min)))
    (cond
     ;; If currently on an interactive char, jump to its run start.
     ((and (> pos (point-min)) (get-text-property pos prop))
      (or (previous-single-property-change pos prop nil (point-min))
          (point-min)))
     (t
      ;; Walk backwards to the previous boundary where property becomes non-nil,
      ;; then return that run's start (the boundary itself).
      (let (chg)
        (while (and (> pos (point-min))
                    (setq chg (previous-single-property-change pos prop nil (point-min)))
                    (not (and (> chg (point-min)) (get-text-property (1- chg) prop))))
          (setq pos chg chg nil))
        (when (and chg (> chg (point-min)) (get-text-property (1- chg) prop))
          chg))))))

(defun context-navigator-view--find-next-itemish-pos (&optional start)
  "Return nearest position >= START with either an item or the \"..\" up marker."
  (let* ((start (or start (point)))
         (p1 (text-property-not-all start (point-max) 'context-navigator-item nil))
         (p2 (text-property-not-all start (point-max) 'context-navigator-groups-up nil)))
    (cond
     ((and p1 p2) (min p1 p2))
     (p1 p1)
     (p2 p2)
     (t nil))))

(defun context-navigator-view--find-prev-itemish-pos (&optional start)
  "Return nearest position < START with either an item or the \"..\" up marker."
  (let* ((start (or start (point)))
         (pos nil)
         (best nil))
    (setq pos (context-navigator-view--find-next-itemish-pos (point-min)))
    (while (and pos (< pos start))
      (setq best pos)
      (setq pos (context-navigator-view--find-next-itemish-pos (1+ pos))))
    best))

(defun context-navigator-view--move-next-interactive ()
  "Move to the next interactive element without toggling title/stats; wraps."
  (interactive)
  (let* ((here (point))
         (props '(context-navigator-title
                  context-navigator-item
                  context-navigator-group-slug
                  context-navigator-action
                  context-navigator-toggle
                  context-navigator-stats-toggle
                  context-navigator-groups-up))
         ;; If inside an interactive segment, skip to its end first.
         (cur-end
          (if (cl-some (lambda (p) (get-text-property here p)) props)
              (cl-reduce #'max
                         (mapcar (lambda (p)
                                   (if (get-text-property here p)
                                       (or (next-single-property-change here p nil (point-max))
                                           (point-max))
                                     (1+ here)))
                                 props)
                         :initial-value (1+ here))
            (1+ here)))
         (pos (context-navigator-view--find-next-interactive-pos cur-end)))
    (unless pos
      ;; wrap to the first interactive element
      (setq pos (context-navigator-view--find-next-interactive-pos (point-min))))
    (if pos
        (goto-char pos)
      (message "%s" (context-navigator-i18n :no-interactive-elements)))))

(defun context-navigator-view-tab-next ()
  "Move point to the next interactive element.

On title line: toggle collapse. On Stats header: toggle stats. Wrap at end."
  (interactive)
  (let* ((here (point)))
    (when (get-text-property here 'context-navigator-title)
      (context-navigator-view-toggle-collapse)
      (cl-return-from context-navigator-view-tab-next))
    (when (get-text-property here 'context-navigator-stats-toggle)
      (context-navigator-view-stats-toggle)
      (cl-return-from context-navigator-view-tab-next))
    (let* ((cur-end (if (get-text-property here 'context-navigator-interactive)
                        (or (next-single-property-change here 'context-navigator-interactive nil (point-max))
                            (point-max))
                      (1+ here)))
           (pos (context-navigator-view--find-next-interactive-pos cur-end)))
      (unless pos
        (setq pos (context-navigator-view--find-next-interactive-pos (point-min))))
      (if pos (goto-char pos) (message "%s" (context-navigator-i18n :no-interactive-elements))))))

(defun context-navigator-view-tab-previous ()
  "Move point to the previous interactive element. Wrap at start."
  (interactive)
  (let* ((here (point))
         (cur-beg (if (get-text-property here 'context-navigator-interactive)
                      (or (previous-single-property-change here 'context-navigator-interactive nil (point-min))
                          (point-min))
                    here))
         (pos (context-navigator-view--find-prev-interactive-pos cur-beg)))
    (unless pos
      (setq pos (context-navigator-view--find-prev-interactive-pos (point-max))))
    (if pos (goto-char pos) (message "%s" (context-navigator-i18n :no-interactive-elements)))))



(defun context-navigator-view-toggle-enabled ()
  "Toggle inclusion of the item at point in gptel sync (model :enabled).

Toggles the item's enabled flag and, when push→gptel is ON, applies the
updated set to gptel immediately. The new state is persisted via the
existing debounced autosave."
  (interactive)
  (when-let* ((item (context-navigator-view--at-item)))
    (let* ((key (context-navigator-model-item-key item))
           (name (or (context-navigator-item-name item) key)))
      ;; Push snapshot for Undo/Redo (global) before changing the model
      (when (fboundp 'context-navigator-snapshot-push)
        (ignore-errors (context-navigator-snapshot-push)))
      ;; Toggle model
      (ignore-errors (context-navigator-toggle-item key))
      ;; Apply to gptel immediately when auto-push is ON and refresh indicators snapshot
      (when (and (boundp 'context-navigator--push-to-gptel)
                 context-navigator--push-to-gptel)
        (let* ((st (ignore-errors (context-navigator--state-get)))
               (items (and st (context-navigator-state-items st))))
          (let ((res (ignore-errors (context-navigator-gptel-apply (or items '())))))
            (ignore-errors
              (context-navigator-debug :debug :ui "toggle:t apply -> %S" res)))
          ;; Immediately refresh cached keys so lamps reflect actual presence now.
          (let* ((lst (ignore-errors (context-navigator-gptel-pull)))
                 (pulled-keys (and (listp lst)
                                   (mapcar #'context-navigator-model-item-key lst)))
                 (raw-keys (and (or (null pulled-keys) (= (length pulled-keys) 0))
                                (fboundp 'context-navigator-gptel--raw-keys)
                                (ignore-errors (context-navigator-gptel--raw-keys))))
                 (fallback-keys
                  (when (and (or (null pulled-keys) (= (length pulled-keys) 0))
                             (or (null raw-keys) (= (length raw-keys) 0)))
                    (and (listp items)
                         (mapcar #'context-navigator-model-item-key
                                 (cl-remove-if-not #'context-navigator-item-enabled items)))))
                 (keys (or pulled-keys raw-keys fallback-keys '())))
            (let ((h (sxhash-equal keys)))
              (setq context-navigator-view--gptel-keys keys
                    context-navigator-view--gptel-keys-hash h)
              (ignore-errors
                (context-navigator-debug :debug :ui
                                         "toggle:t immediate pull -> keys=%d hash=%s%s"
                                         (length keys) h
                                         (if (and (or (null pulled-keys) (= (length pulled-keys) 0))
                                                  (or (null raw-keys) (= (length raw-keys) 0)))
                                             " (fallback)" "")))))))
      ;; Report new state briefly
      (let* ((st2 (ignore-errors (context-navigator--state-get)))
             (idx (and st2 (context-navigator-state-index st2)))
             (it2 (and idx (gethash key idx)))
             (en (and it2 (context-navigator-item-enabled it2))))
        (message (context-navigator-i18n (if en :item-enabled :item-disabled)) name))
      ;; Refresh UI and advance to the next item
      (context-navigator-view--schedule-render)
      (context-navigator-view--render-if-visible)
      (context-navigator-view-next-item))))

(defun context-navigator-view-toggle-gptel ()
  "Toggle gptel membership for the item at point (no mass apply).

Order of operations:
- change gptel context first (add/remove item)
- then refresh lamps/indicators in the sidebar."
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
      (context-navigator-view-next-item)
      (pcase res
        (:added   (message (context-navigator-i18n :gptel-added-one) (or (context-navigator-item-name item) key)))
        (:removed (message (context-navigator-i18n :gptel-removed-one) (or (context-navigator-item-name item) key)))
        (_        (message (context-navigator-i18n :gptel-no-change) (or (context-navigator-item-name item) key)))))))

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
      (message (context-navigator-i18n :deleted-from-model) (or (context-navigator-item-name item) key)))))

(defun context-navigator-view-refresh ()
  "Force re-render of the sidebar, if visible."
  (interactive)
  (context-navigator-view--render-if-visible))

(defun context-navigator-view-quit ()
  "Close the sidebar window. Remove subscriptions but do not kill the buffer."
  (interactive)
  (when-let* ((buf (get-buffer context-navigator-view--buffer-name)))
    (with-current-buffer buf
      (context-navigator-view--remove-subs))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (window-live-p win)
        (delete-window win)))
    ;; Remove window-balance protections when Navigator windows are gone.
    (ignore-errors
      (when (fboundp 'context-navigator-view-windows-teardown)
        (context-navigator-view-windows-teardown)))))

(defun context-navigator-view--subscribe-model-events ()
  "Subscribe to generic model refresh events."
  (push (context-navigator-events-subscribe
         :model-refreshed
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (context-navigator-view--invalidate-openable)
                 (when (fboundp 'context-navigator-stats-invalidate)
                   (context-navigator-stats-invalidate))
                 (let ((st (ignore-errors (context-navigator--state-get))))
                   (if (and (context-navigator-state-p st)
                            (context-navigator-state-loading-p st))
                       (context-navigator-view--render-if-visible)
                     (context-navigator-view--schedule-render))))))))
        context-navigator-view--subs))

(defun context-navigator-view--subscribe-load-events ()
  "Subscribe to context loading lifecycle events."
  (push (context-navigator-events-subscribe
         :context-load-start
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq context-navigator-view--load-progress (cons 0 0))
                 (context-navigator-view--invalidate-openable)
                 (when (fboundp 'context-navigator-stats-invalidate)
                   (context-navigator-stats-invalidate))
                 ;; На старте не крутим спиннер — показываем статический индикатор.
                 (context-navigator-view--spinner-stop)
                 (context-navigator-view--render-if-visible))))))
        context-navigator-view--subs)
  (push (context-navigator-events-subscribe
         :context-load-step
         (lambda (_root pos total)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq context-navigator-view--load-progress (cons pos total))
                 ;; Крутим спиннер только после появления шагов (процентов).
                 (context-navigator-view--spinner-start)
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs)
  (push (context-navigator-events-subscribe
         :context-load-done
         (lambda (root ok-p)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq context-navigator-view--load-progress nil)
                 (context-navigator-view--invalidate-openable)
                 (when (fboundp 'context-navigator-stats-invalidate)
                   (context-navigator-stats-invalidate))
                 (context-navigator-view--spinner-stop)
                 (unless ok-p
                   (let* ((st (ignore-errors (context-navigator--state-get)))
                          (slug (and st (context-navigator-state-current-group-slug st)))
                          (file (and slug (ignore-errors (context-navigator-persist-context-file root slug)))))
                     (when (and (stringp file) (file-exists-p file))
                       (message (context-navigator-i18n :group-file-unreadable-hint) (or slug "<unknown>")))
                     (when context-navigator-auto-open-groups-on-error
                       (setq context-navigator-view--mode 'groups)
                       (ignore-errors (context-navigator-groups-open)))))
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs))

(defun context-navigator-view--subscribe-groups-events ()
  "Subscribe to groups list updates."
  (push (context-navigator-events-subscribe
         :groups-list-updated
         (lambda (root groups)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (let* ((st (ignore-errors (context-navigator--state-get)))
                        (cur-root (and st (context-navigator-state-last-project-root st))))
                   ;; Защита от устаревших событий: принимаем только для актуального root
                   (when (equal root cur-root)
                     (setq context-navigator-view--groups groups)
                     (when (eq context-navigator-view--mode 'groups)
                       (context-navigator-view--schedule-render)))))))))
        context-navigator-view--subs))

(defun context-navigator-view--subscribe-project-events ()
  "Subscribe to project switch updates to refresh groups list when needed."
  (push (context-navigator-events-subscribe
         :project-switch
         (lambda (root)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 ;; cd into the project root (or a sensible global dir) for the Navigator buffer
                 (let* ((proj-dir (and (stringp root)
                                       (not (string-empty-p root))
                                       (expand-file-name root)))
                        (global-dir (and (boundp 'context-navigator-global-dir)
                                         (expand-file-name (symbol-value 'context-navigator-global-dir))))
                        (target
                         (cond
                          ((and proj-dir (file-directory-p proj-dir)) (file-name-as-directory proj-dir))
                          ((and global-dir (file-directory-p global-dir)) (file-name-as-directory global-dir))
                          ((file-directory-p (expand-file-name "~")) (file-name-as-directory (expand-file-name "~")))
                          (t default-directory))))
                   (setq default-directory target))
                 ;; Invalidate cached groups and refresh when groups view is active.
                 (setq context-navigator-view--groups nil)
                 (when (eq context-navigator-view--mode 'groups)
                   (ignore-errors (context-navigator-groups-open))
                   (context-navigator-view--schedule-render)))))))
        context-navigator-view--subs))
;; GPTel indicator helpers moved to context-navigator-view-indicators.el
;; Implementations live in that module; the view simply requires it at top
;; so functions like `context-navigator-view--collect-gptel-keys',
;; `context-navigator-view--update-gptel-keys-if-changed',
;; `context-navigator-view--maybe-refresh-gptel-keys',
;; `context-navigator-view--on-gptel-change',
;; `context-navigator-view--subscribe-gptel-events',
;; `context-navigator-view--init-gptel-cache' and
;; `context-navigator-view--start-gptel-poll-timer' are available at runtime.
;; Declare these symbols to keep the byte-compiler and tooling happy.
(declare-function context-navigator-view--collect-gptel-keys "context-navigator-view-indicators" ())
(declare-function context-navigator-view--update-gptel-keys-if-changed "context-navigator-view-indicators" (keys))
(declare-function context-navigator-view--maybe-refresh-gptel-keys "context-navigator-view-indicators" ())
(declare-function context-navigator-view--on-gptel-change "context-navigator-view-indicators" (&rest args))
(declare-function context-navigator-view--subscribe-gptel-events "context-navigator-view-indicators" ())
(declare-function context-navigator-view--init-gptel-cache "context-navigator-view-indicators" ())
(declare-function context-navigator-view--start-gptel-poll-timer "context-navigator-view-indicators" ())

;; Implementations live in context-navigator-view-indicators.el. See that
;; module for details and further refactoring.

(defun context-navigator-view--install-subs ()
  "Subscribe to relevant events (buffer-local tokens).
Guard against duplicate subscriptions."
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
    ;; Install buffer-list and window selection hooks (may be no-ops in tests,
    ;; implementations are provided below to avoid void-function errors).
    (context-navigator-view--install-buffer-list-hook)
    (context-navigator-view--install-window-select-hook)
    ;; Update modeline status as point moves inside the buffer
    (setq context-navigator-view--status-post-cmd-fn
          (lambda ()
            (when (and (eq major-mode 'context-navigator-view-mode)
                       (boundp 'context-navigator-view-modeline-enable)
                       context-navigator-view-modeline-enable)
              (force-mode-line-update nil))))
    (add-hook 'post-command-hook context-navigator-view--status-post-cmd-fn nil t)
    (context-navigator-view--initial-compute-counters)
    ;; Optional polling
    (context-navigator-view--start-gptel-poll-timer)
    ;; Гарантированная отписка при убийстве буфера (локально)
    (add-hook 'kill-buffer-hook #'context-navigator-view--remove-subs nil t)))

(defun context-navigator-view--remove-subs ()
  "Unsubscribe buffer-local tokens."
  (when context-navigator-view--subs
    (mapc #'context-navigator-events-unsubscribe context-navigator-view--subs)
    (setq context-navigator-view--subs nil))
  ;; Remove buffer-list hook if installed.
  (when context-navigator-view--buflist-fn
    (remove-hook 'buffer-list-update-hook context-navigator-view--buflist-fn)
    (setq context-navigator-view--buflist-fn nil))
  ;; Cancel timers and drop cached counters.
  (context-navigator-view--invalidate-openable)
  (context-navigator-view--spinner-stop)
  ;; Remove focus render hook if installed.
  (when context-navigator-view--winselect-fn
    (remove-hook 'window-selection-change-functions context-navigator-view--winselect-fn)
    (setq context-navigator-view--winselect-fn nil))
  ;; Remove post-command status updater if installed.
  (when context-navigator-view--status-post-cmd-fn
    (remove-hook 'post-command-hook context-navigator-view--status-post-cmd-fn t)
    (setq context-navigator-view--status-post-cmd-fn nil))
  ;; Cancel gptel poll timer if running.
  (when (timerp context-navigator-view--gptel-poll-timer)
    (cancel-timer context-navigator-view--gptel-poll-timer)
    (setq context-navigator-view--gptel-poll-timer nil)))

(defun context-navigator-view--format-bindings (pairs map)
  "Format help lines for PAIRS using MAP.
PAIRS is an alist of (COMMAND . DESCRIPTION).
MAP is a keymap to search for COMMAND bindings."
  (mapconcat
   (lambda (cell)
     (let* ((cmd (car cell))
            (desc (cdr cell))
            (keys (mapcar #'key-description (where-is-internal cmd map))))
       (format "%-18s %s"
               (if keys (string-join keys ", ") "<unbound>")
               (or desc (symbol-name cmd)))))
   pairs
   "\n"))

(defun context-navigator-view-help ()
  "Show localized, column-formatted help for Context Navigator without truncation."
  (interactive)
  (with-help-window "*Context Navigator Help*"
    (let* ((map context-navigator-view-mode-map)
           ;; Command → i18n key for description
           (pairs '((context-navigator-view-next-item         . :help-next-item)
                    (context-navigator-view-previous-item     . :help-previous-item)
                    (context-navigator-view-activate          . :help-activate)
                    (context-navigator-view-preview           . :help-preview)
                    (context-navigator-view-toggle-enabled     . :help-toggle-gptel)
                    (context-navigator-view-delete-dispatch   . :help-delete)
                    (context-navigator-view-refresh-dispatch  . :help-refresh)
                    (context-navigator-view-go-up             . :help-go-up)
                    (context-navigator-view-group-create      . :help-group-create)
                    (context-navigator-view-group-rename      . :help-group-rename)
                    (context-navigator-view-group-duplicate   . :help-group-duplicate)
                    (context-navigator-view-toggle-push       . :help-toggle-push)
                    (context-navigator-view-toggle-auto-project . :help-toggle-auto)
                    (context-navigator-view-open-all-buffers  . :help-open-all)
                    (context-navigator-view-push-now          . :help-push-now)
                    (context-navigator-view-clear-group       . :help-clear-group)
                    (context-navigator-view-clear-gptel       . :help-clear-gptel)
                    (context-navigator-view-quit              . :help-quit)
                    (context-navigator-view-help              . :help-help)))
           ;; Build (keys . desc) then padded line strings
           (rows-raw
            (mapcar
             (lambda (cell)
               (let* ((cmd  (car cell))
                      (desc (context-navigator-i18n (cdr cell)))
                      (keys (mapcar #'key-description (where-is-internal cmd map)))
                      (ks   (if keys (string-join keys ", ") "<unbound>")))
                 (cons ks desc)))
             pairs))
           (keyw (apply #'max 0 (mapcar (lambda (x) (string-width (car x))) rows-raw)))
           (lines (mapcar (lambda (x) (format (format "%%-%ds %%s" (max 14 keyw)) (car x) (cdr x)))
                          rows-raw))
           ;; Detect/help the real window width for proper column calculation.
           (ww (let* ((buf "*Context Navigator Help*")
                      (win (or (get-buffer-window buf t)
                               (get-buffer-window (current-buffer) t)))
                      (maxw (apply #'max 80 (mapcar #'window-body-width (window-list)))))
                 (or (and win (window-body-width win))
                     maxw
                     (frame-width)
                     80)))
           (spacing "  ")
           ;; Try to place in 3/2/1 columns to fit without truncation.
           (choose-cols
            (lambda ()
              (let ((n (length lines)))
                (cl-loop for c in '(3 2 1) do
                         (let* ((cols c)
                                (rows (ceiling (/ (float n) (max 1 cols))))
                                ;; compute column widths for this layout
                                (colw
                                 (cl-loop for ci from 0 below cols collect
                                          (let ((w 0))
                                            (cl-loop for ri from 0 below rows
                                                     for idx = (+ ri (* ci rows))
                                                     when (< idx n) do
                                                     (setq w (max w (string-width (nth idx lines)))))
                                            w)))
                                (total (+ (apply #'+ colw) (* (string-width spacing) (1- cols)))))
                           (when (<= total ww)
                             (cl-return cols))))
                1)))  ;; default 1
           (cols (funcall choose-cols))
           (rows (ceiling (/ (float (length lines)) (max 1 cols))))
           ;; Recompute exact per-column widths for chosen layout
           (colw
            (cl-loop for ci from 0 below cols collect
                     (let ((w 0))
                       (cl-loop for ri from 0 below rows
                                for idx = (+ ri (* ci rows))
                                when (< idx (length lines)) do
                                (setq w (max w (string-width (nth idx lines)))))
                       w))))
      ;; Title
      (princ (context-navigator-i18n :help-title)) (princ "\n\n")
      ;; Emit lines as rows×cols grid with padding; no truncation.
      (dotimes (r rows)
        (let ((acc ""))
          (dotimes (c cols)
            (let* ((idx (+ r (* c rows)))
                   (s (or (nth idx lines) ""))
                   (pad (if (< c (1- cols))
                            (format (format "%%-%ds" (nth c colw)) s)
                          s)))
              (setq acc (if (string-empty-p acc) pad (concat acc spacing pad)))))
          (princ acc) (princ "\n")))
      (princ "\n")
      ;; Global keys section (localized)
      (princ (context-navigator-i18n :help-global-title)) (princ "\n")
      (princ (context-navigator-i18n :help-global-summary)) (princ "\n\n")
      ;; Groups mode summary (localized)
      (princ (context-navigator-i18n :help-groups-summary)) (princ "\n"))))

;; Ensure autoload for the transient entry so “?” works even without generated autoloads
(autoload 'context-navigator-view-transient "context-navigator-transient"
  "Open Context Navigator transient menu." t)

(defun context-navigator-view-open-menu ()
  "Open Navigator menu (transient) or fallback to Help when unavailable."
  (interactive)
  ;; Make sure transient is available if installed
  (unless (featurep 'transient)
    (require 'transient nil t))
  ;; Best-effort load of our transient menu
  (unless (fboundp 'context-navigator-view-transient)
    (ignore-errors (require 'context-navigator-transient)))
  (if (fboundp 'context-navigator-view-transient)
      (call-interactively 'context-navigator-view-transient)
    (call-interactively 'context-navigator-view-help)))

(defvar context-navigator-view-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Dispatch RET depending on mode
    (define-key m (kbd "RET")       #'context-navigator-view-activate)
    (define-key m (kbd "C-m")       #'context-navigator-view-activate)
    (define-key m [return]          #'context-navigator-view-activate)
    (define-key m (kbd "<return>")  #'context-navigator-view-activate)
    (define-key m [kp-enter]        #'context-navigator-view-activate)
    (define-key m (kbd "v")       #'context-navigator-view-preview)
    (define-key m (kbd "n")   #'context-navigator-view-next-item)
    (define-key m (kbd "p")   #'context-navigator-view-previous-item)
    ;; Vim-like navigation keys
    (define-key m (kbd "j")   #'context-navigator-view-next-item)
    (define-key m (kbd "k")   #'context-navigator-view-previous-item)
    (define-key m (kbd "<down>") #'context-navigator-view-next-item)
    (define-key m (kbd "<up>")   #'context-navigator-view-previous-item)
    (define-key m (kbd "l")   #'context-navigator-view-activate)
    (define-key m (kbd "SPC")   #'context-navigator-view-toggle-enabled)
    (define-key m (kbd "t")   #'context-navigator-view-toggle-enabled)
    (define-key m (kbd "m")   #'context-navigator-view-toggle-enabled)
    (define-key m (kbd "M")   #'context-navigator-view-toggle-all-gptel)

    ;; TAB navigation between interactive elements
    ;; Bind several TAB event representations to be robust across terminals/minor-modes.
    (define-key m (kbd "TAB")       #'context-navigator-view-tab-next)
    (define-key m (kbd "<tab>")     #'context-navigator-view-tab-next)
    (define-key m [tab]             #'context-navigator-view-tab-next)
    (define-key m (kbd "C-i")       #'context-navigator-view-tab-next)
    (define-key m (kbd "<backtab>") #'context-navigator-view-tab-previous)
    (define-key m [backtab]         #'context-navigator-view-tab-previous)
    (define-key m (kbd "S-<tab>")   #'context-navigator-view-tab-previous)
    ;; Remap global indent command to our TAB-next to ensure override everywhere.
    (define-key m [remap indent-for-tab-command] #'context-navigator-view-tab-next)

    ;; Ensure delete-other-windows behaves sensibly when the sidebar is present:
    ;; close sidebar windows first to avoid making a side window the only window.
    (define-key m [remap delete-other-windows] #'context-navigator-delete-other-windows)

    ;; New global toggles/actions in sidebar
    (define-key m (kbd "G")   #'context-navigator-view-show-groups)
    (define-key m (kbd "x")   #'context-navigator-view-toggle-push)
    (define-key m (kbd "A")   #'context-navigator-view-toggle-auto-project)
    (define-key m (kbd "P")   #'context-navigator-view-push-now)
    ;; Stats toggle
    (define-key m (kbd "s")   #'context-navigator-view-stats-toggle)

    ;; Align with transient: U → unload context
    (define-key m (kbd "U")   #'context-navigator-context-unload)
    (define-key m (kbd "C")   #'context-navigator-view-clear-gptel)
    ;; d and g are dispatched depending on mode
    (define-key m (kbd "d")   #'context-navigator-view-delete-dispatch)
    (define-key m (kbd "g")   #'context-navigator-view-refresh-dispatch)
    ;; Additional action: open all context buffers in background
    (define-key m (kbd "O")   #'context-navigator-view-open-all-buffers)
    (define-key m (kbd "o")   #'context-navigator-view-open-all-buffers)
    (define-key m (kbd "K")   #'context-navigator-view-close-all-buffers)
    ;; Clear group (explicit shortcut matching UI hint)
    (define-key m (kbd "E")   #'context-navigator-view-clear-group)
    ;; Groups-specific keys
    (define-key m (kbd "u")   #'context-navigator-view-go-up)      ;; show groups from items (Up)
    (define-key m (kbd "h")   #'context-navigator-view-go-up)      ;; alias (help/docs use h)
    (define-key m (kbd "a")   #'context-navigator-view-group-create)
    (define-key m (kbd "+")   #'context-navigator-view-group-create)
    (define-key m (kbd "r")   #'context-navigator-view-group-rename)
    (define-key m (kbd "e")   #'context-navigator-view-group-edit-description)
    (define-key m (kbd "c")   #'context-navigator-view-group-duplicate)
    (define-key m (kbd "q")   #'context-navigator-view-quit)
    (define-key m (kbd "?")   #'context-navigator-view-open-menu)
    m)
  "Keymap for =context-navigator-view-mode'.")

;; Ensure bindings are updated after reloads (defvar won't reinitialize an existing keymap).
(when (keymapp context-navigator-view-mode-map)
  ;; Keep legacy binding in sync
  (define-key context-navigator-view-mode-map (kbd "SPC") #'context-navigator-view-toggle-enabled)
  (define-key context-navigator-view-mode-map (kbd "t") #'context-navigator-view-toggle-enabled)
  ;; Ensure RET variants are bound across terminals/tty/GUI
  (define-key context-navigator-view-mode-map (kbd "RET")       #'context-navigator-view-activate)
  (define-key context-navigator-view-mode-map (kbd "C-m")       #'context-navigator-view-activate)
  (define-key context-navigator-view-mode-map [return]          #'context-navigator-view-activate)
  (define-key context-navigator-view-mode-map (kbd "<return>")  #'context-navigator-view-activate)
  (define-key context-navigator-view-mode-map [kp-enter]        #'context-navigator-view-activate)
  ;; Make TAB robust across reloads/terminals/minor-modes
  (define-key context-navigator-view-mode-map (kbd "TAB")       #'context-navigator-view-tab-next)
  (define-key context-navigator-view-mode-map (kbd "<tab>")     #'context-navigator-view-tab-next)
  (define-key context-navigator-view-mode-map [tab]             #'context-navigator-view-tab-next)
  (define-key context-navigator-view-mode-map (kbd "C-i")       #'context-navigator-view-tab-next)
  (define-key context-navigator-view-mode-map (kbd "<backtab>") #'context-navigator-view-tab-previous)
  (define-key context-navigator-view-mode-map [backtab]         #'context-navigator-view-tab-previous)
  (define-key context-navigator-view-mode-map (kbd "S-<tab>")   #'context-navigator-view-tab-previous)
  ;; Ensure remaps also apply after reload
  (define-key context-navigator-view-mode-map
              [remap indent-for-tab-command] #'context-navigator-view-tab-next)
  (define-key context-navigator-view-mode-map
              [remap delete-other-windows]   #'context-navigator-delete-other-windows)
  ;; Arrow keys
  (define-key context-navigator-view-mode-map (kbd "<down>") #'context-navigator-view-next-item)
  (define-key context-navigator-view-mode-map (kbd "<up>")   #'context-navigator-view-previous-item)
  ;; Groups navigation alias: keep 'h' consistent with help/docs
  (define-key context-navigator-view-mode-map (kbd "h") #'context-navigator-view-go-up)
  ;; New binding sync after reloads (keep in sync with primary map)
  (define-key context-navigator-view-mode-map (kbd "G") #'context-navigator-view-show-groups)
  (define-key context-navigator-view-mode-map (kbd "x") #'context-navigator-view-toggle-push)
  (define-key context-navigator-view-mode-map (kbd "U") #'context-navigator-context-unload)
  (define-key context-navigator-view-mode-map (kbd "K") #'context-navigator-view-close-all-buffers)
  (define-key context-navigator-view-mode-map (kbd "s") #'context-navigator-view-stats-toggle)
  (define-key context-navigator-view-mode-map (kbd "E") #'context-navigator-view-clear-group)
  (define-key context-navigator-view-mode-map (kbd "e") #'context-navigator-view-group-edit-description)
  (define-key context-navigator-view-mode-map (kbd "?") #'context-navigator-view-open-menu))
;; Ensure group line keymap inherits major mode map so keyboard works on group lines
(when (and (keymapp context-navigator-view--group-line-keymap)
           (keymapp context-navigator-view-mode-map))
  (set-keymap-parent context-navigator-view--group-line-keymap context-navigator-view-mode-map))
(when (and (boundp 'context-navigator-view--title-line-keymap)
           (keymapp context-navigator-view--title-line-keymap)
           (keymapp context-navigator-view-mode-map))
  (set-keymap-parent context-navigator-view--title-line-keymap context-navigator-view-mode-map))

(defun context-navigator-view--hl-line-range ()
  "Return region to highlight for the current line.

Highlight:
- title line (has 'context-navigator-title)
- Stats header (has 'context-navigator-stats-toggle)
- item lines (have 'context-navigator-item)
- group lines (have 'context-navigator-group-slug)
- the \"..\" line (has 'context-navigator-groups-up)
- Stats content lines (have 'context-navigator-stats-line)

Do not highlight purely decorative separators."
  (when (or (get-text-property (point) 'context-navigator-title)
            (get-text-property (point) 'context-navigator-stats-toggle)
            (get-text-property (point) 'context-navigator-item)
            (get-text-property (point) 'context-navigator-group-slug)
            (get-text-property (point) 'context-navigator-groups-up)
            (get-text-property (point) 'context-navigator-stats-line))
    (cons (line-beginning-position)
          (min (point-max) (1+ (line-end-position))))))

(define-derived-mode context-navigator-view-mode special-mode "Context-Nav"
  "Major mode for context-navigator sidebar buffer."
  (buffer-disable-undo)
  (setq truncate-lines t
        cursor-type t)
  ;; Ensure our header-line face is remapped for every Navigator buffer.
  (when (fboundp 'context-navigator-view-controls--ensure-headerline-face)
    (context-navigator-view-controls--ensure-headerline-face))
  ;; Install modeline/header-line for the sidebar buffer.
  ;; Use the dedicated helper so we show the status even when the global
  ;; mode-line is disabled (fallback to header-line).
  (when (fboundp 'context-navigator-modeline--apply)
    (context-navigator-modeline--apply (current-buffer)))
  ;; Apply header-line controls (toggles and actions) for Navigator.
  (when (fboundp 'context-navigator-headerline--apply)
    (context-navigator-headerline--apply (current-buffer)))
  (setq-local hl-line-range-function #'context-navigator-view--hl-line-range)
  (hl-line-mode 1))

;;;###autoload
(defun context-navigator-view-open ()
  "Open the context-navigator sidebar on the left."
  (interactive)
  (let* ((buf (get-buffer-create context-navigator-view--buffer-name))
         (win (display-buffer-in-side-window buf
                                             (append
                                              context-navigator-view-window-params
                                              (list (cons 'window-width
                                                          (or (and (boundp 'context-navigator-view-width)
                                                                   (symbol-value 'context-navigator-view-width))
                                                              33)))))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (context-navigator-view-mode)
        (setq-local buffer-read-only t)
        (context-navigator-view--install-subs)
        (context-navigator-view--render)))
    (when (window-live-p win)
      ;; Mark this window as our sidebar so visit logic can detect and avoid replacing it.
      ;; If the buffer is shown in multiple windows (rare), mark them all.
      (dolist (w (get-buffer-window-list buf nil t))
        (when (window-live-p w)
          (set-window-parameter w 'context-navigator-view 'sidebar)))
      ;; Ensure window-balance protections are active when Navigator is in use.
      (ignore-errors
        (when (fboundp 'context-navigator-view-windows-setup)
          (context-navigator-view-windows-setup)))
      (select-window win))
    win))

;;;###autoload
(defun context-navigator-view-close ()
  "Close the context-navigator sidebar if visible."
  (interactive)
  (context-navigator-view-quit))

;;;###autoload
(defun context-navigator-view-toggle ()
  "Toggle the context-navigator sidebar."
  (interactive)
  (if (get-buffer-window context-navigator-view--buffer-name t)
      (context-navigator-view-close)
    (context-navigator-view-open)))

;;; Buffer-mode (magit-like) entry points

(defun context-navigator--buffer-mode--split (direction size)
  "Split selected window in DIRECTION ('right or 'below) using SIZE."
  (let* ((base (selected-window))
         (cols (window-total-width base))
         (rows (window-total-height base))
         (amt (cond
               ((and (numberp size) (> size 0) (< size 1))
                (if (eq direction 'right) (floor (* cols size)) (floor (* rows size))))
               ((and (numberp size) (>= size 1)) size)
               (t nil))))
    (split-window base amt direction)))

;;;###autoload
(defun context-navigator-buffer-open ()
  "Open the Navigator buffer in a regular window (magit-like).
- Reuse other window when available, else split (right by default).
- Always select the Navigator window."
  (interactive)
  (let* ((buf (get-buffer-create context-navigator-view--buffer-name))
         (visible (get-buffer-window buf 0))
         (placement (if (boundp 'context-navigator-buffer-placement)
                        context-navigator-buffer-placement
                      'reuse-other-window))
         (split-size (if (boundp 'context-navigator-buffer-split-size)
                         context-navigator-buffer-split-size
                       0.5))
         win)
    (if (window-live-p visible)
        (setq win visible)
      (pcase placement
        ('reuse-other-window
         (let* ((wins (seq-filter (lambda (w) (and (window-live-p w)
                                                   (not (eq w (selected-window)))))
                                  (window-list (selected-frame) 'no-minibuffer)))
                (w (car wins)))
           (if (window-live-p w)
               (setq win w)
             (setq win (context-navigator--buffer-mode--split 'right split-size)))))
        ('split-right
         (setq win (context-navigator--buffer-mode--split 'right split-size)))
        ('split-below
         (setq win (context-navigator--buffer-mode--split 'below split-size)))
        (_
         (setq win (context-navigator--buffer-mode--split 'right split-size)))))
    (when (window-live-p win)
      (set-window-buffer win buf)
      (when (window-live-p win)
        (set-window-parameter win 'context-navigator-view 'buffer))
      ;; Ensure window-balance protections are active in buffer mode as well.
      (ignore-errors
        (when (fboundp 'context-navigator-view-windows-setup)
          (context-navigator-view-windows-setup)))
      (with-current-buffer buf
        (context-navigator-view-mode)
        (setq-local buffer-read-only t)
        (context-navigator-view--install-subs)
        (context-navigator-view--render))
      (select-window win))
    win))

;;;###autoload
(defun context-navigator-buffer-close ()
  "Close Navigator buffer windows on the current frame (do not kill the buffer)."
  (interactive)
  (let* ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (dolist (w (window-list (selected-frame) 'no-minibuffer))
        (when (and (window-live-p w)
                   (eq (window-buffer w) buf))
          (delete-window w)))
      ;; Remove window-balance protections if no Navigator windows remain.
      (ignore-errors
        (when (fboundp 'context-navigator-view-windows-teardown)
          (context-navigator-view-windows-teardown))))))

;;;###autoload
(defun context-navigator-buffer-toggle ()
  "Toggle Navigator buffer visibility on the current frame."
  (interactive)
  (if (get-buffer-window context-navigator-view--buffer-name 0)
      (context-navigator-buffer-close)
    (context-navigator-buffer-open)))

;; Helpers for group mode

(defun context-navigator-view--at-group ()
  "Return cons (SLUG . DISPLAY) for group at point, or nil."
  (let* ((slug (get-text-property (point) 'context-navigator-group-slug))
         (disp (get-text-property (point) 'context-navigator-group-display)))
    (when (and (stringp slug) (not (string-empty-p slug)))
      (cons slug (or disp slug)))))

(defun context-navigator-view--open-group-at-point ()
  "Switch to group at point and return t on success."
  (interactive)
  (when-let* ((cell (context-navigator-view--at-group))
              (slug (car cell)))
    (ignore-errors (context-navigator-group-switch slug))
    (setq context-navigator-view--mode 'items)
    (context-navigator-view--schedule-render)
    t))

(defun context-navigator-view-mouse-open-group (event)
  "Open group at mouse EVENT position."
  (interactive "e")
  (mouse-set-point event)
  (context-navigator-view--open-group-at-point))

;; Sidebar wrappers for global toggles/actions
(defun context-navigator-view-toggle-push ()
  "Toggle push-to-gptel session flag and refresh header immediately."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: toggle-push (event=%S)" last-input-event))
  (ignore-errors (context-navigator-toggle-push-to-gptel))
  ;; Immediately refresh cached keys from gptel so indicators reflect actual presence,
  ;; regardless of push flag.
  (let* ((lst (ignore-errors (context-navigator-gptel-pull)))
         (keys (and (listp lst)
                    (mapcar #'context-navigator-model-item-key lst)))
         (h (sxhash-equal keys)))
    (setq context-navigator-view--gptel-keys keys
          context-navigator-view--gptel-keys-hash h)
    (ignore-errors
      (context-navigator-debug :debug :ui
                               "toggle:push -> keys=%d hash=%s"
                               (length (or keys '())) h)))
  ;; Force immediate redraw for visible sidebar
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local context-navigator-render--last-hash nil)
        (setq-local context-navigator-view--last-render-key nil))))
  (context-navigator-view--render-if-visible))

(defun context-navigator-view-toggle-auto-project ()
  "Toggle auto-project-switch session flag and refresh header immediately."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: toggle-auto (event=%S)" last-input-event))
  (ignore-errors (context-navigator-toggle-auto-project-switch))
  ;; Force immediate redraw for visible sidebar
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local context-navigator-render--last-hash nil)
        (setq-local context-navigator-view--last-render-key nil))))
  (context-navigator-view--render-if-visible))

(defun context-navigator-view-push-now ()
  "Manually push current items to gptel (reset + add) and redraw immediately."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: push-now (event=%S)" last-input-event))
  (ignore-errors (context-navigator-push-to-gptel-now))
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local context-navigator-render--last-hash nil)
        (setq-local context-navigator-view--last-render-key nil))))
  (context-navigator-view--render-if-visible))

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
      (message "%s" (context-navigator-i18n :razor-only-org-mode)))))

;;;###autoload
(defun context-navigator-view-open-all-buffers ()
  "Open all file/buffer/selection items from current model in background (no window selection).

This opens file-backed buffers (via `find-file-noselect') for items of
type `file', `buffer' and `selection' when they reference an existing file.
Buffers are opened in background; we do not change window focus."
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
    (message (context-navigator-i18n :opened-context-buffers-bg) count)))

(defun context-navigator-view-close-all-buffers ()
  "Close all live buffers that belong to items in the current model."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: close-all-buffers (event=%S)" last-input-event))
  (let* ((bufs (context-navigator-view--collect-closable-buffers))
         (count 0))
    (dolist (b bufs)
      (when (buffer-live-p b)
        (ignore-errors (kill-buffer b))
        (setq count (1+ count))))
    (context-navigator-view--schedule-render)
    (message (context-navigator-i18n :closed-context-buffers) count)))

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
        (setq-local context-navigator-render--last-hash nil)
        (setq-local context-navigator-view--last-render-key nil))))
  (context-navigator-view--render-if-visible))

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
        (setq-local context-navigator-render--last-hash nil)
        (setq-local context-navigator-view--last-render-key nil)
        ;; Clear the cached gptel keys snapshot so lamps update immediately.
        (setq-local context-navigator-view--gptel-keys nil)
        (setq-local context-navigator-view--gptel-keys-hash (sxhash-equal '())))))
  (context-navigator-view--render-if-visible))

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

;;; Dispatchers and commands

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
     ((eq act 'razor) (context-navigator-view-razor-run))
     ;; Header toggles
     ((eq tgl 'push) (context-navigator-view-toggle-push))
     ((eq tgl 'auto) (context-navigator-view-toggle-auto-project))
     ((eq context-navigator-view--mode 'groups)
      (or (context-navigator-view--open-group-at-point)
          (message "%s" (context-navigator-i18n :no-group-at-point))))
     (t
      (if (get-text-property (point) 'context-navigator-groups-up)
          (context-navigator-view-go-up)
        (context-navigator-view-visit))))))

(defun context-navigator-view-refresh-dispatch ()
  "g action: refresh items or groups, depending on mode."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (ignore-errors (context-navigator-groups-open))
    (context-navigator-view-refresh)))

(defun context-navigator-view-delete-dispatch ()
  "d action: delete item (items mode) or group (groups mode)."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (if-let* ((cell (context-navigator-view--at-group)))
          (ignore-errors (context-navigator-group-delete (car cell)))
        (message "%s" (context-navigator-i18n :no-group-at-point)))
    (context-navigator-view-delete-from-model)))

(defun context-navigator-view-go-up ()
  "Switch from items to groups view; in groups view do nothing.

- From items -> switch to groups and fetch list
- From groups -> no-op"
  (interactive)
  (when (not (eq context-navigator-view--mode 'groups))
    (setq context-navigator-view--mode 'groups)
    (ignore-errors (context-navigator-groups-open))
    ;; Render immediately for responsive UX (avoid waiting for debounce)
    (context-navigator-view--render-if-visible)))

(defun context-navigator-view-group-create ()
  "Create a new group (groups mode)."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let ((slug (ignore-errors (context-navigator-group-create))))
        ;; After successful creation, switch to items view immediately.
        (when (and slug (stringp slug))
          (setq context-navigator-view--mode 'items)
          (context-navigator-view--schedule-render)))
    (message "%s" (context-navigator-i18n :press-h-open-groups-first))))

(defun context-navigator-view-group-rename ()
  "Rename selected group (groups mode)."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let ((slug (car (or (context-navigator-view--at-group) '(nil)))))
        (ignore-errors (context-navigator-group-rename slug)))
    (message "%s" (context-navigator-i18n :press-h-open-groups-first))))

(defun context-navigator-view-group-duplicate ()
  "Duplicate selected group (groups mode)."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let ((slug (car (or (context-navigator-view--at-group) '(nil)))))
        (ignore-errors (context-navigator-group-duplicate slug)))
    (message "%s" (context-navigator-i18n :press-h-open-groups-first))))

(defun context-navigator-view-group-edit-description ()
  "Edit description for selected group (groups mode) or current group otherwise."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let ((slug (car (or (context-navigator-view--at-group) '(nil)))))
        (ignore-errors (context-navigator-group-edit-description slug)))
    (ignore-errors (context-navigator-group-edit-description))))

;;;###autoload
(defun context-navigator-view-show-groups ()
  "Open sidebar (if needed) and show the groups list for current project/global."
  (interactive)
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    ;; Ensure sidebar is open
    (unless (and buf (get-buffer-window buf t))
      (ignore-errors (context-navigator-view-open))
      (setq buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq context-navigator-view--mode 'groups))
      (ignore-errors (context-navigator-groups-open))
      (when-let ((win (get-buffer-window buf t)))
        (select-window win))
      (context-navigator-view--schedule-render))))

(require 'context-navigator-view-title)

(defun context-navigator-view-stats-toggle ()
  "Toggle visibility of the Stats block and refresh the view."
  (interactive)
  (when (fboundp 'context-navigator-stats-toggle)
    (context-navigator-stats-toggle))
  (context-navigator-view--schedule-render)
  (context-navigator-view--render-if-visible))



(provide 'context-navigator-view)
;;; context-navigator-view.el ends here
