;;; context-navigator-sidebar.el --- Sidebar UI (side window) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Lightweight, event-driven sidebar:
;; - Opens in a left side window with configurable width
;; - Subscribes to :model-refreshed and :context-load-(start|done)
;; - Renders via context-navigator-render, optional icons
;; - Minimal keymap: RET/SPC to visit/preview, d delete, g refresh, q quit
;;
;; Functional by design: no state mutation outside buffer-local vars for UI.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-events)
(require 'context-navigator-render)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-icons)
(require 'context-navigator-persist)
(require 'context-navigator-i18n)

(defcustom context-navigator-auto-open-groups-on-error t
  "When non-nil, automatically switch the sidebar to the groups list if a group fails to load."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-highlight-active-group t
  "When non-nil, highlight the active group in the groups list."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-controls-style 'text
  "Style for sidebar controls (toggles and footer buttons):
- auto  : prefer compact icon-like labels when possible
- icons : force compact icon-like labels
- text  : verbose text labels"
  :type '(choice (const auto) (const icons) (const text))
  :group 'context-navigator)

(defcustom context-navigator-openable-count-ttl 0.3
  "TTL (seconds) for cached openable buffers count in the sidebar footer."
  :type 'number :group 'context-navigator)

(defcustom context-navigator-openable-soft-cap 100
  "Soft cap for counting openable buffers. Counting short-circuits at this value."
  :type 'integer :group 'context-navigator)

(defcustom context-navigator-openable-remote-mode 'lazy
  "How to treat remote/TRAMP paths when counting openable buffers:
- lazy   : do not call file-exists-p; consider a file openable when no live buffer exists
- strict : verify file existence with file-exists-p (may be slow on TRAMP)
- off    : ignore remote files when counting"
  :type '(choice (const lazy) (const strict) (const off))
  :group 'context-navigator)

(defcustom context-navigator-gptel-indicator-poll-interval 1.0
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
(declare-function context-navigator-toggle-item "context-navigator-core" (key &optional enabled))
(declare-function context-navigator-remove-item-by-key "context-navigator-core" (key))
;; group commands (core)
(declare-function context-navigator-groups-open "context-navigator-core" ())
(declare-function context-navigator-group-switch "context-navigator-core" (&optional slug))
(declare-function context-navigator-group-create "context-navigator-core" (&optional display-name))
(declare-function context-navigator-group-rename "context-navigator-core" (&optional old-slug new-display))
(declare-function context-navigator-group-delete "context-navigator-core" (&optional slug))
(declare-function context-navigator-group-duplicate "context-navigator-core" (&optional src-slug new-display))

(defconst context-navigator-sidebar--buffer-name "*context-navigator-sidebar/")

(defvar-local context-navigator-sidebar--subs nil)
(defvar-local context-navigator-sidebar--header "Context")
(defvar-local context-navigator-sidebar--mode 'items) ;; 'items | 'groups
(defvar-local context-navigator-sidebar--groups nil)  ;; cached groups plists
(defvar-local context-navigator-sidebar--last-lines nil)
(defvar context-navigator-sidebar--group-line-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m [mouse-1] #'context-navigator-sidebar-mouse-open-group)
    m)
  "Keymap attached to group lines to support mouse clicks.")
(defvar-local context-navigator-sidebar--load-progress nil) ;; cons (POS . TOTAL) | nil)
(defvar-local context-navigator-sidebar--winselect-fn nil)  ;; function added to window-selection-change-functions
(defvar-local context-navigator-sidebar--gptel-keys nil)    ;; cached stable keys from gptel (for indicators)
(defvar-local context-navigator-sidebar--gptel-keys-hash nil) ;; reserved for future use
(defvar-local context-navigator-sidebar--sorted-items nil)  ;; cached sorted items (list) for current generation
(defvar-local context-navigator-sidebar--sorted-gen nil)    ;; generation number of the cached sorted items
(defvar-local context-navigator-sidebar--openable-count nil)          ;; cached count (int) or nil
(defvar-local context-navigator-sidebar--openable-plus nil)           ;; non-nil when soft-cap reached
(defvar-local context-navigator-sidebar--openable-stamp 0.0)          ;; float-time of last compute
(defvar-local context-navigator-sidebar--openable-timer nil)          ;; pending timer for recompute
(defvar-local context-navigator-sidebar--buflist-fn nil)              ;; function added to buffer-list-update-hook
(defvar-local context-navigator-sidebar--gptel-poll-timer nil)        ;; polling timer for gptel indicators (or nil)
(defvar-local context-navigator-sidebar--last-render-key nil)        ;; cached render key to skip redundant renders

(defcustom context-navigator-sidebar-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames used by the sidebar loading spinner (list of single-frame strings)."
  :type '(repeat string)
  :group 'context-navigator)

(defcustom context-navigator-sidebar-spinner-interval
  0.1
  "Spinner animation interval in seconds for the sidebar loading indicator."
  :type 'number
  :group 'context-navigator)

(defvar-local context-navigator-sidebar--spinner-timer nil)          ;; loading spinner timer
(defvar-local context-navigator-sidebar--spinner-index 0)

(defvar context-navigator-sidebar-window-params
  '((side . left) (slot . -1))
  "Default parameters for the sidebar window.")

(defun context-navigator-sidebar--header (state)
  "Compute compact header title from STATE.

Rules:
- Items mode: [<project>: <group>] when group is active, otherwise [<project>]
- Groups mode: [<project>] only
- Global (no project): use ~ as project name → items: [~: <group>] / groups: [~]

Note: status toggles [→gptel:on/off] [auto-proj:on/off] are rendered in the footer."
  (let* ((root (context-navigator-state-last-project-root state))
         (group (context-navigator-state-current-group-slug state))
         (proj-name (if root
                        (file-name-nondirectory (directory-file-name root))
                      "~")))
    (cond
     ((eq context-navigator-sidebar--mode 'groups)
      (format "[%s]" proj-name))
     (t
      (if group
          (format "[%s: %s]" proj-name group)
        (format "[%s]" proj-name))))))

(defun context-navigator-sidebar--state-items ()
  "Get items from core state."
  (let* ((st (ignore-errors (context-navigator--state-get))))
    (and st (context-navigator-state-items st))))

;; Helpers

(defun context-navigator-sidebar--wrap-segments (segments total-width)
  "Wrap SEGMENTS (list of strings) into lines within TOTAL-WIDTH.
Returns a list of line strings."
  (let ((acc '())
        (cur ""))
    (dolist (seg segments)
      (let* ((sw (string-width seg))
             (cw (string-width cur)))
        (if (<= (+ cw sw) total-width)
            (setq cur (concat cur seg))
          (when (> (length cur) 0)
            (push cur acc))
          (setq cur seg))))
    (when (> (length cur) 0)
      (push cur acc))
    (nreverse acc)))


(defun context-navigator-sidebar--make-toggle-segments ()
  "Build header toggle segments for push→gptel and auto-project with mouse keymaps.
Respects `context-navigator-controls-style' for compact icon/text labels."
  (let* ((push-on (and (boundp 'context-navigator--push-to-gptel)
                       context-navigator--push-to-gptel))
         (auto-on (and (boundp 'context-navigator--auto-project-switch)
                       context-navigator--auto-project-switch))
         (gptel-available (ignore-errors (context-navigator-gptel-available-p)))
         (style (or context-navigator-controls-style 'auto))
         ;; label builders
         (lbl-push
          (pcase style
            ((or 'icons 'auto) " [→]")
            (_ (format " [→gptel: %s]" (if push-on "on" "off")))))
         (lbl-auto
          (pcase style
            ((or 'icons 'auto) " [A]")
            (_ (format " [%s: %s]" (context-navigator-i18n :auto-proj) (if auto-on "on" "off"))))))
    (let* ((seg1 (let* ((s (copy-sequence lbl-push))
                        (m (let ((km (make-sparse-keymap)))
                             (when gptel-available
                               (define-key km [mouse-1] #'context-navigator-sidebar-toggle-push))
                             km))
                        (fg (if (and push-on gptel-available) "green4" "gray"))
                        (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
                   (add-text-properties beg (length s)
                                        (list 'mouse-face 'highlight
                                              'help-echo (if gptel-available
                                                             (context-navigator-i18n :toggle-push)
                                                           "gptel not available")
                                              'keymap m
                                              'context-navigator-toggle 'push
                                              'face (if gptel-available
                                                        (list :foreground fg)
                                                      'shadow))
                                        s)
                   s))
           (seg2 (let* ((s (copy-sequence lbl-auto))
                        (m (let ((km (make-sparse-keymap)))
                             (define-key km [mouse-1] #'context-navigator-sidebar-toggle-auto-project)
                             km))
                        (fg (if auto-on "green4" "gray"))
                        (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
                   (add-text-properties beg (length s)
                                        (list 'mouse-face 'highlight
                                              'help-echo (context-navigator-i18n :toggle-auto)
                                              'keymap m
                                              'context-navigator-toggle 'auto
                                              'face (list :foreground fg))
                                        s)
                   s)))
      (list seg1 seg2))))


(defun context-navigator-sidebar--header-toggle-lines (total-width)
  "Return header toggle lines wrapped to TOTAL-WIDTH."
  (let ((toggles (context-navigator-sidebar--make-toggle-segments)))
    (context-navigator-sidebar--wrap-segments toggles total-width)))

;; Openable count helpers (footer [O]) ---------------------------------------

(defun context-navigator-sidebar--invalidate-openable ()
  "Invalidate cached openable counters and cancel pending timers."
  (setq context-navigator-sidebar--openable-count nil
        context-navigator-sidebar--openable-plus nil
        context-navigator-sidebar--openable-stamp 0.0)
  (when (timerp context-navigator-sidebar--openable-timer)
    (cancel-timer context-navigator-sidebar--openable-timer)
    (setq context-navigator-sidebar--openable-timer nil)))

(defun context-navigator-sidebar--openable--candidate-p (item)
  "Return non-nil if ITEM can be opened in background (file-backed).
Respects `context-navigator-openable-remote-mode' for remote paths and
counts only enabled items."
  (let ((enabled (and (context-navigator-item-enabled item) t)))
    (when enabled
      (pcase (context-navigator-item-type item)
        ('buffer
         (let* ((buf (context-navigator-item-buffer item))
                (p (context-navigator-item-path item)))
           (and (stringp p)
                ;; already open: skip
                (not (and buf (buffer-live-p buf)))
                (let ((remote (file-remote-p p)))
                  (cond
                   ;; ignore remotes
                   ((and remote (eq context-navigator-openable-remote-mode 'off)) nil)
                   ;; lazy: do not hit file-exists-p on remote
                   ((and remote (eq context-navigator-openable-remote-mode 'lazy))
                    (not (get-file-buffer p)))
                   ;; local or strict remote
                   (t (and (file-exists-p p) (not (get-file-buffer p)))))))))
        ('selection
         (let ((p (context-navigator-item-path item)))
           (and (stringp p)
                (let ((remote (file-remote-p p)))
                  (cond
                   ((and remote (eq context-navigator-openable-remote-mode 'off)) nil)
                   ((and remote (eq context-navigator-openable-remote-mode 'lazy))
                    (not (get-file-buffer p)))
                   (t (and (file-exists-p p) (not (get-file-buffer p)))))))))
        ('file
         (let ((p (context-navigator-item-path item)))
           (and (stringp p)
                (let ((remote (file-remote-p p)))
                  (cond
                   ((and remote (eq context-navigator-openable-remote-mode 'off)) nil)
                   ((and remote (eq context-navigator-openable-remote-mode 'lazy))
                    (not (get-file-buffer p)))
                   (t (and (file-exists-p p) (not (get-file-buffer p)))))))))
        (_ nil)))))

(defun context-navigator-sidebar--compute-openable (cap)
  "Return cons (COUNT . PLUS) for openable items, short-circuiting at CAP.
PLUS is non-nil when CAP was reached."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st)))
         (n 0)
         (limit (or cap most-positive-fixnum)))
    (dolist (it (or items '()))
      (when (context-navigator-sidebar--openable--candidate-p it)
        (setq n (1+ n))
        (when (>= n limit)
          (cl-return))))
    (cons n (and (numberp cap) (>= n (or cap 0))))))

(defun context-navigator-sidebar--openable-count-refresh ()
  "Recompute openable count synchronously with soft-cap; update cache and schedule UI refresh if changed."
  (let* ((res (context-navigator-sidebar--compute-openable context-navigator-openable-soft-cap))
         (n (car res))
         (plus (cdr res))
         (changed (not (equal context-navigator-sidebar--openable-count n))))
    (setq context-navigator-sidebar--openable-count n
          context-navigator-sidebar--openable-plus plus
          context-navigator-sidebar--openable-stamp (float-time))
    (when changed
      (context-navigator-sidebar--schedule-render))
    n))

(defun context-navigator-sidebar--openable-count-get ()
  "Return cons (COUNT . PLUS) from cache; schedule a debounced refresh when stale."
  (let* ((now (float-time))
         (ttl (or context-navigator-openable-count-ttl 0.3)))
    (when (or (null context-navigator-sidebar--openable-count)
              (> (- now context-navigator-sidebar--openable-stamp) (max 0 ttl)))
      (context-navigator-events-debounce
       :sidebar-openable 0.18
       #'context-navigator-sidebar--openable-count-refresh))
    (cons (or context-navigator-sidebar--openable-count 0)
          (and context-navigator-sidebar--openable-plus
               (> (or context-navigator-sidebar--openable-count 0) 0)))))

;; Loading spinner helpers ----------------------------------------------------

(defun context-navigator-sidebar--spinner-start ()
  "Start or restart the lightweight loading spinner timer."
  (when (timerp context-navigator-sidebar--spinner-timer)
    (cancel-timer context-navigator-sidebar--spinner-timer))
  (setq context-navigator-sidebar--spinner-index 0)
  (let ((interval (or context-navigator-sidebar-spinner-interval 0.1)))
    (setq context-navigator-sidebar--spinner-timer
          (run-at-time 0 interval
                       (lambda ()
                         (setq context-navigator-sidebar--spinner-index
                               (1+ (or context-navigator-sidebar--spinner-index 0)))
                         (context-navigator-sidebar--schedule-render))))))

(defun context-navigator-sidebar--spinner-stop ()
  "Stop the loading spinner timer and reset index."
  (when (timerp context-navigator-sidebar--spinner-timer)
    (cancel-timer context-navigator-sidebar--spinner-timer))
  (setq context-navigator-sidebar--spinner-timer nil)
  (setq context-navigator-sidebar--spinner-index 0))


(defun context-navigator-sidebar--footer-control-segments ()
  "Build footer control segments: toggles + [Push now], [Open buffers] and [Clear gptel].
Respects `context-navigator-controls-style' for compact icon/text labels."
  (let* ((push-on (and (boundp 'context-navigator--push-to-gptel)
                       context-navigator--push-to-gptel))
         ;; use cached keys from gptel (do not pull during render)
         (has-gptel (and (listp context-navigator-sidebar--gptel-keys)
                         (> (length context-navigator-sidebar--gptel-keys) 0)))
         (style (or context-navigator-controls-style 'auto))
         (segs '()))
    ;; Toggles first (push → gptel, auto-project)
    (setq segs (append segs (context-navigator-sidebar--make-toggle-segments)))
    ;; [Open buffers] — open file-backed buffers/selections/files in background (lazy/strict remote)
    (cl-destructuring-bind (openable . plus)
        (context-navigator-sidebar--openable-count-get)
      (let* ((label (pcase style
                      ((or 'icons 'auto) " [O]")
                      (_ (format " [%s]" (context-navigator-i18n :open-buffers)))))
             (s (copy-sequence label))
             (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
        ;; Mark as an action so RET handler can attempt it.
        (add-text-properties beg (length s)
                             (list 'context-navigator-action 'open-buffers)
                             s)
        (if (> openable 0)
            (let* ((disp (if plus (format "Open %d+ context buffer(s) in background (o)" openable)
                           (format "Open %d context buffer(s) in background (o)" openable)))
                   (m (let ((km (make-sparse-keymap)))
                        (define-key km [mouse-1] #'context-navigator-sidebar-open-all-buffers)
                        km)))
              (add-text-properties beg (length s)
                                   (list 'mouse-face 'highlight
                                         'help-echo disp
                                         'keymap m)
                                   s))
          (add-text-properties beg (length s)
                               (list 'face 'shadow
                                     'help-echo "No buffers to open")
                               s))
        (push s segs)))
    ;; [Push now] — always shown; when auto-push is ON it is inactive (visually and without keymap).
    (let* ((label (pcase style
                    ((or 'icons 'auto) " [⇪]")
                    (_ (concat " [" (context-navigator-i18n :push-now) "]"))))
           (s (copy-sequence label))
           (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
      ;; Always mark footer segments with an explicit action property so RET handler can invoke them.
      (add-text-properties beg (length s) (list 'context-navigator-action 'push-now) s)
      (if push-on
          ;; Inactive representation: shadowed, no keymap, hint explaining why it's disabled.
          (add-text-properties beg (length s)
                               (list 'face 'shadow
                                     'help-echo (context-navigator-i18n :push-tip))
                               s)
        (let ((m (let ((km (make-sparse-keymap)))
                   (define-key km [mouse-1] #'context-navigator-sidebar-push-now)
                   km)))
          (add-text-properties beg (length s)
                               (list 'mouse-face 'highlight
                                     'help-echo (context-navigator-i18n :push-tip)
                                     'keymap m)
                               s)))
      (push s segs))
    ;; [Clear gptel] — always shown; inactive (shadowed) when no entries available.
    (let* ((label (pcase style
                    ((or 'icons 'auto) " [✖]")
                    (_ (concat " [" (context-navigator-i18n :clear-gptel) "]"))))
           (s (copy-sequence label))
           (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
      ;; Always expose an action property so RET can activate/attempt the command.
      (add-text-properties beg (length s) (list 'context-navigator-action 'clear-gptel) s)
      (if has-gptel
          (let ((m (let ((km (make-sparse-keymap)))
                     (define-key km [mouse-1] #'context-navigator-sidebar-clear-gptel)
                     km)))
            (add-text-properties beg (length s)
                                 (list 'mouse-face 'highlight
                                       'help-echo (context-navigator-i18n :clear-tip)
                                       'keymap m)
                                 s))
        (add-text-properties beg (length s)
                             (list 'face 'shadow
                                   'help-echo (context-navigator-i18n :clear-tip))
                             s))
      (push s segs))
    (nreverse segs)))


(defun context-navigator-sidebar--footer-control-lines (total-width)
  "Return footer control lines wrapped to TOTAL-WIDTH."
  (let ((controls (context-navigator-sidebar--footer-control-segments)))
    (context-navigator-sidebar--wrap-segments controls total-width)))


(defun context-navigator-sidebar--groups-header-lines (header total-width)
  "Return list of header lines for groups view using HEADER and TOTAL-WIDTH."
  (let* ((title (or header (context-navigator-i18n :groups)))
         (hl (propertize (format " %s" title) 'face 'mode-line-emphasis))
         ;; Use full-width textual separator across the sidebar (box-drawing).
         (sep (propertize (make-string (max 1 total-width) ?─) 'face 'shadow)))
    (list hl sep)))


(defun context-navigator-sidebar--groups-body-lines (state)
  "Return list of lines for groups body using STATE."
  (let* ((active (and (context-navigator-state-p state)
                      (context-navigator-state-current-group-slug state))))
    (cond
     ((not (listp context-navigator-sidebar--groups))
      (list (propertize (context-navigator-i18n :no-groups) 'face 'shadow)))
     (t
      (let (lines)
        (dolist (pl context-navigator-sidebar--groups)
          (let* ((slug (or (plist-get pl :slug) ""))
                 (disp (or (plist-get pl :display) slug))
                 (ic (or (ignore-errors (context-navigator-icons-for-group)) "📁"))
                 (txt (concat ic " " disp))
                 (s (copy-sequence txt)))
            (add-text-properties 0 (length s)
                                 (list 'context-navigator-group-slug slug
                                       'context-navigator-group-display disp
                                       'mouse-face 'highlight
                                       'keymap context-navigator-sidebar--group-line-keymap
                                       'help-echo (context-navigator-i18n :mouse-open-group))
                                 s)
            (when (and context-navigator-highlight-active-group
                       active (string= active slug))
              (add-text-properties 0 (length s) (list 'face 'mode-line-emphasis) s))
            (setq lines (append lines (list s)))))
        lines)))))


(defun context-navigator-sidebar--groups-footer-lines (total-width)
  "Return footer lines for groups view wrapped to TOTAL-WIDTH."
  (let* ((ctrl-lines (context-navigator-sidebar--footer-control-lines total-width))
         (help-segments
          (list (context-navigator-i18n :groups-help-open)
                (context-navigator-i18n :groups-help-add)
                (context-navigator-i18n :groups-help-rename)
                (context-navigator-i18n :groups-help-delete)
                (context-navigator-i18n :groups-help-copy)
                (context-navigator-i18n :groups-help-refresh)
                (context-navigator-i18n :groups-help-back)
                (context-navigator-i18n :groups-help-quit)
                (context-navigator-i18n :groups-help-help)))
         (help-lines
          (mapcar (lambda (s) (propertize s 'face 'shadow))
                  (context-navigator-sidebar--wrap-segments help-segments total-width))))
    ;; Insert a blank separator line between body and controls, then controls,
    ;; then an empty line before help lines to satisfy the requested layout.
    (append (list "") ctrl-lines (cons "" help-lines))))


(defun context-navigator-sidebar--render-groups (state header total-width)
  "Render groups view using STATE, HEADER and TOTAL-WIDTH.
Returns the list of lines that were rendered."
  (let* ((lines (append
                 (context-navigator-sidebar--groups-header-lines header total-width)
                 (context-navigator-sidebar--groups-body-lines state)
                 (context-navigator-sidebar--groups-footer-lines total-width))))
    (setq context-navigator-sidebar--last-lines lines
          context-navigator-sidebar--header header)
    (context-navigator-render-apply-to-buffer (current-buffer) lines)
    ;; Focus current group line (cursor on group name) when available.
    (let* ((active (and (context-navigator-state-p state)
                        (context-navigator-state-current-group-slug state)))
           (pos nil))
      (when (and (stringp active) (not (string-empty-p active)))
        (let ((p (point-min)) (found nil))
          (while (and (not found)
                      (setq p (text-property-not-all p (point-max) 'context-navigator-group-slug nil)))
            (when (equal (get-text-property p 'context-navigator-group-slug) active)
              (setq found p))
            (setq p (1+ p)))
          (setq pos found)))
      (unless pos
        (setq pos (text-property-not-all (point-min) (point-max)
                                         'context-navigator-group-slug nil)))
      (when pos
        (goto-char pos)
        (beginning-of-line)))
    lines))


(defun context-navigator-sidebar--items-header-toggle-lines (total-width)
  "Return header toggle lines for items view wrapped to TOTAL-WIDTH."
  (context-navigator-sidebar--header-toggle-lines total-width))


(defun context-navigator-sidebar--items-base-lines (state header total-width)
  "Return a list: (hl sep up rest...) for items view base lines.
The result already includes header line (HL), separator (SEP), the \"..\" line (UP),
and the item lines (REST...)."
  (let* ((items (context-navigator-state-items state))
         ;; generation-aware cached sort: avoid re-sorting identical model generation
         (gen (or (and (context-navigator-state-p state)
                       (context-navigator-state-generation state))
                  0))
         (sorted-items
          (if (and (listp context-navigator-sidebar--sorted-items)
                   (integerp context-navigator-sidebar--sorted-gen)
                   (= gen context-navigator-sidebar--sorted-gen))
              context-navigator-sidebar--sorted-items
            (let ((s (sort (copy-sequence (or items '()))
                           (lambda (a b)
                             (let ((na (downcase (or (context-navigator-item-name a) "")))
                                   (nb (downcase (or (context-navigator-item-name b) ""))))
                               (string-lessp na nb))))))
              (setq context-navigator-sidebar--sorted-items s
                    context-navigator-sidebar--sorted-gen gen)
              s)))
         (left-width (max 16 (min (- total-width 10) (floor (* 0.55 total-width)))))
         (base (let ((context-navigator-render--gptel-keys context-navigator-sidebar--gptel-keys))
                 (context-navigator-render-build-lines sorted-items header
                                                       #'context-navigator-icons-for-item
                                                       left-width)))
         (hl (car base))
         ;; Replace short separator generated by render with a full-width textual separator (box-drawing), unobtrusive.
         (sep (propertize (make-string (max 1 total-width) ?─) 'face 'shadow))
         (rest (cddr base))
         (up (let ((s (copy-sequence "..")))
               (add-text-properties 0 (length s)
                                    (list 'context-navigator-groups-up t
                                          'face 'shadow)
                                    s)
               s)))
    (list hl sep up rest)))


(defun context-navigator-sidebar--items-footer-lines (total-width)
  "Return footer lines for items view wrapped to TOTAL-WIDTH."
  (let* ((ctrl-lines (context-navigator-sidebar--footer-control-lines total-width))
         (help-segments (list (context-navigator-i18n :items-help-view-groups)
                              (context-navigator-i18n :items-help-help)))
         (help-lines
          (mapcar (lambda (s) (propertize s 'face 'shadow))
                  (context-navigator-sidebar--wrap-segments help-segments total-width))))
    ;; Blank line before controls to visually separate items list from controls.
    (append (list "") ctrl-lines (cons "" help-lines))))


(defun context-navigator-sidebar--render-items (state header total-width)
  "Render items view using STATE, HEADER and TOTAL-WIDTH.
Returns the list of lines that were rendered."
  (cl-destructuring-bind (hl sep up rest)
      (context-navigator-sidebar--items-base-lines state header total-width)
    (let* ((lines (append (list hl sep up) rest
                          (context-navigator-sidebar--items-footer-lines total-width))))
      (setq context-navigator-sidebar--last-lines lines
            context-navigator-sidebar--header header)
      (context-navigator-render-apply-to-buffer (current-buffer) lines)
      lines)))


;; Entry point

(defun context-navigator-sidebar--render-loading (state header total-width)
  "Render a lightweight loading/preloader view into the sidebar buffer (spinner only).

The spinner is centered horizontally within TOTAL-WIDTH. An optional percentage
is shown to the right of the spinner when load progress is available."
  (let* ((hl (propertize (format " %s" header) 'face 'mode-line-emphasis))
         (sep (propertize (make-string (max 1 total-width) ?─) 'face 'shadow))
         (frames (or context-navigator-sidebar-spinner-frames
                     '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")))
         (len (length frames))
         (idx (or context-navigator-sidebar--spinner-index 0))
         (ch (if (> len 0) (nth (mod idx (max 1 len)) frames) ""))
         (pct (when (and context-navigator-sidebar--load-progress
                         (numberp (car context-navigator-sidebar--load-progress))
                         (numberp (cdr context-navigator-sidebar--load-progress))
                         (> (cdr context-navigator-sidebar--load-progress) 0))
                (floor (* 100.0 (/ (float (car context-navigator-sidebar--load-progress))
                                   (max 1 (cdr context-navigator-sidebar--load-progress)))))))
         (spin (concat ch (when pct (format " %d%%" pct))))
         ;; center spin line within total-width (respecting visual width)
         (spin-w (max 0 (string-width spin)))
         (left-pad (max 0 (floor (/ (max 0 (- total-width spin-w)) 2))))
         (spin-line (concat (make-string left-pad ? ) spin))
         (sline (propertize (concat " " spin-line) 'face 'shadow))
         (lines (list hl sep "" sline "")))
    (setq context-navigator-sidebar--last-lines lines
          context-navigator-sidebar--header header)
    (context-navigator-render-apply-to-buffer (current-buffer) lines)
    lines))

(defun context-navigator-sidebar--render ()
  "Render current view (items or groups) into the sidebar buffer.

Uses a composite render key to skip full rendering when nothing relevant changed.
Key components:
 - model generation
 - current view mode (items/groups)
 - sidebar width (total)
 - gptel keys hash
 - cached openable count / soft-plus marker
 - header string (display)

When the key equals `context-navigator-sidebar--last-render-key' the function
returns without rebuilding buffer contents.

Optimization: if the core state indicates loading in progress we render a
very small, cheap preloader view immediately (no icons, no sorting, no file
checks) so project switching feels responsive while the data loads in the
background."
  (let* ((state (context-navigator--state-get))
         (header (context-navigator-sidebar--header state))
         (win (get-buffer-window (current-buffer) 'visible))
         (total (or (and win (window-body-width win))
                    (and (boundp 'context-navigator-sidebar-width)
                         (symbol-value 'context-navigator-sidebar-width))
                    33))
         ;; Components for early-exit render key
         (gen (or (and (context-navigator-state-p state)
                       (context-navigator-state-generation state))
                  0))
         (mode context-navigator-sidebar--mode)
         ;; Use sxhash-equal to produce a stable-ish fingerprint of gptel keys list
         (gptel-hash (sxhash-equal context-navigator-sidebar--gptel-keys))
         ;; Use cached openable count (may be nil) — normalize to integer and plus marker.
         (openable (or context-navigator-sidebar--openable-count 0))
         (plus (and context-navigator-sidebar--openable-plus t))
         ;; Compose key
         (key (list gen mode total gptel-hash openable plus header)))
    (unless (equal key context-navigator-sidebar--last-render-key)
      (setq context-navigator-sidebar--last-render-key key)
      ;; Fast path: show minimal preloader when loading or when progress is reported by events.
      (when (or (and (context-navigator-state-p state)
                     (context-navigator-state-loading-p state))
                context-navigator-sidebar--load-progress)
        (context-navigator-sidebar--render-loading state header total)
        (cl-return-from context-navigator-sidebar--render))
      (cond
       ((eq context-navigator-sidebar--mode 'groups)
        (context-navigator-sidebar--render-groups state header total))
       (t
        (context-navigator-sidebar--render-items state header total))))))
(defun context-navigator-sidebar--render-if-visible ()
  "Render sidebar if its buffer is visible."
  (when-let* ((buf (get-buffer context-navigator-sidebar--buffer-name))
              (win (get-buffer-window buf t)))
    (with-selected-window win
      (with-current-buffer buf
        (context-navigator-sidebar--render)))))

(defun context-navigator-sidebar--schedule-render ()
  "Debounced request to render the sidebar if visible. Reset render cache to force update."
  ;; Ensure next render is not short-circuited by the render hash cache.
  (let ((buf (get-buffer context-navigator-sidebar--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local context-navigator-render--last-hash nil)
        (setq-local context-navigator-sidebar--last-render-key nil))))
  (context-navigator-events-debounce
   :sidebar-render 0.06
   #'context-navigator-sidebar--render-if-visible))

(defun context-navigator-sidebar--at-item ()
  "Return item at point (from text properties) or nil."
  (let ((it (get-text-property (point) 'context-navigator-item)))
    it))

(defun context-navigator-sidebar--visit (preview)
  "Open item at point. If PREVIEW non-nil, show in other window.

When the sidebar buffer/window is currently selected, prefer opening targets
in another window (never replace the sidebar buffer). This avoids the
situation where visiting a file replaces the sidebar buffer and makes it
hard to restore the sidebar afterward."
  (when-let* ((item (context-navigator-sidebar--at-item)))
    (let* ((selected-win (selected-window))
           (is-sidebar (and (window-live-p selected-win)
                            (window-parameter selected-win 'context-navigator-sidebar)
                            (eq (window-buffer selected-win) (current-buffer))))
           (open-in-other (or preview is-sidebar)))
      (pcase (context-navigator-item-type item)
        ('file
         (let ((f (context-navigator-item-path item)))
           (when (and (stringp f) (file-exists-p f))
             (if open-in-other
                 (find-file-other-window f)
               (find-file f)))))
        ('buffer
         (let ((buf (or (context-navigator-item-buffer item)
                        (and (context-navigator-item-path item)
                             (find-file-noselect (context-navigator-item-path item))))))
           (when (buffer-live-p buf)
             (let ((buf-win (get-buffer-window buf 0)))
               (if (and buf-win (not (window-parameter buf-win 'context-navigator-sidebar)))
                   ;; Buffer is visible in a normal window: jump there.
                   (select-window buf-win)
                 ;; Otherwise open in other window if requested, else in current window.
                 (if open-in-other
                     (switch-to-buffer-other-window buf)
                   (switch-to-buffer buf)))))))
        ('selection
         (let* ((f   (context-navigator-item-path item))
                (buf (or (context-navigator-item-buffer item)
                         (and (stringp f) (file-exists-p f)
                              (find-file-noselect f))))
                (b   (context-navigator-item-beg item))
                (e   (context-navigator-item-end item))
                (valid-pos (and (integerp b) (integerp e))))
           (cond
            ;; If a live buffer exists: prefer selecting its non-sidebar window.
            ((and (bufferp buf) (buffer-live-p buf))
             (let ((buf-win (get-buffer-window buf 0)))
               (if (and buf-win (not (window-parameter buf-win 'context-navigator-sidebar)))
                   (select-window buf-win)
                 (if open-in-other
                     (switch-to-buffer-other-window buf)
                   (switch-to-buffer buf))))
             (when valid-pos
               (goto-char (min b e))
               (push-mark (max b e) t t)))
            ;; Otherwise open the file: prefer existing non-sidebar window if any.
            ((and (stringp f) (file-exists-p f))
             (let* ((existing-buf (get-file-buffer f))
                    (buf-win (and existing-buf (get-buffer-window existing-buf 0))))
               (if (and buf-win (not (window-parameter buf-win 'context-navigator-sidebar)))
                   (select-window buf-win)
                 (if open-in-other
                     (find-file-other-window f)
                   (find-file f))))
             (when valid-pos
               (goto-char (min b e))
               (push-mark (max b e) t t)))
            (t
             (message "Cannot locate selection target")))))
        (_ (message "Unknown item"))))))

(defun context-navigator-sidebar-visit ()
  "Visit item at point."
  (interactive)
  (context-navigator-sidebar--visit nil))

(defun context-navigator-sidebar-preview ()
  "Preview item at point in other window."
  (interactive)
  (context-navigator-sidebar--visit t))

(defun context-navigator-sidebar-next-item ()
  "Move point to the next item line or \"..\" or group line."
  (interactive)
  (let* ((start (min (1+ (line-end-position)) (point-max)))
         (pos (or (context-navigator-sidebar--find-next-itemish-pos start)
                  (text-property-not-all start (point-max) 'context-navigator-group-slug nil))))
    (unless pos
      ;; wrap to the first item-ish or group element
      (setq pos (or (context-navigator-sidebar--find-next-itemish-pos (point-min))
                    (text-property-not-all (point-min) (point-max) 'context-navigator-group-slug nil))))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

(defun context-navigator-sidebar-previous-item ()
  "Move point to the previous item line or \"..\" or group line."
  (interactive)
  (let* ((start (line-beginning-position))
         (pos (or (context-navigator-sidebar--find-prev-itemish-pos start)
                  (let ((p nil) (best nil))
                    (setq p (text-property-not-all (point-min) start 'context-navigator-group-slug nil))
                    (while (and p (< p start))
                      (setq best p)
                      (setq p (text-property-not-all (1+ p) start 'context-navigator-group-slug nil)))
                    best))))
    (unless pos
      ;; wrap to the last item-ish or group element
      (setq pos (or (context-navigator-sidebar--find-prev-itemish-pos (point-max))
                    (let ((p nil) (best nil))
                      (setq p (text-property-not-all (point-min) (point-max) 'context-navigator-group-slug nil))
                      (while p
                        (setq best p)
                        (setq p (text-property-not-all (1+ p) (point-max) 'context-navigator-group-slug nil)))
                      best))))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

;; TAB navigation helpers ----------------------------------------------------

(defun context-navigator-sidebar--find-next-interactive-pos (&optional start)
  "Return the nearest position >= START (or point) with an interactive property.
Searches for known interactive properties used in the sidebar."
  (let* ((start (or start (point)))
         (props '(context-navigator-item
                  context-navigator-group-slug
                  context-navigator-action
                  context-navigator-toggle
                  context-navigator-groups-up))
         (best nil))
    (dolist (p props)
      (let ((pos (text-property-not-all start (point-max) p nil)))
        (when (and pos (or (null best) (< pos best)))
          (setq best pos))))
    best))

(defun context-navigator-sidebar--find-prev-interactive-pos (&optional start)
  "Return the nearest position < START (or point) with an interactive property."
  (let* ((start (or start (point)))
         (pos nil)
         (best nil))
    (setq pos (context-navigator-sidebar--find-next-interactive-pos (point-min)))
    (while (and pos (< pos start))
      (setq best pos)
      (setq pos (context-navigator-sidebar--find-next-interactive-pos (1+ pos))))
    best))

(defun context-navigator-sidebar--find-next-itemish-pos (&optional start)
  "Return nearest position >= START with either an item or the \"..\" up marker."
  (let* ((start (or start (point)))
         (p1 (text-property-not-all start (point-max) 'context-navigator-item nil))
         (p2 (text-property-not-all start (point-max) 'context-navigator-groups-up nil)))
    (cond
     ((and p1 p2) (min p1 p2))
     (p1 p1)
     (p2 p2)
     (t nil))))

(defun context-navigator-sidebar--find-prev-itemish-pos (&optional start)
  "Return nearest position < START with either an item or the \"..\" up marker."
  (let* ((start (or start (point)))
         (pos nil)
         (best nil))
    (setq pos (context-navigator-sidebar--find-next-itemish-pos (point-min)))
    (while (and pos (< pos start))
      (setq best pos)
      (setq pos (context-navigator-sidebar--find-next-itemish-pos (1+ pos))))
    best))

(defun context-navigator-sidebar-tab-next ()
  "Move point to the next interactive element (items, groups, toggles, actions).

Wraps to the top when no further element is found after point."
  (interactive)
  (let* ((here (point))
         (props '(context-navigator-item
                  context-navigator-group-slug
                  context-navigator-action
                  context-navigator-toggle
                  context-navigator-groups-up))
         ;; If we are inside an interactive segment, skip to its end first,
         ;; so repeated TAB moves to the next segment (not within the same one).
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
         (pos (context-navigator-sidebar--find-next-interactive-pos cur-end)))
    (unless pos
      ;; wrap
      (setq pos (context-navigator-sidebar--find-next-interactive-pos (point-min))))
    (if pos
        (goto-char pos)
      (message "No interactive elements"))))

(defun context-navigator-sidebar-tab-previous ()
  "Move point to the previous interactive element (items, groups, toggles, actions).

Wraps to the bottom when no previous element is found before point."
  (interactive)
  (let* ((here (point))
         (props '(context-navigator-item
                  context-navigator-group-slug
                  context-navigator-action
                  context-navigator-toggle
                  context-navigator-groups-up))
         ;; If we are inside an interactive segment, start from the segment's begin,
         ;; so Shift-TAB moves to the previous segment (not to the start of the same one).
         (cur-beg
          (if (cl-some (lambda (p) (get-text-property here p)) props)
              (cl-reduce #'min
                         (mapcar (lambda (p)
                                   (if (get-text-property here p)
                                       (or (previous-single-property-change here p nil (point-min))
                                           (point-min))
                                     here))
                                 props)
                         :initial-value here)
            here))
         (pos (context-navigator-sidebar--find-prev-interactive-pos cur-beg)))
    (unless pos
      ;; wrap to last interactive element
      (setq pos (context-navigator-sidebar--find-prev-interactive-pos (point-max))))
    (if pos
        (goto-char pos)
      (message "No interactive elements"))))

(defun context-navigator-sidebar--remove-at-point ()
  "Remove the item at point from gptel context."
  (interactive)
  (when-let* ((item (context-navigator-sidebar--at-item)))
    (let* ((cur (context-navigator-sidebar--state-items))
           (key (context-navigator-model-item-key item))
           (keep (cl-remove-if (lambda (it)
                                 (string= (context-navigator-model-item-key it) key))
                               cur)))
      (ignore-errors (context-navigator-gptel-apply keep))
      ;; model обновится через :gptel-change → core sync
      (message "Removed from context: %s" (or (context-navigator-item-name item) key)))))
(make-obsolete 'context-navigator-sidebar--remove-at-point
               'context-navigator-sidebar-delete-from-model
               "0.2.1")

(defun context-navigator-sidebar-toggle-enabled ()
  "Toggle enabled/disabled for the item at point, apply to gptel.

Deprecated for 't' binding: use `context-navigator-sidebar-toggle-gptel' to toggle gptel membership."
  (interactive)
  (when-let* ((item (context-navigator-sidebar--at-item)))
    (let* ((key (context-navigator-model-item-key item)))
      (message "Deprecated: toggling model only for %s" (or (context-navigator-item-name item) key))
      (let* ((st (ignore-errors (context-navigator-toggle-item key))))
        (ignore-errors st))
      (context-navigator-sidebar--schedule-render)
      (context-navigator-sidebar--render-if-visible)
      (context-navigator-sidebar-next-item))))

(defun context-navigator-sidebar-toggle-gptel ()
  "Toggle gptel membership for the item at point (no mass apply).

Order of operations:
- change gptel context first (add/remove item)
- then refresh lamps/indicators in the sidebar."
  (interactive)
  (when-let* ((item (context-navigator-sidebar--at-item)))
    (let* ((res (ignore-errors (context-navigator-gptel-toggle-one item)))
           (key (context-navigator-model-item-key item)))
      ;; Force next render not to short-circuit
      (let ((buf (get-buffer context-navigator-sidebar--buffer-name)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq-local context-navigator-render--last-hash nil))))
      ;; Refresh indicators; :gptel-change will also trigger a refresh
      (context-navigator-sidebar--schedule-render)
      (context-navigator-sidebar--render-if-visible)
      (context-navigator-sidebar-next-item)
      (pcase res
        (:added   (message "Added to gptel: %s" (or (context-navigator-item-name item) key)))
        (:removed (message "Removed from gptel: %s" (or (context-navigator-item-name item) key)))
        (_        (message "No change for: %s" (or (context-navigator-item-name item) key)))))))

(defun context-navigator-sidebar-delete-from-model ()
  "Delete the item at point from the model permanently and apply to gptel."
  (interactive)
  (when-let* ((item (context-navigator-sidebar--at-item)))
    (let* ((key (context-navigator-model-item-key item))
           (st (ignore-errors (context-navigator-remove-item-by-key key)))
           (items (and (context-navigator-state-p st) (context-navigator-state-items st))))
      (when (and items (boundp 'context-navigator--push-to-gptel) context-navigator--push-to-gptel)
        (ignore-errors (context-navigator-gptel-apply items)))
      (message "Deleted from model: %s" (or (context-navigator-item-name item) key)))))

(defun context-navigator-sidebar-refresh ()
  "Force re-render of the sidebar, if visible."
  (interactive)
  (context-navigator-sidebar--render-if-visible))

(defun context-navigator-sidebar-quit ()
  "Close the sidebar window and kill its buffer, removing subscriptions."
  (interactive)
  (when-let* ((buf (get-buffer context-navigator-sidebar--buffer-name)))
    (with-current-buffer buf
      (context-navigator-sidebar--remove-subs))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (window-live-p win)
        (delete-window win)))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun context-navigator-sidebar--subscribe-model-events ()
  "Subscribe to generic model refresh events."
  (push (context-navigator-events-subscribe
         :model-refreshed
         (lambda (&rest _)
           (context-navigator-sidebar--invalidate-openable)
           (let ((st (ignore-errors (context-navigator--state-get))))
             (if (and (context-navigator-state-p st)
                      (context-navigator-state-loading-p st))
                 (context-navigator-sidebar--render-if-visible)
               (context-navigator-sidebar--schedule-render)))))
        context-navigator-sidebar--subs))

(defun context-navigator-sidebar--subscribe-load-events ()
  "Subscribe to context loading lifecycle events."
  (push (context-navigator-events-subscribe
         :context-load-start
         (lambda (&rest _)
           (setq context-navigator-sidebar--load-progress (cons 0 0))
           (context-navigator-sidebar--invalidate-openable)
           (context-navigator-sidebar--spinner-start)
           (context-navigator-sidebar--render-if-visible)))
        context-navigator-sidebar--subs)
  (push (context-navigator-events-subscribe
         :context-load-step
         (lambda (_root pos total)
           (setq context-navigator-sidebar--load-progress (cons pos total))
           (context-navigator-sidebar--spinner-start)
           (context-navigator-sidebar--schedule-render)))
        context-navigator-sidebar--subs)
  (push (context-navigator-events-subscribe
         :context-load-done
         (lambda (root ok-p)
           (setq context-navigator-sidebar--load-progress nil)
           (context-navigator-sidebar--invalidate-openable)
           (context-navigator-sidebar--spinner-stop)
           (unless ok-p
             (let* ((st (ignore-errors (context-navigator--state-get)))
                    (slug (and st (context-navigator-state-current-group-slug st)))
                    (file (and slug (ignore-errors (context-navigator-persist-context-file root slug)))))
               (when (and (stringp file) (file-exists-p file))
                 (message "Context Navigator: group '%s' file looks unreadable. Press h to open groups, then d to delete." (or slug "<unknown>")))
               (when context-navigator-auto-open-groups-on-error
                 (setq context-navigator-sidebar--mode 'groups)
                 (ignore-errors (context-navigator-groups-open)))))
           (context-navigator-sidebar--schedule-render)))
        context-navigator-sidebar--subs))

(defun context-navigator-sidebar--subscribe-groups-events ()
  "Subscribe to groups list updates."
  (push (context-navigator-events-subscribe
         :groups-list-updated
         (lambda (_root groups)
           (setq context-navigator-sidebar--groups groups)
           (when (eq context-navigator-sidebar--mode 'groups)
             (context-navigator-sidebar--schedule-render))))
        context-navigator-sidebar--subs))

(defun context-navigator-sidebar--subscribe-gptel-events ()
  "Subscribe to gptel changes and keep cached keys in sync."
  (push (context-navigator-events-subscribe
         :gptel-change
         (lambda (&rest _)
           (let* ((lst (ignore-errors (context-navigator-gptel-pull)))
                  (keys (and (listp lst)
                             (mapcar #'context-navigator-model-item-key lst))))
             (setq context-navigator-sidebar--gptel-keys keys
                   context-navigator-sidebar--gptel-keys-hash (sxhash-equal keys)))
           (context-navigator-sidebar--schedule-render)))
        context-navigator-sidebar--subs))

(defun context-navigator-sidebar--init-gptel-cache ()
  "Initialize cached gptel keys once."
  (let* ((lst (ignore-errors (context-navigator-gptel-pull)))
         (keys (and (listp lst)
                    (mapcar #'context-navigator-model-item-key lst))))
    (setq context-navigator-sidebar--gptel-keys keys
          context-navigator-sidebar--gptel-keys-hash (sxhash-equal keys))))

(defun context-navigator-sidebar--install-buffer-list-hook ()
  "Install buffer-list-update hook to recompute quick counters."
  (setq context-navigator-sidebar--buflist-fn
        (lambda ()
          (let ((buf (get-buffer context-navigator-sidebar--buffer-name)))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (context-navigator-sidebar--invalidate-openable)
                (context-navigator-sidebar--schedule-render))))))
  (add-hook 'buffer-list-update-hook context-navigator-sidebar--buflist-fn))

(defun context-navigator-sidebar--install-window-select-hook ()
  "Install window-selection-change hook to re-render on focus."
  (setq context-navigator-sidebar--winselect-fn
        (lambda (_frame)
          (let ((win (selected-window)))
            (when (and (window-live-p win)
                       (eq (window-buffer win) (get-buffer context-navigator-sidebar--buffer-name)))
              (with-selected-window win
                (context-navigator-sidebar--schedule-render))))))
  (add-hook 'window-selection-change-functions context-navigator-sidebar--winselect-fn))

(defun context-navigator-sidebar--initial-compute-counters ()
  "Compute quick counters once on install."
  (context-navigator-sidebar--openable-count-refresh))

(defun context-navigator-sidebar--start-gptel-poll-timer ()
  "Start lightweight polling for gptel indicators while sidebar is visible."
  (let ((int (or context-navigator-gptel-indicator-poll-interval 0)))
    (when (> int 0)
      (setq context-navigator-sidebar--gptel-poll-timer
            (run-at-time 0 int
                         (lambda ()
                           (let ((buf (get-buffer context-navigator-sidebar--buffer-name)))
                             (when (buffer-live-p buf)
                               (with-current-buffer buf
                                 (when (get-buffer-window buf t)
                                   (let* ((lst (ignore-errors (context-navigator-gptel-pull)))
                                          (keys (and (listp lst)
                                                     (mapcar #'context-navigator-model-item-key lst)))
                                          (h (sxhash-equal keys)))
                                     (unless (equal h context-navigator-sidebar--gptel-keys-hash)
                                       (setq context-navigator-sidebar--gptel-keys keys
                                             context-navigator-sidebar--gptel-keys-hash h)
                                       (context-navigator-sidebar--schedule-render)))))))))))))

(defun context-navigator-sidebar--install-subs ()
  "Subscribe to relevant events (buffer-local tokens).
Guard against duplicate subscriptions."
  (unless context-navigator-sidebar--subs
    ;; Model and loading lifecycle
    (context-navigator-sidebar--subscribe-model-events)
    (context-navigator-sidebar--subscribe-load-events)
    (context-navigator-sidebar--subscribe-groups-events)
    ;; gptel events + advices + initial cache
    (context-navigator-sidebar--subscribe-gptel-events)
    (ignore-errors (context-navigator-gptel-on-change-register))
    (context-navigator-sidebar--init-gptel-cache)
    ;; Hooks and initial compute
    (context-navigator-sidebar--install-buffer-list-hook)
    (context-navigator-sidebar--install-window-select-hook)
    (context-navigator-sidebar--initial-compute-counters)
    ;; Optional polling
    (context-navigator-sidebar--start-gptel-poll-timer)
    ;; Гарантированная отписка при убийстве буфера (локально)
    (add-hook 'kill-buffer-hook #'context-navigator-sidebar--remove-subs nil t)))

(defun context-navigator-sidebar--remove-subs ()
  "Unsubscribe buffer-local tokens."
  (when context-navigator-sidebar--subs
    (mapc #'context-navigator-events-unsubscribe context-navigator-sidebar--subs)
    (setq context-navigator-sidebar--subs nil))
  ;; Remove buffer-list hook if installed.
  (when context-navigator-sidebar--buflist-fn
    (remove-hook 'buffer-list-update-hook context-navigator-sidebar--buflist-fn)
    (setq context-navigator-sidebar--buflist-fn nil))
  ;; Cancel timers and drop cached counters.
  (context-navigator-sidebar--invalidate-openable)
  (context-navigator-sidebar--spinner-stop)
  ;; Remove focus render hook if installed.
  (when context-navigator-sidebar--winselect-fn
    (remove-hook 'window-selection-change-functions context-navigator-sidebar--winselect-fn)
    (setq context-navigator-sidebar--winselect-fn nil))
  ;; Cancel gptel poll timer if running.
  (when (timerp context-navigator-sidebar--gptel-poll-timer)
    (cancel-timer context-navigator-sidebar--gptel-poll-timer)
    (setq context-navigator-sidebar--gptel-poll-timer nil))
  ;; Also unregister gptel advices installed for UI updates.
  (ignore-errors (context-navigator-gptel-on-change-unregister)))

(defun context-navigator-sidebar--format-bindings (pairs map)
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

(defun context-navigator-sidebar-help ()
  "Show localized, column-formatted help for Context Navigator without truncation."
  (interactive)
  (with-help-window "*Context Navigator Help*"
    (let* ((map context-navigator-sidebar-mode-map)
           ;; Command → i18n key for description
           (pairs '((context-navigator-sidebar-next-item         . :help-next-item)
                    (context-navigator-sidebar-previous-item     . :help-previous-item)
                    (context-navigator-sidebar-activate          . :help-activate)
                    (context-navigator-sidebar-preview           . :help-preview)
                    (context-navigator-sidebar-toggle-gptel      . :help-toggle-gptel)
                    (context-navigator-sidebar-delete-dispatch   . :help-delete)
                    (context-navigator-sidebar-refresh-dispatch  . :help-refresh)
                    (context-navigator-sidebar-go-up             . :help-go-up)
                    (context-navigator-sidebar-group-create      . :help-group-create)
                    (context-navigator-sidebar-group-rename      . :help-group-rename)
                    (context-navigator-sidebar-group-duplicate   . :help-group-duplicate)
                    (context-navigator-sidebar-toggle-push       . :help-toggle-push)
                    (context-navigator-sidebar-toggle-auto-project . :help-toggle-auto)
                    (context-navigator-sidebar-open-all-buffers  . :help-open-all)
                    (context-navigator-sidebar-push-now          . :help-push-now)
                    (context-navigator-sidebar-clear-gptel       . :help-clear-gptel)
                    (context-navigator-sidebar-quit              . :help-quit)
                    (context-navigator-sidebar-help              . :help-help)))
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

(defvar context-navigator-sidebar-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Dispatch RET depending on mode
    (define-key m (kbd "RET") #'context-navigator-sidebar-activate)
    (define-key m (kbd "SPC") #'context-navigator-sidebar-preview)
    (define-key m (kbd "n")   #'context-navigator-sidebar-next-item)
    (define-key m (kbd "p")   #'context-navigator-sidebar-previous-item)
    ;; Vim-like navigation keys
    (define-key m (kbd "j")   #'context-navigator-sidebar-next-item)
    (define-key m (kbd "k")   #'context-navigator-sidebar-previous-item)
    (define-key m (kbd "l")   #'context-navigator-sidebar-activate)
    (define-key m (kbd "t")   #'context-navigator-sidebar-toggle-gptel)

    ;; TAB navigation between interactive elements
    ;; Bind several TAB event representations to be robust across terminals/minor-modes.
    (define-key m (kbd "TAB")       #'context-navigator-sidebar-tab-next)
    (define-key m (kbd "<tab>")     #'context-navigator-sidebar-tab-next)
    (define-key m [tab]             #'context-navigator-sidebar-tab-next)
    (define-key m (kbd "C-i")       #'context-navigator-sidebar-tab-next)
    (define-key m (kbd "<backtab>") #'context-navigator-sidebar-tab-previous)
    (define-key m [backtab]         #'context-navigator-sidebar-tab-previous)
    (define-key m (kbd "S-<tab>")   #'context-navigator-sidebar-tab-previous)
    ;; Remap global indent command to our TAB-next to ensure override everywhere.
    (define-key m [remap indent-for-tab-command] #'context-navigator-sidebar-tab-next)

    ;; Ensure delete-other-windows behaves sensibly when the sidebar is present:
    ;; close sidebar windows first to avoid making a side window the only window.
    (define-key m [remap delete-other-windows] #'context-navigator-delete-other-windows)

    ;; New global toggles/actions in sidebar
    (define-key m (kbd "x")   #'context-navigator-sidebar-toggle-push)
    (define-key m (kbd "T")   #'context-navigator-sidebar-toggle-auto-project)
    (define-key m (kbd "P")   #'context-navigator-sidebar-push-now)
    (define-key m (kbd "C")   #'context-navigator-sidebar-clear-gptel)
    ;; d and g are dispatched depending on mode
    (define-key m (kbd "d")   #'context-navigator-sidebar-delete-dispatch)
    (define-key m (kbd "g")   #'context-navigator-sidebar-refresh-dispatch)
    ;; Additional action: open all context buffers in background
    (define-key m (kbd "o")   #'context-navigator-sidebar-open-all-buffers)
    ;; Groups-specific keys
    (define-key m (kbd "h")   #'context-navigator-sidebar-go-up)      ;; show groups from items
    (define-key m (kbd "a")   #'context-navigator-sidebar-group-create)
    (define-key m (kbd "r")   #'context-navigator-sidebar-group-rename)
    (define-key m (kbd "c")   #'context-navigator-sidebar-group-duplicate)
    (define-key m (kbd "q")   #'context-navigator-sidebar-quit)
    (define-key m (kbd "?")   #'context-navigator-sidebar-help)
    m)
  "Keymap for =context-navigator-sidebar-mode'.")

;; Ensure bindings are updated after reloads (defvar won't reinitialize an existing keymap).
(when (keymapp context-navigator-sidebar-mode-map)
  (define-key context-navigator-sidebar-mode-map (kbd "t") #'context-navigator-sidebar-toggle-gptel))

(defun context-navigator-sidebar--hl-line-range ()
  "Return region to highlight for the current line.

Highlight:
- item lines (have 'context-navigator-item)
- group lines (have 'context-navigator-group-slug)
- the \"..\" line (has 'context-navigator-groups-up)

Do not highlight header/separator lines."
  (when (or (get-text-property (point) 'context-navigator-item)
            (get-text-property (point) 'context-navigator-group-slug)
            (get-text-property (point) 'context-navigator-groups-up))
    (cons (line-beginning-position)
          (min (point-max) (1+ (line-end-position))))))

(define-derived-mode context-navigator-sidebar-mode special-mode "Context-Nav"
  "Major mode for context-navigator sidebar buffer."
  (buffer-disable-undo)
  (setq truncate-lines t
        cursor-type t
        mode-line-format nil)
  (setq-local hl-line-range-function #'context-navigator-sidebar--hl-line-range)
  (hl-line-mode 1))

;;;###autoload
(defun context-navigator-sidebar-open ()
  "Open the context-navigator sidebar on the left."
  (interactive)
  (let* ((buf (get-buffer-create context-navigator-sidebar--buffer-name))
         (win (display-buffer-in-side-window buf
                                             (append
                                              context-navigator-sidebar-window-params
                                              (list (cons 'window-width
                                                          (or (and (boundp 'context-navigator-sidebar-width)
                                                                   (symbol-value 'context-navigator-sidebar-width))
                                                              33)))))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (context-navigator-sidebar-mode)
        (setq-local buffer-read-only t)
        (context-navigator-sidebar--install-subs)
        (context-navigator-sidebar--render)))
    (when (window-live-p win)
      ;; Mark this window as our sidebar so visit logic can detect and avoid replacing it.
      (set-window-parameter win 'context-navigator-sidebar t)
      (select-window win))
    win))

;;;###autoload
(defun context-navigator-sidebar-close ()
  "Close the context-navigator sidebar if visible."
  (interactive)
  (context-navigator-sidebar-quit))

;;;###autoload
(defun context-navigator-sidebar-toggle ()
  "Toggle the context-navigator sidebar."
  (interactive)
  (if (get-buffer-window context-navigator-sidebar--buffer-name t)
      (context-navigator-sidebar-close)
    (context-navigator-sidebar-open)))

;; Helpers for group mode

(defun context-navigator-sidebar--at-group ()
  "Return cons (SLUG . DISPLAY) for group at point, or nil."
  (let* ((slug (get-text-property (point) 'context-navigator-group-slug))
         (disp (get-text-property (point) 'context-navigator-group-display)))
    (when (and (stringp slug) (not (string-empty-p slug)))
      (cons slug (or disp slug)))))

(defun context-navigator-sidebar--open-group-at-point ()
  "Switch to group at point and return t on success."
  (interactive)
  (when-let* ((cell (context-navigator-sidebar--at-group))
              (slug (car cell)))
    (ignore-errors (context-navigator-group-switch slug))
    (setq context-navigator-sidebar--mode 'items)
    (context-navigator-sidebar--schedule-render)
    t))

(defun context-navigator-sidebar-mouse-open-group (event)
  "Open group at mouse EVENT position."
  (interactive "e")
  (mouse-set-point event)
  (context-navigator-sidebar--open-group-at-point))

;; Sidebar wrappers for global toggles/actions
(defun context-navigator-sidebar-toggle-push ()
  "Toggle push-to-gptel session flag and refresh header."
  (interactive)
  (ignore-errors (context-navigator-toggle-push-to-gptel))
  (context-navigator-sidebar--schedule-render))

(defun context-navigator-sidebar-toggle-auto-project ()
  "Toggle auto-project-switch session flag and refresh header."
  (interactive)
  (ignore-errors (context-navigator-toggle-auto-project-switch))
  (context-navigator-sidebar--schedule-render))

(defun context-navigator-sidebar-push-now ()
  "Manually push current items to gptel (reset + add)."
  (interactive)
  (ignore-errors (context-navigator-push-to-gptel-now))
  (context-navigator-sidebar--schedule-render))

(defun context-navigator-sidebar-open-all-buffers ()
  "Open all file/buffer/selection items from current model in background (no window selection).

This opens file-backed buffers (via `find-file-noselect') for items of
type `file', `buffer' and `selection' when they reference an existing file.
Buffers are opened in background; we do not change window focus."
  (interactive)
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
    (context-navigator-sidebar--schedule-render)
    (message "Opened %d context buffer(s) in background" count)))

(defun context-navigator-sidebar-clear-gptel ()
  "Manually clear gptel context without touching the model."
  (interactive)
  (ignore-errors (context-navigator-clear-gptel-now))
  (context-navigator-sidebar--schedule-render))

;;; Dispatchers and commands

(defun context-navigator-sidebar-activate ()
  "RET action:
- On toggle segments in header: toggle push/auto flags
- On footer action segments: invoke the assigned action (push/clear/open buffers)
- In groups mode: open group at point
- In items mode: \"..\" goes to groups; otherwise visit item."
  (interactive)
  (let ((act (get-text-property (point) 'context-navigator-action))
        (tgl (get-text-property (point) 'context-navigator-toggle)))
    (cond
     ;; Footer actions (explicit)
     ((eq act 'push-now) (context-navigator-sidebar-push-now))
     ((eq act 'clear-gptel) (context-navigator-sidebar-clear-gptel))
     ((eq act 'open-buffers) (context-navigator-sidebar-open-all-buffers))
     ;; Header toggles
     ((eq tgl 'push) (context-navigator-sidebar-toggle-push))
     ((eq tgl 'auto) (context-navigator-sidebar-toggle-auto-project))
     ((eq context-navigator-sidebar--mode 'groups)
      (or (context-navigator-sidebar--open-group-at-point)
          (message "No group at point")))
     (t
      (if (get-text-property (point) 'context-navigator-groups-up)
          (context-navigator-sidebar-go-up)
        (context-navigator-sidebar-visit))))))

(defun context-navigator-sidebar-refresh-dispatch ()
  "g action: refresh items or groups, depending on mode."
  (interactive)
  (if (eq context-navigator-sidebar--mode 'groups)
      (ignore-errors (context-navigator-groups-open))
    (context-navigator-sidebar-refresh)))

(defun context-navigator-sidebar-delete-dispatch ()
  "d action: delete item (items mode) or group (groups mode)."
  (interactive)
  (if (eq context-navigator-sidebar--mode 'groups)
      (if-let* ((cell (context-navigator-sidebar--at-group)))
          (ignore-errors (context-navigator-group-delete (car cell)))
        (message "No group at point"))
    (context-navigator-sidebar-delete-from-model)))

(defun context-navigator-sidebar-go-up ()
  "Toggle between items and groups views.
- From items -> switch to groups and fetch list
- From groups -> switch back to items"
  (interactive)
  (if (eq context-navigator-sidebar--mode 'groups)
      (progn
        (setq context-navigator-sidebar--mode 'items)
        (context-navigator-sidebar--schedule-render))
    (setq context-navigator-sidebar--mode 'groups)
    (ignore-errors (context-navigator-groups-open))
    (context-navigator-sidebar--schedule-render)))

(defun context-navigator-sidebar-group-create ()
  "Create a new group (groups mode)."
  (interactive)
  (if (eq context-navigator-sidebar--mode 'groups)
      (let ((slug (ignore-errors (context-navigator-group-create))))
        ;; After successful creation, switch to items view immediately.
        (when (and slug (stringp slug))
          (setq context-navigator-sidebar--mode 'items)
          (context-navigator-sidebar--schedule-render)))
    (message "Press h to open groups list first")))

(defun context-navigator-sidebar-group-rename ()
  "Rename selected group (groups mode)."
  (interactive)
  (if (eq context-navigator-sidebar--mode 'groups)
      (let ((slug (car (or (context-navigator-sidebar--at-group) '(nil)))))
        (ignore-errors (context-navigator-group-rename slug)))
    (message "Press h to open groups list first")))

(defun context-navigator-sidebar-group-duplicate ()
  "Duplicate selected group (groups mode)."
  (interactive)
  (if (eq context-navigator-sidebar--mode 'groups)
      (let ((slug (car (or (context-navigator-sidebar--at-group) '(nil)))))
        (ignore-errors (context-navigator-group-duplicate slug)))
    (message "Press h to open groups list first")))

;;;###autoload
(defun context-navigator-sidebar-show-groups ()
  "Open sidebar (if needed) and show the groups list for current project/global."
  (interactive)
  (let ((buf (get-buffer context-navigator-sidebar--buffer-name)))
    ;; Ensure sidebar is open
    (unless (and buf (get-buffer-window buf t))
      (ignore-errors (context-navigator-sidebar-open))
      (setq buf (get-buffer context-navigator-sidebar--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq context-navigator-sidebar--mode 'groups))
      (ignore-errors (context-navigator-groups-open))
      (when-let ((win (get-buffer-window buf t)))
        (select-window win))
      (context-navigator-sidebar--schedule-render))))

(provide 'context-navigator-sidebar)
;;; context-navigator-sidebar.el ends here
