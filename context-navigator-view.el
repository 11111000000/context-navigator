;;; context-navigator-view.el --- Sidebar UI (side window) -*- lexical-binding: t; -*-

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

(defcustom context-navigator-openable-count-ttl 1.0
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
(declare-function context-navigator-toggle-item "context-navigator-core" (key &optional enabled))
(declare-function context-navigator-remove-item-by-key "context-navigator-core" (key))
(declare-function context-navigator-context-clear-current-group "context-navigator-core" ())
(declare-function context-navigator-context-unload "context-navigator-core" ())
;; group commands (core)
(declare-function context-navigator-groups-open "context-navigator-core" ())
(declare-function context-navigator-group-switch "context-navigator-core" (&optional slug))
(declare-function context-navigator-group-create "context-navigator-core" (&optional display-name))
(declare-function context-navigator-group-rename "context-navigator-core" (&optional old-slug new-display))
(declare-function context-navigator-group-delete "context-navigator-core" (&optional slug))
(declare-function context-navigator-group-duplicate "context-navigator-core" (&optional src-slug new-display))

(defconst context-navigator-view--buffer-name "*context-navigator*")

(defun context-navigator-view--in-buffer (fn)
  "Run FN inside the Navigator view buffer when available."
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (funcall fn)))))

(defvar-local context-navigator-view--subs nil)
(defvar-local context-navigator-view--header "Context")
(defvar-local context-navigator-view--mode 'items) ;; 'items | 'groups
(defvar-local context-navigator-view--groups nil)  ;; cached groups plists
(defvar-local context-navigator-view--last-lines nil)
(defvar context-navigator-view--group-line-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m [mouse-1] #'context-navigator-view-mouse-open-group)
    m)
  "Keymap attached to group lines to support mouse clicks.")
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
    (define-key m [mouse-1] #'context-navigator-view-toggle-collapse)
    ;; TAB on title behaves like in Magit: toggle collapse
    (define-key m (kbd "TAB")       #'context-navigator-view-toggle-collapse)
    (define-key m (kbd "<tab>")     #'context-navigator-view-toggle-collapse)
    (define-key m [tab]             #'context-navigator-view-toggle-collapse)
    (define-key m (kbd "C-i")       #'context-navigator-view-toggle-collapse)
    ;; RET on title also toggles collapse/expand
    (define-key m (kbd "RET")       #'context-navigator-view-toggle-collapse)
    (define-key m (kbd "C-m")       #'context-navigator-view-toggle-collapse)
    (define-key m [return]          #'context-navigator-view-toggle-collapse)
    (define-key m (kbd "<return>")  #'context-navigator-view-toggle-collapse)
    m)
  "Keymap attached to the title line to support mouse/TAB/RET collapse/expand.")

(defcustom context-navigator-view-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames used by the sidebar loading spinner (list of single-frame strings)."
  :type '(repeat string)
  :group 'context-navigator)

(defcustom context-navigator-view-spinner-interval
  0.1
  "Spinner animation interval in seconds for the sidebar loading indicator."
  :type 'number
  :group 'context-navigator)

(defvar-local context-navigator-view--spinner-timer nil)          ;; loading spinner timer
(defvar-local context-navigator-view--spinner-index 0)
(defvar-local context-navigator-view--spinner-last-time 0.0)      ;; last tick timestamp (float-time)
(defvar-local context-navigator-view--spinner-degraded nil)       ;; when non-nil, render static indicator

(defvar context-navigator-view-window-params
  '((side . left) (slot . -1))
  "Default parameters for the sidebar window.")

(defcustom context-navigator-view-spinner-degrade-threshold
  0.25
  "If spinner timer slips by more than this (seconds), degrade to a static indicator until load completes."
  :type 'number :group 'context-navigator)

(defun context-navigator-view--header (state)
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

(defun context-navigator-view--wrap-segments (segments total-width)
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


(defun context-navigator-view--make-toggle-segments ()
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
                               (define-key km [mouse-1] #'context-navigator-view-toggle-push)
                               (define-key km [header-line mouse-1] #'context-navigator-view-toggle-push))
                             km))
                        (fg (if (and push-on gptel-available) "green4" "gray"))
                        (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
                   (add-text-properties beg (length s)
                                        (list 'mouse-face 'highlight
                                              'help-echo (if gptel-available
                                                             (context-navigator-i18n :toggle-push)
                                                           "gptel not available")
                                              'keymap m
                                              'local-map m
                                              'context-navigator-toggle 'push
                                              'face (if gptel-available
                                                        (list :foreground fg)
                                                      'shadow))
                                        s)
                   s))
           (seg2 (let* ((s (copy-sequence lbl-auto))
                        (m (let ((km (make-sparse-keymap)))
                             (define-key km [mouse-1] #'context-navigator-view-toggle-auto-project)
                             (define-key km [header-line mouse-1] #'context-navigator-view-toggle-auto-project)
                             km))
                        (fg (if auto-on "green4" "gray"))
                        (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
                   (add-text-properties beg (length s)
                                        (list 'mouse-face 'highlight
                                              'help-echo (context-navigator-i18n :toggle-auto)
                                              'keymap m
                                              'local-map m
                                              'context-navigator-toggle 'auto
                                              'face (list :foreground fg))
                                        s)
                   s)))
      (list seg1 seg2))))


(defun context-navigator-view--header-toggle-lines (total-width)
  "Return header toggle lines wrapped to TOTAL-WIDTH."
  (let ((toggles (context-navigator-view--make-toggle-segments)))
    (context-navigator-view--wrap-segments toggles total-width)))

;; Openable count helpers (footer [O]) ---------------------------------------

(defun context-navigator-view--invalidate-openable ()
  "Invalidate cached openable counters and cancel pending timers (safe outside the view buffer)."
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq context-navigator-view--openable-count nil
              context-navigator-view--openable-plus nil
              context-navigator-view--openable-stamp 0.0)
        (let ((tm (and (boundp 'context-navigator-view--openable-timer)
                       context-navigator-view--openable-timer)))
          (when (timerp tm)
            (cancel-timer tm))
          (setq context-navigator-view--openable-timer nil))))))

(defun context-navigator-view--openable--candidate-p (item)
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

(defun context-navigator-view--compute-openable (cap)
  "Return cons (COUNT . PLUS) for openable items, short-circuiting at CAP.
PLUS is non-nil when CAP was reached."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st)))
         (n 0)
         (limit (or cap most-positive-fixnum)))
    (catch 'done
      (dolist (it (or items '()))
        (when (context-navigator-view--openable--candidate-p it)
          (setq n (1+ n))
          (when (>= n limit)
            (throw 'done t)))))
    (cons n (and (numberp cap) (>= n (or cap 0))))))

(defun context-navigator-view--openable-count-refresh ()
  "Recompute openable count synchronously with soft-cap; update cache and schedule UI refresh if changed."
  (let* ((res (context-navigator-view--compute-openable context-navigator-openable-soft-cap))
         (n (car res))
         (plus (cdr res))
         ;; Guard against void-variable when timers fire outside the sidebar buffer/loading order
         (old (and (boundp 'context-navigator-view--openable-count)
                   context-navigator-view--openable-count))
         (changed (not (equal old n))))
    (setq context-navigator-view--openable-count n
          context-navigator-view--openable-plus plus
          context-navigator-view--openable-stamp (float-time))
    (when changed
      (context-navigator-view--schedule-render))
    n))

(defun context-navigator-view--openable-count-get ()
  "Return cons (COUNT . PLUS) from cache; schedule a debounced refresh when stale."
  (let* ((now (float-time))
         (ttl (or context-navigator-openable-count-ttl 0.3))
         ;; Robust when variables are not yet bound in the running buffer/timer context
         (count (and (boundp 'context-navigator-view--openable-count)
                     context-navigator-view--openable-count))
         (stamp (if (boundp 'context-navigator-view--openable-stamp)
                    context-navigator-view--openable-stamp
                  0.0))
         (plus  (and (boundp 'context-navigator-view--openable-plus)
                     context-navigator-view--openable-plus)))
    (when (or (null count)
              (> (- now stamp) (max 0 ttl)))
      (context-navigator-events-debounce
       :sidebar-openable 0.18
       (lambda ()
         (let ((buf (get-buffer context-navigator-view--buffer-name)))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (context-navigator-view--openable-count-refresh)))))))
    (cons (or count 0)
          (and plus
               (> (or count 0) 0)))))

;; Closable count helpers (footer [∅]) ---------------------------------------

(defun context-navigator-view--collect-closable-buffers ()
  "Collect unique live buffers corresponding to current model items.
Returns a list of buffer objects; excludes the sidebar buffer itself."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st)))
         (seen (make-hash-table :test 'eq))
         (res '())
         (sidebar-buf (get-buffer context-navigator-view--buffer-name)))
    (dolist (it (or items '()))
      (let ((type (context-navigator-item-type it)))
        (cond
         ((eq type 'buffer)
          (let ((buf (or (context-navigator-item-buffer it)
                         (and (context-navigator-item-path it)
                              (get-file-buffer (context-navigator-item-path it))))))
            (when (and (bufferp buf) (buffer-live-p buf)
                       (not (eq buf sidebar-buf))
                       (not (gethash buf seen)))
              (puthash buf t seen)
              (push buf res))))
         ((eq type 'selection)
          (let ((p (context-navigator-item-path it)))
            (when (and (stringp p) (get-file-buffer p))
              (let ((buf (get-file-buffer p)))
                (when (and (buffer-live-p buf)
                           (not (eq buf sidebar-buf))
                           (not (gethash buf seen)))
                  (puthash buf t seen)
                  (push buf res))))))
         ((eq type 'file)
          (let ((p (context-navigator-item-path it)))
            (when (and (stringp p) (get-file-buffer p))
              (let ((buf (get-file-buffer p)))
                (when (and (buffer-live-p buf)
                           (not (eq buf sidebar-buf))
                           (not (gethash buf seen)))
                  (puthash buf t seen)
                  (push buf res)))))))))
    (nreverse res)))

;; Loading spinner helpers ----------------------------------------------------

(defun context-navigator-view--spinner-start ()
  "Start or restart the lightweight loading spinner timer.
Degrades to a static indicator when timer slippage exceeds threshold."
  (when (timerp context-navigator-view--spinner-timer)
    (cancel-timer context-navigator-view--spinner-timer))
  (setq context-navigator-view--spinner-index 0)
  (setq context-navigator-view--spinner-last-time (float-time))
  (setq context-navigator-view--spinner-degraded nil)
  (let* ((interval (or context-navigator-view-spinner-interval 0.1))
         (buf (current-buffer)))
    (setq context-navigator-view--spinner-timer
          (run-at-time 0 interval
                       (lambda ()
                         (let ((bb (if (buffer-live-p buf) buf (get-buffer context-navigator-view--buffer-name))))
                           (when (buffer-live-p bb)
                             (with-current-buffer bb
                               (let* ((now (float-time))
                                      (dt (- now (or context-navigator-view--spinner-last-time now)))
                                      (thr (or context-navigator-view-spinner-degrade-threshold 0.25)))
                                 (setq context-navigator-view--spinner-last-time now)
                                 (when (> dt (+ interval thr))
                                   ;; таймер подтормаживает — больше не дёргаем анимацию
                                   (setq context-navigator-view--spinner-degraded t)
                                   (when (timerp context-navigator-view--spinner-timer)
                                     (cancel-timer context-navigator-view--spinner-timer))
                                   (setq context-navigator-view--spinner-timer nil))
                                 (when (not context-navigator-view--spinner-degraded)
                                   (setq context-navigator-view--spinner-index
                                         (1+ (or context-navigator-view--spinner-index 0))))
                                 (context-navigator-view--schedule-render))))))))))
(defun context-navigator-view--spinner-stop ()
  "Stop the loading spinner timer and reset index."
  (when (timerp context-navigator-view--spinner-timer)
    (cancel-timer context-navigator-view--spinner-timer))
  (setq context-navigator-view--spinner-timer nil)
  (setq context-navigator-view--spinner-index 0)
  (setq context-navigator-view--spinner-last-time 0.0)
  (setq context-navigator-view--spinner-degraded nil))


(defun context-navigator-view--footer-control-segments ()
  "Build footer control segments: toggles + [Open buffers], [Close buffers], [Clear group] and a gptel mass toggle.

Changes:
- Hide [Push now] when auto-push is ON
- Rename [Clear gptel] to \"Выключить все в gptel\"
- When all items are disabled, replace it with \"Включить все gptel\""
  (let* ((push-on (and (boundp 'context-navigator--push-to-gptel)
                       context-navigator--push-to-gptel))
         ;; use cached keys from gptel (do not pull during render)
         (has-gptel (and (listp context-navigator-view--gptel-keys)
                         (> (length context-navigator-view--gptel-keys) 0)))
         (style (or context-navigator-controls-style 'auto))
         ;; current items presence for [Clear group]
         (st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st)))
         (have-items (and (listp items) (> (length items) 0)))
         (all-disabled
          (and (listp items)
               (or (= (length items) 0)
                   (cl-every (lambda (it) (not (context-navigator-item-enabled it))) items))))
         (segs '()))
    ;; Toggles first (push → gptel, auto-project)
    (setq segs (append segs (context-navigator-view--make-toggle-segments)))
    ;; [Open buffers] — open file-backed buffers/selections/files in background (lazy/strict remote)
    (cl-destructuring-bind (openable . plus)
        (context-navigator-view--openable-count-get)
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
                        (define-key km [mouse-1] #'context-navigator-view-open-all-buffers)
                        (define-key km [header-line mouse-1] #'context-navigator-view-open-all-buffers)
                        km)))
              (add-text-properties beg (length s)
                                   (list 'mouse-face 'highlight
                                         'help-echo disp
                                         'keymap m
                                         'local-map m)
                                   s))
          (add-text-properties beg (length s)
                               (list 'face 'shadow
                                     'help-echo "No buffers to open")
                               s))
        (push s segs)))
    ;; [Close buffers] — close all live buffers that belong to the current group items
    (let* ((closable-bufs (context-navigator-view--collect-closable-buffers))
           (closable (length closable-bufs))
           (label (pcase style
                    ((or 'icons 'auto) " [∅]")
                    (_ (format " [%s]" (context-navigator-i18n :close-buffers)))))
           (s (copy-sequence label))
           (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
      (add-text-properties beg (length s)
                           (list 'context-navigator-action 'close-buffers)
                           s)
      (if (> closable 0)
          (let ((m (let ((km (make-sparse-keymap)))
                     (define-key km [mouse-1] #'context-navigator-view-close-all-buffers)
                     (define-key km [header-line mouse-1] #'context-navigator-view-close-all-buffers)
                     km)))
            (add-text-properties beg (length s)
                                 (list 'mouse-face 'highlight
                                       'help-echo (format "Close %d context buffer(s) (K)" closable)
                                       'keymap m
                                       'local-map m)
                                 s))
        (add-text-properties beg (length s)
                             (list 'face 'shadow
                                   'help-echo "No context buffers are open")
                             s))
      (push s segs))
    ;; [Clear group] — clears current group's items; inactive when no items present.
    (let* ((label (pcase style
                    ((or 'icons 'auto) " [✖]")
                    (_ (concat " [" (context-navigator-i18n :clear-group) "]"))))
           (s (copy-sequence label))
           (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
      (add-text-properties beg (length s)
                           (list 'context-navigator-action 'clear-group)
                           s)
      (if have-items
          (let ((m (let ((km (make-sparse-keymap)))
                     (define-key km [mouse-1] #'context-navigator-view-clear-group)
                     (define-key km [header-line mouse-1] #'context-navigator-view-clear-group)
                     km)))
            (add-text-properties beg (length s)
                                 (list 'mouse-face 'highlight
                                       'help-echo (context-navigator-i18n :clear-group-tip)
                                       'keymap m
                                       'local-map m)
                                 s))
        (add-text-properties beg (length s)
                             (list 'face 'shadow
                                   'help-echo (context-navigator-i18n :clear-group-tip))
                             s))
      (push s segs))
    ;; [Push now] — only when auto-push is OFF and there are items.
    (when (not push-on)
      (let* ((label (pcase style
                      ((or 'icons 'auto) " [⇪]")
                      (_ (concat " [" (context-navigator-i18n :push-now) "]"))))
             (s (copy-sequence label))
             (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
        (add-text-properties beg (length s) (list 'context-navigator-action 'push-now) s)
        (if (not have-items)
            (add-text-properties beg (length s)
                                 (list 'face 'shadow
                                       'help-echo (context-navigator-i18n :push-tip))
                                 s)
          (let ((m (let ((km (make-sparse-keymap)))
                     (define-key km [mouse-1] #'context-navigator-view-push-now)
                     (define-key km [header-line mouse-1] #'context-navigator-view-push-now)
                     km)))
            (add-text-properties beg (length s)
                                 (list 'mouse-face 'highlight
                                       'help-echo (context-navigator-i18n :push-tip)
                                       'keymap m
                                       'local-map m)
                                 s)))
        (push s segs)))
    ;; [Toggle all gptel]: \"Выключить все в gptel\" vs \"Включить все gptel\"
    (let* ((label (pcase style
                    ((or 'icons 'auto) " [⌦]")
                    (_ (concat " " (if all-disabled "[Включить все gptel]" "[Выключить все в gptel]")))))
           (s (copy-sequence label))
           (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
      ;; Mark with a dedicated action so RET can dispatch appropriately.
      (add-text-properties beg (length s) (list 'context-navigator-action 'toggle-all-gptel) s)
      (let ((m (let ((km (make-sparse-keymap)))
                 (define-key km [mouse-1] #'context-navigator-view-toggle-all-gptel)
                 (define-key km [header-line mouse-1] #'context-navigator-view-toggle-all-gptel)
                 km)))
        (if (and all-disabled (not have-items))
            ;; nothing to enable — keep visible but inactive
            (add-text-properties beg (length s)
                                 (list 'face 'shadow
                                       'help-echo "No items to enable")
                                 s)
          (add-text-properties beg (length s)
                               (list 'mouse-face 'highlight
                                     'help-echo (if all-disabled
                                                    "Enable all items and push to gptel"
                                                  (context-navigator-i18n :clear-tip))
                                     'keymap m
                                     'local-map m)
                               s)))
      (push s segs))
    (nreverse segs)))


(defun context-navigator-view--footer-control-lines (total-width)
  "Return footer control lines wrapped to TOTAL-WIDTH."
  (let ((controls (context-navigator-view--footer-control-segments)))
    (context-navigator-view--wrap-segments controls total-width)))


(defun context-navigator-view--groups-header-lines (header total-width)
  "Return list with a single clickable title line for groups view.

Shows only [project] and supports TAB/RET/mouse-1 collapse like items."
  (list (context-navigator-view--title-line header)))


(defun context-navigator-view--groups-body-lines (state)
  "Return list of lines for groups body using STATE."
  (let* ((active (and (context-navigator-state-p state)
                      (context-navigator-state-current-group-slug state))))
    (cond
     ((not (listp context-navigator-view--groups))
      (list (propertize (context-navigator-i18n :no-groups) 'face 'shadow)))
     (t
      (let (lines)
        (dolist (pl context-navigator-view--groups)
          (let* ((slug (or (plist-get pl :slug) ""))
                 (disp (or (plist-get pl :display) slug))
                 (ic (or (ignore-errors (context-navigator-icons-for-group)) "📁"))
                 (txt (concat ic " " disp))
                 (s (copy-sequence txt)))
            (add-text-properties 0 (length s)
                                 (list 'context-navigator-group-slug slug
                                       'context-navigator-group-display disp
                                       'mouse-face 'highlight
                                       'keymap context-navigator-view--group-line-keymap
                                       'local-map context-navigator-view--group-line-keymap
                                       'help-echo (context-navigator-i18n :mouse-open-group))
                                 s)
            (when (and context-navigator-highlight-active-group
                       active (string= active slug))
              (add-text-properties 0 (length s) (list 'face 'mode-line-emphasis) s))
            (setq lines (append lines (list s)))))
        lines)))))


(defun context-navigator-view--groups-footer-lines (total-width)
  "Return footer lines for groups view wrapped to TOTAL-WIDTH (controls moved to header-line).

Remove the single-key \"? for help\" hint; the header-line contains controls
and the transient/help command can be invoked explicitly."
  (let* ((help-segments
          (list (context-navigator-i18n :groups-help-open)
                (context-navigator-i18n :groups-help-add)
                (context-navigator-i18n :groups-help-rename)
                (context-navigator-i18n :groups-help-delete)
                (context-navigator-i18n :groups-help-copy)
                (context-navigator-i18n :groups-help-refresh)
                (context-navigator-i18n :groups-help-back)
                (context-navigator-i18n :groups-help-quit)))
         (help-lines
          (mapcar (lambda (s) (propertize s 'face 'shadow))
                  (context-navigator-view--wrap-segments help-segments total-width))))
    ;; Controls/toggles are shown in the header-line. Keep a blank line, then help lines.
    (append (list "") help-lines)))


(defun context-navigator-view--render-groups (state header total-width)
  "Render groups view using STATE, HEADER and TOTAL-WIDTH.
Returns the list of lines that were rendered."
  (let* ((hl-lines (context-navigator-view--groups-header-lines header total-width))
         (hl (car hl-lines))
         (body (append
                (context-navigator-view--groups-body-lines state)
                (context-navigator-view--groups-footer-lines total-width)))
         (lines (if context-navigator-view--collapsed-p
                    (list "" hl)
                  (cons "" (cons hl body)))))
    (setq context-navigator-view--last-lines lines
          context-navigator-view--header header)
    (context-navigator-render-apply-to-buffer (current-buffer) lines)
    ;; Keep point where user navigated with n/p; only auto-focus when:
    ;; - entering the groups view (first render in this mode), or
    ;; - the active group changed since last render.
    ;; Do not steal point when user moves to footer/help with TAB/Shift-TAB.
    (unless context-navigator-view--collapsed-p
      (let* ((active (and (context-navigator-state-p state)
                          (context-navigator-state-current-group-slug state)))
             (here (point))
             (on-group (get-text-property here 'context-navigator-group-slug))
             (need-focus (or (not (eq context-navigator-view--last-mode 'groups))
                             (not (equal context-navigator-view--last-active-group active)))))
        (when need-focus
          (let (pos)
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
              (beginning-of-line)))))
      (setq context-navigator-view--last-active-group active
            context-navigator-view--last-mode 'groups))
    lines))

(defun context-navigator-view--items-header-toggle-lines (total-width)
  "Return header toggle lines for items view wrapped to TOTAL-WIDTH."
  (context-navigator-view--header-toggle-lines total-width))

(defun context-navigator-view-toggle-collapse ()
  "Toggle collapse/expand of the items section under the title line."
  (interactive)
  (setq context-navigator-view--collapsed-p (not context-navigator-view--collapsed-p))
  (context-navigator-view--schedule-render))

(defun context-navigator-view--title-line (header)
  "Return a single interactive title line built from HEADER with a disclosure triangle.
TAB or mouse-1 on this line toggles collapsing the section below."
  (let* ((arrow (if context-navigator-view--collapsed-p "▸" "▾"))
         (txt (format " %s %s" arrow (or header "Context")))
         (s (copy-sequence txt)))
    (add-text-properties 0 (length s)
                         (list 'context-navigator-title t
                               'mouse-face 'highlight
                               'help-echo "TAB: collapse/expand"
                               'keymap context-navigator-view--title-line-keymap
                               'local-map context-navigator-view--title-line-keymap
                               'face 'mode-line-emphasis)
                         s)
    s))

(defun context-navigator-view--items-base-lines (state header total-width)
  "Return a list: (hl sep up rest...) for items view base lines.
HL is the clickable [project[: group]] title line placed above \"..\".
SEP is currently empty (no extra separator in the buffer).
UP is the \"..\" line.
REST is a list of item lines."
  (let* ((items (context-navigator-state-items state))
         ;; generation-aware cached sort: avoid re-sorting identical model generation
         (gen (or (and (context-navigator-state-p state)
                       (context-navigator-state-generation state))
                  0))
         (root (and (context-navigator-state-p state)
                    (context-navigator-state-last-project-root state)))
         ;; relpath cache per generation/root
         (_ (unless (and context-navigator-view--relpaths-hash
                         (= gen (or context-navigator-view--sorted-gen -1))
                         (equal root context-navigator-view--sorted-root))
              (setq context-navigator-view--relpaths-hash (make-hash-table :test 'equal))))
         (relpath-of
          (lambda (it)
            (let* ((key (context-navigator-model-item-key it))
                   (cached (and context-navigator-view--relpaths-hash
                                (gethash key context-navigator-view--relpaths-hash)))
                   (p (and (stringp (context-navigator-item-path it))
                           (context-navigator-item-path it))))
              (or cached
                  (let* ((rel (condition-case _err
                                  (if (and p root)
                                      (file-relative-name (expand-file-name p)
                                                          (file-name-as-directory (expand-file-name root)))
                                    p)
                                (error p))))
                    (when (hash-table-p context-navigator-view--relpaths-hash)
                      (puthash key rel context-navigator-view--relpaths-hash))
                    rel)))))
         ;; natural-lessp (case-insensitive with numeric chunks)
         (natural-lessp
          (lambda (a b)
            (cl-labels
                ((chunks (s)
                   (let ((i 0) (n (length s)) (cur "") (res '()))
                     (while (< i n)
                       (let ((c (aref s i)))
                         (if (and (>= c ?0) (<= c ?9))
                             ;; flush cur then read number
                             (progn
                               (when (> (length cur) 0)
                                 (push (cons :str (downcase cur)) res)
                                 (setq cur ""))
                               (let ((j i))
                                 (while (and (< j n)
                                             (let ((d (aref s j))) (and (>= d ?0) (<= d ?9))))
                                   (setq j (1+ j)))
                                 (push (cons :num (string-to-number (substring s i j))) res)
                                 (setq i (1- j))))
                           (setq cur (concat cur (string c)))))
                       (setq i (1+ i)))
                     (when (> (length cur) 0)
                       (push (cons :str (downcase cur)) res))
                     (nreverse res)))
                 (cmp (xa xb)
                   (cond
                    ((and (null xa) (null xb)) nil)
                    ((null xa) t)
                    ((null xb) nil)
                    (t
                     (let* ((a1 (car xa)) (b1 (car xb)))
                       (cond
                        ((and (eq (car a1) :num) (eq (car b1) :num))
                         (if (/= (cdr a1) (cdr b1))
                             (< (cdr a1) (cdr b1))
                           (cmp (cdr xa) (cdr xb))))
                        ((and (eq (car a1) :str) (eq (car b1) :str))
                         (let ((sa (cdr a1)) (sb (cdr b1)))
                           (if (string= sa sb)
                               (cmp (cdr xa) (cdr xb))
                             (string-lessp sa sb))))
                        ((eq (car a1) :num) t)
                        (t nil)))))))
              (cmp (chunks (or a "")) (chunks (or b ""))))))
         (in-subdir-p
          (lambda (rel)
            (and (stringp rel)
                 (not (string-prefix-p ".." rel))
                 (string-match-p "/" rel))))
         (basename-of
          (lambda (rel-or-path)
            (file-name-nondirectory (or rel-or-path ""))))
         (sorted-items
          (if (and (listp context-navigator-view--sorted-items)
                   (integerp context-navigator-view--sorted-gen)
                   (= gen context-navigator-view--sorted-gen)
                   (equal root context-navigator-view--sorted-root))
              context-navigator-view--sorted-items
            (let ((s (sort (copy-sequence (or items '()))
                           (lambda (a b)
                             (let* ((ra (funcall relpath-of a))
                                    (rb (funcall relpath-of b))
                                    (pa (and (stringp (context-navigator-item-path a))
                                             (context-navigator-item-path a)))
                                    (pb (and (stringp (context-navigator-item-path b))
                                             (context-navigator-item-path b)))
                                    (has-pa (and (stringp pa) (> (length pa) 0)))
                                    (has-pb (and (stringp pb) (> (length pb) 0)))
                                    (sa (and has-pa (funcall in-subdir-p ra)))
                                    (sb (and has-pb (funcall in-subdir-p rb))))
                               (cond
                                ;; Items with paths first; without paths at the bottom
                                ((and (not has-pa) (not has-pb))
                                 ;; both without paths -> natural by name
                                 (funcall natural-lessp
                                          (downcase (or (context-navigator-item-name a) ""))
                                          (downcase (or (context-navigator-item-name b) ""))))
                                ((and (not has-pa) has-pb) nil)
                                ((and has-pa (not has-pb)) t)
                                ;; Both have paths: subdirs first
                                ((and sa (not sb)) t)
                                ((and (not sa) sb) nil)
                                ;; Same group: natural sort by relpath (dir/base)
                                (t (funcall natural-lessp
                                            (downcase (or ra ""))
                                            (downcase (or rb ""))))))))))
              (setq context-navigator-view--sorted-items s
                    context-navigator-view--sorted-gen gen
                    context-navigator-view--sorted-root root)
              s)))
         (left-width (max 16 (min (- total-width 10) (floor (* 0.55 total-width)))))
         (base (let ((context-navigator-render--gptel-keys context-navigator-view--gptel-keys))
                 (context-navigator-render-build-lines sorted-items header
                                                       #'context-navigator-icons-for-item
                                                       left-width)))
         ;; Title/header is rendered inside the buffer (interactive, collapsible).
         (hl (context-navigator-view--title-line header))
         (sep "")
         (rest (cddr base))
         (up (let ((s (copy-sequence "..")))
               (add-text-properties 0 (length s)
                                    (list 'context-navigator-groups-up t
                                          'mouse-face 'highlight
                                          'help-echo (context-navigator-i18n :status-up-to-groups)
                                          'face 'shadow)
                                    s)
               s)))
    (list hl sep up rest)))


(defun context-navigator-view--status-text-at-point ()
  "Return status text for the current point:
- title line → show the header text (project[: group])
- item line → relative path to project root (or absolute when no root)
- \"..\" line → localized \"<to groups>\"
- footer action/toggle → their help-echo
- otherwise → empty string."
  (cond
   ;; On title/header line
   ((get-text-property (point) 'context-navigator-title)
    (or context-navigator-view--header ""))
   ;; On an item: show relative path (dirs + basename), or buffer name when no path
   ((get-text-property (point) 'context-navigator-item)
    (let* ((it (get-text-property (point) 'context-navigator-item))
           (p  (context-navigator-item-path it))
           (st (ignore-errors (context-navigator--state-get)))
           (root (and st (ignore-errors (context-navigator-state-last-project-root st)))))
      (cond
       ((and (stringp p) (> (length p) 0))
        (condition-case _err
            (if (stringp root)
                (file-relative-name (expand-file-name p)
                                    (file-name-as-directory (expand-file-name root)))
              (abbreviate-file-name p))
          (error (abbreviate-file-name p))))
       (t (or (context-navigator-item-name it) "")))))
   ;; On up line
   ((get-text-property (point) 'context-navigator-groups-up)
    (context-navigator-i18n :status-up-to-groups))
   ;; On footer action or header toggle: show help-echo
   ((or (get-text-property (point) 'context-navigator-action)
        (get-text-property (point) 'context-navigator-toggle))
    (or (get-text-property (point) 'help-echo) ""))
   (t "")))



(defun context-navigator-view--items-footer-lines (total-width)
  "Return footer lines for items view wrapped to TOTAL-WIDTH (controls moved to header-line).

Per-point status is shown in the modeline now; do not render it inside the
buffer footer. Also remove the single-key help (\"? for help\") from the footer."
  (let* ((help-segments '()) ;; no short inline help any more
         (help-lines
          (mapcar (lambda (s) (propertize s 'face 'shadow))
                  (context-navigator-view--wrap-segments help-segments total-width))))
    ;; No extra blank lines at the bottom of the items view.
    '()))


(defun context-navigator-view--render-items (state header total-width)
  "Render items view using STATE, HEADER and TOTAL-WIDTH.
Returns the list of lines that were rendered."
  (cl-destructuring-bind (hl _sep up rest)
      (context-navigator-view--items-base-lines state header total-width)
    (let* ((body (append (list up) rest
                         (context-navigator-view--items-footer-lines total-width)))
           (lines (if context-navigator-view--collapsed-p
                      (list "" hl)
                    (cons "" (cons hl body)))))
      (setq context-navigator-view--last-lines lines
            context-navigator-view--header header)
      (context-navigator-render-apply-to-buffer (current-buffer) lines)
      lines)))


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
               (if (and buf-win (not (window-parameter buf-win 'context-navigator-view)))
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
               (if (and buf-win (not (window-parameter buf-win 'context-navigator-view)))
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
               (if (and buf-win (not (window-parameter buf-win 'context-navigator-view)))
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

(defun context-navigator-view-visit ()
  "Visit item at point."
  (interactive)
  (context-navigator-view--visit nil))

(defun context-navigator-view-preview ()
  "Preview item at point in other window."
  (interactive)
  (context-navigator-view--visit t))

(defun context-navigator-view-next-item ()
  "Move point to the next item line or \"..\" or group line."
  (interactive)
  (let* ((start (min (1+ (line-end-position)) (point-max)))
         (pos (or (context-navigator-view--find-next-itemish-pos start)
                  (text-property-not-all start (point-max) 'context-navigator-group-slug nil))))
    (unless pos
      ;; wrap to the first item-ish or group element
      (setq pos (or (context-navigator-view--find-next-itemish-pos (point-min))
                    (text-property-not-all (point-min) (point-max) 'context-navigator-group-slug nil))))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

(defun context-navigator-view-previous-item ()
  "Move point to the previous item line or \"..\" or group line."
  (interactive)
  (let* ((start (line-beginning-position))
         (pos (or (context-navigator-view--find-prev-itemish-pos start)
                  (let ((p nil) (best nil))
                    (setq p (text-property-not-all (point-min) start 'context-navigator-group-slug nil))
                    (while (and p (< p start))
                      (setq best p)
                      (setq p (text-property-not-all (1+ p) start 'context-navigator-group-slug nil)))
                    best))))
    (unless pos
      ;; wrap to the last item-ish or group element
      (setq pos (or (context-navigator-view--find-prev-itemish-pos (point-max))
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

(defun context-navigator-view--find-next-interactive-pos (&optional start)
  "Return the nearest position >= START (or point) with an interactive property.
Searches for known interactive properties used in the sidebar."
  (let* ((start (or start (point)))
         (props '(context-navigator-title
                  context-navigator-item
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

(defun context-navigator-view--find-prev-interactive-pos (&optional start)
  "Return the nearest position < START (or point) with an interactive property."
  (let* ((start (or start (point)))
         (pos nil)
         (best nil))
    (setq pos (context-navigator-view--find-next-interactive-pos (point-min)))
    (while (and pos (< pos start))
      (setq best pos)
      (setq pos (context-navigator-view--find-next-interactive-pos (1+ pos))))
    best))

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

(defun context-navigator-view-tab-next ()
  "Move point to the next interactive element (title, items, groups, toggles, actions).

Special case: when on the title line, TAB toggles collapse/expand (magit-like).

Wraps to the top when no further element is found after point."
  (interactive)
  (let* ((here (point)))
    ;; On title line: toggle collapse and do not move
    (when (get-text-property here 'context-navigator-title)
      (context-navigator-view-toggle-collapse)
      (cl-return-from context-navigator-view-tab-next))
    (let* ((props '(context-navigator-title
                    context-navigator-item
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
           (pos (context-navigator-view--find-next-interactive-pos cur-end)))
      (unless pos
        ;; wrap
        (setq pos (context-navigator-view--find-next-interactive-pos (point-min))))
      (if pos
          (progn (goto-char pos))
        (message "No interactive elements")))))

(defun context-navigator-view-tab-previous ()
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
         (pos (context-navigator-view--find-prev-interactive-pos cur-beg)))
    (unless pos
      ;; wrap to last interactive element
      (setq pos (context-navigator-view--find-prev-interactive-pos (point-max))))
    (if pos
        (progn (goto-char pos))
      (message "No interactive elements"))))

(defun context-navigator-view--remove-at-point ()
  "Remove the item at point from gptel context."
  (interactive)
  (when-let* ((item (context-navigator-view--at-item)))
    (let* ((cur (context-navigator-view--state-items))
           (key (context-navigator-model-item-key item))
           (keep (cl-remove-if (lambda (it)
                                 (string= (context-navigator-model-item-key it) key))
                               cur)))
      (ignore-errors (context-navigator-gptel-apply keep))
      ;; model обновится через :gptel-change → core sync
      (message "Removed from context: %s" (or (context-navigator-item-name item) key)))))
(make-obsolete 'context-navigator-view--remove-at-point
               'context-navigator-view-delete-from-model
               "0.2.1")

(defun context-navigator-view-toggle-enabled ()
  "Toggle inclusion of the item at point in gptel sync (model :enabled).

Toggles the item's enabled flag and, when push→gptel is ON, applies the
updated set to gptel immediately. The new state is persisted via the
existing debounced autosave."
  (interactive)
  (when-let* ((item (context-navigator-view--at-item)))
    (let* ((key (context-navigator-model-item-key item))
           (name (or (context-navigator-item-name item) key)))
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
        (message (if en "Enabled: %s" "Disabled: %s") name))
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
        (:added   (message "Added to gptel: %s" (or (context-navigator-item-name item) key)))
        (:removed (message "Removed from gptel: %s" (or (context-navigator-item-name item) key)))
        (_        (message "No change for: %s" (or (context-navigator-item-name item) key)))))))

(defun context-navigator-view-delete-from-model ()
  "Delete the item at point from the model permanently and apply to gptel."
  (interactive)
  (when-let* ((item (context-navigator-view--at-item)))
    (let* ((key (context-navigator-model-item-key item))
           (st (ignore-errors (context-navigator-remove-item-by-key key)))
           (items (and (context-navigator-state-p st) (context-navigator-state-items st))))
      (when (and items (boundp 'context-navigator--push-to-gptel) context-navigator--push-to-gptel)
        (ignore-errors (context-navigator-gptel-apply items)))
      (message "Deleted from model: %s" (or (context-navigator-item-name item) key)))))

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
        (delete-window win)))))

(defun context-navigator-view--subscribe-model-events ()
  "Subscribe to generic model refresh events."
  (push (context-navigator-events-subscribe
         :model-refreshed
         (lambda (&rest _)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (context-navigator-view--invalidate-openable)
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
                 (context-navigator-view--spinner-stop)
                 (unless ok-p
                   (let* ((st (ignore-errors (context-navigator--state-get)))
                          (slug (and st (context-navigator-state-current-group-slug st)))
                          (file (and slug (ignore-errors (context-navigator-persist-context-file root slug)))))
                     (when (and (stringp file) (file-exists-p file))
                       (message "Context Navigator: group '%s' file looks unreadable. Press h to open groups, then d to delete." (or slug "<unknown>")))
                     (when context-navigator-auto-open-groups-on-error
                       (setq context-navigator-view--mode 'groups)
                       (ignore-errors (context-navigator-groups-open)))))
                 (context-navigator-view--schedule-render))))))
        context-navigator-view--subs))

(defun context-navigator-view--subscribe-groups-events ()
  "Subscribe to groups list updates."
  (push (context-navigator-events-subscribe
         :groups-list-updated
         (lambda (_root groups)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq context-navigator-view--groups groups)
                 (when (eq context-navigator-view--mode 'groups)
                   (context-navigator-view--schedule-render)))))))
        context-navigator-view--subs))

(defun context-navigator-view--subscribe-project-events ()
  "Subscribe to project switch updates to refresh groups list when needed."
  (push (context-navigator-events-subscribe
         :project-switch
         (lambda (_root)
           (let ((buf (get-buffer context-navigator-view--buffer-name)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 ;; Invalidate cached groups and refresh when groups view is active.
                 (setq context-navigator-view--groups nil)
                 (when (eq context-navigator-view--mode 'groups)
                   (ignore-errors (context-navigator-groups-open))
                   (context-navigator-view--schedule-render)))))))
        context-navigator-view--subs))
(defun context-navigator-view--gptel-noisy-src-p (src)
  "Вернуть t, если событие пришло от шумного источника, который нужно игнорировать.
Это предотвращает обратную связь от variable-watcher'ов."
  (memq src '(gptel-context gptel-context--alist gptel--context)))

(defun context-navigator-view--collect-gptel-keys ()
  "Собрать список ключей из gptel, предпочитая «сырые» ключи.
Порядок приоритетов:
1. Сырые ключи (read-only, если доступны).
2. Вытянуть из gptel (pull) и преобразовать в ключи.
3. Фоллбек: включённые элементы из текущего состояния."
  (let* ((raw-keys (and (fboundp 'context-navigator-gptel--raw-keys)
                        (ignore-errors (context-navigator-gptel--raw-keys))))
         (pulled-keys
          (and (or (null raw-keys) (= (length raw-keys) 0))
               (let ((lst (ignore-errors (context-navigator-gptel-pull))))
                 (and (listp lst)
                      (mapcar #'context-navigator-model-item-key lst)))))
         (fallback-keys
          (when (and (or (null pulled-keys) (= (length pulled-keys) 0))
                     (or (null raw-keys) (= (length raw-keys) 0)))
            (let* ((st (ignore-errors (context-navigator--state-get)))
                   (items (and st (context-navigator-state-items st))))
              (and (listp items)
                   (mapcar #'context-navigator-model-item-key
                           (cl-remove-if-not #'context-navigator-item-enabled items)))))))
    (or raw-keys pulled-keys fallback-keys '())))

(defun context-navigator-view--update-gptel-keys-if-changed (keys)
  "Обновить кэшированные ключи и перерисовать вид, если KEYs изменились."
  (let ((h (sxhash-equal keys)))
    (unless (equal h context-navigator-view--gptel-keys-hash)
      (setq context-navigator-view--gptel-keys keys
            context-navigator-view--gptel-keys-hash h)
      (ignore-errors
        (context-navigator-debug :debug :ui
                                 "gptel-change: keys=%d hash=%s"
                                 (length keys) h))
      (context-navigator-view--schedule-render))))

(defun context-navigator-view--maybe-refresh-gptel-keys ()
  "Собрать ключи и обновить состояние, если они изменились.
Предполагается, что вызывается из буфера представления."
  (let ((keys (context-navigator-view--collect-gptel-keys)))
    (context-navigator-view--update-gptel-keys-if-changed keys)))

(defun context-navigator-view--on-gptel-change (&rest args)
  "Хэндлер изменений gptel: синхронизировать кэш ключей и перерисовать вид.
Игнорирует шумные события от variable-watcher'ов."
  (let ((src (car args)))
    (unless (context-navigator-view--gptel-noisy-src-p src)
      (let ((buf (get-buffer context-navigator-view--buffer-name)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (context-navigator-view--maybe-refresh-gptel-keys)))))))

(defun context-navigator-view--subscribe-gptel-events ()
  "Подписаться на изменения gptel и держать кэш ключей в синхронизации."
  (push (context-navigator-events-subscribe
         :gptel-change
         #'context-navigator-view--on-gptel-change)
        context-navigator-view--subs))


(defun context-navigator-view--init-gptel-cache ()
  "Initialize cached gptel keys once."
  (let* ((lst (ignore-errors (context-navigator-gptel-pull)))
         (pulled (and (listp lst)
                      (mapcar #'context-navigator-model-item-key lst)))
         (raw (and (or (null pulled) (= (length pulled) 0))
                   (fboundp 'context-navigator-gptel--raw-keys)
                   (ignore-errors (context-navigator-gptel--raw-keys))))
         (keys (or pulled raw '()))
         (h (sxhash-equal keys)))
    (setq context-navigator-view--gptel-keys keys
          context-navigator-view--gptel-keys-hash h)
    (ignore-errors
      (context-navigator-debug :debug :ui
                               "gptel-init: pulled %d keys, hash=%s"
                               (length (or keys '())) h))))

(defun context-navigator-view--close-hijacked-windows ()
  "Close any windows previously marked as sidebar that now show a foreign buffer.

This helper is safe to call multiple times and is intended to be invoked
debounced from `buffer-list-update-hook' to avoid UI thrash.

Note: only applies to sidebar windows; normal buffer-mode windows are not auto-closed."
  (let ((our (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p our)
      (dolist (w (window-list nil nil))
        (when (and (window-live-p w)
                   (eq (window-parameter w 'context-navigator-view) 'sidebar)
                   (not (eq (window-buffer w) our)))
          (ignore-errors (delete-window w)))))))

(defun context-navigator-view--install-buffer-list-hook ()
  "Install buffer-list-update hook to recompute quick counters and detect hijacked sidebar windows."
  (setq context-navigator-view--buflist-fn
        (lambda ()
          (let ((buf (get-buffer context-navigator-view--buffer-name)))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (context-navigator-view--invalidate-openable)
                (context-navigator-view--schedule-render))))
          ;; Debounced scan for windows that were marked as sidebar but now show a foreign buffer.
          (context-navigator-events-debounce
           :sidebar-hijack-check 0.25
           (lambda ()
             (let ((our (get-buffer context-navigator-view--buffer-name)))
               (when (buffer-live-p our)
                 (dolist (w (window-list nil nil))
                   (when (and (window-live-p w)
                              (eq (window-parameter w 'context-navigator-view) 'sidebar)
                              (not (eq (window-buffer w) our)))
                     (ignore-errors (delete-window w))))))))))
  (add-hook 'buffer-list-update-hook context-navigator-view--buflist-fn))

(defun context-navigator-view--install-window-select-hook ()
  "Install window-selection-change hook to re-render on focus and auto-close hijacked sidebar windows.

Only applies to sidebar windows (side-window). Buffer-mode windows are not auto-closed."
  (setq context-navigator-view--winselect-fn
        (lambda (_frame)
          (let ((win (selected-window)))
            (when (and (window-live-p win)
                       (eq (window-parameter win 'context-navigator-view) 'sidebar))
              (let* ((our (get-buffer context-navigator-view--buffer-name))
                     (cur (and (window-live-p win) (window-buffer win))))
                (if (not (eq cur our))
                    ;; Foreign buffer shown in a window that we previously marked
                    ;; as sidebar — close it to avoid dangling side windows.
                    (when (window-live-p win)
                      (delete-window win))
                  ;; Sidebar window actually shows our buffer — gentle refresh.
                  (with-selected-window win
                    (context-navigator-view--schedule-render))))))))
  (add-hook 'window-selection-change-functions context-navigator-view--winselect-fn))

(defun context-navigator-view--initial-compute-counters ()
  "Compute quick counters once on install."
  (context-navigator-view--openable-count-refresh))

(defun context-navigator-view--start-gptel-poll-timer ()
  "Start lightweight polling for gptel indicators while sidebar is visible."
  (let ((int (or context-navigator-gptel-indicator-poll-interval 0)))
    (when (> int 0)
      (setq context-navigator-view--gptel-poll-timer
            (run-at-time 0 int
                         (lambda ()
                           (let ((buf (get-buffer context-navigator-view--buffer-name)))
                             (when (buffer-live-p buf)
                               (with-current-buffer buf
                                 (when (get-buffer-window buf t)
                                   ;; Prefer raw keys (read-only) to avoid triggering gptel collectors.
                                   (let* ((raw-keys (and (fboundp 'context-navigator-gptel--raw-keys)
                                                         (ignore-errors (context-navigator-gptel--raw-keys))))
                                          (pulled-keys
                                           (and (or (null raw-keys) (= (length raw-keys) 0))
                                                (let ((lst (ignore-errors (context-navigator-gptel-pull))))
                                                  (and (listp lst)
                                                       (mapcar #'context-navigator-model-item-key lst)))))
                                          (fallback-keys
                                           (when (and (or (null pulled-keys) (= (length pulled-keys) 0))
                                                      (or (null raw-keys) (= (length raw-keys) 0)))
                                             (let* ((st (ignore-errors (context-navigator--state-get)))
                                                    (items (and st (context-navigator-state-items st))))
                                               (and (listp items)
                                                    (mapcar #'context-navigator-model-item-key
                                                            (cl-remove-if-not #'context-navigator-item-enabled items))))))
                                          (keys (or raw-keys pulled-keys fallback-keys '()))
                                          (h (sxhash-equal keys))
                                          (use-fallback (and (or (null pulled-keys) (= (length pulled-keys) 0))
                                                             (or (null raw-keys) (= (length raw-keys) 0)))))
                                     (unless (equal h context-navigator-view--gptel-keys-hash)
                                       (setq context-navigator-view--gptel-keys keys
                                             context-navigator-view--gptel-keys-hash h)
                                       (ignore-errors
                                         (context-navigator-debug :debug :ui
                                                                  "gptel-poll: updated keys=%d hash=%s%s"
                                                                  (length keys) h
                                                                  (if use-fallback " (fallback)" "")))
                                       (context-navigator-view--schedule-render)))))))))))))

(defun context-navigator-view--install-subs ()
  "Subscribe to relevant events (buffer-local tokens).
Guard against duplicate subscriptions."
  (unless context-navigator-view--subs
    ;; Model and loading lifecycle
    (context-navigator-view--subscribe-model-events)
    (context-navigator-view--subscribe-load-events)
    (context-navigator-view--subscribe-groups-events)
    (context-navigator-view--subscribe-project-events)
    ;; gptel events + advices + initial cache
    (context-navigator-view--subscribe-gptel-events)
    (ignore-errors (context-navigator-gptel-on-change-register))
    (context-navigator-view--init-gptel-cache)
    ;; Hooks and initial compute
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
    (setq context-navigator-view--gptel-poll-timer nil))
  ;; Also unregister gptel advices installed for UI updates.
  (ignore-errors (context-navigator-gptel-on-change-unregister)))

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

(defvar context-navigator-view-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Dispatch RET depending on mode
    (define-key m (kbd "RET")       #'context-navigator-view-activate)
    (define-key m (kbd "C-m")       #'context-navigator-view-activate)
    (define-key m [return]          #'context-navigator-view-activate)
    (define-key m (kbd "<return>")  #'context-navigator-view-activate)
    (define-key m [kp-enter]        #'context-navigator-view-activate)
    (define-key m (kbd "SPC")       #'context-navigator-view-preview)
    (define-key m (kbd "n")   #'context-navigator-view-next-item)
    (define-key m (kbd "p")   #'context-navigator-view-previous-item)
    ;; Vim-like navigation keys
    (define-key m (kbd "j")   #'context-navigator-view-next-item)
    (define-key m (kbd "k")   #'context-navigator-view-previous-item)
    (define-key m (kbd "<down>") #'context-navigator-view-next-item)
    (define-key m (kbd "<up>")   #'context-navigator-view-previous-item)
    (define-key m (kbd "l")   #'context-navigator-view-activate)
    (define-key m (kbd "e")   #'context-navigator-view-toggle-enabled)

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
    (define-key m (kbd "r")   #'context-navigator-view-group-rename)
    (define-key m (kbd "c")   #'context-navigator-view-group-duplicate)
    (define-key m (kbd "q")   #'context-navigator-view-quit)
    (define-key m (kbd "?")   #'context-navigator-view-transient)
    m)
  "Keymap for =context-navigator-view-mode'.")

;; Ensure bindings are updated after reloads (defvar won't reinitialize an existing keymap).
(when (keymapp context-navigator-view-mode-map)
  ;; Keep legacy binding in sync
  (define-key context-navigator-view-mode-map (kbd "e") #'context-navigator-view-toggle-enabled)
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
  (define-key context-navigator-view-mode-map (kbd "E") #'context-navigator-view-clear-group)
  (define-key context-navigator-view-mode-map (kbd "?") #'context-navigator-view-transient))
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
- item lines (have 'context-navigator-item)
- group lines (have 'context-navigator-group-slug)
- the \"..\" line (has 'context-navigator-groups-up)

Do not highlight header/separator lines."
  (when (or (get-text-property (point) 'context-navigator-title)
            (get-text-property (point) 'context-navigator-item)
            (get-text-property (point) 'context-navigator-group-slug)
            (get-text-property (point) 'context-navigator-groups-up))
    (cons (line-beginning-position)
          (min (point-max) (1+ (line-end-position))))))

(define-derived-mode context-navigator-view-mode special-mode "Context-Nav"
  "Major mode for context-navigator sidebar buffer."
  (buffer-disable-undo)
  (setq truncate-lines t
        cursor-type t)
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
          (delete-window w))))))

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
    (message "Opened %d context buffer(s) in background" count)))

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
    (message "Closed %d context buffer(s)" count)))

(defun context-navigator-view-clear-group ()
  "Clear current group's items and redraw immediately."
  (interactive)
  (ignore-errors (context-navigator-debug :debug :ui "UI action: clear-group (event=%S)" last-input-event))
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
- On toggle segments in header: toggle push/auto flags
- On footer action segments: invoke the assigned action (push/open buffers/close buffers/clear group/toggle-all-gptel)
- In groups mode: open group at point
- In items mode: \"..\" goes to groups; otherwise visit item."
  (interactive)
  ;; Title line: collapse/expand
  (when (get-text-property (point) 'context-navigator-title)
    (context-navigator-view-toggle-collapse)
    (cl-return-from context-navigator-view-activate))
  (let ((act (get-text-property (point) 'context-navigator-action))
        (tgl (get-text-property (point) 'context-navigator-toggle)))
    (cond
     ;; Footer actions (explicit)
     ((eq act 'push-now) (context-navigator-view-push-now))
     ((eq act 'open-buffers) (context-navigator-view-open-all-buffers))
     ((eq act 'close-buffers) (context-navigator-view-close-all-buffers))
     ((eq act 'clear-group) (context-navigator-view-clear-group))
     ((eq act 'toggle-all-gptel) (context-navigator-view-toggle-all-gptel))
     ;; Header toggles
     ((eq tgl 'push) (context-navigator-view-toggle-push))
     ((eq tgl 'auto) (context-navigator-view-toggle-auto-project))
     ((eq context-navigator-view--mode 'groups)
      (or (context-navigator-view--open-group-at-point)
          (message "No group at point")))
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
        (message "No group at point"))
    (context-navigator-view-delete-from-model)))

(defun context-navigator-view-go-up ()
  "Toggle between items and groups views.
- From items -> switch to groups and fetch list
- From groups -> switch back to items"
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (progn
        (setq context-navigator-view--mode 'items)
        (context-navigator-view--schedule-render))
    (setq context-navigator-view--mode 'groups)
    (ignore-errors (context-navigator-groups-open))
    (context-navigator-view--schedule-render)))

(defun context-navigator-view-group-create ()
  "Create a new group (groups mode)."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let ((slug (ignore-errors (context-navigator-group-create))))
        ;; After successful creation, switch to items view immediately.
        (when (and slug (stringp slug))
          (setq context-navigator-view--mode 'items)
          (context-navigator-view--schedule-render)))
    (message "Press h to open groups list first")))

(defun context-navigator-view-group-rename ()
  "Rename selected group (groups mode)."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let ((slug (car (or (context-navigator-view--at-group) '(nil)))))
        (ignore-errors (context-navigator-group-rename slug)))
    (message "Press h to open groups list first")))

(defun context-navigator-view-group-duplicate ()
  "Duplicate selected group (groups mode)."
  (interactive)
  (if (eq context-navigator-view--mode 'groups)
      (let ((slug (car (or (context-navigator-view--at-group) '(nil)))))
        (ignore-errors (context-navigator-group-duplicate slug)))
    (message "Press h to open groups list first")))

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

(provide 'context-navigator-view)
;;; context-navigator-view.el ends here
