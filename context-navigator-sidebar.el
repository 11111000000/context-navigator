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

(defcustom context-navigator-auto-open-groups-on-error t
  "When non-nil, automatically switch the sidebar to the groups list if a group fails to load."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-highlight-active-group nil
  "When non-nil, highlight the active group in the groups list."
  :type 'boolean :group 'context-navigator)

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
(defvar-local context-navigator-sidebar--load-progress nil) ;; cons (POS . TOTAL) | nil

(defvar context-navigator-sidebar-window-params
  '((side . left) (slot . -1))
  "Default parameters for the sidebar window.")

(defun context-navigator-sidebar--header (state)
  "Compute header from STATE and local progress, with toggle indicators.

Shows:
- [<root>] Groups                 when in groups mode
- [<root>] group: <slug>          when in items mode
- Global context ...              when no root
- Status toggles: [→gptel: on/off] [auto-proj: on/off]
- When push is off: [Push now] [Clear]"
  (let* ((root (context-navigator-state-last-project-root state))
         (loading (context-navigator-state-loading-p state))
         (group (context-navigator-state-current-group-slug state))
         (base (if root
                   (format "[%s]" (abbreviate-file-name root))
                 "Global context"))
         (extra
          (cond
           ((eq context-navigator-sidebar--mode 'groups) " Groups")
           (group (format "  group: %s" group))
           (t nil)))
         (suffix
          (cond
           ((and loading context-navigator-sidebar--load-progress)
            (format "  [Loading… %d/%d]"
                    (car context-navigator-sidebar--load-progress)
                    (cdr context-navigator-sidebar--load-progress)))
           (loading "  [Loading…]")
           (t nil)))
         ;; session flags (safe if core not loaded yet)
         (push-on (and (boundp 'context-navigator--push-to-gptel)
                       context-navigator--push-to-gptel))
         (auto-on (and (boundp 'context-navigator--auto-project-switch)
                       context-navigator--auto-project-switch))
         ;; clickable header segments
         (seg-push (let* ((s (copy-sequence (format " [→gptel: %s]" (if push-on "on" "off"))))
                          (m (let ((km (make-sparse-keymap)))
                               (define-key km [mouse-1] #'context-navigator-sidebar-toggle-push)
                               km)))
                     (add-text-properties 0 (length s)
                                          (list 'mouse-face 'highlight
                                                'help-echo "Toggle push to gptel (x)"
                                                'keymap m)
                                          s)
                     s))
         (seg-auto (let* ((s (copy-sequence (format " [auto-proj: %s]" (if auto-on "on" "off"))))
                          (m (let ((km (make-sparse-keymap)))
                               (define-key km [mouse-1] #'context-navigator-sidebar-toggle-auto-project)
                               km)))
                     (add-text-properties 0 (length s)
                                          (list 'mouse-face 'highlight
                                                'help-echo "Toggle auto project switch (T)"
                                                'keymap m)
                                          s)
                     s))
         (seg-push-now (when (not push-on)
                         (let* ((s (copy-sequence " [Push now]"))
                                (m (let ((km (make-sparse-keymap)))
                                     (define-key km [mouse-1] #'context-navigator-sidebar-push-now)
                                     km)))
                           (add-text-properties 0 (length s)
                                                (list 'mouse-face 'highlight
                                                      'help-echo "Push current items to gptel now (P)"
                                                      'keymap m)
                                                s)
                           s)))
         (seg-clear (when (not push-on)
                      (let* ((s (copy-sequence " [Clear]"))
                             (m (let ((km (make-sparse-keymap)))
                                  (define-key km [mouse-1] #'context-navigator-sidebar-clear-gptel)
                                  km)))
                        (add-text-properties 0 (length s)
                                             (list 'mouse-face 'highlight
                                                   'help-echo "Clear gptel context (C)"
                                                   'keymap m)
                                             s)
                        s))))
    (concat base (or extra "") (or suffix "")
            seg-push seg-auto
            (or seg-push-now "") (or seg-clear ""))))

(defun context-navigator-sidebar--state-items ()
  "Get items from core state."
  (let* ((st (ignore-errors (context-navigator--state-get))))
    (and st (context-navigator-state-items st))))

(defun context-navigator-sidebar--render ()
  "Render current view (items or groups) into the sidebar buffer."
  (let* ((state (context-navigator--state-get))
         (header (context-navigator-sidebar--header state))
         (win (get-buffer-window (current-buffer) 'visible))
         (total (or (and win (window-body-width win))
                    (and (boundp 'context-navigator-sidebar-width)
                         (symbol-value 'context-navigator-sidebar-width))
                    32)))
    (cond
     ;; Groups mode: render list of groups
     ((eq context-navigator-sidebar--mode 'groups)
      (let* ((title (or header "Groups"))
             (hl (propertize (format " %s" title) 'face 'mode-line-emphasis))
             (sep (make-string (max 8 (min 120 (length hl))) ?-))
             (lines (list hl sep))
             (active (and (context-navigator-state-p state)
                          (context-navigator-state-current-group-slug state))))
        (if (not (listp context-navigator-sidebar--groups))
            (setq lines (append lines (list (propertize "No groups (press a to add)" 'face 'shadow))))
          (dolist (pl context-navigator-sidebar--groups)
            (let* ((slug (or (plist-get pl :slug) ""))
                   (disp (or (plist-get pl :display) slug))
                   (s (copy-sequence disp)))
              (add-text-properties 0 (length s)
                                   (list 'context-navigator-group-slug slug
                                         'context-navigator-group-display disp
                                         'mouse-face 'highlight
                                         'keymap context-navigator-sidebar--group-line-keymap
                                         'help-echo "mouse-1: open group")
                                   s)
              (when (and context-navigator-highlight-active-group
                         active (string= active slug))
                (add-text-properties 0 (length s) (list 'face 'mode-line-emphasis) s))
              (setq lines (append lines (list s))))))
        ;; footer
        (setq lines (append lines (list "" (propertize "RET open  a add  r rename  d delete  c copy  g refresh  h back  q quit  ? help" 'face 'shadow))))
        (setq context-navigator-sidebar--last-lines lines
              context-navigator-sidebar--header header)
        (context-navigator-render-apply-to-buffer (current-buffer) lines)))
     ;; Items mode: render items with ".." line
     (t
      (let* ((items (context-navigator-state-items state))
             (sorted-items
              (sort (copy-sequence (or items '()))
                    (lambda (a b)
                      (let ((na (downcase (or (context-navigator-item-name a) "")))
                            (nb (downcase (or (context-navigator-item-name b) ""))))
                        (string-lessp na nb)))))
             ;; Aim for ~55% for left column, but leave at least 10 chars for right part and 2 spaces padding.
             (left-width (max 16 (min (- total 10) (floor (* 0.55 total)))))
             (base (context-navigator-render-build-lines sorted-items header
                                                         #'context-navigator-icons-for-item
                                                         left-width))
             (hl (car base))
             (sep (cadr base))
             (rest (cddr base))
             (up (let ((s (copy-sequence "..")))
                   (add-text-properties 0 (length s)
                                        (list 'context-navigator-groups-up t
                                              'face 'shadow)
                                        s)
                   s))
             (lines (append (list hl sep up) rest)))
        ;; footer
        (setq lines (append lines (list "" (propertize "Press h to view groups, ? for help" 'face 'shadow))))
        (setq context-navigator-sidebar--last-lines lines
              context-navigator-sidebar--header header)
        (context-navigator-render-apply-to-buffer (current-buffer) lines))))))

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
        (setq-local context-navigator-render--last-hash nil))))
  (context-navigator-events-debounce
   :sidebar-render 0.06
   #'context-navigator-sidebar--render-if-visible))

(defun context-navigator-sidebar--at-item ()
  "Return item at point (from text properties) or nil."
  (let ((it (get-text-property (point) 'context-navigator-item)))
    it))

(defun context-navigator-sidebar--visit (preview)
  "Open item at point. If PREVIEW non-nil, show in other window."
  (when-let* ((item (context-navigator-sidebar--at-item)))
    (pcase (context-navigator-item-type item)
      ('file
       (let ((f (context-navigator-item-path item)))
         (when (and (stringp f) (file-exists-p f))
           (if preview
               (find-file-other-window f)
             (find-file f)))))
      ('buffer
       (let ((buf (or (context-navigator-item-buffer item)
                      (and (context-navigator-item-path item)
                           (find-file-noselect (context-navigator-item-path item))))))
         (when (buffer-live-p buf)
           (if preview
               (switch-to-buffer-other-window buf)
             (switch-to-buffer buf)))))
      ('selection
       (let ((f (context-navigator-item-path item))
             (b (context-navigator-item-beg item))
             (e (context-navigator-item-end item)))
         (when (and f (file-exists-p f) (integerp b) (integerp e))
           (if preview
               (find-file-other-window f)
             (find-file f))
           (goto-char (min b e))
           (push-mark (max b e) t t))))
      (_ (message "Unknown item")))))

(defun context-navigator-sidebar-visit ()
  "Visit item at point."
  (interactive)
  (context-navigator-sidebar--visit nil))

(defun context-navigator-sidebar-preview ()
  "Preview item at point in other window."
  (interactive)
  (context-navigator-sidebar--visit t))

(defun context-navigator-sidebar-next-item ()
  "Move point to the next item line."
  (interactive)
  (let* ((start (min (1+ (line-end-position)) (point-max)))
         (pos (text-property-not-all start (point-max)
                                     'context-navigator-item nil)))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

(defun context-navigator-sidebar-previous-item ()
  "Move point to the previous item line."
  (interactive)
  (let ((pos nil))
    (save-excursion
      (let ((done nil))
        (while (and (not done) (> (line-beginning-position) (point-min)))
          (forward-line -1)
          (when (get-text-property (point) 'context-navigator-item)
            (setq pos (point))
            (setq done t)))))
    (if pos
        (goto-char pos)
      ;; Fallback to first item if any
      (let ((first (text-property-not-all (point-min) (point-max)
                                          'context-navigator-item nil)))
        (when first (goto-char first)))))
  (beginning-of-line))

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

This updates the core model via `context-navigator-toggle-item' and then
applies the desired items to gptel. After the operation we schedule a
sidebar re-render so the visual indicator updates immediately."
  (interactive)
  (when-let* ((item (context-navigator-sidebar--at-item)))
    (let* ((key (context-navigator-model-item-key item))
           (st (ignore-errors (context-navigator-toggle-item key))) ;; update model via core API
           (idx (and (context-navigator-state-p st) (context-navigator-state-index st)))
           (it* (and idx (gethash key idx)))
           (enabled (and it* (context-navigator-item-enabled it*)))
           (items (and (context-navigator-state-p st) (context-navigator-state-items st))))
      (when (and items (boundp 'context-navigator--push-to-gptel) context-navigator--push-to-gptel)
        ;; Apply desired state to gptel (add/remove depending on enabled).
        (ignore-errors (context-navigator-gptel-apply items)))
      ;; Force the next render to apply text even if hash matched previously.
      (let ((buf (get-buffer context-navigator-sidebar--buffer-name)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq-local context-navigator-render--last-hash nil))))
      ;; Ensure the sidebar updates visually right away.
      (context-navigator-sidebar--schedule-render)
      (context-navigator-sidebar--render-if-visible)
      (message "Toggled: %s -> %s"
               (or (context-navigator-item-name item) key)
               (if enabled "enabled" "disabled")))))

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

(defun context-navigator-sidebar--install-subs ()
  "Subscribe to relevant events (buffer-local tokens).
Guard against duplicate subscriptions."
  (unless context-navigator-sidebar--subs
    (push (context-navigator-events-subscribe
           :model-refreshed
           (lambda (&rest _) (context-navigator-sidebar--schedule-render)))
          context-navigator-sidebar--subs)
    (push (context-navigator-events-subscribe
           :context-load-start
           (lambda (&rest _)
             (setq context-navigator-sidebar--load-progress nil)
             (context-navigator-sidebar--schedule-render)))
          context-navigator-sidebar--subs)
    (push (context-navigator-events-subscribe
           :context-load-step
           (lambda (_root pos total)
             (setq context-navigator-sidebar--load-progress (cons pos total))
             (context-navigator-sidebar--schedule-render)))
          context-navigator-sidebar--subs)
    (push (context-navigator-events-subscribe
           :context-load-done
           (lambda (root ok-p)
             (setq context-navigator-sidebar--load-progress nil)
             ;; Warn user if group file is unreadable/corrupted; optionally auto-open groups list.
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
          context-navigator-sidebar--subs)
    (push (context-navigator-events-subscribe
           :groups-list-updated
           (lambda (_root groups)
             (setq context-navigator-sidebar--groups groups)
             (when (eq context-navigator-sidebar--mode 'groups)
               (context-navigator-sidebar--schedule-render))))
          context-navigator-sidebar--subs)
    ;; Гарантированная отписка при убийстве буфера
    (add-hook 'kill-buffer-hook #'context-navigator-sidebar--remove-subs nil t)))

(defun context-navigator-sidebar--remove-subs ()
  "Unsubscribe buffer-local tokens."
  (when context-navigator-sidebar--subs
    (mapc #'context-navigator-events-unsubscribe context-navigator-sidebar--subs)
    (setq context-navigator-sidebar--subs nil)))

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
  "Show help for context-navigator sidebar."
  (interactive)
  (with-help-window "*Context Navigator Help*"
    (let ((pairs '((context-navigator-sidebar-next-item . "next item")
                   (context-navigator-sidebar-previous-item . "previous item")
                   (context-navigator-sidebar-activate . "RET: visit item / open group")
                   (context-navigator-sidebar-preview . "preview item (other window)")
                   (context-navigator-sidebar-toggle-enabled . "toggle enabled/disabled")
                   (context-navigator-sidebar-delete-dispatch . "delete (item or group)")
                   (context-navigator-sidebar-refresh-dispatch . "refresh (items or groups)")
                   (context-navigator-sidebar-go-up . "show groups list")
                   (context-navigator-sidebar-group-create . "add group")
                   (context-navigator-sidebar-group-rename . "rename group")
                   (context-navigator-sidebar-group-duplicate . "duplicate group")
                   (context-navigator-sidebar-toggle-push . "toggle push → gptel")
                   (context-navigator-sidebar-toggle-auto-project . "toggle auto-project switching")
                   (context-navigator-sidebar-push-now . "push now to gptel (when auto-push is off)")
                   (context-navigator-sidebar-clear-gptel . "clear gptel context")
                   (context-navigator-sidebar-quit . "quit sidebar")
                   (context-navigator-sidebar-help . "show this help"))))
      (princ "Context Navigator — keys:\n\n")
      (princ (context-navigator-sidebar--format-bindings pairs context-navigator-sidebar-mode-map))
      (princ "\n\nGlobal keys (context-navigator-mode):\n")
      (princ "C-c i n  toggle sidebar\n")
      (princ "C-c i l  load context (project/global)\n")
      (princ "C-c i S  save context\n")
      (princ "C-c i s  refresh\n")
      (princ "C-c i u  unload (clear) context\n")
      (princ "C-c i g  open groups list\n")
      (princ "\nGroups mode keys: RET open, a add, r rename, d delete, c copy, g refresh, h back, q quit\n"))))

(defvar context-navigator-sidebar-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Dispatch RET depending on mode
    (define-key m (kbd "RET") #'context-navigator-sidebar-activate)
    (define-key m (kbd "SPC") #'context-navigator-sidebar-preview)
    (define-key m (kbd "n")   #'context-navigator-sidebar-next-item)
    (define-key m (kbd "p")   #'context-navigator-sidebar-previous-item)
    (define-key m (kbd "t")   #'context-navigator-sidebar-toggle-enabled)
    ;; New global toggles/actions in sidebar
    (define-key m (kbd "x")   #'context-navigator-sidebar-toggle-push)
    (define-key m (kbd "T")   #'context-navigator-sidebar-toggle-auto-project)
    (define-key m (kbd "P")   #'context-navigator-sidebar-push-now)
    (define-key m (kbd "C")   #'context-navigator-sidebar-clear-gptel)
    ;; d and g are dispatched depending on mode
    (define-key m (kbd "d")   #'context-navigator-sidebar-delete-dispatch)
    (define-key m (kbd "g")   #'context-navigator-sidebar-refresh-dispatch)
    ;; Groups-specific keys
    (define-key m (kbd "h")   #'context-navigator-sidebar-go-up)      ;; show groups from items
    (define-key m (kbd "a")   #'context-navigator-sidebar-group-create)
    (define-key m (kbd "r")   #'context-navigator-sidebar-group-rename)
    (define-key m (kbd "c")   #'context-navigator-sidebar-group-duplicate)
    (define-key m (kbd "q")   #'context-navigator-sidebar-quit)
    (define-key m (kbd "?")   #'context-navigator-sidebar-help)
    m)
  "Keymap for =context-navigator-sidebar-mode'.")

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
                                                              32)))))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (context-navigator-sidebar-mode)
        (setq-local buffer-read-only t)
        (context-navigator-sidebar--install-subs)
        (context-navigator-sidebar--render)))
    (when (window-live-p win)
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

(defun context-navigator-sidebar-clear-gptel ()
  "Manually clear gptel context without touching the model."
  (interactive)
  (ignore-errors (context-navigator-clear-gptel-now))
  (context-navigator-sidebar--schedule-render))

;;; Dispatchers and commands

(defun context-navigator-sidebar-activate ()
  "RET action: open item (items mode) or switch to group (groups mode)."
  (interactive)
  (if (eq context-navigator-sidebar--mode 'groups)
      (or (context-navigator-sidebar--open-group-at-point)
          (message "No group at point"))
    ;; Items mode: if on ".." go to groups, otherwise visit item
    (if (get-text-property (point) 'context-navigator-groups-up)
        (context-navigator-sidebar-go-up)
      (context-navigator-sidebar-visit))))

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
      (ignore-errors (context-navigator-group-create))
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
