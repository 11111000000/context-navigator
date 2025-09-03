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
  "Compute compact header title from STATE (without toggle segments).

Title only:
- [<project>: <group>]            when group is active
- [<project>] Groups             when in groups mode
- [context-navigator]            when no root

Note: status toggles [→gptel:on/off] [auto-proj:on/off] are rendered in the footer."
  (let* ((root (context-navigator-state-last-project-root state))
         (loading (context-navigator-state-loading-p state))
         (group (context-navigator-state-current-group-slug state))
         ;; Use project basename (or a sensible default) instead of full path.
         (proj-name (if root
                        (file-name-nondirectory (directory-file-name root))
                      "context-navigator"))
         (base (if proj-name
                   (if group
                       (format "[%s: %s]" proj-name group)
                     (format "[%s]" proj-name))
                 (context-navigator-i18n :global-context)))
         (extra
          (when (eq context-navigator-sidebar--mode 'groups)
            (concat " " (context-navigator-i18n :groups))))
         (suffix
          (cond
           ((and loading context-navigator-sidebar--load-progress)
            (format "  [%s %d/%d]"
                    (context-navigator-i18n :loading)
                    (car context-navigator-sidebar--load-progress)
                    (cdr context-navigator-sidebar--load-progress)))
           (loading (format "  [%s]" (context-navigator-i18n :loading)))
           (t nil))))
    (concat base (or extra "") (or suffix ""))))

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
         (style (or context-navigator-controls-style 'auto))
         ;; label builders
         (lbl-push
          (pcase style
            ((or 'icons 'auto) " [→]")
            (_ (format " [→gptel: %s]" (if push-on "on" "off")))))
         (lbl-auto
          (pcase style
            ((or 'icons 'auto) " [A]")
            (_ (format " [auto-proj: %s]" (if auto-on "on" "off"))))))
    (let* ((seg1 (let* ((s (copy-sequence lbl-push))
                        (m (let ((km (make-sparse-keymap)))
                             (define-key km [mouse-1] #'context-navigator-sidebar-toggle-push)
                             km))
                        (fg (if push-on "green4" "gray")))
                   (add-text-properties 0 (length s)
                                        (list 'mouse-face 'highlight
                                              'help-echo (context-navigator-i18n :toggle-push)
                                              'keymap m
                                              'context-navigator-toggle 'push
                                              'face (list :foreground fg))
                                        s)
                   s))
           (seg2 (let* ((s (copy-sequence lbl-auto))
                        (m (let ((km (make-sparse-keymap)))
                             (define-key km [mouse-1] #'context-navigator-sidebar-toggle-auto-project)
                             km))
                        (fg (if auto-on "green4" "gray")))
                   (add-text-properties 0 (length s)
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


(defun context-navigator-sidebar--footer-control-segments ()
  "Build footer control segments: toggles + [Push now] and [Clear gptel].
Respects `context-navigator-controls-style' for compact icon/text labels."
  (let* ((push-on (and (boundp 'context-navigator--push-to-gptel)
                       context-navigator--push-to-gptel))
         (gptel-items (ignore-errors (context-navigator-gptel-pull)))
         (has-gptel (and (listp gptel-items) (> (length gptel-items) 0)))
         (style (or context-navigator-controls-style 'auto))
         (segs '()))
    ;; Toggles first (push → gptel, auto-project)
    (setq segs (append segs (context-navigator-sidebar--make-toggle-segments)))
    ;; [Push now] — always shown; when auto-push is ON it is inactive (visually and without keymap).
    (let* ((label (pcase style
                    ((or 'icons 'auto) " [⇪]")
                    (_ (concat " [" (context-navigator-i18n :push-now) "]"))))
           (s (copy-sequence label)))
      ;; Always mark footer segments with an explicit action property so RET handler can invoke them.
      (add-text-properties 0 (length s) (list 'context-navigator-action 'push-now) s)
      (if push-on
          ;; Inactive representation: shadowed, no keymap, hint explaining why it's disabled.
          (add-text-properties 0 (length s)
                               (list 'face 'shadow
                                     'help-echo (context-navigator-i18n :push-tip))
                               s)
        (let ((m (let ((km (make-sparse-keymap)))
                   (define-key km [mouse-1] #'context-navigator-sidebar-push-now)
                   km)))
          (add-text-properties 0 (length s)
                               (list 'mouse-face 'highlight
                                     'help-echo (context-navigator-i18n :push-tip)
                                     'keymap m)
                               s)))
      (push s segs))
    ;; [Clear gptel] — always shown; inactive (shadowed) when no entries available.
    (let* ((label (pcase style
                    ((or 'icons 'auto) " [✖]")
                    (_ (concat " [" (context-navigator-i18n :clear-gptel) "]"))))
           (s (copy-sequence label)))
      ;; Always expose an action property so RET can activate/attempt the command.
      (add-text-properties 0 (length s) (list 'context-navigator-action 'clear-gptel) s)
      (if has-gptel
          (let ((m (let ((km (make-sparse-keymap)))
                     (define-key km [mouse-1] #'context-navigator-sidebar-clear-gptel)
                     km)))
            (add-text-properties 0 (length s)
                                 (list 'mouse-face 'highlight
                                       'help-echo (context-navigator-i18n :clear-tip)
                                       'keymap m)
                                 s))
        (add-text-properties 0 (length s)
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
         (sorted-items
          (sort (copy-sequence (or items '()))
                (lambda (a b)
                  (let ((na (downcase (or (context-navigator-item-name a) "")))
                        (nb (downcase (or (context-navigator-item-name b) ""))))
                    (string-lessp na nb)))))
         (left-width (max 16 (min (- total-width 10) (floor (* 0.55 total-width)))))
         (gptel-keys (let ((lst (ignore-errors (context-navigator-gptel-pull))))
                       (when (listp lst)
                         (mapcar #'context-navigator-model-item-key lst))))
         (base (let ((context-navigator-render--gptel-keys gptel-keys))
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

(defun context-navigator-sidebar--render ()
  "Render current view (items or groups) into the sidebar buffer."
  (let* ((state (context-navigator--state-get))
         (header (context-navigator-sidebar--header state))
         (win (get-buffer-window (current-buffer) 'visible))
         (total (or (and win (window-body-width win))
                    (and (boundp 'context-navigator-sidebar-width)
                         (symbol-value 'context-navigator-sidebar-width))
                    33)))
    (cond
     ((eq context-navigator-sidebar--mode 'groups)
      (context-navigator-sidebar--render-groups state header total))
     (t
      (context-navigator-sidebar--render-items state header total)))))

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
       (let* ((f   (context-navigator-item-path item))
              (buf (or (context-navigator-item-buffer item)
                       (and (stringp f) (file-exists-p f)
                            (find-file-noselect f))))
              (b   (context-navigator-item-beg item))
              (e   (context-navigator-item-end item))
              (valid-pos (and (integerp b) (integerp e))))
         (cond
          ;; Если буфер живой: используем уже открытое окно (если есть),
          ;; иначе открываем в соседнем окне.
          ((and (bufferp buf) (buffer-live-p buf))
           (let ((win (get-buffer-window buf 0)))
             (if win
                 (select-window win)
               (switch-to-buffer-other-window buf)))
           (when valid-pos
             (goto-char (min b e))
             (push-mark (max b e) t t)))
          ;; Иначе пробуем открыть файл в соседнем окне и перейти к региону.
          ((and (stringp f) (file-exists-p f))
           (let ((win (and (get-file-buffer f)
                           (get-buffer-window (get-file-buffer f) 0))))
             (if win
                 (select-window win)
               (find-file-other-window f)))
           (when valid-pos
             (goto-char (min b e))
             (push-mark (max b e) t t)))
          (t
           (message "Cannot locate selection target")))))
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
  "Move point to the next item line or \"..\"."
  (interactive)
  (let* ((start (min (1+ (line-end-position)) (point-max)))
         (pos (context-navigator-sidebar--find-next-itemish-pos start)))
    (unless pos
      ;; wrap to the first item-ish element
      (setq pos (context-navigator-sidebar--find-next-itemish-pos (point-min))))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

(defun context-navigator-sidebar-previous-item ()
  "Move point to the previous item line or \"..\"."
  (interactive)
  (let* ((start (line-beginning-position))
         (pos (context-navigator-sidebar--find-prev-itemish-pos start)))
    (unless pos
      ;; wrap to the last item-ish element
      (setq pos (context-navigator-sidebar--find-prev-itemish-pos (point-max))))
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
    ;; gptel changes: update indicators without importing anything
    (push (context-navigator-events-subscribe
           :gptel-change
           (lambda (&rest _)
             (context-navigator-sidebar--schedule-render)))
          context-navigator-sidebar--subs)
    ;; register gptel advices (UI-only)
    (ignore-errors (context-navigator-gptel-on-change-register))
    ;; Гарантированная отписка при убийстве буфера
    (add-hook 'kill-buffer-hook #'context-navigator-sidebar--remove-subs nil t)))

(defun context-navigator-sidebar--remove-subs ()
  "Unsubscribe buffer-local tokens."
  (when context-navigator-sidebar--subs
    (mapc #'context-navigator-events-unsubscribe context-navigator-sidebar--subs)
    (setq context-navigator-sidebar--subs nil))
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
      (princ "\n")
      (princ "Global keys (context-navigator-mode):\n")
      (princ "C-c n  transient: n panel, p project, a add, g groups, s save, l load, u unload, x push, T auto, P push-now, C clear\n")
      (princ "\nGroups mode keys: RET open, a add, r rename, d delete, c copy, g refresh, h back, q quit\n"))))

(defvar context-navigator-sidebar-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Dispatch RET depending on mode
    (define-key m (kbd "RET") #'context-navigator-sidebar-activate)
    (define-key m (kbd "SPC") #'context-navigator-sidebar-preview)
    (define-key m (kbd "n")   #'context-navigator-sidebar-next-item)
    (define-key m (kbd "p")   #'context-navigator-sidebar-previous-item)
    (define-key m (kbd "t")   #'context-navigator-sidebar-toggle-enabled)

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
                                                              33)))))))
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
  "RET action:
- On toggle segments in header: toggle push/auto flags
- On footer action segments: invoke the assigned action (push/clear)
- In groups mode: open group at point
- In items mode: \"..\" goes to groups; otherwise visit item."
  (interactive)
  (let ((act (get-text-property (point) 'context-navigator-action))
        (tgl (get-text-property (point) 'context-navigator-toggle)))
    (cond
     ;; Footer actions (explicit)
     ((eq act 'push-now) (context-navigator-sidebar-push-now))
     ((eq act 'clear-gptel) (context-navigator-sidebar-clear-gptel))
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
