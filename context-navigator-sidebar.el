;;; context-navigator-sidebar.el --- Sidebar UI (side window) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Lightweight, event-driven sidebar:
;; - Opens in a left side window with configurable width
;; - Subscribes to :model-refreshed and :context-load-(start|done)
;; - Renders via context-navigator-render, optional icons
;; - Minimal keymap: RET/SPC to visit/preview, d remove, g refresh, q quit
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

;; Forward declarations to avoid load cycle; core provides these.
(declare-function context-navigator--state-get "context-navigator-core")
(declare-function context-navigator-state-last-project-root "context-navigator-core" (state))
(declare-function context-navigator-state-loading-p "context-navigator-core" (state))
(declare-function context-navigator-state-items "context-navigator-core" (state))

(defconst context-navigator-sidebar--buffer-name "*context-navigator-sidebar/")

(defvar-local context-navigator-sidebar--subs nil)
(defvar-local context-navigator-sidebar--header "Context")
(defvar-local context-navigator-sidebar--last-lines nil)
(defvar-local context-navigator-sidebar--load-progress nil) ;; cons (POS . TOTAL) | nil

(defvar context-navigator-sidebar-window-params
  '((side . left) (slot . -1))
  "Default parameters for the sidebar window.")

(defun context-navigator-sidebar--header (state)
  "Compute header from STATE and local progress."
  (let* ((root (context-navigator-state-last-project-root state))
         (loading (context-navigator-state-loading-p state))
         (base (if root
                   (format "Context for [%s]" (abbreviate-file-name root))
                 "Global context"))
         (suffix
          (cond
           ((and loading context-navigator-sidebar--load-progress)
            (format "  [Loading… %d/%d]"
                    (car context-navigator-sidebar--load-progress)
                    (cdr context-navigator-sidebar--load-progress)))
           (loading "  [Loading…]")
           (t nil))))
    (concat base (or suffix ""))))

(defun context-navigator-sidebar--state-items ()
  "Get items from core state."
  (let* ((st (ignore-errors (context-navigator--state-get))))
    (and st (context-navigator-state-items st))))

(defun context-navigator-sidebar--render ()
  "Render current items into the sidebar buffer."
  (let* ((state (context-navigator--state-get))
         (items (context-navigator-state-items state))
         (header (context-navigator-sidebar--header state))
         (win (get-buffer-window (current-buffer) 'visible))
         (total (or (and win (window-body-width win))
                    (and (boundp 'context-navigator-sidebar-width)
                         (symbol-value 'context-navigator-sidebar-width))
                    32))
         ;; Aim for ~55% for left column, but leave at least 10 chars for right part and 2 spaces padding.
         (left-width (max 16 (min (- total 10) (floor (* 0.55 total)))))
         (lines (context-navigator-render-build-lines items header
                                                      #'context-navigator-icons-for-item
                                                      left-width)))
    (setq context-navigator-sidebar--last-lines lines
          context-navigator-sidebar--header header)
    (context-navigator-render-apply-to-buffer (current-buffer) lines)))

(defun context-navigator-sidebar--render-if-visible ()
  "Render sidebar if its buffer is visible."
  (when-let* ((buf (get-buffer context-navigator-sidebar--buffer-name))
              (win (get-buffer-window buf t)))
    (with-selected-window win
      (with-current-buffer buf
        (context-navigator-sidebar--render)))))

(defun context-navigator-sidebar--schedule-render ()
  "Debounced request to render the sidebar if visible."
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
           (lambda (&rest _)
             (setq context-navigator-sidebar--load-progress nil)
             (context-navigator-sidebar--schedule-render)))
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
    (let ((pairs '((context-navigator-sidebar-visit . "visit item")
                   (context-navigator-sidebar-preview . "preview item (other window)")
                   (context-navigator-sidebar--remove-at-point . "remove item from gptel context")
                   (context-navigator-sidebar-refresh . "refresh")
                   (context-navigator-sidebar-quit . "quit sidebar")
                   (context-navigator-sidebar-help . "show this help"))))
      (princ "Context Navigator — keys:\n\n")
      (princ (context-navigator-sidebar--format-bindings pairs context-navigator-sidebar-mode-map))
      (princ "\n\nGlobal keys (context-navigator-mode):\n")
      (princ "C-c i n  toggle sidebar\n")
      (princ "C-c i l  load context (project/global)\n")
      (princ "C-c i s  save context\n")
      (princ "C-c i g  refresh\n")
      (princ "C-c i u  unload (clear) context\n"))))

(defvar context-navigator-sidebar-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'context-navigator-sidebar-visit)
    (define-key m (kbd "SPC") #'context-navigator-sidebar-preview)
    (define-key m (kbd "d")   #'context-navigator-sidebar--remove-at-point) ;; явное remove
    (define-key m (kbd "g")   #'context-navigator-sidebar-refresh)
    (define-key m (kbd "q")   #'context-navigator-sidebar-quit)
    (define-key m (kbd "?")   #'context-navigator-sidebar-help)
    m)
  "Keymap for =context-navigator-sidebar-mode'.")

(define-derived-mode context-navigator-sidebar-mode special-mode "Context-Nav"
  "Major mode for context-navigator sidebar buffer."
  (buffer-disable-undo)
  (setq truncate-lines t
        cursor-type t
        mode-line-format nil)
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

(provide 'context-navigator-sidebar)
;;; context-navigator-sidebar.el ends here
