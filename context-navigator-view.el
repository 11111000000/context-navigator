;;; context-navigator-view.el --- Display modes (sidebar|buffer) and magit-like buffer open -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Display mode switcher and magit-like buffer opener:
;; - context-navigator-display-mode: 'sidebar | 'buffer
;; - context-navigator-open: open according to current display mode
;; - context-navigator-buffer-open: open Navigator in a normal window (magit-like)
;; - context-navigator-toggle-display-mode: toggle display mode
;;
;; Buffer mode reuses the existing sidebar major mode and rendering, but:
;; - does NOT mark the window with 'context-navigator-sidebar parameter
;; - does NOT use a side window; opens in another window if available,
;;   otherwise splits the current one via a customizable function.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'context-navigator-render)
(require 'context-navigator-events)
(require 'context-navigator-sidebar) ;; reuse buffer name, mode and internals

(defgroup context-navigator-view nil
  "Display mode settings for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-display-mode 'sidebar
  "Default display mode for Context Navigator: 'sidebar or 'buffer."
  :type '(choice (const sidebar) (const buffer))
  :group 'context-navigator-view)

(defcustom context-navigator-buffer-split-function #'split-window-sensibly
  "Function used to create a second window in buffer mode when only one window exists.
It should return the newly created window or nil."
  :type 'function :group 'context-navigator-view)

(defcustom context-navigator-buffer-select-window t
  "When non-nil, select the window that shows Navigator after opening in buffer mode."
  :type 'boolean :group 'context-navigator-view)

(defun context-navigator--view--ensure-setup (buf)
  "Ensure BUF is initialized with Navigator mode, subscriptions and first render."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (context-navigator-sidebar-mode)
      (setq-local buffer-read-only t)
      ;; install subs/timers/hooks idempotently
      (context-navigator-sidebar--install-subs)
      (context-navigator-sidebar--render))))

;;;###autoload
(defun context-navigator-buffer-open ()
  "Open Navigator in a regular window (magit-like):
- if a window on the selected frame already shows Navigator → select it
- else if there is another window → display Navigator there and select it
- else split the current window via `context-navigator-buffer-split-function'
and show Navigator in the new window; select it."
  (interactive)
  (let* ((buf-name (if (boundp 'context-navigator-sidebar--buffer-name)
                       (symbol-value 'context-navigator-sidebar--buffer-name)
                     "*context-navigator*"))
         (buf (get-buffer-create buf-name))
         (here (selected-window))
         (existing (get-buffer-window buf (selected-frame)))
         (target
          (cond
           ;; Reuse visible window in the current frame
           (existing existing)
           ;; Use any other window in the current frame
           ((let* ((wins (cl-remove-if (lambda (w) (eq w here))
                                       (window-list (selected-frame) 'no-mini))))
              (when wins
                (let ((w (car wins)))
                  (set-window-buffer w buf)
                  w))))
           ;; Create a second window via split function
           (t
            (let* ((split-fn (or context-navigator-buffer-split-function
                                 #'split-window-sensibly))
                   (w (or (funcall split-fn) (split-window-sensibly))))
              (when (window-live-p w)
                (set-window-buffer w buf))
              w))))))
  (context-navigator--view--ensure-setup buf)
  (when (and context-navigator-buffer-select-window
             (window-live-p target))
    (select-window target))
  target)

;;;###autoload
(defun context-navigator-open ()
  "Open Navigator according to `context-navigator-display-mode'."
  (interactive)
  (pcase context-navigator-display-mode
    ('buffer  (context-navigator-buffer-open))
    (_        (context-navigator-sidebar-open))))

;;;###autoload
(defun context-navigator-toggle-display-mode ()
  "Toggle `context-navigator-display-mode' between 'sidebar and 'buffer."
  (interactive)
  (setq context-navigator-display-mode
        (if (eq context-navigator-display-mode 'sidebar) 'buffer 'sidebar))
  (message "Context Navigator display mode: %s" context-navigator-display-mode))

(provide 'context-navigator-view)
;;; context-navigator-view.el ends here
