;;; context-navigator-core.el --- Core state and commands -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Core: global state container (immutable updates), user-facing commands,
;; customization options. Minimal side-effects: module wiring (events/hooks).
;;
;; State is kept as a struct; all public mutators are pure (return new struct).
;; Only a thin setter installs new state in the global var.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)
(require 'context-navigator-events)
(require 'context-navigator-persist)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-project)

;; Compatibility shim: some Emacs/CL versions used in CI may not provide
;; `cl-copy-struct`. Provide a lightweight fallback so tests and code that
;; (perhaps transitively) call `cl-copy-struct` don't fail with
;; (void-function cl-copy-struct).
(unless (fboundp 'cl-copy-struct)
  (defun cl-copy-struct (obj)
    "Fallback compatibility for `cl-copy-struct'.
If OBJ is a `context-navigator-state', return a shallow copy via
`context-navigator--state-copy'. For vectors return `copy-sequence',
otherwise attempt `copy-tree' as a best-effort generic copy."
    (cond
     ((and (fboundp 'context-navigator-state-p)
           (context-navigator-state-p obj))
      (context-navigator--state-copy obj))
     ((vectorp obj)
      (copy-sequence obj))
     (t
      (copy-tree obj)))))

;; Forward declaration to avoid load cycle with sidebar
(declare-function context-navigator-sidebar-toggle "context-navigator-sidebar" ())

(defgroup context-navigator nil
  "Modern context manager for Emacs/gptel (functional core)."
  :group 'convenience
  :prefix "context-navigator-")

(defcustom context-navigator-auto-refresh t
  "Auto refresh sidebar/model after external changes."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-debug nil
  "Enable debug logging."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-sidebar-width 32
  "Sidebar window width in columns."
  :type 'integer :group 'context-navigator)

(defcustom context-navigator-max-filename-length 64
  "Maximum display length for file names."
  :type 'integer :group 'context-navigator)

(defcustom context-navigator-context-switch-interval 0.7
  "Throttle interval (seconds) for project context switching."
  :type 'number :group 'context-navigator)

(defcustom context-navigator-context-load-batch-size 64
  "Batch size for async context loading."
  :type 'integer :group 'context-navigator)

(defcustom context-navigator-autosave t
  "Autosave context file on model refresh (when not inhibited)."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-autoload t
  "Autoload context when switching projects."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-dir-name ".context"
  "Directory name under project root to store context."
  :type 'string :group 'context-navigator)

(defcustom context-navigator-context-file-name "context.el"
  "Context file name inside the context directory."
  :type 'string :group 'context-navigator)

(defcustom context-navigator-global-dir (expand-file-name "~/.context")
  "Global directory for context when project is not detected."
  :type 'directory :group 'context-navigator)

(cl-defstruct (context-navigator-state
               (:constructor context-navigator--state-make))
  "Global state (pure value).
Do not mutate fields in place; use helpers to return a new struct."
  (items nil :documentation "List<context-navigator-item>")
  (index (make-hash-table :test 'equal) :documentation "Key->item")
  (generation 0 :documentation "Monotonic generation number.")
  (inhibit-refresh nil)
  (inhibit-autosave nil)
  (loading-p nil)
  (last-project-root nil))

(defvar context-navigator--state
  (context-navigator--state-make)
  "Global state value. Treat as immutable; use setters that return new values.")

(defun context-navigator--state-copy (state)
  "Return a shallow copy of STATE as a new context-navigator-state struct.
This avoids depending on cl-copy-struct and keeps copying explicit."
  (context-navigator--state-make
   :items (context-navigator-state-items state)
   :index (context-navigator-state-index state)
   :generation (context-navigator-state-generation state)
   :inhibit-refresh (context-navigator-state-inhibit-refresh state)
   :inhibit-autosave (context-navigator-state-inhibit-autosave state)
   :loading-p (context-navigator-state-loading-p state)
   :last-project-root (context-navigator-state-last-project-root state)))

;; Backwards-compatible alias used in some call sites/tests.
(defalias 'copy-context-navigator-state #'context-navigator--state-copy)

(defun context-navigator--log (fmt &rest args)
  "Log debug message FMT with ARGS when `context-navigator-debug' is non-nil."
  (when context-navigator-debug
    (apply #'message (concat "[context-navigator] " fmt) args)))

(defun context-navigator--state-get ()
  "Return current global state value."
  context-navigator--state)

(defun context-navigator--set-state (new-state)
  "Install NEW-STATE as the current global state."
  (setq context-navigator--state new-state))

(defun context-navigator--state-with-items (state items)
  "Return new STATE' with ITEMS and recomputed index/generation."
  (let* ((uni (context-navigator-model-uniq items))
         (idx (context-navigator-model-build-index uni))
         (new (context-navigator--state-copy state)))
    (setf (context-navigator-state-items new) uni)
    (setf (context-navigator-state-index new) idx)
    (setf (context-navigator-state-generation new)
          (1+ (context-navigator-state-generation new)))
    new))

;;;###autoload
(defun context-navigator-refresh ()
  "Recompute indices and publish a light refresh event."
  (interactive)
  (let* ((cur (context-navigator--state-get))
         (items (context-navigator-state-items cur))
         (new (context-navigator--state-with-items
               (context-navigator--state-copy cur) items)))
    (context-navigator--set-state new)
    (context-navigator-events-publish :model-refreshed new)
    (context-navigator--log "Refreshed generation=%s"
                            (context-navigator-state-generation new))
    new))

(defvar context-navigator-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c i s") #'context-navigator-refresh)
    (define-key m (kbd "C-c i n") #'context-navigator-sidebar-toggle)
    (define-key m (kbd "C-c i l") #'context-navigator-context-load)
    (define-key m (kbd "C-c i S") #'context-navigator-context-save)
    (define-key m (kbd "C-c i u") #'context-navigator-context-unload)
    m)
  "Keymap for `context-navigator-mode'.")

(defvar context-navigator--event-tokens nil
  "Subscription tokens registered by core while the mode is enabled.")

(defvar context-navigator--suppress-apply-until nil
  "If non-nil, a time (float-time) until which applying to gptel is suppressed.")

(defun context-navigator--apply-allowed-p ()
  "Return non-nil when it is allowed to apply to gptel now."
  (let ((till context-navigator--suppress-apply-until))
    (or (null till)
        (> (float-time) till))))

(defun context-navigator--suppress-apply-for (seconds)
  "Suppress applying to gptel for SECONDS (float)."
  (setq context-navigator--suppress-apply-until
        (+ (float-time) (or seconds 1.0))))

(defun context-navigator--sync-from-gptel ()
  "Pull items from gptel (if available) and install them into the model.

Behavior:
- Items present in gptel become enabled in the model (fields prefer incoming).
- Items absent from gptel remain in the model but are marked disabled (not lost).
This keeps user's context history intact while reflecting current gptel state."
  (if (context-navigator-gptel-available-p)
      (let* ((incoming (or (context-navigator-gptel-pull) '())) ;; enabled=t
             (cur (context-navigator--state-get))
             (old (and cur (context-navigator-state-items cur)))
             (old-index (context-navigator-model-build-index (or old '())))
             (inc-index (context-navigator-model-build-index (or incoming '())))
             (keys (let (acc)
                     (maphash (lambda (k _v) (push k acc)) old-index)
                     (maphash (lambda (k _v) (push k acc)) inc-index)
                     (delete-dups acc)))
             (merged
              (delq nil
                    (mapcar
                     (lambda (k)
                       (let ((in (gethash k inc-index))
                             (ov (gethash k old-index)))
                         (cond
                          ;; Prefer incoming item (already enabled)
                          (in in)
                          ;; Otherwise keep old item but ensure it's disabled
                          (ov (context-navigator-item-create
                               :type (context-navigator-item-type ov)
                               :name (context-navigator-item-name ov)
                               :path (context-navigator-item-path ov)
                               :buffer (context-navigator-item-buffer ov)
                               :beg (context-navigator-item-beg ov)
                               :end (context-navigator-item-end ov)
                               :size (context-navigator-item-size ov)
                               :enabled nil
                               :meta (context-navigator-item-meta ov)))
                          (t nil))))
                     keys)))
             (new (context-navigator--state-with-items (copy-context-navigator-state cur) merged)))
        (context-navigator--set-state new)
        (context-navigator-events-publish :model-refreshed new)
        (context-navigator--log "Synced from gptel, items=%s gen=%s"
                                (length merged)
                                (context-navigator-state-generation new))
        new)
    (when context-navigator-debug
      (message "[context-navigator/core] gptel unavailable; skip sync"))
    nil))

(defun context-navigator--on-gptel-change (&rest args)
  "Handle :gptel-change event (debounced).
Special case:
- When OP is `gptel-context-remove-all' or :reset, mark all current model
  items as disabled (do not lose them), then schedule a sync from gptel.
Also suppress auto-apply for a short time window to avoid re-adding."
  (let* ((op (car args))
         (st (context-navigator--state-get)))
    (when (and context-navigator-auto-refresh
               (not (and st (context-navigator-state-inhibit-refresh st))))
      (if (memq op '(gptel-context-remove-all :reset))
          (progn
            ;; Suppress auto-apply for a brief window to avoid re-adding.
            (context-navigator--suppress-apply-for 1.0)
            ;; Mark all existing items disabled (history preserved).
            (let* ((old (and st (context-navigator-state-items st)))
                   (disabled
                    (mapcar (lambda (it)
                              (if (context-navigator-item-enabled it)
                                  (context-navigator-item-create
                                   :type (context-navigator-item-type it)
                                   :name (context-navigator-item-name it)
                                   :path (context-navigator-item-path it)
                                   :buffer (context-navigator-item-buffer it)
                                   :beg (context-navigator-item-beg it)
                                   :end (context-navigator-item-end it)
                                   :size (context-navigator-item-size it)
                                   :enabled nil
                                   :meta (context-navigator-item-meta it))
                                it))
                            (or old '()))))
              ;; Set new items and publish :model-refreshed.
              (context-navigator-set-items disabled))
            ;; Additionally schedule a sync from gptel (will see empty).
            (context-navigator-events-debounce
             :core-sync 0.05
             #'context-navigator--sync-from-gptel))
        ;; Generic case: just sync from gptel.
        (context-navigator-events-debounce
         :core-sync 0.05
         #'context-navigator--sync-from-gptel)))))

(defun context-navigator--load-context-for-root (root)
  "Load context for ROOT (or global) asynchronously and apply to gptel."
  ;; Kick off async load; core will apply and publish :context-load-done
  (context-navigator-persist-load-async
   root
   (lambda (items)
     ;; Inhibit autosave/refresh during apply — create a copy and mutate the copy.
     (let* ((cur (context-navigator--state-get))
            (new (context-navigator--state-copy cur)))
       (setf (context-navigator-state-inhibit-refresh new) t)
       (setf (context-navigator-state-inhibit-autosave new) t)
       (setf (context-navigator-state-loading-p new) t)
       (context-navigator--set-state new))
     (if (context-navigator--apply-allowed-p)
         (progn
           ;; Apply to gptel and mirror back to model
           (ignore-errors (context-navigator-gptel-apply (or items '())))
           (context-navigator--sync-from-gptel))
       ;; Suppressed apply: just update the model. Mark items disabled to reflect cleared gptel.
       (let* ((disabled
               (mapcar (lambda (it)
                         (if (context-navigator-item-enabled it)
                             (context-navigator-item-create
                              :type (context-navigator-item-type it)
                              :name (context-navigator-item-name it)
                              :path (context-navigator-item-path it)
                              :buffer (context-navigator-item-buffer it)
                              :beg (context-navigator-item-beg it)
                              :end (context-navigator-item-end it)
                              :size (context-navigator-item-size it)
                              :enabled nil
                              :meta (context-navigator-item-meta it))
                           it))
                       (or items '()))))
         (context-navigator-set-items disabled)))
     ;; Clear inhibit flags and notify done — again operate on a fresh copy.
     (let* ((cur2 (context-navigator--state-get))
            (new2 (context-navigator--state-copy cur2)))
       (setf (context-navigator-state-inhibit-refresh new2) nil)
       (setf (context-navigator-state-inhibit-autosave new2) nil)
       (setf (context-navigator-state-loading-p new2) nil)
       (context-navigator--set-state new2))
     (context-navigator-events-publish :context-load-done root (and items t)))))

(defun context-navigator--on-project-switch (root)
  "Handle :project-switch event with ROOT (string or nil)."
  ;; Update last project root via an immutable-style copy
  (let* ((cur (context-navigator--state-get))
         (new (context-navigator--state-copy cur)))
    (setf (context-navigator-state-last-project-root new) root)
    (context-navigator--set-state new))
  (context-navigator--log "Project switch -> %s" (or root "~"))
  (when context-navigator-autoload
    (context-navigator--load-context-for-root root)))

;;;###autoload
(defun context-navigator-context-load (&optional prompt)
  "Manually load context for current project or globally.
With PROMPT (prefix argument), prompt for a root directory; empty input = global."
  (interactive "P")
  (let* ((root (if prompt
                   (let ((dir (read-directory-name "Load context for root (empty for global): " nil nil t)))
                     (and (stringp dir)
                          (not (string-empty-p (string-trim dir)))
                          (expand-file-name dir)))
                 (ignore-errors
                   (context-navigator-project-current-root (current-buffer))))))
    ;; Update last project root via a copy to avoid direct mutation
    (let* ((cur (context-navigator--state-get))
           (new (context-navigator--state-copy cur)))
      (setf (context-navigator-state-last-project-root new) root)
      (context-navigator--set-state new))
    (context-navigator--log "Manual load -> %s" (or root "~"))
    (context-navigator--load-context-for-root root)))

;;;###autoload
(defun context-navigator-context-save ()
  "Manually save current model items to context file for the active root."
  (interactive)
  (let* ((st (context-navigator--state-get))
         (items (and st (context-navigator-state-items st)))
         (root (and st (context-navigator-state-last-project-root st)))
         (file (ignore-errors (context-navigator-persist-save items root))))
    (if file
        (message "Context saved to %s" (abbreviate-file-name file))
      (message "Failed to save context"))))

;;;###autoload
(defun context-navigator-context-unload ()
  "Unload/clear context and switch to global (nil root).
Removes all gptel context entries and resets state flags safely."
  (interactive)
  (let ((root nil))
    ;; Mark root as global (use copy to avoid direct mutation)
    (let* ((cur (context-navigator--state-get))
           (new (context-navigator--state-copy cur)))
      (setf (context-navigator-state-last-project-root new) root)
      (context-navigator--set-state new))
    ;; Inhibit autosave/refresh during apply (operate on a copy)
    (let* ((cur (context-navigator--state-get))
           (new (context-navigator--state-copy cur)))
      (setf (context-navigator-state-inhibit-refresh new) t)
      (setf (context-navigator-state-inhibit-autosave new) t)
      (setf (context-navigator-state-loading-p new) t)
      (context-navigator--set-state new))
    ;; Apply empty set, mirror back
    (ignore-errors (context-navigator-gptel-apply '()))
    (context-navigator--sync-from-gptel)
    ;; Clear inhibit flags and notify done (operate on a fresh copy)
    (let* ((cur2 (context-navigator--state-get))
           (new2 (context-navigator--state-copy cur2)))
      (setf (context-navigator-state-inhibit-refresh new2) nil)
      (setf (context-navigator-state-inhibit-autosave new2) nil)
      (setf (context-navigator-state-loading-p new2) nil)
      (context-navigator--set-state new2))
    (context-navigator-events-publish :context-load-done root nil)
    (message "Context unloaded (global mode)")))

;;;###autoload
(define-minor-mode context-navigator-mode
  "Global mode for context-navigator (lightweight).
Sets up event wiring and keybindings."
  :init-value nil
  :global t
  :keymap context-navigator-mode-map
  (if context-navigator-mode
      (progn
        ;; Install gptel advices and project hooks
        (ignore-errors (context-navigator-gptel-on-change-register))
        (ignore-errors (context-navigator-project-setup-hooks))
        ;; Subscribe to events
        (push (context-navigator-events-subscribe :gptel-change #'context-navigator--on-gptel-change)
              context-navigator--event-tokens)
        (push (context-navigator-events-subscribe :project-switch #'context-navigator--on-project-switch)
              context-navigator--event-tokens)
        (push (context-navigator-events-subscribe
               :model-refreshed
               (lambda (state)
                 (when (and context-navigator-autosave
                            (not (context-navigator-state-inhibit-autosave state)))
                   (let* ((root (context-navigator-state-last-project-root state))
                          (items (context-navigator-state-items state)))
                     (ignore-errors
                       (context-navigator-persist-save items root))))))
              context-navigator--event-tokens)
        ;; Initial sync
        (context-navigator--sync-from-gptel)
        (context-navigator--log "mode enabled"))
    ;; Teardown
    (mapc #'context-navigator-events-unsubscribe context-navigator--event-tokens)
    (setq context-navigator--event-tokens nil)
    (ignore-errors (context-navigator-gptel-on-change-unregister))
    (ignore-errors (context-navigator-project-teardown-hooks))
    (context-navigator--log "mode disabled")))

(defun context-navigator-set-items (items)
  "Replace current model ITEMS with ITEMS and publish :model-refreshed.
Return the new state."
  (let* ((cur (context-navigator--state-get))
         (new (context-navigator--state-with-items (context-navigator--state-copy cur) items)))
    (context-navigator--set-state new)
    (context-navigator-events-publish :model-refreshed new)
    new))

(defun context-navigator-add-item (item)
  "Add ITEM to the model (deduplicated by key; last wins). Return new state."
  (let* ((cur (context-navigator--state-get))
         (old (and cur (context-navigator-state-items cur)))
         (items (append old (list item))))
    (context-navigator-set-items items)))

(defun context-navigator-remove-item-by-key (key)
  "Remove item with stable KEY from the model. Return new state."
  (let* ((cur (context-navigator--state-get))
         (old (and cur (context-navigator-state-items cur)))
         (keep (cl-remove-if (lambda (it)
                               (string= (context-navigator-model-item-key it) key))
                             old)))
    (context-navigator-set-items keep)))

(defun context-navigator-toggle-item (key &optional enabled)
  "Toggle enabled flag for item with KEY. If ENABLED non-nil, set explicitly.
Return new state. If KEY not found, return current state."
  (let* ((cur (context-navigator--state-get))
         (idx (and cur (context-navigator-state-index cur)))
         (it  (and idx (gethash key idx))))
    (if (not (context-navigator-item-p it))
        cur
      (let* ((new-enabled (if (null enabled)
                              (not (context-navigator-item-enabled it))
                            (and enabled t)))
             (updated (context-navigator-item-create
                       :type (context-navigator-item-type it)
                       :name (context-navigator-item-name it)
                       :path (context-navigator-item-path it)
                       :buffer (context-navigator-item-buffer it)
                       :beg (context-navigator-item-beg it)
                       :end (context-navigator-item-end it)
                       :size (context-navigator-item-size it)
                       :enabled new-enabled
                       :meta (context-navigator-item-meta it)))
             (items (mapcar (lambda (x)
                              (if (string= (context-navigator-model-item-key x) key)
                                  updated x))
                            (context-navigator-state-items cur))))
        (context-navigator-set-items items)))))

(defun context-navigator-set-items (items)
  "Replace current model ITEMS with ITEMS and publish :model-refreshed.
Return the new state."
  (let* ((cur (context-navigator--state-get))
         (new (context-navigator--state-with-items (context-navigator--state-copy cur) items)))
    (context-navigator--set-state new)
    (context-navigator-events-publish :model-refreshed new)
    new))

(defun context-navigator-add-item (item)
  "Add ITEM to the model (deduplicated by key; last wins). Return new state."
  (let* ((cur (context-navigator--state-get))
         (old (and cur (context-navigator-state-items cur)))
         (items (append old (list item))))
    (context-navigator-set-items items)))

(defun context-navigator-remove-item-by-key (key)
  "Remove item with stable KEY from the model. Return new state."
  (let* ((cur (context-navigator--state-get))
         (old (and cur (context-navigator-state-items cur)))
         (keep (cl-remove-if (lambda (it)
                               (string= (context-navigator-model-item-key it) key))
                             old)))
    (context-navigator-set-items keep)))

(defun context-navigator-toggle-item (key &optional enabled)
  "Toggle enabled flag for item with KEY. If ENABLED non-nil, set explicitly.
Return new state. If KEY not found, return current state."
  (let* ((cur (context-navigator--state-get))
         (idx (and cur (context-navigator-state-index cur)))
         (it  (and idx (gethash key idx))))
    (if (not (context-navigator-item-p it))
        cur
      (let* ((new-enabled (if (null enabled)
                              (not (context-navigator-item-enabled it))
                            (and enabled t)))
             (updated (context-navigator-item-create
                       :type (context-navigator-item-type it)
                       :name (context-navigator-item-name it)
                       :path (context-navigator-item-path it)
                       :buffer (context-navigator-item-buffer it)
                       :beg (context-navigator-item-beg it)
                       :end (context-navigator-item-end it)
                       :size (context-navigator-item-size it)
                       :enabled new-enabled
                       :meta (context-navigator-item-meta it)))
             (items (mapcar (lambda (x)
                              (if (string= (context-navigator-model-item-key x) key)
                                  updated x))
                            (context-navigator-state-items cur))))
        (context-navigator-set-items items)))))

(provide 'context-navigator-core)
;;; context-navigator-core.el ends here
