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
(declare-function context-navigator-sidebar-show-groups "context-navigator-sidebar" ())

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

(defcustom context-navigator-default-push-to-gptel nil
  "Default session state for pushing Navigator context to gptel."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-default-auto-project-switch nil
  "Default session state for automatic project switching."
  :type 'boolean :group 'context-navigator)

(defvar context-navigator--push-to-gptel context-navigator-default-push-to-gptel
  "Session flag: when non-nil, Navigator pushes current context to gptel.")

(defvar context-navigator--auto-project-switch context-navigator-default-auto-project-switch
  "Session flag: when non-nil, Navigator reacts to project switch events.")

(defcustom context-navigator-dir-name ".context"
  "Directory name under project root to store context."
  :type 'string :group 'context-navigator)

(defcustom context-navigator-context-file-name "context.el"
  "Context file name inside the context directory."
  :type 'string :group 'context-navigator)

(defcustom context-navigator-global-dir (expand-file-name "~/.context")
  "Global directory for context when project is not detected."
  :type 'directory :group 'context-navigator)

(defcustom context-navigator-create-default-group-file t
  "When non-nil, ensure 'default' group file exists on first load if missing.
Creates <root>/.context/default.el (or ~/.context/default.el in global mode)."
  :type 'boolean :group 'context-navigator)

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
  (last-project-root nil)
  (current-group-slug nil)
  (load-token 0))

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
   :last-project-root (context-navigator-state-last-project-root state)
   :current-group-slug (context-navigator-state-current-group-slug state)
   :load-token (context-navigator-state-load-token state)))

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
    ;; Transient entry point on C-c n
    (define-key m (kbd "C-c n") #'context-navigator-transient)
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

(defun context-navigator-toggle-push-to-gptel ()
  "Toggle session flag to push Navigator context to gptel."
  (interactive)
  (setq context-navigator--push-to-gptel (not context-navigator--push-to-gptel))
  (message "Navigator → gptel push: %s" (if context-navigator--push-to-gptel "on" "off"))
  (context-navigator-refresh))

(defun context-navigator-toggle-auto-project-switch ()
  "Toggle session flag for automatic project switching."
  (interactive)
  (setq context-navigator--auto-project-switch (not context-navigator--auto-project-switch))
  (message "Auto project switch: %s" (if context-navigator--auto-project-switch "on" "off"))
  (context-navigator-refresh))

(defun context-navigator-push-to-gptel-now ()
  "Manually push current model to gptel (reset + add enabled)."
  (interactive)
  (unless (context-navigator-gptel-available-p)
    (user-error "gptel is unavailable"))
  ;; Try a best-effort clear, then apply desired state.
  (when (fboundp 'gptel-context-remove-all)
    (ignore-errors (gptel-context-remove-all)))
  (let* ((st (context-navigator--state-get))
         (items (and st (context-navigator-state-items st))))
    (ignore-errors (context-navigator-gptel-apply (or items '())))
    (message "Pushed %d items to gptel" (length (or items '())))))

(defun context-navigator-clear-gptel-now ()
  "Manually clear gptel context (does not touch the model)."
  (interactive)
  (if (fboundp 'gptel-context-remove-all)
      (ignore-errors (gptel-context-remove-all))
    (ignore-errors (context-navigator-gptel-apply '())))
  (message "gptel context cleared"))

(defun context-navigator-switch-to-current-buffer-project ()
  "Switch Navigator to the project of the current buffer (manual)."
  (interactive)
  (let ((root (ignore-errors (context-navigator-project-current-root (current-buffer)))))
    (let ((context-navigator--auto-project-switch t))
      (context-navigator--on-project-switch root))))

(defun context-navigator--sync-from-gptel ()
  "Deprecated: Navigator no longer pulls from gptel. No-op."
  (when context-navigator-debug
    (message "[context-navigator/core] sync-from-gptel is disabled"))
  (context-navigator--state-get))
(make-obsolete 'context-navigator--sync-from-gptel nil "0.3.0")

(defun context-navigator--on-gptel-change (&rest _args)
  "Deprecated: gptel changes no longer affect Navigator. No-op."
  nil)
(make-obsolete 'context-navigator--on-gptel-change nil "0.3.0")

(defun context-navigator--load-group-for-root (root slug)
  "Load group SLUG for ROOT (or global) asynchronously and apply to gptel."
  ;; Persist only :current in state.el (без :groups).
  (let* ((st (or (context-navigator-persist-state-load root) '())))
    (setq st (if (plist-member st :version) (copy-sequence st)
               (plist-put (copy-sequence st) :version 1)))
    (setq st (plist-put st :current slug))
    (ignore-errors (context-navigator-persist-state-save root st)))
  ;; Update core state: set flags and current group; increment load-token to guard against races.
  (let* ((cur (context-navigator--state-get))
         (new (context-navigator--state-copy cur))
         (token (1+ (or (context-navigator-state-load-token new) 0))))
    (setf (context-navigator-state-inhibit-refresh new) t)
    (setf (context-navigator-state-inhibit-autosave new) t)
    (setf (context-navigator-state-loading-p new) t)
    (setf (context-navigator-state-current-group-slug new) slug)
    (setf (context-navigator-state-load-token new) token)
    (context-navigator--set-state new)
    ;; Reset gptel fully before loading a new group to avoid mixing (only when push is on).
    (when context-navigator--push-to-gptel
      (if (fboundp 'gptel-context-remove-all)
          (ignore-errors (gptel-context-remove-all))
        (ignore-errors (context-navigator-gptel-apply '()))))
    (context-navigator-events-publish :group-switch-start root slug)
    (context-navigator-persist-load-group-async
     root slug
     (lambda (items)
       ;; Ignore stale callbacks (race) by comparing tokens.
       (let* ((st (context-navigator--state-get))
              (alive (and (context-navigator-state-p st)
                          (= (or (context-navigator-state-load-token st) 0)
                             token))))
         (when alive
           (when context-navigator--push-to-gptel
             (ignore-errors (context-navigator-gptel-apply (or items '()))))
           (context-navigator-set-items (or items '())))
         ;; Clear inhibit flags and notify done.
         (let* ((cur2 (context-navigator--state-get))
                (new2 (context-navigator--state-copy cur2)))
           (setf (context-navigator-state-inhibit-refresh new2) nil)
           (setf (context-navigator-state-inhibit-autosave new2) nil)
           (setf (context-navigator-state-loading-p new2) nil)
           (context-navigator--set-state new2))
         (context-navigator-events-publish :context-load-done root (listp items))
         (context-navigator-events-publish :group-switch-done root slug (listp items)))))))

(defun context-navigator--load-context-for-root (root)
  "Load current group for ROOT (or global) using state.el.

Behavior:
- If :current is missing in state.el, set it to \"default\"
- Then delegate actual loading to `context-navigator--load-group-for-root'."
  (let* ((st (or (context-navigator-persist-state-load root) '()))
         (slug (or (plist-get st :current) "default"))
         (st* (if (plist-member st :version) (copy-sequence st)
                (plist-put (copy-sequence st) :version 1))))
    (unless (plist-member st* :current)
      (setq st* (plist-put st* :current slug))
      (ignore-errors (context-navigator-persist-state-save root st*)))
    ;; Optionally ensure default group file exists on first load
    (when (and (string= slug "default")
               (boundp 'context-navigator-create-default-group-file)
               context-navigator-create-default-group-file)
      (let ((f (ignore-errors (context-navigator-persist-context-file root slug))))
        (when (and (stringp f) (not (file-exists-p f)))
          (ignore-errors (context-navigator-persist-save '() root slug)))))
    (context-navigator--load-group-for-root root slug)))

(defun context-navigator--on-project-switch (root)
  "Handle :project-switch event with ROOT (string or nil)."
  (if (not context-navigator--auto-project-switch)
      nil
    ;; Enabled:
    ;; Always update last-project-root and log.
    (let* ((cur (context-navigator--state-get))
           (new (context-navigator--state-copy cur)))
      (setf (context-navigator-state-last-project-root new) root)
      (context-navigator--set-state new))
    (context-navigator--log "Project switch -> %s" (or root "~"))
    (if context-navigator-autoload
        (progn
          ;; Inhibit autosave/refresh during transition to avoid leaking saves.
          (let* ((cur1 (context-navigator--state-get))
                 (new1 (context-navigator--state-copy cur1)))
            (setf (context-navigator-state-inhibit-refresh new1) t)
            (setf (context-navigator-state-inhibit-autosave new1) t)
            (setf (context-navigator-state-loading-p new1) t)
            (context-navigator--set-state new1))
          ;; Clear model and (optionally) gptel before loading context of the new project.
          (context-navigator-set-items '())
          (when context-navigator--push-to-gptel
            (if (fboundp 'gptel-context-remove-all)
                (ignore-errors (gptel-context-remove-all))
              (ignore-errors (context-navigator-gptel-apply '()))))
          (context-navigator--load-context-for-root root))
      ;; When autoload is disabled, do not touch current context/model.
      nil)))

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
    ;; Update last project root via a copy to avoid direct mutation; inhibit autosave/refresh while switching.
    (let* ((cur (context-navigator--state-get))
           (new (context-navigator--state-copy cur)))
      (setf (context-navigator-state-last-project-root new) root)
      (setf (context-navigator-state-inhibit-refresh new) t)
      (setf (context-navigator-state-inhibit-autosave new) t)
      (setf (context-navigator-state-loading-p new) t)
      (context-navigator--set-state new))
    ;; Fully unload previous context (model + gptel) before loading the new one.
    (context-navigator-set-items '())
    (when context-navigator--push-to-gptel
      (if (fboundp 'gptel-context-remove-all)
          (ignore-errors (gptel-context-remove-all))
        (ignore-errors (context-navigator-gptel-apply '()))))
    (context-navigator--log "Manual load -> %s" (or root "~"))
    (context-navigator--load-context-for-root root)))

;;;###autoload
(defun context-navigator-context-save ()
  "Manually save current model items to the active group's file for the active root."
  (interactive)
  (let* ((st (context-navigator--state-get))
         (items (and st (context-navigator-state-items st)))
         (root (and st (context-navigator-state-last-project-root st)))
         (slug (and st (context-navigator-state-current-group-slug st))))
    (if (and (stringp slug) (not (string-empty-p slug)))
        (let ((file (ignore-errors (context-navigator-persist-save items root slug))))
          (if file
              (message "Context saved to %s" (abbreviate-file-name file))
            (message "Failed to save context")))
      (message "No active group — open groups list to select one"))))

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
    ;; Apply empty set to gptel (when push is on) and clear model.
    (when context-navigator--push-to-gptel
      (if (fboundp 'gptel-context-remove-all)
          (ignore-errors (gptel-context-remove-all))
        (ignore-errors (context-navigator-gptel-apply '()))))
    (context-navigator-set-items '())
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
        ;; Install project hooks
        (ignore-errors (context-navigator-project-setup-hooks))
        ;; Subscribe to events
        ;; gptel-change subscription disabled (no pull from gptel anymore)
        (push (context-navigator-events-subscribe :project-switch #'context-navigator--on-project-switch)
              context-navigator--event-tokens)
        (push (context-navigator-events-subscribe
               :model-refreshed
               (lambda (state)
                 (when (and context-navigator-autosave
                            (not (context-navigator-state-inhibit-autosave state)))
                   (let* ((root (context-navigator-state-last-project-root state))
                          (slug (context-navigator-state-current-group-slug state))
                          (items (context-navigator-state-items state)))
                     ;; Guard: do not save when no active group; avoid legacy single-file write.
                     (when (and (stringp slug) (not (string-empty-p slug)))
                       (ignore-errors
                         (context-navigator-persist-save items root slug)))))))
              context-navigator--event-tokens)
        ;; Initial gptel sync disabled (Navigator no longer pulls from gptel)
        (context-navigator--log "mode enabled"))
    ;; Teardown
    (mapc #'context-navigator-events-unsubscribe context-navigator--event-tokens)
    (setq context-navigator--event-tokens nil)
    (ignore-errors (context-navigator-project-teardown-hooks))
    (context-navigator--log "mode disabled")))

(defun context-navigator-set-items (items)
  "Replace current model ITEMS with ITEMS and publish :model-refreshed.
Prunes dead buffer items (non-live buffers). Return the new state."
  (let* ((pruned
          (cl-remove-if
           (lambda (it)
             (and (eq (context-navigator-item-type it) 'buffer)
                  (not (buffer-live-p (context-navigator-item-buffer it)))))
           (or items '())))
         (removed (- (length (or items '())) (length pruned)))
         (cur (context-navigator--state-get))
         (new (context-navigator--state-with-items (context-navigator--state-copy cur) pruned)))
    (when (> removed 0)
      (message "Removed %d dead buffer item(s)" removed))
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

;;;; Groups: list/open, switch, CRUD

(defun context-navigator--current-root ()
  "Return current root from state (or nil for global)."
  (let* ((st (context-navigator--state-get)))
    (and st (context-navigator-state-last-project-root st))))

(defun context-navigator--groups-sortless (a b)
  "Case-insensitive sort predicate by :display for group plists A and B."
  (let ((da (downcase (or (plist-get a :display) (plist-get a :slug) "")))
        (db (downcase (or (plist-get b :display) (plist-get b :slug) ""))))
    (string-lessp da db)))

(defun context-navigator--groups-candidates (root)
  "Return alist (DISPLAY . SLUG) for completing-read."
  (let* ((groups (ignore-errors (context-navigator-persist-list-groups root))))
    (mapcar (lambda (pl)
              (cons (or (plist-get pl :display) (plist-get pl :slug))
                    (plist-get pl :slug)))
            (sort groups #'context-navigator--groups-sortless))))

(defun context-navigator--state-read (root)
  "Read state plist for ROOT (or global), ensure :version present."
  (let* ((st (or (ignore-errors (context-navigator-persist-state-load root)) '())))
    (if (plist-member st :version) st
      (plist-put (copy-sequence st) :version 1))))

(defun context-navigator--state-write (root st)
  "Write state ST for ROOT (or global)."
  (ignore-errors (context-navigator-persist-state-save root st)))

;;;###autoload
(defun context-navigator-groups-open ()
  "Publish groups list for current project/global and let sidebar render it."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (groups (ignore-errors (context-navigator-persist-list-groups root))))
    (setq groups (sort (or groups '()) #'context-navigator--groups-sortless))
    (context-navigator-events-publish :groups-list-updated root groups)
    groups))

;;;###autoload
(defun context-navigator-group-switch (&optional slug)
  "Switch active group to SLUG for current project/global.

Autosaves current group before switching. Prompts when SLUG is nil."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (st (context-navigator--state-get))
         (cur-slug (and st (context-navigator-state-current-group-slug st)))
         (slug (or slug
                   (cdr (assoc (completing-read "Switch to group: "
                                                (context-navigator--groups-candidates root)
                                                nil t)
                               (context-navigator--groups-candidates root))))))
    (when (and (stringp cur-slug) (not (string-empty-p cur-slug)))
      (let* ((items (and st (context-navigator-state-items st))))
        (ignore-errors (context-navigator-persist-save items root cur-slug))))
    (context-navigator--load-group-for-root root (or slug "default"))))

(defun context-navigator--assert-unique-slug (root slug)
  "Signal error if SLUG already exists for ROOT."
  (let* ((groups (ignore-errors (context-navigator-persist-list-groups root))))
    (when (cl-find slug groups :key (lambda (pl) (plist-get pl :slug)) :test #'equal)
      (error "Group '%s' already exists" slug))))

;;;###autoload
(defun context-navigator-group-create (&optional display-name)
  "Create a new group with DISPLAY-NAME in current project/global.
Группа должна появиться в списке как файл, поэтому сразу создаём пустой файл."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (name (or display-name (read-string "New group name: ")))
         (name (string-trim name))
         (_ (when (string-empty-p name)
              (user-error "Invalid name")))
         (slug (context-navigator-persist-slugify name))
         (file (context-navigator-persist-context-file root slug)))
    (context-navigator--assert-unique-slug root slug)
    (make-directory (file-name-directory file) t)
    ;; Create empty payload file
    (ignore-errors (context-navigator-persist-save '() root slug))
    (context-navigator-groups-open)
    ;; If auto-push is on, reset gptel immediately to avoid any carry-over.
    (when context-navigator--push-to-gptel
      (if (fboundp 'gptel-context-remove-all)
          (ignore-errors (gptel-context-remove-all))
        (ignore-errors (context-navigator-gptel-apply '()))))
    ;; Immediately switch to the newly created (empty) group.
    (ignore-errors (context-navigator--load-group-for-root root slug))
    (message "Created group: %s (%s)" name slug)
    slug))

;;;###autoload
(defun context-navigator-group-rename (&optional old-slug new-display)
  "Rename an existing group OLD-SLUG to NEW-DISPLAY (re-slugified)."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (cand (context-navigator--groups-candidates root))
         (old-slug (or old-slug
                       (cdr (assoc (completing-read "Rename group: " cand nil t) cand))))
         (_ (when (equal old-slug "default") (user-error "Cannot rename 'default'")))
         (new-display (or new-display (read-string (format "New name for %s: " old-slug))))
         (new-display (string-trim new-display))
         (_ (when (string-empty-p new-display)
              (user-error "Invalid name")))
         (new-slug (context-navigator-persist-slugify new-display)))
    (unless (equal old-slug new-slug)
      (context-navigator--assert-unique-slug root new-slug))
    ;; FS: rename file if exists
    (let* ((old-file (context-navigator-persist-context-file root old-slug))
           (new-file (context-navigator-persist-context-file root new-slug)))
      (when (and (file-readable-p old-file) (not (equal old-file new-file)))
        (make-directory (file-name-directory new-file) t)
        (rename-file old-file new-file t)))
    ;; State: only adjust :current if needed
    (let* ((st (context-navigator--state-read root)))
      (when (equal (plist-get st :current) old-slug)
        (setq st (plist-put (copy-sequence st) :current new-slug))
        (context-navigator--state-write root st)))
    ;; Update core copy
    (let* ((cur (context-navigator--state-get))
           (cur* (context-navigator--state-copy cur)))
      (when (equal (context-navigator-state-current-group-slug cur*) old-slug)
        (setf (context-navigator-state-current-group-slug cur*) new-slug))
      (context-navigator--set-state cur*))
    (context-navigator-groups-open)
    (message "Renamed group: %s → %s (%s)" old-slug new-display new-slug)
    new-slug))

;;;###autoload
(defun context-navigator-group-delete (&optional slug)
  "Delete group SLUG; if active, clear context and unset current."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (cand (context-navigator--groups-candidates root))
         (slug (or slug
                   (cdr (assoc (completing-read "Delete group: " cand nil t) cand)))))
    (when (equal slug "default")
      (user-error "Cannot delete 'default'"))
    (when (yes-or-no-p (format "Delete group '%s'? " slug))
      ;; FS
      (let ((file (context-navigator-persist-context-file root slug)))
        (when (file-exists-p file)
          (ignore-errors (delete-file file))))
      ;; State: drop from :groups mapping and unset :current if needed
      (let* ((st (context-navigator--state-read root))
             (deleted-active (equal (plist-get st :current) slug))
             (groups (and (plist-member st :groups) (plist-get st :groups)))
             (groups* (and (listp groups)
                           (cl-remove-if (lambda (cell) (equal (car-safe cell) slug))
                                         groups))))
        ;; Remove slug from mapping (if mapping present)
        (when (plist-member st :groups)
          (setq st (plist-put (copy-sequence st) :groups groups*)))
        ;; If deleted group was active, unset :current and clear model/gptel
        (when deleted-active
          (setq st (plist-put (copy-sequence st) :current nil))
          ;; Clear gptel and model
          (when context-navigator--push-to-gptel
            (if (fboundp 'gptel-context-remove-all)
                (ignore-errors (gptel-context-remove-all))
              (ignore-errors (context-navigator-gptel-apply '()))))
          (context-navigator-set-items '())
          ;; Update core state field
          (let* ((cur (context-navigator--state-get))
                 (cur* (context-navigator--state-copy cur)))
            (setf (context-navigator-state-current-group-slug cur*) nil)
            (context-navigator--set-state cur*))
          ;; Show groups list
          (ignore-errors (context-navigator-sidebar-show-groups))))
      ;; Persist updated state regardless of active status
      (let* ((st-final (context-navigator--state-read root))
             ;; ensure we don't re-introduce the deleted slug if another writer raced:
             (groups-final (and (plist-member st-final :groups) (plist-get st-final :groups)))
             (groups-final* (and (listp groups-final)
                                 (cl-remove-if (lambda (cell) (equal (car-safe cell) slug))
                                               groups-final))))
        (when (plist-member st-final :groups)
          (setq st-final (plist-put (copy-sequence st-final) :groups groups-final*)))
        (when (equal (plist-get st-final :current) slug)
          (setq st-final (plist-put (copy-sequence st-final) :current nil)))
        (context-navigator--state-write root st-final))
      ;; Always refresh groups list for UI
      (context-navigator-groups-open)
      (message "Deleted group: %s" slug)
      t)))

;;;###autoload
(defun context-navigator-group-duplicate (&optional src-slug new-display)
  "Duplicate SRC-SLUG into a new group named NEW-DISPLAY (slugified)."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (cand (context-navigator--groups-candidates root))
         (src (or src-slug
                  (cdr (assoc (completing-read "Duplicate group: " cand nil t) cand))))
         (new-display (or new-display (read-string (format "New name for duplicate of %s: " src))))
         (new-display (string-trim new-display))
         (_ (when (string-empty-p new-display)
              (user-error "Invalid name")))
         (dst (context-navigator-persist-slugify new-display)))
    (context-navigator--assert-unique-slug root dst)
    ;; Copy file if exists; otherwise create empty file payload
    (let* ((src-file (context-navigator-persist-context-file root src))
           (dst-file (context-navigator-persist-context-file root dst)))
      (make-directory (file-name-directory dst-file) t)
      (if (file-readable-p src-file)
          (copy-file src-file dst-file t)
        (ignore-errors (context-navigator-persist-save '() root dst))))
    (context-navigator-groups-open)
    (message "Duplicated group %s → %s (%s)" src new-display dst)
    dst))

(provide 'context-navigator-core)
;;; context-navigator-core.el ends here
