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
(require 'context-navigator-log)

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

;; Forward declaration to avoid load cycle with sidebar/buffer view
(declare-function context-navigator-view-open "context-navigator-view" ())
(declare-function context-navigator-view-close "context-navigator-view" ())
(declare-function context-navigator-view-toggle "context-navigator-view" ())
(declare-function context-navigator-view-show-groups "context-navigator-view" ())
(declare-function context-navigator-buffer-open "context-navigator-view" ())
(declare-function context-navigator-buffer-close "context-navigator-view" ())
(declare-function context-navigator-buffer-toggle "context-navigator-view" ())
;; Forward declarations from project module (for byte-compiler friendliness)
(declare-function context-navigator-project-current-root "context-navigator-project" (&optional buffer))
(declare-function context-navigator-project--interesting-buffer-p "context-navigator-project" (buffer))

(defgroup context-navigator nil
  "Modern context manager for Emacs/gptel (functional core)."
  :group 'convenience
  :prefix "context-navigator-")

(defcustom context-navigator-auto-refresh t
  "Auto refresh sidebar/model after external changes."
  :type 'boolean :group 'context-navigator)



(defcustom context-navigator-view-width 33
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

(defcustom context-navigator-gptel-apply-batch-size 20
  "How many items to push to gptel per tick when applying in the background."
  :type 'integer :group 'context-navigator)

(defcustom context-navigator-gptel-apply-batch-interval 0.05
  "Delay between gptel apply batches (seconds)."
  :type 'number :group 'context-navigator)

(defcustom context-navigator-gptel-require-visible-window nil
  "When non-nil, defer applying to gptel until a gptel window is visible on the current frame."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-gptel-visible-poll-interval 0.5
  "Polling interval (seconds) to check for a visible gptel window when deferring apply."
  :type 'number :group 'context-navigator)

(defcustom context-navigator-autosave t
  "Autosave context file on model refresh (when not inhibited)."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-autosave-debounce 0.5
  "Debounce interval (seconds) for autosave on model refresh.

When many :model-refreshed events occur in quick succession, autosave is
debounced to avoid excessive disk IO. The debounced callback will use the
latest state at execution time; setting this to 0 disables debouncing
(autosave will still occur but may be suppressed by other inhibit flags)."
  :type 'number :group 'context-navigator)

(defcustom context-navigator-autoload t
  "Autoload context when switching projects."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-default-push-to-gptel nil
  "Default session state for pushing Navigator context to gptel."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-default-auto-project-switch t
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

(defconst context-navigator-persist-version 3
  "Persist format version used by Context Navigator (v3).")

(defcustom context-navigator-protect-sidebar-windows t
  "When non-nil, prevent global window-balancing operations from affecting the Context Navigator sidebar.

When enabled the package will short-circuit common balancing commands (e.g. `balance-windows'
and `balance-windows-area') when the sidebar is present so the sidebar window is not resized
or removed. This is enabled by default."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-protect-buffer-windows nil
  "When non-nil, protect Navigator buffer windows (magit-like mode) from balance operations.
Disabled by default."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-display-mode 'buffer
  "How to display Navigator: 'buffer (magit-like) or 'sidebar."
  :type '(choice (const buffer) (const sidebar))
  :group 'context-navigator)

(defcustom context-navigator-remember-display-mode t
  "When non-nil, remember last chosen display mode between sessions."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-buffer-placement 'reuse-other-window
  "Placement policy for magit-like buffer mode:
- reuse-other-window : show in other window when available, else split
- split-right        : split selected window to the right
- split-below        : split selected window below"
  :type '(choice (const reuse-other-window)
                 (const split-right)
                 (const split-below))
  :group 'context-navigator)

(defcustom context-navigator-buffer-split-size 0.5
  "Size for splitting in buffer mode.
When 0<value<1 treat as fraction of current window; otherwise columns/rows."
  :type 'number :group 'context-navigator)

(defun context-navigator--sidebar-visible-p ()
  "Return non-nil when a window marked as 'sidebar displays our buffer on the current frame."
  (let ((buf (and (boundp 'context-navigator-view--buffer-name)
                  (get-buffer context-navigator-view--buffer-name))))
    (catch 'hit
      (dolist (w (window-list nil nil))
        (when (and (window-live-p w)
                   (eq (window-buffer w) buf)
                   (eq (window-parameter w 'context-navigator-view) 'sidebar))
          (throw 'hit t)))
      nil)))

(defun context-navigator--buffer-mode-visible-p ()
  "Return non-nil when Navigator buffer window (marked 'buffer) is visible on current frame."
  (let ((buf (and (boundp 'context-navigator-view--buffer-name)
                  (get-buffer context-navigator-view--buffer-name))))
    (catch 'hit
      (dolist (w (window-list nil nil))
        (when (and (window-live-p w)
                   (eq (window-buffer w) buf)
                   (eq (window-parameter w 'context-navigator-view) 'buffer))
          (throw 'hit t)))
      nil)))

(defun context-navigator--protect-balance-windows (orig-fn &rest args)
  "Advice wrapper around ORIG-FN that no-ops when the sidebar Navigator window must be protected.

Protection applies only to the sidebar display mode; Navigator in buffer mode is a normal
window and should not prevent global window-balancing commands."
  (if (and context-navigator-protect-sidebar-windows
           (context-navigator--sidebar-visible-p))
      (progn
        (context-navigator-debug :debug :core "Skipping window balancing while Navigator sidebar window is visible")
        nil)
    (apply orig-fn args)))

;; Install lightweight advices for common balancing functions when available.
(when (fboundp 'advice-add)
  (ignore-errors
    (advice-add 'balance-windows :around #'context-navigator--protect-balance-windows))
  (ignore-errors
    (advice-add 'balance-windows-area :around #'context-navigator--protect-balance-windows)))

;;;###autoload
(defun context-navigator-open ()
  "Open Navigator in the current display mode."
  (interactive)
  (pcase context-navigator-display-mode
    ('sidebar (ignore-errors (context-navigator-view-open)))
    ('buffer  (ignore-errors (context-navigator-buffer-open)))
    (_        (ignore-errors (context-navigator-buffer-open)))))

;;;###autoload
(defun context-navigator-close ()
  "Close Navigator in the current display mode."
  (interactive)
  (pcase context-navigator-display-mode
    ('sidebar (ignore-errors (context-navigator-view-close)))
    ('buffer  (ignore-errors (context-navigator-buffer-close)))
    (_        (ignore-errors (context-navigator-buffer-close)))))

;;;###autoload
(defun context-navigator-toggle ()
  "Toggle Navigator visibility in the current display mode."
  (interactive)
  (pcase context-navigator-display-mode
    ('sidebar (ignore-errors (context-navigator-view-toggle)))
    ('buffer  (ignore-errors (context-navigator-buffer-toggle)))
    (_        (ignore-errors (context-navigator-buffer-toggle)))))

;;;###autoload
(defun context-navigator-display-mode-toggle ()
  "Toggle Navigator display mode between 'buffer and 'sidebar and reopen."
  (interactive)
  (setq context-navigator-display-mode
        (if (eq context-navigator-display-mode 'buffer) 'sidebar 'buffer))
  (when (and (boundp 'context-navigator-remember-display-mode)
             context-navigator-remember-display-mode)
    (ignore-errors
      (customize-save-variable 'context-navigator-display-mode context-navigator-display-mode)))
  (ignore-errors (context-navigator-close))
  (ignore-errors (context-navigator-open))
  (message "Navigator display mode: %s" context-navigator-display-mode))

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
  "Log via centralized logger at :info level under :core topic."
  (apply #'context-navigator-debug (append (list :info :core fmt) args)))

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

(defcustom context-navigator-global-key nil
  "Global key sequence for opening the Context Navigator transient.
Example: \"C-c n\". When nil (default), nothing is bound to keep defaults unobtrusive."
  :type '(choice (const :tag "None" nil) (string :tag "Key sequence"))
  :group 'context-navigator
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'context-navigator--update-global-keybinding)
           (ignore-errors (context-navigator--update-global-keybinding)))))

(defvar context-navigator--current-global-key nil
  "Internally tracks the currently active global keybinding for the transient.")

(defun context-navigator--update-global-keybinding ()
  "Apply `context-navigator-global-key' in `context-navigator-mode-map'."
  (when (keymapp context-navigator-mode-map)
    ;; Remove old binding if present
    (when context-navigator--current-global-key
      (ignore-errors
        (define-key context-navigator-mode-map
                    (kbd context-navigator--current-global-key) nil)))
    ;; Install new binding if configured
    (if (and (stringp context-navigator-global-key)
             (not (string-empty-p context-navigator-global-key)))
        (progn
          (define-key context-navigator-mode-map
                      (kbd context-navigator-global-key) #'context-navigator-transient)
          (setq context-navigator--current-global-key context-navigator-global-key))
      (setq context-navigator--current-global-key nil))))

(defvar context-navigator-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Remap delete-other-windows to close navigator side windows first when mode enabled.
    (define-key m [remap delete-other-windows] #'context-navigator-delete-other-windows)
    ;; no default global bindings; see `context-navigator-global-key'
    m)
  "Keymap for `context-navigator-mode'.")

(defun context-navigator-delete-other-windows ()
  "Close any Context Navigator sidebar windows first; then call `delete-other-windows'.
This avoids the situation where a side window (the sidebar) becomes the only
window after `delete-other-windows' and preserves the user's intended layout.

If no sidebar windows are present, behave like `delete-other-windows'."
  (interactive)
  (let ((buf (and (boundp 'context-navigator-view--buffer-name)
                  (get-buffer context-navigator-view--buffer-name))))
    (when buf
      (dolist (w (get-buffer-window-list buf nil t))
        (when (window-live-p w)
          (delete-window w)))))
  ;; Finally, perform the normal delete-other-windows in the currently selected frame.
  (when (fboundp 'delete-other-windows)
    (call-interactively #'delete-other-windows)))

;; Apply user-defined binding (if any) on load
(ignore-errors (context-navigator--update-global-keybinding))

;; Keep keybinding in sync even when set via setq or let.
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher
   'context-navigator-global-key
   (lambda (_sym _newval _op _where)
     (ignore-errors (context-navigator--update-global-keybinding)))))

(defvar context-navigator--event-tokens nil
  "Subscription tokens registered by core while the mode is enabled.")

;; gptel background apply scheduler (batched)
(defvar context-navigator--gptel-batch-timer nil)
(defvar context-navigator--gptel-batch-queue nil)
(defvar context-navigator--gptel-batch-total 0)
(defvar context-navigator--gptel-batch-token 0)
(defvar context-navigator--gptel-visible-poll-timer nil)

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
  "Toggle session flag for automatic project switching.
When turning ON, ensure `context-navigator-mode' is enabled so the core subscribes
to :project-switch events. Then immediately switch Navigator to the project of
the most recently visited file-backed buffer (if any). If no file buffer has a
project, fall back to the first \"interesting\" buffer (file/gptel/Dired).
If still nothing is found, switch to the global context."
  (interactive)
  (setq context-navigator--auto-project-switch (not context-navigator--auto-project-switch))
  (message "Auto project switch: %s" (if context-navigator--auto-project-switch "on" "off"))
  ;; Force a quick UI refresh for toggles
  (context-navigator-refresh)
  ;; On enabling auto-project, ensure mode is on and then publish a project switch.
  (when context-navigator--auto-project-switch
    (unless (bound-and-true-p context-navigator-mode)
      (context-navigator-mode 1))
    (let ((root (ignore-errors (context-navigator--pick-root-for-autoproject))))
      (context-navigator-events-publish :project-switch root))))

(defun context-navigator-push-to-gptel-now ()
  "Manually push current model to gptel (reset + add enabled).

Graceful when gptel is absent: show an informative message and do nothing."
  (interactive)
  (if (not (context-navigator-gptel-available-p))
      (progn
        (message "gptel not available — skipping push (Navigator works fine without it)")
        nil)
    ;; Try a best-effort clear, then apply desired state.
    (when (fboundp 'gptel-context-remove-all)
      (ignore-errors (let ((inhibit-message t) (message-log-max nil)) (gptel-context-remove-all))))
    (let* ((st (context-navigator--state-get))
           (items (and st (context-navigator-state-items st))))
      (ignore-errors (context-navigator-gptel-apply (or items '())))
      (message "Pushed %d items to gptel" (length (or items '()))))))

(defun context-navigator-clear-gptel-now ()
  "Manually clear gptel context (does not touch the model)."
  (interactive)
  (if (fboundp 'gptel-context-remove-all)
      (ignore-errors (gptel-context-remove-all))
    (ignore-errors (context-navigator-gptel-apply '())))
  ;; Notify listeners that gptel was cleared so UI and other adapters can refresh.
  (ignore-errors (context-navigator-events-publish :gptel-change :cleared))
  (message "gptel context cleared"))

(defun context-navigator-switch-to-current-buffer-project ()
  "Switch Navigator to the project of the current buffer (manual)."
  (interactive)
  (let ((root (ignore-errors (context-navigator-project-current-root (current-buffer)))))
    (let ((context-navigator--auto-project-switch t))
      (context-navigator--on-project-switch root))))

(defun context-navigator--load-group-for-root (root slug)
  "Load group SLUG for ROOT (or global) asynchronously and apply to gptel (batched)."
  ;; Обновляем core-состояние раньше всего, чтобы UI сразу показал прелоадер.
  (let* ((cur (context-navigator--state-get))
         (new (context-navigator--state-copy cur))
         (token (1+ (or (context-navigator-state-load-token new) 0))))
    (setf (context-navigator-state-inhibit-refresh new) t)
    (setf (context-navigator-state-inhibit-autosave new) t)
    (setf (context-navigator-state-loading-p new) t)
    (setf (context-navigator-state-current-group-slug new) slug)
    ;; Зафиксировать корень проекта в состоянии, чтобы заголовок показывал имя проекта.
    (setf (context-navigator-state-last-project-root new) root)
    (setf (context-navigator-state-load-token new) token)
    (context-navigator--set-state new)
    ;; Сообщаем о старте загрузки ДО любых I/O — прелоадер появится мгновенно.
    (context-navigator-events-publish :context-load-start root)
    ;; Persist :current в state.el — асинхронно и только если значение реально изменилось.
    (run-at-time 0 nil
                 (lambda ()
                   (let* ((st (or (context-navigator-persist-state-load root) '()))
                          (st1 (if (plist-member st :version) (copy-sequence st)
                                 (plist-put (copy-sequence st) :version 1)))
                          (cur (plist-get st1 :current)))
                     (unless (equal cur slug)
                       (setq st1 (plist-put st1 :current slug))
                       (ignore-errors (context-navigator-persist-state-save root st1))))))
    ;; Отменяем любые предыдущие фоновые очереди для gptel.
    (context-navigator--gptel-cancel-batch)
    ;; Reset gptel полностью перед загрузкой новой группы (если push включён), асинхронно.
    (when context-navigator--push-to-gptel
      (if (fboundp 'gptel-context-remove-all)
          (run-at-time 0 nil (lambda () (ignore-errors (let ((inhibit-message t) (message-log-max nil)) (gptel-context-remove-all)))))
        (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-gptel-apply '()))))))
    (context-navigator-events-publish :group-switch-start root slug)
    (context-navigator-persist-load-group-async
     root slug
     (lambda (items)
       ;; Игнорируем устаревшие колбэки по токену.
       (let* ((st (context-navigator--state-get))
              (alive (and (context-navigator-state-p st)
                          (= (or (context-navigator-state-load-token st) 0)
                             token))))
         (when alive
           (when context-navigator--push-to-gptel
             ;; Применяем к gptel порциями в фоне. Игнорируем требование видимости окна,
             ;; чтобы автопуш работал без нажатия Push.
             (run-at-time 0 nil
                          (lambda ()
                            (let ((context-navigator-gptel-require-visible-window nil))
                              (ignore-errors
                                (context-navigator--gptel-defer-or-start (or items '()) token))))))
           ;; Модель и UI отрисовываем сразу — это даёт мгновенную отзывчивость.
           (context-navigator-set-items (or items '())))
         ;; Снимаем флаги и уведомляем завершение.
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
- If :current points to a non-existent group file, rewrite it to \"default\"
- Then delegate actual loading to `context-navigator--load-group-for-root'."
  (let* ((st (or (context-navigator-persist-state-load root) '()))
         (slug (or (plist-get st :current) "default"))
         (st* (if (plist-member st :version) (copy-sequence st)
                (plist-put (copy-sequence st) :version 1))))
    ;; Ensure :current exists (save async)
    (unless (plist-member st* :current)
      (setq st* (plist-put st* :current slug))
      (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-persist-state-save root st*)))))
    ;; If the current group file is missing/unreadable, fall back to default.
    ;; Избегаем синхронного stat на TRAMP.
    (let* ((file (ignore-errors (context-navigator-persist-context-file root slug)))
           (remote (and (stringp file) (file-remote-p file)))
           (missing (or (null file) (and (not remote) (not (file-readable-p file))))))
      (when missing
        (setq slug "default")
        (setq st* (plist-put (copy-sequence st*) :current slug))
        (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-persist-state-save root st*))))))
    ;; Ensure default group file exists when selected
    (when (and (string= slug "default")
               (boundp 'context-navigator-create-default-group-file)
               context-navigator-create-default-group-file)
      (let ((f (ignore-errors (context-navigator-persist-context-file root slug))))
        (when (and (stringp f) (not (file-exists-p f)))
          (ignore-errors (context-navigator-persist-save '() root slug)))))
    (context-navigator--load-group-for-root root slug)))

(defun context-navigator--gptel-visible-p ()
  "Return non-nil if a gptel buffer is visible in any window on the selected frame."
  (catch 'yes
    (dolist (w (window-list nil 'no-mini))
      (when (window-live-p w)
        (with-current-buffer (window-buffer w)
          (when (derived-mode-p 'gptel-mode)
            (throw 'yes t)))))
    nil))

(defun context-navigator--gptel-cancel-batch ()
  "Cancel any pending batched apply to gptel."
  (when (timerp context-navigator--gptel-batch-timer)
    (cancel-timer context-navigator--gptel-batch-timer))
  (setq context-navigator--gptel-batch-timer nil
        context-navigator--gptel-batch-queue nil
        context-navigator--gptel-batch-total 0
        context-navigator--gptel-batch-token 0)
  (when (timerp context-navigator--gptel-visible-poll-timer)
    (cancel-timer context-navigator--gptel-visible-poll-timer))
  (setq context-navigator--gptel-visible-poll-timer nil)
  t)

(defun context-navigator--gptel-start-batch (items token)
  "Start batched add of ITEMS to gptel, bound to LOAD TOKEN.
Assumes gptel has been cleared beforehand."
  (context-navigator--gptel-cancel-batch)
  (setq context-navigator--gptel-batch-queue (copy-sequence (or items '())))
  (setq context-navigator--gptel-batch-total (length context-navigator--gptel-batch-queue))
  (setq context-navigator--gptel-batch-token token)
  (let ((kick (lambda ()
                (let* ((st (context-navigator--state-get)))
                  (unless (and (context-navigator-state-p st)
                               (= (or (context-navigator-state-load-token st) 0)
                                  context-navigator--gptel-batch-token))
                    ;; token mismatch → cancel
                    (context-navigator--gptel-cancel-batch))
                  (when context-navigator--gptel-batch-queue
                    (let ((n (max 1 (or context-navigator-gptel-apply-batch-size 20)))
                          (ops 0))
                      (dotimes (_i n)
                        (when-let ((it (car context-navigator--gptel-batch-queue)))
                          (setq context-navigator--gptel-batch-queue (cdr context-navigator--gptel-batch-queue))
                          (when (context-navigator-item-enabled it)
                            (ignore-errors (context-navigator-gptel-add-one it)))
                          (setq ops (1+ ops))))
                      (when (null context-navigator--gptel-batch-queue)
                        ;; done
                        (context-navigator-events-publish :gptel-change :batch-done context-navigator--gptel-batch-total)
                        (context-navigator--gptel-cancel-batch))))))))
    (setq context-navigator--gptel-batch-timer
          (run-at-time 0 (or context-navigator-gptel-apply-batch-interval 0.05) kick))))

(defun context-navigator--gptel-defer-or-start (items token)
  "Defer batched apply until gptel window is visible when required.
Otherwise start immediately."
  (if (and context-navigator-gptel-require-visible-window
           (not (context-navigator--gptel-visible-p)))
      ;; defer and poll
      (progn
        (context-navigator--gptel-cancel-batch)
        (setq context-navigator--gptel-batch-queue (copy-sequence (or items '())))
        (setq context-navigator--gptel-batch-total (length context-navigator--gptel-batch-queue))
        (setq context-navigator--gptel-batch-token token)
        (let ((poll (lambda ()
                      (let ((st (context-navigator--state-get)))
                        (if (not (and (context-navigator-state-p st)
                                      (= (or (context-navigator-state-load-token st) 0)
                                         context-navigator--gptel-batch-token)))
                            (context-navigator--gptel-cancel-batch)
                          (when (context-navigator--gptel-visible-p)
                            (when (timerp context-navigator--gptel-visible-poll-timer)
                              (cancel-timer context-navigator--gptel-visible-poll-timer))
                            (setq context-navigator--gptel-visible-poll-timer nil)
                            (context-navigator--gptel-start-batch context-navigator--gptel-batch-queue token)))))))
          (setq context-navigator--gptel-visible-poll-timer
                (run-at-time 0 (or context-navigator-gptel-visible-poll-interval 0.5) poll))))
    ;; start now
    (context-navigator--gptel-start-batch items token)))

(defun context-navigator--on-project-switch (root)
  "Handle :project-switch event with ROOT (string or nil)."
  ;; Always update last project root for UI/header, regardless of auto-switch flag.
  (let* ((cur (context-navigator--state-get))
         (new (context-navigator--state-copy cur)))
    (setf (context-navigator-state-last-project-root new) root)
    (context-navigator--set-state new))
  (context-navigator--log "Project switch -> %s" (or root "~"))
  (when context-navigator--auto-project-switch
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
            ;; Avoid blocking UI by performing a remove-all asynchronously.
            (if (fboundp 'gptel-context-remove-all)
                (run-at-time 0 nil (lambda () (ignore-errors (let ((inhibit-message t) (message-log-max nil)) (gptel-context-remove-all)))))
              (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-gptel-apply '()))))))
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
      ;; Async clear to avoid blocking interactive manual load command.
      (if (fboundp 'gptel-context-remove-all)
          (run-at-time 0 nil (lambda () (ignore-errors (let ((inhibit-message t) (message-log-max nil)) (gptel-context-remove-all)))))
        (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-gptel-apply '()))))))
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
(defun context-navigator-context-clear-current-group ()
  "Clear all items in the active group and persist an empty context.
Also clears gptel context when push→gptel is enabled."
  (interactive)
  (let* ((st (context-navigator--state-get))
         (root (and st (context-navigator-state-last-project-root st)))
         (slug (and st (context-navigator-state-current-group-slug st))))
    (if (and (stringp slug) (not (string-empty-p slug)))
        (progn
          (context-navigator-set-items '())
          (ignore-errors (context-navigator-persist-save '() root slug))
          (when (and (boundp 'context-navigator--push-to-gptel)
                     context-navigator--push-to-gptel)
            (ignore-errors (context-navigator-clear-gptel-now)))
          (message "Cleared current group's context (%s)" slug))
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
    ;; Run asynchronously so UI commands don't block.
    (when context-navigator--push-to-gptel
      (if (fboundp 'gptel-context-remove-all)
          (run-at-time 0 nil (lambda () (ignore-errors (let ((inhibit-message t) (message-log-max nil)) (gptel-context-remove-all)))))
        (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-gptel-apply '()))))))
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
        ;; Ensure global keybinding is applied when the mode is enabled
        (ignore-errors (context-navigator--update-global-keybinding))
        ;; Install project hooks
        (ignore-errors (context-navigator-project-setup-hooks))
        ;; Subscribe to events
        ;; gptel-change subscription disabled (no pull from gptel anymore)
        (push (context-navigator-events-subscribe :project-switch #'context-navigator--on-project-switch)
              context-navigator--event-tokens)
        (push (context-navigator-events-subscribe
               :model-refreshed
               (lambda (_state)
                 ;; Debounced autosave: schedule a save using the latest state when the timer fires.
                 ;; We debounce at the module-level to coalesce many rapid model updates.
                 (when context-navigator-autosave
                   (context-navigator-events-debounce
                    :autosave
                    (or context-navigator-autosave-debounce 0.5)
                    (lambda ()
                      (let ((st (context-navigator--state-get)))
                        (when (and context-navigator-autosave
                                   (context-navigator-state-p st)
                                   (not (context-navigator-state-inhibit-autosave st)))
                          (let ((root (context-navigator-state-last-project-root st))
                                (slug (context-navigator-state-current-group-slug st))
                                (items (context-navigator-state-items st)))
                            ;; Guard: do not save when no active group; avoid legacy single-file write.
                            (when (and (stringp slug) (not (string-empty-p slug)))
                              (ignore-errors (context-navigator-persist-save items root slug)))))))))))
              context-navigator--event-tokens)
        ;; Initial gptel sync disabled (Navigator no longer pulls from gptel)
        ;; If auto-project is already ON, trigger an initial project switch immediately.
        (when context-navigator--auto-project-switch
          (let ((root (ignore-errors (context-navigator--pick-root-for-autoproject))))
            (context-navigator-events-publish :project-switch root)))
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
    ;; Immediate autosave on any change (in addition to debounced autosave):
    (when (and context-navigator-autosave
               (context-navigator-state-p new)
               (not (context-navigator-state-inhibit-autosave new)))
      (let ((root (context-navigator-state-last-project-root new))
            (slug (context-navigator-state-current-group-slug new))
            (items (context-navigator-state-items new)))
        ;; Save only when a named group is active; avoid legacy single-file writes.
        (when (and (stringp slug) (not (string-empty-p slug)))
          (ignore-errors (context-navigator-persist-save items root slug)))))
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
  "Publish groups list for current project/global and let sidebar render it.

Fallback: when no last project root is recorded yet, try to detect the
current buffer's project root on-the-fly so the groups from the active
project are listed even if auto-project switching is off.

Additionally, if the context directory is missing/empty and
`context-navigator-create-default-group-file' is non-nil, initialize a
default group on demand (create .context, default.el, and state.el)."
  (interactive)
  (let* ((root (or (context-navigator--current-root)
                   (ignore-errors (context-navigator-project-current-root (current-buffer)))))
         (groups (ignore-errors (context-navigator-persist-list-groups root))))
    ;; Обновим last-project-root в состоянии, чтобы заголовок показывал имя проекта,
    ;; даже если авто-переключение проекта выключено и root определён налёту.
    (let* ((cur (context-navigator--state-get)))
      (when (not (equal (context-navigator-state-last-project-root cur) root))
        (let ((new (context-navigator--state-copy cur)))
          (setf (context-navigator-state-last-project-root new) root)
          (context-navigator--set-state new))))
    ;; Auto-initialize default group if nothing exists yet.
    (when (and context-navigator-create-default-group-file
               (or (null groups) (= (length groups) 0)))
      (let* ((default-slug "default"))
        ;; Ensure directory and default file
        (ignore-errors (context-navigator-persist-save '() root default-slug))
        ;; Ensure state has :current set and mapping contains default
        (let* ((st (context-navigator--state-read root))
               (alist (and (plist-member st :groups) (plist-get st :groups)))
               (alist* (cons (cons default-slug "default")
                             (and (listp alist)
                                  (cl-remove-if (lambda (cell) (equal (car-safe cell) default-slug)) alist)))))
          (setq st (plist-put (copy-sequence st) :current default-slug))
          (when (plist-member st :groups)
            (setq st (plist-put (copy-sequence st) :groups alist*)))
          (context-navigator--state-write root st))))
    (setq groups (ignore-errors (context-navigator-persist-list-groups root)))
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
Группа должна появиться в списке как файл, поэтому сразу создаём пустой файл.

Корень проекта определяется как:
- текущий сохранённый root в состоянии, либо
- текущий проект буфера (fallback), если root ещё не зафиксирован."
  (interactive)
  (let* ((root (or (context-navigator--current-root)
                   (ignore-errors (context-navigator-project-current-root (current-buffer)))))
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
    ;; Update core state with the resolved root so header/ops reflect it.
    (let* ((cur (context-navigator--state-get))
           (new (context-navigator--state-copy cur)))
      (setf (context-navigator-state-last-project-root new) root)
      (context-navigator--set-state new))
    (context-navigator-groups-open)
    ;; If auto-push is on, reset gptel immediately to avoid any carry-over.
    ;; Run asynchronously to avoid any hiccup in group creation UI.
    (when context-navigator--push-to-gptel
      (if (fboundp 'gptel-context-remove-all)
          (run-at-time 0 nil (lambda () (ignore-errors (let ((inhibit-message t) (message-log-max nil)) (gptel-context-remove-all)))))
        (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-gptel-apply '()))))))
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
(defun context-navigator-group-delete (&optional slug no-confirm)
  "Delete group SLUG; if active or referenced, switch :current to \"default\".
When NO-CONFIRM is non-nil or Emacs runs in batch mode (noninteractive), do not prompt."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (cand (context-navigator--groups-candidates root))
         (slug (or slug
                   (cdr (assoc (completing-read "Delete group: " cand nil t) cand)))))
    (when (equal slug "default")
      (user-error "Cannot delete 'default'"))
    (let ((need-confirm (and (not noninteractive) (not no-confirm))))
      (when (or (not need-confirm)
                (yes-or-no-p (format "Delete group '%s'? " slug)))
        ;; FS: delete file immediately — the single source of truth
        (let ((file (context-navigator-persist-context-file root slug)))
          (when (file-exists-p file)
            (ignore-errors (delete-file file))))
        ;; State: drop from :groups mapping and adjust :current
        (let* ((st (context-navigator--state-read root))
               (deleted-active (equal (plist-get st :current) slug))
               (groups (and (plist-member st :groups) (plist-get st :groups)))
               (groups* (and (listp groups)
                             (cl-remove-if (lambda (cell) (equal (car-safe cell) slug))
                                           groups))))
          ;; Remove slug from mapping (if mapping present)
          (when (plist-member st :groups)
            (setq st (plist-put (copy-sequence st) :groups groups*)))
          ;; If :current points at the deleted group, rewrite it to default
          (when (equal (plist-get st :current) slug)
            (setq st (plist-put (copy-sequence st) :current "default")))
          ;; Ensure default group file exists if we point to it
          (when (and (equal (plist-get st :current) "default")
                     (boundp 'context-navigator-create-default-group-file)
                     context-navigator-create-default-group-file)
            (let ((df (ignore-errors (context-navigator-persist-context-file root "default"))))
              (when (and (stringp df) (not (file-exists-p df)))
                (ignore-errors (context-navigator-persist-save '() root "default")))))
          (context-navigator--state-write root st)
          ;; If the deleted group was active, immediately load default
          (when deleted-active
            (ignore-errors (context-navigator--load-group-for-root root "default")))))))
  ;; Always refresh groups list for UI
  (context-navigator-groups-open)
  (message "Deleted group: %s" slug)
  t)

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

(defun context-navigator--pick-root-for-autoproject ()
  "Return a project root for immediate auto-project activation.
Strategy:
- Prefer the most recently visited file-backed buffer's project.
- Fallback to the most recent \"interesting\" buffer (file/gptel/Dired).
- As a last resort, return nil (global context)."
  (let* ((buf-file
          (cl-find-if (lambda (b) (with-current-buffer b buffer-file-name))
                      (buffer-list)))
         (root-file (and buf-file (context-navigator-project-current-root buf-file))))
    (or root-file
        (let* ((buf-any (cl-find-if #'context-navigator-project--interesting-buffer-p
                                    (buffer-list))))
          (and buf-any (context-navigator-project-current-root buf-any))))))

(defun context-navigator--reinit-after-reload ()
  "Reinstall hooks/subscriptions when file is reloaded and mode is ON.
Also triggers an immediate project switch so header shows actual project."
  (when (bound-and-true-p context-navigator-mode)
    ;; Unsubscribe only our own tokens; do not reset the global event bus
    ;; to avoid breaking other modules’ subscriptions (logs, UI, etc.).
    (mapc #'context-navigator-events-unsubscribe context-navigator--event-tokens)
    (setq context-navigator--event-tokens nil)
    ;; Ensure project hooks installed
    (ignore-errors (context-navigator-project-setup-hooks))
    ;; Re-subscribe core listeners
    (push (context-navigator-events-subscribe :project-switch #'context-navigator--on-project-switch)
          context-navigator--event-tokens)
    (push (context-navigator-events-subscribe
           :model-refreshed
           (lambda (_state)
             ;; Debounced autosave (latest state at execution time)
             (when context-navigator-autosave
               (context-navigator-events-debounce
                :autosave
                (or context-navigator-autosave-debounce 0.5)
                (lambda ()
                  (let ((st (context-navigator--state-get)))
                    (when (and context-navigator-autosave
                               (context-navigator-state-p st)
                               (not (context-navigator-state-inhibit-autosave st)))
                      (let ((root (context-navigator-state-last-project-root st))
                            (slug (context-navigator-state-current-group-slug st))
                            (items (context-navigator-state-items st)))
                        (when (and (stringp slug) (not (string-empty-p slug)))
                          (ignore-errors (context-navigator-persist-save items root slug)))))))))))
          context-navigator--event-tokens)
    ;; Trigger initial project switch so UI/header reflect actual project
    (when context-navigator--auto-project-switch
      (let ((root (ignore-errors (context-navigator--pick-root-for-autoproject))))
        (context-navigator-events-publish :project-switch root)))))

;; Auto-reinit after reload (eval-buffer/byte-compile)
(context-navigator--reinit-after-reload)

(provide 'context-navigator-core)
;;; context-navigator-core.el ends here
