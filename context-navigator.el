;;; context-navigator.el --- Modern context manager for Emacs/gptel -*- lexical-binding: t; -*-

;; Version: 1.0.0
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "30.1") (gptel "0.8.0") (transient "0.4.0"))
;; Homepage: https://github.com/gptel-extensions/context-navigator
;; Keywords: tools, convenience, ai

;;; Commentary:
;;
;; context-navigator provides a modern context manager and sidebar UI for gptel.
;; It shows the current context (files, buffers, selections) in a tree, lets
;; you navigate/preview items, auto-refreshes when the gptel context changes,
;; and can serialize/auto-load context per project.
;;
;; Key features:
;; - Sidebar (left side) with tree-widget items: files, buffers, selections
;; - Navigation (RET) and preview (SPC)
;; - Auto-refresh via advices on gptel-context mutators
;; - Per-project context serialization and auto-load/save mode
;; - Optional icons via pro-tabs and all-the-icons
;;
;; Non-goals:
;; - No heavy refresh on every post-command. Only on explicit context changes
;;   and project switches (throttled).
;;
;; See commands:
;; - context-navigator-toggle / context-navigator-show / context-navigator-quit
;; - context-navigator-refresh
;; - context-navigator-menu (transient)
;; - context-navigator-mode (global minor mode)
;; - context-navigator-context-save / load
;; - context-navigator-autoload (option)
;;
;; Enjoy!  🚀

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'wid-edit)
(require 'tree-widget)
(require 'transient)
(require 'gptel)
(require 'gptel-context)
(require 'subr-x)

;; Optional deps loaded lazily/safely
;; - all-the-icons
;; - pro-tabs

;; ----------------------------------------------------------------------------
;; Customization
;; ----------------------------------------------------------------------------
(defgroup context-navigator nil
  "Modern context manager for gptel."
  :group 'convenience
  :group 'gptel
  :prefix "context-navigator-")

(defcustom context-navigator-sidebar-width 35
  "Width of the context-navigator sidebar window."
  :type 'integer
  :group 'context-navigator)

(defcustom context-navigator-auto-refresh t
  "Automatically refresh sidebar when gptel context changes."
  :type 'boolean
  :group 'context-navigator)

(defcustom context-navigator-debug nil
  "When non-nil, emit debug messages (timings, queue sizes) to *Messages*."
  :type 'boolean
  :group 'context-navigator)

(defcustom context-navigator-show-line-numbers nil
  "Show line numbers for buffer selections in context."
  :type 'boolean
  :group 'context-navigator)

(defcustom context-navigator-autosave t
  "When non-nil, automatically save the project/global gptel context to disk when it changes.
This installs lightweight advice on gptel-context mutators. If
`context-navigator-autoload' is enabled, this option is effectively
on regardless of its value."
  :type 'boolean
  :group 'context-navigator
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'context-navigator--setup-project-context-save-advice)
           (if val
               (context-navigator--setup-project-context-save-advice)
             (unless (bound-and-true-p context-navigator-autoload)
               (when (fboundp 'context-navigator--teardown-project-context-save-advice)
                 (context-navigator--teardown-project-context-save-advice)))))))

(defcustom context-navigator-autoload t
  "When non-nil, automatically load the per-project/global gptel context
for the current buffer when switching buffers or projects.

Enabling this also ensures autosave advice is active regardless of
`context-navigator-autosave'."
  :type 'boolean
  :group 'context-navigator
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (when (fboundp 'context-navigator--setup-autoload-hooks)
               (context-navigator--setup-autoload-hooks))
           (when (fboundp 'context-navigator--teardown-autoload-hooks)
             (context-navigator--teardown-autoload-hooks))
           (unless (bound-and-true-p context-navigator-autosave)
             (when (fboundp 'context-navigator--teardown-project-context-save-advice)
               (context-navigator--teardown-project-context-save-advice))))))




(defcustom context-navigator-max-filename-length 25
  "Maximum length for displayed item names in sidebar."
  :type 'integer
  :group 'context-navigator)

(defcustom context-navigator-enable-icons t
  "When non-nil, show icons in the sidebar (pro-tabs / all-the-icons if available)."
  :type 'boolean
  :group 'context-navigator)

(defcustom context-navigator-icons-disable-on-remote t
  "When non-nil, do not use all-the-icons for remote (TRAMP) files/buffers."
  :type 'boolean
  :group 'context-navigator)

(defcustom context-navigator-dir-name ".context"
  "Service directory in project root for storing serialized context."
  :type 'string
  :group 'context-navigator)

(defcustom context-navigator-context-file-name "context.el"
  "Filename to store serialized project/global context."
  :type 'string
  :group 'context-navigator)

(defcustom context-navigator-global-dir (expand-file-name "~/.context/")
  "Global directory to store context when outside of a project."
  :type 'directory
  :group 'context-navigator)

(defcustom context-navigator-context-switch-interval 0.7
  "Minimum interval (seconds) between automatic project context switches."
  :type 'number
  :group 'context-navigator)

(defcustom context-navigator-context-load-batch-size 8
  "How many context entries to load per timer tick during async loading."
  :type 'integer
  :group 'context-navigator)

;; ----------------------------------------------------------------------------
;; Data model and state
;; ----------------------------------------------------------------------------

(cl-defstruct context-navigator-item
  type          ; 'file | 'buffer | 'selection
  name          ; string to display
  path          ; absolute file path (for files)
  buffer        ; buffer object (for buffers/selections)
  start         ; selection start (selection only)
  end           ; selection end (selection only)
  size          ; size in chars/bytes
  icon          ; icon string
  enabled       ; boolean: non-nil means included in GPTel context
  description)  ; additional info

(defvar context-navigator--state
  (list :sidebar-buffer nil
        :sidebar-window nil
        :context-items '()
        :model-items '()
        :model-index (make-hash-table :test 'equal)
        :selected-item nil
        :refresh-timer nil
        :advices-installed nil
        :current-project-root nil
        :last-refresh-echo 0.0
        :context-loading-p nil
        :context-load-timer nil
        :context-load-queue nil
        :context-load-acc nil)
  "Global state (plist) for context-navigator.")

;; Simple icon caches to avoid heavy computation on every refresh
(defvar context-navigator--mode-icon-cache (make-hash-table :test 'eq)
  "Cache of all-the-icons icons keyed by major mode symbol.")
(defvar context-navigator--ext-icon-cache (make-hash-table :test 'equal)
  "Cache of all-the-icons file icons keyed by file extension (lowercased) or basename.")

;;;###autoload
(defun context-navigator-clear-icon-cache ()
  "Clear cached icons. Useful after theme/icon pack changes."
  (interactive)
  (clrhash context-navigator--mode-icon-cache)
  (clrhash context-navigator--ext-icon-cache)
  (when (called-interactively-p 'interactive)
    (message "context-navigator: icon caches cleared")))

(defvar context-navigator--inhibit-refresh nil
  "Non-nil to temporarily inhibit auto refresh.")

(defvar context-navigator--in-context-load nil
  "Non-nil while loading project context (suppresses recursion/auto-save).")

(defvar context-navigator--hooks-installed nil
  "Internal: whether project-switch hooks are installed.")

(defvar context-navigator--last-project-root nil
  "Last project root for which context was auto-loaded.")

(defvar context-navigator--last-context-switch-time 0
  "Timestamp (float-time) of last context switch.")

(defvar context-navigator--sidebar-open-global nil
  "Non-nil means the navigator sidebar should be visible in all frames/tabs.")

(defvar context-navigator--visibility-hooks-installed nil
  "Internal: whether visibility sync hooks are installed.")

(defvar context-navigator--in-window-change-hook nil
  "Reentrancy guard for context-navigator--window-buffer-change-hook.")

(defun context-navigator--state-get (key)
  "Get KEY from the global state."
  (plist-get context-navigator--state key))

(defun context-navigator--state-put (key value)
  "Set KEY in the global state to VALUE."
  (setq context-navigator--state (plist-put context-navigator--state key value)))

;; ----------------------------------------------------------------------------
;; Model (source of truth): items with enabled/disabled status
;; ----------------------------------------------------------------------------

(defun context-navigator--item-key (item)
  "Return a stable key for ITEM used in the model/index."
  (pcase (context-navigator-item-type item)
    ('file
     (format "file:%s"
             (expand-file-name (or (context-navigator-item-path item)
                                   (context-navigator-item-name item)
                                   ""))))
    ('buffer
     (let* ((buf (context-navigator-item-buffer item))
            (bname (and (buffer-live-p buf) (buffer-name buf)))
            (bpath (and (buffer-live-p buf)
                        (with-current-buffer buf
                          (and buffer-file-name (expand-file-name buffer-file-name))))))
       (format "buf:%s:%s" (or bname "") (or bpath ""))))
    ('selection
     (let* ((buf (context-navigator-item-buffer item))
            (bname (and (buffer-live-p buf) (buffer-name buf)))
            (s (or (context-navigator-item-start item) 0))
            (e (or (context-navigator-item-end item) 0)))
       (format "sel:%s:%s-%s" (or bname "") s e)))
    (_ (format "unknown:%s" (context-navigator-item-name item)))))

(defun context-navigator--model-index-get ()
  "Return the model index hash-table, creating it if missing."
  (or (context-navigator--state-get :model-index)
      (let ((ht (make-hash-table :test 'equal)))
        (context-navigator--state-put :model-index ht)
        ht)))

(defun context-navigator--model-items ()
  "Return the current list of model items."
  (context-navigator--state-get :model-items))

(defun context-navigator--model-set-items (items)
  "Replace model items with ITEMS and rebuild index without coercing :enabled."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (it items)
      (puthash (context-navigator--item-key it) it ht))
    (context-navigator--state-put :model-items items)
    (context-navigator--state-put :model-index ht)
    items))

(defun context-navigator--model-put (item &optional enabled)
  "Insert or update ITEM in the model.
ENABLED can be:
- :enabled   (force t)
- :disabled  (force nil)
- t          (force t)
- nil        (keep current value)
Returns the stored item object."
  (let* ((current (context-navigator-item-enabled item))
         (en (cond
              ((eq enabled :enabled) t)
              ((eq enabled :disabled) nil)
              ((eq enabled t) t)
              ((eq enabled nil) current)
              (t enabled)))
         (it (if (eq en current)
                 item
               (let ((copy (copy-context-navigator-item item)))
                 (setf (context-navigator-item-enabled copy) en)
                 copy)))
         (key (context-navigator--item-key it))
         (ht (context-navigator--model-index-get))
         (cur (gethash key ht)))
    (if cur
        ;; Replace in-place in list
        (let* ((lst (context-navigator--model-items))
               (new (mapcar (lambda (x)
                              (if (string= (context-navigator--item-key x) key) it x))
                            lst)))
          (context-navigator--state-put :model-items new)
          (puthash key it ht)
          it)
      ;; Append to the end to preserve perceived order
      (let* ((lst (context-navigator--model-items))
             (new (nconc (copy-sequence lst) (list it))))
        (context-navigator--state-put :model-items new)
        (puthash key it ht)
        it))))

(defun context-navigator--model-get (key)
  "Get model item by KEY."
  (gethash key (context-navigator--model-index-get)))

(defun context-navigator--model-del (key)
  "Delete model item by KEY."
  (let* ((ht (context-navigator--model-index-get))
         (lst (context-navigator--model-items)))
    (remhash key ht)
    (context-navigator--state-put
     :model-items
     (cl-remove-if (lambda (x) (string= (context-navigator--item-key x) key)) lst))))

(defun context-navigator--ensure-model-seeded ()
  "Seed the model from current gptel context if the model is empty."
  (when (null (context-navigator--model-items))
    (let* ((items (ignore-errors (context-navigator--collector-gptel (current-buffer)))))
      (when items
        (let ((with-en
               (mapcar (lambda (it)
                         (let ((copy (copy-context-navigator-item it)))
                           (setf (context-navigator-item-enabled copy) t)
                           copy))
                       items)))
          (context-navigator--model-set-items with-en))))))

(defun context-navigator--alist-entry->pairs (entry)
  "Expand a gptel CONTEXT entry to a list of (KEY . PARAMS) pairs.
PARAMS is a plist describing how to remove/add the entry."
  (pcase entry
    (`(,(and buf (pred bufferp)) . ,ovs)
     (let (pairs)
       (dolist (ov ovs)
         (when (and (overlayp ov) (overlay-start ov) (overlay-end ov))
           (let* ((s (overlay-start ov))
                  (e (overlay-end ov))
                  (whole-buf (with-current-buffer buf
                               (and (= s (point-min)) (= e (point-max))))))
             (if whole-buf
                 (push (cons (format "buf:%s:%s"
                                     (buffer-name buf)
                                     (or (with-current-buffer buf
                                           (and buffer-file-name (expand-file-name buffer-file-name)))
                                         ""))
                             (list :type 'buffer :buffer buf))
                       pairs)
               (push (cons (format "sel:%s:%s-%s" (buffer-name buf) s e)
                           (list :type 'selection :buffer buf :start s :end e))
                     pairs)))))
       (nreverse pairs)))
    (`(,(and path (pred stringp)) . ,_props)
     (list (cons (format "file:%s" (expand-file-name path))
                 (list :type 'file :path path))))
    (`(,path)
     (list (cons (format "file:%s" (expand-file-name path))
                 (list :type 'file :path path))))
    (_ nil)))

(defun context-navigator--apply-model-to-gptel ()
  "Ensure gptel context matches enabled items in the model."
  (let ((gtbuf (context-navigator--find-gptel-buffer)))
    (if (not (and gtbuf (buffer-live-p gtbuf)))
        ;; Нет активного GPTel-буфера — обновим только UI; GPTel синхронизируется позже.
        (context-navigator--auto-refresh)
      (with-current-buffer gtbuf
        (let* ((alist (context-navigator--context-alist))
               (gptel-map (make-hash-table :test 'equal))
               (desired (cl-remove-if-not
                         (lambda (it) (context-navigator-item-enabled it))
                         (context-navigator--model-items)))
               (desired-map (make-hash-table :test 'equal)))
          ;; Build current map from gptel alist
          (dolist (entry alist)
            (dolist (pair (context-navigator--alist-entry->pairs entry))
              (puthash (car pair) (cdr pair) gptel-map)))
          ;; Build desired map from model
          (dolist (it desired)
            (puthash (context-navigator--item-key it) it desired-map))
          (let ((context-navigator--in-context-load t)
                (context-navigator--inhibit-refresh t))
            ;; Remove extras
            (maphash
             (lambda (_k v)
               (unless (gethash (pcase (plist-get v :type)
                                  ('file (format "file:%s" (expand-file-name (plist-get v :path))))
                                  ('buffer (format "buf:%s:%s"
                                                   (and (buffer-live-p (plist-get v :buffer))
                                                        (buffer-name (plist-get v :buffer)))
                                                   (with-current-buffer (plist-get v :buffer)
                                                     (or (and buffer-file-name (expand-file-name buffer-file-name)) ""))))
                                  ('selection (format "sel:%s:%s-%s"
                                                      (and (buffer-live-p (plist-get v :buffer))
                                                           (buffer-name (plist-get v :buffer)))
                                                      (plist-get v :start)
                                                      (plist-get v :end))))
                                desired-map)
                 (pcase (plist-get v :type)
                   ('file
                    (when (fboundp 'gptel-context-remove)
                      (ignore-errors (gptel-context-remove (plist-get v :path)))))
                   ('buffer
                    (when (fboundp 'gptel-context-remove)
                      (ignore-errors (gptel-context-remove (plist-get v :buffer)))))
                   ('selection
                    (when (fboundp 'gptel-context-remove)
                      (ignore-errors
                        (gptel-context-remove (plist-get v :buffer)
                                              (plist-get v :start)
                                              (plist-get v :end))))))))
             gptel-map)
            ;; Add missing
            (maphash
             (lambda (_k it)
               (unless (gethash (context-navigator--item-key it) gptel-map)
                 (pcase (context-navigator-item-type it)
                   ('file
                    (let ((path (context-navigator-item-path it)))
                      (when (and path (fboundp 'gptel-context-add-file))
                        (ignore-errors (gptel-context-add-file path)))))
                   ('buffer
                    (let ((buf (context-navigator-item-buffer it)))
                      (when (and (buffer-live-p buf)
                                 (fboundp 'gptel-context--add-region))
                        (with-current-buffer buf
                          (ignore-errors
                            (gptel-context--add-region buf (point-min) (point-max) t))))))
                   ('selection
                    (let ((buf (context-navigator-item-buffer it))
                          (s (context-navigator-item-start it))
                          (e (context-navigator-item-end it)))
                      (when (and (buffer-live-p buf) s e (fboundp 'gptel-context--add-region))
                        (ignore-errors (gptel-context--add-region buf s e t))))))))
             desired-map)))))
    (context-navigator--auto-refresh)))

(defun context-navigator--sync-model-from-gptel ()
  "Reflect current gptel context into the model (enabled = present in gptel)."
  (let* ((gtbuf (context-navigator--find-gptel-buffer))
         (items (if (and gtbuf (buffer-live-p gtbuf))
                    (with-current-buffer gtbuf
                      (ignore-errors (context-navigator--collector-gptel (current-buffer))))
                  (ignore-errors (context-navigator--collector-gptel (current-buffer)))))
         (present (make-hash-table :test 'equal)))
    ;; Mark/add enabled
    (dolist (it (or items '()))
      (let ((copy (copy-context-navigator-item it)))
        (setf (context-navigator-item-enabled copy) t)
        (puthash (context-navigator--item-key copy) t present)
        (context-navigator--model-put copy :enabled)))
    ;; Mark missing ones disabled
    (dolist (it (copy-sequence (context-navigator--model-items)))
      (let ((k (context-navigator--item-key it)))
        (unless (gethash k present)
          (let ((copy (copy-context-navigator-item it)))
            (setf (context-navigator-item-enabled copy) nil)
            (context-navigator--model-put copy :disabled)))))))

(defun context-navigator--advice-sync-model (&rest _)
  "Advice target: sync internal model with gptel after any mutation."
  (context-navigator--sync-model-from-gptel)
  (context-navigator--auto-refresh))

(defun context-navigator--cancel-context-load ()
  "Cancel any ongoing async context load timer and clear flags."
  (when-let ((tm (context-navigator--state-get :context-load-timer)))
    (when (timerp tm)
      (cancel-timer tm)))
  (context-navigator--state-put :context-load-timer nil)
  (context-navigator--state-put :context-load-queue nil)
  (context-navigator--state-put :context-loading-p nil))

(defun context-navigator--process-context-load-queue ()
  "Process a batch from the async context load queue."
  (let* ((batch (max 1 context-navigator-context-load-batch-size))
         (q (context-navigator--state-get :context-load-queue)))
    (if (null q)
        (progn
          ;; Finish: build model from accumulated items, apply to GPTel, clear flags, refresh UI.
          (let ((context-navigator--in-context-load t)
                (context-navigator--inhibit-refresh t))
            (let ((acc (nreverse (or (context-navigator--state-get :context-load-acc) '()))))
              (context-navigator--model-set-items acc)
              (context-navigator--apply-model-to-gptel)))
          (context-navigator--cancel-context-load)
          (context-navigator--state-put :context-loading-p nil)
          (context-navigator--state-put :context-load-acc nil)
          (setq context-navigator--last-project-root
                (context-navigator--state-get :current-project-root))
          (setq context-navigator--inhibit-refresh nil)
          (when context-navigator-debug
            (message "context-navigator: load complete (%d items)"
                     (length (context-navigator--model-items))))
          (context-navigator-refresh))
      (let ((context-navigator--in-context-load t)
            (context-navigator--inhibit-refresh t))
        (dotimes (_ batch)
          (when q
            (let ((fn (car q)))
              (setq q (cdr q))
              (ignore-errors (funcall fn)))))
        (context-navigator--state-put :context-load-queue q)
        (when context-navigator-debug
          (message "context-navigator: load-queue left %d" (length q)))
        (when (null q)
          ;; Finalize immediately on last tick.
          (let ((context-navigator--in-context-load t)
                (context-navigator--inhibit-refresh t))
            (let ((acc (nreverse (or (context-navigator--state-get :context-load-acc) '()))))
              (context-navigator--model-set-items acc)
              (context-navigator--apply-model-to-gptel)))
          (context-navigator--cancel-context-load)
          (context-navigator--state-put :context-loading-p nil)
          (context-navigator--state-put :context-load-acc nil)
          (setq context-navigator--last-project-root
                (context-navigator--state-get :current-project-root))
          (setq context-navigator--inhibit-refresh nil)
          (when context-navigator-debug
            (message "context-navigator: load complete (%d items)"
                     (length (context-navigator--model-items))))
          (context-navigator-refresh))))))

(defun context-navigator--load-context-async (root)
  "Load context spec for ROOT into the internal model asynchronously.
Shows \"Loading…\" immediately, processes entries in background batches,
then applies the final model to GPTel once."
  (context-navigator--cancel-context-load)
  (context-navigator--state-put :current-project-root root)
  (context-navigator--state-put :context-loading-p t)
  (context-navigator--state-put :context-load-acc nil)
  ;; Show loading immediately
  (when-let ((buf (context-navigator--state-get :sidebar-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (context-navigator--render-sidebar))))
  ;; Read spec and enqueue work
  (let* ((file (context-navigator--context-file root))
         (spec (context-navigator--read-file-sexp file)))
    (if (null spec)
        ;; No spec: clear model and GPTel
        (progn
          (let ((context-navigator--in-context-load t)
                (context-navigator--inhibit-refresh t))
            (context-navigator--model-set-items nil)
            (ignore-errors (gptel-context-remove-all)))
          (context-navigator--state-put :context-loading-p nil)
          (setq context-navigator--last-project-root (context-navigator--state-get :current-project-root))
          (context-navigator-refresh))
      ;; Build queue of small tasks
      (let (queue)
        (dolist (it spec)
          (pcase it
            (`(:file ,rel . ,rest)
             (let* ((abs (context-navigator--project-abspath rel root))
                    (enabled (plist-get rest :enabled)))
               (when (file-exists-p abs)
                 (push
                  (lambda ()
                    (let* ((attrs (context-navigator--file-attrs abs))
                           (size (file-attribute-size attrs))
                           (name (file-name-nondirectory abs))
                           (icon (context-navigator--icon-for 'file nil abs))
                           (desc (format "%s (%d bytes)"
                                         (or (file-name-directory abs) "") size))
                           (node (make-context-navigator-item
                                  :type 'file :name name :path abs :size size
                                  :icon icon :enabled (if (null enabled) t enabled)
                                  :description desc)))
                      (let ((acc (context-navigator--state-get :context-load-acc)))
                        (context-navigator--state-put :context-load-acc (cons node acc)))))
                  queue))))
            (`(:buffer ,rel :regions ,regions . ,rest)
             (let* ((abs (context-navigator--project-abspath rel root))
                    (enabled (plist-get rest :enabled))
                    ;; Открываем файл только для включённых элементов — ускоряет загрузку больших проектов.
                    (buf (when (and (file-exists-p abs)
                                    (or (eq enabled t) (null enabled)))
                           (ignore-errors (find-file-noselect abs)))))
               (dolist (pair regions)
                 (when (and (consp pair) (numberp (car pair)) (numberp (cdr pair)))
                   (let ((s (car pair)) (e (cdr pair)))
                     (push
                      (lambda ()
                        (let* ((name (format "%s (selection)" (file-name-nondirectory abs)))
                               (icon (context-navigator--icon-for 'selection buf abs))
                               (desc (if (and context-navigator-show-line-numbers (buffer-live-p buf))
                                         (with-current-buffer buf
                                           (format "Lines %d-%d" (line-number-at-pos s) (line-number-at-pos e)))
                                       (format "%d chars" (- e s))))
                               (node (make-context-navigator-item
                                      :type 'selection :name name :path abs :buffer buf
                                      :start s :end e :size (- e s) :icon icon
                                      :enabled (if (null enabled) t enabled)
                                      :description desc)))
                          (let ((acc (context-navigator--state-get :context-load-acc)))
                            (context-navigator--state-put :context-load-acc (cons node acc)))))
                      queue))))))))
        (setq queue (nreverse queue))
        (context-navigator--state-put :context-load-queue queue)
        ;; Start repeating timer to process batches
        (context-navigator--state-put
         :context-load-timer
         (run-at-time 0.01 0.01 #'context-navigator--process-context-load-queue))))))

;; ----------------------------------------------------------------------------
;; Utilities
;; ----------------------------------------------------------------------------

(defcustom context-navigator-file-stat-ttl 0.3
  "TTL in seconds for cached file attributes to reduce I/O during collection."
  :type 'number
  :group 'context-navigator)

(defvar context-navigator--stat-cache (make-hash-table :test 'equal)
  "Cache mapping PATH -> (TIMESTAMP . ATTRS) for short-lived file stats.")

(defun context-navigator--file-attrs (path)
  "Return (possibly cached) file attributes for PATH honoring `context-navigator-file-stat-ttl'."
  (let* ((now (float-time))
         (cell (and path (gethash path context-navigator--stat-cache))))
    (if (and cell (< (- now (car cell)) context-navigator-file-stat-ttl))
        (cdr cell)
      (let ((attrs (file-attributes path)))
        (puthash path (cons now attrs) context-navigator--stat-cache)
        attrs))))

(defun context-navigator--truncate (s n)
  "Truncate string S to N chars with ellipsis."
  (if (and (stringp s) (> (length s) n))
      (concat (substring s 0 (max 0 (- n 3))) "...")
    (or s "")))

(defun context-navigator--guess-mode-for-file (path)
  "Guess major mode for PATH without visiting permanently."
  (when (and (stringp path) (not (file-directory-p path)))
    (with-temp-buffer
      (setq buffer-file-name path)
      (let ((enable-local-variables nil)
            (enable-local-eval nil)
            (inhibit-message t))
        (set-auto-mode))
      major-mode)))

(defun context-navigator--icon-for (type &optional buffer path mode backend)
  "Return an icon string for TYPE using optional BUFFER/PATH/MODE hints.

Preference:
1) pro-tabs icon for buffer (if available)
2) all-the-icons by major mode (cached)
3) all-the-icons by filename/extension (cached)
4) fallback emoji/symbol"
  (when context-navigator-enable-icons
    (let* ((backend (or backend 'tab-line))
           (use-pro-tabs (fboundp 'pro-tabs--icon))
           (remote-p (or (and (stringp path) (file-remote-p path))
                         (and (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (and (stringp default-directory)
                                     (file-remote-p default-directory))))))
           (allow-icons (and (not (and context-navigator-icons-disable-on-remote remote-p))
                             (progn
                               (unless (featurep 'all-the-icons)
                                 (ignore-errors (require 'all-the-icons nil t)))
                               (featurep 'all-the-icons)))))
      (pcase type
        ('file
         (or
          ;; pro-tabs for buffers
          (when (and use-pro-tabs (buffer-live-p buffer))
            (ignore-errors (pro-tabs--icon buffer backend)))
          ;; all-the-icons (if allowed)
          (when allow-icons
            (let* ((mm (or mode
                           (and (buffer-live-p buffer) (buffer-local-value 'major-mode buffer))
                           ;; Predict mode via auto-mode-alist (no visiting)
                           (assoc-default (or path "") auto-mode-alist 'string-match)
                           ;; Fallback: last resort heavy guess
                           (and path (context-navigator--guess-mode-for-file path))))
                   (ext (and (stringp path) (downcase (or (file-name-extension path) "")))))
              (or
               ;; Cached icon by major-mode
               (when mm
                 (or (gethash mm context-navigator--mode-icon-cache)
                     (let ((ic (ignore-errors (all-the-icons-icon-for-mode mm))))
                       (when ic (puthash mm ic context-navigator--mode-icon-cache))
                       ic)))
               ;; Cached icon by extension or basename
               (when (stringp path)
                 (let* ((key (or ext (file-name-nondirectory path))))
                   (or (gethash key context-navigator--ext-icon-cache)
                       (let ((ic (ignore-errors
                                   (all-the-icons-icon-for-file (file-name-nondirectory path)))))
                         (when ic (puthash key ic context-navigator--ext-icon-cache))
                         ic)))))))
          "•"))
        ((or 'buffer 'selection)
         (or
          (when (and use-pro-tabs (buffer-live-p buffer))
            (ignore-errors (pro-tabs--icon buffer backend)))
          (when (and allow-icons (or buffer mode))
            (let ((mm (or mode (and (buffer-live-p buffer)
                                    (buffer-local-value 'major-mode buffer)))))
              (or (and mm (gethash mm context-navigator--mode-icon-cache))
                  (let ((ic (and mm (ignore-errors (all-the-icons-icon-for-mode mm)))))
                    (when ic (puthash mm ic context-navigator--mode-icon-cache))
                    ic))))
          (if (eq type 'selection) "✂" "•")))
        (_ "•")))))

(defun context-navigator--normalize-face (face)
  "Return a safe face spec from FACE or nil.
Strips spurious quoting and invalid entries."
  (cl-labels
      ((unq (x) (if (and (consp x) (eq (car x) 'quote)) (cadr x) x))
       (valid (s) (and (symbolp s) (facep s) s)))
    (let ((f (unq face)))
      (cond
       ((symbolp f) (valid f))
       ((and (listp f) (keywordp (car f))) f)
       ((listp f)
        (let* ((parts (mapcar #'unq f))
               (faces (delq nil
                            (mapcar (lambda (p)
                                      (cond
                                       ((symbolp p) (valid p))
                                       ((and (listp p) (keywordp (car p))) p)
                                       (t nil)))
                                    parts))))
          (cond
           ((null faces) nil)
           ((and (= (length faces) 1) (symbolp (car faces))) (car faces))
           (t faces))))
       (t nil)))))

(defun context-navigator--insert-with-face (str face)
  "Insert STR with FACE (normalized) into current buffer."
  (let ((f (context-navigator--normalize-face face)))
    (if f
        (widget-insert (propertize str 'face f))
      (widget-insert str))))

;; ----------------------------------------------------------------------------
;; Context collectors
;; ----------------------------------------------------------------------------

(defun context-navigator--context-alist ()
  "Return a normalized gptel context alist.
Each element is either:
- (BUFFER . OVERLAYS)
- (PATH . PROPS)
- (PATH) ; bare path
Uses gptel-context--alist if bound, else gptel-context--collect."
  (cond
   ((boundp 'gptel-context--alist) gptel-context--alist)
   ((fboundp 'gptel-context--collect) (gptel-context--collect))
   (t nil)))

(defun context-navigator--uniq-items (items)
  "Deduplicate ITEMS by key: selection=(buf+start+end), file=path, buffer=bufname."
  (let ((seen (make-hash-table :test 'equal))
        out)
    (dolist (it items)
      (let* ((type (context-navigator-item-type it))
             (key (pcase type
                    ('selection
                     (format "sel:%s:%s-%s"
                             (and (buffer-live-p (context-navigator-item-buffer it))
                                  (buffer-name (context-navigator-item-buffer it)))
                             (context-navigator-item-start it)
                             (context-navigator-item-end it)))
                    ('file (or (context-navigator-item-path it)
                               (context-navigator-item-name it)))
                    (_ (or (and (buffer-live-p (context-navigator-item-buffer it))
                                (buffer-name (context-navigator-item-buffer it)))
                           (context-navigator-item-name it))))))
        (unless (gethash key seen)
          (puthash key t seen)
          (push it out))))
    (nreverse out)))

(defun context-navigator--collector-current (buffer)
  "Collect context from current BUFFER (not enabled by default)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((file (buffer-file-name))
             (mode major-mode)
             (buf buffer)
             items)
        (cond
         ((and file (use-region-p))
          (push (make-context-navigator-item
                 :type 'selection
                 :name (format "%s (selection)" (file-name-nondirectory file))
                 :path file
                 :buffer buf
                 :start (region-beginning)
                 :end (region-end)
                 :size (- (region-end) (region-beginning))
                 :icon (context-navigator--icon-for 'selection buf file mode)
                 :description (if context-navigator-show-line-numbers
                                  (format "Lines %d-%d"
                                          (line-number-at-pos (region-beginning))
                                          (line-number-at-pos (region-end)))
                                (format "%d chars" (- (region-end) (region-beginning)))))
                items))
         (file
          (push (make-context-navigator-item
                 :type 'file
                 :name (file-name-nondirectory file)
                 :path file
                 :buffer buf
                 :size (buffer-size)
                 :icon (context-navigator--icon-for 'file buf file mode)
                 :description (format "%s (%d chars)"
                                      (or (file-name-directory file) "")
                                      (buffer-size)))
                items))
         ((use-region-p)
          (push (make-context-navigator-item
                 :type 'selection
                 :name (format "%s (selection)" (buffer-name buf))
                 :buffer buf
                 :start (region-beginning)
                 :end (region-end)
                 :size (- (region-end) (region-beginning))
                 :icon (context-navigator--icon-for 'selection buf nil mode)
                 :description (if context-navigator-show-line-numbers
                                  (format "Lines %d-%d"
                                          (line-number-at-pos (region-beginning))
                                          (line-number-at-pos (region-end)))
                                (format "%d chars" (- (region-end) (region-beginning)))))
                items))
         (t
          (push (make-context-navigator-item
                 :type 'buffer
                 :name (buffer-name buf)
                 :buffer buf
                 :size (buffer-size)
                 :icon (context-navigator--icon-for 'buffer buf nil mode)
                 :description (format "%d chars" (buffer-size)))
                items)))
        items))))

(defun context-navigator--collector-visible (_buffer)
  "Collect context from all visible windows' buffers (not enabled by default)."
  (let (items)
    (dolist (w (window-list nil 'no-mini))
      (let ((b (window-buffer w)))
        (when (buffer-live-p b)
          (setq items (nconc items (or (context-navigator--collector-current b) '()))))))
    (context-navigator--uniq-items items)))

(defun context-navigator--collector-gptel (_buffer)
  "Collect items from gptel-context."
  (let* ((gtbuf (context-navigator--find-gptel-buffer))
         (alist (if (and gtbuf (buffer-live-p gtbuf))
                    (with-current-buffer gtbuf
                      (context-navigator--context-alist))
                  (context-navigator--context-alist)))
         items)
    (dolist (entry alist)
      (pcase entry
        (`(,(and buf (pred bufferp)) . ,ovs)
         (dolist (ov ovs)
           (when (and (overlayp ov) (overlay-start ov) (overlay-end ov))
             (let* ((start (overlay-start ov))
                    (end (overlay-end ov))
                    (whole-buf (with-current-buffer buf
                                 (and (= start (point-min)) (= end (point-max)))))
                    (type (if whole-buf 'buffer 'selection))
                    (name (buffer-name buf))
                    (icon (context-navigator--icon-for type buf))
                    (desc (with-current-buffer buf
                            (if (eq type 'buffer)
                                (format "%d chars" (buffer-size))
                              (if context-navigator-show-line-numbers
                                  (format "Lines %d-%d"
                                          (line-number-at-pos start)
                                          (line-number-at-pos end))
                                (format "%d chars" (- end start)))))))
               (push (make-context-navigator-item
                      :type type
                      :name (if (eq type 'buffer) name (format "%s (selection)" name))
                      :buffer buf
                      :start (unless (eq type 'buffer) start)
                      :end (unless (eq type 'buffer) end)
                      :size (if (eq type 'buffer)
                                (with-current-buffer buf (buffer-size))
                              (- end start))
                      :icon icon
                      :description desc)
                     items)))))
        (`(,(and path (pred stringp)) . ,props)
         (when (file-exists-p path)
           (let* ((attrs (context-navigator--file-attrs path))
                  (size (file-attribute-size attrs))
                  (name (file-name-nondirectory path))
                  (icon (context-navigator--icon-for 'file nil path))
                  (desc (if-let ((mime (plist-get props :mime)))
                            (format "%s (%s, %d bytes)"
                                    (or (file-name-directory path) "") mime size)
                          (format "%s (%d bytes)"
                                  (or (file-name-directory path) "") size))))
             (push (make-context-navigator-item
                    :type 'file
                    :name name
                    :path path
                    :size size
                    :icon icon
                    :description desc)
                   items))))
        (`(,path)
         (when (file-exists-p path)
           (let* ((attrs (context-navigator--file-attrs path))
                  (size (file-attribute-size attrs))
                  (name (file-name-nondirectory path))
                  (icon (context-navigator--icon-for 'file nil path))
                  (desc (format "%s (%d bytes)"
                                (or (file-name-directory path) "") size)))
             (push (make-context-navigator-item
                    :type 'file
                    :name name
                    :path path
                    :size size
                    :icon icon
                    :description desc)
                   items))))))
    (nreverse items)))

(defun context-navigator--detect-root ()
  "Detect current project root via project.el or projectile."
  (or
   (when (require 'project nil t)
     (when-let ((proj (project-current nil)))
       (if (fboundp 'project-root)
           (project-root proj)
         (car (project-roots proj)))))
   (when (require 'projectile nil t)
     (ignore-errors (projectile-project-root)))))

(defun context-navigator--pc-special-buffer-p (buf)
  "Return non-nil if BUF is special/temporary."
  (when (buffer-live-p buf)
    (let ((n (buffer-name buf)))
      (or (string-prefix-p " " n)
          (string-prefix-p "*" n)
          (eq (buffer-local-value 'major-mode buf) 'context-navigator-sidebar-mode)))))

(defun context-navigator--pc-pick-active-buffer ()
  "Pick most relevant non-special buffer from visible windows."
  (or
   (let ((b (window-buffer (selected-window))))
     (unless (context-navigator--pc-special-buffer-p b) b))
   (catch 'found
     (dolist (w (window-list nil 'no-mini))
       (let ((b (window-buffer w)))
         (unless (context-navigator--pc-special-buffer-p b)
           (throw 'found b))))
     nil)
   (let ((b (current-buffer)))
     (unless (context-navigator--pc-special-buffer-p b) b))))

(defun context-navigator--project-root-of-buffer (buf)
  "Return project root of BUF using project.el or projectile."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (or
       (context-navigator--detect-root)
       nil))))

(defun context-navigator--collector-project-file (_buffer)
  "Collector that reads per-project context file .context/context.el (project-local).
This returns file items for the current project context file when present.

Note: This collector is intentionally NOT part of `context-navigator-context-collectors`
to keep the sidebar a pure projection of the runtime gptel-context (single source of truth)."
  (let* ((root (or (context-navigator--state-get :current-project-root)
                   (when-let ((b (context-navigator--pc-pick-active-buffer)))
                     (context-navigator--project-root-of-buffer b)))))
    (when root
      (let* ((ctx-file (context-navigator--context-file root)))
        (when (file-readable-p ctx-file)
          (condition-case _err
              (let* ((data (with-temp-buffer
                             (insert-file-contents ctx-file)
                             (goto-char (point-min))
                             (read (current-buffer))))
                     (items nil))
                (dolist (entry data)
                  (let ((path (plist-get entry :file)))
                    (when (and path (stringp path))
                      (let* ((abs (if (file-name-absolute-p path)
                                      path
                                    (expand-file-name path root))))
                        (when (file-exists-p abs)
                          (let* ((attrs (file-attributes abs))
                                 (size (file-attribute-size attrs))
                                 (name (file-name-nondirectory abs))
                                 (desc (format "%s (%d bytes)"
                                               (or (file-name-directory abs) "") size)))
                            (push (make-context-navigator-item
                                   :type 'file
                                   :name name
                                   :path abs
                                   :size size
                                   :icon (context-navigator--icon-for 'file nil abs)
                                   :description desc)
                                  items)))))))
                (nreverse items))
            (error nil)))))))

(make-obsolete 'context-navigator--collector-project-file
               "Not used by the sidebar; sidebar shows only runtime gptel-context."
               "1.0.0")

(defvar context-navigator-context-collectors
  '(context-navigator--collector-gptel)
  "List of collector functions to aggregate context items.

Each function is called with current buffer and should return a
list of `context-navigator-item' objects. Results are deduplicated.")

(defun context-navigator--collect-context ()
  "Collect and deduplicate context items from all collectors."
  (let (items)
    (dolist (fn context-navigator-context-collectors)
      (when (functionp fn)
        (setq items (nconc items (ignore-errors (funcall fn (current-buffer)))))))
    (context-navigator--uniq-items items)))

;; ----------------------------------------------------------------------------
;; Sidebar UI
;; ----------------------------------------------------------------------------

(defvar context-navigator-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'context-navigator-goto-item)
    (define-key map (kbd "SPC") #'context-navigator-preview-item)
    (define-key map (kbd "t")   #'context-navigator-toggle-enabled-at-point)
    (define-key map (kbd "d")   #'context-navigator-disable-item-at-point)
    (define-key map (kbd "C-d") #'context-navigator-remove-item-at-point)
    (define-key map (kbd "r")   #'context-navigator-refresh)
    (define-key map (kbd "q")   #'context-navigator-quit)
    (define-key map (kbd "?")   #'context-navigator-help)
    (define-key map (kbd "TAB") #'widget-forward)
    (define-key map (kbd "S-TAB") #'widget-backward)
    map)
  "Keymap for context-navigator sidebar.")

(define-derived-mode context-navigator-sidebar-mode special-mode "context-navigator"
  "Major mode for context-navigator sidebar."
  (setq buffer-read-only t
        truncate-lines t)
  ;; Disable echo-area/tooltips for help-echo to prevent spam like “Expand node”.
  (setq-local show-help-function #'ignore)
  ;; Also stop help-at-pt (idle tooltip/messages) from printing widget hints like “Expand node”.
  (setq-local help-at-pt-display-when-idle nil)
  (hl-line-mode 1))

(defun context-navigator--create-item-widget (item)
  "Create a tree-widget for ITEM.

Preserve per-file expanded/collapsed state across refreshes by honoring
the :expanded-file-paths entry in the internal state. That list is
populated before re-rendering the sidebar so user-opened nodes remain open."
  (let* ((name (context-navigator--truncate
                (context-navigator-item-name item)
                context-navigator-max-filename-length))
         (raw-icon (context-navigator-item-icon item))
         (icon (if (stringp raw-icon) raw-icon (format "%s" (or raw-icon ""))))
         (desc (context-navigator-item-description item))
         (type (context-navigator-item-type item))
         (enabled (context-navigator-item-enabled item))
         (state-icon (context-navigator--state-icon enabled))
         (sel-buf (context-navigator--pc-pick-active-buffer))
         (sel-p (cond
                 ((memq type '(buffer selection))
                  (eq (context-navigator-item-buffer item) sel-buf))
                 ((eq type 'file)
                  (let ((p (context-navigator-item-path item)))
                    (and (buffer-live-p sel-buf)
                         (with-current-buffer sel-buf
                           (and buffer-file-name
                                (string=
                                 (expand-file-name buffer-file-name)
                                 (and p (expand-file-name p))))))))
                 (t nil)))
         ;; Build tag: state icon + file/buf icon + name with face by state
         (tag-str (cond
                   ((not enabled)
                    (concat state-icon " " icon " " (propertize name 'face 'context-navigator-disabled-item)))
                   (sel-p
                    (concat state-icon " " icon " " (propertize name 'face 'context-navigator-active-buffer)))
                   (t
                    (concat state-icon " " icon " " name))))
         (expanded-list (context-navigator--state-get :expanded-file-paths))
         (path (context-navigator-item-path item))
         (open-p (and path expanded-list (member path expanded-list))))
    `(tree-widget
      :node (push-button
             :tag ,tag-str
             :format "%[%t%]\n"
             :help-echo nil
             :notify context-navigator--item-action
             :button-data ,item)
      :help-echo nil
      :dynargs context-navigator--item-children
      :has-children ,(eq type 'file)
      :open ,open-p)))

(defun context-navigator--item-action (widget &optional _event)
  "Handle activation on WIDGET."
  (when-let ((item (widget-get widget :button-data)))
    (context-navigator--state-put :selected-item item)
    (context-navigator-goto-item-internal item)))

(defun context-navigator--widget-item-at-point ()
  "Return the context-navigator-item at point, traversing widget parents if needed."
  (let* ((w (widget-at (point))))
    (when w
      (or (widget-get w :button-data)
          (let* ((cur w)
                 (limit 10)
                 (item nil))
            ;; Walk up until a widget carries :button-data
            (while (and cur (> limit 0) (not item))
              (setq item (widget-get cur :button-data))
              (setq cur (widget-get cur :parent))
              (setq limit (1- limit)))
            (or item
                ;; If we're on a tree-widget or its child, get node's button data
                (let* ((tw (or (and (eq (widget-type w) 'tree-widget) w)
                               (let ((p (widget-get w :parent)))
                                 (and p (eq (widget-type p) 'tree-widget) p)))))
                  (when (and tw (fboundp 'tree-widget-node))
                    (let ((node (tree-widget-node tw)))
                      (and node (widget-get node :button-data)))))))))))

(defun context-navigator--find-gptel-buffer ()
  "Try to find the active GPTel chat buffer."
  (or (context-navigator--state-get :gptel-buffer)
      ;; Prefer a visible window with a GPTel buffer
      (catch 'buf
        (dolist (w (window-list nil 'no-mini))
          (with-current-buffer (window-buffer w)
            (when (derived-mode-p 'gptel-mode)
              (throw 'buf (current-buffer)))))
        nil)
      ;; Fallback: any GPTel buffer
      (catch 'buf
        (dolist (b (buffer-list))
          (when (buffer-live-p b)
            (with-current-buffer b
              (when (derived-mode-p 'gptel-mode)
                (throw 'buf b)))))
        nil)))

;;;###autoload
(defun context-navigator-remove-item-at-point ()
  "Permanently remove the context item at point from model and GPTel, then refresh."
  (interactive)
  (let ((item (context-navigator--widget-item-at-point)))
    (unless item (user-error "No context item at point"))
    (unless (fboundp 'gptel-context-remove)
      (user-error "gptel-context-remove is not available"))
    (let* ((key (context-navigator--item-key item))
           (runner
            (lambda ()
              (pcase (context-navigator-item-type item)
                ('file
                 (let ((path (context-navigator-item-path item)))
                   (when path (gptel-context-remove path))))
                ('buffer
                 (let ((buf (context-navigator-item-buffer item)))
                   (when buf (gptel-context-remove buf))))
                ('selection
                 (let ((buf (context-navigator-item-buffer item))
                       (start (context-navigator-item-start item))
                       (end (context-navigator-item-end item)))
                   (if (and buf start end)
                       (gptel-context-remove buf start end)
                     (when buf (gptel-context-remove buf)))))
                (_ (user-error "Unknown item type"))))))
      ;; Выполним удаление в GPTel с подавлением автосейва, затем обновим модель и сохраним
      (let ((context-navigator--in-context-load t))
        (if-let ((gtbuf (context-navigator--find-gptel-buffer)))
            (with-current-buffer gtbuf
              (funcall runner))
          (funcall runner)))
      ;; Удалить из модели
      (context-navigator--model-del key)
      ;; Сохранить контекст на диск после удаления из модели и GPTel
      (when (or context-navigator-autosave (bound-and-true-p context-navigator-autoload))
        (ignore-errors
          (context-navigator-context-save (context-navigator--detect-root))))
      (context-navigator-refresh)
      (message "Deleted item from model and GPTel."))))

;;;###autoload
(defun context-navigator-disable-item-at-point ()
  "Disable the item at point (keep in list, exclude from GPTel)."
  (interactive)
  (let* ((item (context-navigator--widget-item-at-point)))
    (unless item (user-error "No context item at point"))
    (let* ((key (context-navigator--item-key item))
           (model (or (context-navigator--model-get key) item))
           (already-disabled (not (context-navigator-item-enabled model)))
           (copy (copy-context-navigator-item model)))
      (unless already-disabled
        (setf (context-navigator-item-enabled copy) nil)
        (context-navigator--model-put copy :disabled)
        (context-navigator--apply-model-to-gptel)
        (when (or context-navigator-autosave (bound-and-true-p context-navigator-autoload))
          (ignore-errors
            (context-navigator-context-save (context-navigator--detect-root)))))
      (context-navigator-refresh)
      (message (if already-disabled
                   "Item already disabled."
                 "Disabled item (kept for later).")))))

;;;###autoload
(defun context-navigator-toggle-enabled-at-point ()
  "Toggle enabled/disabled state for the item at point and reconcile GPTel."
  (interactive)
  (let* ((item (context-navigator--widget-item-at-point)))
    (unless item (user-error "No context item at point"))
    (let* ((key (context-navigator--item-key item))
           (model (or (context-navigator--model-get key) item))
           (cur (and (context-navigator-item-enabled model) t))
           (new (not cur))
           (copy (copy-context-navigator-item model)))
      (setf (context-navigator-item-enabled copy) new)
      (context-navigator--model-put copy (if new :enabled :disabled))
      (context-navigator--apply-model-to-gptel)
      (when (or context-navigator-autosave (bound-and-true-p context-navigator-autoload))
        (ignore-errors
          (context-navigator-context-save (context-navigator--detect-root))))
      (context-navigator-refresh)
      (message (if new "Enabled item (added to GPTel)"
                 "Disabled item (excluded from GPTel)")))))

(defun context-navigator--item-children (tree)
  "Return children for TREE (file details)."
  (let* ((item (widget-get (tree-widget-node tree) :button-data)))
    (when (and item (eq (context-navigator-item-type item) 'file))
      (list
       `(item :tag ,(format "📊 Size: %d bytes" (or (context-navigator-item-size item) 0))
              :help-echo nil)
       `(item :tag ,(format "📁 Path: %s" (or (context-navigator-item-path item) ""))
              :help-echo nil)))))

(defun context-navigator--gather-open-file-paths ()
  "Return list of file paths for tree nodes currently open in the sidebar buffer.

This inspects the existing widget tree in the sidebar and records file
paths whose tree nodes are currently expanded. The result is used to
restore the open/closed state across re-renders."
  (let ((res nil))
    (when-let ((buf (context-navigator--state-get :sidebar-buffer)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (condition-case nil
                (let ((done nil)
                      (cycle-limit 10000))
                  (while (and (not done) (< (point) (point-max)) (> cycle-limit 0))
                    (let ((w (widget-at)))
                      (when (and w (eq (widget-type w) 'tree-widget))
                        (let ((node (tree-widget-node w)))
                          (when (and node (widget-get node :open))
                            (let ((item (widget-get node :button-data)))
                              (when (and item (eq (context-navigator-item-type item) 'file))
                                (push (context-navigator-item-path item) res)))))))
                    (let ((pos-before (point)))
                      (condition-case nil
                          (widget-forward 1)
                        (error (setq done t)))
                      (let ((pos-after (point)))
                        (cond
                         ((> pos-after pos-before) nil) ; ok, moved
                         (t
                          ;; No movement, try forward-char
                          (condition-case nil
                              (progn (forward-char 1)
                                     (if (= (point) pos-before)
                                         (setq done t)))
                            (error (setq done t)))))))
                    (setq cycle-limit (1- cycle-limit)))
                  (error nil))))))
      (nreverse res))))

(defface context-navigator-active-buffer
  '((t :foreground "green3" :weight bold))
  "Face for highlighting the current buffer in context-navigator sidebar.")

(defface context-navigator-disabled-item
  '((t :foreground "gray55"))
  "Face for disabled (excluded) items in the sidebar.")

(defface context-navigator-state-enabled
  '((t :foreground "green3" :weight bold))
  "Face for the enabled state indicator dot.")

(defface context-navigator-state-disabled
  '((t :foreground "gray60"))
  "Face for the disabled state indicator dot.")

(defun context-navigator--displayable-emoji-p (char)
  "Return non-nil if CHAR is displayable in the current frame."
  (and (display-graphic-p)
       (char-displayable-p char)))

(defun context-navigator--state-icon (enabled)
  "Return a propertized state indicator string for ENABLED."
  (let* ((have-emoji (and (context-navigator--displayable-emoji-p ?🟢)
                          (context-navigator--displayable-emoji-p ?⚪)))
         (str (if have-emoji
                  (if enabled "🟢" "⚪")
                "●"))
         (face (if enabled 'context-navigator-state-enabled
                 'context-navigator-state-disabled)))
    (propertize str 'face face)))

(defcustom context-navigator-squelch-tree-widget-echo t
  "When non-nil, silence tree-widget help-echo messages like “Expand node”."
  :type 'boolean
  :group 'context-navigator)

(when context-navigator-squelch-tree-widget-echo
  (with-eval-after-load 'tree-widget
    ;; Some Emacs builds provide `tree-widget-button-echo' to generate
    ;; “Expand node” / “Collapse node”. If present, override it to return nil.
    (when (fboundp 'tree-widget-button-echo)
      (advice-add 'tree-widget-button-echo :override (lambda (&rest _) nil)))))

(defun context-navigator--scrub-help-echo ()
  "Remove help-echo from text and overlays in the sidebar."
  (let ((inhibit-read-only t))
    (save-excursion
      (save-restriction
        (widen)
        (remove-text-properties (point-min) (point-max) '(help-echo nil))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (overlayp ov)
            (overlay-put ov 'help-echo nil)))))))

(defun context-navigator--render-sidebar ()
  "Render sidebar contents from current state."
  (let ((inhibit-read-only t)
        (inhibit-message t)
        (items (context-navigator--state-get :context-items))
        ;; Preserve point and scroll to avoid jumping to the first item on refresh.
        (win (get-buffer-window (current-buffer) t))
        (pt-line (line-number-at-pos (point)))
        (col (current-column))
        (ws-line (let* ((w (get-buffer-window (current-buffer) t))
                        (ws (and w (window-start w))))
                   (and ws (line-number-at-pos ws))))
        (widgets-created nil))
    ;; Preserve expanded file nodes so expanding a file is not lost after a refresh.
    (condition-case nil
        (let ((expanded (context-navigator--gather-open-file-paths)))
          (when expanded
            (context-navigator--state-put :expanded-file-paths expanded)))
      (error nil))
    (erase-buffer)
    ;; Header
    ;; Show project name instantly on buffer switch, but ignore Dired buffers.
    ;; Only react to file-visiting buffers and GPTel chat buffers.
    (let* ((state-root (context-navigator--state-get :current-project-root))
           (b (context-navigator--pc-pick-active-buffer))
           (use-buf-root (and b
                              (with-current-buffer b
                                (and (not (derived-mode-p 'dired-mode))
                                     (or buffer-file-name
                                         (derived-mode-p 'gptel-mode))))))
           (buf-root (when use-buf-root
                       (context-navigator--project-root-of-buffer b)))
           (root (cond
                  ((eq state-root :global) nil) ; stay global
                  ((and (stringp buf-root) (not (equal buf-root state-root))) buf-root)
                  ((stringp state-root) state-root)
                  (t buf-root)))
           (title (if (and root (stringp root))
                      (file-name-nondirectory (directory-file-name root))
                    "~")))
      (context-navigator--insert-with-face (format "🚀 %s\n\n" title) 'header-line))
    ;; Body
    (let ((loading (context-navigator--state-get :context-loading-p)))
      (cond
       (loading
        (context-navigator--insert-with-face "⏳ Loading context…\n\n" 'shadow))
       ((null items)
        (context-navigator--insert-with-face
         "No context available\nOpen files or select text to add context.\n\n"
         'italic))
       (t
        (dolist (it items)
          (widget-create (context-navigator--create-item-widget it))
          (setq widgets-created t))
        (when widgets-created
          (widget-insert "\n")))))
    (widget-setup)
    (context-navigator--scrub-help-echo)
    ;; Restore scroll/point if possible; otherwise, default to first item only once.
    (cond
     ((and win ws-line pt-line)
      (let ((inhibit-point-motion-hooks t))
        (save-excursion
          (goto-char (point-min))
          (forward-line (max 0 (1- ws-line)))
          (set-window-start win (point)))
        (goto-char (point-min))
        (forward-line (max 0 (1- pt-line)))
        (move-to-column col)))
     ((and items widgets-created)
      (goto-char (point-min))
      (condition-case _ (widget-forward 1) (error nil))))))

(defun context-navigator--create-sidebar ()
  "Create sidebar buffer/window."
  (let* ((buffer (get-buffer-create "*context-navigator/"))
         (window (display-buffer-in-side-window
                  buffer
                  `((side . left)
                    (window-width . ,context-navigator-sidebar-width)
                    (slot . 0)))))
    (with-current-buffer buffer
      (context-navigator-sidebar-mode))
    (context-navigator--state-put :sidebar-buffer buffer)
    (context-navigator--state-put :sidebar-window window)
    (context-navigator--setup-context-advice)
    (when (and context-navigator-autosave
               (fboundp 'context-navigator--setup-project-context-save-advice))
      (context-navigator--setup-project-context-save-advice))
    ;; Hooks for highlight/auto-load are installed globally via
    ;; `context-navigator--setup-autoload-hooks'. Nothing extra here.
    buffer))

(defun context-navigator--ensure-sidebar ()
  "Ensure sidebar exists and is visible."
  (let ((buf (context-navigator--state-get :sidebar-buffer))
        (win (context-navigator--state-get :sidebar-window)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (context-navigator--create-sidebar)))
    (unless (and win (window-live-p win))
      (setq win (display-buffer-in-side-window
                 buf
                 `((side . left)
                   (window-width . ,context-navigator-sidebar-width)
                   (slot . 0))))
      (context-navigator--state-put :sidebar-window win))
    buf))

;; ----------------------------------------------------------------------------
;; Sidebar visibility across frames/tabs (global sync)
;; ----------------------------------------------------------------------------

(defun context-navigator--sidebar-window-in-frame (frame)
  "Return the window in FRAME showing the navigator sidebar, or nil."
  (let ((buf (context-navigator--state-get :sidebar-buffer)))
    (when (and buf (buffer-live-p buf))
      (with-selected-frame frame
        (get-buffer-window buf)))))

(defun context-navigator--sidebar-visible-p (&optional frame)
  "Return non-nil if the sidebar is visible in FRAME (or current frame)."
  (let ((fr (or frame (selected-frame))))
    (and (context-navigator--sidebar-window-in-frame fr) t)))

(defun context-navigator--open-in-frame (frame)
  "Ensure the sidebar is visible in FRAME."
  (with-selected-frame frame
    (let* ((buf (or (and (buffer-live-p (context-navigator--state-get :sidebar-buffer))
                         (context-navigator--state-get :sidebar-buffer))
                    (context-navigator--create-sidebar)))
           (win (get-buffer-window buf)))
      (unless (and win (window-live-p win))
        (setq win (display-buffer-in-side-window
                   buf
                   `((side . left)
                     (window-width . ,context-navigator-sidebar-width)
                     (slot . 0)))))
      (when (eq frame (selected-frame))
        (context-navigator--state-put :sidebar-window win))
      buf)))

(defun context-navigator--hide-in-frame (frame)
  "Hide the sidebar window in FRAME if present."
  (with-selected-frame frame
    (when-let* ((buf (context-navigator--state-get :sidebar-buffer))
                (win (get-buffer-window buf)))
      (when (window-live-p win)
        (delete-window win)))
    (when (eq frame (selected-frame))
      (context-navigator--state-put :sidebar-window nil))))

(defun context-navigator--sync-sidebar-visibility (&optional frame &rest _)
  "Ensure sidebar visibility in FRAME matches global state.
When FRAME is nil, sync the selected frame."
  (let ((fr (or frame (selected-frame))))
    (if context-navigator--sidebar-open-global
        (context-navigator--open-in-frame fr)
      (context-navigator--hide-in-frame fr))))

(defun context-navigator--sync-all-frames ()
  "Sync sidebar visibility across all frames to the global state."
  (dolist (fr (frame-list))
    (context-navigator--sync-sidebar-visibility fr)))

(defun context-navigator--sync-current-frame (&rest _)
  "Hook helper to sync sidebar visibility in the current frame."
  (context-navigator--sync-sidebar-visibility (selected-frame)))

(defun context-navigator--visibility-hooks-setup ()
  "Install hooks to keep sidebar visibility synced across frames/tabs."
  (unless context-navigator--visibility-hooks-installed
    (add-hook 'window-configuration-change-hook #'context-navigator--sync-current-frame)
    (add-hook 'after-make-frame-functions #'context-navigator--sync-sidebar-visibility)
    (add-hook 'window-selection-change-functions #'context-navigator--sync-current-frame)
    (setq context-navigator--visibility-hooks-installed t)))

(defun context-navigator--visibility-hooks-teardown ()
  "Remove hooks that sync sidebar visibility."
  (when context-navigator--visibility-hooks-installed
    (remove-hook 'window-configuration-change-hook #'context-navigator--sync-current-frame)
    (remove-hook 'after-make-frame-functions #'context-navigator--sync-sidebar-visibility)
    (remove-hook 'window-selection-change-functions #'context-navigator--sync-current-frame)
    (setq context-navigator--visibility-hooks-installed nil)))

;; ----------------------------------------------------------------------------
;; Navigation
;; ----------------------------------------------------------------------------

(defun context-navigator-goto-item-internal (item)
  "Navigate to ITEM's file/buffer/region and recenter."
  (let* ((type (context-navigator-item-type item))
         (buf (context-navigator-item-buffer item))
         (path (context-navigator-item-path item))
         (beg (context-navigator-item-start item))
         (end (context-navigator-item-end item)))
    (cond
     ;; Open files on RET: jump to existing window or open in other window
     ((eq type 'file)
      (let* ((existing-buf (or (and (buffer-live-p buf) buf)
                               (and path (get-file-buffer path))))
             (win (and existing-buf (get-buffer-window existing-buf))))
        (cond
         (win
          (select-window win))
         ((and path (file-exists-p path))
          (find-file-other-window path))
         (t
          (user-error "File not found: %s" path))))
      (recenter)
      (message "📍 Opened file: %s" (or (context-navigator-item-name item) path)))
     ;; Buffers and selections
     (t
      ;; Ленивая подгрузка буфера, если он ещё не открыт (важно для selection из сохранённого контекста)
      (unless (buffer-live-p buf)
        (when (and path (file-exists-p path))
          (setq buf (find-file-noselect path))))
      (when (buffer-live-p buf)
        (if-let ((win (get-buffer-window buf)))
            (select-window win)
          (switch-to-buffer-other-window buf))
        ;; Do not activate region; just jump to the start if available.
        (when beg (goto-char beg))
        (recenter)
        (message "📍 Navigated to: %s" (context-navigator-item-name item)))))))

;;;###autoload
(defun context-navigator-goto-item ()
  "Navigate to the item at point."
  (interactive)
  (when-let* ((w (widget-at))
              (item (widget-get w :button-data)))
    (context-navigator-goto-item-internal item)))

;;;###autoload
(defun context-navigator-preview-item ()
  "Preview item at point in its window without leaving the sidebar."
  (interactive)
  (when-let* ((w (widget-at))
              (item (widget-get w :button-data)))
    (let ((buf (context-navigator-item-buffer item)))
      (when (buffer-live-p buf)
        (when-let ((win (get-buffer-window buf t)))
          (with-selected-window win
            (when-let ((s (context-navigator-item-start item)))
              (goto-char s)
              (recenter))))
        (message "👁️ Previewing: %s" (context-navigator-item-name item))))))

;; ----------------------------------------------------------------------------
;; Refresh and auto-refresh
;; ----------------------------------------------------------------------------

(defun context-navigator-refresh ()
  "Render the sidebar from the internal model (seeding from GPTel if needed)."
  (interactive)
  (let* ((t0 (float-time)))
    (context-navigator--ensure-model-seeded)
    (let* ((items (context-navigator--model-items))
           (buf (context-navigator--state-get :sidebar-buffer))
           (t1 (float-time)))
      (context-navigator--state-put :context-items items)
      (when (and buf (buffer-live-p buf))
        (when-let ((win (car (get-buffer-window-list buf nil t))))
          (context-navigator--state-put :sidebar-window win))
        (with-current-buffer buf
          (context-navigator--render-sidebar)))
      (when context-navigator-debug
        (message "context-navigator: refresh %d items in %.1f ms"
                 (length items) (* 1000.0 (- t1 t0)))))))

(defun context-navigator--debounced-refresh ()
  "Internal: perform a single refresh and clear debounce timer."
  (context-navigator--state-put :refresh-timer nil)
  (context-navigator-refresh))

(defun context-navigator--auto-refresh ()
  "Auto-refresh sidebar if enabled (debounced)."
  (when (and context-navigator-auto-refresh
             (not context-navigator--inhibit-refresh)
             (context-navigator--state-get :sidebar-buffer))
    (unless (context-navigator--state-get :refresh-timer)
      (context-navigator--state-put
       :refresh-timer
       (run-at-time 0.05 nil #'context-navigator--debounced-refresh)))))

(defvar context-navigator--context-advised-fns
  '(gptel-context-add
    gptel-context-add-file
    gptel-context--add-region
    gptel-context-remove
    gptel-context-remove-all
    gptel-context--add-directory)
  "gptel-context mutators that should trigger auto-refresh/auto-save.")

(defun context-navigator--advice-refresh (&rest _)
  "Advice target: refresh sidebar after context changes."
  (context-navigator--auto-refresh))

(defun context-navigator--around-remove-all (orig &rest args)
  "Around advice for `gptel-context-remove-all' to suppress per-item effects.
Runs the original with suppression flags, then performs a single save/refresh
only if not already inside a higher-level bulk operation."
  (let ((outer-load context-navigator--in-context-load)
        (outer-inhibit context-navigator--inhibit-refresh))
    (let ((context-navigator--in-context-load t)
          (context-navigator--inhibit-refresh t))
      (apply orig args))
    ;; If this wasn't part of an outer bulk op, do one save+refresh.
    (unless (or outer-load outer-inhibit)
      (context-navigator--advice-save-context)
      (context-navigator--auto-refresh))))

(defun context-navigator--setup-context-advice ()
  "Install advices on context-mutating functions (refresh debounced, bulk-safe)."
  (when (and (featurep 'gptel-context)
             context-navigator-auto-refresh
             (not (context-navigator--state-get :advices-installed)))
    ;; After-advices: refresh (debounced) + sync model from gptel
    (dolist (fn context-navigator--context-advised-fns)
      (when (fboundp fn)
        (advice-add fn :after #'context-navigator--advice-refresh)
        (advice-add fn :after #'context-navigator--advice-sync-model)))
    ;; Around-advise: suppress per-item storm during remove-all, do single finalize
    (when (fboundp 'gptel-context-remove-all)
      (unless (advice-member-p #'context-navigator--around-remove-all 'gptel-context-remove-all)
        (advice-add 'gptel-context-remove-all :around #'context-navigator--around-remove-all)))
    (context-navigator--state-put :advices-installed t)))

;; ----------------------------------------------------------------------------
;; Refresh advice teardown
;; ----------------------------------------------------------------------------

(defun context-navigator--teardown-context-advice ()
  "Remove context advices."
  (when (context-navigator--state-get :advices-installed)
    (dolist (fn context-navigator--context-advised-fns)
      (when (advice-member-p #'context-navigator--advice-refresh fn)
        (advice-remove fn #'context-navigator--advice-refresh))
      (when (advice-member-p #'context-navigator--advice-sync-model fn)
        (advice-remove fn #'context-navigator--advice-sync-model)))
    ;; Remove the around advice from remove-all if present
    (when (advice-member-p #'context-navigator--around-remove-all 'gptel-context-remove-all)
      (advice-remove 'gptel-context-remove-all #'context-navigator--around-remove-all))
    ;; Cancel pending refresh timer if any
    (when-let ((tm (context-navigator--state-get :refresh-timer)))
      (when (timerp tm) (cancel-timer tm)))
    (context-navigator--state-put :refresh-timer nil)
    (context-navigator--state-put :advices-installed nil)))

;; ----------------------------------------------------------------------------
;; Sidebar commands
;; ----------------------------------------------------------------------------

;;;###autoload
(defun context-navigator-toggle ()
  "Toggle the context-navigator sidebar in the current frame only."
  (interactive)
  (let ((root (context-navigator--detect-root)))
    (if (context-navigator--sidebar-visible-p (selected-frame))
        (progn
          (context-navigator--hide-in-frame (selected-frame))
          (context-navigator--state-put :sidebar-window nil)
          (setq context-navigator--sidebar-open-global nil))
      (setq context-navigator--sidebar-open-global t)
      (context-navigator--state-put :current-project-root root)
      (context-navigator--ensure-sidebar)
      ;; Async load to avoid blocking UI; shows “Loading…” immediately.
      (context-navigator--load-context-async root))))

;;;###autoload
(defun context-navigator-show ()
  "Show the context-navigator sidebar in the current frame and refresh."
  (interactive)
  (let ((root (context-navigator--detect-root)))
    (setq context-navigator--sidebar-open-global t)
    (context-navigator--state-put :current-project-root root)
    (context-navigator--ensure-sidebar)
    ;; Async load with loading indicator
    (context-navigator--load-context-async root)))

;;;###autoload
(defun context-navigator-quit ()
  "Hide the context-navigator sidebar in the current frame."
  (interactive)
  (setq context-navigator--sidebar-open-global nil)
  (context-navigator--hide-in-frame (selected-frame))
  ;; Cancel timers to avoid late refreshes after quitting.
  (when-let ((tm (context-navigator--state-get :refresh-timer)))
    (when (timerp tm) (cancel-timer tm)))
  (context-navigator--state-put :refresh-timer nil)
  (context-navigator--cancel-context-load)
  (context-navigator--state-put :sidebar-window nil))

;;;###autoload
(defun context-navigator-help ()
  "Show short help in the echo area."
  (interactive)
  (message "GPTel Navigator: RET=goto, SPC=preview, t=toggle, d=disable, C-d=delete, r=refresh, q=quit, TAB/S-TAB=navigate"))

;; ----------------------------------------------------------------------------
;; Transient menu
;; ----------------------------------------------------------------------------

;;;###autoload (autoload 'context-navigator-menu "context-navigator" nil t)
(transient-define-prefix context-navigator-menu ()
  "Main menu for context-navigator."
  ["GPTel Navigator"
   ["View"
    ("n" "Toggle sidebar" context-navigator-toggle)
    ("r" "Refresh context" context-navigator-refresh)
    ("u" "Unload project (global context)" context-navigator-unload)]
   ["Navigate"
    ("g" "Goto item" context-navigator-goto-item
     :if (lambda () (context-navigator--state-get :sidebar-window)))
    ("p" "Preview item" context-navigator-preview-item
     :if (lambda () (context-navigator--state-get :sidebar-window)))]
   ["Settings"
    ("?" "Help" context-navigator-help)
    ("q" "Quit" context-navigator-quit
     :if (lambda () (context-navigator--state-get :sidebar-window)))]])

;; ----------------------------------------------------------------------------
;; Global minor mode
;; ----------------------------------------------------------------------------

;;;###autoload
(define-minor-mode context-navigator-mode
  "Global minor mode for GPTel Navigator."
  :global t
  :lighter " 🚀"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c in") #'context-navigator-menu)
            (define-key map (kbd "C-c il") #'context-navigator-context-load)
            (define-key map (kbd "C-c is") #'context-navigator-context-save)
            (define-key map (kbd "C-c ic") #'context-navigator-toggle)
            map)
  (if context-navigator-mode
      (progn
        ;; Keep it lean; no post-command hooks.
        (context-navigator--setup-context-advice)
        (when context-navigator-autosave
          (context-navigator--setup-project-context-save-advice)))
    (context-navigator--teardown-context-advice)
    (unless (bound-and-true-p context-navigator-autoload)
      (context-navigator--teardown-project-context-save-advice))))

;; ----------------------------------------------------------------------------
;; Project context serialization and autoload
;; ----------------------------------------------------------------------------

(defun context-navigator--ensure-dir (dir)
  "Ensure DIR exists."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun context-navigator--context-file (root)
  "Return path to context file for ROOT or the global one."
  (if (and root (stringp root))
      (expand-file-name context-navigator-context-file-name
                        (expand-file-name context-navigator-dir-name root))
    (expand-file-name context-navigator-context-file-name context-navigator-global-dir)))

(defun context-navigator--project-relativize (path root)
  "Return PATH relative to ROOT if PATH is inside ROOT."
  (let ((abs (expand-file-name path)))
    (if (and root (file-in-directory-p abs root))
        (file-relative-name abs root)
      abs)))

(defun context-navigator--project-abspath (path root)
  "Return absolute path for PATH relative to ROOT when not absolute."
  (if (and root (not (file-name-absolute-p path)))
      (expand-file-name path root)
    (expand-file-name path)))

(defun context-navigator--serialize-context (&optional root)
  "Serialize internal model into a portable list for saving.
Format (v2):
- (:file RELPATH [:mime MIME] :enabled t/nil)
- (:buffer RELPATH :regions ((BEG . END) ...) :enabled t/nil) ; only file-backed buffers
Non-file buffers are ignored."
  (let ((items (context-navigator--model-items))
        out)
    (dolist (it items)
      (pcase (context-navigator-item-type it)
        ('file
         (let* ((path (context-navigator-item-path it)))
           (when path
             (push (list :file (context-navigator--project-relativize path root)
                         :enabled (and (context-navigator-item-enabled it) t))
                   out))))
        ('buffer
         (with-current-buffer (context-navigator-item-buffer it)
           (when-let ((path buffer-file-name))
             ;; Store as file-level entry (full buffer)
             (push (list :file (context-navigator--project-relativize path root)
                         :enabled (and (context-navigator-item-enabled it) t))
                   out))))
        ('selection
         (let ((buf (context-navigator-item-buffer it))
               (s (context-navigator-item-start it))
               (e (context-navigator-item-end it)))
           (when (and (buffer-live-p buf) s e)
             (with-current-buffer buf
               (when-let ((path buffer-file-name))
                 (push (list :buffer (context-navigator--project-relativize path root)
                             :regions (list (cons s e))
                             :enabled (and (context-navigator-item-enabled it) t))
                       out))))))))
    (nreverse out)))

(defun context-navigator--read-file-sexp (file)
  "Read first sexp from FILE, returning it or nil on error."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (read (current-buffer))
        (error nil)))))

;;;###autoload
(defun context-navigator-context-save (&optional root)
  "Save current model (enabled/disabled) for project ROOT (or global if ROOT nil)."
  (interactive)
  (let* ((root (or root (context-navigator--detect-root)))
         (dir (if root
                  (expand-file-name context-navigator-dir-name root)
                context-navigator-global-dir))
         (file (context-navigator--context-file root))
         (data (context-navigator--serialize-context root)))
    (context-navigator--ensure-dir dir)
    (with-temp-file file
      (insert ";; Auto-generated by context-navigator. Do not edit manually.\n")
      (prin1 data (current-buffer))
      (insert "\n"))
    (when (called-interactively-p 'interactive)
      (message "Saved context (with enabled flags) to %s" file))
    file))

;;;###autoload
(defun context-navigator-context-load (&optional root)
  "Load project context for ROOT (or global) into the model and apply to GPTel."
  (interactive)
  (let* ((root (or root (context-navigator--detect-root)))
         (file (context-navigator--context-file root))
         (spec (context-navigator--read-file-sexp file)))
    (if spec
        (progn
          (context-navigator--state-put :current-project-root root)
          ;; Build items from spec (reuse async loader logic)
          (let ((context-navigator--in-context-load t)
                (context-navigator--inhibit-refresh t))
            (context-navigator--load-context-async root))
          (when (called-interactively-p 'interactive)
            (message "Loaded context (model) from %s" file))
          t)
      ;; No spec: clear model and GPTel
      (let ((context-navigator--in-context-load t)
            (context-navigator--inhibit-refresh t))
        (context-navigator--state-put :model-items '())
        (context-navigator--state-put :model-index (make-hash-table :test 'equal))
        (ignore-errors (gptel-context-remove-all)))
      (context-navigator-refresh)
      (when (called-interactively-p 'interactive)
        (message "No saved context at %s — cleared current model and GPTel" file))
      nil)))

;;;###autoload
(defun context-navigator-unload ()
  "Switch to global context: load global model and apply to GPTel, or clear if absent.
Also set GPTel chat buffer directory to filesystem root (/)."
  (interactive)
  (let ((cur-root (context-navigator--detect-root)))
    ;; Prevent immediate autoload switch back to CUR-ROOT
    (setq context-navigator--last-project-root cur-root)
    ;; Mark UI/state as global (non-string to avoid being treated as a dir)
    (context-navigator--state-put :current-project-root :global))
  (let* ((file (context-navigator--context-file nil))
         (spec (context-navigator--read-file-sexp file)))
    (if spec
        (progn
          ;; Build model from global spec and apply
          (let ((context-navigator--in-context-load t)
                (context-navigator--inhibit-refresh t))
            (context-navigator--load-context-async nil))
          (when (called-interactively-p 'interactive)
            (message "Loaded global context from %s" file)))
      ;; Clear model and GPTel
      (let ((context-navigator--in-context-load t)
            (context-navigator--inhibit-refresh t))
        (context-navigator--state-put :model-items '())
        (context-navigator--state-put :model-index (make-hash-table :test 'equal))
        (ignore-errors (gptel-context-remove-all)))
      (when (called-interactively-p 'interactive)
        (message "No global context at %s — cleared current model and GPTel" file))))
  ;; Switch GPTel chat buffer `default-directory' to filesystem root
  (when-let ((gtbuf (context-navigator--find-gptel-buffer)))
    (with-current-buffer gtbuf
      (context-navigator--state-put :prev-gptel-default-directory default-directory)
      (setq default-directory (expand-file-name "/"))))
  (when (buffer-live-p (context-navigator--state-get :sidebar-buffer))
    (context-navigator-refresh))
  t)

;; ----------------------------------------------------------------------------
;; Project auto-load/save global mode
;; ----------------------------------------------------------------------------

(defun context-navigator--maybe-load-project-context (&rest _)
  "Auto-load context when the active buffer's project root changes (throttled).

The context switch only occurs for file-visiting buffers and GPTel chat buffers,
and is ignored for Dired buffers.

If there is no project for the active buffer, load the global context from
`context-navigator-global-dir'; if it doesn't exist, clear the context."
  (unless (or context-navigator--in-context-load
              (context-navigator--state-get :context-loading-p))
    (when-let ((buf (context-navigator--pc-pick-active-buffer)))
      (with-current-buffer buf
        (when (and (not (derived-mode-p 'dired-mode))
                   (or buffer-file-name (derived-mode-p 'gptel-mode)))
          (let* ((now (float-time))
                 (last context-navigator--last-context-switch-time)
                 (throttle-ok (>= (- now last) context-navigator-context-switch-interval))
                 (root (context-navigator--detect-root)))
            (when (and throttle-ok
                       (not (equal root context-navigator--last-project-root)))
              (setq context-navigator--last-context-switch-time now)
              (context-navigator--load-context-async root))))))))

(defun context-navigator--window-buffer-change-hook (&rest _)
  "Hook wrapper to maybe load project context and refresh sidebar highlight.
Reentrancy-guarded to avoid triggering itself via window/config changes."
  (unless context-navigator--in-window-change-hook
    (let ((context-navigator--in-window-change-hook t))
      (context-navigator--maybe-load-project-context)
      ;; Re-render sidebar to update highlight for the active buffer/file,
      ;; but skip while bulk operations suppress refreshes.
      (let ((buf (context-navigator--state-get :sidebar-buffer)))
        (when (and (buffer-live-p buf)
                   (not context-navigator--inhibit-refresh))
          (with-current-buffer buf
            (context-navigator--render-sidebar)))))))

(defun context-navigator--advice-save-context (&rest _)
  "Auto-save project context after mutating gptel context."
  (when (and (not context-navigator--in-context-load)
             (or context-navigator-autosave
                 (bound-and-true-p context-navigator-autoload)))
    (ignore-errors
      (context-navigator-context-save (context-navigator--detect-root)))))

(defun context-navigator--setup-project-context-save-advice ()
  "Install advice to auto-save project context."
  (dolist (fn context-navigator--context-advised-fns)
    (when (fboundp fn)
      (unless (advice-member-p #'context-navigator--advice-save-context fn)
        (advice-add fn :after #'context-navigator--advice-save-context)))))

(defun context-navigator--teardown-project-context-save-advice ()
  "Remove advice that auto-saves project context."
  (dolist (fn context-navigator--context-advised-fns)
    (when (advice-member-p #'context-navigator--advice-save-context fn)
      (advice-remove fn #'context-navigator--advice-save-context))))

(defun context-navigator--setup-autoload-hooks ()
  "Enable automatic context loading for the current buffer/project."
  (context-navigator--setup-project-context-save-advice)
  ;; Single unified hook that both maybe-loads and updates sidebar highlight.
  (add-hook 'buffer-list-update-hook #'context-navigator--window-buffer-change-hook)
  (when (boundp 'window-selection-change-functions)
    (add-hook 'window-selection-change-functions #'context-navigator--window-buffer-change-hook))
  ;; Initial run for current buffer
  (context-navigator--window-buffer-change-hook))

(defun context-navigator--teardown-autoload-hooks ()
  "Disable automatic context loading hooks."
  (remove-hook 'buffer-list-update-hook #'context-navigator--window-buffer-change-hook)
  (when (boundp 'window-selection-change-functions)
    (remove-hook 'window-selection-change-functions #'context-navigator--window-buffer-change-hook)))

;; Initialize autoload hooks if the option is enabled at load time.
(when (bound-and-true-p context-navigator-autoload)
  (ignore-errors (context-navigator--setup-autoload-hooks)))

(provide 'context-navigator)
;; Ensure hard deletion semantics for gptel-context-remove-all: purge model and save.
(defun context-navigator--around-remove-all (orig-fn &rest args)
  "Ensure gptel-context-remove-all purges model, sidebar, and saved context."
  (let ((context-navigator--inhibit-refresh t))
    (prog1 (apply orig-fn args)
      ;; Clear our model completely (hard delete)
      (context-navigator--model-set-items nil)
      ;; Persist to disk if autosave is enabled
      (when context-navigator-autosave
        (context-navigator-context-save))
      ;; Update UI
      (context-navigator--debounced-refresh))))

;;; context-navigator.el ends here
