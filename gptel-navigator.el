;;; gptel-navigator.el --- Modern context manager for gptel -*- lexical-binding: t; -*-

;; Version: 1.0.0
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "30.1") (gptel "0.8.0") (transient "0.4.0"))
;; Homepage: https://github.com/gptel-extensions/gptel-navigator
;; Keywords: tools, convenience, ai

;;; Commentary:
;;
;; gptel-navigator provides a modern context manager and sidebar UI for gptel.
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
;; - gptel-navigator-toggle / gptel-navigator-show / gptel-navigator-quit
;; - gptel-navigator-refresh
;; - gptel-navigator-menu (transient)
;; - gptel-navigator-mode (global minor mode)
;; - gptel-navigator-context-save / load
;; - gptel-navigator-autoload (option)
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
(defgroup gptel-navigator nil
  "Modern context manager for gptel."
  :group 'convenience
  :group 'gptel
  :prefix "gptel-navigator-")

(defcustom gptel-navigator-sidebar-width 35
  "Width of the gptel-navigator sidebar window."
  :type 'integer
  :group 'gptel-navigator)

(defcustom gptel-navigator-auto-refresh t
  "Automatically refresh sidebar when gptel context changes."
  :type 'boolean
  :group 'gptel-navigator)

(defcustom gptel-navigator-show-line-numbers nil
  "Show line numbers for buffer selections in context."
  :type 'boolean
  :group 'gptel-navigator)

(defcustom gptel-navigator-autosave t
  "When non-nil, automatically save the project/global gptel context to disk when it changes.
This installs lightweight advice on gptel-context mutators. If
`gptel-navigator-autoload' is enabled, this option is effectively
on regardless of its value."
  :type 'boolean
  :group 'gptel-navigator
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'gptel-navigator--setup-project-context-save-advice)
           (if val
               (gptel-navigator--setup-project-context-save-advice)
             (unless (bound-and-true-p gptel-navigator-autoload)
               (when (fboundp 'gptel-navigator--teardown-project-context-save-advice)
                 (gptel-navigator--teardown-project-context-save-advice)))))))

(defcustom gptel-navigator-autoload t
  "When non-nil, automatically load the per-project/global gptel context
for the current buffer when switching buffers or projects.

Enabling this also ensures autosave advice is active regardless of
`gptel-navigator-autosave'."
  :type 'boolean
  :group 'gptel-navigator
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (when (fboundp 'gptel-navigator--setup-autoload-hooks)
               (gptel-navigator--setup-autoload-hooks))
           (when (fboundp 'gptel-navigator--teardown-autoload-hooks)
             (gptel-navigator--teardown-autoload-hooks))
           (unless (bound-and-true-p gptel-navigator-autosave)
             (when (fboundp 'gptel-navigator--teardown-project-context-save-advice)
               (gptel-navigator--teardown-project-context-save-advice))))))


;; ----------------------------------------------------------------------------
;; Autosave advice and basic context persistence (safe placeholders)
;; ----------------------------------------------------------------------------

(defvar gptel-navigator--advices-installed nil
  "Internal flag: whether autosave advices for gptel-context have been installed early.
This exists to avoid calling gptel-navigator--state-get before it is defined
during load/eval order. The full state plist is synchronized when available.")

(defun gptel-navigator--maybe-autosave-context (&rest _)
  "Autosave project/global context when appropriate."
  (when (and gptel-navigator-autosave
             (not gptel-navigator--in-context-load))
    (ignore-errors
      (gptel-navigator-context-save))))

(defun gptel-navigator--setup-project-context-save-advice ()
  "Install advice on gptel-context mutators to autosave context."
  (unless gptel-navigator--advices-installed
    (dolist (fn '(gptel-context-add
                  gptel-context-remove
                  gptel-context-clear
                  gptel-context--set
                  gptel-context-set
                  gptel-context-add-file
                  gptel-context-remove))
      (when (fboundp fn)
        (advice-add fn :after #'gptel-navigator--maybe-autosave-context)))
    (setq gptel-navigator--advices-installed t)
    (when (fboundp 'gptel-navigator--state-put)
      (gptel-navigator--state-put :advices-installed t))))

(defun gptel-navigator--teardown-project-context-save-advice ()
  "Remove autosave advice previously installed."
  (when gptel-navigator--advices-installed
    (dolist (fn '(gptel-context-add
                  gptel-context-remove
                  gptel-context-clear
                  gptel-context--set
                  gptel-context-set
                  gptel-context-add-file
                  gptel-context-remove))
      (when (fboundp fn)
        (advice-remove fn #'gptel-navigator--maybe-autosave-context)))
    (setq gptel-navigator--advices-installed nil)
    (when (fboundp 'gptel-navigator--state-put)
      (gptel-navigator--state-put :advices-installed nil))))

(defun gptel-navigator--context-file ()
  "Return the default file path for saving the context."
  (let* ((proj (when (fboundp 'project-current)
                 (ignore-errors (project-current))))
         (root (when (and proj (fboundp 'project-root))
                 (ignore-errors (file-name-as-directory (project-root proj)))))
         (dir (if root
                  (expand-file-name gptel-navigator-dir-name root)
                gptel-navigator-global-dir)))
    (expand-file-name gptel-navigator-context-file-name dir)))

(defun gptel-navigator--context-snapshot ()
  "Return a serializable snapshot of the current gptel context.
Only includes file-backed entries to keep it robust and readable."
  (let ((alist (gptel-navigator--context-alist))
        out)
    (dolist (entry alist)
      (pcase entry
        ;; Buffer + overlays: reduce to file or file-backed selections (as files)
        (`(,(and buf (pred bufferp)) . ,ovs)
         (when (buffer-live-p buf)
           (let ((path (buffer-local-value 'buffer-file-name buf)))
             (dolist (ov ovs)
               (when (and (overlayp ov) (overlay-start ov) (overlay-end ov))
                 (let ((start (overlay-start ov))
                       (end (overlay-end ov)))
                   (when (and path
                              (= start (with-current-buffer buf (point-min)))
                              (= end (with-current-buffer buf (point-max))))
                     (push `(:type file :path ,path) out))))))))
        ;; (path . props) or bare path
        (`(,(and path (pred stringp)) . ,_props)
         (push `(:type file :path ,path) out))
        ((and (pred stringp) path)
         (push `(:type file :path ,path) out))))
    (nreverse out)))

(defun gptel-navigator-context-save (&optional file)
  "Persist a lightweight gptel context snapshot to FILE.
Returns the file path on success."
  (interactive)
  (let* ((file (or file (gptel-navigator--context-file)))
         (dir (and file (file-name-directory file)))
         (items (gptel-navigator--context-snapshot)))
    (when (and file dir)
      (ignore-errors (make-directory dir t))
      (with-temp-file file
        (let ((print-length nil)
              (print-level nil))
          (prin1
           `(:saved-at ,(current-time-string)
                       :emacs ,emacs-version
                       :items ,items)
           (current-buffer)))))
    file))

(defun gptel-navigator-context-load (&optional file)
  "Load a lightweight gptel context snapshot from FILE.
Restores only file entries into the gptel context."
  (interactive)
  (let ((file (or file (gptel-navigator--context-file))))
    (when (and file (file-readable-p file))
      (let* ((data (with-temp-buffer
                     (insert-file-contents file)
                     (goto-char (point-min))
                     (read (current-buffer))))
             (items (plist-get data :items))
             (files (delq nil
                          (mapcar (lambda (it)
                                    (let ((type (plist-get it :type))
                                          (path (plist-get it :path)))
                                      (when (and (eq type 'file)
                                                 (stringp path)
                                                 (file-readable-p path))
                                        path)))
                                  items))))
        (when files
          (condition-case _err
              (cond
               ((fboundp 'gptel-context--set) (gptel-context--set files))
               ((fboundp 'gptel-context-set) (gptel-context-set files))
               ((boundp 'gptel-context--alist)
                (setq gptel-context--alist files)))
            (error nil))))
      t)))

(defcustom gptel-navigator-max-filename-length 25
  "Maximum length for displayed item names in sidebar."
  :type 'integer
  :group 'gptel-navigator)

(defcustom gptel-navigator-enable-icons t
  "When non-nil, show icons in the sidebar (pro-tabs / all-the-icons if available)."
  :type 'boolean
  :group 'gptel-navigator)

(defcustom gptel-navigator-dir-name ".gptel-navigator"
  "Service directory in project root for storing serialized context."
  :type 'string
  :group 'gptel-navigator)

(defcustom gptel-navigator-context-file-name "context.el"
  "Filename to store serialized project/global context."
  :type 'string
  :group 'gptel-navigator)

(defcustom gptel-navigator-global-dir (expand-file-name "~/.gptel-navigator/")
  "Global directory to store context when outside of a project."
  :type 'directory
  :group 'gptel-navigator)

(defcustom gptel-navigator-context-switch-interval 1.5
  "Minimum interval (seconds) between automatic project context switches."
  :type 'number
  :group 'gptel-navigator)

(defcustom gptel-navigator-context-load-batch-size 8
  "How many context entries to load per timer tick during async loading."
  :type 'integer
  :group 'gptel-navigator)

;; ----------------------------------------------------------------------------
;; Data model and state
;; ----------------------------------------------------------------------------

(cl-defstruct gptel-navigator-item
  type          ; 'file | 'buffer | 'selection
  name          ; string to display
  path          ; absolute file path (for files)
  buffer        ; buffer object (for buffers/selections)
  start         ; selection start (selection only)
  end           ; selection end (selection only)
  size          ; size in chars/bytes
  icon          ; icon string
  description)  ; additional info

(defvar gptel-navigator--state
  (list :sidebar-buffer nil
        :sidebar-window nil
        :context-items '()
        :selected-item nil
        :refresh-timer nil
        :advices-installed nil
        :current-project-root nil
        :last-refresh-echo 0.0
        :context-loading-p nil
        :context-load-timer nil
        :context-load-queue nil)
  "Global state (plist) for gptel-navigator.")

(defvar gptel-navigator--inhibit-refresh nil
  "Non-nil to temporarily inhibit auto refresh.")

(defvar gptel-navigator--in-context-load nil
  "Non-nil while loading project context (suppresses recursion/auto-save).")

(defvar gptel-navigator--hooks-installed nil
  "Internal: whether project-switch hooks are installed.")

(defvar gptel-navigator--last-project-root nil
  "Last project root for which context was auto-loaded.")

(defvar gptel-navigator--last-context-switch-time 0
  "Timestamp (float-time) of last context switch.")

(defvar gptel-navigator--sidebar-open-global nil
  "Non-nil means the navigator sidebar should be visible in all frames/tabs.")

(defvar gptel-navigator--visibility-hooks-installed nil
  "Internal: whether visibility sync hooks are installed.")

(defvar gptel-navigator--in-window-change-hook nil
  "Reentrancy guard for gptel-navigator--window-buffer-change-hook.")

(defun gptel-navigator--state-get (key)
  "Get KEY from the global state."
  (plist-get gptel-navigator--state key))

(defun gptel-navigator--state-put (key value)
  "Set KEY in the global state to VALUE."
  (setq gptel-navigator--state (plist-put gptel-navigator--state key value)))

(defun gptel-navigator--cancel-context-load ()
  "Cancel any ongoing async context load timer and clear flags."
  (when-let ((tm (gptel-navigator--state-get :context-load-timer)))
    (when (timerp tm)
      (cancel-timer tm)))
  (gptel-navigator--state-put :context-load-timer nil)
  (gptel-navigator--state-put :context-load-queue nil)
  (gptel-navigator--state-put :context-loading-p nil))

(defun gptel-navigator--process-context-load-queue ()
  "Process a batch from the async context load queue."
  (let* ((batch (max 1 gptel-navigator-context-load-batch-size))
         (q (gptel-navigator--state-get :context-load-queue)))
    (if (null q)
        (progn
          ;; Finish: clear flags, record root, refresh UI.
          (gptel-navigator--cancel-context-load)
          (setq gptel-navigator--last-project-root
                (gptel-navigator--state-get :current-project-root))
          (setq gptel-navigator--inhibit-refresh nil)
          (gptel-navigator-refresh))
      (let ((gptel-navigator--in-context-load t)
            (gptel-navigator--inhibit-refresh t))
        (dotimes (_ batch)
          (when q
            (let ((fn (car q)))
              (setq q (cdr q))
              (ignore-errors (funcall fn)))))
        (gptel-navigator--state-put :context-load-queue q)
        ;; If finished in this tick, finalize immediately.
        (when (null q)
          (gptel-navigator--cancel-context-load)
          (setq gptel-navigator--last-project-root
                (gptel-navigator--state-get :current-project-root))
          (setq gptel-navigator--inhibit-refresh nil)
          (gptel-navigator-refresh))))))

(defun gptel-navigator--load-context-async (root)
  "Start non-blocking load of context for ROOT, rendering \"Loading…\"."
  (gptel-navigator--cancel-context-load)
  (gptel-navigator--state-put :current-project-root root)
  (gptel-navigator--state-put :context-loading-p t)
  ;; Show loading immediately
  (when-let ((buf (gptel-navigator--state-get :sidebar-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (gptel-navigator--render-sidebar))))
  ;; Build queue
  (let* ((file (gptel-navigator--context-file root))
         (spec (gptel-navigator--read-file-sexp file))
         (queue nil))
    ;; First step: clear current context
    (push (lambda () (ignore-errors (gptel-context-remove-all))) queue)
    (when spec
      (dolist (it spec)
        (pcase it
          (`(:file ,rel . ,_rest)
           (let ((abs (gptel-navigator--project-abspath rel root)))
             (push (lambda () (ignore-errors (gptel-context-add-file abs))) queue)))
          (`(:buffer ,rel :regions ,regions . ,_rest)
           (let* ((abs (gptel-navigator--project-abspath rel root)))
             (push
              (lambda ()
                (let ((buf (ignore-errors (find-file-noselect abs))))
                  (when (buffer-live-p buf)
                    (dolist (pair regions)
                      (when (and (consp pair) (numberp (car pair)) (numberp (cdr pair)))
                        (ignore-errors
                          (gptel-context--add-region buf (car pair) (cdr pair) t)))))))
              queue))))))
    (setq queue (nreverse queue))
    (gptel-navigator--state-put :context-load-queue queue)
    ;; Schedule repeating timer to process queue
    (gptel-navigator--state-put
     :context-load-timer
     (run-at-time 0.05 0.03 #'gptel-navigator--process-context-load-queue))))

;; ----------------------------------------------------------------------------
;; Utilities
;; ----------------------------------------------------------------------------

(defun gptel-navigator--truncate (s n)
  "Truncate string S to N chars with ellipsis."
  (if (and (stringp s) (> (length s) n))
      (concat (substring s 0 (max 0 (- n 3))) "...")
    (or s "")))

(defun gptel-navigator--guess-mode-for-file (path)
  "Guess major mode for PATH without visiting permanently."
  (when (and (stringp path) (not (file-directory-p path)))
    (with-temp-buffer
      (setq buffer-file-name path)
      (let ((enable-local-variables nil)
            (enable-local-eval nil)
            (inhibit-message t))
        (set-auto-mode))
      major-mode)))

(defun gptel-navigator--icon-for (type &optional buffer path mode backend)
  "Return an icon string for TYPE using optional BUFFER/PATH/MODE hints.

Preference:
1) pro-tabs icon for buffer (if available)
2) all-the-icons by major mode
3) all-the-icons by filename
4) fallback emoji/symbol"
  (when gptel-navigator-enable-icons
    (let* ((backend (or backend 'tab-line))
           (use-pro-tabs (fboundp 'pro-tabs--icon)))
      (unless (featurep 'all-the-icons)
        (ignore-errors (require 'all-the-icons nil t)))
      (pcase type
        ('file
         (or
          (when (and use-pro-tabs (buffer-live-p buffer))
            (ignore-errors (pro-tabs--icon buffer backend)))
          (when (featurep 'all-the-icons)
            (let* ((mm (or mode
                           (and (buffer-live-p buffer) (buffer-local-value 'major-mode buffer))
                           (gptel-navigator--guess-mode-for-file path))))
              (or (ignore-errors (all-the-icons-icon-for-mode mm))
                  (when (and path (stringp path))
                    (ignore-errors
                      (all-the-icons-icon-for-file (file-name-nondirectory path)))))))
          "•"))
        ((or 'buffer 'selection)
         (or
          (when (and use-pro-tabs (buffer-live-p buffer))
            (ignore-errors (pro-tabs--icon buffer backend)))
          (when (and (featurep 'all-the-icons)
                     (or buffer mode))
            (let ((mm (or mode (and (buffer-live-p buffer)
                                    (buffer-local-value 'major-mode buffer)))))
              (ignore-errors (all-the-icons-icon-for-mode mm))))
          (if (eq type 'selection) "✂" "•")))
        (_ "•")))))

(defun gptel-navigator--normalize-face (face)
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

(defun gptel-navigator--insert-with-face (str face)
  "Insert STR with FACE (normalized) into current buffer."
  (let ((f (gptel-navigator--normalize-face face)))
    (if f
        (widget-insert (propertize str 'face f))
      (widget-insert str))))

;; ----------------------------------------------------------------------------
;; Context collectors
;; ----------------------------------------------------------------------------

(defun gptel-navigator--context-alist ()
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

(defun gptel-navigator--uniq-items (items)
  "Deduplicate ITEMS by key: selection=(buf+start+end), file=path, buffer=bufname."
  (let ((seen (make-hash-table :test 'equal))
        out)
    (dolist (it items)
      (let* ((type (gptel-navigator-item-type it))
             (key (pcase type
                    ('selection
                     (format "sel:%s:%s-%s"
                             (and (buffer-live-p (gptel-navigator-item-buffer it))
                                  (buffer-name (gptel-navigator-item-buffer it)))
                             (gptel-navigator-item-start it)
                             (gptel-navigator-item-end it)))
                    ('file (or (gptel-navigator-item-path it)
                               (gptel-navigator-item-name it)))
                    (_ (or (and (buffer-live-p (gptel-navigator-item-buffer it))
                                (buffer-name (gptel-navigator-item-buffer it)))
                           (gptel-navigator-item-name it))))))
        (unless (gethash key seen)
          (puthash key t seen)
          (push it out))))
    (nreverse out)))

(defun gptel-navigator--collector-current (buffer)
  "Collect context from current BUFFER (not enabled by default)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((file (buffer-file-name))
             (mode major-mode)
             (buf buffer)
             items)
        (cond
         ((and file (use-region-p))
          (push (make-gptel-navigator-item
                 :type 'selection
                 :name (format "%s (selection)" (file-name-nondirectory file))
                 :path file
                 :buffer buf
                 :start (region-beginning)
                 :end (region-end)
                 :size (- (region-end) (region-beginning))
                 :icon (gptel-navigator--icon-for 'selection buf file mode)
                 :description (if gptel-navigator-show-line-numbers
                                  (format "Lines %d-%d"
                                          (line-number-at-pos (region-beginning))
                                          (line-number-at-pos (region-end)))
                                (format "%d chars" (- (region-end) (region-beginning)))))
                items))
         (file
          (push (make-gptel-navigator-item
                 :type 'file
                 :name (file-name-nondirectory file)
                 :path file
                 :buffer buf
                 :size (buffer-size)
                 :icon (gptel-navigator--icon-for 'file buf file mode)
                 :description (format "%s (%d chars)"
                                      (or (file-name-directory file) "")
                                      (buffer-size)))
                items))
         ((use-region-p)
          (push (make-gptel-navigator-item
                 :type 'selection
                 :name (format "%s (selection)" (buffer-name buf))
                 :buffer buf
                 :start (region-beginning)
                 :end (region-end)
                 :size (- (region-end) (region-beginning))
                 :icon (gptel-navigator--icon-for 'selection buf nil mode)
                 :description (if gptel-navigator-show-line-numbers
                                  (format "Lines %d-%d"
                                          (line-number-at-pos (region-beginning))
                                          (line-number-at-pos (region-end)))
                                (format "%d chars" (- (region-end) (region-beginning)))))
                items))
         (t
          (push (make-gptel-navigator-item
                 :type 'buffer
                 :name (buffer-name buf)
                 :buffer buf
                 :size (buffer-size)
                 :icon (gptel-navigator--icon-for 'buffer buf nil mode)
                 :description (format "%d chars" (buffer-size)))
                items)))
        items))))

(defun gptel-navigator--collector-visible (_buffer)
  "Collect context from all visible windows' buffers (not enabled by default)."
  (let (items)
    (dolist (w (window-list nil 'no-mini))
      (let ((b (window-buffer w)))
        (when (buffer-live-p b)
          (setq items (nconc items (or (gptel-navigator--collector-current b) '()))))))
    (gptel-navigator--uniq-items items)))

(defun gptel-navigator--collector-gptel (_buffer)
  "Collect items from gptel-context."
  (let ((alist (gptel-navigator--context-alist))
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
                    (icon (gptel-navigator--icon-for type buf))
                    (desc (with-current-buffer buf
                            (if (eq type 'buffer)
                                (format "%d chars" (buffer-size))
                              (if gptel-navigator-show-line-numbers
                                  (format "Lines %d-%d"
                                          (line-number-at-pos start)
                                          (line-number-at-pos end))
                                (format "%d chars" (- end start)))))))
               (push (make-gptel-navigator-item
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
           (let* ((attrs (file-attributes path))
                  (size (file-attribute-size attrs))
                  (name (file-name-nondirectory path))
                  (icon (gptel-navigator--icon-for 'file nil path))
                  (desc (if-let ((mime (plist-get props :mime)))
                            (format "%s (%s, %d bytes)"
                                    (or (file-name-directory path) "") mime size)
                          (format "%s (%d bytes)"
                                  (or (file-name-directory path) "") size))))
             (push (make-gptel-navigator-item
                    :type 'file
                    :name name
                    :path path
                    :size size
                    :icon icon
                    :description desc)
                   items))))
        (`(,path)
         (when (file-exists-p path)
           (let* ((attrs (file-attributes path))
                  (size (file-attribute-size attrs))
                  (name (file-name-nondirectory path))
                  (icon (gptel-navigator--icon-for 'file nil path))
                  (desc (format "%s (%d bytes)"
                                (or (file-name-directory path) "") size)))
             (push (make-gptel-navigator-item
                    :type 'file
                    :name name
                    :path path
                    :size size
                    :icon icon
                    :description desc)
                   items))))))
    (nreverse items)))

(defun gptel-navigator--detect-root ()
  "Detect current project root via project.el or projectile."
  (or
   (when (require 'project nil t)
     (when-let ((proj (project-current nil)))
       (if (fboundp 'project-root)
           (project-root proj)
         (car (project-roots proj)))))
   (when (require 'projectile nil t)
     (ignore-errors (projectile-project-root)))))

(defun gptel-navigator--pc-special-buffer-p (buf)
  "Return non-nil if BUF is special/temporary."
  (when (buffer-live-p buf)
    (let ((n (buffer-name buf)))
      (or (string-prefix-p " " n)
          (string-prefix-p "*" n)
          (eq (buffer-local-value 'major-mode buf) 'gptel-navigator-sidebar-mode)))))

(defun gptel-navigator--pc-pick-active-buffer ()
  "Pick most relevant non-special buffer from visible windows."
  (or
   (let ((b (window-buffer (selected-window))))
     (unless (gptel-navigator--pc-special-buffer-p b) b))
   (catch 'found
     (dolist (w (window-list nil 'no-mini))
       (let ((b (window-buffer w)))
         (unless (gptel-navigator--pc-special-buffer-p b)
           (throw 'found b))))
     nil)
   (let ((b (current-buffer)))
     (unless (gptel-navigator--pc-special-buffer-p b) b))))

(defun gptel-navigator--project-root-of-buffer (buf)
  "Return project root of BUF using project.el or projectile."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (or
       (gptel-navigator--detect-root)
       nil))))

(defun gptel-navigator--collector-project-file (_buffer)
  "Collector that reads ROOT/.gptel-navigator/context.el and returns file items."
  (let* ((root (or (gptel-navigator--state-get :current-project-root)
                   (when-let ((b (gptel-navigator--pc-pick-active-buffer)))
                     (gptel-navigator--project-root-of-buffer b)))))
    (when (and root (file-directory-p root))
      (let* ((file (expand-file-name gptel-navigator-context-file-name
                                     (expand-file-name gptel-navigator-dir-name root))))
        (when (file-readable-p file)
          (condition-case _err
              (let* ((sexp (with-temp-buffer
                             (insert-file-contents file)
                             (goto-char (point-min))
                             (read (current-buffer))))
                     items)
                (dolist (it sexp)
                  (pcase it
                    (`(:file ,rel . ,_rest)
                     (let* ((abs (if (file-name-absolute-p rel)
                                     rel
                                   (expand-file-name rel root))))
                       (when (file-exists-p abs)
                         (let* ((attrs (file-attributes abs))
                                (size (file-attribute-size attrs))
                                (name (file-name-nondirectory abs)))
                           (push (make-gptel-navigator-item
                                  :type 'file
                                  :name name
                                  :path abs
                                  :size size
                                  :icon (gptel-navigator--icon-for 'file nil abs)
                                  :description (format "%s (%d bytes)"
                                                       (or (file-name-directory abs) "")
                                                       size))
                                 items)))))))
                (nreverse items))
            (error nil)))))))

(defvar gptel-navigator-context-collectors
  '(gptel-navigator--collector-project-file
    gptel-navigator--collector-gptel)
  "List of collector functions to aggregate context items.

Each function is called with current buffer and should return a
list of `gptel-navigator-item' objects. Results are deduplicated.")

(defun gptel-navigator--collect-context ()
  "Collect and deduplicate context items from all collectors."
  (let (items)
    (dolist (fn gptel-navigator-context-collectors)
      (when (functionp fn)
        (setq items (nconc items (ignore-errors (funcall fn (current-buffer)))))))
    (gptel-navigator--uniq-items items)))

;; ----------------------------------------------------------------------------
;; Sidebar UI
;; ----------------------------------------------------------------------------

(defvar gptel-navigator-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gptel-navigator-goto-item)
    (define-key map (kbd "SPC") #'gptel-navigator-preview-item)
    (define-key map (kbd "d")   #'gptel-navigator-remove-item-at-point)
    (define-key map (kbd "C-d") #'gptel-navigator-remove-item-at-point)
    (define-key map (kbd "r")   #'gptel-navigator-refresh)
    (define-key map (kbd "q")   #'gptel-navigator-quit)
    (define-key map (kbd "?")   #'gptel-navigator-help)
    (define-key map (kbd "TAB") #'widget-forward)
    (define-key map (kbd "S-TAB") #'widget-backward)
    map)
  "Keymap for gptel-navigator sidebar.")

(define-derived-mode gptel-navigator-sidebar-mode special-mode "gptel-navigator"
  "Major mode for gptel-navigator sidebar."
  (setq buffer-read-only t
        truncate-lines t)
  ;; Disable echo-area/tooltips for help-echo to prevent spam like “Expand node”.
  (setq-local show-help-function #'ignore)
  ;; Also stop help-at-pt (idle tooltip/messages) from printing widget hints like “Expand node”.
  (setq-local help-at-pt-display-when-idle nil)
  (hl-line-mode 1))

(defun gptel-navigator--create-item-widget (item)
  "Create a tree-widget for ITEM."
  (let* ((name (gptel-navigator--truncate
                (gptel-navigator-item-name item)
                gptel-navigator-max-filename-length))
         (raw-icon (gptel-navigator-item-icon item))
         (icon (if (stringp raw-icon) raw-icon (format "%s" (or raw-icon ""))))
         (desc (gptel-navigator-item-description item))
         (type (gptel-navigator-item-type item))
         (sel-buf (gptel-navigator--pc-pick-active-buffer))
         (sel-p (cond
                 ((memq type '(buffer selection))
                  (eq (gptel-navigator-item-buffer item) sel-buf))
                 ((eq type 'file)
                  (let ((p (gptel-navigator-item-path item)))
                    (and (buffer-live-p sel-buf)
                         (with-current-buffer sel-buf
                           (and buffer-file-name
                                (string=
                                 (expand-file-name buffer-file-name)
                                 (and p (expand-file-name p))))))))
                 (t nil)))
         ;; Сделать текущий буфер зелёным (success), без светлого фона
         (button-face (if sel-p 'gptel-navigator-active-buffer 'default)))
    `(tree-widget
      :node (push-button
             :tag ,(format "%s %s" icon name)
             :format "%[%t%]\n"
             :button-face ,button-face
             :help-echo nil
             :notify gptel-navigator--item-action
             :button-data ,item)
      :help-echo nil
      :dynargs gptel-navigator--item-children
      :has-children ,(eq type 'file)
      )))

(defun gptel-navigator--item-action (widget &optional _event)
  "Handle activation on WIDGET."
  (when-let ((item (widget-get widget :button-data)))
    (gptel-navigator--state-put :selected-item item)
    (gptel-navigator-goto-item-internal item)))

(defun gptel-navigator--widget-item-at-point ()
  "Return the gptel-navigator-item at point, traversing widget parents if needed."
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

(defun gptel-navigator--find-gptel-buffer ()
  "Try to find the active GPTel chat buffer."
  (or (gptel-navigator--state-get :gptel-buffer)
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
(defun gptel-navigator-remove-item-at-point ()
  "Remove the context item at point from GPTel context, then refresh sidebar."
  (interactive)
  (let ((item (gptel-navigator--widget-item-at-point)))
    (unless item (user-error "No context item at point"))
    (unless (fboundp 'gptel-context-remove)
      (user-error "gptel-context-remove is not available"))
    (let ((runner
           (lambda ()
             (pcase (gptel-navigator-item-type item)
               ('file
                (let ((path (gptel-navigator-item-path item)))
                  (when path (gptel-context-remove path))))
               ('buffer
                (let ((buf (gptel-navigator-item-buffer item)))
                  (when buf (gptel-context-remove buf))))
               ('selection
                (let ((buf (gptel-navigator-item-buffer item))
                      (start (gptel-navigator-item-start item))
                      (end (gptel-navigator-item-end item)))
                  (if (and buf start end)
                      (gptel-context-remove buf start end)
                    (when buf (gptel-context-remove buf)))))
               (_ (user-error "Unknown item type"))))))
      ;; Если есть GPTel-буфер, выполним внутри него; иначе — глобально.
      (if-let ((gtbuf (gptel-navigator--find-gptel-buffer)))
          (with-current-buffer gtbuf
            (funcall runner))
        (funcall runner)))
    (gptel-navigator-refresh)
    (message "Removed item from GPTel context.")))

(defun gptel-navigator--item-children (tree)
  "Return children for TREE (file details)."
  (let* ((item (widget-get (tree-widget-node tree) :button-data)))
    (when (and item (eq (gptel-navigator-item-type item) 'file))
      (list
       `(item :tag ,(format "📊 Size: %d bytes" (or (gptel-navigator-item-size item) 0))
              :help-echo nil)
       `(item :tag ,(format "📁 Path: %s" (or (gptel-navigator-item-path item) ""))
              :help-echo nil)))))

(defface gptel-navigator-active-buffer
  '((t :foreground "green3" :weight bold))
  "Face for highlighting the current buffer in gptel-navigator sidebar.")

(defcustom gptel-navigator-squelch-tree-widget-echo t
  "When non-nil, silence tree-widget help-echo messages like “Expand node”."
  :type 'boolean
  :group 'gptel-navigator)

(when gptel-navigator-squelch-tree-widget-echo
  (with-eval-after-load 'tree-widget
    ;; Some Emacs builds provide `tree-widget-button-echo' to generate
    ;; “Expand node” / “Collapse node”. If present, override it to return nil.
    (when (fboundp 'tree-widget-button-echo)
      (advice-add 'tree-widget-button-echo :override (lambda (&rest _) nil)))))

(defun gptel-navigator--scrub-help-echo ()
  "Remove help-echo from text and overlays in the sidebar."
  (let ((inhibit-read-only t))
    (save-excursion
      (save-restriction
        (widen)
        (remove-text-properties (point-min) (point-max) '(help-echo nil))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (overlayp ov)
            (overlay-put ov 'help-echo nil)))))))

(defun gptel-navigator--render-sidebar ()
  "Render sidebar contents from current state."
  (let ((inhibit-read-only t)
        (inhibit-message t)
        (items (gptel-navigator--state-get :context-items))
        ;; Preserve point and scroll to avoid jumping to the first item on refresh.
        (win (get-buffer-window (current-buffer) t))
        (pt-line (line-number-at-pos (point)))
        (col (current-column))
        (ws-line (let* ((w (get-buffer-window (current-buffer) t))
                        (ws (and w (window-start w))))
                   (and ws (line-number-at-pos ws)))))
    (erase-buffer)
    ;; Header
    (let* ((root (or (gptel-navigator--state-get :current-project-root)
                     (when-let ((b (gptel-navigator--pc-pick-active-buffer)))
                       (gptel-navigator--project-root-of-buffer b))))
           (title (if (and root (stringp root))
                      (file-name-nondirectory (directory-file-name root))
                    "~")))
      (gptel-navigator--insert-with-face (format "🚀 %s\n\n" title) 'header-line))
    ;; Body
    (let ((loading (gptel-navigator--state-get :context-loading-p)))
      (cond
       (loading
        (gptel-navigator--insert-with-face "⏳ Loading context…\n\n" 'shadow))
       ((null items)
        (gptel-navigator--insert-with-face
         "No context available\nOpen files or select text to add context.\n\n"
         'italic))
       (t
        (dolist (it items)
          (widget-create (gptel-navigator--create-item-widget it)))
        (widget-insert "\n"))))
    (widget-setup)
    (gptel-navigator--scrub-help-echo)
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
     (items
      (goto-char (point-min))
      (widget-forward 1)))))

(defun gptel-navigator--create-sidebar ()
  "Create sidebar buffer/window."
  (let* ((buffer (get-buffer-create "*gptel-navigator/"))
         (window (display-buffer-in-side-window
                  buffer
                  `((side . left)
                    (window-width . ,gptel-navigator-sidebar-width)
                    (slot . 0)))))
    (with-current-buffer buffer
      (gptel-navigator-sidebar-mode))
    (gptel-navigator--state-put :sidebar-buffer buffer)
    (gptel-navigator--state-put :sidebar-window window)
    (gptel-navigator--setup-context-advice)
    (when (and gptel-navigator-autosave
               (fboundp 'gptel-navigator--setup-project-context-save-advice))
      (gptel-navigator--setup-project-context-save-advice))
    ;; Install lightweight hooks to keep highlight up-to-date on buffer switch.
    (unless gptel-navigator--hooks-installed
      (add-hook 'window-selection-change-functions #'gptel-navigator--window-buffer-change-hook)
      (add-hook 'buffer-list-update-hook #'gptel-navigator--window-buffer-change-hook)
      (setq gptel-navigator--hooks-installed t))
    buffer))

(defun gptel-navigator--ensure-sidebar ()
  "Ensure sidebar exists and is visible."
  (let ((buf (gptel-navigator--state-get :sidebar-buffer))
        (win (gptel-navigator--state-get :sidebar-window)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (gptel-navigator--create-sidebar)))
    (unless (and win (window-live-p win))
      (setq win (display-buffer-in-side-window
                 buf
                 `((side . left)
                   (window-width . ,gptel-navigator-sidebar-width)
                   (slot . 0))))
      (gptel-navigator--state-put :sidebar-window win))
    buf))

;; ----------------------------------------------------------------------------
;; Sidebar visibility across frames/tabs (global sync)
;; ----------------------------------------------------------------------------

(defun gptel-navigator--sidebar-window-in-frame (frame)
  "Return the window in FRAME showing the navigator sidebar, or nil."
  (let ((buf (gptel-navigator--state-get :sidebar-buffer)))
    (when (and buf (buffer-live-p buf))
      (with-selected-frame frame
        (get-buffer-window buf)))))

(defun gptel-navigator--sidebar-visible-p (&optional frame)
  "Return non-nil if the sidebar is visible in FRAME (or current frame)."
  (let ((fr (or frame (selected-frame))))
    (and (gptel-navigator--sidebar-window-in-frame fr) t)))

(defun gptel-navigator--open-in-frame (frame)
  "Ensure the sidebar is visible in FRAME."
  (with-selected-frame frame
    (let* ((buf (or (and (buffer-live-p (gptel-navigator--state-get :sidebar-buffer))
                         (gptel-navigator--state-get :sidebar-buffer))
                    (gptel-navigator--create-sidebar)))
           (win (get-buffer-window buf)))
      (unless (and win (window-live-p win))
        (setq win (display-buffer-in-side-window
                   buf
                   `((side . left)
                     (window-width . ,gptel-navigator-sidebar-width)
                     (slot . 0)))))
      (when (eq frame (selected-frame))
        (gptel-navigator--state-put :sidebar-window win))
      buf)))

(defun gptel-navigator--hide-in-frame (frame)
  "Hide the sidebar window in FRAME if present."
  (with-selected-frame frame
    (when-let* ((buf (gptel-navigator--state-get :sidebar-buffer))
                (win (get-buffer-window buf)))
      (when (window-live-p win)
        (delete-window win)))
    (when (eq frame (selected-frame))
      (gptel-navigator--state-put :sidebar-window nil))))

(defun gptel-navigator--sync-sidebar-visibility (&optional frame &rest _)
  "Ensure sidebar visibility in FRAME matches global state.
When FRAME is nil, sync the selected frame."
  (let ((fr (or frame (selected-frame))))
    (if gptel-navigator--sidebar-open-global
        (gptel-navigator--open-in-frame fr)
      (gptel-navigator--hide-in-frame fr))))

(defun gptel-navigator--sync-all-frames ()
  "Sync sidebar visibility across all frames to the global state."
  (dolist (fr (frame-list))
    (gptel-navigator--sync-sidebar-visibility fr)))

(defun gptel-navigator--sync-current-frame (&rest _)
  "Hook helper to sync sidebar visibility in the current frame."
  (gptel-navigator--sync-sidebar-visibility (selected-frame)))

(defun gptel-navigator--visibility-hooks-setup ()
  "Install hooks to keep sidebar visibility synced across frames/tabs."
  (unless gptel-navigator--visibility-hooks-installed
    (add-hook 'window-configuration-change-hook #'gptel-navigator--sync-current-frame)
    (add-hook 'after-make-frame-functions #'gptel-navigator--sync-sidebar-visibility)
    (add-hook 'window-selection-change-functions #'gptel-navigator--sync-current-frame)
    (setq gptel-navigator--visibility-hooks-installed t)))

(defun gptel-navigator--visibility-hooks-teardown ()
  "Remove hooks that sync sidebar visibility."
  (when gptel-navigator--visibility-hooks-installed
    (remove-hook 'window-configuration-change-hook #'gptel-navigator--sync-current-frame)
    (remove-hook 'after-make-frame-functions #'gptel-navigator--sync-sidebar-visibility)
    (remove-hook 'window-selection-change-functions #'gptel-navigator--sync-current-frame)
    (setq gptel-navigator--visibility-hooks-installed nil)))

;; ----------------------------------------------------------------------------
;; Navigation
;; ----------------------------------------------------------------------------

(defun gptel-navigator-goto-item-internal (item)
  "Navigate to ITEM's file/buffer/region and recenter."
  (let* ((type (gptel-navigator-item-type item))
         (buf (gptel-navigator-item-buffer item))
         (path (gptel-navigator-item-path item))
         (beg (gptel-navigator-item-start item))
         (end (gptel-navigator-item-end item)))
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
      (message "📍 Opened file: %s" (or (gptel-navigator-item-name item) path)))
     ;; Buffers and selections
     (t
      (when (buffer-live-p buf)
        (if-let ((win (get-buffer-window buf)))
            (select-window win)
          (switch-to-buffer-other-window buf))
        ;; Do not activate region; just jump to the start if available.
        (when beg (goto-char beg))
        (recenter)
        (message "📍 Navigated to: %s" (gptel-navigator-item-name item)))))))

;;;###autoload
(defun gptel-navigator-goto-item ()
  "Navigate to the item at point."
  (interactive)
  (when-let* ((w (widget-at))
              (item (widget-get w :button-data)))
    (gptel-navigator-goto-item-internal item)))

;;;###autoload
(defun gptel-navigator-preview-item ()
  "Preview item at point in its window without leaving the sidebar."
  (interactive)
  (when-let* ((w (widget-at))
              (item (widget-get w :button-data)))
    (let ((buf (gptel-navigator-item-buffer item)))
      (when (buffer-live-p buf)
        (when-let ((win (get-buffer-window buf t)))
          (with-selected-window win
            (when-let ((s (gptel-navigator-item-start item)))
              (goto-char s)
              (recenter))))
        (message "👁️ Previewing: %s" (gptel-navigator-item-name item))))))

;; ----------------------------------------------------------------------------
;; Refresh and auto-refresh
;; ----------------------------------------------------------------------------

(defun gptel-navigator-refresh ()
  "Collect context and render the sidebar."
  (interactive)
  (let* ((items (gptel-navigator--collect-context))
         (buf (gptel-navigator--state-get :sidebar-buffer))
         (now (float-time))
         (last (or (gptel-navigator--state-get :last-refresh-echo) 0)))
    (gptel-navigator--state-put :context-items items)
    (when (and buf (buffer-live-p buf))
      (when-let ((win (car (get-buffer-window-list buf nil t))))
        (gptel-navigator--state-put :sidebar-window win))
      (with-current-buffer buf
        (gptel-navigator--render-sidebar)))
    ;; Никогда не показываем message о количестве элементов (вообще)
    ;; (when (called-interactively-p 'interactive)
    ;;   (gptel-navigator--state-put :last-refresh-echo now)
    ;;   (message "🔄 GPTel context refreshed (%d items)" (length items)))
    ))

(defun gptel-navigator--auto-refresh ()
  "Auto-refresh sidebar if enabled."
  (when (and gptel-navigator-auto-refresh
             (not gptel-navigator--inhibit-refresh)
             (gptel-navigator--state-get :sidebar-buffer))
    (gptel-navigator-refresh)))

(defvar gptel-navigator--context-advised-fns
  '(gptel-context-add
    gptel-context-add-file
    gptel-context--add-region
    gptel-context-remove
    gptel-context-remove-all
    gptel-context--add-directory)
  "gptel-context mutators that should trigger auto-refresh/auto-save.")

(defun gptel-navigator--advice-refresh (&rest _)
  "Advice target: refresh sidebar after context changes."
  (gptel-navigator--auto-refresh))

(defun gptel-navigator--setup-context-advice ()
  "Install :after advices on context-mutating functions."
  (when (and (featurep 'gptel-context)
             gptel-navigator-auto-refresh
             (not (gptel-navigator--state-get :advices-installed)))
    (dolist (fn gptel-navigator--context-advised-fns)
      (when (fboundp fn)
        (advice-add fn :after #'gptel-navigator--advice-refresh)))
    (gptel-navigator--state-put :advices-installed t)))

;; ----------------------------------------------------------------------------
;; Autosave advice (safe stubs)
;; ----------------------------------------------------------------------------

(defun gptel-navigator--maybe-autosave-context (&rest _)
  "Autosave project/global context when appropriate."
  (when (and (bound-and-true-p gptel-navigator-autosave)
             (fboundp 'gptel-navigator-context-save))
    (ignore-errors
      (gptel-navigator-context-save))))

(defun gptel-navigator--setup-project-context-save-advice ()
  "Install autosave advice on gptel-context mutators."
  (unless (gptel-navigator--state-get :save-advices-installed)
    (dolist (fn gptel-navigator--context-advised-fns)
      (when (fboundp fn)
        (advice-add fn :after #'gptel-navigator--maybe-autosave-context)))
    (gptel-navigator--state-put :save-advices-installed t)))

(defun gptel-navigator--teardown-project-context-save-advice ()
  "Remove autosave advice previously installed."
  (when (gptel-navigator--state-get :save-advices-installed)
    (dolist (fn gptel-navigator--context-advised-fns)
      (when (advice-member-p #'gptel-navigator--maybe-autosave-context fn)
        (advice-remove fn #'gptel-navigator--maybe-autosave-context)))
    (gptel-navigator--state-put :save-advices-installed nil)))

(defun gptel-navigator--teardown-context-advice ()
  "Remove context advices."
  (when (gptel-navigator--state-get :advices-installed)
    (dolist (fn gptel-navigator--context-advised-fns)
      (when (advice-member-p #'gptel-navigator--advice-refresh fn)
        (advice-remove fn #'gptel-navigator--advice-refresh)))
    (gptel-navigator--state-put :advices-installed nil)))

;; ----------------------------------------------------------------------------
;; Sidebar commands
;; ----------------------------------------------------------------------------

;;;###autoload
(defun gptel-navigator-toggle ()
  "Toggle the gptel-navigator sidebar in the current frame only."
  (interactive)
  (let ((root (gptel-navigator--detect-root)))
    (if (gptel-navigator--sidebar-visible-p (selected-frame))
        (progn
          (gptel-navigator--hide-in-frame (selected-frame))
          (gptel-navigator--state-put :sidebar-window nil)
          (setq gptel-navigator--sidebar-open-global nil))
      (setq gptel-navigator--sidebar-open-global t)
      (gptel-navigator--state-put :current-project-root root)
      (ignore-errors (gptel-navigator-context-load root))
      (gptel-navigator--ensure-sidebar)
      (gptel-navigator-refresh))))

;;;###autoload
(defun gptel-navigator-show ()
  "Show the gptel-navigator sidebar in the current frame and refresh."
  (interactive)
  (let ((root (gptel-navigator--detect-root)))
    (setq gptel-navigator--sidebar-open-global t)
    (gptel-navigator--state-put :current-project-root root)
    (ignore-errors (gptel-navigator-context-load root))
    (gptel-navigator--ensure-sidebar)
    (gptel-navigator-refresh)))

;;;###autoload
(defun gptel-navigator-quit ()
  "Hide the gptel-navigator sidebar in the current frame."
  (interactive)
  (setq gptel-navigator--sidebar-open-global nil)
  (gptel-navigator--hide-in-frame (selected-frame))
  (gptel-navigator--state-put :sidebar-window nil))

;;;###autoload
(defun gptel-navigator-help ()
  "Show short help in the echo area."
  (interactive)
  (message "GPTel Navigator: RET=goto, SPC=preview, r=refresh, q=quit, TAB/S-TAB=navigate"))

;; ----------------------------------------------------------------------------
;; Transient menu
;; ----------------------------------------------------------------------------

;;;###autoload (autoload 'gptel-navigator-menu "gptel-navigator" nil t)
(transient-define-prefix gptel-navigator-menu ()
  "Main menu for gptel-navigator."
  ["GPTel Navigator"
   ["View"
    ("n" "Toggle sidebar" gptel-navigator-toggle)
    ("r" "Refresh context" gptel-navigator-refresh)
    ("u" "Unload project (global context)" gptel-navigator-unload)]
   ["Navigate"
    ("g" "Goto item" gptel-navigator-goto-item
     :if (lambda () (gptel-navigator--state-get :sidebar-window)))
    ("p" "Preview item" gptel-navigator-preview-item
     :if (lambda () (gptel-navigator--state-get :sidebar-window)))]
   ["Settings"
    ("?" "Help" gptel-navigator-help)
    ("q" "Quit" gptel-navigator-quit
     :if (lambda () (gptel-navigator--state-get :sidebar-window)))]])

;; ----------------------------------------------------------------------------
;; Global minor mode
;; ----------------------------------------------------------------------------

;;;###autoload
(define-minor-mode gptel-navigator-mode
  "Global minor mode for GPTel Navigator."
  :global t
  :lighter " 🚀"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c in") #'gptel-navigator-menu)
            (define-key map (kbd "C-c il") #'gptel-navigator-context-load)
            (define-key map (kbd "C-c is") #'gptel-navigator-context-save)
            (define-key map (kbd "C-c ic") #'gptel-navigator-toggle)
            map)
  (if gptel-navigator-mode
      (progn
        ;; Keep it lean; no post-command hooks.
        (gptel-navigator--setup-context-advice)
        (when gptel-navigator-autosave
          (gptel-navigator--setup-project-context-save-advice)))
    (gptel-navigator--teardown-context-advice)
    (unless (bound-and-true-p gptel-navigator-autoload)
      (gptel-navigator--teardown-project-context-save-advice))))

;; ----------------------------------------------------------------------------
;; Project context serialization and autoload
;; ----------------------------------------------------------------------------

(defun gptel-navigator--ensure-dir (dir)
  "Ensure DIR exists."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun gptel-navigator--context-file (root)
  "Return path to context file for ROOT or the global one."
  (if (and root (stringp root))
      (expand-file-name gptel-navigator-context-file-name
                        (expand-file-name gptel-navigator-dir-name root))
    (expand-file-name gptel-navigator-context-file-name gptel-navigator-global-dir)))

(defun gptel-navigator--project-relativize (path root)
  "Return PATH relative to ROOT if PATH is inside ROOT."
  (let ((abs (expand-file-name path)))
    (if (and root (file-in-directory-p abs root))
        (file-relative-name abs root)
      abs)))

(defun gptel-navigator--project-abspath (path root)
  "Return absolute path for PATH relative to ROOT when not absolute."
  (if (and root (not (file-name-absolute-p path)))
      (expand-file-name path root)
    (expand-file-name path)))

(defun gptel-navigator--serialize-context (&optional root)
  "Serialize `gptel-context--alist' into a portable list for saving.
Format:
- (:file RELPATH [:mime MIME])
- (:buffer RELPATH :regions ((BEG . END) ...)) ; only file-backed buffers
Non-file buffers are ignored in v1."
  (let ((alist (gptel-navigator--context-alist))
        out)
    (dolist (entry alist)
      (pcase entry
        (`(,(and buf (pred bufferp)) . ,ovs)
         (with-current-buffer buf
           (when-let ((path (buffer-file-name)))
             (let ((regions (delq nil
                                  (mapcar (lambda (ov)
                                            (when (and (overlay-start ov) (overlay-end ov))
                                              (cons (overlay-start ov) (overlay-end ov))))
                                          ovs))))
               (if (and regions (cl-some #'identity regions))
                   (push (list :buffer (gptel-navigator--project-relativize path root)
                               :regions regions)
                         out)
                 (push (list :file (gptel-navigator--project-relativize path root)) out))))))
        (`(,(and path (pred stringp)) . ,props)
         (let* ((rel (gptel-navigator--project-relativize path root))
                (mime (plist-get props :mime)))
           (if mime
               (push (list :file rel :mime mime) out)
             (push (list :file rel) out))))
        (`(,path)
         (push (list :file (gptel-navigator--project-relativize path root)) out))))
    (nreverse out)))

(defun gptel-navigator--read-file-sexp (file)
  "Read first sexp from FILE, returning it or nil on error."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (read (current-buffer))
        (error nil)))))

;;;###autoload
(defun gptel-navigator-context-save (&optional root)
  "Save current gptel context for project ROOT (or global if ROOT nil)."
  (interactive)
  (let* ((root (or root (gptel-navigator--detect-root)))
         (dir (if root
                  (expand-file-name gptel-navigator-dir-name root)
                gptel-navigator-global-dir))
         (file (gptel-navigator--context-file root))
         (data (gptel-navigator--serialize-context root)))
    (gptel-navigator--ensure-dir dir)
    (with-temp-file file
      (insert ";; Auto-generated by gptel-navigator. Do not edit manually.\n")
      (prin1 data (current-buffer))
      (insert "\n"))
    (when (called-interactively-p 'interactive)
      (message "Saved gptel context to %s" file))
    file))

;;;###autoload
(defun gptel-navigator-context-load (&optional root)
  "Load project context for ROOT (or global). Replaces current gptel context."
  (interactive)
  (let* ((root (or root (gptel-navigator--detect-root)))
         (file (gptel-navigator--context-file root))
         (result nil))
    (let ((gptel-navigator--in-context-load t)
          (gptel-navigator--inhibit-refresh t))
      (if-let ((spec (gptel-navigator--read-file-sexp file)))
          (progn
            ;; Replace current context with loaded spec
            (gptel-context-remove-all)
            (dolist (it spec)
              (pcase it
                (`(:file ,rel . ,_rest)
                 (let ((abs (gptel-navigator--project-abspath rel root)))
                   (ignore-errors (gptel-context-add-file abs))))
                (`(:buffer ,rel :regions ,regions . ,_rest)
                 (let* ((abs (gptel-navigator--project-abspath rel root))
                        (buf (ignore-errors (find-file-noselect abs))))
                   (when (buffer-live-p buf)
                     (dolist (pair regions)
                       (when (and (consp pair) (numberp (car pair)) (numberp (cdr pair)))
                         (ignore-errors
                           (gptel-context--add-region buf (car pair) (cdr pair) t)))))))))
            (setq result t))
        ;; No spec available: clear context
        (gptel-context-remove-all)
        (setq result nil)))
    (setq gptel-navigator--last-project-root root)
    (when (buffer-live-p (gptel-navigator--state-get :sidebar-buffer))
      (gptel-navigator-refresh))
    (when (called-interactively-p 'interactive)
      (message (if result
                   "Loaded gptel context from %s"
                 "No saved gptel context at %s — cleared current context")
               file))
    result))

;;;###autoload
(defun gptel-navigator-unload ()
  "Close the current project: switch to the global GPTel context (or clear),
and set GPTel chat buffer's directory to filesystem root (/).

Forces GPTel Navigator to behave as if we're outside any project:
- Sets internal state to a global sentinel
- Loads the global context from `gptel-navigator-global-dir' if present
- Otherwise clears the current GPTel context
- Changes `default-directory' of the active GPTel buffer to \"/\"

Autoload throttling is set so we don't immediately jump back to the project's
context while staying in the same project buffer."
  (interactive)
  (let ((cur-root (gptel-navigator--detect-root)))
    ;; Prevent immediate autoload switch back to CUR-ROOT
    (setq gptel-navigator--last-project-root cur-root)
    ;; Mark UI/state as global (non-string to avoid being treated as a dir)
    (gptel-navigator--state-put :current-project-root :global))
  (let* ((file (gptel-navigator--context-file nil))
         (result nil))
    (let ((gptel-navigator--in-context-load t)
          (gptel-navigator--inhibit-refresh t))
      (if-let ((spec (gptel-navigator--read-file-sexp file)))
          (progn
            ;; Replace current context with global spec
            (gptel-context-remove-all)
            (dolist (it spec)
              (pcase it
                (`(:file ,rel . ,_rest)
                 (let ((abs (gptel-navigator--project-abspath rel nil)))
                   (ignore-errors (gptel-context-add-file abs))))
                (`(:buffer ,rel :regions ,regions . ,_rest)
                 (let* ((abs (gptel-navigator--project-abspath rel nil))
                        (buf (ignore-errors (find-file-noselect abs))))
                   (when (buffer-live-p buf)
                     (dolist (pair regions)
                       (when (and (consp pair) (numberp (car pair)) (numberp (cdr pair)))
                         (ignore-errors
                           (gptel-context--add-region buf (car pair) (cdr pair) t)))))))))
            (setq result t))
        ;; No global spec available: clear context
        (gptel-context-remove-all)
        (setq result nil)))
    ;; Also switch GPTel chat buffer `default-directory' to filesystem root
    (when-let ((gtbuf (gptel-navigator--find-gptel-buffer)))
      (with-current-buffer gtbuf
        ;; Keep previous dir in state in case we want to restore later
        (gptel-navigator--state-put :prev-gptel-default-directory default-directory)
        (setq default-directory (expand-file-name "/"))))
    (when (buffer-live-p (gptel-navigator--state-get :sidebar-buffer))
      (gptel-navigator-refresh))
    (when (called-interactively-p 'interactive)
      (message (if result
                   "Loaded global gptel context from %s"
                 "No global gptel context at %s — cleared current context")
               file))
    result))

;; ----------------------------------------------------------------------------
;; Project auto-load/save global mode
;; ----------------------------------------------------------------------------

(defun gptel-navigator--maybe-load-project-context ()
  "Auto-load context when the active buffer's project root changes (throttled).

The context switch only occurs if the active buffer is a file-visiting buffer
(i.e., has `buffer-file-name' non-nil).

If there is no project for the active buffer, load the global context from
`gptel-navigator-global-dir'; if it doesn't exist, clear the context."
  (unless gptel-navigator--in-context-load
    (when-let ((buf (gptel-navigator--pc-pick-active-buffer)))
      (with-current-buffer buf
        (when buffer-file-name ;; <--- Only act for file-visiting buffers now!
          (let* ((now (float-time))
                 (last gptel-navigator--last-context-switch-time)
                 (throttle-ok (>= (- now last) gptel-navigator-context-switch-interval))
                 (root (gptel-navigator--detect-root)))
            (when (and throttle-ok
                       (not (equal root gptel-navigator--last-project-root)))
              (setq gptel-navigator--last-context-switch-time now)
              (gptel-navigator--load-context-async root))))))))

(defun gptel-navigator--window-buffer-change-hook (&rest _)
  "Hook wrapper to maybe load project context and refresh sidebar highlight.
Reentrancy-guarded to avoid triggering itself via window/config changes."
  (unless gptel-navigator--in-window-change-hook
    (let ((gptel-navigator--in-window-change-hook t))
      (gptel-navigator--maybe-load-project-context)
      ;; Re-render sidebar to update highlight for the active buffer/file.
      (let ((buf (gptel-navigator--state-get :sidebar-buffer)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (gptel-navigator--render-sidebar)))))))

(defun gptel-navigator--advice-save-context (&rest _)
  "Auto-save project context after mutating gptel context."
  (when (and (not gptel-navigator--in-context-load)
             (or gptel-navigator-autosave
                 (bound-and-true-p gptel-navigator-autoload)))
    (ignore-errors
      (gptel-navigator-context-save (gptel-navigator--detect-root)))))

(defun gptel-navigator--setup-project-context-save-advice ()
  "Install advice to auto-save project context."
  (dolist (fn gptel-navigator--context-advised-fns)
    (when (fboundp fn)
      (unless (advice-member-p #'gptel-navigator--advice-save-context fn)
        (advice-add fn :after #'gptel-navigator--advice-save-context)))))

(defun gptel-navigator--teardown-project-context-save-advice ()
  "Remove advice that auto-saves project context."
  (dolist (fn gptel-navigator--context-advised-fns)
    (when (advice-member-p #'gptel-navigator--advice-save-context fn)
      (advice-remove fn #'gptel-navigator--advice-save-context))))

(defun gptel-navigator--setup-autoload-hooks ()
  "Enable automatic context loading for the current buffer/project."
  (gptel-navigator--setup-project-context-save-advice)
  (add-hook 'buffer-list-update-hook #'gptel-navigator--maybe-load-project-context)
  (add-hook 'window-configuration-change-hook #'gptel-navigator--maybe-load-project-context)
  (when (boundp 'window-buffer-change-functions)
    (add-hook 'window-buffer-change-functions #'gptel-navigator--window-buffer-change-hook))
  (when (boundp 'window-selection-change-functions)
    (add-hook 'window-selection-change-functions #'gptel-navigator--window-buffer-change-hook))
  (when (boundp 'tab-bar-selection-change-functions)
    (add-hook 'tab-bar-selection-change-functions #'gptel-navigator--window-buffer-change-hook))
  (when (boundp 'tab-bar-switch-hook)
    (add-hook 'tab-bar-switch-hook #'gptel-navigator--window-buffer-change-hook))
  ;; Initial load for current buffer
  (gptel-navigator--maybe-load-project-context))

(defun gptel-navigator--teardown-autoload-hooks ()
  "Disable automatic context loading hooks."
  (remove-hook 'buffer-list-update-hook #'gptel-navigator--maybe-load-project-context)
  (remove-hook 'window-configuration-change-hook #'gptel-navigator--maybe-load-project-context)
  (when (boundp 'window-buffer-change-functions)
    (remove-hook 'window-buffer-change-functions #'gptel-navigator--window-buffer-change-hook))
  (when (boundp 'window-selection-change-functions)
    (remove-hook 'window-selection-change-functions #'gptel-navigator--window-buffer-change-hook))
  (when (boundp 'tab-bar-selection-change-functions)
    (remove-hook 'tab-bar-selection-change-functions #'gptel-navigator--window-buffer-change-hook))
  (when (boundp 'tab-bar-switch-hook)
    (remove-hook 'tab-bar-switch-hook #'gptel-navigator--window-buffer-change-hook)))

;; Initialize autoload hooks if the option is enabled at load time.
(when (bound-and-true-p gptel-navigator-autoload)
  (ignore-errors (gptel-navigator--setup-autoload-hooks)))

(provide 'gptel-navigator)
;;; gptel-navigator.el ends here
