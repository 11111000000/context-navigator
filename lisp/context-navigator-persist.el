;;; context-navigator-persist.el --- Persist/load context (v3) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Safe, functional persistence layer:
;; - v3 s-exp format (no eval, read one form)
;; - project and global locations
;; - async loading with batching and :context-load-start events
;; - pure conversions to/from model items
;;
;; This module does not apply context to gptel by itself.
;; Core is responsible for calling gptel after load and publishing :context-load-done.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)
(require 'context-navigator-events)
(require 'context-navigator-log)

(defun context-navigator-persist--ensure-dir (dir)
  "Create DIR if missing, returning DIR."
  (unless (file-directory-p dir)
    (make-directory dir t))
  dir)

(defun context-navigator-persist--global-dir ()
  "Return configured global dir for context files."
  (expand-file-name (or (and (boundp 'context-navigator-global-dir)
                             (symbol-value 'context-navigator-global-dir))
                        "~/.context")))

(defun context-navigator-persist--project-dir (root)
  "Return project context dir under ROOT."
  (expand-file-name
   (or (and (boundp 'context-navigator-dir-name)
            (symbol-value 'context-navigator-dir-name))
       ".context")
   (or root default-directory)))

(defun context-navigator-persist--writable-target-dir-p (dir)
  "Return non-nil if DIR exists and is writable, or its parent directory is writable when DIR is missing."
  (let* ((d (and dir (file-name-as-directory (expand-file-name dir)))))
    (cond
     ((null d) nil)
     ((file-directory-p d) (file-writable-p d))
     (t (let ((parent (file-name-directory (directory-file-name d))))
          (and parent (file-writable-p parent)))))))

(defun context-navigator-persist-context-file (root &optional group-slug)
  "Return absolute path to context file for ROOT (or global).

When GROUP-SLUG is non-nil, return the path to that group's file
(ROOT or global): <dir>/<group-slug>.el. Otherwise, return the
legacy single-file path (context.el) for backward compatibility."
  (let* ((dir (if (and root (stringp root))
                  (context-navigator-persist--project-dir root)
                (context-navigator-persist--global-dir)))
         (fname (if (and group-slug (stringp group-slug) (not (string-empty-p group-slug)))
                    (concat (file-name-sans-extension group-slug) ".el")
                  (or (and (boundp 'context-navigator-context-file-name)
                           (symbol-value 'context-navigator-context-file-name))
                      "context.el"))))
    (expand-file-name fname dir)))

(defun context-navigator-persist--relativize (path root)
  "Return PATH as relative to ROOT when possible; else PATH."
  (if (and root (stringp path))
      (let* ((ep (expand-file-name path))
             (er (file-name-as-directory (expand-file-name root))))
        (if (string-prefix-p er ep)
            (file-relative-name ep er)
          ep))
    path))

(defun context-navigator-persist--absolutize (path root)
  "Return absolute PATH, using ROOT if PATH is relative."
  (if (and (stringp path) (file-name-absolute-p path))
      (expand-file-name path)
    (expand-file-name (or path "") (or root default-directory))))

(defun context-navigator-persist--item->sexp (item root)
  "Convert ITEM to v3 item plist, using ROOT for relative paths."
  (pcase (context-navigator-item-type item)
    ('file
     (let* ((p (context-navigator-item-path item))
            (rp (context-navigator-persist--relativize p root))
            (mime (plist-get (context-navigator-item-meta item) :mime)))
       (append (list :type 'file :path rp :enabled (not (null (context-navigator-item-enabled item))))
               (when mime (list :mime mime)))))
    ('selection
     (let* ((p (context-navigator-item-path item))
            (rp (context-navigator-persist--relativize p root)))
       (list :type 'selection :path rp
             :beg (context-navigator-item-beg item)
             :end (context-navigator-item-end item)
             :enabled (not (null (context-navigator-item-enabled item))))))
    ('buffer
     ;; Persist only file-backed buffers; otherwise skip by returning nil.
     (let* ((p (or (context-navigator-item-path item)
                   (and (buffer-live-p (context-navigator-item-buffer item))
                        (buffer-local-value 'buffer-file-name (context-navigator-item-buffer item))))))
       (when p
         (let ((rp (context-navigator-persist--relativize p root)))
           (list :type 'buffer :path rp :enabled (not (null (context-navigator-item-enabled item))))))))
    (_ nil)))

(defun context-navigator-persist--sexp->item (pl root)
  "Convert v3 item PL (plist or alist) to model item, using ROOT for path base."
  (let* ((get (lambda (k)
                (cond
                 ((keywordp k) (plist-get pl k))
                 ((and (listp pl) (consp (car pl))) (alist-get k pl))
                 (t nil))))
         (type (or (funcall get :type) (funcall get 'type)))
         (enabled (let ((e (funcall get :enabled))) (and e (not (eq e :false)))))
         (path (funcall get :path))
         (abs (and path (context-navigator-persist--absolutize path root))))
    (pcase type
      ((or 'file :file)
       (context-navigator-item-create
        :type 'file :name (file-name-nondirectory (or abs path))
        :path abs :enabled enabled))
      ((or 'selection :selection)
       (let ((beg (funcall get :beg))
             (end (funcall get :end)))
         (when (and (integerp beg) (integerp end))
           (context-navigator-item-create
            :type 'selection
            :name (format "%s:%s-%s" (file-name-nondirectory (or abs path)) beg end)
            :path abs :beg beg :end end :enabled enabled))))
      ((or 'buffer :buffer)
       (context-navigator-item-create
        :type 'buffer
        :name (file-name-nondirectory (or abs path))
        :path abs :enabled enabled))
      (_ nil))))

(defun context-navigator-persist-migrate-if-needed (sexp)
  "Migrate SEXP to v3 format plist if necessary. Returns plist or signals error."
  (cond
   ;; Already v3 map
   ((and (listp sexp)
         (plist-member sexp :version)
         (eq (plist-get sexp :version) context-navigator-persist-version))
    sexp)
   ;; v2/v1 could be bare list of items; wrap them
   ((and (listp sexp)
         (or (plist-member sexp :items)
             (and sexp (listp (car sexp)))))
    (list :version context-navigator-persist-version
          :generator "context-navigator/migrated"
          :root nil
          :items (if (plist-member sexp :items)
                     (plist-get sexp :items)
                   sexp)))
   (t
    (error "Unknown context format"))))

(defun context-navigator-persist-save (items root &optional group-slug)
  "Persist ITEMS for ROOT (or global) to disk using v3 format.
When GROUP-SLUG is non-nil, save into that group's file <dir>/<group-slug>.el.
Returns the file path or nil on error."
  (let* ((file (context-navigator-persist-context-file root group-slug))
         (dir (file-name-directory file))
         (payload
          (list :version context-navigator-persist-version
                :generator "context-navigator/1.1.0"
                :root (and root (expand-file-name root))
                :items (delq nil (mapcar (lambda (it)
                                           (context-navigator-persist--item->sexp it root))
                                         items)))))
    (if (not (context-navigator-persist--writable-target-dir-p dir))
        (progn
          (context-navigator-debug :warn :persist "skip save: target dir not writable: %s"
                                   (abbreviate-file-name (or dir "")))
          nil)
      (condition-case err
          (progn
            (context-navigator-persist--ensure-dir dir)
            (with-temp-file file
              (let ((print-length nil)
                    (print-level nil))
                (prin1 payload (current-buffer))
                (insert "\n")))
            file)
        (error
         (context-navigator-debug :error :persist "save error: %S" err)
         nil)))))

(defun context-navigator-persist--read-one (file)
  "Read single s-exp from FILE safely (no eval)."
  (with-temp-buffer
    (condition-case _err
        (progn
          (when (file-readable-p file)
            (insert-file-contents file)
            (goto-char (point-min))
            (read (current-buffer))))
      (error nil))))

(defun context-navigator-persist--load-batch-size ()
  "Return batch size for async load."
  (or (and (boundp 'context-navigator-context-load-batch-size)
           (symbol-value 'context-navigator-context-load-batch-size))
      64))

(defun context-navigator-persist--group-file-readable-p (file)
  "Return non-nil if FILE should be attempted to load.
Remote files are not checked for readability synchronously."
  (let ((remote (and (stringp file) (file-remote-p file))))
    (not (or (null file)
             (and (not remote) (not (file-readable-p file)))))))

(defun context-navigator-persist--read-migrated-v3 (file)
  "Read FILE and migrate to v3 format plist, or nil on error.
Emits debug messages mirroring the original behavior."
  (let ((raw (context-navigator-persist--read-one file)))
    (cond
     ((not raw)
      (context-navigator-debug :warn :persist "empty or invalid form in %s"
                               (abbreviate-file-name file))
      nil)
     (t
      (condition-case err
          (context-navigator-persist-migrate-if-needed raw)
        (error
         (context-navigator-debug :error :persist "migrate error: %S (file=%s)"
                                  err (abbreviate-file-name file))
         nil))))))

(defun context-navigator-persist--process-items-async (items-pl root batch callback)
  "Process ITEMS-PL in batches asynchronously, converting them and invoking CALLBACK.
Publishes :context-load-step events during progress."
  (let ((total (length items-pl))
        (acc (list))
        (pos 0)
        step-fn)
    (setq step-fn
          (lambda ()
            (let* ((chunk (cl-subseq items-pl pos (min total (+ pos batch)))))
              (setq pos (+ pos (length chunk)))
              (dolist (pl chunk)
                (let ((it (ignore-errors (context-navigator-persist--sexp->item pl root))))
                  (when it (push it acc))))
              (if (< pos total)
                  (progn
                    (context-navigator-events-publish :context-load-step root pos total)
                    (run-at-time 0.0 nil step-fn))
                (let ((items (nreverse acc)))
                  (funcall callback items))))))
    ;; Kick off processing loop on next tick
    (run-at-time 0.0 nil step-fn)))



(defun context-navigator-persist-state-file (root)
  "Return absolute path to state.el for ROOT (or global)."
  (let ((dir (if (and root (stringp root))
                 (context-navigator-persist--project-dir root)
               (context-navigator-persist--global-dir))))
    (expand-file-name "state.el" dir)))

(defun context-navigator-persist-state-load (root)
  "Load and return state plist from state.el for ROOT (or global), or nil."
  (let* ((file (context-navigator-persist-state-file root)))
    (when (file-readable-p file)
      (let ((raw (ignore-errors (context-navigator-persist--read-one file))))
        (cond
         ((and (listp raw)
               (or (plist-member raw :version)
                   (plist-member raw :current)
                   (plist-member raw :groups)))
          ;; Ensure :version present (default 1).
          (let ((st (copy-sequence raw)))
            (unless (plist-member st :version)
              (setq st (plist-put st :version 1)))
            st))
         (t nil))))))

(defun context-navigator-persist-state-save (root state)
  "Save STATE plist to state.el for ROOT (or global). Return filepath or nil."
  (let* ((file (context-navigator-persist-state-file root))
         (dir (file-name-directory file))
         (pl (if (and (listp state) (plist-member state :version))
                 state
               (plist-put (copy-sequence (or state '())) :version 1))))
    (if (not (context-navigator-persist--writable-target-dir-p dir))
        (progn
          (context-navigator-debug :warn :persist "skip state save: target dir not writable: %s"
                                   (abbreviate-file-name (or dir "")))
          nil)
      (condition-case err
          (progn
            (context-navigator-persist--ensure-dir dir)
            (with-temp-file file
              (let ((print-length nil)
                    (print-level nil))
                (prin1 pl (current-buffer))
                (insert "\n")))
            file)
        (error
         (context-navigator-debug :error :persist "state save error: %S" err)
         nil)))))

(defun context-navigator-persist-slugify (name)
  "Normalize NAME to a safe group slug:
- downcase
- replace invalid chars (anything except [:alnum:] _ -) with '-'
- collapse consecutive '-' to single
- trim leading/trailing '-' and '_'
- cut to 100 chars"
  (let* ((s (or name "")))
    (setq s (downcase s))
    (setq s (replace-regexp-in-string "[^[:alnum:]_-]+" "-" s))
    (setq s (replace-regexp-in-string "-+" "-" s))
    (setq s (replace-regexp-in-string "^[-_]+" "" s))
    (setq s (replace-regexp-in-string "[-_]+$" "" s))
    (when (> (length s) 100)
      (setq s (substring s 0 100)))
    s))

(defun context-navigator-persist-list-groups (root)
  "Return list of group descriptors for ROOT (or global).
Each element is a plist: (:slug :display :path :mtime).

Источник правды — реальные .el-файлы групп; однако если в state.el есть
:groups, то их отображаемые имена учитываются (и группы без файлов
подставляются с путём на основе <dir>/<slug>.el и mtime=nil)."
  (let* ((dir (if (and root (stringp root))
                  (context-navigator-persist--project-dir root)
                (context-navigator-persist--global-dir)))
         (st (ignore-errors (context-navigator-persist-state-load root)))
         (mapping
          ;; Build slug->display hash from state[:groups] (alist)
          (when (and (listp st) (plist-member st :groups))
            (let ((alist (plist-get st :groups))
                  (ht (make-hash-table :test 'equal)))
              (dolist (cell alist)
                (let ((slug (car cell))
                      (disp (cdr cell)))
                  (when (and (stringp slug) (not (string-empty-p slug)))
                    (puthash slug (or disp slug) ht))))
              ht)))
         (acc (make-hash-table :test 'equal)))
    ;; 1) Collect groups from regular files, ignoring state.el and legacy context.el
    (when (file-directory-p dir)
      (dolist (p (directory-files dir t "\\.el\\'"))
        (let* ((bn (file-name-nondirectory p))
               (attrs (file-attributes p 'string)))
          (unless (or (string= bn "state.el")
                      (string= bn "context.el")     ;; legacy single-file, ignore
                      (not attrs)                    ;; cannot stat
                      (file-attribute-type attrs))   ;; non-regular (dir/symlink)
            (let* ((slug (file-name-sans-extension bn))
                   (mtime (and attrs (file-attribute-modification-time attrs)))
                   (disp (or (and mapping (gethash slug mapping)) slug)))
              (puthash slug
                       (list :slug slug
                             :display disp
                             :path p
                             :mtime (and mtime (float-time mtime)))
                       acc))))))
    ;; 2) Include mapping-only groups (no files yet)
    (when mapping
      (maphash
       (lambda (slug disp)
         (unless (gethash slug acc)
           (let ((p (expand-file-name (concat slug ".el") dir)))
             (puthash slug
                      (list :slug slug :display disp :path p :mtime nil)
                      acc))))
       mapping))
    ;; 3) Return as list (sorting is performed by callers/UI when needed)
    (let (lst)
      (maphash (lambda (_slug pl) (push pl lst)) acc)
      (nreverse lst))))

(defun context-navigator-persist-load-group-async (root slug callback)
  "Asynchronously load group SLUG for ROOT (or global) and call CALLBACK with items list.

The actual file read/parsing is deferred to the next idle slice so the caller
(e.g. project switch) can return quickly and the sidebar can render a
preloader without being blocked."
  (let* ((file (context-navigator-persist-context-file root slug))
         (batch (context-navigator-persist--load-batch-size)))
    (context-navigator-events-publish :context-load-start root)
    (if (not (context-navigator-persist--group-file-readable-p file))
        (progn
          (context-navigator-debug :warn :persist "group file missing/unreadable: %s"
                                   (abbreviate-file-name (or file "")))
          (run-at-time 0 nil (lambda () (funcall callback nil))))
      ;; Defer blocking read/parse to avoid jank on project switch
      (run-at-time 0 nil
                   (lambda ()
                     (let* ((v3 (context-navigator-persist--read-migrated-v3 file))
                            (items-pl (and v3 (plist-get v3 :items))))
                       (context-navigator-persist--process-items-async
                        items-pl root batch callback)))))))

(defun context-navigator-persist-load-async (root callback)
  "Deprecated: asynchronous load of legacy single-file context (context.el).

Load context for ROOT (or global when ROOT is nil) from the legacy single-file
format and call CALLBACK with the list of items. Publishes :context-load-start
and :context-load-step events similarly to `context-navigator-persist-load-group-async'."
  (let* ((file (context-navigator-persist-context-file root nil))
         (batch (context-navigator-persist--load-batch-size)))
    (context-navigator-events-publish :context-load-start root)
    (if (not (context-navigator-persist--group-file-readable-p file))
        (run-at-time 0 nil (lambda () (funcall callback nil)))
      (run-at-time 0 nil
                   (lambda ()
                     (let* ((v3 (context-navigator-persist--read-migrated-v3 file))
                            (items-pl (and v3 (plist-get v3 :items))))
                       (context-navigator-persist--process-items-async
                        items-pl root batch callback)))))))

;; Mark legacy single-file async loader as obsolete in favor of per-group loader.
(make-obsolete 'context-navigator-persist-load-async
               'context-navigator-persist-load-group-async
               "0.3.0")

(defun context-navigator-persist-group-item-count (file)
  "Return number of items stored in group FILE (v3 format).
Returns 0 when FILE is unreadable or on error. Avoids TRAMP sync checks."
  (let ((ok (and (stringp file)
                 (context-navigator-persist--group-file-readable-p file))))
    (if (not ok)
        0
      (let* ((v3 (ignore-errors (context-navigator-persist--read-migrated-v3 file)))
             (items (and (listp v3) (plist-get v3 :items))))
        (if (listp items) (length items) 0)))))

(defun context-navigator-persist-group-enabled-count (file)
  "Return cons (ENABLED . TOTAL) for items in group FILE (v3 format).
When FILE is unreadable or invalid, return (0 . 0). Avoids TRAMP sync checks."
  (let ((ok (and (stringp file)
                 (context-navigator-persist--group-file-readable-p file))))
    (if (not ok)
        (cons 0 0)
      (let* ((v3 (ignore-errors (context-navigator-persist--read-migrated-v3 file)))
             (items (and (listp v3) (plist-get v3 :items))))
        (if (not (listp items))
            (cons 0 0)
          (let ((total 0) (en 0))
            (dolist (pl items)
              (setq total (1+ total))
              (let ((e (or (plist-get pl :enabled)
                           (and (listp pl) (alist-get :enabled pl)))))
                (when e (setq en (1+ en)))))
            (cons en total)))))))

(provide 'context-navigator-persist)
;;; context-navigator-persist.el ends here
