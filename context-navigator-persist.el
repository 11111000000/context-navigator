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

(defconst context-navigator-persist--version 3)

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

(defun context-navigator-persist-context-file (root)
  "Return absolute path to context file for ROOT (or global)."
  (if (and root (stringp root))
      (expand-file-name
       (or (and (boundp 'context-navigator-context-file-name)
                (symbol-value 'context-navigator-context-file-name))
           "context.el")
       (context-navigator-persist--project-dir root))
    (expand-file-name
     (or (and (boundp 'context-navigator-context-file-name)
              (symbol-value 'context-navigator-context-file-name))
         "context.el")
     (context-navigator-persist--global-dir))))

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
         (eq (plist-get sexp :version) context-navigator-persist--version))
    sexp)
   ;; v2/v1 could be bare list of items; wrap them
   ((and (listp sexp)
         (or (plist-member sexp :items)
             (and sexp (listp (car sexp)))))
    (list :version context-navigator-persist--version
          :generator "context-navigator/migrated"
          :root nil
          :items (if (plist-member sexp :items)
                     (plist-get sexp :items)
                   sexp)))
   (t
    (error "Unknown context format"))))

(defun context-navigator-persist-save (items root)
  "Persist ITEMS for ROOT (or global) to disk using v3 format.
Returns the file path or nil on error."
  (let* ((file (context-navigator-persist-context-file root))
         (dir (file-name-directory file))
         (payload
          (list :version context-navigator-persist--version
                :generator "context-navigator/0.1.x"
                :root (and root (expand-file-name root))
                :items (delq nil (mapcar (lambda (it)
                                           (context-navigator-persist--item->sexp it root))
                                         items)))))
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
       (message "[context-navigator/persist] save error: %S" err)
       nil))))

(defun context-navigator-persist--read-one (file)
  "Read single s-exp from FILE safely (no eval)."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (condition-case nil
        (read (current-buffer))
      (error nil))))

(defun context-navigator-persist-load-async (root callback)
  "Asynchronously load context for ROOT (or global) and CALLBACK with items.
Publishes :context-load-start ROOT. Batches are processed by timer to avoid jank.
CALLBACK is called once with full list of items (enabled or not)."
  (let* ((file (context-navigator-persist-context-file root))
         (batch (or (and (boundp 'context-navigator-context-load-batch-size)
                         (symbol-value 'context-navigator-context-load-batch-size))
                    64)))
    (context-navigator-events-publish :context-load-start root)
    (if (not (file-readable-p file))
        ;; Nothing to load; still callback with empty list.
        (run-at-time 0 nil (lambda () (funcall callback nil)))
      (let* ((raw (context-navigator-persist--read-one file))
             (v3 (cond
                  ((not raw) nil)
                  (t (condition-case err
                         (context-navigator-persist-migrate-if-needed raw)
                       (error
                        (when (bound-and-true-p context-navigator-debug)
                          (message "[context-navigator/persist] migrate error: %S" err))
                        nil)))))
             (items-pl (and v3 (plist-get v3 :items)))
             (total (length items-pl))
             (acc (list))
             (pos 0)
             (step-fn nil))
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
        (run-at-time 0.0 nil step-fn)))))

(provide 'context-navigator-persist)
;;; context-navigator-persist.el ends here
