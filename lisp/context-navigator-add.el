;;; context-navigator-add.el --- Add logic for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Core logic for adding files/regions/buffers into the Context Navigator model.
;; This module contains small, focused functions and interactive entry points
;; unrelated to transient UI assembly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dired)

(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-i18n)
(require 'context-navigator-ui)

(defgroup context-navigator-add nil
  "Settings for universal add operations."
  :group 'context-navigator)

(defcustom context-navigator-max-file-size (* 1 1024 1024)
  "Maximum file size (bytes) to include when adding files recursively.
Files larger than this threshold are skipped."
  :type 'integer :group 'context-navigator-add)

;; ---------------- Filesystem helpers ----------------

(defun context-navigator-add-regular-file-p (path)
  "Return non-nil if PATH is a regular file."
  (and (stringp path)
       (file-exists-p path)
       (file-regular-p path)))

(defun context-navigator-add-file-size (path)
  "Return size of PATH in bytes or nil."
  (when (and (stringp path) (file-exists-p path))
    (let ((attrs (file-attributes path 'string)))
      (and attrs (file-attribute-size attrs)))))

(defun context-navigator-add-collect-recursive (dir)
  "Collect regular files under DIR (recursive)."
  (let* ((all (ignore-errors (directory-files-recursively dir ".*" t)))
         (files (cl-remove-if-not #'file-regular-p all)))
    files))

(defun context-navigator-add-gather-files (paths)
  "From PATHS (files/dirs), return plist:
(:files L :skipped-too-big N :skipped-nonregular M :remote K)."
  (let ((files '())
        (skipped-too-big 0)
        (skipped-nonregular 0)
        (remote 0)
        (limit (or context-navigator-max-file-size most-positive-fixnum)))
    (dolist (p paths)
      (cond
       ((and (stringp p) (file-directory-p p))
        (dolist (f (context-navigator-add-collect-recursive p))
          (let ((sz (context-navigator-add-file-size f)))
            (when (file-remote-p f) (setq remote (1+ remote)))
            (cond
             ((null sz) (setq skipped-nonregular (1+ skipped-nonregular)))
             ((> sz limit) (setq skipped-too-big (1+ skipped-too-big)))
             (t (push f files))))))
       ((context-navigator-add-regular-file-p p)
        (let ((sz (context-navigator-add-file-size p)))
          (when (file-remote-p p) (setq remote (1+ remote)))
          (if (and sz (> sz limit))
              (setq skipped-too-big (1+ skipped-too-big))
            (push p files))))
       (t
        (setq skipped-nonregular (1+ skipped-nonregular)))))
    (list :files (nreverse (delete-dups files))
          :skipped-too-big skipped-too-big
          :skipped-nonregular skipped-nonregular
          :remote remote)))

;; ---------------- Preview UI ----------------

(defun context-navigator-add-preview-and-confirm (files stats)
  "Show preview buffer for FILES and STATS, return non-nil to proceed."
  (let* ((buf (get-buffer-create "*Context Navigator Add Preview*"))
         (total (length files))
         (too-big (plist-get stats :skipped-too-big))
         (nonreg (plist-get stats :skipped-nonregular))
         (remote (plist-get stats :remote))
         (sum-bytes (cl-loop for f in files
                             for s = (context-navigator-add-file-size f)
                             when s sum s)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format (context-navigator-i18n :preview-title)
                        total
                        (context-navigator-human-size sum-bytes)))
        (insert "\n")
        (when (> too-big 0)
          (insert (format (context-navigator-i18n :preview-skipped-too-big)
                          too-big
                          (context-navigator-human-size context-navigator-max-file-size)))
          (insert "\n"))
        (when (> nonreg 0)
          (insert (format (context-navigator-i18n :preview-skipped-nonregular) nonreg))
          (insert "\n"))
        (when (> remote 0)
          (insert (format (context-navigator-i18n :preview-remote) remote))
          (insert "\n"))
        (insert "\n")
        (insert (context-navigator-i18n :preview-files))
        (insert "\n")
        (dolist (f files)
          (insert (format "  %s (%s)\n"
                          (abbreviate-file-name f)
                          (context-navigator-human-size (context-navigator-add-file-size f)))))
        (goto-char (point-min))
        (view-mode 1)))
    (display-buffer buf '((display-buffer-pop-up-window)))
    (unwind-protect
        (context-navigator-ui-ask :confirm-add total)
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;; ---------------- Pure-ish builders ----------------

(defun context-navigator-add--normalize-paths (files)
  "Return list of absolute paths from FILES, dropping non-strings."
  (delq nil (mapcar (lambda (p) (and (stringp p) (expand-file-name p))) files)))

(defun context-navigator-add--build-file-item (path)
  "Create a file item for PATH (absolute)."
  (when (and (stringp path) (file-exists-p path) (not (file-directory-p path)))
    (context-navigator-item-create
     :type 'file
     :name (file-name-nondirectory path)
     :path (expand-file-name path)
     :enabled t)))

(defun context-navigator-add--dedupe-old-items (old-items abs-hash)
  "Return OLD-ITEMS filtered to drop any that reference ABS-HASH keys by path."
  (cl-remove-if
   (lambda (it)
     (let* ((p (context-navigator-item-path it))
            (bp (and (bufferp (context-navigator-item-buffer it))
                     (buffer-live-p (context-navigator-item-buffer it))
                     (buffer-local-value 'buffer-file-name (context-navigator-item-buffer it))))
            (pp (and (stringp p) (expand-file-name p)))
            (bb (and (stringp bp) (expand-file-name bp))))
       (or (and pp (gethash pp abs-hash))
           (and bb (gethash bb abs-hash)))))
   (or old-items '())))

(defun context-navigator-add--merge-files-into-items (old-items files)
  "Return cons (MERGED . NEW-ITEMS) by merging FILES into OLD-ITEMS, deduping by path."
  (let* ((abs (context-navigator-add--normalize-paths files))
         (aset (let ((h (make-hash-table :test 'equal)))
                 (dolist (p abs) (puthash p t h)) h))
         (keep (context-navigator-add--dedupe-old-items old-items aset))
         (new-items (delq nil (mapcar #'context-navigator-add--build-file-item files)))
         (merged (append keep new-items)))
    (cons merged new-items)))

;; ---------------- gptel batched apply helpers ----------------

(defun context-navigator-add--refresh-gptel-keys-snapshot ()
  "Refresh cached gptel keys snapshot in Navigator buffer to update indicators."
  (ignore-errors
    (let* ((lst (context-navigator-gptel-pull))
           (keys (and (listp lst)
                      (mapcar #'context-navigator-model-item-key lst)))
           (h (sxhash-equal keys)))
      (with-current-buffer (get-buffer-create "*context-navigator*")
        (setq-local context-navigator-view--gptel-keys keys)
        (setq-local context-navigator-view--gptel-keys-hash h)))))

(defun context-navigator-add--apply-items-batched (items)
  "Background-apply ITEMS to gptel via core batch when push is ON."
  (when (and (boundp 'context-navigator--push-to-gptel)
             context-navigator--push-to-gptel
             (listp items))
    (let* ((st (context-navigator--state-get))
           (token (and st (context-navigator-state-load-token st))))
      (ignore-errors
        (let ((context-navigator-gptel-require-visible-window nil))
          (context-navigator--gptel-defer-or-start items token))))
    ;; Best-effort immediate refresh of indicators; events will refine it later.
    (run-at-time 0.1 nil #'context-navigator-add--refresh-gptel-keys-snapshot)))

;; ---------------- Mutators (model + apply + UI) ----------------

(defun context-navigator-add-files (files)
  "Add FILES (list of paths) to the model, deduping by absolute path.
Replace any existing items that reference the same files. Apply to gptel (batched) when enabled."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (old (and (context-navigator-state-p st) (context-navigator-state-items st)))
         (res (context-navigator-add--merge-files-into-items old files))
         (merged (car res))
         (new-items (cdr res)))
    (context-navigator-set-items merged)
    (context-navigator-add--apply-items-batched new-items)
    (context-navigator-ui-info :added-files (length new-items))))

;; ---------------- Universal add dispatch helpers ----------------

(defun context-navigator-add--region-active-p ()
  "Return non-nil if there is an active region or a non-point mark."
  (let ((mk (mark t)))
    (or (use-region-p)
        (and (number-or-marker-p mk) (/= (point) mk)))))

(defun context-navigator-add--dired-selection ()
  "Handle Dired selection for universal add."
  (let* ((sel (dired-get-marked-files nil nil)))
    (if (null sel)
        (context-navigator-ui-info :no-files-selected)
      (let* ((has-dir (cl-some #'file-directory-p sel))
             (stats (context-navigator-add-gather-files sel))
             (files (plist-get stats :files))
             (remote (plist-get stats :remote)))
        (if has-dir
            (progn
              (if (and (> remote 0)
                       (not (context-navigator-ui-ask :warn-remote-selected remote)))
                  (context-navigator-ui-info :aborted)
                (if (context-navigator-add-preview-and-confirm files stats)
                    (context-navigator-add-files files)
                  (context-navigator-ui-info :aborted))))
          (context-navigator-add-files files))))))

(defun context-navigator-add--add-selection-from-region ()
  "Create and add a selection item from the active region/mark."
  (let* ((buf (current-buffer))
         (p   (buffer-file-name buf))
         (mk  (mark t))
         (beg (if (use-region-p)
                  (region-beginning)
                (min (point) (or (and (number-or-marker-p mk) (prefix-numeric-value mk)) (point)))))
         (end (if (use-region-p)
                  (region-end)
                (max (point) (or (and (number-or-marker-p mk) (prefix-numeric-value mk)) (point)))))
         (nm (if p
                 (format "%s:%s-%s" (file-name-nondirectory p) beg end)
               (format "%s:%s-%s" (buffer-name buf) beg end)))
         (sel (context-navigator-item-create
               :type 'selection :name nm
               :path p :buffer buf :beg beg :end end :enabled t)))
    (ignore-errors (context-navigator-add-item sel))
    ;; Fully clear region/mark so the next call won't treat a stale mark as selection
    (ignore-errors (deactivate-mark t))
    (ignore-errors (set-marker (mark-marker) nil (current-buffer)))
    ;; Apply only the selection (tests expect selection to be primary)
    (context-navigator-add--apply-items-batched (list sel))
    (context-navigator-ui-info :added-selection)))

(defun context-navigator-add--add-current-file ()
  "Add the current file-backed buffer as a file item."
  (let* ((p (buffer-file-name (current-buffer))))
    (if (file-remote-p p)
        (if (context-navigator-ui-ask :warn-remote-current)
            (context-navigator-add-files (list p))
          (context-navigator-ui-info :aborted))
      (context-navigator-add-files (list p)))))

(defun context-navigator-add--add-current-buffer ()
  "Add the current buffer as a buffer item."
  (let* ((b (current-buffer))
         (it (context-navigator-item-create
              :type 'buffer :name (buffer-name b) :buffer b :enabled t)))
    (ignore-errors (context-navigator-add-item it))
    (context-navigator-add--apply-items-batched (list it))
    (context-navigator-ui-info :added-buffer)))

;;;###autoload
(defun context-navigator-add-universal ()
  "Add current selection/file/buffer or Dired selection to the context.

Behavior:
- Dired:
  - If selection includes directories, collect files recursively with preview + confirmation.
  - Otherwise add marked files (filtering by max size).
- Active region: add as selection.
- File-backed buffer (no region): add file.
- Otherwise: add whole buffer (as buffer item).

TRAMP/remote: show a warning and confirm before proceeding."
  (interactive)
  (cond
   ((derived-mode-p 'dired-mode)
    (context-navigator-add--dired-selection))
   ((context-navigator-add--region-active-p)
    (context-navigator-add--add-selection-from-region))
   ((buffer-file-name (current-buffer))
    (context-navigator-add--add-current-file))
   (t
    (context-navigator-add--add-current-buffer))))

(provide 'context-navigator-add)
;;; context-navigator-add.el ends here
