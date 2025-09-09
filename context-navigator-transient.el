;;; context-navigator-transient.el --- Transient menu for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Global transient on C-c n:
;;  n: toggle sidebar
;;  p: switch to current buffer's project
;;  a: add current file/region/buffer/dired selection (minimal)
;;  g: groups list
;;  s: save context
;;  l: load context
;;  u: unload context
;;  x: toggle push → gptel
;;  T: toggle auto-project
;;  P: push now
;;  C: clear gptel

;;; Code:

(require 'transient)
(require 'cl-lib)
(require 'subr-x)
(require 'dired)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-i18n)
(require 'context-navigator-path-add)
(require 'context-navigator-sidebar)
(require 'context-navigator-log)

(defgroup context-navigator-add nil
  "Settings for universal add operations."
  :group 'context-navigator)

(defcustom context-navigator-max-file-size (* 2 1024 1024)
  "Maximum file size (bytes) to include when adding files recursively.
Files larger than this threshold are skipped."
  :type 'integer :group 'context-navigator-add)

;;;###autoload
(transient-define-prefix context-navigator-transient ()
  "Context Navigator"
  [["Panel/Project"
    ("n" (lambda () (context-navigator-i18n :tr-toggle-sidebar)) context-navigator-sidebar-toggle)
    ("p" (lambda () (context-navigator-i18n :tr-switch-project)) context-navigator-switch-to-current-buffer-project)]
   ["Context/Groups"
    ("g" (lambda () (context-navigator-i18n :tr-groups-list)) context-navigator-sidebar-show-groups)
    ("X" (lambda () (context-navigator-i18n :tr-unload)) context-navigator-context-unload)]
   ["Actions"
    ("a" (lambda () (context-navigator-i18n :tr-add-universal)) context-navigator-add-universal)
    ("f" (lambda () (context-navigator-i18n :add-from-minibuf)) context-navigator-add-from-minibuffer)
    ("t" (lambda () (context-navigator-i18n :add-from-text)) context-navigator-add-from-text)
    ("o" (lambda () (context-navigator-i18n :tr-open-buffers)) context-navigator-sidebar-open-all-buffers)]
   ["GPTel"
    ("G" (lambda () (context-navigator-i18n :tr-toggle-push)) context-navigator-toggle-push-to-gptel)
    ("A" (lambda () (context-navigator-i18n :tr-toggle-auto)) context-navigator-toggle-auto-project-switch)
    ("P" (lambda () (context-navigator-i18n :tr-push-now)) context-navigator-push-to-gptel-now)
    ("C" (lambda () (context-navigator-i18n :clear-gptel)) context-navigator-clear-gptel-now)]
   ["Logs"
    ("D" "Toggle logs" context-navigator-log-toggle)
    ("L" "Open logs" context-navigator-log-open)
    ("K" "Clear logs" context-navigator-log-clear)
    ("V" "Set level" context-navigator-log-set-level)
    ("F" "Toggle file log" context-navigator-log-toggle-file-persistence)]])


(defun context-navigator-transient--maybe-apply-to-gptel ()
  "Apply current model to gptel when push is ON."
  (when (and (boundp 'context-navigator--push-to-gptel)
             context-navigator--push-to-gptel)
    (let* ((st (context-navigator--state-get))
           (items (and st (context-navigator-state-items st))))
      (ignore-errors (context-navigator-gptel-apply (or items '()))))))

(defun context-navigator-transient--apply-items-batched (items)
  "Background-apply ITEMS to gptel via core batch when push is ON."
  (when (and (boundp 'context-navigator--push-to-gptel)
             context-navigator--push-to-gptel
             (listp items))
    (let* ((st (context-navigator--state-get))
           (token (and st (context-navigator-state-load-token st))))
      (ignore-errors
        ;; Применяем сразу, даже без видимого окна gptel.
        (let ((context-navigator-gptel-require-visible-window nil))
          (context-navigator--gptel-defer-or-start items token))))))

(defun context-navigator-transient--regular-file-p (path)
  "Return non-nil if PATH is a regular file."
  (and (stringp path)
       (file-exists-p path)
       (file-regular-p path)))

(defun context-navigator-transient--file-size (path)
  "Return size of PATH in bytes or nil."
  (when (and (stringp path) (file-exists-p path))
    (let ((attrs (file-attributes path 'string)))
      (and attrs (file-attribute-size attrs)))))

(defun context-navigator-transient--collect-recursive (dir)
  "Collect regular files under DIR (recursive)."
  (let* ((all (ignore-errors (directory-files-recursively dir ".*" t)))
         (files (cl-remove-if-not #'file-regular-p all)))
    files))

(defun context-navigator-transient--gather-files (paths)
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
        (dolist (f (context-navigator-transient--collect-recursive p))
          (let ((sz (context-navigator-transient--file-size f)))
            (when (file-remote-p f) (setq remote (1+ remote)))
            (cond
             ((null sz) (setq skipped-nonregular (1+ skipped-nonregular)))
             ((> sz limit) (setq skipped-too-big (1+ skipped-too-big)))
             (t (push f files))))))
       ((context-navigator-transient--regular-file-p p)
        (let ((sz (context-navigator-transient--file-size p)))
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

(defun context-navigator-transient--human-size (bytes)
  "Return human-readable size for BYTES."
  (cond
   ((null bytes) "?")
   ((< bytes 1024) (format "%d B" bytes))
   ((< bytes (* 1024 1024)) (format "%.1f KB" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1f MB" (/ bytes 1048576.0)))
   (t (format "%.1f GB" (/ bytes 1073741824.0)))))

(defun context-navigator-transient--preview-and-confirm (files stats)
  "Show preview buffer for FILES and STATS, return non-nil to proceed."
  (let* ((buf (get-buffer-create "*Context Navigator Add Preview*"))
         (total (length files))
         (too-big (plist-get stats :skipped-too-big))
         (nonreg (plist-get stats :skipped-nonregular))
         (remote (plist-get stats :remote))
         (sum-bytes (cl-loop for f in files
                             for s = (context-navigator-transient--file-size f)
                             when s sum s)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format (context-navigator-i18n :preview-title)
                        total
                        (context-navigator-transient--human-size sum-bytes)))
        (insert "\n")
        (when (> too-big 0)
          (insert (format (context-navigator-i18n :preview-skipped-too-big)
                          too-big
                          (context-navigator-transient--human-size context-navigator-max-file-size)))
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
                          (context-navigator-transient--human-size (context-navigator-transient--file-size f)))))
        (goto-char (point-min))
        (view-mode 1)))
    (display-buffer buf '((display-buffer-pop-up-window)))
    (unwind-protect
        (yes-or-no-p (format (context-navigator-i18n :confirm-add) total))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun context-navigator-transient--add-files (files)
  "Add FILES (list of paths) as enabled items."
  (let ((new-items '()))
    (dolist (p files)
      (when (and (stringp p) (file-exists-p p) (not (file-directory-p p)))
        (let* ((it (context-navigator-item-create
                    :type 'file
                    :name (file-name-nondirectory p)
                    :path (expand-file-name p)
                    :enabled t)))
          (ignore-errors (context-navigator-add-item it))
          (push it new-items))))
    (context-navigator-transient--apply-items-batched (nreverse new-items))
    (message (context-navigator-i18n :added-files) (length files))))

;;;###autoload
(defun context-navigator-add-universal ()
  "Add current selection/file/buffer or Dired selection to context.

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
   ;; Dired: marked files (recursive for directories)
   ((derived-mode-p 'dired-mode)
    (let* ((sel (dired-get-marked-files nil nil)))
      (if (null sel)
          (message "%s" (context-navigator-i18n :no-files-selected))
        (let* ((has-dir (cl-some #'file-directory-p sel))
               (stats (context-navigator-transient--gather-files sel))
               (files (plist-get stats :files))
               (remote (plist-get stats :remote)))
          (if has-dir
              (progn
                ;; При наличии директорий — предупреждаем о TRAMP и показываем предпросмотр c подтверждением
                (if (and (> remote 0)
                         (not (yes-or-no-p (format (context-navigator-i18n :warn-remote-selected) remote))))
                    (message "%s" (context-navigator-i18n :aborted))
                  (if (context-navigator-transient--preview-and-confirm files stats)
                      (context-navigator-transient--add-files files)
                    (message "%s" (context-navigator-i18n :aborted)))))
            ;; Только файлы: добавляем без каких-либо вопросов/предпросмотров
            (context-navigator-transient--add-files files))))))

   ;; Active region -> selection item
   ((use-region-p)
    (let* ((b   (current-buffer))
           (p   (buffer-file-name b))
           (beg (region-beginning))
           (end (region-end))
           (nm (if p
                   (format "%s:%s-%s" (file-name-nondirectory p) beg end)
                 (format "%s:%s-%s" (buffer-name b) beg end)))
           (it (context-navigator-item-create
                :type 'selection :name nm
                :path p :buffer b :beg beg :end end :enabled t)))
      (ignore-errors (context-navigator-add-item it))
      ;; Clear mark so subsequent calls don't treat stale mark as selection
      (ignore-errors
        (set-marker (mark-marker) nil (current-buffer))
        (setq mark-active nil))
      (context-navigator-transient--apply-items-batched (list it))
      (message "%s" (context-navigator-i18n :added-selection))))
   ;; File-backed buffer -> file
   ((buffer-file-name (current-buffer))
    (let* ((p (buffer-file-name (current-buffer))))
      (if (file-remote-p p)
          (if (yes-or-no-p (context-navigator-i18n :warn-remote-current))
              (context-navigator-transient--add-files (list p))
            (message "%s" (context-navigator-i18n :aborted)))
        (context-navigator-transient--add-files (list p)))))
   ;; Fallback: buffer item
   (t
    (let* ((b (current-buffer))
           (it (context-navigator-item-create
                :type 'buffer :name (buffer-name b) :buffer b :enabled t)))
      (ignore-errors (context-navigator-add-item it))
      (context-navigator-transient--apply-items-batched (list it))
      (message "%s" (context-navigator-i18n :added-buffer))))))


(provide 'context-navigator-transient)
;;; context-navigator-transient.el ends here
