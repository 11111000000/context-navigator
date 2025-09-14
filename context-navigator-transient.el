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
(require 'context-navigator-view)
(require 'context-navigator-log)
(require 'context-navigator-razor)

(defgroup context-navigator-add nil
  "Settings for universal add operations."
  :group 'context-navigator)

(defcustom context-navigator-max-file-size (* 1 1024 1024)
  "Maximum file size (bytes) to include when adding files recursively.
Files larger than this threshold are skipped."
  :type 'integer :group 'context-navigator-add)

(defcustom context-navigator-transient-display 'auto
  "Preferred backend for displaying Context Navigator transients:
- auto     : use posframe when transient-posframe is available; otherwise a small window
- posframe : force posframe (requires the transient-posframe package)
- window   : always use a small fixed-height window below the selected window"
  :type '(choice (const auto) (const posframe) (const window))
  :group 'context-navigator-add)

;;;###autoload
(transient-define-prefix context-navigator-transient ()
  "Context Navigator"
  [["Panel/Project"
    ("n" (lambda () (context-navigator-i18n :tr-toggle-sidebar)) context-navigator-toggle)
    ("M" "Display Mode" context-navigator-display-mode-toggle)
    ("p" (lambda () (context-navigator-i18n :tr-switch-project)) context-navigator-switch-to-current-buffer-project)]
   ["Context/Groups"
    ("g" (lambda () (context-navigator-i18n :tr-groups-list)) context-navigator-view-show-groups)
    ("E" (lambda () (context-navigator-i18n :clear-group)) context-navigator-context-clear-current-group)]
   ["Actions"
    ("a" (lambda () (context-navigator-i18n :tr-add-universal)) context-navigator-add-universal)
    ("f" (lambda () (context-navigator-i18n :add-from-minibuf)) context-navigator-add-from-minibuffer)
    ("t" (lambda () (context-navigator-i18n :add-from-text)) context-navigator-add-from-text)
    ("o" (lambda () (context-navigator-i18n :tr-open-buffers)) context-navigator-view-open-all-buffers)]
   ["GPTel"
    ("G" (lambda () (context-navigator-i18n :tr-toggle-push)) context-navigator-toggle-push-to-gptel)
    ("A" (lambda () (context-navigator-i18n :tr-toggle-auto)) context-navigator-toggle-auto-project-switch)
    ("P" (lambda () (context-navigator-i18n :tr-push-now)) context-navigator-push-to-gptel-now)
    ("C" (lambda () (context-navigator-i18n :clear-gptel)) context-navigator-clear-gptel-now)
    ("R" (lambda () (context-navigator-i18n :tr-razor)) context-navigator-razor-run
     :if (lambda () (derived-mode-p 'org-mode)))]
   ["Logs"
    ("D" "Toggle logs" context-navigator-log-toggle)
    ("L" "Open logs" context-navigator-log-open)
    ("K" "Clear logs" context-navigator-log-clear)
    ("V" "Set level" context-navigator-log-set-level)
    ("F" "Toggle file log" context-navigator-log-toggle-file-persistence)]])

;;;###autoload
(transient-define-prefix context-navigator-view-transient ()
  "Navigator menu"
  [[:description (lambda () "Navigate")
                 ("RET" (lambda () (context-navigator-i18n :help-activate)) context-navigator-view-activate)
                 ("SPC" (lambda () (context-navigator-i18n :help-preview))  context-navigator-view-preview)
                 ("n"   (lambda () (context-navigator-i18n :help-next-item))     context-navigator-view-next-item)
                 ("p"   (lambda () (context-navigator-i18n :help-previous-item)) context-navigator-view-previous-item)
                 ("j"   (lambda () (context-navigator-i18n :help-next-item))     context-navigator-view-next-item)
                 ("k"   (lambda () (context-navigator-i18n :help-previous-item)) context-navigator-view-previous-item)
                 ("h"   (lambda ()
                          (if (eq context-navigator-view--mode 'groups)
                              (context-navigator-i18n :groups-help-back)
                            (context-navigator-i18n :items-help-view-groups)))
                  context-navigator-view-go-up)]
   [:description (lambda () "Items")
                 :if (lambda () (eq context-navigator-view--mode 'items))
                 ("t" (lambda () (context-navigator-i18n :help-toggle-gptel)) context-navigator-view-toggle-enabled)
                 ("d" (lambda () (context-navigator-i18n :help-delete))       context-navigator-view-delete-dispatch)
                 ("g" (lambda () (context-navigator-i18n :help-refresh))      context-navigator-view-refresh-dispatch)
                 ("o" (lambda ()
                        (cl-destructuring-bind (n . plus) (context-navigator-view--openable-count-get)
                          (format "%s (%d%s)" (context-navigator-i18n :open-buffers) n (if plus "+" ""))))
                  context-navigator-view-open-all-buffers)
                 ("K" (lambda ()
                        (let ((n (length (context-navigator-view--collect-closable-buffers))))
                          (format "%s (%d)" (context-navigator-i18n :close-buffers) n)))
                  context-navigator-view-close-all-buffers)]
   [:description (lambda () "Groups")
                 :if (lambda () (eq context-navigator-view--mode 'groups))
                 ("RET" (lambda () (context-navigator-i18n :groups-help-open))   context-navigator-view-activate)
                 ("a"   (lambda () (context-navigator-i18n :groups-help-add))    context-navigator-view-group-create)
                 ("r"   (lambda () (context-navigator-i18n :groups-help-rename)) context-navigator-view-group-rename)
                 ("c"   (lambda () (context-navigator-i18n :groups-help-copy))   context-navigator-view-group-duplicate)
                 ("d"   (lambda () (context-navigator-i18n :groups-help-delete)) context-navigator-view-delete-dispatch)
                 ("g"   (lambda () (context-navigator-i18n :groups-help-refresh)) context-navigator-view-refresh-dispatch)]
   ["Session"
    ("G" (lambda ()
           (format "Push→gptel: %s"
                   (if (and (boundp 'context-navigator--push-to-gptel)
                            context-navigator--push-to-gptel) "ON" "OFF")))
     context-navigator-view-toggle-push)
    ("A" (lambda ()
           (format "Auto-project: %s"
                   (if (and (boundp 'context-navigator--auto-project-switch)
                            context-navigator--auto-project-switch) "ON" "OFF")))
     context-navigator-view-toggle-auto-project)
    ("P" (lambda () (context-navigator-i18n :push-now))     context-navigator-view-push-now)
    ("C" (lambda () (context-navigator-i18n :clear-gptel)) context-navigator-view-clear-gptel)
    ("E" (lambda () (context-navigator-i18n :clear-group)) context-navigator-view-clear-group)
    ("X" (lambda () (context-navigator-i18n :tr-unload))   context-navigator-context-unload)]
   ["Help/Exit"
    ("H" (lambda () (context-navigator-i18n :help-help)) context-navigator-view-help)
    ("q" (lambda () (context-navigator-i18n :help-quit)) context-navigator-view-quit)]])

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

   ;; Active region -> selection item (robust even when transient-mark-mode is off)
   ((let ((mk (mark t)))
      (and (number-or-marker-p mk)
           (not (equal mk (point)))))
    (let* ((b   (current-buffer))
           (p   (buffer-file-name b))
           (mk  (mark t))
           (mpos (if (markerp mk) (marker-position mk) mk))
           (beg (min (point) mpos))
           (end (max (point) mpos))
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


;; Always show our transients in a small pop-up window, regardless of previous pop-up sizes.
;; Prefer posframe (via transient-posframe) when available and allowed.
;; We locally override `transient-display-buffer-action' around our two entry commands.
(defun context-navigator--with-small-transient (orig-fun &rest args)
  "Call ORIG-FUN displaying transient via posframe when available/allowed,
otherwise use a small fixed-height window."
  (let* ((backend (or context-navigator-transient-display 'auto))
         (use-posframe (and (memq backend '(auto posframe))
                            (require 'transient-posframe nil t))))
    (if use-posframe
        (progn
          ;; Enable posframe backend for transient when package is available.
          (when (fboundp 'transient-posframe-mode)
            (funcall 'transient-posframe-mode 1))
          (apply orig-fun args))
      ;; Fallback: small fixed-height pop-up window below the selected window.
      (let ((transient-display-buffer-action
             '(display-buffer-below-selected
               . ((side . bottom)
                  (slot . 0)
                  ;; Use a small fixed height (lines). Tweak to taste.
                  (window-height . 12)
                  ;; Keep it from being expanded by other commands.
                  (preserve-size . (t . nil))
                  (window-parameters . ((no-other-window . t)
                                        (no-delete-other-windows . t)))))))
        (apply orig-fun args)))))

(advice-add 'context-navigator-transient :around #'context-navigator--with-small-transient)
(advice-add 'context-navigator-view-transient :around #'context-navigator--with-small-transient)

(provide 'context-navigator-transient)
;;; context-navigator-transient.el ends here
