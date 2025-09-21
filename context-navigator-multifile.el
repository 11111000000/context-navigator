;;; context-navigator-multifile.el --- Multifile/Chunk-edit view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Aggregating buffer to browse and (safely) edit context items across multiple files.
;; - One buffer with sections per item (icon, name, relpath, indicator)
;; - n/p (j/k) navigate sections; RET: visit/edit; t toggle; d delete; p push; q quit
;; - Selection items open indirect buffers narrowed to region for safe editing
;; - File items show read-only head preview; RET visits the file
;; - Auto-push to gptel after save when push→gptel is ON
;;
;; MVP: minimal, robust skeleton with lazy subscriptions and cleanup.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-icons)
(require 'context-navigator-events)
(require 'context-navigator-i18n)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-controls-icons)

;; Forward decls (byte-compiler friendliness)
(declare-function context-navigator-state-items "context-navigator-core" (state))
(declare-function context-navigator-state-last-project-root "context-navigator-core" (state))
(declare-function context-navigator-toggle-item "context-navigator-core" (key &optional enabled))
(declare-function context-navigator-remove-item-by-key "context-navigator-core" (key))

(defgroup context-navigator-multifile nil
  "Multifile/Chunk-edit view for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-mf-buffer-name "*Context Multifile View*"
  "Buffer name for the Multifile View."
  :type 'string :group 'context-navigator-multifile)

(defcustom context-navigator-mf-file-preview-lines 20
  "How many lines of file content to preview per file item."
  :type 'integer :group 'context-navigator-multifile)

(defcustom context-navigator-mf-selection-preview-lines 12
  "How many lines of selection content to preview per selection item."
  :type 'integer :group 'context-navigator-multifile)

(defcustom context-navigator-mf-kill-indirect-on-close t
  "When non-nil, kill all indirect edit buffers created by the view on close."
  :type 'boolean :group 'context-navigator-multifile)

(defcustom context-navigator-mf-open-all-threshold 50
  "Warn when creating more than this many indirect buffers with Edit-All."
  :type 'integer :group 'context-navigator-multifile)

(defcustom context-navigator-mf-remote-preview-mode 'lazy
  "Remote preview policy for TRAMP:
- lazy  : do not read files, show stub note
- strict: read head of remote file (may be slow)
- off   : hide preview for remote files"
  :type '(choice (const lazy) (const strict) (const off))
  :group 'context-navigator-multifile)

(defcustom context-navigator-mf-preview-full t
  "When non-nil, file items show full file content in the preview by default."
  :type 'boolean :group 'context-navigator-multifile)

(defcustom context-navigator-mf-file-preview-max-bytes 0
  "Maximum bytes to read for file preview when `context-navigator-mf-preview-full' is non-nil.
0 or nil means unlimited (read whole file)."
  :type 'integer :group 'context-navigator-multifile)

;; Buffer-local state
(defvar-local context-navigator-mf--subs nil)
(defvar-local context-navigator-mf--items nil)
(defvar-local context-navigator-mf--pos->key nil)         ;; alist (pos . key) for section headers
(defvar-local context-navigator-mf--key->indirect nil)    ;; hash key -> indirect buffer
(defvar-local context-navigator-mf--gptel-keys nil)       ;; cached stable keys from gptel
(defvar-local context-navigator-mf--filter-enabled-only nil)
(defvar-local context-navigator-mf--collapsed-all nil)    ;; collapse all previews
(defvar-local context-navigator-mf--collapsed-keys (make-hash-table :test 'equal)) ;; per-section collapse state

(defvar context-navigator-multifile-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'context-navigator-multifile-activate)
    (define-key m (kbd "C-m") #'context-navigator-multifile-activate)
    (define-key m (kbd "v")   #'context-navigator-multifile-visit)
    (define-key m (kbd "n")   #'context-navigator-multifile-next)
    (define-key m (kbd "j")   #'context-navigator-multifile-next)
    (define-key m (kbd "k")   #'context-navigator-multifile-prev)
    (define-key m (kbd "t")   #'context-navigator-multifile-toggle)
    (define-key m (kbd "d")   #'context-navigator-multifile-delete)
    (define-key m (kbd "p")   #'context-navigator-multifile-push)
    (define-key m (kbd "P")   #'context-navigator-multifile-push)
    (define-key m (kbd "f")   #'context-navigator-multifile-toggle-filter)
    (define-key m (kbd "E")   #'context-navigator-multifile-edit-all)
    (define-key m (kbd "z")   #'context-navigator-multifile-toggle-collapse-all)
    ;; Toggle section collapse on TAB when point is on a header
    (define-key m (kbd "TAB")       #'context-navigator-multifile-toggle-section)
    (define-key m (kbd "<tab>")     #'context-navigator-multifile-toggle-section)
    (define-key m [tab]             #'context-navigator-multifile-toggle-section)
    (define-key m (kbd "C-i")       #'context-navigator-multifile-toggle-section)
    (define-key m (kbd "?")   #'context-navigator-multifile-help)
    (define-key m (kbd "q")   #'context-navigator-multifile-close)
    m)
  "Keymap for `context-navigator-multifile-mode'.")

(define-derived-mode context-navigator-multifile-mode special-mode "Context-MF"
  "Major mode for Context Multifile View."
  (setq buffer-read-only t
        truncate-lines t)
  (hl-line-mode 1)
  ;; Header-line controls for Multifile buffer
  (setq header-line-format '((:eval (context-navigator-multifile--headerline-format))))
  (add-hook 'kill-buffer-hook #'context-navigator-multifile--cleanup nil t))

(defun context-navigator-multifile--visible-p ()
  (get-buffer-window (current-buffer) t))

(defun context-navigator-multifile--icon-or-label (key &optional state text)
  "Return icon for KEY with STATE or fallback TEXT (space-prefixed)."
  (let ((ico (and (fboundp 'context-navigator-controls-icons-available-p)
                  (context-navigator-controls-icons-available-p)
                  (context-navigator-controls-icon key state))))
    (if (and (stringp ico) (> (length ico) 0))
        (concat " " ico)
      (concat " " (or text (symbol-name key))))))

(defun context-navigator-multifile--headerline-format ()
  "Build header-line controls for the Multifile buffer."
  (let* ((state (if context-navigator-mf--collapsed-all 'off 'on))
         (seg-collapse (propertize
                        (context-navigator-multifile--icon-or-label 'mf-collapse state "[↧]")
                        'mouse-face 'highlight
                        'help-echo (if (eq state 'on) "Collapse all previews (z)" "Expand all previews (z)")
                        'keymap (let ((m (make-sparse-keymap)))
                                  (define-key m [mouse-1] #'context-navigator-multifile-toggle-collapse-all)
                                  m)))
         (seg-filter (propertize
                      (context-navigator-multifile--icon-or-label 'mf-filter
                                                                  (if context-navigator-mf--filter-enabled-only 'on 'off)
                                                                  "[F]")
                      'mouse-face 'highlight
                      'help-echo "Toggle filter: enabled/all (f)"
                      'keymap (let ((m (make-sparse-keymap)))
                                (define-key m [mouse-1] #'context-navigator-multifile-toggle-filter)
                                m)))
         (seg-edit-all (propertize
                        (context-navigator-multifile--icon-or-label 'mf-edit-all nil "[Edit All]")
                        'mouse-face 'highlight
                        'help-echo "Open edit buffers for all selections (E)"
                        'keymap (let ((m (make-sparse-keymap)))
                                  (define-key m [mouse-1] #'context-navigator-multifile-edit-all)
                                  m)))
         (seg-close (propertize
                     " [×]"
                     'mouse-face 'highlight
                     'help-echo "Close Multifile (q)"
                     'keymap (let ((m (make-sparse-keymap)))
                               (define-key m [mouse-1] #'context-navigator-multifile-close)
                               m))))
    (concat seg-collapse seg-filter seg-edit-all seg-close)))

(defun context-navigator-multifile--section-collapsed-p (key)
  "Return non-nil when section KEY is collapsed.
Global collapse overrides per-section state."
  (or context-navigator-mf--collapsed-all
      (and (hash-table-p context-navigator-mf--collapsed-keys)
           (gethash key context-navigator-mf--collapsed-keys))))

(defun context-navigator-multifile-toggle-section ()
  "Toggle collapse for section at point when on a header line.
If not on a header, move to next section."
  (interactive)
  (let* ((on-hdr (get-text-property (point) 'cn-mf-section))
         (it (or (context-navigator-multifile--item-at-point)
                 (let ((pos (previous-single-property-change (1+ (point)) 'cn-mf-section nil (point-min))))
                   (and pos (goto-char pos) (context-navigator-multifile--item-at-point))))))
    (if (not (and on-hdr it))
        (context-navigator-multifile-next)
      (let* ((key (context-navigator-model-item-key it)))
        (unless (hash-table-p context-navigator-mf--collapsed-keys)
          (setq context-navigator-mf--collapsed-keys (make-hash-table :test 'equal)))
        (if (gethash key context-navigator-mf--collapsed-keys)
            (remhash key context-navigator-mf--collapsed-keys)
          (puthash key t context-navigator-mf--collapsed-keys))
        (context-navigator-multifile--render)))))

(defun context-navigator-multifile-mouse-toggle-section (event)
  "Mouse click toggles the section under EVENT."
  (interactive "e")
  (mouse-set-point event)
  (context-navigator-multifile-toggle-section))

(defun context-navigator-multifile--cleanup ()
  "Unsubscribe and optionally kill indirect edit buffers."
  (when context-navigator-mf--subs
    (mapc #'context-navigator-events-unsubscribe context-navigator-mf--subs)
    (setq context-navigator-mf--subs nil))
  (when (and context-navigator-mf-kill-indirect-on-close
             (hash-table-p context-navigator-mf--key->indirect))
    (maphash
     (lambda (_key buf)
       (when (buffer-live-p buf)
         (ignore-errors (kill-buffer buf))))
     context-navigator-mf--key->indirect))
  (setq context-navigator-mf--key->indirect (make-hash-table :test 'equal))
  t)

(defun context-navigator-multifile--subscribe ()
  "Subscribe to model/gptel events buffer-locally."
  (setq context-navigator-mf--subs nil)
  (push (context-navigator-events-subscribe
         :model-refreshed
         (lambda (&rest _)
           (when (get-buffer context-navigator-mf-buffer-name)
             (with-current-buffer context-navigator-mf-buffer-name
               (context-navigator-multifile--render)))))
        context-navigator-mf--subs)
  (push (context-navigator-events-subscribe
         :gptel-change
         (lambda (&rest _)
           (when (get-buffer context-navigator-mf-buffer-name)
             (with-current-buffer context-navigator-mf-buffer-name
               (context-navigator-multifile--update-gptel-keys)
               (context-navigator-multifile--render)))))
        context-navigator-mf--subs))

(defun context-navigator-multifile--pull-items ()
  "Fetch items from core; optionally filter via local toggle only.

Note: Always show all items by default. Local filter (f) can show only enabled."
  (let* ((st (context-navigator--state-get))
         (lst (and st (context-navigator-state-items st))))
    (setq context-navigator-mf--items
          (if context-navigator-mf--filter-enabled-only
              (cl-remove-if-not #'context-navigator-item-enabled lst)
            lst))))

(defun context-navigator-multifile--update-gptel-keys ()
  "Refresh cached gptel keys snapshot."
  (let* ((lst (ignore-errors (context-navigator-gptel-pull)))
         (keys (and (listp lst)
                    (mapcar #'context-navigator-model-item-key lst))))
    (setq context-navigator-mf--gptel-keys keys)))

(defun context-navigator-multifile--indicator (present)
  (let ((raw (if present "●" "○"))
        (color (if present "green4" "gray")))
    (propertize raw 'face (list :foreground color :height 0.8)
                'display '(raise 0.08))))

(defun context-navigator-multifile--icon (item)
  (or (ignore-errors (context-navigator-icons-for-item item)) ""))

(defun context-navigator-multifile--relpath (p root)
  (condition-case _err
      (if (and (stringp root) (stringp p))
          (file-relative-name (expand-file-name p)
                              (file-name-as-directory (expand-file-name root)))
        p)
    (error p)))

(defun context-navigator-multifile--insert-header (item root)
  "Insert section header for ITEM, return start position.

Header shows:
- gptel indicator + icon + name [+ enabled status]
- relative path on the right
- clickable actions: [visit] [t] [d] [p]"
  (let* ((key (context-navigator-model-item-key item))
         (name (or (context-navigator-item-name item) key))
         (path (or (context-navigator-item-path item) ""))
         (rel  (context-navigator-multifile--relpath path root))
         (present (and context-navigator-mf--gptel-keys
                       (member key context-navigator-mf--gptel-keys)))
         (ind  (context-navigator-multifile--indicator present))
         (ic   (context-navigator-multifile--icon item))
         (en   (and (context-navigator-item-enabled item) t))
         (en-tag (if en
                     (propertize "[on]" 'face 'success)
                   (propertize "[off]" 'face 'shadow)))
         (left (string-trim (mapconcat #'identity (delq nil (list ind ic name en-tag)) " ")))
         (right (if (and (stringp rel) (not (string-empty-p rel)))
                    (format " — %s" rel) "")))
    (let ((start (point)))
      ;; Base header text
      (insert (format "%s%s" left right))
      ;; Actions: build clickable segments and append
      (let ((mk (lambda (label cmd hint)
                  (let* ((s (copy-sequence (format " [%s]" label)))
                         (km (let ((m (make-sparse-keymap)))
                               (define-key m [mouse-1] cmd)
                               (define-key m (kbd "RET") cmd)
                               m)))
                    (add-text-properties 0 (length s)
                                         (list 'mouse-face 'highlight
                                               'help-echo hint
                                               'keymap km
                                               'local-map km
                                               'cn-mf-action t
                                               'cn-item item)
                                         s)
                    s))))
        (let* ((seg-visit (funcall mk "visit" #'context-navigator-multifile-visit "RET/mouse-1: visit"))
               (seg-t     (funcall mk "t"     #'context-navigator-multifile-toggle "t/mouse-1: toggle enable"))
               (seg-d     (funcall mk "d"     #'context-navigator-multifile-delete "d/mouse-1: delete"))
               (seg-p     (funcall mk "p"     #'context-navigator-multifile-push   "p/mouse-1: push")))
          (insert " " seg-visit seg-t seg-d seg-p)))
      (insert "\n")
      ;; Apply header properties precisely on the header line we just inserted
      (let* ((hend (save-excursion (forward-line -1) (line-end-position)))
             (hbeg (save-excursion (forward-line -1) (line-beginning-position)))
             (km (let ((m (make-sparse-keymap)))
                   ;; Mouse click/TAB on header toggles collapse/expand of this section
                   (define-key m [mouse-1] #'context-navigator-multifile-mouse-toggle-section)
                   (define-key m (kbd "TAB")   #'context-navigator-multifile-toggle-section)
                   (define-key m (kbd "<tab>") #'context-navigator-multifile-toggle-section)
                   (define-key m [tab]         #'context-navigator-multifile-toggle-section)
                   (define-key m (kbd "C-i")   #'context-navigator-multifile-toggle-section)
                   m)))
        (put-text-property hbeg hend 'cn-mf-section t)
        (put-text-property hbeg hend 'cn-item item)
        (put-text-property hbeg hend 'mouse-face 'highlight)
        (put-text-property hbeg hend 'keymap km)
        (put-text-property hbeg hend 'local-map km)
        (put-text-property hbeg hend 'help-echo
                           "Click/TAB: collapse/expand; RET: visit/edit; t: toggle; d: delete; p: push"))
      (push (cons start key) context-navigator-mf--pos->key)
      start)))

(defun context-navigator-multifile--insert-preview-file (path)
  (let* ((remote (and (stringp path) (file-remote-p path)))
         (mode context-navigator-mf-remote-preview-mode))
    (cond
     ((null path) (insert "  <no path>\n"))
     ((and remote (eq mode 'off))
      (insert "  <remote preview disabled>\n"))
     ((and remote (eq mode 'lazy))
      (insert "  <remote preview (lazy): not reading file>\n"))
     (t
      (condition-case _err
          (let (rows)
            (with-temp-buffer
              (cond
               ((and context-navigator-mf-preview-full
                     (numberp context-navigator-mf-file-preview-max-bytes)
                     (> context-navigator-mf-file-preview-max-bytes 0))
                (insert-file-contents path nil 0 context-navigator-mf-file-preview-max-bytes))
               (context-navigator-mf-preview-full
                (insert-file-contents path))
               (t
                (insert-file-contents path nil 0 65536)))
              (goto-char (point-min))
              (if context-navigator-mf-preview-full
                  (while (not (eobp))
                    (push (buffer-substring (line-beginning-position) (line-end-position)) rows)
                    (forward-line 1))
                (dotimes (_i context-navigator-mf-file-preview-lines)
                  (push (buffer-substring (line-beginning-position) (line-end-position)) rows)
                  (forward-line 1))))
            (dolist (ln (nreverse rows))
              (insert (propertize (format "  %s\n" ln) 'face 'shadow))))
        (error (insert "  <unreadable>\n")))))))

(defun context-navigator-multifile--insert-preview-selection (item)
  (let* ((buf (or (context-navigator-item-buffer item)
                  (and (stringp (context-navigator-item-path item))
                       (file-exists-p (context-navigator-item-path item))
                       (find-file-noselect (context-navigator-item-path item)))))
         (b (context-navigator-item-beg item))
         (e (context-navigator-item-end item)))
    (cond
     ((not (buffer-live-p buf))
      (insert "  <selection: base buffer not available>\n"))
     ((or (not (integerp b)) (not (integerp e)))
      (insert "  <selection: invalid bounds>\n"))
     (t
      (let* ((s (with-current-buffer buf
                  (buffer-substring-no-properties (max (point-min) (min b e))
                                                  (min (point-max) (max b e)))))
             (lines (split-string s "\n"))
             (first (cl-subseq lines 0 (min context-navigator-mf-selection-preview-lines (length lines)))))
        (dolist (ln first)
          (insert (propertize (format "  %s\n" ln) 'face 'shadow))))))))

(defun context-navigator-multifile--insert-preview (item)
  "Insert preview block for ITEM."
  (pcase (context-navigator-item-type item)
    ('file (context-navigator-multifile--insert-preview-file (context-navigator-item-path item)))
    ('selection (context-navigator-multifile--insert-preview-selection item))
    ('buffer
     (let* ((p (context-navigator-item-path item)))
       (if (and (stringp p) (file-exists-p p))
           (context-navigator-multifile--insert-preview-file p)
         (insert "  <buffer-only; not editable here>\n"))))
    (_ (insert "  <unknown item>\n"))))

(defun context-navigator-multifile--render ()
  "Render the Multifile View."
  (let* ((inhibit-read-only t)
         (st (context-navigator--state-get))
         (root (and st (context-navigator-state-last-project-root st))))
    (erase-buffer)
    (setq context-navigator-mf--pos->key nil)
    (context-navigator-multifile--pull-items)
    (context-navigator-multifile--update-gptel-keys)
    (insert "🧭 Context Multifile View\n\n")
    (dolist (it (or context-navigator-mf--items '()))
      (context-navigator-multifile--insert-header it root)
      (let ((key (context-navigator-model-item-key it)))
        (unless (context-navigator-multifile--section-collapsed-p key)
          (context-navigator-multifile--insert-preview it)))
      (insert "\n"))
    ;; Footer hint
    (let ((hint (ignore-errors (context-navigator-i18n :menu-hint))))
      (when (stringp hint)
        (insert (propertize hint 'face 'shadow) "\n")))
    (goto-char (point-min))))

(defun context-navigator-multifile--item-at-point ()
  (get-text-property (point) 'cn-item))

(defun context-navigator-multifile--pos-for-key (key)
  "Return header start position for item with KEY in current buffer, or nil."
  (car (cl-find-if (lambda (cell) (equal (cdr cell) key))
                   context-navigator-mf--pos->key)))

(defun context-navigator-multifile-next ()
  "Move to the next section."
  (interactive)
  (let ((pos (next-single-property-change (point) 'cn-mf-section nil (point-max))))
    (when pos (goto-char pos))
    (unless pos
      (message "End of sections"))))

(defun context-navigator-multifile-prev ()
  "Move to the previous section."
  (interactive)
  (let ((pos nil)
        (p (previous-single-property-change (point) 'cn-mf-section nil (point-min))))
    ;; Walk back to the last header before point
    (while (and p (> p (point-min)))
      (setq pos p)
      (setq p (previous-single-property-change (max (1- p) (point-min)) 'cn-mf-section nil (point-min))))
    (if pos
        (goto-char pos)
      (message "Start of sections"))))

(defun context-navigator-multifile-visit ()
  "Visit item at point (file/buffer/selection jump)."
  (interactive)
  (let ((it (context-navigator-multifile--item-at-point)))
    (unless it (user-error "No item at point"))
    (pcase (context-navigator-item-type it)
      ('file
       (let ((p (context-navigator-item-path it)))
         (when (and (stringp p) (file-exists-p p))
           (find-file-other-window p))))
      ('buffer
       (let* ((buf (or (context-navigator-item-buffer it)
                       (and (context-navigator-item-path it)
                            (find-file-noselect (context-navigator-item-path it))))))
         (when (buffer-live-p buf)
           (switch-to-buffer-other-window buf))))
      ('selection
       (let* ((p (context-navigator-item-path it))
              (buf (or (context-navigator-item-buffer it)
                       (and (stringp p) (file-exists-p p)
                            (find-file-noselect p))))
              (b (context-navigator-item-beg it))
              (e (context-navigator-item-end it)))
         (when (buffer-live-p buf)
           (switch-to-buffer-other-window buf)
           (when (and (integerp b) (integerp e))
             (goto-char (min b e))
             (push-mark (max b e) t t)))))
      (_ (message "Unknown item type")))))

(defun context-navigator-multifile--indirect-after-save ()
  "After-save hook: auto-push current model to gptel when enabled."
  (when (and (boundp 'context-navigator--push-to-gptel)
             context-navigator--push-to-gptel)
    (let* ((st (context-navigator--state-get))
           (items (and st (context-navigator-state-items st))))
      (ignore-errors (context-navigator-gptel-apply (or items '()))))))

(defun context-navigator-multifile-edit ()
  "Edit selection at point via indirect buffer + narrow-to-region."
  (interactive)
  (let ((it (context-navigator-multifile--item-at-point)))
    (unless it (user-error "No item at point"))
    (pcase (context-navigator-item-type it)
      ('selection
       (let* ((p (context-navigator-item-path it))
              (base (or (context-navigator-item-buffer it)
                        (and (stringp p) (file-exists-p p)
                             (find-file-noselect p)))))
         (unless (buffer-live-p base)
           (user-error "Base buffer not available"))
         (unless (buffer-local-value 'buffer-file-name base)
           (user-error "Selection base buffer is not visiting a file — editing blocked"))
         (let* ((key (context-navigator-model-item-key it))
                (slug (substring key 0 (min 40 (length key))))
                (name (generate-new-buffer-name (format "*cn-edit:%s*" slug)))
                (b (context-navigator-item-beg it))
                (e (context-navigator-item-end it))
                (nb b) (ne e))
           ;; Clamp region to current base buffer bounds to avoid errors on stale positions
           (with-current-buffer base
             (let ((pmin (point-min)) (pmax (point-max)))
               (setq nb (max pmin (min b e)))
               (setq ne (min pmax (max b e)))))
           (let ((ind (with-current-buffer base
                        (clone-indirect-buffer-other-window name t))))
             (with-current-buffer ind
               (let ((inhibit-read-only t))
                 (widen)
                 (narrow-to-region nb ne)
                 (setq-local buffer-read-only nil)
                 (add-hook 'after-save-hook #'context-navigator-multifile--indirect-after-save nil t)
                 ;; Track in mapping; cleanup on kill
                 (with-current-buffer context-navigator-mf-buffer-name
                   (puthash key ind context-navigator-mf--key->indirect))
                 (add-hook 'kill-buffer-hook
                           (lambda ()
                             (let ((dead (current-buffer)))
                               (when (get-buffer context-navigator-mf-buffer-name)
                                 (with-current-buffer context-navigator-mf-buffer-name
                                   (maphash
                                    (lambda (k b)
                                      (when (eq b dead)
                                        (remhash k context-navigator-mf--key->indirect)))
                                    context-navigator-mf--key->indirect))))))
                 (message "Narrowed edit; C-x C-s to save"))))))))
    (_
     (context-navigator-multifile-visit))))

(defun context-navigator-multifile-toggle ()
  "Toggle enabled flag for item at point."
  (interactive)
  (let* ((it (context-navigator-multifile--item-at-point)))
    (unless it (user-error "No item at point"))
    (let* ((key (context-navigator-model-item-key it)))
      (ignore-errors (context-navigator-toggle-item key))
      (when (and (boundp 'context-navigator--push-to-gptel)
                 context-navigator--push-to-gptel)
        (let* ((st (context-navigator--state-get))
               (items (and st (context-navigator-state-items st))))
          (ignore-errors (context-navigator-gptel-apply items))))
      (context-navigator-multifile--render))))

(defun context-navigator-multifile-delete ()
  "Delete item at point."
  (interactive)
  (let* ((it (context-navigator-multifile--item-at-point)))
    (unless it (user-error "No item at point"))
    (let* ((key (context-navigator-model-item-key it)))
      (ignore-errors (context-navigator-remove-item-by-key key))
      (when (hash-table-p context-navigator-mf--key->indirect)
        (let ((buf (gethash key context-navigator-mf--key->indirect)))
          (when (buffer-live-p buf) (ignore-errors (kill-buffer buf)))
          (remhash key context-navigator-mf--key->indirect)))
      (when (and (boundp 'context-navigator--push-to-gptel)
                 context-navigator--push-to-gptel)
        (let* ((st (context-navigator--state-get))
               (items (and st (context-navigator-state-items st))))
          (ignore-errors (context-navigator-gptel-apply items))))
      (context-navigator-multifile--render))))

(defun context-navigator-multifile-push ()
  "Toggle gptel membership for item at point."
  (interactive)
  (let ((it (context-navigator-multifile--item-at-point)))
    (unless it (user-error "No item at point"))
    (ignore-errors (context-navigator-gptel-toggle-one it))
    (context-navigator-multifile--update-gptel-keys)
    (context-navigator-multifile--render)))

(defun context-navigator-multifile-activate ()
  "RET: edit for selection; otherwise visit."
  (interactive)
  (let ((it (context-navigator-multifile--item-at-point)))
    (unless it (user-error "No item at point"))
    (if (eq (context-navigator-item-type it) 'selection)
        (context-navigator-multifile-edit)
      (context-navigator-multifile-visit))))

(defun context-navigator-multifile-toggle-filter ()
  "Toggle filter: enabled-only vs all (no push enforcement)."
  (interactive)
  (setq context-navigator-mf--filter-enabled-only
        (not context-navigator-mf--filter-enabled-only))
  (message (if context-navigator-mf--filter-enabled-only
               "Filter: enabled-only"
             "Filter: all items"))
  (context-navigator-multifile--render))

(defun context-navigator-multifile-toggle-collapse-all ()
  "Toggle collapse/expand for all previews."
  (interactive)
  (setq context-navigator-mf--collapsed-all (not context-navigator-mf--collapsed-all))
  ;; When expanding all, clear per-section collapsed state for a clean slate.
  (when (not context-navigator-mf--collapsed-all)
    (setq context-navigator-mf--collapsed-keys (make-hash-table :test 'equal)))
  (context-navigator-multifile--render))

(defun context-navigator-multifile-help ()
  "Open Navigator transient menu for quick help, or show a brief hint."
  (interactive)
  (cond
   ((fboundp 'context-navigator-view-transient)
    (call-interactively 'context-navigator-view-transient))
   (t
    (message "Multifile: j/k/n – navigate, RET – visit/edit, t – toggle, d – delete, p – push, f – filter, E – edit all, q – quit"))))

(defun context-navigator-multifile-edit-all ()
  "Open indirect buffers for all selection items (with threshold confirmation).

Uses section header positions to locate items robustly (no name search)."
  (interactive)
  (let* ((sels (cl-remove-if-not (lambda (it) (eq (context-navigator-item-type it) 'selection))
                                 (or context-navigator-mf--items '())))
         (n (length sels)))
    (when (and (> n context-navigator-mf-open-all-threshold)
               (not (yes-or-no-p (format "Open %d edit buffers? " n))))
      (cl-return-from context-navigator-multifile-edit-all (message "Aborted")))
    (save-excursion
      (dolist (it sels)
        (let* ((key (context-navigator-model-item-key it))
               (pos (context-navigator-multifile--pos-for-key key)))
          (when pos
            (goto-char pos)
            (ignore-errors (context-navigator-multifile-edit))))))

    (message "Opened %d selection edit buffers" n)))

;;;###autoload
(defun context-navigator-multifile-open ()
  "Open the Context Multifile View buffer."
  (interactive)
  (let* ((buf (get-buffer-create context-navigator-mf-buffer-name))
         (win (display-buffer buf '((display-buffer-pop-up-window)))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (context-navigator-multifile-mode)
        (setq-local context-navigator-mf--key->indirect (make-hash-table :test 'equal))
        (context-navigator-multifile--subscribe)
        (context-navigator-multifile--render)))
    (when (window-live-p win) (select-window win))
    win))

;;;###autoload
(defun context-navigator-multifile-close ()
  "Close Context Multifile View and cleanup."
  (interactive)
  (when-let ((buf (get-buffer context-navigator-mf-buffer-name)))
    (with-current-buffer buf
      (context-navigator-multifile--cleanup))
    (dolist (w (get-buffer-window-list buf nil t))
      (when (window-live-p w) (delete-window w)))
    (kill-buffer buf)))

(provide 'context-navigator-multifile)
;;; context-navigator-multifile.el ends here
