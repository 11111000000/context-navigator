;;; context-navigator-keyspec.el --- Middle-path keyspec for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; A small, centralized key spec (one source of truth) with minimal metadata:
;; - :id       stable identifier
;; - :cmd      command symbol
;; - :keys     list of key strings (kbd style)
;; - :contexts (items groups multifile global)
;; - :section  navigate | act | groups | session | tools
;; - :desc-key i18n key for help/labels (optional)
;;
;; Provides:
;; - context-navigator-keys-apply-to: apply keys for a given context to a keymap
;; - context-navigator-keys-help:     extract (section . ((key . desc) ...)) for a context

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup context-navigator-keys nil
  "Centralized key specification for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-keyspec
  '(
    ;; Global (sidebar buffer)
    (:id menu :cmd context-navigator-view-open-menu
         :keys ("?") :contexts (global) :section tools :desc-key :help-help)
    (:id quit :cmd context-navigator-view-quit
         :keys ("q") :contexts (global) :section tools :desc-key :help-quit)

    ;; Navigate (items/groups)
    (:id next :cmd context-navigator-view-next-item
         :keys ("j" "n" "<down>") :contexts (items groups-split) :section navigate :desc-key :help-next-item)
    (:id prev :cmd context-navigator-view-previous-item
         :keys ("k" "p" "<up>") :contexts (items groups-split) :section navigate :desc-key :help-previous-item)
    (:id activate :cmd context-navigator-view-activate
         :keys ("l" "RET" "<return>" "<kp-enter>") :contexts (items groups) :section navigate :desc-key :help-activate)
    (:id preview :cmd context-navigator-view-preview
         :keys ("v") :contexts (items) :section navigate :desc-key :help-preview)
    (:id tab-next :cmd context-navigator-view-tab-next
         :keys ("TAB" "<tab>" "C-i") :contexts (items groups) :section navigate :desc-key :help-next-item)
    (:id tab-prev :cmd context-navigator-view-tab-previous
         :keys ("<backtab>" "S-<tab>") :contexts (items groups) :section navigate :desc-key :help-previous-item)
    (:id groups-split :cmd context-navigator-groups-split-toggle
         :keys ("G" "h") :contexts (items groups-split) :section navigate :desc-key :toggle-groups-split)

    ;; Groups split (bottom panel) — dedicated context so bindings don’t clash with sidebar    
    (:id gs-close :cmd context-navigator-groups-split-close
         :keys ("q") :contexts (groups-split) :section navigate :desc-key :help-quit)
    (:id gs-next :cmd next-line
         :keys ("j" "n" "<down>") :contexts (groups-split) :section navigate :desc-key :help-next-item)
    (:id gs-prev :cmd previous-line
         :keys ("k" "p" "<up>") :contexts (groups-split) :section navigate :desc-key :help-previous-item)
    (:id gs-activate :cmd context-navigator-groups-split-select
         :keys ("l" "RET" "<return>" "<kp-enter>") :contexts (groups-split) :section navigate :desc-key :help-activate)
    (:id gs-refresh :cmd context-navigator-groups-open
         :keys ("g") :contexts (groups-split) :section act :desc-key :help-refresh)
    (:id gs-mg-toggle :cmd context-navigator-view-toggle-multi-group
         :keys ("M") :contexts (groups-split) :section groups :desc-key :toggle-multi-group)

    ;; Items actions
    ;; Items: toggle enabled (SPC/t)
    (:id toggle-dispatch :cmd context-navigator-view-toggle-dispatch
         :keys ("SPC" "t") :contexts (items) :section act :desc-key :help-toggle-gptel)
    ;; Groups: toggle selection (SPC/t)
    (:id toggle-dispatch-groups :cmd context-navigator-view-toggle-dispatch
         :keys ("SPC" "t") :contexts (groups) :section act :desc-key :toggle-multi-group)
    (:id toggle-all :cmd context-navigator-view-toggle-all-gptel
         :keys ("T") :contexts (items) :section act :desc-key :toggle-all-gptel)
    (:id disable-all :cmd context-navigator-view-disable-all-gptel
         :keys ("U") :contexts (items) :section act :desc-key :disable-all-gptel)
    (:id enable-all :cmd context-navigator-view-enable-all-gptel
         :keys ("M") :contexts (items) :section act :desc-key :enable-all-gptel)
    (:id delete :cmd context-navigator-view-delete-dispatch
         :keys ("d") :contexts (items) :section act :desc-key :help-delete)
    (:id refresh :cmd context-navigator-view-refresh-dispatch
         :keys ("g") :contexts (items groups) :section act :desc-key :help-refresh)
    (:id open-buffers :cmd context-navigator-view-open-all-buffers
         :keys ("o") :contexts (items) :section act :desc-key :help-open-all)
    (:id close-buffers :cmd context-navigator-view-close-all-buffers
         :keys ("c") :contexts (items) :section act :desc-key :close-buffers)
    (:id clear-group :cmd context-navigator-view-clear-group
         :keys ("x") :contexts (items) :section act :desc-key :help-clear-group)
    (:id clear-gptel :cmd context-navigator-view-clear-gptel
         :keys ("X") :contexts (items) :section act :desc-key :help-clear-gptel)
    (:id stats :cmd context-navigator-view-stats-toggle
         :keys ("S") :contexts (items groups groups-split) :section act :desc-key :stats)

    ;; Session (items/groups)
    (:id push-toggle :cmd context-navigator-view-toggle-push
         :keys ("V") :contexts (items groups) :section session :desc-key :help-toggle-push)
    (:id auto-toggle :cmd context-navigator-view-toggle-auto-project
         :keys ("A") :contexts (items groups) :section session :desc-key :help-toggle-auto)
    (:id push-now :cmd context-navigator-view-push-now
         :keys ("P") :contexts (items groups) :section session :desc-key :help-push-now)
    (:id unload :cmd context-navigator-context-unload
         :keys ("u") :contexts (items groups) :section session :desc-key :tr-unload)

    ;; Groups CRUD / selection
    (:id group-create :cmd context-navigator-view-group-create
         :keys ("a" "+") :contexts (groups) :section groups :desc-key :help-group-create)
    (:id group-rename :cmd context-navigator-view-group-rename
         :keys ("r") :contexts (groups) :section groups :desc-key :help-group-rename)
    (:id group-edit-desc :cmd context-navigator-view-group-edit-description
         :keys ("e") :contexts (groups) :section groups :desc-key :groups-help-edit-description)
    (:id group-duplicate :cmd context-navigator-view-group-duplicate
         :keys ("y") :contexts (groups) :section groups :desc-key :help-group-duplicate)
    (:id group-delete :cmd context-navigator-view-delete-dispatch
         :keys ("d") :contexts (groups) :section groups :desc-key :groups-help-delete)
    (:id group-toggle-select :cmd context-navigator-view-group-toggle-select
         :keys () :contexts (groups) :section groups :desc-key :toggle-multi-group)
    (:id multigroup-toggle :cmd context-navigator-view-toggle-multi-group
         :keys ("M") :contexts (groups) :section groups :desc-key :toggle-multi-group)

    ;; Multifile
    (:id mf-visit :cmd context-navigator-multifile-activate
         :keys ("RET" "<return>") :contexts (multifile) :section navigate :desc-key :mf-action-visit)
    (:id mf-next :cmd context-navigator-multifile-next
         :keys ("j" "n") :contexts (multifile) :section navigate :desc-key :help-next-item)
    (:id mf-prev :cmd context-navigator-multifile-prev
         :keys ("k" "p") :contexts (multifile) :section navigate :desc-key :help-previous-item)
    (:id mf-visit2 :cmd context-navigator-multifile-visit
         :keys ("v") :contexts (multifile) :section act :desc-key :mf-action-visit)
    (:id mf-toggle :cmd context-navigator-multifile-toggle
         :keys ("t") :contexts (multifile) :section act :desc-key :mf-action-toggle)
    (:id mf-delete :cmd context-navigator-multifile-delete
         :keys ("d") :contexts (multifile) :section act :desc-key :mf-action-delete)
    (:id mf-push :cmd context-navigator-multifile-push
         :keys ("P") :contexts (multifile) :section act :desc-key :mf-action-push)
    (:id mf-filter :cmd context-navigator-multifile-toggle-filter
         :keys ("f") :contexts (multifile) :section tools :desc-key :mf-filter-hint)
    (:id mf-edit-all :cmd context-navigator-multifile-edit-all
         :keys ("E") :contexts (multifile) :section tools :desc-key :mf-edit-all-hint)
    (:id mf-collapse :cmd context-navigator-multifile-toggle-collapse-all
         :keys ("z") :contexts (multifile) :section tools :desc-key :mf-collapse-hint)
    (:id mf-help :cmd context-navigator-multifile-help
         :keys ("?") :contexts (multifile) :section tools :desc-key :help-help)
    (:id mf-close :cmd context-navigator-multifile-close
         :keys ("q") :contexts (multifile) :section tools :desc-key :help-quit)
    )
  "Centralized keyspec for Context Navigator (middle path)."
  :type '(repeat plist)
  :group 'context-navigator-keys)

(defun context-navigator-keys--entries-for (context)
  "Return keyspec entries that include CONTEXT in :contexts."
  (cl-remove-if-not
   (lambda (pl) (memq context (plist-get pl :contexts)))
   context-navigator-keyspec))

;;;###autoload
(defun context-navigator-keys-apply-to (map context)
  "Apply keys for CONTEXT from keyspec to keymap MAP."
  (when (keymapp map)
    (dolist (pl (context-navigator-keys--entries-for context))
      (let* ((cmd  (plist-get pl :cmd))
             (keys (ignore-errors (context-navigator-keys--effective-keys pl context))))
        (when (and (symbolp cmd) (fboundp cmd) (listp keys))
          (dolist (k keys)
            (ignore-errors (define-key map (kbd k) cmd))))))))

(defun context-navigator-keys--desc (pl)
  "Return human label for PL via :desc-key and i18n."
  (let* ((k (plist-get pl :desc-key)))
    (cond
     ((and k (fboundp 'context-navigator-i18n)) (context-navigator-i18n k))
     (t (symbol-name (or (plist-get pl :id) (plist-get pl :cmd)))))))

;;;###autoload
(defun context-navigator-keys-help (context)
  "Return grouped help data for CONTEXT as an alist:
((section . ((key . desc) ...)) ...)."
  (let* ((entries (context-navigator-keys--entries-for context))
         (by-section (make-hash-table :test 'eq)))
    (dolist (pl entries)
      (let* ((sec (or (plist-get pl :section) 'act))
             (desc (context-navigator-keys--desc pl))
             (keys (ignore-errors (context-navigator-keys--effective-keys pl context))))
        (dolist (k (or keys '()))
          (let* ((lst (gethash sec by-section)))
            (push (cons k desc) lst)
            (puthash sec lst by-section)))))
    (let (res)
      (maphash (lambda (sec lst)
                 (push (cons sec (nreverse (delete-dups lst))) res))
               by-section)
      (nreverse res))))

;;;###autoload
(defun context-navigator-keys-keys-for (id context)
  "Return list of key strings for action ID in CONTEXT (effective, with profile overlays).
Falls back to broader contexts when exact CONTEXT not found:
- items<->groups share many actions; try each other when missing
- global as last resort."
  (let* ((entries context-navigator-keyspec)
         (ctx-chain
          (cond
           ((eq context 'items)     '(items groups global))
           ((eq context 'groups)    '(groups items global))
           ((eq context 'multifile) '(multifile global))
           (t                       (list context 'global)))))
    (catch 'hit
      (dolist (ctx ctx-chain)
        (dolist (pl entries)
          (when (and (eq (plist-get pl :id) id)
                     (memq ctx (plist-get pl :contexts)))
            (let ((ks (ignore-errors (context-navigator-keys--effective-keys pl ctx))))
              (when (and ks (listp ks) (> (length ks) 0))
                (throw 'hit (copy-sequence ks)))))))
      nil)))

;;;###autoload
(defun context-navigator-keys-first-key (id context)
  "Return the first key string for action ID in CONTEXT, or nil."
  (let ((lst (context-navigator-keys-keys-for id context)))
    (car-safe lst)))

(defun context-navigator-keys--collision-alist (context)
  "Return alist of (KEY . IDS) for duplicate keys in CONTEXT."
  (let* ((entries (context-navigator-keys--entries-for context))
         (acc (make-hash-table :test 'equal)))
    (dolist (pl entries)
      (let* ((id   (plist-get pl :id))
             (keys (or (plist-get pl :keys) '())))
        (dolist (k keys)
          (puthash k (cons id (gethash k acc)) acc))))
    (let (res)
      (maphash (lambda (k ids)
                 (when (> (length ids) 1)
                   (push (cons k (nreverse ids)) res)))
               acc)
      (nreverse res))))

;;;###autoload
(defun context-navigator-keys-lint-collisions (&optional context)
  "Interactive lint: show collisions for CONTEXT (default items)."
  (interactive)
  (let* ((ctx (or context 'items))
         (cols (context-navigator-keys--collision-alist ctx)))
    (if (null cols)
        (message "[keyspec] No collisions in %s" ctx)
      (with-current-buffer (get-buffer-create "*CN Key Lint*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Collisions in %s:\n\n" ctx))
          (dolist (cell cols)
            (insert (format "  %s -> %s\n" (car cell) (mapconcat #'symbol-name (cdr cell) ", "))))
          (goto-char (point-min))
          (view-mode 1))
        (display-buffer "*CN Key Lint*")))))

;;;###autoload
(defun context-navigator-keys-apply-known-keymaps ()
  "Apply keyspec to known mode maps (idempotent, safe).
- context-navigator-view-mode-map         ← items + global
- context-navigator-multifile-mode-map    ← multifile + global
- context-navigator-groups-split-mode-map ← groups-split + global"
  (interactive)
  ;; View (only global/items; 'groups now lives in the split)
  (when (and (boundp 'context-navigator-view-mode-map)
             (keymapp context-navigator-view-mode-map))
    (context-navigator-keys-apply-to context-navigator-view-mode-map 'global)
    (context-navigator-keys-apply-to context-navigator-view-mode-map 'items))
  ;; Multifile
  (when (and (boundp 'context-navigator-multifile-mode-map)
             (keymapp context-navigator-multifile-mode-map))
    (context-navigator-keys-apply-to context-navigator-multifile-mode-map 'multifile)
    (context-navigator-keys-apply-to context-navigator-multifile-mode-map 'global))
  ;; Groups split (bottom panel)
  (when (and (boundp 'context-navigator-groups-split-mode-map)
             (keymapp context-navigator-groups-split-mode-map))
    ;; Apply global first so local context can override collisions
    (context-navigator-keys-apply-to context-navigator-groups-split-mode-map 'global)
    (context-navigator-keys-apply-to context-navigator-groups-split-mode-map 'groups-split))
  t)

;; Auto-reapply on spec changes (when available)
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher
   'context-navigator-keyspec
   (lambda (_sym _val _op _where)
     (ignore-errors (context-navigator-keys-apply-known-keymaps)))))

;; --- Profiles/overlays (middle path) ----------------------------------------

(defcustom context-navigator-keys-profile 'classic
  "Active key profile overlay: 'classic | 'vimish | 'custom.
Profile may add/remove alias keys per action/context without changing the base spec."
  :type '(choice (const classic) (const vimish) (const custom))
  :group 'context-navigator-keys)

(defcustom context-navigator-keys-profile-overlays
  '((classic
     ;; Classic already covered in base spec (n/p arrows); keep empty.
     )
    (vimish
     ;; Ensure vim-like aliases present (most already in base spec).
     (:id next     :contexts (items groups) :add ("j"))
     (:id prev     :contexts (items groups) :add ("k"))
     (:id groups-split  :contexts (items groups) :add ("h"))
     (:id activate :contexts (items groups) :add ("l"))))
  "Overlay edits per profile: a list keyed by profile symbol of plist edits:
(:id ID :contexts (ctx...) :add (keys...) [:remove (keys...)])"
  :type '(alist :key-type symbol :value-type (repeat plist))
  :group 'context-navigator-keys)

(defun context-navigator-keys--overlay-edits (id context)
  "Return overlay edits plist for action ID in CONTEXT for current profile."
  (let* ((prof (or context-navigator-keys-profile 'classic))
         (lst (cdr (assoc prof context-navigator-keys-profile-overlays))))
    (cl-find-if
     (lambda (pl)
       (and (eq (plist-get pl :id) id)
            (memq context (plist-get pl :contexts))))
     lst)))

(defun context-navigator-keys--effective-keys (pl context)
  "Return effective keys for keyspec entry PL under CONTEXT with profile overlays."
  (let* ((base (copy-sequence (or (plist-get pl :keys) '())))
         (id   (plist-get pl :id))
         (ed   (and id (context-navigator-keys--overlay-edits id context)))
         (adds (and ed (plist-get ed :add)))
         (rems (and ed (plist-get ed :remove)))
         (cur  base))
    ;; remove requested
    (when (listp rems)
      (setq cur (cl-remove-if (lambda (k) (member k rems)) cur)))
    ;; add requested (preserve order, avoid duplicates)
    (when (listp adds)
      (dolist (k adds)
        (unless (member k cur) (setq cur (append cur (list k))))))
    cur))

;; Re-apply keymaps and which-key when profile changes
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher
   'context-navigator-keys-profile
   (lambda (&rest _)
     (ignore-errors (context-navigator-keys-apply-known-keymaps))
     (when (fboundp 'context-navigator-which-key-apply!)
       (ignore-errors (context-navigator-which-key-apply!))))))

(provide 'context-navigator-keyspec)
;;; context-navigator-keyspec.el ends here
