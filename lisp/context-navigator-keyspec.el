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
         :keys ("j" "n" "<down>") :contexts (items groups) :section navigate :desc-key :help-next-item)
    (:id prev :cmd context-navigator-view-previous-item
         :keys ("k" "p" "<up>") :contexts (items groups) :section navigate :desc-key :help-previous-item)
    (:id activate :cmd context-navigator-view-activate
         :keys ("l" "RET" "<return>" "<kp-enter>") :contexts (items groups) :section navigate :desc-key :help-activate)
    (:id preview :cmd context-navigator-view-preview
         :keys ("v") :contexts (items) :section navigate :desc-key :help-preview)
    (:id tab-next :cmd context-navigator-view-tab-next
         :keys ("TAB" "<tab>" "C-i") :contexts (items groups) :section navigate :desc-key :help-activate)
    (:id tab-prev :cmd context-navigator-view-tab-previous
         :keys ("<backtab>" "S-<tab>") :contexts (items groups) :section navigate :desc-key :help-activate)
    (:id go-up :cmd context-navigator-view-go-up
         :keys ("h") :contexts (items groups) :section navigate :desc-key :help-go-up)

    ;; Items actions
    (:id toggle-enabled :cmd context-navigator-view-toggle-enabled
         :keys ("SPC" "t" "m") :contexts (items) :section act :desc-key :help-toggle-gptel)
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
         :keys ("s") :contexts (items) :section act :desc-key :stats)

    ;; Session (items/groups)
    (:id push-toggle :cmd context-navigator-view-toggle-push
         :keys ("V") :contexts (items groups) :section session :desc-key :help-toggle-push)
    (:id auto-toggle :cmd context-navigator-view-toggle-auto-project
         :keys ("A") :contexts (items groups) :section session :desc-key :help-toggle-auto)
    (:id push-now :cmd context-navigator-view-push-now
         :keys ("p") :contexts (items groups) :section session :desc-key :help-push-now)

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
         :keys ("SPC" "t" "m") :contexts (groups) :section groups :desc-key :toggle-multi-group)
    (:id multigroup-toggle :cmd context-navigator-view-toggle-multi-group
         :keys ("G") :contexts (groups) :section groups :desc-key :toggle-multi-group)

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
         :keys ("p") :contexts (multifile) :section act :desc-key :mf-action-push)
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
      (let ((cmd  (plist-get pl :cmd))
            (keys (or (plist-get pl :keys) '())))
        (when (and (symbolp cmd) (fboundp cmd))
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
             (desc (context-navigator-keys--desc pl)))
        (dolist (k (or (plist-get pl :keys) '()))
          (let* ((lst (gethash sec by-section)))
            (push (cons k desc) lst)
            (puthash sec lst by-section)))))
    (let (res)
      (maphash (lambda (sec lst)
                 (push (cons sec (nreverse (delete-dups lst))) res))
               by-section)
      (nreverse res))))

(provide 'context-navigator-keyspec)
;;; context-navigator-keyspec.el ends here
