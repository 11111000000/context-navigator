;;; context-navigator-groups.el --- Groups CRUD and helpers -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Groups listing/open and CRUD operations extracted from core to reduce coupling.
;; This module encapsulates:
;; - list/open groups for current root
;; - switch group (async load)
;; - create/rename/delete/duplicate
;; - edit description
;; - small helpers for reading/writing state.el and sorting groups
;;
;; Dependencies are confined to persist/events/log/project; we only declare
;; minimal Core functions to avoid require-cycle.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-persist)
(require 'context-navigator-events)
(require 'context-navigator-log)
(require 'context-navigator-project)

;; Minimal Core declarations (do not require core to avoid cycles)
(declare-function context-navigator--state-get "context-navigator-core" ())
(declare-function context-navigator--state-copy "context-navigator-core" (state))
(declare-function context-navigator--set-state "context-navigator-core" (state))
(declare-function context-navigator-state-p "context-navigator-core" (state))
(declare-function context-navigator-state-last-project-root "context-navigator-core" (state))
(declare-function context-navigator-state-current-group-slug "context-navigator-core" (state))
(declare-function context-navigator-persist-save "context-navigator-persist" (items root slug))
(declare-function context-navigator-persist-load-group-async "context-navigator-persist" (root slug callback))
(declare-function context-navigator-set-items "context-navigator-core" (items))
(declare-function context-navigator--load-group-for-root "context-navigator-core" (root slug))

(defun context-navigator--current-root ()
  "Return current root from state (or nil for global)."
  (let* ((st (ignore-errors (context-navigator--state-get))))
    (and st (context-navigator-state-last-project-root st))))

(defun context-navigator--groups-sortless (a b)
  "Case-insensitive sort predicate by :display for group plists A and B."
  (let ((da (downcase (or (plist-get a :display) (plist-get a :slug) "")))
        (db (downcase (or (plist-get b :display) (plist-get b :slug) ""))))
    (string-lessp da db)))

(defun context-navigator--groups-candidates (root)
  "Return alist (DISPLAY . SLUG) for completing-read."
  (let* ((groups (ignore-errors (context-navigator-persist-list-groups root))))
    (mapcar (lambda (pl)
              (cons (or (plist-get pl :display) (plist-get pl :slug))
                    (plist-get pl :slug)))
            (sort groups #'context-navigator--groups-sortless))))

(defun context-navigator--state-read (root)
  "Read state plist for ROOT (or global), ensure :version present."
  (let* ((st (or (ignore-errors (context-navigator-persist-state-load root)) '())))
    (if (plist-member st :version) st
      (plist-put (copy-sequence st) :version 1))))

(defun context-navigator--state-write (root st)
  "Write state ST for ROOT (or global)."
  (ignore-errors (context-navigator-persist-state-save root st)))

;;;###autoload
(defun context-navigator-groups-open ()
  "Publish groups list for current project/global and let sidebar render it."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (groups (ignore-errors (context-navigator-persist-list-groups root))))
    ;; Auto-initialize default group if nothing exists yet.
    (when (and (boundp 'context-navigator-create-default-group-file)
               context-navigator-create-default-group-file
               (or (null groups) (= (length groups) 0)))
      (let* ((default-slug "default"))
        (ignore-errors (context-navigator-persist-save '() root default-slug))
        (let* ((st (context-navigator--state-read root))
               (alist (and (plist-member st :groups) (plist-get st :groups)))
               (alist* (cons (cons default-slug "default")
                             (and (listp alist)
                                  (cl-remove-if (lambda (cell) (equal (car-safe cell) default-slug)) alist)))))
          (setq st (plist-put (copy-sequence st) :current default-slug))
          (when (plist-member st :groups)
            (setq st (plist-put (copy-sequence st) :groups alist*)))
          (context-navigator--state-write root st))))
    (setq groups (ignore-errors (context-navigator-persist-list-groups root)))
    (setq groups (sort (or groups '()) #'context-navigator--groups-sortless))
    (context-navigator-events-publish :groups-list-updated root groups)
    groups))

;;;###autoload
(defun context-navigator-group-switch (&optional slug)
  "Switch active group to SLUG for current project/global. Prompts when SLUG is nil."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (cand (context-navigator--groups-candidates root))
         (slug (or slug
                   (cdr (assoc (completing-read "Switch to group: " cand nil t) cand)))))
    (let* ((st (ignore-errors (context-navigator--state-get))))
      (when (and st (stringp (context-navigator-state-current-group-slug st)))
        (let* ((items (and st (ignore-errors (context-navigator-state-items st)))))
          (ignore-errors (context-navigator-persist-save items root (context-navigator-state-current-group-slug st))))))
    (ignore-errors (context-navigator--load-group-for-root root (or slug "default")))))

(defun context-navigator--assert-unique-slug (root slug)
  "Signal error if SLUG already exists for ROOT."
  (let* ((groups (ignore-errors (context-navigator-persist-list-groups root))))
    (when (cl-find slug groups :key (lambda (pl) (plist-get pl :slug)) :test #'equal)
      (error "Group '%s' already exists" slug))))

;;;###autoload
(defun context-navigator-group-create (&optional display-name)
  "Create a new group with DISPLAY-NAME in current project/global."
  (interactive)
  (let* ((root (or (context-navigator--current-root)
                   (ignore-errors (context-navigator-project-current-root (current-buffer)))))
         (name (or display-name (read-string "New group name: ")))
         (name (string-trim name)))
    (when (string-empty-p name)
      (user-error "Invalid name"))
    (ignore-errors (context-navigator-events-cancel :autosave))
    (let* ((slug (context-navigator-persist-slugify name))
           (file (context-navigator-persist-context-file root slug)))
      (context-navigator--assert-unique-slug root slug)
      (make-directory (file-name-directory file) t)
      (ignore-errors (context-navigator-persist-save '() root slug))
      (ignore-errors (context-navigator--load-group-for-root root slug))
      (message (context-navigator-i18n :group-created) name slug)
      slug)))

;;;###autoload
(defun context-navigator-group-rename (&optional old-slug new-display)
  "Rename an existing group OLD-SLUG to NEW-DISPLAY (re-slugified)."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (cand (context-navigator--groups-candidates root))
         (old-slug (or old-slug
                       (cdr (assoc (completing-read "Rename group: " cand nil t) cand))))
         (_ (when (equal old-slug "default") (user-error "Cannot rename 'default'")))
         (new-display (or new-display (read-string (format (context-navigator-i18n :group-rename-prompt) old-slug))))
         (new-display (string-trim new-display))
         (_ (when (string-empty-p new-display)
              (user-error "Invalid name")))
         (new-slug (context-navigator-persist-slugify new-display)))
    (unless (equal old-slug new-slug)
      (context-navigator--assert-unique-slug root new-slug))
    (let* ((old-file (context-navigator-persist-context-file root old-slug))
           (new-file (context-navigator-persist-context-file root new-slug)))
      (when (and (file-readable-p old-file) (not (equal old-file new-file)))
        (make-directory (file-name-directory new-file) t)
        (rename-file old-file new-file t)))
    (let* ((st (context-navigator--state-read root)))
      (when (equal (plist-get st :current) old-slug)
        (setq st (plist-put (copy-sequence st) :current new-slug))
        (context-navigator--state-write root st)))
    (let* ((cur (ignore-errors (context-navigator--state-get)))
           (cur* (and cur (context-navigator--state-copy cur))))
      (when (and cur* (equal (context-navigator-state-current-group-slug cur*) old-slug))
        (setf (context-navigator-state-current-group-slug cur*) new-slug))
      (when cur* (context-navigator--set-state cur*)))
    (context-navigator-groups-open)
    (message (context-navigator-i18n :group-renamed) old-slug new-display new-slug)
    new-slug))

;;;###autoload
(defun context-navigator-group-delete (&optional slug no-confirm)
  "Delete group SLUG; if active or referenced, switch :current to \"default\".
When NO-CONFIRM is non-nil or in batch mode, do not prompt."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (cand (context-navigator--groups-candidates root))
         (slug (or slug
                   (cdr (assoc (completing-read "Delete group: " cand nil t) cand)))))
    (when (equal slug "default")
      (user-error "Cannot delete 'default'"))
    (let ((need-confirm (and (not noninteractive) (not no-confirm))))
      (when (or (not need-confirm)
                (yes-or-no-p (format (context-navigator-i18n :group-delete-confirm) slug)))
        (let ((file (context-navigator-persist-context-file root slug)))
          (when (file-exists-p file)
            (ignore-errors (delete-file file))))
        (let* ((st (context-navigator--state-read root))
               (deleted-active (equal (plist-get st :current) slug))
               (groups (and (plist-member st :groups) (plist-get st :groups)))
               (groups* (and (listp groups)
                             (cl-remove-if (lambda (cell) (equal (car-safe cell) slug))
                                           groups))))
          (when (plist-member st :groups)
            (setq st (plist-put (copy-sequence st) :groups groups*)))
          (when (equal (plist-get st :current) slug)
            (setq st (plist-put (copy-sequence st) :current "default")))
          (when (and (equal (plist-get st :current) "default")
                     (boundp 'context-navigator-create-default-group-file)
                     context-navigator-create-default-group-file)
            (let ((df (ignore-errors (context-navigator-persist-context-file root "default"))))
              (when (and (stringp df) (not (file-exists-p df)))
                (ignore-errors (context-navigator-persist-save '() root "default")))))
          (context-navigator--state-write root st)
          (when deleted-active
            (ignore-errors (context-navigator--load-group-for-root root "default")))))))
  (context-navigator-groups-open)
  (message (context-navigator-i18n :group-deleted) slug)
  t)

;;;###autoload
(defun context-navigator-group-duplicate (&optional src-slug new-display)
  "Duplicate SRC-SLUG into a new group named NEW-DISPLAY (slugified)."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (cand (context-navigator--groups-candidates root))
         (src (or src-slug
                  (cdr (assoc (completing-read "Duplicate group: " cand nil t) cand))))
         (new-display (or new-display (read-string (format (context-navigator-i18n :group-duplicate-prompt) src))))
         (new-display (string-trim new-display))
         (_ (when (string-empty-p new-display)
              (user-error "Invalid name")))
         (dst (context-navigator-persist-slugify new-display)))
    (context-navigator--assert-unique-slug root dst)
    (let* ((src-file (context-navigator-persist-context-file root src))
           (dst-file (context-navigator-persist-context-file root dst)))
      (make-directory (file-name-directory dst-file) t)
      (if (file-readable-p src-file)
          (copy-file src-file dst-file t)
        (ignore-errors (context-navigator-persist-save '() root dst))))
    (context-navigator-groups-open)
    (message (context-navigator-i18n :group-duplicated) src new-display dst)
    dst))

;;;###autoload
(defun context-navigator-group-edit-description (&optional slug new-desc)
  "Add or edit description for group SLUG in current project/global."
  (interactive)
  (let* ((root (context-navigator--current-root))
         (cand (ignore-errors (context-navigator--groups-candidates root)))
         (cur-st (ignore-errors (context-navigator--state-get)))
         (cur (and cur-st (context-navigator-state-current-group-slug cur-st)))
         (slug (or slug
                   (cdr (assoc (completing-read
                                (if cur
                                    (format (context-navigator-i18n :group-edit-desc-default) cur)
                                  "Edit description for group: ")
                                cand nil t nil nil
                                (car (rassoc cur cand)))
                               cand))))
         (_ (when (or (null slug) (string-empty-p slug))
              (user-error "No group selected")))
         (st (context-navigator--state-read root))
         (alist (and (plist-member st :descriptions) (plist-get st :descriptions)))
         (old (and (listp alist) (cdr (assoc slug alist))))
         (input (or new-desc
                    (read-string (format (context-navigator-i18n :group-edit-desc-for) slug) old)))
         (desc (string-trim input))
         (alist* (let ((clean (and (listp alist)
                                   (cl-remove-if (lambda (cell) (equal (car-safe cell) slug)) alist))))
                   (if (string-empty-p desc)
                       clean
                     (cons (cons slug desc) clean)))))
    (setq st (plist-put (copy-sequence st) :descriptions alist*))
    (context-navigator--state-write root st)
    (context-navigator-groups-open)
    (force-mode-line-update t)
    (if (string-empty-p desc)
        (message (context-navigator-i18n :group-desc-cleared) slug)
      (message (context-navigator-i18n :group-desc-updated) slug))
    desc))

(provide 'context-navigator-groups)
;;; context-navigator-groups.el ends here
