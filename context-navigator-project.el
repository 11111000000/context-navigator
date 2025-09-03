;;; context-navigator-project.el --- Project detection and switch events -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Detect project roots using project.el or projectile (soft deps)
;; and publish :project-switch events on meaningful changes.
;; Filters out uninteresting buffers (e.g., Dired).
;;
;; Side-effects are limited to hook setup/teardown and event publishing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-events)

(defvar context-navigator-project--hooks-installed nil)
(defvar context-navigator-project--last-root nil)
(defvar context-navigator-project--last-switch-time 0.0)

(defun context-navigator-project--now () (float-time))

(defun context-navigator-project-current-root (&optional buffer)
  "Return absolute project root for BUFFER (or current) or nil."
  (let* ((buf (or buffer (current-buffer)))
         (root
          (or
           (when (fboundp 'project-current)
             (let ((proj (with-current-buffer buf (project-current nil))))
               (when proj (expand-file-name (project-root proj)))))
           (when (fboundp 'projectile-project-root)
             (with-current-buffer buf
               (ignore-errors (projectile-project-root)))))))
    (when (and (stringp root) (not (string-empty-p root)))
      (directory-file-name (expand-file-name root)))))

(defun context-navigator-project--interesting-buffer-p (buffer)
  "Return non-nil if BUFFER should trigger project switching.

Previously Dired buffers were excluded; include them so that entering a
Dired buffer inside a project also triggers project context loading when
auto-project switching is enabled."
  (with-current-buffer buffer
    (and
     ;; Trigger on file-backed buffers, gptel buffers, and also Dired buffers.
     (or buffer-file-name
         (eq major-mode 'gptel-mode)
         (derived-mode-p 'dired-mode)))))

(defun context-navigator-project--maybe-publish-switch (&optional buffer)
  "Publish :project-switch only when the project root actually changes."
  (let* ((buf (or buffer (current-buffer))))
    (when (context-navigator-project--interesting-buffer-p buf)
      (let* ((root (context-navigator-project-current-root buf)))
        (unless (equal root context-navigator-project--last-root)
          (setq context-navigator-project--last-root root
                context-navigator-project--last-switch-time (context-navigator-project--now))
          (context-navigator-events-publish :project-switch root)
          root)))))

(defun context-navigator-project--on-buffer-list-update ()
  (context-navigator-project--maybe-publish-switch (current-buffer)))

(defun context-navigator-project--on-window-selection-change (_frame)
  (context-navigator-project--maybe-publish-switch (window-buffer (selected-window))))

(defun context-navigator-project-setup-hooks ()
  "Install lightweight hooks to track project changes."
  (unless context-navigator-project--hooks-installed
    (add-hook 'buffer-list-update-hook #'context-navigator-project--on-buffer-list-update)
    (add-hook 'window-selection-change-functions #'context-navigator-project--on-window-selection-change)
    (setq context-navigator-project--hooks-installed t))
  t)

(defun context-navigator-project-teardown-hooks ()
  "Remove previously installed hooks."
  (when context-navigator-project--hooks-installed
    (remove-hook 'buffer-list-update-hook #'context-navigator-project--on-buffer-list-update)
    (remove-hook 'window-selection-change-functions #'context-navigator-project--on-window-selection-change)
    (setq context-navigator-project--hooks-installed nil))
  t)

(provide 'context-navigator-project)
;;; context-navigator-project.el ends here
