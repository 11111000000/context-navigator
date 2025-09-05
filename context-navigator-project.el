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
  "Return absolute project root for BUFFER (or current) or nil.

This attempts several strategies for robustness:
- Prefer `project-current' / `project-root' in the buffer itself.
- Fallback: if that fails, try detecting the project using the buffer's
  `default-directory' (useful for special buffers like gptel that are not
  file-backed but inherit a directory from their originating project).
- Finally, try projectile fallbacks with the same approaches."
  (let* ((buf (or buffer (current-buffer)))
         ;; buffer's default-directory may be the best hint for non-file buffers
         (buf-dir (when (bufferp buf) (with-current-buffer buf default-directory)))
         (root
          (or
           ;; 1) Prefer project.el detection in the buffer context
           (when (fboundp 'project-current)
             (let ((proj (with-current-buffer buf (project-current nil))))
               (when proj (expand-file-name (project-root proj)))))
           ;; 2) Fallback: try project.el by treating buf-dir as default-directory
           (when (and (fboundp 'project-current) buf-dir)
             (let ((default-directory buf-dir))
               (when-let ((proj (project-current nil)))
                 (expand-file-name (project-root proj)))))
           ;; 3) projectile in buffer context (if available)
           (when (fboundp 'projectile-project-root)
             (with-current-buffer buf
               (ignore-errors (projectile-project-root))))
           ;; 4) projectile fallback using buf-dir
           (when (and (fboundp 'projectile-project-root) buf-dir)
             (let ((default-directory buf-dir))
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
     ;; Trigger on file-backed buffers, gptel buffers (or modes derived from it),
     ;; and also Dired buffers.
     (or buffer-file-name
         (derived-mode-p 'gptel-mode)
         (derived-mode-p 'dired-mode)))))

(defun context-navigator-project--maybe-publish-switch (&optional buffer)
  "Publish :project-switch only when the project root actually changes.

Throttled by `context-navigator-context-switch-interval' to avoid publishing
a flurry of events during rapid buffer/window changes. When a change is
detected but the last publish was too recent, schedule a debounced publish
for the remaining interval."
  (let* ((buf (or buffer (current-buffer))))
    (when (context-navigator-project--interesting-buffer-p buf)
      (let* ((root (context-navigator-project-current-root buf)))
        (unless (equal root context-navigator-project--last-root)
          (let* ((now (context-navigator-project--now))
                 (elapsed (and (numberp context-navigator-project--last-switch-time)
                               (- now context-navigator-project--last-switch-time)))
                 (interval (if (and (boundp 'context-navigator-context-switch-interval)
                                    (numberp context-navigator-context-switch-interval))
                               context-navigator-context-switch-interval
                             0.0)))
            (if (or (not (numberp elapsed)) (>= elapsed interval))
                (progn
                  (setq context-navigator-project--last-root root
                        context-navigator-project--last-switch-time now)
                  (context-navigator-events-publish :project-switch root)
                  root)
              ;; Too soon: debounce a publish for the remaining time.
              (let ((delay (max 0.0 (- interval (or elapsed 0.0)))))
                (context-navigator-events-debounce
                 :project-switch delay
                 (lambda ()
                   (setq context-navigator-project--last-root root
                         context-navigator-project--last-switch-time (context-navigator-project--now))
                   (context-navigator-events-publish :project-switch root)))))
            nil))))))

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
