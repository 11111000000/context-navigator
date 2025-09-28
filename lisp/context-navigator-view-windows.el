;;; context-navigator-view-windows.el --- Window balance protections for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Keep side-window (sidebar) safe from global window-balancing operations.
;; Installs lightweight advices around balance-windows / balance-windows-area
;; while the Navigator view is in use.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-core)

(defvar context-navigator-view-windows--adv-installed nil
  "Non-nil when advices protecting Navigator windows are installed.")

(defun context-navigator-view-windows--sidebar-visible-p ()
  "Return non-nil when a window marked as Navigator sidebar is visible on the current frame."
  (let ((buf (and (boundp 'context-navigator-view--buffer-name)
                  (get-buffer context-navigator-view--buffer-name))))
    (catch 'hit
      (dolist (w (window-list nil nil))
        (when (and (window-live-p w)
                   (eq (window-buffer w) buf)
                   (eq (window-parameter w 'context-navigator-view) 'sidebar))
          (throw 'hit t)))
      nil)))

(defun context-navigator-view-windows--buffer-mode-visible-p ()
  "Return non-nil when Navigator buffer-mode window is visible on the current frame."
  (let ((buf (and (boundp 'context-navigator-view--buffer-name)
                  (get-buffer context-navigator-view--buffer-name))))
    (catch 'hit
      (dolist (w (window-list nil nil))
        (when (and (window-live-p w)
                   (eq (window-buffer w) buf)
                   (eq (window-parameter w 'context-navigator-view) 'buffer))
          (throw 'hit t)))
      nil)))

(defun context-navigator-view-windows--protect-balance (orig-fn &rest args)
  "Advice wrapper that no-ops when Navigator windows must be protected."
  (let* ((protect-sidebar (and (boundp 'context-navigator-protect-sidebar-windows)
                               context-navigator-protect-sidebar-windows))
         (protect-buffer  (and (boundp 'context-navigator-protect-buffer-windows)
                               context-navigator-protect-buffer-windows))
         (sidebar (and protect-sidebar (context-navigator-view-windows--sidebar-visible-p)))
         (bufferw (and protect-buffer (context-navigator-view-windows--buffer-mode-visible-p))))
    (if (or sidebar bufferw)
        (progn
          (ignore-errors
            (when (fboundp 'context-navigator-debug)
              (context-navigator-debug :debug :ui "Skipping window balance (Navigator visible)")))
          nil)
      (apply orig-fn args))))

(defun context-navigator-view-windows-setup ()
  "Install advices protecting Navigator windows (idempotent)."
  (unless context-navigator-view-windows--adv-installed
    (when (fboundp 'advice-add)
      (ignore-errors
        (advice-add 'balance-windows :around #'context-navigator-view-windows--protect-balance))
      (ignore-errors
        (advice-add 'balance-windows-area :around #'context-navigator-view-windows--protect-balance)))
    (setq context-navigator-view-windows--adv-installed t))
  t)

(defun context-navigator-view-windows-teardown ()
  "Remove previously installed advices (idempotent)."
  (when context-navigator-view-windows--adv-installed
    (ignore-errors
      (advice-remove 'balance-windows #'context-navigator-view-windows--protect-balance))
    (ignore-errors
      (advice-remove 'balance-windows-area #'context-navigator-view-windows--protect-balance))
    (setq context-navigator-view-windows--adv-installed nil))
  t)

(provide 'context-navigator-view-windows)
;;; context-navigator-view-windows.el ends here
