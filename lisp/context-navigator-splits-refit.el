;;; context-navigator-splits-refit.el --- Robust refit for Navigator splits -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Ensures that Groups/Stats splits are always fitted to their content and never
;; exceed half of the Navigator window height, especially when the second split
;; is opened. Works by debounced refit on window-configuration changes and by
;; advising open/close of splits.

;;; Code:

(require 'cl-lib)

;; Soft requires: these symbols may or may not be loaded at file load time.
;; Declare without initializing to avoid clobbering real defaults in split modules.
(defvar context-navigator-groups-split-buffer-name)
(defvar context-navigator-stats-split-buffer-name)

;; Predicted major modes (used to detect split buffers).
(defvar cnsr--groups-mode-sym 'context-navigator-groups-split-mode)
(defvar cnsr--stats-mode-sym  'context-navigator-stats-split-mode)
(defvar cnsr--nav-mode-sym    'context-navigator-view-mode)

(defvar cnsr--refit-pending nil)
(defgroup context-navigator-splits-refit nil
  "Robust refit for Navigator splits."
  :group 'context-navigator)

(defcustom context-navigator-splits-refit-enable nil
  "When non-nil, install global refit hooks/advices for Navigator splits.
Disabled by default to avoid extra hooks in performance-sensitive setups."
  :type 'boolean :group 'context-navigator-splits-refit)

(defcustom context-navigator-splits-refit-delay 0.03
  "Debounce delay (seconds) before refitting all splits after a layout change."
  :type 'number :group 'context-navigator-splits-refit)

(defun cnsr--nav-window ()
  "Return a live Navigator window if any."
  (cl-loop for w in (window-list nil 'no-minibuf)
           for buf = (and (window-live-p w) (window-buffer w))
           when (and buf
                     (buffer-live-p buf)
                     (with-current-buffer buf (eq major-mode cnsr--nav-mode-sym)))
           return w))

(defun cnsr--split-windows ()
  "Return list of visible windows that belong to Groups/Stats splits."
  (cl-loop for w in (window-list nil 'no-minibuf)
           for buf = (and (window-live-p w) (window-buffer w))
           when (and buf (buffer-live-p buf)
                     (with-current-buffer buf
                       (or (eq major-mode cnsr--groups-mode-sym)
                           (eq major-mode cnsr--stats-mode-sym))))
           collect w))

(defun cnsr--content-lines (win)
  "Return number of logical lines in WIN's buffer (min 1)."
  (with-current-buffer (window-buffer win)
    (max 1 (count-lines (point-min) (point-max)))))

(defun cnsr--half-of (w)
  "Return half of the total height of window W (min 1)."
  (max 1 (floor (/ (window-total-height w) 2))))

(defun cnsr--fit-one (win navw)
  "Fit WIN to its content, clamped by half-height of NAVW when available."
  (when (window-live-p win)
    (let* ((content (cnsr--content-lines win))
           (half    (if (and navw (window-live-p navw))
                        (cnsr--half-of navw)
                      most-positive-fixnum))
           (desired (min content half)))
      ;; Allow resizing first, then fit, then fix height and preserve it to stabilize.
      (set-window-parameter win 'window-size-fixed nil)
      (set-window-parameter win 'window-preserved-size nil)
      ;; Fit with explicit max (desired) and min 1 line.
      (ignore-errors (fit-window-to-buffer win desired 1))
      ;; Preserve the resulting height and fix it so later rebalancing won't inflate it.
      (set-window-parameter win 'window-preserved-size (cons t (window-total-height win)))
      (set-window-parameter win 'window-size-fixed 'height))))

(defun context-navigator-splits-refit-all ()
  "Refit all visible Navigator splits to their content."
  (interactive)
  (let* ((navw (cnsr--nav-window))
         (wins (cnsr--split-windows))
         ;; Let side-window combination resize cooperate with our per-window preserved sizes.
         (window-combination-resize t))
    (dolist (w wins) (cnsr--fit-one w navw))))

(defun cnsr--schedule-refit ()
  "Debounce scheduling of refit for all splits."
  (unless cnsr--refit-pending
    (setq cnsr--refit-pending t)
    (run-at-time context-navigator-splits-refit-delay nil
                 (lambda ()
                   (setq cnsr--refit-pending nil)
                   (ignore-errors
                     (context-navigator-splits-refit-all))
                   ;; One more pass after layout fully settles.
                   (run-at-time context-navigator-splits-refit-delay nil
                                (lambda ()
                                  (ignore-errors
                                    (context-navigator-splits-refit-all))))))))

(defun cnsr--wcch ()
  "Hook for window-configuration changes."
  ;; Only schedule when any split is visible to avoid unnecessary work.
  (when (cnsr--split-windows)
    (cnsr--schedule-refit)))

(when context-navigator-splits-refit-enable
  ;; Install the hook globally; it's cheap and debounced.
  (add-hook 'window-configuration-change-hook #'cnsr--wcch)

  ;; Add advice around split open/close to trigger refit immediately and after delay.
  (with-eval-after-load 'context-navigator-groups-split
    (ignore-errors
      ;; Ensure a sane default buffer name before opening, if the module hasn't set it yet.
      (defun cnsr--groups-ensure-buffer-name (&rest _)
        (unless (and (boundp 'context-navigator-groups-split-buffer-name)
                     (stringp context-navigator-groups-split-buffer-name))
          (setq context-navigator-groups-split-buffer-name "*Context Navigator: Groups*")))
      (advice-add 'context-navigator-groups-split-open  :before #'cnsr--groups-ensure-buffer-name)
      (advice-add 'context-navigator-groups-split-open  :after (lambda (&rest _) (cnsr--schedule-refit)))
      (advice-add 'context-navigator-groups-split-close :after (lambda (&rest _) (cnsr--schedule-refit)))))

  (with-eval-after-load 'context-navigator-stats-split
    (ignore-errors
      ;; Ensure a sane default buffer name before opening, if the module hasn't set it yet.
      (defun cnsr--stats-ensure-buffer-name (&rest _)
        (unless (and (boundp 'context-navigator-stats-split-buffer-name)
                     (stringp context-navigator-stats-split-buffer-name))
          (setq context-navigator-stats-split-buffer-name "*Context Navigator: Stats*")))
      (advice-add 'context-navigator-stats-split-open  :before #'cnsr--stats-ensure-buffer-name)
      (advice-add 'context-navigator-stats-split-open  :after (lambda (&rest _) (cnsr--schedule-refit)))
      (advice-add 'context-navigator-stats-split-close :after (lambda (&rest _) (cnsr--schedule-refit))))))

(provide 'context-navigator-splits-refit)
;;; context-navigator-splits-refit.el ends here
