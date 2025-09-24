;;; context-navigator-view-spinner.el --- Spinner helpers for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Lightweight spinner implementation used by the sidebar during async loads.
;; This module hosts the actual timer logic; the view calls wrappers with
;; legacy names that delegate here.

;;; Code:

(require 'subr-x)

;; Buffer-local state variables are declared in the view module; declare here
;; for the byte-compiler’s sake (do not change bindings).
(defvar context-navigator-view--spinner-timer nil)
(defvar context-navigator-view--spinner-index 0)
(defvar context-navigator-view--spinner-last-time 0.0)
(defvar context-navigator-view--spinner-degraded nil)
(defvar context-navigator-view--buffer-name "*context-navigator*")

;; Settings and a render trigger are provided by the view module.
(defvar context-navigator-view-spinner-interval 0.1)
(defvar context-navigator-view-spinner-degrade-threshold 0.25)
(declare-function context-navigator-view--schedule-render "context-navigator-view" ())

(defun context-navigator-view-spinner-start ()
  "Start or restart the sidebar loading spinner timer.
Degrade to a static indicator if timer slippage exceeds a threshold."
  (interactive)
  (when (timerp context-navigator-view--spinner-timer)
    (cancel-timer context-navigator-view--spinner-timer))
  (setq context-navigator-view--spinner-index 0)
  (setq context-navigator-view--spinner-last-time (float-time))
  (setq context-navigator-view--spinner-degraded nil)
  (let* ((interval (or context-navigator-view-spinner-interval 0.1))
         (buf (current-buffer)))
    (setq context-navigator-view--spinner-timer
          (run-at-time 0 interval
                       (lambda ()
                         (let ((bb (if (buffer-live-p buf) buf
                                     (get-buffer context-navigator-view--buffer-name))))
                           (when (buffer-live-p bb)
                             (with-current-buffer bb
                               (let* ((now (float-time))
                                      (dt (- now (or context-navigator-view--spinner-last-time now)))
                                      (thr (or context-navigator-view-spinner-degrade-threshold 0.25)))
                                 (setq context-navigator-view--spinner-last-time now)
                                 (when (> dt (+ interval thr))
                                   ;; Timer slipped; stop animation and render a static indicator.
                                   (setq context-navigator-view--spinner-degraded t)
                                   (when (timerp context-navigator-view--spinner-timer)
                                     (cancel-timer context-navigator-view--spinner-timer))
                                   (setq context-navigator-view--spinner-timer nil))
                                 (when (not context-navigator-view--spinner-degraded)
                                   (setq context-navigator-view--spinner-index
                                         (1+ (or context-navigator-view--spinner-index 0))))
                                 (context-navigator-view--schedule-render))))))))))

(defun context-navigator-view-spinner-stop ()
  "Stop the sidebar loading spinner timer and reset its state."
  (interactive)
  (when (timerp context-navigator-view--spinner-timer)
    (cancel-timer context-navigator-view--spinner-timer))
  (setq context-navigator-view--spinner-timer nil)
  (setq context-navigator-view--spinner-index 0)
  (setq context-navigator-view--spinner-last-time 0.0)
  (setq context-navigator-view--spinner-degraded nil))

(provide 'context-navigator-view-spinner)
;;; context-navigator-view-spinner.el ends here
