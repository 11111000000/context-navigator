;;; context-navigator-events.el --- Event bus and debouncer -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Lightweight publish/subscribe and debouncing.
;; Side-effects are localized here; other layers remain pure.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar context-navigator--event-subscribers (make-hash-table :test 'eq)
  "Map topic symbol -> list of subscriber functions.")

(defvar context-navigator--debounce-timers nil
  "Alist of (KEY . TIMER) for debouncing.")

(defun context-navigator-events-reset ()
  "Reset subscribers and timers (for tests)."
  (clrhash context-navigator--event-subscribers)
  (dolist (cell context-navigator--debounce-timers)
    (when (timerp (cdr cell))
      (cancel-timer (cdr cell))))
  (setq context-navigator--debounce-timers nil))

(defun context-navigator-events-subscribe (topic fn)
  "Subscribe FN to TOPIC. Return token cons (TOPIC . FN)."
  (let* ((lst (gethash topic context-navigator--event-subscribers))
         (new (cons fn lst)))
    (puthash topic new context-navigator--event-subscribers)
    (cons topic fn)))

(defun context-navigator-events-unsubscribe (token)
  "Unsubscribe TOKEN previously returned by subscribe."
  (pcase-let ((`(,topic . ,fn) token))
    (let ((lst (gethash topic context-navigator--event-subscribers)))
      (when lst
        (puthash topic (delq fn lst) context-navigator--event-subscribers)))))

(defun context-navigator-events-publish (topic &rest args)
  "Publish ARGS to TOPIC, safely calling subscribers."
  (let ((lst (copy-sequence (gethash topic context-navigator--event-subscribers))))
    (dolist (fn lst)
      (condition-case err
          (apply fn args)
        (error (if (fboundp 'context-navigator-debug)
                   (context-navigator-debug :error :events "subscriber error: %S" err)
                 (message "[context-navigator/events] subscriber error: %S" err)))))))

(defun context-navigator-events-debounce (key delay fn)
  "Debounce FN under KEY for DELAY seconds.
Cancels any existing timer with same KEY and schedules FN."
  (let ((cell (assoc key context-navigator--debounce-timers)))
    (when (and cell (timerp (cdr cell)))
      (cancel-timer (cdr cell))
      (setq context-navigator--debounce-timers
            (assq-delete-all key context-navigator--debounce-timers))))
  (let ((timer (run-at-time delay nil
                            (lambda ()
                              (setq context-navigator--debounce-timers
                                    (assq-delete-all key context-navigator--debounce-timers))
                              (funcall fn)))))
    (push (cons key timer) context-navigator--debounce-timers)
    timer))

(provide 'context-navigator-events)
;;; context-navigator-events.el ends here
