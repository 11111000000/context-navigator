;;; context-navigator-view-indicators.el --- GPTel indicators for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; GPTel indicators support extracted from context-navigator-view.
;; Provides the functions previously defined in the view module so other
;; modules can require this file without creating a load-cycle.
;;
;; This is an incremental extraction: implementations are verbatim from the
;; previous location but live here so we can later progressively refactor
;; internals (polling, watchers, caching) independently.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'context-navigator-events)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-log)
(require 'context-navigator-core)

(defun context-navigator-view--gptel-noisy-src-p (src)
  "Return t when SRC appears to be a noisy source (ignore it).

This prevents feedback loops from variable-watchers and similar noisy
signals."
  (memq src '(gptel-context gptel-context--alist gptel--context)))

(defun context-navigator-view--collect-gptel-keys ()
  "Collect GPTel keys, preferring raw keys when available.

Priority:
1. Raw keys (read-only, if available)
2. Pull from gptel (and convert to model keys)
3. Fallback: enabled items from current core state."
  (let* ((raw-keys (and (fboundp 'context-navigator-gptel--raw-keys)
                        (ignore-errors (context-navigator-gptel--raw-keys))))
         (pulled-keys
          (and (or (null raw-keys) (= (length raw-keys) 0))
               (let ((lst (ignore-errors (context-navigator-gptel-pull))))
                 (and (listp lst)
                      (mapcar #'context-navigator-model-item-key lst)))))
         (fallback-keys
          (when (and (or (null pulled-keys) (= (length pulled-keys) 0))
                     (or (null raw-keys) (= (length raw-keys) 0)))
            (let* ((st (ignore-errors (context-navigator--state-get)))
                   (items (and st (context-navigator-state-items st))))
              (and (listp items)
                   (mapcar #'context-navigator-model-item-key
                           (cl-remove-if-not #'context-navigator-item-enabled items)))))))
    (or raw-keys pulled-keys fallback-keys '())))

(defun context-navigator-view--update-gptel-keys-if-changed (keys)
  "Update cached GPTel KEYS and schedule a render if they changed."
  (let ((h (sxhash-equal keys)))
    (unless (equal h context-navigator-view--gptel-keys-hash)
      (setq context-navigator-view--gptel-keys keys
            context-navigator-view--gptel-keys-hash h)
      (ignore-errors
        (context-navigator-debug :debug :ui
                                 "gptel-change: keys=%d hash=%s"
                                 (length keys) h))
      (context-navigator-view--schedule-render))))

(defun context-navigator-view--maybe-refresh-gptel-keys ()
  "Collect gptel keys and update cache if they differ.

Intended to run inside the view buffer."
  (let ((keys (context-navigator-view--collect-gptel-keys)))
    (context-navigator-view--update-gptel-keys-if-changed keys)))

(defun context-navigator-view--on-gptel-change (&rest args)
  "Handler for :gptel-change events (debounced, batch-aware).

- For lifecycle subtypes (:batch-start/:batch-done/:batch-cancel) we either
  do nothing (start/cancel) or refresh once on :batch-done.
- For noisy variable-watcher-like sources, ignore.
- For regular changes, debounce keys snapshot refresh to coalesce bursts."
  (let ((sub (car args)))
    (cond
     ;; Lifecycle subtypes from core batcher
     ((keywordp sub)
      (pcase sub
        (:batch-start nil)
        (:batch-cancel nil)
        (:batch-done
         (let ((buf (get-buffer context-navigator-view--buffer-name)))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (context-navigator-view--maybe-refresh-gptel-keys))))))
      nil)
     ;; Regular/source-symbol changes
     (t
      (unless (context-navigator-view--gptel-noisy-src-p sub)
        (let ((buf (get-buffer context-navigator-view--buffer-name)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (context-navigator-events-debounce
               :gptel-keys 0.2
               (lambda ()
                 (let ((b (get-buffer context-navigator-view--buffer-name)))
                   (when (buffer-live-p b)
                     (with-current-buffer b
                       (context-navigator-view--maybe-refresh-gptel-keys))))))))))))))

(defun context-navigator-view--subscribe-gptel-events ()
  "Subscribe to :gptel-change and keep the cached keys in sync."
  (push (context-navigator-events-subscribe
         :gptel-change
         #'context-navigator-view--on-gptel-change)
        context-navigator-view--subs))

(defun context-navigator-view--init-gptel-cache ()
  "Initialize cached gptel keys once (best-effort)."
  (let* ((lst (ignore-errors (context-navigator-gptel-pull)))
         (pulled (and (listp lst)
                      (mapcar #'context-navigator-model-item-key lst)))
         (raw (and (or (null pulled) (= (length pulled) 0))
                   (fboundp 'context-navigator-gptel--raw-keys)
                   (ignore-errors (context-navigator-gptel--raw-keys))))
         (keys (or pulled raw '()))
         (h (sxhash-equal keys)))
    (setq context-navigator-view--gptel-keys keys
          context-navigator-view--gptel-keys-hash h)
    (ignore-errors
      (context-navigator-debug :debug :ui
                               "gptel-init: pulled %d keys, hash=%s"
                               (length (or keys '())) h))))

(defun context-navigator-view--gptel-poll-raw-keys ()
  "Return raw GPTel keys using the low-level accessor, or nil on error."
  (and (fboundp 'context-navigator-gptel--raw-keys)
       (ignore-errors (context-navigator-gptel--raw-keys))))

(defun context-navigator-view--gptel-poll-fallback-keys ()
  "Compute fallback keys from the current model state (non-blocking)."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st))))
    (and (listp items)
         (mapcar #'context-navigator-model-item-key
                 (cl-remove-if-not #'context-navigator-item-enabled items)))))

(defun context-navigator-view--gptel-poll-compute (raw-keys)
  "Given RAW-KEYS return a list (keys h use-fallback)."
  (let* ((fallback-keys (when (or (null raw-keys) (= (length raw-keys) 0))
                          (context-navigator-view--gptel-poll-fallback-keys)))
         (keys (or raw-keys fallback-keys '()))
         (h (sxhash-equal keys))
         (use-fallback (and (or (null raw-keys) (= (length raw-keys) 0))
                            (listp fallback-keys))))
    (list keys h use-fallback)))

(defun context-navigator-view--gptel-poll-process (keys h use-fallback)
  "Update cached KEYS/H and schedule a render if they differ from cache."
  (unless (equal h context-navigator-view--gptel-keys-hash)
    (setq context-navigator-view--gptel-keys keys
          context-navigator-view--gptel-keys-hash h)
    (ignore-errors
      (context-navigator-debug :debug :ui
                               "gptel-poll: updated keys=%d hash=%s%s"
                               (length keys) h
                               (if use-fallback " (fallback)" "")))
    (context-navigator-view--schedule-render)))

(defun context-navigator-view--gptel-poll-interval ()
  "Return polling interval for GPTel indicators (non-negative integer)."
  (or context-navigator-gptel-indicator-poll-interval 0))

(defun context-navigator-view--gptel-poll-run-once ()
  "Perform a single poll iteration: compute raw keys and update cache when visible.
Intended to run inside the view buffer context."
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (get-buffer-window buf t)
          (let* ((raw-keys (context-navigator-view--gptel-poll-raw-keys))
                 (res (context-navigator-view--gptel-poll-compute raw-keys))
                 (keys (nth 0 res))
                 (h (nth 1 res))
                 (use-fallback (nth 2 res)))
            (context-navigator-view--gptel-poll-process keys h use-fallback)))))))

(defun context-navigator-view--gptel-poll-callback ()
  "Callback invoked by the poll timer; delegate to `-run-once'.
Kept lightweight to satisfy `run-at-time' requirements."
  (context-navigator-view--gptel-poll-run-once))

(defun context-navigator-view--start-gptel-poll-timer ()
  "Start optional polling to refresh gptel indicators while the sidebar is visible.

Uses `context-navigator-gptel-indicator-poll-interval' to control frequency.
Non-blocking: only reads raw variables and falls back to current model state;
does not call collectors or APIs that may allocate or block."
  (let ((int (context-navigator-view--gptel-poll-interval)))
    (when (> int 0)
      (setq context-navigator-view--gptel-poll-timer
            (run-at-time 0 int #'context-navigator-view--gptel-poll-callback)))))

(provide 'context-navigator-view-indicators)
;;; context-navigator-view-indicators.el ends here
