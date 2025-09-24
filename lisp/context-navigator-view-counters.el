;;; context-navigator-view-counters.el --- Quick counters API for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Quick counters for the sidebar: openable (files/selections that can be opened)
;; and collect-closable-buffers (live buffers referenced by items).
;;
;; These functions operate on the sidebar buffer named by
;; `context-navigator-view--buffer-name' and update buffer-local cache
;; variables that the view reads for rendering. This keeps the caching
;; policy and TTL logic colocated with the counter implementation while
;; allowing the View to call a small API.
;;
;; Public API:
;; - context-navigator-view-counters-invalidate
;; - context-navigator-view-counters-get-openable  -> (COUNT . PLUS)
;; - context-navigator-view-counters-refresh-openable
;; - context-navigator-view-counters-collect-closable

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-core)
(require 'context-navigator-events)

(defgroup context-navigator-view-counters nil
  "Counters helpers for Context Navigator view."
  :group 'context-navigator)

;; Helper: candidate check copied/adapted from view
(defun context-navigator-view-counters--openable--candidate-p (item)
  "Return non-nil if ITEM can be opened in background (file-backed).
This mirrors the original view logic with respect to `context-navigator-openable-remote-mode'."
  (let ((enabled (and (context-navigator-item-enabled item) t)))
    (when enabled
      (pcase (context-navigator-item-type item)
        ('buffer
         (let* ((buf (context-navigator-item-buffer item))
                (p (context-navigator-item-path item)))
           (and (stringp p)
                ;; already open: skip
                (not (and buf (buffer-live-p buf)))
                (let ((remote (file-remote-p p)))
                  (cond
                   ((and remote (eq context-navigator-openable-remote-mode 'off)) nil)
                   ((and remote (eq context-navigator-openable-remote-mode 'lazy))
                    (not (get-file-buffer p)))
                   (t (and (file-exists-p p) (not (get-file-buffer p)))))))))
        ('selection
         (let ((p (context-navigator-item-path item)))
           (and (stringp p)
                (let ((remote (file-remote-p p)))
                  (cond
                   ((and remote (eq context-navigator-openable-remote-mode 'off)) nil)
                   ((and remote (eq context-navigator-openable-remote-mode 'lazy))
                    (not (get-file-buffer p)))
                   (t (and (file-exists-p p) (not (get-file-buffer p)))))))))
        ('file
         (let ((p (context-navigator-item-path item)))
           (and (stringp p)
                (let ((remote (file-remote-p p)))
                  (cond
                   ((and remote (eq context-navigator-openable-remote-mode 'off)) nil)
                   ((and remote (eq context-navigator-openable-remote-mode 'lazy))
                    (not (get-file-buffer p)))
                   (t (and (file-exists-p p) (not (get-file-buffer p)))))))))
        (_ nil)))))

(defun context-navigator-view-counters--compute-openable (cap)
  "Return cons (COUNT . PLUS) for openable items, short-circuiting at CAP.
PLUS is non-nil when CAP was reached."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st)))
         (n 0)
         (limit (or cap most-positive-fixnum)))
    (catch 'done
      (dolist (it (or items '()))
        (when (context-navigator-view-counters--openable--candidate-p it)
          (setq n (1+ n))
          (when (>= n limit)
            (throw 'done t)))))
    (cons n (and (numberp cap) (>= n (or cap 0))))))

;;; Public API ---------------------------------------------------------------

(defun context-navigator-view-counters-invalidate ()
  "Invalidate cached openable counters and cancel pending timers (safe outside the view buffer)."
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; These are buffer-local vars originally declared in view.el.
        (setq context-navigator-view--openable-count nil
              context-navigator-view--openable-plus nil
              context-navigator-view--openable-stamp 0.0)
        (let ((tm (and (boundp 'context-navigator-view--openable-timer)
                       context-navigator-view--openable-timer)))
          (when (timerp tm)
            (cancel-timer tm))
          (setq context-navigator-view--openable-timer nil))))))

(defun context-navigator-view-counters-refresh-openable ()
  "Recompute openable count synchronously with soft-cap; update cache and schedule UI render if changed.
Intended to be safe to call from timers and hooks."
  (let* ((res (context-navigator-view-counters--compute-openable context-navigator-openable-soft-cap))
         (n (car res))
         (plus (cdr res)))
    (let ((buf (get-buffer context-navigator-view--buffer-name)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((old (and (boundp 'context-navigator-view--openable-count)
                          context-navigator-view--openable-count))
                (changed (not (equal (and (boundp 'context-navigator-view--openable-count)
                                          context-navigator-view--openable-count)
                                     n))))
            (setq context-navigator-view--openable-count n
                  context-navigator-view--openable-plus plus
                  context-navigator-view--openable-stamp (float-time))
            (when changed
              (context-navigator-view--schedule-render)))
          n)))))

(defun context-navigator-view-counters-get-openable ()
  "Return cons (COUNT . PLUS) from cache; schedule a debounced refresh when stale.

This is the primary read API used by the view render code. Reads cache from
the Navigator sidebar buffer to respect buffer-local cache variables.
If the cache is empty or zero while there are items in the model, compute
synchronously once to provide a useful value."
  (let* ((now (float-time))
         (ttl (or context-navigator-openable-count-ttl 0.3))
         (buf (get-buffer context-navigator-view--buffer-name))
         (vals (and (buffer-live-p buf)
                    (with-current-buffer buf
                      (list :count (and (boundp 'context-navigator-view--openable-count)
                                        context-navigator-view--openable-count)
                            :stamp (if (boundp 'context-navigator-view--openable-stamp)
                                       context-navigator-view--openable-stamp
                                     0.0)
                            :plus  (and (boundp 'context-navigator-view--openable-plus)
                                        context-navigator-view--openable-plus)))))
         (count (plist-get vals :count))
         (stamp (or (plist-get vals :stamp) 0.0))
         (plus  (plist-get vals :plus)))
    ;; Compute once now when cache is empty OR zero while model has items,
    ;; to avoid returning 0 spuriously right after a refresh in tests/UI.
    (when (and (buffer-live-p buf)
               (or (null count)
                   (and (integerp count)
                        (= count 0)
                        (let* ((st (ignore-errors (context-navigator--state-get)))
                               (items (and st (context-navigator-state-items st))))
                          (and (listp items) (> (length items) 0))))))
      (let* ((res (context-navigator-view-counters--compute-openable context-navigator-openable-soft-cap))
             (n (car res))
             (pl (cdr res)))
        (with-current-buffer buf
          (setq context-navigator-view--openable-count n
                context-navigator-view--openable-plus pl
                context-navigator-view--openable-stamp (float-time)))
        (setq count n
              plus pl
              stamp (float-time))))
    ;; Schedule a debounced refresh when stale.
    (when (or (null count)
              (> (- now stamp) (max 0 ttl)))
      (context-navigator-events-debounce
       :sidebar-openable 0.18
       (lambda ()
         (let ((b (get-buffer context-navigator-view--buffer-name)))
           (when (buffer-live-p b)
             (with-current-buffer b
               (context-navigator-view-counters-refresh-openable)))))))
    (cons (or count 0)
          (and plus
               (> (or count 0) 0)))))

(defun context-navigator-view-counters-collect-closable ()
  "Collect unique live buffers corresponding to current model items.
Returns a list of buffer objects; excludes the sidebar buffer itself.

This mirrors the original view helper and is used by the footer action."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st)))
         (seen (make-hash-table :test 'eq))
         (res '())
         (sidebar-buf (get-buffer context-navigator-view--buffer-name)))
    (dolist (it (or items '()))
      (let ((type (context-navigator-item-type it)))
        (cond
         ((eq type 'buffer)
          (let ((buf (or (context-navigator-item-buffer it)
                         (and (context-navigator-item-path it)
                              (get-file-buffer (context-navigator-item-path it))))))
            (when (and (bufferp buf) (buffer-live-p buf)
                       (not (eq buf sidebar-buf))
                       (not (gethash buf seen)))
              (puthash buf t seen)
              (push buf res))))
         ((eq type 'selection)
          (let ((p (context-navigator-item-path it)))
            (when (and (stringp p) (get-file-buffer p))
              (let ((buf (get-file-buffer p)))
                (when (and (buffer-live-p buf)
                           (not (eq buf sidebar-buf))
                           (not (gethash buf seen)))
                  (puthash buf t seen)
                  (push buf res))))))
         ((eq type 'file)
          (let ((p (context-navigator-item-path it)))
            (when (and (stringp p) (get-file-buffer p))
              (let ((buf (get-file-buffer p)))
                (when (and (buffer-live-p buf)
                           (not (eq buf sidebar-buf))
                           (not (gethash buf seen)))
                  (puthash buf t seen)
                  (push buf res)))))))))
    (nreverse res)))

(provide 'context-navigator-view-counters)
;;; context-navigator-view-counters.el ends here
