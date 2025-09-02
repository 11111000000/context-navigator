;;; context-navigator-fp.el --- Functional helpers for context-navigator  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  context-navigator
;; Author: az
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Pure helper functions to encourage functional programming style:
;; - non-destructive alist/plist ops
;; - small set operations/diff by key
;; - simple utilities to work with sequences

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun context-navigator-fp-alist-set (alist key val)
  "Return new ALIST with KEY set to VAL (non-destructive, order-preserving).
If KEY exists, replace its value; otherwise append at the end."
  (let ((found nil)
        (res   nil))
    (dolist (cell alist)
      (if (eq (car cell) key)
          (progn
            (push (cons key val) res)
            (setq found t))
        (push (cons (car cell) (cdr cell)) res)))
    (setq res (nreverse res))
    (if found
        res
      (append res (list (cons key val))))))

(defun context-navigator-fp-alist-del (alist key)
  "Return new ALIST without KEY (non-destructive)."
  (let ((res nil))
    (dolist (cell alist)
      (unless (eq (car cell) key)
        (push (cons (car cell) (cdr cell)) res)))
    (nreverse res)))

(defun context-navigator-fp-plist-set (plist key val)
  "Return new PLIST with KEY set to VAL (non-destructive)."
  (let ((copy (copy-sequence plist)))
    (plist-put copy key val)))

(defun context-navigator-fp-plist-del (plist key)
  "Return new PLIST without KEY (non-destructive)."
  (let ((res nil)
        (skip nil))
    (while plist
      (let ((k (pop plist))
            (v (and plist (pop plist))))
        (if (eq k key)
            (setq skip t)
          (setq res (append res (list k v))))))
    res))

(defun context-navigator-fp-ensure-list (x)
  "Ensure X is a list (wrap non-nil non-list into a singleton list)."
  (cond
   ((null x) nil)
   ((listp x) x)
   (t (list x))))

(defun context-navigator-fp-unique-by (seq key-fn)
  "Return new list with duplicates removed by KEY-FN (last occurrence wins).
Order of resulting elements follows the order of their last appearance in SEQ."
  (let ((seen (make-hash-table :test 'equal))
        (last (make-hash-table :test 'equal)))
    ;; 1) remember the last value for each key and the last index
    (let ((i 0))
      (dolist (it seq)
        (puthash (funcall key-fn it) it seen)
        (puthash (funcall key-fn it) i last)
        (setq i (1+ i))))
    ;; 2) collect keys in the order of last appearance
    (let ((ordered nil)
          (emitted (make-hash-table :test 'equal))
          (j 0))
      (dolist (it seq)
        (let* ((k (funcall key-fn it))
               (is-last (= j (gethash k last))))
          (when (and is-last (not (gethash k emitted)))
            (puthash k t emitted)
            (push k ordered)))
        (setq j (1+ j)))
      (setq ordered (nreverse ordered))
      ;; 3) map back to last values
      (mapcar (lambda (k) (gethash k seen)) ordered))))

(defun context-navigator-fp-diff-by-key (old-list new-list key-fn &optional equal-p)
  "Diff OLD-LIST and NEW-LIST by KEY-FN.
Return plist: (:add ADD-LST :remove REM-LST :update UPD-LST)

- ADD-LST: items present in NEW but not in OLD
- REM-LST: items present in OLD but not in NEW
- UPD-LST: items whose keys are in both but values differ per EQUAL-P (default `equal')"
  (let* ((equal-p (or equal-p #'equal))
         (old-h (make-hash-table :test 'equal))
         (new-h (make-hash-table :test 'equal))
         add rem upd)
    (dolist (x old-list) (puthash (funcall key-fn x) x old-h))
    (dolist (x new-list) (puthash (funcall key-fn x) x new-h))
    ;; additions
    (maphash
     (lambda (k v)
       (unless (gethash k old-h)
         (push v add)))
     new-h)
    ;; removals / updates
    (maphash
     (lambda (k v-old)
       (let ((v-new (gethash k new-h)))
         (cond
          ((null v-new) (push v-old rem))
          ((not (funcall equal-p v-old v-new)) (push v-new upd)))))
     old-h)
    (list :add (nreverse add)
          :remove (nreverse rem)
          :update (nreverse upd))))

(provide 'context-navigator-fp)
;;; context-navigator-fp.el ends here
