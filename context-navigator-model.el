;;; context-navigator-model.el --- Pure model for context-navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Pure data model and operations:
;; - item struct and stable keys
;; - normalization and diff
;; - no side effects, no IO

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-fp)

(cl-defstruct (context-navigator-item
               (:constructor context-navigator-item-create))
  "Context item (pure value)."
  type       ;; symbol: 'file | 'buffer | 'selection
  name       ;; display name (string)
  path       ;; absolute or project-relative path (string or nil)
  buffer     ;; buffer or nil (for buffer/selection kinds)
  beg        ;; region start (int or nil)
  end        ;; region end (int or nil)
  size       ;; file size (int or nil)
  enabled    ;; boolean (t/nil)
  meta)      ;; plist with extra info (mime, mode, etc)

(defun context-navigator-model-item-key (item)
  "Stable key for ITEM."
  (pcase (context-navigator-item-type item)
    ('file
     (format "file:%s" (or (context-navigator-item-path item) "")))
    ('buffer
     (format "buf:%s:%s"
             (or (context-navigator-item-name item) "")
             (or (context-navigator-item-path item) "")))
    ('selection
     (format "sel:%s:%s-%s"
             (or (context-navigator-item-path item) "")
             (or (context-navigator-item-beg item) 0)
             (or (context-navigator-item-end item) 0)))
    (_ (format "unknown:%s" (or (context-navigator-item-name item) "")))))

(defun context-navigator-model-item= (a b)
  "Value equality of items (ignoring transient/non-essential fields in META)."
  (and (eq (context-navigator-item-type a) (context-navigator-item-type b))
       (equal (context-navigator-item-name a) (context-navigator-item-name b))
       (equal (context-navigator-item-path a) (context-navigator-item-path b))
       (equal (context-navigator-item-beg a) (context-navigator-item-beg b))
       (equal (context-navigator-item-end a) (context-navigator-item-end b))
       (equal (context-navigator-item-size a) (context-navigator-item-size b))
       (eq (not (null (context-navigator-item-enabled a)))
           (not (null (context-navigator-item-enabled b))))
       ;; meta equality is shallow; callers may provide stricter comparator if needed
       (equal (context-navigator-item-meta a) (context-navigator-item-meta b))))

(defun context-navigator-model-uniq (items)
  "Deduplicate ITEMS by stable key (last occurrence wins)."
  (context-navigator-fp-unique-by items #'context-navigator-model-item-key))

(defun context-navigator-model-diff (old-items new-items)
  "Diff OLD-ITEMS and NEW-ITEMS by stable keys.
Return plist: (:add L :remove L :update L)."
  (context-navigator-fp-diff-by-key
   (context-navigator-model-uniq old-items)
   (context-navigator-model-uniq new-items)
   #'context-navigator-model-item-key
   #'context-navigator-model-item=))

(defun context-navigator-model-build-index (items)
  "Build a hash table index of ITEMS by stable key (pure)."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (it items)
      (puthash (context-navigator-model-item-key it) it ht))
    ht))

(provide 'context-navigator-model)
;;; context-navigator-model.el ends here
