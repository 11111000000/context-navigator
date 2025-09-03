;;; context-navigator-gptel-bridge.el --- gptel capability adapter -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; This module adapts to gptel context API if present.
;; - Pulls current gptel context into model items (pure transformation)
;; - Applies desired items back to gptel (graceful degradation)
;; - Does not install advices by default; core no longer listens to :gptel-change
;;
;; Design:
;; - Functional by default: conversion routines are pure.
;; - Side-effects are constrained to `apply' only.
;; - If precise per-item removal is not available, fallback to reset (remove-all + add).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)
(require 'context-navigator-events)

(defvar context-navigator--gptel-advices nil
  "List of (SYMBOL . FN) advices installed by this module.")

(defun context-navigator-gptel-available-p ()
  "Return non-nil when gptel context functions are available."
  (or (boundp 'gptel-context)
      (fboundp 'gptel-context-add)
      (fboundp 'gptel-context-add-file)
      (fboundp 'gptel-context--add-region)))

(defun context-navigator-gptel--context-list ()
  "Return raw gptel context entries or nil if unavailable.

Try several common gptel entry sources for compatibility with multiple
gptel versions: gptel-context-list, gptel-context--alist, gptel-context--collect,
or the variable gptel-context / gptel--context."
  (let (src val)
    (cond
     ((fboundp 'gptel-context-list)
      (setq src 'gptel-context-list
            val (ignore-errors (gptel-context-list))))
     ((boundp 'gptel-context--alist)
      (setq src 'gptel-context--alist
            val (ignore-errors (symbol-value 'gptel-context--alist))))
     ((fboundp 'gptel-context--collect)
      (setq src 'gptel-context--collect
            val (ignore-errors (gptel-context--collect))))
     ((boundp 'gptel-context)
      (setq src 'gptel-context
            val (ignore-errors (symbol-value 'gptel-context))))
     ((boundp 'gptel--context)
      (setq src 'gptel--context
            val (ignore-errors (symbol-value 'gptel--context))))
     (t (setq src 'none val nil)))
    (when (bound-and-true-p context-navigator-debug)
      (message "[context-navigator/gptel] context-list via %s -> %s entries"
               src (cond
                    ((listp val) (length val))
                    ((vectorp val) (length val))
                    ((null val) 0)
                    (t 'unknown))))
    val))

(defun context-navigator-gptel--entry->item (entry)
  "Convert various gptel ENTRY shapes into one or more context-navigator-item objects.

Supports:
- plist-like entries with :type/:path/:beg/:end etc.
- cons (BUFFER . REGS) where REGS are:
  - list of overlays
  - a pair (BEG . END) of ints/markers
  - a list/vec whose first two elements are BEG/END
  - t/:buffer/:all to denote whole buffer
  (produces buffer or selection items per region)
- cons (PATH . PROPS) or bare PATH string (file item)

Return a single item or a list of items, or nil."
  (let ((dbg (bound-and-true-p context-navigator-debug)))
    (cond
     ;; Plist-style entry (preferred)
     ((and (listp entry)
           (or (plist-member entry :type) (plist-member entry 'type)))
      (let* ((get (lambda (k) (or (plist-get entry k) (alist-get k entry))))
             (type (or (funcall get :type) (funcall get 'type)))
             (path (or (funcall get :path) (funcall get 'path)))
             (beg  (or (funcall get :beg)  (funcall get 'beg)))
             (end  (or (funcall get :end)  (funcall get 'end)))
             (buf  (or (funcall get :buffer) (funcall get 'buffer)))
             (name (or (funcall get :name)
                       (and (stringp path) (file-name-nondirectory path)))))
        (pcase type
          ((or 'file :file)
           (context-navigator-item-create
            :type 'file :name (or name path) :path path :enabled t))
          ((or 'selection :selection 'region :region)
           (when (and (integerp beg) (integerp end))
             (let* ((buf-path (or path
                                  (and (bufferp buf) (buffer-live-p buf)
                                       (buffer-local-value 'buffer-file-name buf))))
                    (nm (or name
                            (if buf-path
                                (format "%s:%s-%s" (file-name-nondirectory buf-path) beg end)
                              (format "%s:%s-%s" (or (and (bufferp buf) (buffer-name buf)) "<selection>") beg end)))))
               (context-navigator-item-create
                :type 'selection
                :name nm
                :path buf-path
                :buffer buf
                :beg beg
                :end end
                :enabled t))))
          ((or 'buffer :buffer)
           (context-navigator-item-create
            :type 'buffer
            :name (or name (and (bufferp buf) (buffer-name buf)) "<buffer>")
            :path path
            :buffer buf
            :enabled t))
          (_ (when dbg (message "[context-navigator/gptel] Unknown plist type in entry=%S" entry))
             nil))))
     ;; (BUFFER . REGS) style (overlays, pairs, lists with positions, or whole buffer markers)
     ((and (consp entry) (bufferp (car entry)))
      (let* ((buf (car entry))
             (raw (cdr entry))
             ;; Normalize raw regions into a list
             (regs (cond
                    ;; raw is a single cons (beg . end)
                    ((and (consp raw)
                          (or (numberp (car raw)) (markerp (car raw)))
                          (or (numberp (cdr raw)) (markerp (cdr raw))))
                     (list raw))
                    ;; vector -> list
                    ((vectorp raw) (append raw nil))
                    ;; already a list
                    (t raw)))
             (items nil)
             (buf-path (and (buffer-live-p buf) (buffer-local-value 'buffer-file-name buf)))
             (pmin (and (buffer-live-p buf) (with-current-buffer buf (point-min))))
             (pmax (and (buffer-live-p buf) (with-current-buffer buf (point-max)))))
        (dolist (r regs)
          (cond
           ;; Overlay case
           ((and (overlayp r) (overlay-start r) (overlay-end r))
            (let ((s (overlay-start r))
                  (e (overlay-end r)))
              (if (and pmin pmax (= s pmin) (= e pmax))
                  (push (context-navigator-item-create
                         :type 'buffer
                         :name (buffer-name buf)
                         :path buf-path
                         :buffer buf
                         :enabled t)
                        items)
                (push (context-navigator-item-create
                       :type 'selection
                       :name (format "%s (selection)" (buffer-name buf))
                       :path buf-path
                       :buffer buf
                       :beg s :end e
                       :enabled t)
                      items))))
           ;; Pair (beg . end) of ints/markers
           ((and (consp r)
                 (or (numberp (car r)) (markerp (car r)))
                 (or (numberp (cdr r)) (markerp (cdr r))))
            (let* ((s (if (markerp (car r)) (marker-position (car r)) (car r)))
                   (e (if (markerp (cdr r)) (marker-position (cdr r)) (cdr r))))
              (if (and pmin pmax (= s pmin) (= e pmax))
                  (push (context-navigator-item-create
                         :type 'buffer
                         :name (buffer-name buf)
                         :path buf-path
                         :buffer buf
                         :enabled t)
                        items)
                (push (context-navigator-item-create
                       :type 'selection
                       :name (format "%s (selection)" (buffer-name buf))
                       :path buf-path
                       :buffer buf
                       :beg s :end e
                       :enabled t)
                      items))))
           ;; List or vector whose first two elements are positions
           ((and (listp r)
                 (>= (length r) 2)
                 (let ((a (nth 0 r)) (b (nth 1 r)))
                   (and (or (numberp a) (markerp a))
                        (or (numberp b) (markerp b)))))
            (let* ((a (nth 0 r)) (b (nth 1 r))
                   (s (if (markerp a) (marker-position a) a))
                   (e (if (markerp b) (marker-position b) b)))
              (if (and pmin pmax (= s pmin) (= e pmax))
                  (push (context-navigator-item-create
                         :type 'buffer
                         :name (buffer-name buf)
                         :path buf-path
                         :buffer buf
                         :enabled t)
                        items)
                (push (context-navigator-item-create
                       :type 'selection
                       :name (format "%s (selection)" (buffer-name buf))
                       :path buf-path
                       :buffer buf
                       :beg s :end e
                       :enabled t)
                      items))))
           ;; Whole buffer marker
           ((memq r '(t all :all :buffer))
            (push (context-navigator-item-create
                   :type 'buffer
                   :name (buffer-name buf)
                   :path buf-path
                   :buffer buf
                   :enabled t)
                  items))
           (t
            (when dbg
              (message "[context-navigator/gptel] Unknown buffer region element: %S (buf=%s)"
                       r (buffer-name buf))))))
        (when dbg
          (let* ((bufs (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'buffer)) items))
                 (sels (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'selection)) items)))
            (message "[context-navigator/gptel] parsed buffer entry %s -> %s items (buffers=%s selections=%s)"
                     (buffer-name buf) (length items) bufs sels)))
        (nreverse items)))
     ;; (PATH . PROPS) or bare \"path\" string
     ((or (and (consp entry) (stringp (car entry)))
          (stringp entry))
      (let ((path (if (stringp entry) entry (car entry))))
        (when (and (stringp path) (file-exists-p path))
          (context-navigator-item-create
           :type 'file :name (file-name-nondirectory path) :path path :enabled t))))
     (t
      (when dbg
        (message "[context-navigator/gptel] Unknown entry shape: %S" entry))
      nil))))

(defun context-navigator-gptel-pull ()
  "Return list<context-navigator-item> reflecting current gptel context.

This function is tolerant: it flattens nested results from the
entry->item converter and returns a unique list of items.

Robustness improvements:
- Accept a single item, a list/vector of items, or mixed lists and extract valid items.
- When `context-navigator-debug' is non-nil, log unexpected conversions."
  (let* ((raw (context-navigator-gptel--context-list))
         (flat
          (and raw
               (mapcan
                (lambda (entry)
                  (let ((it (ignore-errors (context-navigator-gptel--entry->item entry))))
                    (cond
                     ((null it) nil)
                     ((context-navigator-item-p it) (list it))
                     ((and (listp it) (cl-every #'context-navigator-item-p it)) it)
                     ((and (vectorp it))
                      (let (res)
                        (dotimes (i (length it))
                          (let ((elt (aref it i)))
                            (when (context-navigator-item-p elt)
                              (push elt res))))
                        (nreverse res)))
                     ((and (listp it) (context-navigator-item-p (car it))) it)
                     ((listp it)
                      (let (res)
                        (dolist (el it)
                          (when (context-navigator-item-p el) (push el res)))
                        (nreverse res)))
                     (t
                      (when (bound-and-true-p context-navigator-debug)
                        (message "[context-navigator/gptel] Ignored entry->item result for entry=%S -> %S"
                                 entry it))
                      nil))))
                raw)))
         (uniq (and flat (context-navigator-model-uniq flat))))
    (when (bound-and-true-p context-navigator-debug)
      (let* ((count (lambda (pred lst)
                      (cl-count-if pred lst)))
             (files (and uniq (funcall count (lambda (x) (eq (context-navigator-item-type x) 'file)) uniq)))
             (bufs  (and uniq (funcall count (lambda (x) (eq (context-navigator-item-type x) 'buffer)) uniq)))
             (sels  (and uniq (funcall count (lambda (x) (eq (context-navigator-item-type x) 'selection)) uniq))))
        (message "[context-navigator/gptel] pull -> items total=%s files=%s buffers=%s selections=%s"
                 (length (or uniq '())) (or files 0) (or bufs 0) (or sels 0))))
    uniq))

(defun context-navigator-gptel--can-add-file () (fboundp 'gptel-context-add-file))
(defun context-navigator-gptel--can-add-region () (fboundp 'gptel-context--add-region))
(defun context-navigator-gptel--can-remove () (fboundp 'gptel-context-remove))
(defun context-navigator-gptel--can-remove-all () (fboundp 'gptel-context-remove-all))

(defun context-navigator-gptel--add-item (item)
  "Try to add ITEM to gptel context. Return t on success."
  (pcase (context-navigator-item-type item)
    ('file
     (when (context-navigator-gptel--can-add-file)
       (let ((p (context-navigator-item-path item)))
         (when (and (stringp p) (file-readable-p p))
           (gptel-context-add-file p)
           t))))
    ('selection
     (when (context-navigator-gptel--can-add-region)
       (let* ((p (context-navigator-item-path item))
              (b (context-navigator-item-beg item))
              (e (context-navigator-item-end item))
              (buf (or (context-navigator-item-buffer item)
                       (and (stringp p) (file-exists-p p)
                            (find-file-noselect p)))))
         (when (and (bufferp buf) (buffer-live-p buf)
                    (integerp b) (integerp e) (<= (min b e) (max b e)))
           (gptel-context--add-region buf b e)
           t))))
    (_ nil)))

(defun context-navigator-gptel--reset-to (items)
  "Replace gptel context with ITEMS using remove-all + add.
Only enabled items are (re)added.

This function attempts a best-effort remove-all when supported by the
underlying gptel; it publishes a :gptel-change :reset event so listeners
(e.g. the core sync) can react. We keep the call guarded and tolerate
errors from different gptel versions."
  (when (context-navigator-gptel--can-remove-all)
    (ignore-errors (gptel-context-remove-all)))
  (let ((count 0))
    (dolist (it items)
      (when (context-navigator-item-enabled it)
        (setq count (+ count (if (context-navigator-gptel--add-item it) 1 0)))))
    ;; publish a clear reset event so consumers can schedule a sync/update
    (context-navigator-events-publish :gptel-change :reset count)
    (list :method 'reset :count count)))

(defun context-navigator-gptel-apply (desired-items)
  "Apply DESIRED-ITEMS to gptel. Returns a plist describing action.
Когда точечные операции невозможны, выполняется сброс (reset)."
  (unless (context-navigator-gptel-available-p)
    (cl-return-from context-navigator-gptel-apply (list :applied nil :reason 'unavailable)))
  (let* ((current (context-navigator-gptel-pull))
         (diff (context-navigator-model-diff current desired-items))
         (adds (plist-get diff :add))
         (rems (plist-get diff :remove))
         (upds (plist-get diff :update)))
    (cond
     ;; Fall back to reset when core remove/add primitives are missing.
     ((or (not (context-navigator-gptel--can-remove))
          (not (context-navigator-gptel--can-add-file)))
      (let ((r (context-navigator-gptel--reset-to desired-items)))
        (append (list :applied t) r)))
     ;; If selections are involved in removals/updates, conservative reset.
     ((or (cl-some (lambda (x) (eq (context-navigator-item-type x) 'selection)) rems)
          (cl-some (lambda (x) (eq (context-navigator-item-type x) 'selection)) upds))
      (let ((r (context-navigator-gptel--reset-to desired-items)))
        (append (list :applied t) r)))
     (t
      ;; Fine-grained diff: try to remove/update/add as needed.
      (let ((ops 0))
        ;; Removes: attempt to remove files and buffers when possible.
        (dolist (old rems)
          (pcase (context-navigator-item-type old)
            ('file
             (when (context-navigator-gptel--can-remove)
               (ignore-errors
                 (gptel-context-remove (context-navigator-item-path old)))))
            ('buffer
             (when (context-navigator-gptel--can-remove)
               (ignore-errors
                 ;; Try several reasonable identifiers for gptel to remove:
                 ;; 1) the buffer object itself
                 ;; 2) the persisted path
                 ;; 3) the buffer name
                 (let ((buf (context-navigator-item-buffer old))
                       (p (context-navigator-item-path old))
                       (name (context-navigator-item-name old)))
                   (cond
                    ((and buf (buffer-live-p buf))
                     (ignore-errors (gptel-context-remove buf)))
                    ((and (stringp p)) (ignore-errors (gptel-context-remove p)))
                    ((and (stringp name)) (ignore-errors (gptel-context-remove name))))))))
            (_ nil))
          (setq ops (1+ ops)))
        ;; Updates -> remove then add (add only when target is enabled)
        (dolist (nu upds)
          (pcase (context-navigator-item-type nu)
            ('file
             (when (context-navigator-gptel--can-remove)
               (ignore-errors (gptel-context-remove (context-navigator-item-path nu)))))
            ('buffer
             (when (context-navigator-gptel--can-remove)
               (ignore-errors
                 (let ((buf (context-navigator-item-buffer nu))
                       (p (context-navigator-item-path nu))
                       (name (context-navigator-item-name nu)))
                   (cond
                    ((and buf (buffer-live-p buf)) (ignore-errors (gptel-context-remove buf)))
                    ((and (stringp p)) (ignore-errors (gptel-context-remove p)))
                    ((and (stringp name)) (ignore-errors (gptel-context-remove name))))))))
            (_ nil))
          (when (context-navigator-item-enabled nu)
            (when (context-navigator-gptel--add-item nu)
              (setq ops (1+ ops)))))
        ;; Adds (only enabled items are added)
        (dolist (nu adds)
          (when (context-navigator-item-enabled nu)
            (when (context-navigator-gptel--add-item nu)
              (setq ops (1+ ops)))))
        (context-navigator-events-publish :gptel-change :diff ops)
        (list :applied t :method 'diff :ops ops))))))

(defun context-navigator-gptel-on-change-register ()
  "No-op: Navigator no longer listens to gptel changes."
  (setq context-navigator--gptel-advices nil)
  t)

(defun context-navigator-gptel-on-change-unregister ()
  "No-op: Navigator no longer listens to gptel changes."
  (setq context-navigator--gptel-advices nil)
  t)

(provide 'context-navigator-gptel-bridge)
;;; context-navigator-gptel-bridge.el ends here
