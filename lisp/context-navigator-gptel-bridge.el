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
(require 'context-navigator-log)

(defvar context-navigator--gptel-advices nil
  "List of (SYMBOL . FN) advices installed by this module.")
(defvar context-navigator--gptel-var-watchers nil
  "List of (SYMBOL . FN) variable watchers installed by this module.")

(defmacro context-navigator--silence-messages (&rest body)
  "Execute BODY while suppressing echo-area and *Messages* logging."
  (declare (indent 0) (debug t))
  `(let ((inhibit-message t)
         (message-log-max nil))
     ,@body))

(defun context-navigator-gptel--ensure-loaded ()
  "Try to load gptel once. Return non-nil when loaded."
  (or (featurep 'gptel)
      (ignore-errors (require 'gptel nil t))))

(defun context-navigator-gptel-available-p ()
  "Return non-nil when gptel context functions are available.
Attempts to soft-require gptel on first call."
  (context-navigator-gptel--ensure-loaded)
  (or (boundp 'gptel-context)
      (fboundp 'gptel-context-add)
      (fboundp 'gptel-context-add-file)
      (fboundp 'gptel-context--add-region)
      (boundp 'gptel-context--alist)))

(defun context-navigator-gptel--context-list ()
  "Return raw gptel context entries or nil if unavailable.

Important: prefer read-only sources (variables/APIs) to avoid triggering
internal writes in gptel (e.g. `gptel-context--collect' may rewrite
`gptel-context--alist' and fire variable watchers).

Order of preference:
- gptel-context--alist (variable)
- gptel-context (public variable)
- gptel--context (private variable)
- gptel-context-list (public API)
- gptel-context--collect (function, last resort)"
  (context-navigator-gptel--ensure-loaded)
  (let* ((seqs
          (list
           (cons 'alist   (and (boundp 'gptel-context--alist)
                               (ignore-errors (symbol-value 'gptel-context--alist))))
           (cons 'public  (and (boundp 'gptel-context)
                               (ignore-errors (symbol-value 'gptel-context))))
           (cons 'priv    (and (boundp 'gptel--context)
                               (ignore-errors (symbol-value 'gptel--context))))
           (cons 'api     (and (fboundp 'gptel-context-list)
                               (ignore-errors (gptel-context-list))))
           (cons 'collect (and (fboundp 'gptel-context--collect)
                               (ignore-errors (gptel-context--collect)))))) ;; last-resort
         (nonempty
          (cl-find-if (lambda (cell)
                        (let ((v (cdr cell)))
                          (and (sequencep v) (> (length v) 0))))
                      seqs))
         (chosen (or (and nonempty (cdr nonempty))
                     (let ((first (cl-find-if (lambda (cell) (sequencep (cdr cell))) seqs)))
                       (and first (cdr first)))))
         (src (car (or nonempty (cl-find-if (lambda (cell) (sequencep (cdr cell))) seqs)))))
    (ignore-errors
      (context-navigator-debug :trace :gptel
                               "context-list source=%s size=%s" src (length (or chosen '()))))
    chosen))

(defun context-navigator-gptel--raw-keys ()
  "Best-effort stable keys directly from raw gptel list (including selections)."
  (let* ((raw (context-navigator-gptel--context-list))
         (flat (and (sequencep raw)
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
                          (t nil))))
                     raw)))
         (uniq (and flat (context-navigator-model-uniq flat))))
    (and uniq (mapcar #'context-navigator-model-item-key uniq))))

(defun context-navigator-gptel--raw-items-fallback ()
  "Best-effort items from raw gptel list (files and whole buffers).
Selections are not reconstructed here."
  (let ((raw (context-navigator-gptel--context-list))
        (items '()))
    (dolist (e raw)
      (cond
       ((stringp e)
        (push (context-navigator-item-create
               :type 'file :name (file-name-nondirectory e) :path e :enabled t)
              items))
       ((and (consp e) (stringp (car e)))
        (let ((p (car e)))
          (push (context-navigator-item-create
                 :type 'file :name (file-name-nondirectory p) :path p :enabled t)
                items)))
       ((and (consp e) (bufferp (car e)))
        (let* ((buf (car e))
               (p   (and (buffer-live-p buf)
                         (buffer-local-value 'buffer-file-name buf)))
               (nm  (and (buffer-live-p buf) (buffer-name buf))))
          (push (context-navigator-item-create
                 :type 'buffer :name (or nm "<buffer>") :path p :buffer buf :enabled t)
                items)))))
    (nreverse items)))

;; Helpers for pure, shape-specific parsing

(defun context-navigator-gptel--pos (x)
  (cond ((markerp x) (marker-position x))
        ((numberp x) x)
        (t nil)))

(defun context-navigator-gptel--plist-like-get (pl k)
  (or (plist-get pl k) (alist-get k pl)))

(defun context-navigator-gptel--make-file-item (path &optional name)
  (when (and (stringp path) (not (string-empty-p path)))
    (context-navigator-item-create
     :type 'file
     :name (or name (file-name-nondirectory path))
     :path path
     :enabled t)))

(defun context-navigator-gptel--make-buffer-item (buf path &optional name)
  (context-navigator-item-create
   :type 'buffer
   :name (or name
             (and (stringp path) (file-name-nondirectory path))
             (and (bufferp buf) (buffer-live-p buf) (buffer-name buf))
             "<buffer>")
   :path path
   :buffer buf
   :enabled t))

(defun context-navigator-gptel--make-selection-item (buf path beg end &optional name)
  (when (and (integerp beg) (integerp end))
    (context-navigator-item-create
     :type 'selection
     :name (or name
               (if (and (stringp path) (not (string-empty-p path)))
                   (format "%s:%s-%s" (file-name-nondirectory path) beg end)
                 (format "%s:%s-%s" (or (and (bufferp buf) (buffer-live-p buf) (buffer-name buf)) "<selection>") beg end)))
     :path path
     :buffer buf
     :beg beg
     :end end
     :enabled t)))

(defun context-navigator-gptel--coerce-regs-to-list (raw)
  (cond
   ;; Already a single (beg . end)
   ((and (consp raw)
         (or (numberp (car raw)) (markerp (car raw)))
         (or (numberp (cdr raw)) (markerp (cdr raw))))
    (list raw))
   ;; Vector -> list
   ((vectorp raw) (append raw nil))
   ;; List or single atom
   ((listp raw) raw)
   ((null raw) nil)
   (t (list raw))))

(defun context-navigator-gptel--normalize-region (r)
  "Return either :all or a cons (beg . end) or nil."
  (cond
   ;; Whole buffer markers
   ((memq r '(t all :all :buffer)) :all)
   ;; Overlay
   ((and (overlayp r) (overlay-start r) (overlay-end r))
    (cons (overlay-start r) (overlay-end r)))
   ;; Pair (beg . end) of ints/markers
   ((and (consp r)
         (or (numberp (car r)) (markerp (car r)))
         (or (numberp (cdr r)) (markerp (cdr r))))
    (cons (context-navigator-gptel--pos (car r))
          (context-navigator-gptel--pos (cdr r))))
   ;; List whose first two elements are positions
   ((and (listp r)
         (>= (length r) 2)
         (let ((a (nth 0 r)) (b (nth 1 r)))
           (and (or (numberp a) (markerp a))
                (or (numberp b) (markerp b)))))
    (let ((a (nth 0 r)) (b (nth 1 r)))
      (cons (context-navigator-gptel--pos a)
            (context-navigator-gptel--pos b))))
   (t nil)))

(defun context-navigator-gptel--entry->items-from-buffer (buf regs)
  (let* ((buf-live (and (bufferp buf) (buffer-live-p buf)))
         (buf-path (and buf-live (buffer-local-value 'buffer-file-name buf)))
         (pmin (and buf-live (with-current-buffer buf (point-min))))
         (pmax (and buf-live (with-current-buffer buf (point-max))))
         (items nil))
    (dolist (r (context-navigator-gptel--coerce-regs-to-list regs))
      (let ((nr (context-navigator-gptel--normalize-region r)))
        (cond
         ((eq nr :all)
          (push (context-navigator-gptel--make-buffer-item buf buf-path (and buf-live (buffer-name buf))) items))
         ((and (consp nr)
               (integerp (car nr)) (integerp (cdr nr)))
          (let ((s (car nr)) (e (cdr nr)))
            (if (and pmin pmax (= s pmin) (= e pmax))
                (push (context-navigator-gptel--make-buffer-item buf buf-path (and buf-live (buffer-name buf))) items)
              (push (context-navigator-gptel--make-selection-item
                     buf buf-path s e)
                    items))))
         (t
          (context-navigator-debug :debug :gptel
                                   "Unknown buffer region element: %S (buf=%s)"
                                   r (and buf-live (buffer-name buf)))))))
    (let* ((bufs (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'buffer)) items))
           (sels (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'selection)) items)))
      (context-navigator-debug :debug :gptel
                               "parsed buffer entry %s -> %s items (buffers=%s selections=%s)"
                               (and buf-live (buffer-name buf)) (length items) bufs sels))
    (nreverse items)))

(defun context-navigator-gptel--entry->item-from-plist (entry)
  (let* ((get (lambda (k) (context-navigator-gptel--plist-like-get entry k)))
         (type (or (funcall get :type) (funcall get 'type)))
         (path (or (funcall get :path) (funcall get 'path)))
         (beg  (or (funcall get :beg)  (funcall get 'beg)))
         (end  (or (funcall get :end)  (funcall get 'end)))
         (buf  (or (funcall get :buffer) (funcall get 'buffer)))
         (name (funcall get :name)))
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
                          (format "%s:%s-%s" (or (and (bufferp buf) (buffer-live-p buf) (buffer-name buf)) "<selection>") beg end)))))
           (context-navigator-gptel--make-selection-item buf buf-path beg end nm))))
      ((or 'buffer :buffer)
       (context-navigator-gptel--make-buffer-item buf path name))
      (_ (context-navigator-debug :debug :gptel "Unknown plist type in entry=%S" entry)
         nil))))

(defun context-navigator-gptel--entry->item-from-path (entry)
  (let ((path (if (stringp entry) entry (car entry))))
    (when (and (stringp path) (not (string-empty-p path)))
      (context-navigator-gptel--make-file-item path))))

(defun context-navigator-gptel--plist-like-entry-p (entry)
  (when (listp entry)
    (let ((lst entry) (found nil))
      (while (and (consp lst) (consp (cdr lst)) (not found))
        (let ((k (car lst)))
          (when (and (symbolp k) (memq k '(:type type)))
            (setq found t)))
        (setq lst (cddr lst)))
      found)))

(defun context-navigator-gptel--entry->item (entry)
  "Convert gptel ENTRY to one or more context-navigator-item objects.

Returns a single item, a list of items, or nil."
  (cond
   ;; Plist-like entry
   ((context-navigator-gptel--plist-like-entry-p entry)
    (context-navigator-gptel--entry->item-from-plist entry))
   ;; (BUFFER . REGS)
   ((and (consp entry) (bufferp (car entry)))
    (context-navigator-gptel--entry->items-from-buffer (car entry) (cdr entry)))
   ;; (PATH . PROPS) or bare PATH
   ((or (and (consp entry) (stringp (car entry)))
        (stringp entry))
    (context-navigator-gptel--entry->item-from-path entry))
   (t
    (context-navigator-debug :debug :gptel "Unknown entry shape: %S" entry)
    nil)))


(defvar context-navigator-gptel--pull-in-progress nil
  "Non-nil while `context-navigator-gptel-pull' is running (reentrancy guard).")

(defun context-navigator-gptel-pull ()
  "Return list<context-navigator-item> reflecting current gptel context.

Reentrancy is guarded to avoid feedback loops when variable watchers
publish :gptel-change and handlers try to pull immediately."
  (when context-navigator-gptel--pull-in-progress
    (context-navigator-debug :trace :gptel "pull: reentry — skipping")
    (cl-return-from context-navigator-gptel-pull nil))
  (let ((context-navigator-gptel--pull-in-progress t))
    (context-navigator-gptel--ensure-loaded)
    (let* ((raw (or (context-navigator-gptel--context-list)
                    (and (boundp 'gptel--context)
                         (ignore-errors (symbol-value 'gptel--context)))))
           (seq (cond
                 ((null raw) nil)
                 ((vectorp raw) (append raw nil))
                 ((and (listp raw)
                       (or (null raw)
                           (and (listp (car raw))
                                (or (plist-member (car raw) :type)
                                    (plist-member (car raw) 'type)))))
                  raw)
                 ((and (listp raw)
                       (or (plist-member raw :type)
                           (plist-member raw 'type)))
                  (list raw))
                 (t raw)))
           (flat
            (and seq
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
                        (context-navigator-debug :debug :gptel
                                                 "Ignored entry->item result for entry=%S -> %S"
                                                 entry it)
                        nil))))
                  seq)))
           (uniq (and flat (context-navigator-model-uniq flat))))
      (when (and (or (null uniq) (= (length uniq) 0))
                 (sequencep raw) (> (length raw) 0))
        (setq uniq (ignore-errors (context-navigator-gptel--raw-items-fallback))))
      (let* ((count (lambda (pred lst)
                      (cl-count-if pred lst)))
             (files (and uniq (funcall count (lambda (x) (eq (context-navigator-item-type x) 'file)) uniq)))
             (bufs  (and uniq (funcall count (lambda (x) (eq (context-navigator-item-type x) 'buffer)) uniq)))
             (sels  (and uniq (funcall count (lambda (x) (eq (context-navigator-item-type x) 'selection)) uniq))))
        (context-navigator-debug :debug :gptel
                                 "pull -> items total=%s files=%s buffers=%s selections=%s"
                                 (length (or uniq '())) (or files 0) (or bufs 0) (or sels 0)))
      uniq)))

(defun context-navigator-gptel--can-add-file () (fboundp 'gptel-context-add-file))
(defun context-navigator-gptel--can-add-region () (fboundp 'gptel-context--add-region))
(defun context-navigator-gptel--can-remove () (fboundp 'gptel-context-remove))
(defun context-navigator-gptel--can-remove-all () (fboundp 'gptel-context-remove-all))

(defun context-navigator-gptel-clear-all-now ()
  "Best-effort clear of gptel context (silences echo and *Messages*)."
  (when (context-navigator-gptel--can-remove-all)
    (let ((inhibit-message t) (message-log-max nil))
      (ignore-errors (gptel-context-remove-all)))))

(defun context-navigator-gptel--add-item (item)
  "Try to add ITEM to gptel context. Return t on success."
  (context-navigator-gptel--ensure-loaded)
  (pcase (context-navigator-item-type item)
    ('file
     (when (context-navigator-gptel--can-add-file)
       (let* ((p0 (context-navigator-item-path item))
              (root (and (fboundp 'context-navigator--state-get)
                         (let* ((st (ignore-errors (context-navigator--state-get))))
                           (and st (ignore-errors (context-navigator-state-last-project-root st))))))
              (p (cond
                  ((and (stringp p0) (file-name-absolute-p p0)) (expand-file-name p0))
                  ((and (stringp p0) (stringp root))
                   (expand-file-name p0 (file-name-as-directory (expand-file-name root))))
                  (t p0))))
         (when (and (stringp p) (file-readable-p p))
           (context-navigator--silence-messages (gptel-context-add-file p))
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
           (context-navigator--silence-messages (gptel-context--add-region buf b e))
           t))))
    ('buffer
     (let* ((p (context-navigator-item-path item))
            (buf (or (context-navigator-item-buffer item)
                     (and (stringp p) (file-exists-p p)
                          (find-file-noselect p)))))
       (cond
        ;; Prefer region add for whole buffer so indicators match 'buffer' keys
        ((and (context-navigator-gptel--can-add-region)
              (bufferp buf) (buffer-live-p buf))
         (with-current-buffer buf
           (let ((b (point-min)) (e (point-max)))
             (context-navigator--silence-messages (gptel-context--add-region buf b e))
             t)))
        ;; Fallback: add as file (indicators will reflect a file key)
        ((and (context-navigator-gptel--can-add-file)
              (stringp p) (file-readable-p p))
         (context-navigator--silence-messages (gptel-context-add-file p))
         t)
        (t nil))))
    (_ nil)))

(defun context-navigator-gptel--reset-to (items)
  "Replace gptel context with ITEMS using remove-all + add.
Only enabled items are (re)added.

This function attempts a best-effort remove-all when supported by the
underlying gptel; it publishes a :gptel-change :reset event so listeners
(e.g. the core sync) can react. We keep the call guarded and tolerate
errors from different gptel versions."
  (context-navigator-gptel--ensure-loaded)
  (when (context-navigator-gptel--can-remove-all)
    (ignore-errors (context-navigator--silence-messages (gptel-context-remove-all))))
  (let ((count 0))
    (dolist (it items)
      (when (context-navigator-item-enabled it)
        (setq count (+ count (if (context-navigator-gptel--add-item it) 1 0)))))
    ;; publish a clear reset event so consumers can schedule a sync/update
    (context-navigator-events-publish :gptel-change :reset count)
    (list :method 'reset :count count)))

;; Internal helpers extracted from the original `context-navigator-gptel-apply'

(defun context-navigator-gptel--selection-in-diff-p (rems upds)
  "Return non-nil if selections are present in REMS or UPDS."
  (or (cl-some (lambda (x) (eq (context-navigator-item-type x) 'selection)) rems)
      (cl-some (lambda (x) (eq (context-navigator-item-type x) 'selection)) upds)))

(defun context-navigator-gptel--needs-reset-p (rems upds)
  "Return non-nil if we should fallback to reset for this diff.

Reset when:
- precise remove/add is unsupported;
- selections participate in diff;
- buffers participate in diff (gptel cannot reliably remove buffer contexts by buffer)."
  (or (not (context-navigator-gptel--can-remove))
      (not (context-navigator-gptel--can-add-file))
      (context-navigator-gptel--selection-in-diff-p rems upds)
      ;; Any buffer item in removals/updates → do full reset to ensure correctness.
      (cl-some (lambda (x) (eq (context-navigator-item-type x) 'buffer)) rems)
      (cl-some (lambda (x) (eq (context-navigator-item-type x) 'buffer)) upds)))

(defun context-navigator-gptel--remove-one (item)
  "Best-effort removal of ITEM from gptel. Return t on success."
  (context-navigator-gptel--ensure-loaded)
  (when (context-navigator-gptel--can-remove)
    (pcase (context-navigator-item-type item)
      ('file
       (ignore-errors
         (context-navigator--silence-messages
           (gptel-context-remove (context-navigator-item-path item))))
       t)
      ('buffer
       (let ((buf  (context-navigator-item-buffer item))
             (path (context-navigator-item-path item))
             (name (context-navigator-item-name item)))
         (cond
          ((and buf (buffer-live-p buf))
           (ignore-errors (context-navigator--silence-messages (gptel-context-remove buf))) t)
          ((stringp path)
           (ignore-errors (context-navigator--silence-messages (gptel-context-remove path))) t)
          ((stringp name)
           (ignore-errors (context-navigator--silence-messages (gptel-context-remove name))) t)
          (t nil))))
      (_ nil))))

(defun context-navigator-gptel--remove-many (items)
  "Remove multiple ITEMS, returning count of successful ops."
  (let ((ops 0))
    (dolist (it items)
      (when (context-navigator-gptel--remove-one it)
        (setq ops (1+ ops))))
    ops))

(defun context-navigator-gptel--update-one (item)
  "Update ITEM by removing existing and re-adding when enabled.
Return count of successful operations."
  (let ((ops 0))
    (pcase (context-navigator-item-type item)
      ((or 'file 'buffer)
       (when (context-navigator-gptel--remove-one item)
         (setq ops (1+ ops)))))
    (when (context-navigator-item-enabled item)
      (when (context-navigator-gptel--add-item item)
        (setq ops (1+ ops))))
    ops))

(defun context-navigator-gptel--update-many (items)
  "Update multiple ITEMS; return total ops."
  (let ((ops 0))
    (dolist (it items)
      (setq ops (+ ops (context-navigator-gptel--update-one it))))
    ops))

(defun context-navigator-gptel--add-enabled-many (items)
  "Add enabled ITEMS; return count of successful adds."
  (let ((ops 0))
    (dolist (it items)
      (when (and (context-navigator-item-enabled it)
                 (context-navigator-gptel--add-item it))
        (setq ops (1+ ops))))
    ops))

(defun context-navigator-gptel--apply-diff (adds rems upds)
  "Apply fine-grained diff to gptel. Return a plist result."
  (let ((ops 0))
    (setq ops (+ ops (context-navigator-gptel--remove-many rems)))
    (setq ops (+ ops (context-navigator-gptel--update-many upds)))
    (setq ops (+ ops (context-navigator-gptel--add-enabled-many adds)))
    (context-navigator-events-publish :gptel-change :diff ops)
    (list :applied t :method 'diff :ops ops)))

(cl-defun context-navigator-gptel-apply (desired-items)
  "Apply DESIRED-ITEMS to gptel. Returns a plist describing action.
Когда точечные операции невозможны, выполняется сброс (reset)."
  (unless (context-navigator-gptel-available-p)
    (cl-return-from context-navigator-gptel-apply (list :applied nil :reason 'unavailable)))
  ;; Detect converter failure: if pull is empty but raw gptel has entries, do reset.
  (let* ((current (context-navigator-gptel-pull))
         (raw     (context-navigator-gptel--context-list)))
    (when (and (or (null current) (= (length current) 0))
               (sequencep raw) (> (length raw) 0))
      (let ((r (context-navigator-gptel--reset-to desired-items)))
        (cl-return-from context-navigator-gptel-apply (append (list :applied t) r)))))
  (let* ((current (context-navigator-gptel-pull))
         (diff    (context-navigator-model-diff current desired-items))
         (adds    (plist-get diff :add))
         (rems    (plist-get diff :remove))
         (upds    (plist-get diff :update)))
    (if (context-navigator-gptel--needs-reset-p rems upds)
        (let ((r (context-navigator-gptel--reset-to desired-items)))
          (append (list :applied t) r))
      (context-navigator-gptel--apply-diff adds rems upds))))

(defun context-navigator-gptel-present-p (item)
  "Return non-nil if ITEM is currently present in gptel context.

Tries normal pull; if not found (especially for selections), falls back to
reading raw gptel entries and parsing them directly (including a last-resort
`gptel-context--collect' when available)."
  (let* ((lst (ignore-errors (context-navigator-gptel-pull)))
         (keys (and (listp lst)
                    (mapcar #'context-navigator-model-item-key lst)))
         (key (context-navigator-model-item-key item)))
    (or (and (member key keys) t)
        ;; Fallback for selections and odd cases when `pull' misses entries.
        (let* ((type (ignore-errors (context-navigator-item-type item)))
               (raw (or (ignore-errors (context-navigator-gptel--context-list))
                        (and (fboundp 'gptel-context--collect)
                             (ignore-errors (gptel-context--collect)))))
               (flat (and (sequencep raw)
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
                                (t nil))))
                           raw)))
               (uniq (and flat (context-navigator-model-uniq flat))))
          (cond
           ;; For selections, prefer value equality by type/path/beg/end, ignore name/size/enabled
           ((eq type 'selection)
            (or (and uniq
                     (cl-some
                      (lambda (x)
                        (and (eq (context-navigator-item-type x) 'selection)
                             (equal (context-navigator-item-path x)
                                    (context-navigator-item-path item))
                             (equal (context-navigator-item-beg x)
                                    (context-navigator-item-beg item))
                             (equal (context-navigator-item-end x)
                                    (context-navigator-item-end item))))
                      uniq))
                ;; As an extra guard, if keys also match, treat as present
                (and uniq (member key (mapcar #'context-navigator-model-item-key uniq)))))
           ;; For other types, key match is sufficient
           (t
            (and uniq (member key (mapcar #'context-navigator-model-item-key uniq)))))))))

(cl-defun context-navigator-gptel-add-one (item)
  "Add ITEM to gptel context (best-effort). Return t on success."
  (unless (context-navigator-gptel-available-p)
    (cl-return-from context-navigator-gptel-add-one nil))
  (let ((ok (ignore-errors (context-navigator-gptel--add-item item))))
    (when ok
      (context-navigator-events-publish :gptel-change :add 1))
    ok))

(cl-defun context-navigator-gptel-remove-one (item)
  "Remove ITEM from gptel context (best-effort). Return t on success.
Selections may not support precise removal; fallback to reset without the item."
  (unless (context-navigator-gptel-available-p)
    (cl-return-from context-navigator-gptel-remove-one nil))
  (let* ((key (context-navigator-model-item-key item))
         (fallback-reset
          (lambda ()
            (let* ((cur (ignore-errors (context-navigator-gptel-pull)))
                   (keep (cl-remove-if (lambda (it)
                                         (string= (context-navigator-model-item-key it) key))
                                       (or cur '()))))
              (ignore-errors (context-navigator-gptel--reset-to keep))
              t))))
    (pcase (context-navigator-item-type item)
      ('selection
       (funcall fallback-reset))
      ('file
       (if (context-navigator-gptel--can-remove)
           (progn
             (ignore-errors (context-navigator--silence-messages (gptel-context-remove (context-navigator-item-path item))))
             (context-navigator-events-publish :gptel-change :remove 1)
             t)
         (funcall fallback-reset)))
      ('buffer
       (if (context-navigator-gptel--can-remove)
           (let* ((buf (context-navigator-item-buffer item))
                  (p (context-navigator-item-path item))
                  (name (context-navigator-item-name item)))
             (cond
              ((and buf (buffer-live-p buf))
               (ignore-errors (context-navigator--silence-messages (gptel-context-remove buf))))
              ((and (stringp p))
               (ignore-errors (context-navigator--silence-messages (gptel-context-remove p))))
              ((and (stringp name))
               (ignore-errors (context-navigator--silence-messages (gptel-context-remove name)))))
             (context-navigator-events-publish :gptel-change :remove 1)
             t)
         (funcall fallback-reset)))
      (_ (funcall fallback-reset)))))

(defun context-navigator-gptel-toggle-one (item)
  "Toggle ITEM membership in gptel. Return :added, :removed or :noop."
  (cond
   ((context-navigator-gptel-present-p item)
    (if (context-navigator-gptel-remove-one item) :removed :noop))
   (t
    (if (context-navigator-gptel-add-one item) :added :noop))))

(defun context-navigator-gptel--add-var-watcher (sym)
  "Install a variable watcher on SYM to publish :gptel-change. Return watcher fn or nil."
  (when (and (boundp sym)
             (fboundp 'add-variable-watcher))
    ;; Avoid duplicate variable watchers.
    (let ((existing (assoc sym context-navigator--gptel-var-watchers)))
      (if existing
          (cdr existing)
        (let ((fn (lambda (&rest _)
                    (context-navigator-events-publish :gptel-change sym))))
          (add-variable-watcher sym fn)
          fn)))))

(defun context-navigator-gptel--advise (sym)
  "Add an :after advice to SYM that publishes :gptel-change. Return advice fn or nil."
  (when (fboundp sym)
    ;; Avoid duplicate advices for the same symbol.
    (let ((existing (assoc sym context-navigator--gptel-advices)))
      (if existing
          (cdr existing)
        (let ((fn (lambda (&rest _)
                    (context-navigator-events-publish :gptel-change sym))))
          (advice-add sym :after fn)
          fn)))))

(defun context-navigator-gptel-on-change-register ()
  "Install lightweight advices to publish :gptel-change (no variable watchers).

Rationale: variable watchers on gptel internals (like `gptel-context--alist')
tend to produce a feedback loop when any reader calls `gptel-context--collect',
because the variable is rewritten even when content is unchanged.
That triggers :gptel-change, which triggers another read, and so on.

We keep only function advices on mutation entry points."
  ;; Ensure we start clean (remove previously installed advices/watchers).
  (ignore-errors (context-navigator-gptel-on-change-unregister))
  (let (added)
    ;; Advices on common mutation functions (idempotent)
    (dolist (sym '(gptel-context-add
                   gptel-context-add-file
                   gptel-context--add-region
                   gptel-context-remove
                   gptel-context-remove-all))
      (when (and (fboundp sym)
                 (not (assoc sym context-navigator--gptel-advices)))
        (let ((fn (context-navigator-gptel--advise sym)))
          (when fn
            (push (cons sym fn) context-navigator--gptel-advices)
            (setq added t)))))
    added))

(defun context-navigator-gptel-on-change-unregister ()
  "Remove previously installed advices and variable watchers for :gptel-change."
  ;; Advices
  (dolist (cell context-navigator--gptel-advices)
    (condition-case _err
        (advice-remove (car cell) (cdr cell))
      (error nil)))
  (setq context-navigator--gptel-advices nil)
  ;; Variable watchers
  (when (fboundp 'remove-variable-watcher)
    (dolist (cell context-navigator--gptel-var-watchers)
      (condition-case _err
          (remove-variable-watcher (car cell) (cdr cell))
        (error nil))))
  (setq context-navigator--gptel-var-watchers nil)
  t)

;; If gptel is installed/loaded after Context Navigator, ensure we register
;; our lightweight advices to keep UI indicators up-to-date. Do not force-load
;; gptel here — remain functional when gptel is absent.
(with-eval-after-load 'gptel
  (context-navigator-debug :info :gptel "gptel loaded — registering change advices")
  (when (fboundp 'context-navigator-gptel-on-change-register)
    ;; Ensure old watchers (if any) are removed before installing advices.
    (ignore-errors (context-navigator-gptel-on-change-unregister))
    (ignore-errors (context-navigator-gptel-on-change-register)))
  ;; Notify listeners (sidebar/UI) that gptel state might have changed so they can refresh.
  (when (fboundp 'context-navigator-events-publish)
    (ignore-errors (context-navigator-events-publish :gptel-change :on-load))))

(provide 'context-navigator-gptel-bridge)
;;; context-navigator-gptel-bridge.el ends here
