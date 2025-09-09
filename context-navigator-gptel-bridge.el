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

(defun context-navigator-gptel-available-p ()
  "Return non-nil when gptel context functions are available."
  (or (boundp 'gptel-context)
      (fboundp 'gptel-context-add)
      (fboundp 'gptel-context-add-file)
      (fboundp 'gptel-context--add-region)))

(defun context-navigator-gptel--context-list ()
  "Return raw gptel context entries or nil if unavailable.

Try several sources in a robust order. Prefer dynamic variables used by tests,
then public API, then internal variables/collectors. Return the first non-empty
sequence; if all are empty, return the first sequence (possibly empty)."
  (let* ((candidates
          (delq nil
                (list
                 ;; Prefer plain variable used in tests and some gptel versions
                 (and (boundp 'gptel--context)
                      (ignore-errors (symbol-value 'gptel--context)))
                 ;; Then public variable
                 (and (boundp 'gptel-context)
                      (ignore-errors (symbol-value 'gptel-context)))
                 ;; Public API
                 (and (fboundp 'gptel-context-list)
                      (ignore-errors (gptel-context-list)))
                 ;; Internal shapes seen in the wild
                 (and (boundp 'gptel-context--alist)
                      (ignore-errors (symbol-value 'gptel-context--alist)))
                 (and (fboundp 'gptel-context--collect)
                      (ignore-errors (gptel-context--collect))))))
         (nonempty (cl-find-if (lambda (v)
                                 (and (sequencep v)
                                      (> (length v) 0)))
                               candidates))
         (any-seq (or nonempty
                      (cl-find-if (lambda (v) (sequencep v)) candidates))))
    any-seq))

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
  (let ((_dbg (bound-and-true-p context-navigator-debug)))
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
            :name (or name
                      (and (stringp path) (file-name-nondirectory path))
                      (and (bufferp buf) (buffer-name buf))
                      "<buffer>")
            :path path
            :buffer buf
            :enabled t))
          (_ (context-navigator-debug :debug :gptel "Unknown plist type in entry=%S" entry)
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
            (context-navigator-debug :debug :gptel
                                     "Unknown buffer region element: %S (buf=%s)"
                                     r (buffer-name buf))))))
      (let* ((bufs (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'buffer)) items))
             (sels (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'selection)) items)))
        (context-navigator-debug :debug :gptel
                                 "parsed buffer entry %s -> %s items (buffers=%s selections=%s)"
                                 (buffer-name buf) (length items) bufs sels))
      (nreverse items)))
    ;; (PATH . PROPS) or bare \"path\" string
    ((or (and (consp entry) (stringp (car entry)))
         (stringp entry))
     (let ((path (if (stringp entry) entry (car entry))))
       (when (and (stringp path) (file-exists-p path))
         (context-navigator-item-create
          :type 'file :name (file-name-nondirectory path) :path path :enabled t))))
    (t
     (context-navigator-debug :debug :gptel "Unknown entry shape: %S" entry)
     nil)))

(defun context-navigator-gptel-pull ()
  "Return list<context-navigator-item> reflecting current gptel context.

This function is tolerant: it flattens nested results from the
entry->item converter and returns a unique list of items.

Robustness improvements:
- Accept a single item, a list/vector of items, or mixed lists and extract valid items.
- When `context-navigator-debug' is non-nil, log unexpected conversions."
  (let* ((raw (or (context-navigator-gptel--context-list)
                  ;; Fallback for tests/older gptel shapes using a plain variable
                  (and (boundp 'gptel--context)
                       (ignore-errors (symbol-value 'gptel--context)))))
         ;; Normalize RAW to a proper sequence of entries:
         ;; - vector -> list
         ;; - single plist entry -> wrap into a list
         ;; - list of entries -> keep as-is
         (seq (cond
               ((null raw) nil)
               ((vectorp raw) (append raw nil))
               ;; already a list of entries (heuristic: first element is a plist/alist)
               ((and (listp raw)
                     (or (null raw)
                         (and (listp (car raw))
                              (or (plist-member (car raw) :type)
                                  (plist-member (car raw) 'type)))))
                raw)
               ;; single plist-like entry
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
    (let* ((count (lambda (pred lst)
                    (cl-count-if pred lst)))
           (files (and uniq (funcall count (lambda (x) (eq (context-navigator-item-type x) 'file)) uniq)))
           (bufs  (and uniq (funcall count (lambda (x) (eq (context-navigator-item-type x) 'buffer)) uniq)))
           (sels  (and uniq (funcall count (lambda (x) (eq (context-navigator-item-type x) 'selection)) uniq))))
      (context-navigator-debug :debug :gptel
                               "pull -> items total=%s files=%s buffers=%s selections=%s"
                               (length (or uniq '())) (or files 0) (or bufs 0) (or sels 0)))
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
  "Return non-nil if we should fallback to reset for this diff."
  (or (not (context-navigator-gptel--can-remove))
      (not (context-navigator-gptel--can-add-file))
      (context-navigator-gptel--selection-in-diff-p rems upds)))

(defun context-navigator-gptel--remove-one (item)
  "Best-effort removal of ITEM from gptel. Return t on success."
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
  "Return non-nil if ITEM is currently present in gptel context."
  (let* ((lst (ignore-errors (context-navigator-gptel-pull)))
         (keys (and (listp lst)
                    (mapcar #'context-navigator-model-item-key lst)))
         (key (context-navigator-model-item-key item)))
    (and (member key keys) t)))

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
    (let ((fn (lambda (&rest _)
                (context-navigator-events-publish :gptel-change sym))))
      (add-variable-watcher sym fn)
      fn)))

(defun context-navigator-gptel--advise (sym)
  "Add an :after advice to SYM that publishes :gptel-change. Return advice fn or nil."
  (when (fboundp sym)
    (let ((fn (lambda (&rest _)
                (context-navigator-events-publish :gptel-change sym))))
      (advice-add sym :after fn)
      fn)))

(defun context-navigator-gptel-on-change-register ()
  "Install advices and variable watchers to publish :gptel-change.

Used ONLY to keep UI indicators up-to-date; never imports from gptel."
  (let (added)
    ;; Advices on common mutation functions
    (dolist (sym '(gptel-context-add
                   gptel-context-add-file
                   gptel-context--add-region
                   gptel-context-remove
                   gptel-context-remove-all))
      (let ((fn (context-navigator-gptel--advise sym)))
        (when fn
          (push (cons sym fn) context-navigator--gptel-advices)
          (setq added t))))
    ;; Variable watchers (Emacs 29+) for versions mutating variables directly
    (when (fboundp 'add-variable-watcher)
      (dolist (vsym '(gptel-context gptel-context--alist gptel--context))
        (let ((fn (context-navigator-gptel--add-var-watcher vsym)))
          (when fn
            (push (cons vsym fn) context-navigator--gptel-var-watchers)
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
    (ignore-errors (context-navigator-gptel-on-change-register)))
  ;; Notify listeners (sidebar/UI) that gptel state might have changed so they can refresh.
  (when (fboundp 'context-navigator-events-publish)
    (ignore-errors (context-navigator-events-publish :gptel-change :on-load))))

(provide 'context-navigator-gptel-bridge)
;;; context-navigator-gptel-bridge.el ends here
