;;; context-navigator-transient-build.el --- Build transient specs from keyspec -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Middle-path builder for transient menus from the centralized keyspec.
;; - Produces grouped specs for Navigator View transient (items/groups)
;; - Uses :section and :desc-key metadata from keyspec for labels
;; - Picks the first key from :keys for each action (alias keys are not duplicated)
;;
;; Integration approach (recommended in steps):
;; 1) Keep existing transient definitions
;; 2) Replace static blocks gradually with results of these builders
;; 3) Keep dynamic labels where needed (counts/time), use keyspec only for keys/commands

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-keyspec)
(require 'context-navigator-i18n)

(defgroup context-navigator-transient-build nil
  "Transient builders from keyspec for Context Navigator."
  :group 'context-navigator)

(defun context-navigator-transient-build--log (fmt &rest args)
  "Log transient build diagnostics with info level and echo."
  (ignore-errors
    (when (fboundp 'context-navigator-debug)
      (apply #'context-navigator-debug :info :transient fmt args)))
  (ignore-errors
    (apply #'message (concat "[context-navigator] " fmt) args)))

(defun context-navigator-transient-build--title (sec-symbol)
  "Return localized section title for SEC-SYMBOL."
  (pcase sec-symbol
    ('navigate (context-navigator-i18n :tr-navigate))
    ('act      (context-navigator-i18n :tr-actions))
    ('groups   (context-navigator-i18n :tr-groups))
    ('session  (context-navigator-i18n :tr-session))
    (_         "Tools")))

(defun context-navigator-transient-build--entries (context sections)
  "Return list of (key label . cmd) triples for CONTEXT filtered by SECTIONS.
Only the first key from :keys is used per keyspec entry."
  (let* ((entries (cl-remove-if-not
                   (lambda (pl)
                     (and (memq context (plist-get pl :contexts))
                          (memq (or (plist-get pl :section) 'act) sections)))
                   context-navigator-keyspec))
         (triples '()))
    (dolist (pl entries (nreverse triples))
      (let* ((cmd  (plist-get pl :cmd))
             (keys (plist-get pl :keys))
             (id   (plist-get pl :id))
             (desc
              ;; Dynamic labels for session toggles; otherwise use i18n desc-key/name
              (pcase id
                ('push-toggle
                 (format (context-navigator-i18n :push-state)
                         (context-navigator-i18n
                          (if (and (boundp 'context-navigator--push-to-gptel)
                                   context-navigator--push-to-gptel) :on :off))))
                ('auto-toggle
                 (format (context-navigator-i18n :auto-project-state)
                         (context-navigator-i18n
                          (if (and (boundp 'context-navigator--auto-project-switch)
                                   context-navigator--auto-project-switch) :on :off))))
                (_
                 (let ((k (plist-get pl :desc-key)))
                   (if (and k (fboundp 'context-navigator-i18n))
                       (context-navigator-i18n k)
                     (symbol-name (or id cmd))))))))
        (when (and (symbolp cmd) (fboundp cmd)
                   (listp keys) (> (length keys) 0))
          (let* ((k (car keys)))
            (push (list k desc cmd) triples)))))))

(defun context-navigator-transient-build--group (context sections &optional predicate)
  "Build a single transient group spec for CONTEXT over SECTIONS.
When PREDICATE is non-nil, add it as a :if guard for group visibility.
Returns either nil (when empty) or a raw group vector:
  [\"Title\" :if PRED (\"k\" \"Label\" CMD) ...]
This is suitable input for `transient-parse-suffixes'."
  (let* ((entries (context-navigator-transient-build--entries context sections)))
    (when entries
      (let* ((title (context-navigator-transient-build--title (car sections)))
             (heads
              (mapcar (lambda (cell)
                        (let ((k (nth 0 cell))
                              (lbl (nth 1 cell))
                              (cmd (nth 2 cell)))
                          (list k lbl cmd)))
                      entries))
             (elems (append (list title)
                            (and predicate (list :if predicate))
                            heads)))
        (apply #'vector elems)))))

(defun context-navigator-transient-build--group-excluding (context sections exclude-ids &optional predicate)
  "Build a transient group for CONTEXT limited to SECTIONS, excluding EXCLUDE-IDS.
EXCLUDE-IDS is a list of keyspec :id symbols to omit.
When PREDICATE is non-nil, add it as a :if guard for group visibility.
Returns a raw group vector suitable for `transient-parse-suffixes'."
  (let* ((entries
          (cl-remove-if-not
           (lambda (pl)
             (and (memq context (plist-get pl :contexts))
                  (memq (or (plist-get pl :section) 'act) sections)
                  (not (memq (plist-get pl :id) exclude-ids))))
           context-navigator-keyspec)))
    (when entries
      (let* ((title (context-navigator-transient-build--title (car sections)))
             (heads
              (cl-loop for pl in entries
                       for cmd  = (plist-get pl :cmd)
                       for keys = (plist-get pl :keys)
                       for id   = (plist-get pl :id)
                       for lbl  = (let ((k (plist-get pl :desc-key)))
                                    (if (and k (fboundp 'context-navigator-i18n))
                                        (context-navigator-i18n k)
                                      (symbol-name (or id cmd))))
                       when (and (symbolp cmd) (fboundp cmd)
                                 (listp keys) (> (length keys) 0))
                       collect (list (car keys) lbl cmd)))
             (elems (append (list title)
                            (and predicate (list :if predicate))
                            heads)))
        (apply #'vector elems)))))

(defun context-navigator-transient-build-view-items ()
  "Return a list of raw group vectors for the View (items mode).
Groups: Navigate / Actions / Session / Tools."
  (let* ((g1 (context-navigator-transient-build--group 'items   '(navigate)))
         (g2 (context-navigator-transient-build--group 'items   '(act)))
         (g3 (context-navigator-transient-build--group 'items   '(session)))
         (g4 (context-navigator-transient-build--group 'items   '(tools)))
         (lst (delq nil (list g1 g2 g3 g4))))
    (context-navigator-transient-build--log
     "build-view-items: groups=%s (navigate=%s act=%s session=%s tools=%s)"
     (length lst) (and g1 t) (and g2 t) (and g3 t) (and g4 t))
    lst))

(defun context-navigator-transient-build-view-groups ()
  "Return a list of raw group vectors for the View (groups mode).
Groups: Navigate / Groups / Session / Tools."
  (let* ((g1 (context-navigator-transient-build--group 'groups  '(navigate)))
         (g2 (context-navigator-transient-build--group 'groups  '(groups)))
         (g3 (context-navigator-transient-build--group 'groups  '(session)))
         (g4 (context-navigator-transient-build--group 'groups  '(tools)))
         (lst (delq nil (list g1 g2 g3 g4))))
    (context-navigator-transient-build--log
     "build-view-groups: groups=%s (navigate=%s groups=%s session=%s tools=%s)"
     (length lst) (and g1 t) (and g2 t) (and g3 t) (and g4 t))
    lst))

(defun context-navigator-transient-build-groups-split ()
  "Return a list of raw group vectors for the Groups split transient.
Groups: Navigate / Actions / Tools."
  (let* ((g1 (context-navigator-transient-build--group 'groups-split '(navigate)))
         (g2 (context-navigator-transient-build--group 'groups-split '(act)))
         (g3 (context-navigator-transient-build--group 'groups-split '(tools)))
         (lst (delq nil (list g1 g2 g3))))
    (context-navigator-transient-build--log
     "build-groups-split: groups=%s (navigate=%s act=%s tools=%s)"
     (length lst) (and g1 t) (and g2 t) (and g3 t))
    lst))

;; -------- Global transient (Panel/Project, Context/Groups, Actions, Control, Logs)

(defvar context-navigator-transient-global-spec nil
  "Obsolete. Global transient spec is generated from keyspec now.
See `context-navigator-transient-build-global'.")
(put 'context-navigator-transient-global-spec 'obsolete-variable
     "Use keyspec + context-navigator-transient-build-global instead.")

(defun context-navigator-transient-build--title-global (sec)
  "Obsolete. Use `context-navigator-transient-build--title' instead."
  (context-navigator-transient-build--title sec))

(defun context-navigator-transient-build-global ()
  "Build global transient raw groups from keyspec (:contexts contains 'global).
Return a list of group vectors suitable for `transient-parse-suffixes'."
  (let* ((sections '(navigate act session tools))
         (groups '()))
    (dolist (sec sections)
      (let ((g (context-navigator-transient-build--group 'global (list sec))))
        (when g (push g groups))))
    (context-navigator-transient-build--log
     "build-global(from keyspec): groups=%s" (length groups))
    (nreverse groups)))

(provide 'context-navigator-transient-build)
;;; context-navigator-transient-build.el ends here
