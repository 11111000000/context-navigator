;;; context-navigator-view-controls.el --- Controls (toolbar) for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Build header-line/inline toolbar controls (toggles + actions) for the Navigator view.
;; Split out of context-navigator-view.el to reduce coupling.
;;
;; This module does not require the full view to avoid cycles; it declares the
;; helpers it uses from the view. The view should (require 'context-navigator-view-controls)
;; to expose the public controls API.
;;
;; Refactoring note:
;; - Controls are now described declaratively in a registry: logic + appearance
;; - Order/visibility is configured separately for header-line and inline toolbars
;; - Rendering is unified and consumes registry + order

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-i18n)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-controls-icons)

;; Declarations to avoid load cycles; provided by context-navigator-view or core.
(declare-function context-navigator-view-open-all-buffers "context-navigator-view" ())
(declare-function context-navigator-view-close-all-buffers "context-navigator-view" ())
(declare-function context-navigator-view-clear-group "context-navigator-view" ())
(declare-function context-navigator-view-push-now "context-navigator-view" ())
(declare-function context-navigator-view-toggle-all-gptel "context-navigator-view" ())
(declare-function context-navigator-view-toggle-push "context-navigator-view" ())
(declare-function context-navigator-view-toggle-auto-project "context-navigator-view" ())
(declare-function context-navigator-undo "context-navigator-core" ())
(declare-function context-navigator-redo "context-navigator-core" ())
(declare-function context-navigator-view--wrap-segments "context-navigator-view" (segments total-width))
(declare-function context-navigator-view-razor-run "context-navigator-view" ())

(defgroup context-navigator-view-controls nil
  "Toolbar controls (toggles and actions) for Context Navigator view."
  :group 'context-navigator)

;; Layout: order of controls for header-line and inline toolbars.
(defcustom context-navigator-headerline-controls-order
  '(auto-project push push-now undo redo toggle-all-gptel
                 razor open-buffers close-buffers clear-group)
  "Controls order for the header-line toolbar.
Remove a key to hide the control. You may also insert :gap for spacing."
  :type '(repeat (choice symbol (const :gap)))
  :group 'context-navigator-view-controls)

(defcustom context-navigator-inline-controls-order
  '(push auto-project undo redo
         razor push-now open-buffers close-buffers clear-group toggle-all-gptel)
  "Controls order for the inline (wrapped) toolbar.
Remove a key to hide the control. You may also insert :gap for spacing."
  :type '(repeat (choice symbol (const :gap)))
  :group 'context-navigator-view-controls)

;; Registry: declarative descriptors for each control (logic + appearance).
;; Each entry is (KEY . plist), where plist supports:
;;  :type       'toggle | 'action
;;  :icon-key   symbol passed to context-navigator-controls-icon
;;  :command    function symbol to call on click (mouse-1)
;;  :help       string or (lambda () string)
;;  :enabled-p  (lambda () boolean)
;;  :visible-p  (lambda () boolean)
;;  :state-fn   (lambda () 'on | 'off)      ; only for :type 'toggle
;;  :spinner-fn (lambda () string-or-nil)   ; optional spinner
;;  :label-fn   (lambda (style state) string) ; fallback label when no icon/spinner
;;  :face-fn    (lambda (style state) face-or-plist) ; optional face for text fallback
(defcustom context-navigator-controls-registry
  (let ((tr #'context-navigator-i18n))
    `(
      (push
       :type toggle
       :icon-key push
       :command context-navigator-view-toggle-push
       :help ,(lambda () (funcall tr :toggle-push))
       :enabled-p ,(lambda ()
                     (ignore-errors (context-navigator-gptel-available-p)))
       :visible-p ,(lambda () t)
       :state-fn ,(lambda ()
                    (if (and (boundp 'context-navigator--push-to-gptel)
                             context-navigator--push-to-gptel)
                        'on 'off))
       :label-fn ,(lambda (style state)
                    (pcase style
                      ((or 'icons 'auto) " [→]")
                      (_ (format " [→gptel: %s]" (if (eq state 'on) "on" "off"))))))
      (auto-project
       :type toggle
       :icon-key auto-project
       :command context-navigator-view-toggle-auto-project
       :help ,(lambda () (funcall tr :toggle-auto))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :state-fn ,(lambda ()
                    (if (and (boundp 'context-navigator--auto-project-switch)
                             context-navigator--auto-project-switch)
                        'on 'off))
       :label-fn ,(lambda (style state)
                    (pcase style
                      ((or 'icons 'auto) " [A]")
                      (_ (format " [%s: %s]" (funcall tr :auto-proj)
                                 (if (eq state 'on) "on" "off")))))
       :face-fn ,(lambda (_style state)
                   ;; Only used when no graphic icons are available.
                   (list :foreground (if (eq state 'on) "green4" "gray"))))
      (undo
       :type action
       :icon-key undo
       :command context-navigator-undo
       :help ,(lambda () (funcall tr :razor-undo))
       :enabled-p ,(lambda () (fboundp 'context-navigator-undo))
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _state)
                    (pcase style
                      ((or 'icons 'auto) " [↶]")
                      (_ (concat " " (funcall tr :razor-undo) "")))))
      (redo
       :type action
       :icon-key redo
       :command context-navigator-redo
       :help ,(lambda () (funcall tr :razor-redo))
       :enabled-p ,(lambda () (fboundp 'context-navigator-redo))
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (pcase style
                      ((or 'icons 'auto) " [↷]")
                      (_ (concat " " (funcall tr :razor-redo) "")))))
      (razor
       :type action
       :icon-key razor
       :command context-navigator-view-razor-run
       :help ,(lambda () (funcall tr :tr-razor))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :spinner-fn ,(lambda ()
                      (and (boundp 'context-navigator-razor--running)
                           context-navigator-razor--running
                           (fboundp 'context-navigator-razor-spinner-frame)
                           (context-navigator-razor-spinner-frame)))
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (funcall tr :tr-razor))
                      " [R]")))
      (push-now
       :type action
       :icon-key push-now
       :command context-navigator-view-push-now
       :help ,(lambda () (funcall tr :push-now))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (funcall tr :push-now))) " [P]")))
      (open-buffers
       :type action
       :icon-key open-buffers
       :command context-navigator-view-open-all-buffers
       :help ,(lambda () (funcall tr :open-buffers))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (funcall tr :open-buffers))) " [O]")))
      (close-buffers
       :type action
       :icon-key close-buffers
       :command context-navigator-view-close-all-buffers
       :help ,(lambda () (funcall tr :close-buffers))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (funcall tr :close-buffers))) " [K]")))
      (clear-group
       :type action
       :icon-key clear-group
       :command context-navigator-view-clear-group
       :help ,(lambda () (funcall tr :clear-group))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (funcall tr :clear-group))) " [C]")))
      (toggle-all-gptel
       :type action
       :icon-key toggle-all-gptel
       :command context-navigator-view-toggle-all-gptel
       :help ,(lambda () (funcall tr :toggle-all-gptel))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (funcall tr :toggle-all-gptel))) " [T]")))
      ))
  "Registry of Navigator controls for header-line/inline toolbars."
  :type '(alist :key-type symbol :value-type plist)
  :group 'context-navigator-view-controls)

(defun context-navigator-view-controls--plist-fn (val)
  "If VAL is a function, call it with no args, else return VAL."
  (if (functionp val) (funcall val) val))

(defun context-navigator-view-controls--render (key)
  "Render a single control segment for KEY using the controls registry.
Returns a propertized string or nil when not visible."
  (let* ((desc (alist-get key context-navigator-controls-registry))
         (type (plist-get desc :type))
         (cmd  (plist-get desc :command))
         (help (plist-get desc :help))
         (enabled-p (let ((fn (or (plist-get desc :enabled-p) (lambda () t))))
                      (ignore-errors (funcall fn))))
         (visible-p (let ((fn (or (plist-get desc :visible-p) (lambda () t))))
                      (ignore-errors (funcall fn))))
         (style (or context-navigator-controls-style 'auto)))
    (when (and desc visible-p)
      (let* ((gicons (and (fboundp 'context-navigator-controls-icons-available-p)
                          (context-navigator-controls-icons-available-p)))
             (state (when (eq type 'toggle)
                      (let ((fn (plist-get desc :state-fn)))
                        (when (functionp fn) (ignore-errors (funcall fn))))))
             (spinner (let ((fn (plist-get desc :spinner-fn)))
                        (when (functionp fn) (ignore-errors (funcall fn)))))
             (icon-key (plist-get desc :icon-key))
             (ico (and gicons (not spinner)
                       (context-navigator-controls-icon icon-key state)))
             (label (cond
                     (spinner (concat " " spinner))
                     (ico     (concat " " ico))
                     (t (let ((lf (plist-get desc :label-fn)))
                          (when (functionp lf) (funcall lf style state))))))
             ;; Ensure we work on a writable string copy
             (s (copy-sequence (or label "")))
             (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0))
             (km (when (and cmd enabled-p)
                   (let ((m (make-sparse-keymap)))
                     (define-key m [mouse-1] cmd)
                     (define-key m [header-line mouse-1] cmd)
                     m)))
             (help-str (context-navigator-view-controls--plist-fn help)))
        (when (> (length s) 0)
          (let ((props (list 'mouse-face 'highlight
                             'help-echo help-str
                             'context-navigator-key key)))
            (when km
              (setq props (append props (list 'keymap km 'local-map km))))
            (when (eq type 'toggle)
              (setq props (append props (list 'context-navigator-toggle key))))
            (when (eq type 'action)
              (setq props (append props (list 'context-navigator-action key))))
            (add-text-properties beg (length s) props s))
          (unless enabled-p
            (add-text-properties beg (length s) (list 'face 'shadow) s))
          ;; Apply optional face for textual fallback when icons are not used.
          (when-let* ((ff (plist-get desc :face-fn))
                      (face (and (functionp ff) (funcall ff style state))))
            (unless gicons
              (add-text-properties beg (length s)
                                   (list 'face (or (and (symbolp face) face)
                                                   (and (listp face) face)))
                                   s))))
        (and (> (length s) 0) s)))))

(defun context-navigator-view-controls-segments (&optional where)
  "Return ordered control segments as a list of strings.
Optional WHERE is one of:
  :headerline — use `context-navigator-headerline-controls-order' (default)
  :inline     — use `context-navigator-inline-controls-order'"
  (let* ((order (pcase where
                  (:inline     context-navigator-inline-controls-order)
                  (_           context-navigator-headerline-controls-order))))
    (cl-loop for k in order
             if (eq k :gap) collect "    "
             else
             for seg = (context-navigator-view-controls--render k)
             when (stringp seg) collect seg)))

(defun context-navigator-view-controls-lines (total-width)
  "Return wrapped control lines (inline toolbar) for TOTAL-WIDTH.

Wrap segments produced by `context-navigator-view-controls-segments' using
the view's wrapping helper. If any single segment is wider than TOTAL-WIDTH
in the current style, rebuild segments with compact icon labels to ensure
lines fit into TOTAL-WIDTH."
  (let* ((segs (context-navigator-view-controls-segments :inline))
         (too-wide (cl-some (lambda (s) (> (string-width s) total-width)) segs)))
    (when too-wide
      (let ((context-navigator-controls-style 'icons))
        (setq segs (context-navigator-view-controls-segments :inline))))
    (let ((wrap-fn (if (fboundp 'context-navigator-view--wrap-segments)
                       #'context-navigator-view--wrap-segments
                     (lambda (segments total-width)
                       (let ((acc '())
                             (cur ""))
                         (dolist (seg segments)
                           (let* ((sw (string-width seg))
                                  (cw (string-width cur)))
                             (if (<= (+ cw sw) total-width)
                                 (setq cur (concat cur seg))
                               (when (> (length cur) 0)
                                 (push cur acc))
                               (setq cur seg))))
                         (when (> (length cur) 0) (push cur acc))
                         (nreverse acc))))))
      (funcall wrap-fn segs total-width))))

;; Auto-refresh UI when registry or order variables change.
(when (fboundp 'add-variable-watcher)
  (dolist (sym '(context-navigator-headerline-controls-order
                 context-navigator-inline-controls-order
                 context-navigator-controls-registry))
    (add-variable-watcher
     sym
     (lambda (&rest _)
       ;; Avoid full model refresh; do a local sidebar re-render
       (ignore-errors
         (let ((buf (get-buffer "*context-navigator*")))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (setq-local context-navigator-render--last-hash nil)
               (setq-local context-navigator-view--last-render-key nil)
               (when (fboundp 'context-navigator-view--render-if-visible)
                 (context-navigator-view--render-if-visible))))))
       (force-mode-line-update t)))))

(provide 'context-navigator-view-controls)
;;; context-navigator-view-controls.el ends here
