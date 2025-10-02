;;; context-navigator-view-controls.el --- Controls (toolbar) for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Build header-line toolbar controls (toggles + actions) for the Navigator view.
;; Split out of context-navigator-view.el to reduce coupling.
;;
;; This module does not require the full view to avoid cycles; it declares the
;; helpers it uses from the view. The view should
;; to expose the public controls API.
;;
;; Refactoring note:
;; - Controls are now described declaratively in a registry: logic + appearance
;; - Order/visibility is configured via a single header-line order
;; - Rendering is unified and consumes registry + order

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-view-segments)
(require 'context-navigator-i18n)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-controls-icons)

;; Declarations to avoid load cycles; provided by context-navigator-view or core.
(declare-function context-navigator-view-open-all-buffers "context-navigator-view-actions" ())
(declare-function context-navigator-view-close-all-buffers "context-navigator-view-actions" ())
(declare-function context-navigator-view-clear-group "context-navigator-view-actions" ())
(declare-function context-navigator-view-push-now "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-all-gptel "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-push "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-auto-project "context-navigator-view-actions" ())
(declare-function context-navigator-undo "context-navigator-core" ())
(declare-function context-navigator-redo "context-navigator-core" ())

(declare-function context-navigator-view-razor-run "context-navigator-view-actions" ())
(declare-function context-navigator-multifile-open "context-navigator-multifile" ())
(declare-function context-navigator-stats-split-toggle "context-navigator-stats-split" ())
(declare-function context-navigator-view-push-now-dispatch "context-navigator-view-dispatch" ())
(declare-function context-navigator-view-toggle-multi-group "context-navigator-view-dispatch" ())
(declare-function context-navigator-persist-state-load "context-navigator-persist" (root))
(declare-function context-navigator-persist-group-enabled-count "context-navigator-persist" (file))
(declare-function context-navigator-persist-context-file "context-navigator-persist" (root &optional group-slug))
(declare-function context-navigator--state-get "context-navigator-core" ())
(declare-function context-navigator-state-last-project-root "context-navigator-core" (state))
(declare-function context-navigator-state-items "context-navigator-core" (state))
(declare-function context-navigator-state-current-group-slug "context-navigator-core" (state))

(defgroup context-navigator-view-controls nil
  "Toolbar controls (toggles and actions) for Context Navigator view."
  :group 'context-navigator)

(defface context-navigator-headerline
  '((t :background "#2f2f2f" :foreground "gray90"))
  "Default header-line face for Context Navigator.
Used as the header-line background in the Navigator buffer."
  :group 'context-navigator-view-controls)

(defvar-local context-navigator--headerline-face-cookie nil
  "Face-remap cookie for remapping the header-line face in Navigator buffer.")

(defun context-navigator-view-controls--ensure-headerline-face ()
  "Ensure the Navigator buffer uses `context-navigator-headerline' for its header-line."
  (let ((buf (get-buffer "*context-navigator*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Remap the built-in `header-line' face buffer-locally to our custom face.
        ;; Reset any previous remap to keep the operation idempotent.
        (when context-navigator--headerline-face-cookie
          (ignore-errors
            (face-remap-remove-relative context-navigator--headerline-face-cookie))
          (setq context-navigator--headerline-face-cookie nil))
        (setq context-navigator--headerline-face-cookie
              (face-remap-add-relative 'header-line 'context-navigator-headerline))
        ;; Force header-line redraw
        (force-mode-line-update t)))))

;; Apply default header-line face now if the buffer already exists.
(context-navigator-view-controls--ensure-headerline-face)

;; ---- Small pure helpers for gating push/autopush ---------------------------

(defun context-navigator-view-controls--selected-slugs ()
  "Return selected group slugs from state.el for current root (pure)."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (ps (and (stringp root)
                  (ignore-errors (context-navigator-persist-state-load root)))))
    (let ((sel (and (listp ps) (plist-member ps :selected) (plist-get ps :selected))))
      (if (listp sel) sel '()))))

(defun context-navigator-view-controls--sum-enabled-for-slugs (root slugs)
  "Return sum of enabled items across SLUGS (reads per-group v3 files)."
  (let ((sum 0))
    (dolist (slug slugs sum)
      (let* ((file (ignore-errors (context-navigator-persist-context-file root slug)))
             (en.t (and (stringp file)
                        (ignore-errors (context-navigator-persist-group-enabled-count file))))
             (en (and (consp en.t) (car en.t))))
        (when (integerp en)
          (setq sum (+ sum en)))))))

(defun context-navigator-view-controls--push-allowed-p ()
  "Return non-nil when push/auto should be enabled in groups mode.
Rule: selection non-empty AND aggregated enabled items > 0."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (sel (context-navigator-view-controls--selected-slugs)))
    (and (stringp root)
         (listp sel) (> (length sel) 0)
         (> (context-navigator-view-controls--sum-enabled-for-slugs root sel) 0))))

(defun context-navigator-view-controls--items-enabled-p ()
  "Return non-nil when current group (items mode) has at least one enabled item."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (ignore-errors (context-navigator-state-items st)))))
    (and (listp items)
         (cl-some (lambda (it) (and (context-navigator-item-p it)
                               (context-navigator-item-enabled it)))
                  items))))

(defun context-navigator-view-controls--push-disabled-reason ()
  "Return a localized reason string when push is disabled, or nil."
  (let* ((mode (and (boundp 'context-navigator-view--mode)
                    context-navigator-view--mode)))
    (if (eq mode 'groups)
        (let* ((st (ignore-errors (context-navigator--state-get)))
               (root (and st (context-navigator-state-last-project-root st)))
               (sel (context-navigator-view-controls--selected-slugs)))
          (cond
           ((or (null (listp sel)) (= (length sel) 0))
            (context-navigator-i18n :no-group-selected))
           ((<= (context-navigator-view-controls--sum-enabled-for-slugs root sel) 0)
            (context-navigator-i18n :no-enabled-in-selection))
           (t nil)))
      ;; items mode
      (unless (context-navigator-view-controls--items-enabled-p)
        (context-navigator-i18n :razor-no-enabled-items)))))

;; Layout: order of controls for header-line toolbar.
(defcustom context-navigator-headerline-controls-order
  '(push auto-project multi-group :gap stats multifile :gap undo redo :gap toggle-all-gptel :gap razor push-now
         :gap open-buffers close-buffers :gap clear-group)
  "Controls order for the header-line toolbar.
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
       :help ,(lambda ()
                (let ((base (funcall tr :toggle-push))
                      (why (context-navigator-view-controls--push-disabled-reason)))
                  (if (and why (not (context-navigator-view-controls--push-allowed-p)))
                      (format "%s — %s" base why)
                    base)))
       :enabled-p ,(lambda ()
                     (and (ignore-errors (context-navigator-gptel-available-p))
                          (let* ((mode (and (boundp 'context-navigator-view--mode)
                                            context-navigator-view--mode)))
                            (if (eq mode 'groups)
                                (context-navigator-view-controls--push-allowed-p)
                              (context-navigator-view-controls--items-enabled-p)))))
       :visible-p ,(lambda () t)
       :state-fn ,(lambda ()
                    (if (and (boundp 'context-navigator--push-to-gptel)
                             context-navigator--push-to-gptel)
                        'on 'off))
       :label-fn ,(lambda (style state)
                    (pcase style
                      ((or 'icons 'auto) " [→]")
                      (_ (format " [→gptel: %s]" (context-navigator-i18n (if (eq state 'on) :on :off)))))))
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
                      (_ (format " [%s: %s]" (context-navigator-i18n :auto-proj) (context-navigator-i18n (if (eq state 'on) :on :off))))))
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
                        (format " [%s]" (context-navigator-i18n :tr-razor))
                      " [R]")))
      (stats
       :type action
       :icon-key stats
       :command context-navigator-stats-split-toggle
       :help ,(lambda () (funcall tr :stats))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (pcase style
                      ((or 'icons 'auto) " [Σ]")
                      (_ (format " [%s]" (context-navigator-i18n :stats))))))
      (multi-group
       :type toggle
       :icon-key multi-group
       :command context-navigator-view-toggle-multi-group
       :help ,(lambda () (funcall tr :toggle-multi-group))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :state-fn ,(lambda ()
                    (let* ((st (ignore-errors (context-navigator--state-get)))
                           (root (and st (context-navigator-state-last-project-root st)))
                           (ps (and (stringp root)
                                    (ignore-errors (context-navigator-persist-state-load root))))
                           (mg (and (listp ps) (plist-member ps :multi) (plist-get ps :multi))))
                      (if mg 'on 'off)))
       :label-fn ,(lambda (_style state)
                    (if (eq state 'on) " [MG✓]" " [MG]")))
      (push-now
       :type action
       :icon-key push-now
       :command context-navigator-view-push-now-dispatch
       :help ,(lambda ()
                (let ((base (funcall tr :push-now))
                      (mode (and (boundp 'context-navigator-view--mode)
                                 context-navigator-view--mode)))
                  (if (and (not (if (eq mode 'groups)
                                    (context-navigator-view-controls--push-allowed-p)
                                  (context-navigator-view-controls--items-enabled-p))))
                      (let ((why (context-navigator-view-controls--push-disabled-reason)))
                        (if why (format "%s — %s" base why) base))
                    base)))
       :enabled-p ,(lambda ()
                     (let* ((mode (and (boundp 'context-navigator-view--mode)
                                       context-navigator-view--mode)))
                       (if (eq mode 'groups)
                           (context-navigator-view-controls--push-allowed-p)
                         (context-navigator-view-controls--items-enabled-p))))
       :visible-p ,(lambda () t)
       :spinner-fn ,(lambda ()
                      (and (boundp 'context-navigator-view--gptel-batch-start-time)
                           context-navigator-view--gptel-batch-start-time
                           (boundp 'context-navigator-view--spinner-index)
                           (boundp 'context-navigator-view-spinner-frames)
                           (let* ((frames context-navigator-view-spinner-frames)
                                  (idx (or context-navigator-view--spinner-index 0))
                                  (len (length (or frames '()))))
                             (when (> len 0)
                               ;; Two-character frame so segment width doesn't "jump"
                               (let ((fr (nth (mod idx len) frames)))
                                 (concat fr fr))))))
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (context-navigator-i18n :push-now))) " [P]")))
      (open-buffers
       :type action
       :icon-key open-buffers
       :command context-navigator-view-open-all-buffers
       :help ,(lambda () (funcall tr :open-buffers))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (context-navigator-i18n :open-buffers))) " [O]")))
      (close-buffers
       :type action
       :icon-key close-buffers
       :command context-navigator-view-close-all-buffers
       :help ,(lambda () (funcall tr :close-buffers))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (context-navigator-i18n :close-buffers))) " [K]")))
      ;; (clear-gptel
      ;;  :type action
      ;;  :icon-key clear-gptel
      ;;  :command context-navigator-view-clear-gptel
      ;;  :help ,(lambda () (funcall tr :clear-tip))
      ;;  :enabled-p ,(lambda () t)
      ;;  :visible-p ,(lambda () t)
      ;;  :label-fn ,(lambda (style _s)
      ;;               (if (eq style 'text)
      ;;                   (format " [%s]" (context-navigator-i18n :clear-gptel))
      ;;                 " [C]")))
      (clear-group
       :type action
       :icon-key clear-group
       :command context-navigator-view-clear-group
       :help ,(lambda () (funcall tr :clear-group))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (context-navigator-i18n :clear-group))) " [E]")))
      (toggle-all-gptel
       :type action
       :icon-key toggle-all-gptel
       :command context-navigator-view-toggle-all-gptel
       :help ,(lambda () (funcall tr :toggle-all-gptel))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (context-navigator-i18n :toggle-all-gptel))) " [T]")))
      (multifile
       :type action
       :icon-key multifile
       :command context-navigator-multifile-open
       :help ,(lambda () (funcall tr :tr-multifile))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (pcase style
                      ((or 'icons 'auto) " [MF]")
                      (_ " [Multifile]"))))
      ))
  "Registry of Navigator controls for header-line toolbar."
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
                   (ignore-errors (context-navigator-view-ui-make-keymap cmd))))
             (help-str (context-navigator-view-controls--plist-fn help)))
        (when (> (length s) 0)
          (let ((props (list 'mouse-face 'highlight
                             'help-echo help-str
                             'context-navigator-key key
                             'context-navigator-interactive t)))
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

(defun context-navigator-view-controls-segments (&optional _where)
  "Return ordered control segments for the header-line as a list of strings."
  (let* ((order context-navigator-headerline-controls-order)
         (res '()))
    (dolist (k order)
      (if (eq k :gap)
          (push " " res)
        (when-let* ((seg (context-navigator-view-controls--render k)))
          (when (stringp seg)
            (push seg res)))))
    (nreverse res)))

;; Auto-refresh UI when registry or order variables change.
(when (fboundp 'add-variable-watcher)
  (dolist (sym '(context-navigator-headerline-controls-order
                 context-navigator-controls-registry))
    (add-variable-watcher
     sym
     (lambda (&rest _)
       ;; Avoid full model refresh; do a local sidebar re-render
       (ignore-errors
         (let ((buf (get-buffer "*context-navigator*")))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               ;; Invalidate view and headerline caches
               (setq-local context-navigator-render--last-hash nil)
               (setq-local context-navigator-view--last-render-key nil)
               (setq-local context-navigator-headerline--cache-key nil)
               (setq-local context-navigator-headerline--cache-str nil)
               (when (fboundp 'context-navigator-view--render-if-visible)
                 (context-navigator-view--render-if-visible))))))
       ;; Ensure the navigator buffer uses the default header-line face.
       (context-navigator-view-controls--ensure-headerline-face)
       (force-mode-line-update t)))))

(defun context-navigator-view-controls--wrap-segments (segments total-width)
  "Wrap SEGMENTS (list of strings) into lines within TOTAL-WIDTH columns.

Guarantees each returned line's visual width does not exceed TOTAL-WIDTH.
Segments longer than TOTAL-WIDTH are soft-split using `truncate-string-to-width'."
  (let* ((tw (max 1 (or total-width 80)))
         (acc '())
         (cur ""))
    (dolist (seg segments)
      (let ((seg (or seg "")))
        (while (and (stringp seg) (> (length seg) 0))
          (let* ((cw (string-width cur))
                 (avail (max 0 (- tw cw))))
            (cond
             ;; No space left on current line: emit it and continue
             ((= avail 0)
              (when (> (length cur) 0) (push cur acc))
              (setq cur ""))
             ;; Segment fits entirely on current line
             ((<= (string-width seg) avail)
              (setq cur (concat cur seg))
              (setq seg ""))
             ;; Need to split the segment to fit the remaining space
             (t
              (let* ((head (truncate-string-to-width seg avail nil nil))
                     (head-len (length head)))
                ;; If avail is small but truncate returns empty, force a line break
                (if (or (null head) (= head-len 0))
                    (progn
                      (when (> (length cur) 0) (push cur acc))
                      (setq cur "")) ;; retry with same seg on next loop
                  (setq cur (concat cur head))
                  (push cur acc)
                  (setq cur "")
                  ;; Remainder of seg (drop the chars we consumed)
                  (setq seg (substring seg head-len))))))))))
    (when (> (length cur) 0)
      (push cur acc))
    (nreverse acc)))

(defun context-navigator-view-controls-lines (total-width)
  "Return header-line control lines wrapped to TOTAL-WIDTH columns.

When TOTAL-WIDTH is nil or non-positive, try to use the selected window width;
fallback to 80 columns."
  (let* ((tw (cond
              ((and (numberp total-width) (> total-width 0)) total-width)
              ((window-live-p (selected-window)) (window-body-width (selected-window)))
              (t 80)))
         (segments (context-navigator-view-controls-segments)))
    (context-navigator-view-controls--wrap-segments (or segments '()) tw)))

(provide 'context-navigator-view-controls)
;;; context-navigator-view-controls.el ends here
