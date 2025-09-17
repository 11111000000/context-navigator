;;; context-navigator-view-controls.el --- Controls (toolbar) for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Build header-line/inline toolbar controls (toggles + actions) for the Navigator view.
;; Split out of context-navigator-view.el to reduce coupling.
;;
;; This module does not require the full view to avoid cycles; it declares the
;; helpers it uses from the view. The view should (require 'context-navigator-view-controls)
;; to expose the public controls API.

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

(defun context-navigator-view-controls--build-toggles ()
  "Build toggle segments for push→gptel, auto-project and Undo/Redo with mouse keymaps.
Prefers real graphic icons (all-the-icons) without brackets when available and enabled,
falls back to compact labels or text with brackets otherwise."
  (let* ((push-on (and (boundp 'context-navigator--push-to-gptel)
                       context-navigator--push-to-gptel))
         (auto-on (and (boundp 'context-navigator--auto-project-switch)
                       context-navigator--auto-project-switch))
         (gptel-available (ignore-errors (context-navigator-gptel-available-p)))
         (style (or context-navigator-controls-style 'auto))
         (gicons (and (fboundp 'context-navigator-controls-icons-available-p)
                      (context-navigator-controls-icons-available-p)))
         ;; Build labels with icon preference
         (lbl-push
          (let* ((fallback
                  (pcase style
                    ((or 'icons 'auto) " [→]")
                    (_ (format " [→gptel: %s]" (if push-on "on" "off")))))
                 (ico (and gicons
                           (context-navigator-controls-icon 'push (if push-on 'on 'off)))))
            (if gicons
                (concat " " (or ico fallback))
              fallback)))
         (lbl-auto
          (let* ((fallback
                  (pcase style
                    ((or 'icons 'auto) " [A]")
                    (_ (format " [%s: %s]" (context-navigator-i18n :auto-proj) (if auto-on "on" "off")))))
                 (ico (and gicons
                           (context-navigator-controls-icon 'auto-project (if auto-on 'on 'off)))))
            (if gicons
                (concat " " (or ico fallback))
              fallback)))
         (lbl-undo
          (let* ((fallback (pcase style
                             ((or 'icons 'auto) " [↶]")
                             (_ (concat " [" (context-navigator-i18n :razor-undo) "]"))))
                 (ico (and gicons (context-navigator-controls-icon 'undo))))
            (if gicons (concat " " (or ico fallback)) fallback)))
         (lbl-redo
          (let* ((fallback (pcase style
                             ((or 'icons 'auto) " [↷]")
                             (_ (concat " [" (context-navigator-i18n :razor-redo) "]"))))
                 (ico (and gicons (context-navigator-controls-icon 'redo))))
            (if gicons (concat " " (or ico fallback)) fallback))))
    (let* ((seg1 (let* ((s (copy-sequence lbl-push))
                        (m (let ((km (make-sparse-keymap)))
                             (when gptel-available
                               (define-key km [mouse-1] #'context-navigator-view-toggle-push)
                               (define-key km [header-line mouse-1] #'context-navigator-view-toggle-push))
                             km))
                        (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
                   (add-text-properties beg (length s)
                                        (list 'mouse-face 'highlight
                                              'help-echo (if gptel-available
                                                             (context-navigator-i18n :toggle-push)
                                                           "gptel not available")
                                              'keymap m
                                              'local-map m
                                              'context-navigator-toggle 'push)
                                        s)
                   ;; When gptel is not available, dim the icon/label
                   (unless gptel-available
                     (add-text-properties beg (length s) (list 'face 'shadow) s))
                   s))
           (seg2 (let* ((s (copy-sequence lbl-auto))
                        (m (let ((km (make-sparse-keymap)))
                             (define-key km [mouse-1] #'context-navigator-view-toggle-auto-project)
                             (define-key km [header-line mouse-1] #'context-navigator-view-toggle-auto-project)
                             km))
                        (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
                   (add-text-properties beg (length s)
                                        (list 'mouse-face 'highlight
                                              'help-echo (context-navigator-i18n :toggle-auto)
                                              'keymap m
                                              'local-map m
                                              'context-navigator-toggle 'auto)
                                        s)
                   (unless gicons
                     (let ((fg (if auto-on "green4" "gray")))
                       (add-text-properties beg (length s) (list 'face (list :foreground fg)) s)))
                   s))
           (seg3 (let* ((s (copy-sequence lbl-undo))
                        (m (let ((km (make-sparse-keymap)))
                             (when (fboundp 'context-navigator-undo)
                               (define-key km [mouse-1] #'context-navigator-undo)
                               (define-key km [header-line mouse-1] #'context-navigator-undo))
                             km))
                        (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
                   (add-text-properties beg (length s)
                                        (list 'mouse-face 'highlight
                                              'help-echo (context-navigator-i18n :razor-undo)
                                              'keymap m
                                              'local-map m
                                              'context-navigator-toggle 'undo)
                                        s)
                   s))
           (seg4 (let* ((s (copy-sequence lbl-redo))
                        (m (let ((km (make-sparse-keymap)))
                             (when (fboundp 'context-navigator-redo)
                               (define-key km [mouse-1] #'context-navigator-redo)
                               (define-key km [header-line mouse-1] #'context-navigator-redo))
                             km))
                        (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
                   (add-text-properties beg (length s)
                                        (list 'mouse-face 'highlight
                                              'help-echo (context-navigator-i18n :razor-redo)
                                              'keymap m
                                              'local-map m
                                              'context-navigator-toggle 'redo)
                                        s)
                   s))
           )
      (list seg1 seg2 seg3 seg4))))

(defun context-navigator-view-controls--build-actions ()
  "Build action segments (Occam, Open/Close buffers, Push now, Toggle all gptel).

When graphic icons are available (all-the-icons) and enabled, show icons only (no brackets);
otherwise fall back to compact labels or text in brackets. While Occam is running,
show a small spinner in place of its icon."
  (let* ((style (or context-navigator-controls-style 'auto))
         (gicons (and (fboundp 'context-navigator-controls-icons-available-p)
                      (context-navigator-controls-icons-available-p)))
         (razor-running (and (boundp 'context-navigator-razor--running)
                             context-navigator-razor--running))
         (mk (lambda (label action-sym cmd help)
               (let* ((s (copy-sequence label))
                      (km (let ((m (make-sparse-keymap)))
                            (define-key m [mouse-1] cmd)
                            (define-key m [header-line mouse-1] cmd)
                            m))
                      (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
                 (add-text-properties beg (length s)
                                      (list 'mouse-face 'highlight
                                            'help-echo help
                                            'keymap km
                                            'local-map km
                                            'context-navigator-action action-sym)
                                      s)
                 s))))
    (let* ((lbl-razor
            (let* ((fallback (if (eq style 'text)
                                 (format " [%s]" (context-navigator-i18n :tr-razor))
                               " [R]"))
                   (spinner (and razor-running
                                 (fboundp 'context-navigator-razor-spinner-frame)
                                 (context-navigator-razor-spinner-frame)))
                   (ico (and gicons (not razor-running)
                             (context-navigator-controls-icon 'razor))))
              (cond
               (razor-running
                (concat " " (or spinner fallback)))
               (gicons
                (concat " " (or ico fallback)))
               (t fallback))))
           (lbl-push-now
            (let* ((fallback (if (eq style 'text)
                                 (format " [%s]" (capitalize (context-navigator-i18n :push-now)))
                               " [P]"))
                   (ico (and gicons (context-navigator-controls-icon 'push-now))))
              (if gicons (concat " " (or ico fallback)) fallback)))
           (lbl-open
            (let* ((fallback (if (eq style 'text)
                                 (format " [%s]" (capitalize (context-navigator-i18n :open-buffers)))
                               " [O]"))
                   (ico (and gicons (context-navigator-controls-icon 'open-buffers))))
              (if gicons (concat " " (or ico fallback)) fallback)))
           (lbl-close
            (let* ((fallback (if (eq style 'text)
                                 (format " [%s]" (capitalize (context-navigator-i18n :close-buffers)))
                               " [K]"))
                   (ico (and gicons (context-navigator-controls-icon 'close-buffers))))
              (if gicons (concat " " (or ico fallback)) fallback)))
           (lbl-clear-group
            (let* ((fallback (if (eq style 'text)
                                 (format " [%s]" (capitalize (context-navigator-i18n :clear-group)))
                               " [C]"))
                   (ico (and gicons (context-navigator-controls-icon 'clear-group))))
              (if gicons (concat " " (or ico fallback)) fallback)))
           (lbl-toggle-all
            (let* ((fallback (if (eq style 'text)
                                 (format " [%s]" (capitalize (context-navigator-i18n :toggle-all-gptel)))
                               " [T]"))
                   (ico (and gicons (context-navigator-controls-icon 'toggle-all-gptel))))
              (if gicons (concat " " (or ico fallback)) fallback))))
      (list
       (funcall mk lbl-razor 'razor #'context-navigator-view-razor-run
                (context-navigator-i18n :tr-razor))
       (funcall mk lbl-push-now 'push-now #'context-navigator-view-push-now
                (context-navigator-i18n :push-now))
       (funcall mk lbl-open 'open-buffers #'context-navigator-view-open-all-buffers
                (context-navigator-i18n :open-buffers))
       (funcall mk lbl-close 'close-buffers #'context-navigator-view-close-all-buffers
                (context-navigator-i18n :close-buffers))
       (funcall mk lbl-clear-group 'clear-group #'context-navigator-view-clear-group
                (context-navigator-i18n :clear-group))
       (funcall mk lbl-toggle-all 'toggle-all-gptel #'context-navigator-view-toggle-all-gptel
                (context-navigator-i18n :toggle-all-gptel))))))


(defun context-navigator-view-controls-segments ()
  "Return all control segments (toggles and actions).

Compose toggles and actions produced by this module. This avoids load-cycle
with the view module by not delegating building back to the view."
  (append (context-navigator-view-controls--build-toggles)
          (context-navigator-view-controls--build-actions)))

(defun context-navigator-view-controls-lines (total-width)
  "Return wrapped control lines (inline toolbar) for TOTAL-WIDTH.

Wrap segments produced by `context-navigator-view-controls-segments' using
the view's wrapping helper. If any single segment is wider than TOTAL-WIDTH
in the current style, rebuild segments with compact icon labels to ensure
lines fit into TOTAL-WIDTH."
  (let* ((segs (context-navigator-view-controls-segments))
         (too-wide (cl-some (lambda (s) (> (string-width s) total-width)) segs)))
    (when too-wide
      (let ((context-navigator-controls-style 'icons))
        (setq segs (context-navigator-view-controls-segments))))
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

(provide 'context-navigator-view-controls)
;;; context-navigator-view-controls.el ends here
