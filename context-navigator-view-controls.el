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

(defgroup context-navigator-view-controls nil
  "Toolbar controls (toggles and actions) for Context Navigator view."
  :group 'context-navigator)

(defun context-navigator-view-controls--build-toggles ()
  "Build toggle segments for push→gptel, auto-project and Undo/Redo with mouse keymaps.
Respects `context-navigator-controls-style' for compact icon/text labels."
  (let* ((push-on (and (boundp 'context-navigator--push-to-gptel)
                       context-navigator--push-to-gptel))
         (auto-on (and (boundp 'context-navigator--auto-project-switch)
                       context-navigator--auto-project-switch))
         (gptel-available (ignore-errors (context-navigator-gptel-available-p)))
         (style (or context-navigator-controls-style 'auto))
         ;; label builders
         (lbl-push
          (pcase style
            ((or 'icons 'auto) " [→]")
            (_ (format " [→gptel: %s]" (if push-on "on" "off")))))
         (lbl-auto
          (pcase style
            ((or 'icons 'auto) " [A]")
            (_ (format " [%s: %s]" (context-navigator-i18n :auto-proj) (if auto-on "on" "off")))))
         (lbl-redo
          (pcase style
            ((or 'icons 'auto) " [↷]")
            (_ (concat " [" (context-navigator-i18n :razor-redo) "]"))))
         (lbl-undo
          (pcase style
            ((or 'icons 'auto) " [↶]")
            (_ (concat " [" (context-navigator-i18n :razor-undo) "]")))))
    (let* ((seg1 (let* ((s (copy-sequence lbl-push))
                        (m (let ((km (make-sparse-keymap)))
                             (when gptel-available
                               (define-key km [mouse-1] #'context-navigator-view-toggle-push)
                               (define-key km [header-line mouse-1] #'context-navigator-view-toggle-push))
                             km))
                        (fg (if (and push-on gptel-available) "green4" "gray"))
                        (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
                   (add-text-properties beg (length s)
                                        (list 'mouse-face 'highlight
                                              'help-echo (if gptel-available
                                                             (context-navigator-i18n :toggle-push)
                                                           "gptel not available")
                                              'keymap m
                                              'local-map m
                                              'context-navigator-toggle 'push
                                              'face (if gptel-available
                                                        (list :foreground fg)
                                                      'shadow))
                                        s)
                   s))
           (seg2 (let* ((s (copy-sequence lbl-auto))
                        (m (let ((km (make-sparse-keymap)))
                             (define-key km [mouse-1] #'context-navigator-view-toggle-auto-project)
                             (define-key km [header-line mouse-1] #'context-navigator-view-toggle-auto-project)
                             km))
                        (fg (if auto-on "green4" "gray"))
                        (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0)))
                   (add-text-properties beg (length s)
                                        (list 'mouse-face 'highlight
                                              'help-echo (context-navigator-i18n :toggle-auto)
                                              'keymap m
                                              'local-map m
                                              'context-navigator-toggle 'auto
                                              'face (list :foreground fg))
                                        s)
                   s))

           (seg3 (let* ((s (copy-sequence lbl-redo))
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

           (seg4 (let* ((s (copy-sequence lbl-undo))
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
                   s)))
      (list seg1 seg2 seg3 seg4))))

(defun context-navigator-view-controls--build-actions ()
  "Build action segments (Open/Close buffers, Clear gptel, Push now, Toggle all gptel).

Return a list of propertized segment strings. Each segment has a keymap and
the text property `context-navigator-action' set to a symbol describing the action
(e.g. 'push-now, 'open-buffers). Labels respect `context-navigator-controls-style'."
  (let ((mk (lambda (label action-sym cmd help)
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
    (let ((style (or context-navigator-controls-style 'auto)))
      (list
       (funcall mk
                (if (eq style 'text)
                    (format " [%s]" (capitalize (context-navigator-i18n :push-now)))
                  " [P]")
                'push-now #'context-navigator-view-push-now
                (context-navigator-i18n :push-now))
       (funcall mk
                (if (eq style 'text)
                    (format " [%s]" (capitalize (context-navigator-i18n :open-buffers)))
                  " [O]")
                'open-buffers #'context-navigator-view-open-all-buffers
                (context-navigator-i18n :open-buffers))
       (funcall mk
                (if (eq style 'text)
                    (format " [%s]" (capitalize (context-navigator-i18n :close-buffers)))
                  " [K]")
                'close-buffers #'context-navigator-view-close-all-buffers
                (context-navigator-i18n :close-buffers))
       ;; Note: show "Clear gptel" action (clears gptel context), not group clear.
       (funcall mk
                (if (eq style 'text)
                    (format " [%s]" (capitalize (context-navigator-i18n :clear-gptel)))
                  " [∅]")
                'clear-gptel #'context-navigator-view-clear-gptel
                (context-navigator-i18n :clear-gptel))
       (funcall mk
                (if (eq style 'text)
                    (format " [%s]" (capitalize (context-navigator-i18n :toggle-all-gptel)))
                  " [T]")
                'toggle-all-gptel #'context-navigator-view-toggle-all-gptel
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
    (context-navigator-view--wrap-segments segs total-width)))

(provide 'context-navigator-view-controls)
;;; context-navigator-view-controls.el ends here
