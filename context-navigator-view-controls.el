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

;; Declarations to avoid load cycles; provided by context-navigator-view.
(declare-function context-navigator-view--make-toggle-segments "context-navigator-view" ())
(declare-function context-navigator-view--openable-count-get "context-navigator-view" ())
(declare-function context-navigator-view--collect-closable-buffers "context-navigator-view" ())
(declare-function context-navigator-view-open-all-buffers "context-navigator-view" ())
(declare-function context-navigator-view-close-all-buffers "context-navigator-view" ())
(declare-function context-navigator-view-clear-group "context-navigator-view" ())
(declare-function context-navigator-view-push-now "context-navigator-view" ())
(declare-function context-navigator-view-toggle-all-gptel "context-navigator-view" ())

(defgroup context-navigator-view-controls nil
  "Toolbar controls (toggles and actions) for Context Navigator view."
  :group 'context-navigator)

(defun context-navigator-view-controls--build-toggles ()
  "Build toggle segments (push→gptel, auto-project, undo/redo)."
  (context-navigator-view--make-toggle-segments))

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
                    (format " [%s]" (capitalize (context-navigator-i18n :toggle-push)))
                  " [T]")
                'toggle-all-gptel #'context-navigator-view-toggle-all-gptel
                (context-navigator-i18n :toggle-push))))))

(defun context-navigator-view-controls-segments ()
  "Return all control segments (toggles and actions).

Compose toggles and actions produced by this module. This avoids load-cycle
with the view module by not delegating building back to the view."
  (append (context-navigator-view-controls--build-toggles)
          (context-navigator-view-controls--build-actions)))

(defun context-navigator-view-controls-lines (total-width)
  "Return wrapped control lines (inline toolbar) for TOTAL-WIDTH.

Wrap segments produced by `context-navigator-view-controls-segments' using
the view's wrapping helper."
  (let ((segs (context-navigator-view-controls-segments)))
    (context-navigator-view--wrap-segments segs total-width)))

(provide 'context-navigator-view-controls)
;;; context-navigator-view-controls.el ends here
