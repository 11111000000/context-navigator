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
(declare-function context-navigator-view--footer-control-segments "context-navigator-view" ())
(declare-function context-navigator-view--header-toggle-lines "context-navigator-view" (total-width))

(defgroup context-navigator-view-controls nil
  "Toolbar controls (toggles and actions) for Context Navigator view."
  :group 'context-navigator)

(defun context-navigator-view-controls--build-toggles ()
  "Build toggle segments (push→gptel, auto-project, undo/redo)."
  (context-navigator-view--make-toggle-segments))

(defun context-navigator-view-controls--build-actions ()
  "Build action segments (Open/Close buffers, Clear group, Push now, Toggle all gptel).

Note: This implementation reuses the legacy footer segments builder and
filters only action segments to keep behavior identical."
  (let ((all (ignore-errors (context-navigator-view--footer-control-segments))))
    (cl-remove-if-not
     (lambda (s) (and (stringp s)
                      (get-text-property 0 'context-navigator-action s)))
     (or all '()))))

(defun context-navigator-view-controls-segments ()
  "Return all control segments (toggles and actions).

Prefers the legacy builder from the view when available to preserve behavior."
  (or (ignore-errors (context-navigator-view--footer-control-segments))
      (append (context-navigator-view-controls--build-toggles)
              (context-navigator-view-controls--build-actions))))

(defun context-navigator-view-controls-lines (total-width)
  "Return wrapped control lines (legacy inline toolbar) for TOTAL-WIDTH.

Reuses the original header-toggle-lines from the view for consistent wrapping."
  (context-navigator-view--header-toggle-lines total-width))

(provide 'context-navigator-view-controls)
;;; context-navigator-view-controls.el ends here
