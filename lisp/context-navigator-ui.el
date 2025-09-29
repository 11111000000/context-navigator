;;; context-navigator-ui.el --- Small UI wrappers (i18n prompts/messages) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Minimal helpers to unify user prompts/messages with i18n:
;; - context-navigator-ui-ask    : yes-or-no-p with localized prompt
;; - context-navigator-ui-info   : message with localized text
;; - context-navigator-ui-warn   : message with localized text (warning tone)
;; - context-navigator-ui-error  : user-error with localized text
;;
;; These helpers reduce duplication and make it easy to audit i18n use.

;;; Code:

(require 'subr-x)
(require 'context-navigator-i18n)

(defun context-navigator-ui--fmt (key args)
  "Return formatted localized string for KEY with ARGS."
  (apply #'format (context-navigator-i18n key) args))

;;;###autoload
(defun context-navigator-ui-ask (key &rest args)
  "Ask a yes-or-no question using i18n KEY and ARGS."
  (yes-or-no-p (apply #'format (context-navigator-i18n key) args)))

;;;###autoload
(defun context-navigator-ui-info (key &rest args)
  "Show an informational message using i18n KEY and ARGS."
  (let ((fmt (context-navigator-i18n key)))
    (apply #'message fmt args)))

;;;###autoload
(defun context-navigator-ui-warn (key &rest args)
  "Show a warning-styled message using i18n KEY and ARGS."
  (let ((fmt (context-navigator-i18n key)))
    (apply #'message (concat "⚠ " fmt) args)))

;;;###autoload
(defun context-navigator-ui-error (key &rest args)
  "Signal a user-error using i18n KEY and ARGS."
  (let ((fmt (context-navigator-i18n key)))
    (apply #'user-error fmt args)))

(provide 'context-navigator-ui)
;;; context-navigator-ui.el ends here
