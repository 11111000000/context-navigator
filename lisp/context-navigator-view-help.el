;;; context-navigator-view-help.el --- Help and menu for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Extracted help window and menu opener from context-navigator-view.el
;; to reduce the size of the main module and clarify responsibilities.

;;; Code:

(require 'subr-x)
(require 'context-navigator-i18n)
(require 'context-navigator-keyspec)

(defvar context-navigator-view-mode-map)

;;;###autoload
(autoload 'context-navigator-view-transient "context-navigator-transient"
  "Open Context Navigator transient menu." t)

;;;###autoload
(defun context-navigator-view-help ()
  "Show localized, sectioned help for Context Navigator based on the central keyspec."
  (interactive)
  (with-help-window "*Context Navigator Help*"
    (let* ((nav-buf (get-buffer "*context-navigator*"))
           (mode (with-current-buffer (or nav-buf (current-buffer))
                   (if (boundp 'context-navigator-view--mode)
                       context-navigator-view--mode
                     'items)))
           (print-section
            (lambda (title pairs)
              (when (and pairs (> (length pairs) 0))
                (princ title) (princ "\n")
                (let* ((keyw (apply #'max 0 (mapcar (lambda (x) (string-width (car x))) pairs)))
                       (fmt (format "  %%-%ds — %%s\n" (max 2 keyw))))
                  (dolist (cell pairs)
                    (princ (format fmt (car cell) (cdr cell)))))
                (princ "\n"))))
           (flatten
            (lambda (alist)
              (let (acc)
                (dolist (sec alist (nreverse acc))
                  (dolist (p (cdr sec))
                    (push p acc))))))
           (localize-section
            (lambda (sec)
              (pcase sec
                ('navigate (context-navigator-i18n :tr-navigate))
                ('act      (context-navigator-i18n :tr-actions))
                ('groups   (context-navigator-i18n :tr-groups))
                ('session  (context-navigator-i18n :tr-session))
                (_         "Tools"))))
           (order '(navigate act groups session tools))
           (global-help (and (fboundp 'context-navigator-keys-help)
                             (context-navigator-keys-help 'global)))
           (ctx (if (eq mode 'groups) 'groups 'items))
           (ctx-help (and (fboundp 'context-navigator-keys-help)
                          (context-navigator-keys-help ctx))))
      ;; Title
      (princ (context-navigator-i18n :help-title)) (princ "\n\n")
      ;; Global keys
      (funcall print-section
               (context-navigator-i18n :help-global-title)
               (funcall flatten global-help))
      ;; Context keys (Items/Groups)
      (princ (if (eq ctx 'groups) (context-navigator-i18n :tr-groups)
               (context-navigator-i18n :tr-items)))
      (princ "\n\n")
      (dolist (sec order)
        (let* ((block (assoc sec ctx-help))
               (pairs (and block (cdr block)))
               (title (funcall localize-section sec)))
          (when pairs
            (funcall print-section title pairs))))
      ;; Small summaries
      (when (eq ctx 'items)
        (princ (context-navigator-i18n :items-help-view-groups)) (princ (context-navigator-i18n :items-help-help)) (princ "\n"))
      (when (eq ctx 'groups)
        (princ (context-navigator-i18n :help-groups-summary)) (princ "\n")))))

;;;###autoload
(defun context-navigator-view-open-menu ()
  "Open Navigator menu (transient) or fallback to Help when unavailable."
  (interactive)
  ;; Make sure transient is available if installed
  (unless (featurep 'transient)
    (require 'transient nil t))
  ;; Best-effort load of our transient menu
  (unless (fboundp 'context-navigator-view-transient)
    (ignore-errors (require 'context-navigator-transient)))
  (if (fboundp 'context-navigator-view-transient)
      (call-interactively 'context-navigator-view-transient)
    (call-interactively 'context-navigator-view-help)))

;; --- Audit keepalive (dynamic help keys) -------------------------------------
;; This block never runs at runtime, but helps audit scanner account for keys
;; that are passed dynamically via (cdr cell) in this file.
(when nil
  (context-navigator-i18n :help-next-item)
  (context-navigator-i18n :help-previous-item)
  (context-navigator-i18n :help-activate)
  (context-navigator-i18n :help-preview)
  (context-navigator-i18n :help-toggle-gptel)
  (context-navigator-i18n :help-delete)
  (context-navigator-i18n :help-refresh)
  (context-navigator-i18n :help-go-up)
  (context-navigator-i18n :help-group-create)
  (context-navigator-i18n :help-group-rename)
  (context-navigator-i18n :help-group-duplicate)
  (context-navigator-i18n :help-toggle-push)
  (context-navigator-i18n :help-toggle-auto)
  (context-navigator-i18n :help-open-all)
  (context-navigator-i18n :help-push-now)
  (context-navigator-i18n :help-clear-group)
  (context-navigator-i18n :help-clear-gptel)
  (context-navigator-i18n :help-quit)
  (context-navigator-i18n :help-help))

(provide 'context-navigator-view-help)
;;; context-navigator-view-help.el ends here
