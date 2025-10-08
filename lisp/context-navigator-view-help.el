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
(autoload 'context-navigator-groups-split-transient "context-navigator-transient"
  "Open Groups Split transient menu." t)

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
        (princ (context-navigator-i18n :items-help-help)) (princ "\n"))
      (when (eq ctx 'groups)
        (princ (context-navigator-i18n :help-groups-summary)) (princ "\n")))))

;;;###autoload
(defun context-navigator-view-open-menu ()
  "Open Navigator menu.
When the sidebar (Navigator view) is visible or active, open the View transient.
Otherwise open the Global transient. Fallback to Help if transients are unavailable."
  (interactive)
  ;; Make sure transient is available if installed
  (unless (featurep 'transient)
    (require 'transient nil t))
  ;; Best-effort load of our transient menus
  (unless (and (fboundp 'context-navigator-view-transient)
               (fboundp 'context-navigator-transient))
    (ignore-errors (require 'context-navigator-transient)))
  (let* ((nav-buf (and (boundp 'context-navigator-view--buffer-name)
                       (get-buffer context-navigator-view--buffer-name)))
         (sidebar-win (and nav-buf (get-buffer-window nav-buf t)))
         (focused-in-sidebar (and sidebar-win (eq (selected-window) sidebar-win)))
         (in-view (or focused-in-sidebar (eq major-mode 'context-navigator-view-mode))))
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :info :ui
                                 "open-menu: in-view=%s sidebar=%s focused=%s"
                                 (and in-view t) (and sidebar-win t) (and focused-in-sidebar t))))
    (cond
     ;; Groups split buffer → Groups-split transient
     ((eq major-mode 'context-navigator-groups-split-mode)
      (if (fboundp 'context-navigator-groups-split-transient)
          (call-interactively 'context-navigator-groups-split-transient)
        (call-interactively 'context-navigator-view-help)))
     ;; Sidebar is visible/active → View transient
     (in-view
      (if (fboundp 'context-navigator-view-transient)
          (call-interactively 'context-navigator-view-transient)
        (call-interactively 'context-navigator-view-help)))
     ;; Else → Global transient (panel/project/actions/control/logs)
     ((fboundp 'context-navigator-transient)
      (call-interactively 'context-navigator-transient))
     ;; Fallback to Help
     (t
      (call-interactively 'context-navigator-view-help)))))

;; (removed) Obsolete audit-keepalive block; help now derives keys from keyspec.

(provide 'context-navigator-view-help)
;;; context-navigator-view-help.el ends here
