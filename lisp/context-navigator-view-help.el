;;; context-navigator-view-help.el --- Help and menu for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Extracted help window and menu opener from context-navigator-view.el
;; to reduce the size of the main module and clarify responsibilities.

;;; Code:

(require 'subr-x)
(require 'context-navigator-i18n)

(defvar context-navigator-view-mode-map)

;;;###autoload
(autoload 'context-navigator-view-transient "context-navigator-transient"
  "Open Context Navigator transient menu." t)

;;;###autoload
(defun context-navigator-view-help ()
  "Show localized, column-formatted help for Context Navigator without truncation."
  (interactive)
  (with-help-window "*Context Navigator Help*"
    (let* ((map context-navigator-view-mode-map)
           ;; Command → i18n key for description
           (pairs '((context-navigator-view-next-item         . :help-next-item)
                    (context-navigator-view-previous-item     . :help-previous-item)
                    (context-navigator-view-activate          . :help-activate)
                    (context-navigator-view-preview           . :help-preview)
                    (context-navigator-view-toggle-enabled     . :help-toggle-gptel)
                    (context-navigator-view-delete-dispatch   . :help-delete)
                    (context-navigator-view-refresh-dispatch  . :help-refresh)
                    (context-navigator-view-go-up             . :help-go-up)
                    (context-navigator-view-group-create      . :help-group-create)
                    (context-navigator-view-group-rename      . :help-group-rename)
                    (context-navigator-view-group-duplicate   . :help-group-duplicate)
                    (context-navigator-view-toggle-push       . :help-toggle-push)
                    (context-navigator-view-toggle-auto-project . :help-toggle-auto)
                    (context-navigator-view-open-all-buffers  . :help-open-all)
                    (context-navigator-view-push-now          . :help-push-now)
                    (context-navigator-view-clear-group       . :help-clear-group)
                    (context-navigator-view-clear-gptel       . :help-clear-gptel)
                    (context-navigator-view-quit              . :help-quit)
                    (context-navigator-view-help              . :help-help)))
           ;; Build (keys . desc) then padded line strings
           (rows-raw
            (mapcar
             (lambda (cell)
               (let* ((cmd  (car cell))
                      (desc (context-navigator-i18n (cdr cell)))
                      (keys (mapcar #'key-description (where-is-internal cmd map)))
                      (ks   (if keys (string-join keys ", ") "<unbound>")))
                 (cons ks desc)))
             pairs))
           (keyw (apply #'max 0 (mapcar (lambda (x) (string-width (car x))) rows-raw)))
           (lines (mapcar (lambda (x) (format (format "%%-%ds %%s" (max 14 keyw)) (car x) (cdr x)))
                          rows-raw))
           ;; Detect/help the real window width for proper column calculation.
           (ww (let* ((buf "*Context Navigator Help*")
                      (win (or (get-buffer-window buf t)
                               (get-buffer-window (current-buffer) t)))
                      (maxw (apply #'max 80 (mapcar #'window-body-width (window-list)))))
                 (or (and win (window-body-width win))
                     maxw
                     (frame-width)
                     80)))
           (spacing "  ")
           ;; Try to place in 3/2/1 columns to fit without truncation.
           (choose-cols
            (lambda ()
              (let ((n (length lines)))
                (cl-loop for c in '(3 2 1) do
                         (let* ((cols c)
                                (rows (ceiling (/ (float n) (max 1 cols))))
                                ;; compute column widths for this layout
                                (colw
                                 (cl-loop for ci from 0 below cols collect
                                          (let ((w 0))
                                            (cl-loop for ri from 0 below rows
                                                     for idx = (+ ri (* ci rows))
                                                     when (< idx n) do
                                                     (setq w (max w (string-width (nth idx lines)))))
                                            w)))
                                (total (+ (apply #'+ colw) (* (string-width spacing) (1- cols)))))
                           (when (<= total ww)
                             (cl-return cols))))
                1)))  ;; default 1
           (cols (funcall choose-cols))
           (rows (ceiling (/ (float (length lines)) (max 1 cols))))
           ;; Recompute exact per-column widths for chosen layout
           (colw
            (cl-loop for ci from 0 below cols collect
                     (let ((w 0))
                       (cl-loop for ri from 0 below rows
                                for idx = (+ ri (* ci rows))
                                when (< idx (length lines)) do
                                (setq w (max w (string-width (nth idx lines)))))
                       w))))
      ;; Title
      (princ (context-navigator-i18n :help-title)) (princ "\n\n")
      ;; Emit lines as rows×cols grid with padding; no truncation.
      (dotimes (r rows)
        (let ((acc ""))
          (dotimes (c cols)
            (let* ((idx (+ r (* c rows)))
                   (s (or (nth idx lines) ""))
                   (pad (if (< c (1- cols))
                            (format (format "%%-%ds" (nth c colw)) s)
                          s)))
              (setq acc (if (string-empty-p acc) pad (concat acc spacing pad)))))
          (princ acc) (princ "\n")))
      (princ "\n")
      ;; Global keys section (localized)
      (princ (context-navigator-i18n :help-global-title)) (princ "\n")
      (princ (context-navigator-i18n :help-global-summary)) (princ "\n\n")
      ;; Groups mode summary (localized)
      (princ (context-navigator-i18n :help-groups-summary)) (princ "\n"))))

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
