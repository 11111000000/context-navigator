;;; context-navigator-view-navigation.el --- Navigation helpers for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Extracted TAB and property-based navigation helpers from context-navigator-view.el
;; to reduce the size of the main view module and clarify responsibilities.
;;
;; This module hosts:
;; - low-level interactive segment scanners
;; - TAB-next/TAB-previous movement and toggle dispatch on title/stats
;;
;; The main view module should require this file and can declare functions
;; if needed for the byte-compiler.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-i18n)
(require 'context-navigator-ui)

;; Declarations (provided by the main view and stats modules)
(declare-function context-navigator-view-toggle-collapse "context-navigator-view" ())
(declare-function context-navigator-view-stats-toggle "context-navigator-view" ())

(defun context-navigator-view--find-next-interactive-pos (&optional start)
  "Return nearest position >= START with 'context-navigator-interactive."
  (let ((start (or start (point))))
    (text-property-not-all start (point-max) 'context-navigator-interactive nil)))

(defun context-navigator-view--find-prev-interactive-pos (&optional start)
  "Return the beginning of the previous 'context-navigator-interactive run before START.
If START is inside a run, return the beginning of the same run; if START is
exactly at a run start, return the beginning of the previous run. Returns nil
when none exists."
  (let* ((prop 'context-navigator-interactive)
         (pos (or start (point))))
    ;; Start strictly before START to search previous runs.
    (setq pos (max (1- pos) (point-min)))
    (cond
     ;; If currently on an interactive char, jump to its run start.
     ((and (> pos (point-min)) (get-text-property pos prop))
      (or (previous-single-property-change pos prop nil (point-min))
          (point-min)))
     (t
      ;; Walk backwards to the previous boundary where property becomes non-nil,
      ;; then return that run's start (the boundary itself).
      (let (chg)
        (while (and (> pos (point-min))
                    (setq chg (previous-single-property-change pos prop nil (point-min)))
                    (not (and (> chg (point-min)) (get-text-property (1- chg) prop))))
          (setq pos chg chg nil))
        (when (and chg (> chg (point-min)) (get-text-property (1- chg) prop))
          chg))))))

(defun context-navigator-view--find-next-itemish-pos (&optional start)
  "Return nearest position >= START with either an item or the \"..\" up marker."
  (let* ((start (or start (point)))
         (p1 (text-property-not-all start (point-max) 'context-navigator-item nil))
         (p2 (text-property-not-all start (point-max) 'context-navigator-groups-up nil)))
    (cond
     ((and p1 p2) (min p1 p2))
     (p1 p1)
     (p2 p2)
     (t nil))))

(defun context-navigator-view--find-prev-itemish-pos (&optional start)
  "Return nearest position < START with either an item or the \"..\" up marker."
  (let* ((start (or start (point)))
         (pos nil)
         (best nil))
    (setq pos (context-navigator-view--find-next-itemish-pos (point-min)))
    (while (and pos (< pos start))
      (setq best pos)
      (setq pos (context-navigator-view--find-next-itemish-pos (1+ pos))))
    best))

;; Unified element scanning (treat title/items/up/toggles as navigable)

(defun context-navigator-view--ui-props ()
  "Return the list of properties that mark navigable UI elements."
  '(context-navigator-interactive
    context-navigator-title
    context-navigator-item
    context-navigator-group-slug
    context-navigator-action
    context-navigator-toggle
    context-navigator-stats-toggle
    context-navigator-groups-up))

(defun context-navigator-view--find-next-ui-pos (&optional start)
  "Return nearest position >= START that has any UI element property."
  (let* ((start (or start (point)))
         (props (context-navigator-view--ui-props))
         (cands (delq nil
                      (mapcar (lambda (p)
                                (text-property-not-all start (point-max) p nil))
                              props))))
    (when cands (apply #'min cands))))

(defun context-navigator-view--find-prev-ui-pos (&optional start)
  "Return nearest position < START that has any UI element property."
  (let* ((start (or start (point)))
         (pos (context-navigator-view--find-next-ui-pos (point-min)))
         (best nil))
    (while (and pos (< pos start))
      (setq best pos)
      (setq pos (context-navigator-view--find-next-ui-pos (1+ pos))))
    best))

(defun context-navigator-view--run-end (pos)
  "Return end (exclusive) of the current UI element run at POS."
  (let* ((props (context-navigator-view--ui-props))
         (ends (delq nil
                     (mapcar (lambda (p)
                               (when (get-text-property pos p)
                                 (or (next-single-property-change pos p nil (point-max))
                                     (point-max))))
                             props))))
    (if ends (apply #'max (1+ pos) ends) (1+ pos))))

(defun context-navigator-view--run-beg (pos)
  "Return beginning (inclusive) of the current UI element run at POS."
  (let* ((props (context-navigator-view--ui-props))
         (begs (delq nil
                     (mapcar (lambda (p)
                               (when (get-text-property pos p)
                                 (or (previous-single-property-change pos p nil (point-min))
                                     (point-min))))
                             props))))
    (if begs (apply #'min pos begs) pos)))

(defun context-navigator-view--move-next-interactive ()
  "Move to the next interactive element without toggling title/stats; wraps."
  (interactive)
  (let* ((here (point))
         (cur-end (let ((props (context-navigator-view--ui-props)))
                    (if (cl-some (lambda (p) (get-text-property here p)) props)
                        (context-navigator-view--run-end here)
                      (1+ here))))
         (pos (context-navigator-view--find-next-ui-pos cur-end)))
    (unless pos
      ;; wrap to the first UI element
      (setq pos (context-navigator-view--find-next-ui-pos (point-min))))
    (if pos
        (goto-char pos)
      (context-navigator-ui-info :no-interactive-elements))))

(defun context-navigator-view-tab-next ()
  "Move point to the next interactive element.

On title line: toggle collapse. On Stats header: toggle stats. Wrap at end."
  (interactive)
  (let* ((here (point)))
    (when (get-text-property here 'context-navigator-title)
      (context-navigator-view-toggle-collapse)
      (cl-return-from context-navigator-view-tab-next))
    (when (get-text-property here 'context-navigator-stats-toggle)
      (context-navigator-view-stats-toggle)
      (cl-return-from context-navigator-view-tab-next))
    (let* ((cur-end (if (get-text-property here 'context-navigator-interactive)
                        (or (next-single-property-change here 'context-navigator-interactive nil (point-max))
                            (point-max))
                      (1+ here)))
           (pos (context-navigator-view--find-next-interactive-pos cur-end)))
      (unless pos
        (setq pos (context-navigator-view--find-next-interactive-pos (point-min))))
      (if pos (goto-char pos) (context-navigator-ui-info :no-interactive-elements)))))

(defun context-navigator-view-tab-previous ()
  "Move point to the previous interactive element. Wrap at start."
  (interactive)
  (let* ((here (point))
         (cur-beg (if (get-text-property here 'context-navigator-interactive)
                      (or (previous-single-property-change here 'context-navigator-interactive nil (point-min))
                          (point-min))
                    here))
         (pos (context-navigator-view--find-prev-interactive-pos cur-beg)))
    (unless pos
      (setq pos (context-navigator-view--find-prev-interactive-pos (point-max))))
    (if pos (goto-char pos) (context-navigator-ui-info :no-interactive-elements))))

(defun context-navigator-view-next-item ()
  "Move to the next interactive element (wrap at end).

Ходит последовательно по всем сегментам с 'context-navigator-interactive,
включая заголовки, \"..\", элементы и т.п."
  (interactive)
  (let* ((here (point))
         (cur-end (let ((props (context-navigator-view--ui-props)))
                    (if (cl-some (lambda (p) (get-text-property here p)) props)
                        (context-navigator-view--run-end here)
                      (1+ here))))
         (pos (context-navigator-view--find-next-ui-pos cur-end)))
    (unless pos
      (setq pos (context-navigator-view--find-next-ui-pos (point-min))))
    (when pos (goto-char pos))))

(defun context-navigator-view-previous-item ()
  "Move to the previous interactive element (wrap to last).

Ходит последовательно по всем сегментам с 'context-navigator-interactive,
включая заголовки, \"..\", элементы и т.п."
  (interactive)
  (let* ((here (point))
         (cur-beg (let ((props (context-navigator-view--ui-props)))
                    (if (cl-some (lambda (p) (get-text-property here p)) props)
                        (context-navigator-view--run-beg here)
                      here)))
         (pos (context-navigator-view--find-prev-ui-pos cur-beg)))
    (unless pos
      (setq pos (context-navigator-view--find-prev-ui-pos (point-max))))
    (when pos (goto-char pos))))

(provide 'context-navigator-view-navigation)
;;; context-navigator-view-navigation.el ends here
