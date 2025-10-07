;;; context-navigator-view-navigation.el --- Navigation helpers for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Extracted TAB and property-based navigation helpers from context-navigator-view.el
;; to reduce the size of the main view module and clarify responsibilities.
;;
;; This module hosts:
;; - low-level interactive segment scanners
;; - TAB-next/TAB-previous movement (title/stats/items/groups lines)
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
  "Return nearest position >= START with either an item or a group line."
  (let* ((start (or start (point)))
         (p1 (text-property-not-all start (point-max) 'context-navigator-item nil))
         (p3 (text-property-not-all start (point-max) 'context-navigator-group-slug nil))
         (cands (delq nil (list p1 p3))))
    (when cands (apply #'min cands))))

(defun context-navigator-view--find-prev-itemish-pos (&optional start)
  "Return nearest position < START with either an item or a group line."
  (let* ((start (or start (point)))
         (pos nil)
         (best nil))
    (setq pos (context-navigator-view--find-next-itemish-pos (point-min)))
    (while (and pos (< pos start))
      (setq best pos)
      (setq pos (context-navigator-view--find-next-itemish-pos (1+ pos))))
    best))

;; Unified element scanning (treat title/items/groups/toggles as navigable)

(defun context-navigator-view--ui-props ()
  "Return the list of properties that mark navigable UI elements."
  '(context-navigator-interactive
    context-navigator-item
    context-navigator-group-slug
    context-navigator-action
    context-navigator-toggle))

(defun context-navigator-view--run-end (pos)
  "Return end (exclusive) of the current UI element run at POS."
  (let* ((props (context-navigator-view--ui-props))
         (ends  (delq nil
                      (mapcar (lambda (p)
                                (when (get-text-property pos p)
                                  (or (next-single-property-change pos p nil (point-max))
                                      (point-max))))
                              props)))
         (res   (if ends (apply #'max (1+ pos) ends) (1+ pos))))
    res))

(defun context-navigator-view--run-beg (pos)
  "Return beginning (inclusive) of the current UI element run at POS."
  (let* ((props (context-navigator-view--ui-props))
         (begs  (delq nil
                      (mapcar (lambda (p)
                                (when (get-text-property pos p)
                                  (or (previous-single-property-change pos p nil (point-min))
                                      (point-min))))
                              props)))
         (res   (if begs (apply #'min pos begs) pos)))
    res))

(defun context-navigator-view--find-next-ui-pos (&optional start)
  "Return nearest position >= START that has any UI element property.

Exclusive semantics: if START is inside a UI run, skip to the end of that run
before searching. Also clamps START to buffer bounds."
  (let* ((start0 (or start (point)))
         (start  (min (point-max) (max (point-min) start0)))
         (props  (context-navigator-view--ui-props))
         (inside (cl-some (lambda (p) (get-text-property start p)) props))
         ;; Make search exclusive relative to current run.
         (start  (if inside
                     (context-navigator-view--run-end start)
                   start))
         (cands  (delq nil
                       (mapcar (lambda (p)
                                 (text-property-not-all start (point-max) p nil))
                               props))))
    (when cands (apply #'min cands))))

(defun context-navigator-view--find-prev-ui-pos (&optional start)
  "Return nearest position < START that has any UI element property."
  (let* ((start0 (or start (point)))
         (pos    (context-navigator-view--find-next-ui-pos (point-min)))
         (best   nil))
    (while (and pos (< pos start0))
      (setq best pos)
      (setq pos (context-navigator-view--find-next-ui-pos (1+ pos))))
    best))

(defun context-navigator-view--move-next-interactive ()
  "Move to the next interactive element without toggling title/stats; wraps."
  (interactive)
  (let* ((here (point))
         (props (context-navigator-view--ui-props))
         (cur-end (if (cl-some (lambda (p) (get-text-property here p)) props)
                      (context-navigator-view--run-end here)
                    (1+ here)))
         (cur-end (min (point-max) cur-end))
         (pos (context-navigator-view--find-next-ui-pos cur-end)))
    (cond
     (pos
      (goto-char pos))
     (t
      ;; Wrap to the first UI element from buffer start.
      (setq pos (context-navigator-view--find-next-ui-pos (point-min)))
      (when pos (goto-char pos))))))

;;;###autoload
(defun context-navigator-view-tab-next ()
  "TAB-style navigation: jump to next interactive UI element."
  (interactive)
  (context-navigator-view--move-next-interactive))

;;;###autoload
(defun context-navigator-view-tab-previous ()
  "S-TAB-style navigation: jump to previous interactive UI element."
  (interactive)
  (let* ((here (point))
         (beg  (context-navigator-view--run-beg here))
         (pos  (context-navigator-view--find-prev-ui-pos beg)))
    (when pos (goto-char pos))))

;;;###autoload
(defun context-navigator-view-next-item ()
  "Select next item in sidebar."
  (interactive)
  (let* ((here (point))
         (props '(context-navigator-item context-navigator-group-slug))
         (inside (cl-some (lambda (p) (get-text-property here p)) props))
         ;; Exclusive semantics: if we're on an item/group line, start after its run
         (start (if inside
                    (context-navigator-view--run-end here)
                  (min (point-max) (1+ here))))
         (pos (context-navigator-view--find-next-itemish-pos start)))
    ;; Wrap to the first item/group when none found ahead
    (unless pos
      (setq pos (context-navigator-view--find-next-itemish-pos (point-min))))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

;;;###autoload
(defun context-navigator-view-previous-item ()
  "Select previous item in sidebar."
  (interactive)
  (let* ((here (point))
         ;; Exclusive semantics: if we're on an item/group line, use run-beg as the boundary
         (start (context-navigator-view--run-beg here))
         (pos (context-navigator-view--find-prev-itemish-pos start)))
    ;; Wrap to the last item/group when none found before
    (unless pos
      (let ((p (context-navigator-view--find-next-itemish-pos (point-min)))
            (last nil))
        (while p
          (setq last p)
          (setq p (context-navigator-view--find-next-itemish-pos (1+ p))))
        (setq pos last)))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

(provide 'context-navigator-view-navigation)
;;; context-navigator-view-navigation.el ends here
