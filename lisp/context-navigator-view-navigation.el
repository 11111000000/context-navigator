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
  "Return nearest position >= START with either an item, a group line, or the \"..\" up marker."
  (let* ((start (or start (point)))
         (p1 (text-property-not-all start (point-max) 'context-navigator-item nil))
         (p2 (text-property-not-all start (point-max) 'context-navigator-groups-up nil))
         (p3 (text-property-not-all start (point-max) 'context-navigator-group-slug nil))
         (cands (delq nil (list p1 p2 p3))))
    (when cands (apply #'min cands))))

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

;; Sticky current-line highlight to avoid hl-line flicker in Navigator buffers
(defvar-local context-navigator-view--line-ov nil)
(defun context-navigator-view--highlight-current-line ()
  "Highlight the current line with an overlay (stable across log updates)."
  (unless (overlayp context-navigator-view--line-ov)
    (setq context-navigator-view--line-ov (make-overlay (point) (point) nil t t))
    (overlay-put context-navigator-view--line-ov 'priority 1000)
    (overlay-put context-navigator-view--line-ov 'face 'hl-line))
  (move-overlay context-navigator-view--line-ov
                (line-beginning-position)
                (min (point-max) (1+ (line-end-position)))))

;; Unified element scanning (treat title/items/up/toggles as navigable)

(defun context-navigator-view--ui-props ()
  "Return the list of properties that mark navigable UI elements."
  '(context-navigator-interactive
    context-navigator-item
    context-navigator-group-slug
    context-navigator-action
    context-navigator-toggle
    context-navigator-groups-up))

(defun context-navigator-view--find-next-ui-pos (&optional start)
  "Return nearest position >= START that has any UI element property.

Exclusive semantics: if START is inside a UI run, skip to the end of that run
before searching. Also clamps START to buffer bounds. Emits a lightweight
debug trace when `context-navigator-debug' is available."
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
                               props)))
         (res    (when cands (apply #'min cands))))
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :trace :ui
                                 "find-next-ui-pos: start0=%d inside=%s start=%d cands=%S -> %S"
                                 start0 (and inside t) start cands res)))
    res))

(defun context-navigator-view--find-prev-ui-pos (&optional start)
  "Return nearest position < START that has any UI element property."
  (let* ((start0 (or start (point)))
         (pos    (context-navigator-view--find-next-ui-pos (point-min)))
         (best   nil)
         (steps  0))
    (while (and pos (< pos start0))
      (setq best pos)
      (setq pos (context-navigator-view--find-next-ui-pos (1+ pos)))
      (setq steps (1+ steps)))
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :trace :ui
                                 "find-prev-ui-pos: start=%d steps=%d -> %S"
                                 start0 steps best)))
    best))

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
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :trace :ui "run-end: pos=%d ends=%S -> %d" pos ends res)))
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
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :trace :ui "run-beg: pos=%d begs=%S -> %d" pos begs res)))
    res))

(defun context-navigator-view--move-next-interactive ()
  "Move to the next interactive element without toggling title/stats; wraps.

This variant clamps computed endpoints to buffer bounds and emits a debug
trace to help track down cases when no next element is found."
  (interactive)
  (let* ((here (point))
         (props (context-navigator-view--ui-props))
         (cur-end (if (cl-some (lambda (p) (get-text-property here p)) props)
                      (context-navigator-view--run-end here)
                    (1+ here)))
         ;; Clamp cur-end so it never exceeds point-max.
         (cur-end (min (point-max) cur-end))
         (pos (context-navigator-view--find-next-ui-pos cur-end)))
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :trace :ui "move-next-interactive: here=%d cur-end=%d pos=%S" here cur-end pos)))
    ;; Если нашли следующий сегмент на этой же строке — перепрыгиваем на следующую строку
    (when pos
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        (when (and (>= pos bol) (<= pos eol))
          (let ((pos2 (context-navigator-view--find-next-ui-pos (min (point-max) (1+ eol)))))
            (ignore-errors
              (when (fboundp 'context-navigator-debug)
                (context-navigator-debug :trace :ui
                                         "move-next-interactive: same-line pos=%d bol=%d eol=%d -> retry pos2=%S"
                                         pos bol eol pos2)))
            (setq pos pos2)))))
    (unless pos
      ;; wrap к первому интерактивному элементу
      (setq pos (context-navigator-view--find-next-ui-pos (point-min))))
    (if pos
        (let ((run-beg (context-navigator-view--run-beg pos)))
          (goto-char (or run-beg pos))
          ;; Не прыгаем в начало строки — остаемся на начале интерактивного отрезка
          (ignore-errors
            (when (fboundp 'context-navigator-debug)
              (context-navigator-debug :trace :ui
                                       "move-next-interactive: goto=%d run-beg=%d"
                                       (point) run-beg))))
      (context-navigator-ui-info :no-interactive-elements))))

(defun context-navigator-view-tab-next ()
  "Move to the next interactive element; wraps."
  (interactive)
  (context-navigator-view--move-next-interactive))

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

Переходит строго к следующей строке с элементом/группой или \"..\"."
  (interactive)
  (let* ((here (point))
         (bol  (line-beginning-position))
         (eol  (line-end-position))
         ;; Ищем следующий элемент после конца текущей строки
         (pos (context-navigator-view--find-next-itemish-pos (min (point-max) (1+ eol))))
         (wrapped nil))
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :trace :ui
                                 "view-next-item: here=%d bol=%d eol=%d -> pos=%S"
                                 here bol eol pos)))
    (unless pos
      (setq pos (context-navigator-view--find-next-itemish-pos (point-min)))
      (setq wrapped t))
    (if pos
        (progn
          (goto-char pos)
          (beginning-of-line)
          (context-navigator-view--highlight-current-line)
          (ignore-errors
            (when (fboundp 'context-navigator-debug)
              (context-navigator-debug :trace :ui
                                       "view-next-item: goto=%d wrapped=%s"
                                       (point) wrapped))))
      (context-navigator-ui-info :no-interactive-elements))))

(defun context-navigator-view-previous-item ()
  "Move to the previous interactive element (wrap to last).

Переходит строго к предыдущей строке с элементом/группой или \"..\"."
  (interactive)
  (let* ((here (point))
         (bol  (line-beginning-position))
         ;; Ищем предыдущий элемент перед началом текущей строки
         (pos (context-navigator-view--find-prev-itemish-pos bol))
         (wrapped nil))
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :trace :ui
                                 "view-prev-item: here=%d bol=%d -> pos=%S"
                                 here bol pos)))
    (unless pos
      ;; wrap к последнему элементу
      (setq pos (context-navigator-view--find-prev-itemish-pos (point-max)))
      (setq wrapped t))
    (if pos
        (progn
          (goto-char pos)
          (beginning-of-line)
          (context-navigator-view--highlight-current-line)
          (ignore-errors
            (when (fboundp 'context-navigator-debug)
              (context-navigator-debug :trace :ui
                                       "view-prev-item: goto=%d wrapped=%s"
                                       (point) wrapped))))
      (context-navigator-ui-info :no-interactive-elements))))

(provide 'context-navigator-view-navigation)
;;; context-navigator-view-navigation.el ends here
