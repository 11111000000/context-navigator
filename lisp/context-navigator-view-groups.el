;;; context-navigator-view-groups.el --- Groups rendering for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Extracted groups rendering helpers from context-navigator-view.el.
;; These functions assume they run in the Navigator buffer context and
;; operate on buffer-local variables populated by the view (e.g.
;; `context-navigator-view--groups', `context-navigator-view--collapsed-p',
;; etc). They are safe to require from the view module to avoid cycles.

;;; Code:

(require 'subr-x)
(require 'context-navigator-i18n)
(require 'context-navigator-icons)
(require 'context-navigator-render)

(require 'context-navigator-persist)
(require 'context-navigator-stats)
(require 'context-navigator-core)

;;;###autoload
(defun context-navigator-view-groups-header-lines (_header _total-width)
  "No inline title in groups view; return empty list."
  '())

;;;###autoload
(defun context-navigator-view-groups-body-lines (state)
  "Return list of lines for groups body using STATE.
Each group shows:
- a colored indicator ●/◐/○ by enabled ratio (green/orange/gray)
- selection marker [*]/[ ] persisted in state.el (:selected)
- display name and items count."
  (let* ((active (and (context-navigator-state-p state)
                      (context-navigator-state-current-group-slug state)))
         (root   (and (context-navigator-state-p state)
                      (context-navigator-state-last-project-root state)))
         (pstate (or (ignore-errors (context-navigator-persist-state-load root)) '()))
         (selected (and (plist-member pstate :selected) (plist-get pstate :selected)))
         (selected (if (listp selected) selected '()))
         (mg (and (listp pstate) (plist-member pstate :multi) (plist-get pstate :multi))))
    (cond
     ((not (listp context-navigator-view--groups))
      (list (propertize (context-navigator-i18n :no-groups) 'face 'shadow)))
     (t
      (let (lines)
        (dolist (pl context-navigator-view--groups)
          (let* ((slug (or (plist-get pl :slug) ""))
                 (disp (or (plist-get pl :display) slug))
                 (path (or (plist-get pl :path) nil))
                 (cnt  (or (ignore-errors (context-navigator-persist-group-item-count path)) 0))
                 (en.t (or (ignore-errors (context-navigator-persist-group-enabled-count path))
                           (cons 0 0)))
                 (en   (car en.t))
                 (tot  (cdr en.t))
                 (sel-p (member slug selected))
                 ;; Индикатор показываем только для выбранных групп
                 (indi (when sel-p
                         (cond
                          ((and (integerp tot) (> tot 0) (= en tot))
                           (propertize "●" 'face '(:foreground "green4")))
                          ((and (integerp en) (> en 0))
                           (propertize "◐" 'face '(:foreground "orange2")))
                          (t
                           (propertize "○" 'face '(:foreground "gray55"))))))
                 ;; Пиктограмма «папка»
                 (gico "📁")
                 ;; Счётчик (enabled/total), где enabled — зелёный
                 (en-str (propertize (format "%d" (max 0 (or en 0)))
                                     'face '(:foreground "green4")))
                 (cnt-str (format "%s/%d" en-str (max 0 (or tot 0))))
                 (prefix (string-trim (mapconcat #'identity (delq nil (list indi gico)) " ")))
                 (s (concat prefix " " disp " (" cnt-str ")")))
            ;; Базовые интерактивные свойства на всю строку
            (add-text-properties 0 (length s)
                                 (list 'context-navigator-group-slug slug
                                       'context-navigator-group-display disp
                                       'context-navigator-interactive t
                                       'mouse-face 'highlight
                                       'keymap context-navigator-view--group-line-keymap
                                       'local-map context-navigator-view--group-line-keymap
                                       'help-echo (context-navigator-i18n :mouse-open-group))
                                 s)
            ;; Подсвечивать активную группу только на имени (не перетирать цвет индикатора)
            (when (and context-navigator-highlight-active-group
                       active (string= active slug))
              (let ((beg (length prefix))
                    (end (+ (length prefix) (length disp))))
                (when (<= 0 beg end (length s))
                  (add-text-properties beg end (list 'face 'mode-line-emphasis) s))))
            (setq lines (append lines (list s)))))
        lines)))))

;;;###autoload
(defun context-navigator-view--groups-help-lines (_total-width)
  "Return minimal footer hint for groups view (transient provides full help)."
  (let* ((hint (propertize (context-navigator-i18n :menu-hint) 'face 'shadow)))
    (list "" hint)))

;;;###autoload
(defun context-navigator-view-render-groups (state header total-width)
  "Render groups view using STATE and TOTAL-WIDTH.
No inline title or stats in the buffer; only the groups list and a minimal hint."
  (let* ((groups-lines (context-navigator-view-groups-body-lines state))
         (help-lines (context-navigator-view--groups-help-lines total-width))
         ;; No collapsible body and no inline title in the buffer
         (lines (append groups-lines help-lines)))
    (setq context-navigator-view--last-lines lines
          context-navigator-view--header header)
    (context-navigator-render-apply-to-buffer (current-buffer) lines)
    ;; Simple, robust focus: only when explicitly requested once.
    ;; Do not move point on incidental re-renders or window switches.
    (unless context-navigator-view--collapsed-p
      (when (and (boundp 'context-navigator-view--focus-group-once)
                 context-navigator-view--focus-group-once)
        (let* ((want (if (stringp context-navigator-view--focus-group-once)
                         context-navigator-view--focus-group-once
                       (and (context-navigator-state-p state)
                            (context-navigator-state-current-group-slug state)))))
          (let ((pos nil))
            (when (and (stringp want) (not (string-empty-p want)))
              (let ((p (point-min)) (found nil))
                (while (and (not found)
                            (setq p (text-property-not-all p (point-max)
                                                           'context-navigator-group-slug nil)))
                  (when (equal (get-text-property p 'context-navigator-group-slug) want)
                    (setq found p))
                  (setq p (1+ p)))
                (setq pos found)))
            ;; Fallback: first group line if requested slug wasn't found.
            (unless pos
              (setq pos (text-property-not-all (point-min) (point-max)
                                               'context-navigator-group-slug nil)))
            (when pos
              (goto-char pos)
              (beginning-of-line)))))
      ;; Consume the one-shot focus request.
      (setq context-navigator-view--focus-group-once nil))
    lines))

(provide 'context-navigator-view-groups)
;;; context-navigator-view-groups.el ends here
