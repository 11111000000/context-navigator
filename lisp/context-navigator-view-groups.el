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
(require 'context-navigator-view-title)
(require 'context-navigator-persist)
(require 'context-navigator-stats)

;;;###autoload
(defun context-navigator-view-groups-header-lines (header total-width)
  "Return list with a single clickable title line for groups view.

Shows only [project] and supports TAB/RET/mouse-1 collapse like items."
  (list (context-navigator-view--title-line header)))

;;;###autoload
(defun context-navigator-view-groups-body-lines (state)
  "Return list of lines for groups body using STATE.
Each group shows display name with item count in parentheses."
  (let* ((active (and (context-navigator-state-p state)
                      (context-navigator-state-current-group-slug state))))
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
                 (ic (or (ignore-errors (context-navigator-icons-for-group)) "📁"))
                 (txt (format "%s %s (%d)" ic disp cnt))
                 (s (copy-sequence txt)))
            (add-text-properties 0 (length s)
                                 (list 'context-navigator-group-slug slug
                                       'context-navigator-group-display disp
                                       'context-navigator-interactive t
                                       'mouse-face 'highlight
                                       'keymap context-navigator-view--group-line-keymap
                                       'local-map context-navigator-view--group-line-keymap
                                       'help-echo (context-navigator-i18n :mouse-open-group))
                                 s)
            (when (and context-navigator-highlight-active-group
                       active (string= active slug))
              (add-text-properties 0 (length s) (list 'face 'mode-line-emphasis) s))
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
         (lines (append (list "") groups-lines help-lines)))
    (setq context-navigator-view--last-lines lines
          context-navigator-view--header header)
    (context-navigator-render-apply-to-buffer (current-buffer) lines)
    ;; Keep point where user navigated with n/p; only auto-focus when:
    ;; - entering the groups view (first render in this mode), or
    ;; - the active group changed since last render.
    ;; Do not steal point when user moves to footer/help with TAB/Shift-TAB.
    (unless context-navigator-view--collapsed-p
      (let* ((active (and (context-navigator-state-p state)
                          (context-navigator-state-current-group-slug state)))
             (here (point))
             (on-group (get-text-property here 'context-navigator-group-slug))
             (need-focus (or (not (eq context-navigator-view--last-mode 'groups))
                             (not (equal context-navigator-view--last-active-group active)))))
        (when need-focus
          (let (pos)
            (when (and (stringp active) (not (string-empty-p active)))
              (let ((p (point-min)) (found nil))
                (while (and (not found)
                            (setq p (text-property-not-all p (point-max) 'context-navigator-group-slug nil)))
                  (when (equal (get-text-property p 'context-navigator-group-slug) active)
                    (setq found p))
                  (setq p (1+ p)))
                (setq pos found)))
            (unless pos
              (setq pos (text-property-not-all (point-min) (point-max)
                                               'context-navigator-group-slug nil)))
            (when pos
              (goto-char pos)
              (beginning-of-line))))
        (setq context-navigator-view--last-active-group active
              context-navigator-view--last-mode 'groups)))
    lines))

(provide 'context-navigator-view-groups)
;;; context-navigator-view-groups.el ends here
