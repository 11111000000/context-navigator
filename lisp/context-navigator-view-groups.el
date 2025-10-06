;;; context-navigator-view-groups.el --- Groups rendering for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

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

Each group shows a selection indicator (lamp in multi-group mode),
the display name and the items count."
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
                 (sel-p (member slug selected))
                 ;; Lamp indicator like in items (green=selected, gray=not selected)
                 (lamp (when mg
                         (ignore-errors (context-navigator-indicator-string sel-p t))))
                 (gico "📁")
                 (cnt-str (format "%d" (max 0 (or cnt 0))))
                 (prefix (string-trim
                          (mapconcat #'identity
                                     (delq nil (if mg (list lamp gico) (list gico)))
                                     " ")))
                 (s (concat prefix " " disp " (" cnt-str ")")))
            (add-text-properties 0 (length s)
                                 (list 'context-navigator-group-slug slug
                                       'context-navigator-group-display disp
                                       'context-navigator-interactive t
                                       'mouse-face 'highlight
                                       'keymap context-navigator-view--group-line-keymap
                                       'local-map context-navigator-view--group-line-keymap
                                       'help-echo (if mg
                                                      (context-navigator-i18n :toggle-multi-group)
                                                    (context-navigator-i18n :mouse-open-group)))
                                 s)
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
  '())

;;;###autoload
(defun context-navigator-view-render-groups (state header total-width)
  "Render groups view using STATE and TOTAL-WIDTH.
Title is shown in the header-line; the buffer shows controls (top), groups, and a minimal hint."
  (let* ((groups-lines (context-navigator-view-groups-body-lines state))
         (help-lines (context-navigator-view--groups-help-lines total-width))
         (lines (append groups-lines help-lines)))
    (setq context-navigator-view--last-lines lines
          context-navigator-view--header header)
    (context-navigator-render-apply-to-buffer (current-buffer) lines)
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
            (unless pos
              (setq pos (text-property-not-all (point-min) (point-max)
                                               'context-navigator-group-slug nil)))
            (when pos
              (goto-char pos)
              (beginning-of-line)))))
      (setq context-navigator-view--focus-group-once nil))
    lines))

(provide 'context-navigator-view-groups)
;;; context-navigator-view-groups.el ends here
