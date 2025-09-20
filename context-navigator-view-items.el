;;; context-navigator-view-items.el --- Items rendering & status for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Extracted items rendering helpers and per-point status computation
;; from context-navigator-view.el. These functions are intended to run
;; in the Navigator buffer context and use buffer-local vars defined by the view.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-render)
(require 'context-navigator-i18n)
(require 'context-navigator-icons)
(require 'context-navigator-model)
(require 'context-navigator-core)
(require 'context-navigator-stats)
(require 'context-navigator-view-title)

;;;###autoload
(defun context-navigator-view--items-header-toggle-lines (total-width)
  "Return header toggle lines for items view wrapped to TOTAL-WIDTH.

Prefer the controls module implementation when available; fall back to the
legacy local builder for compatibility."
  (if (fboundp 'context-navigator-view-controls-lines)
      (context-navigator-view-controls-lines total-width)
    ;; Fallback: use the original toggle builder and wrap it locally to avoid requiring the full view.
    (let* ((segments (context-navigator-view--make-toggle-segments)))
      (if (fboundp 'context-navigator-view--wrap-segments)
          (context-navigator-view--wrap-segments segments total-width)
        (let ((acc '())
              (cur ""))
          (dolist (seg segments)
            (let* ((sw (string-width seg))
                   (cw (string-width cur)))
              (if (<= (+ cw sw) total-width)
                  (setq cur (concat cur seg))
                (when (> (length cur) 0)
                  (push cur acc))
                (setq cur seg))))
          (when (> (length cur) 0) (push cur acc))
          (nreverse acc))))))

;;;###autoload
(defun context-navigator-view--items-base-lines (state header total-width)
  "Return a list: (hl sep up rest...) for items view base lines.
HL is the clickable [project[: group]] title line placed above \"..\".
SEP is currently empty (no extra separator in the buffer).
UP is the \"..\" line.
REST is a list of item lines."
  (let* ((items (context-navigator-state-items state))
         ;; generation-aware cached sort: avoid re-sorting identical model generation
         (gen (or (and (context-navigator-state-p state)
                       (context-navigator-state-generation state))
                  0))
         (root (and (context-navigator-state-p state)
                    (context-navigator-state-last-project-root state)))
         ;; relpath cache per generation/root
         (_ (unless (and context-navigator-view--relpaths-hash
                         (= gen (or context-navigator-view--sorted-gen -1))
                         (equal root context-navigator-view--sorted-root))
              (setq context-navigator-view--relpaths-hash (make-hash-table :test 'equal))))
         (relpath-of
          (lambda (it)
            (let* ((key (context-navigator-model-item-key it))
                   (cached (and context-navigator-view--relpaths-hash
                                (gethash key context-navigator-view--relpaths-hash)))
                   (p (and (stringp (context-navigator-item-path it))
                           (context-navigator-item-path it))))
              (or cached
                  (let* ((rel (condition-case _err
                                  (if (and p root)
                                      (file-relative-name (expand-file-name p)
                                                          (file-name-as-directory (expand-file-name root)))
                                    p)
                                (error p))))
                    (when (hash-table-p context-navigator-view--relpaths-hash)
                      (puthash key rel context-navigator-view--relpaths-hash))
                    rel)))))
         ;; natural-lessp (case-insensitive with numeric chunks)
         (natural-lessp
          (lambda (a b)
            (cl-labels
                ((chunks (s)
                   (let ((i 0) (n (length s)) (cur "") (res '()))
                     (while (< i n)
                       (let ((c (aref s i)))
                         (if (and (>= c ?0) (<= c ?9))
                             ;; flush cur then read number
                             (progn
                               (when (> (length cur) 0)
                                 (push (cons :str (downcase cur)) res)
                                 (setq cur ""))
                               (let ((j i))
                                 (while (and (< j n)
                                             (let ((d (aref s j))) (and (>= d ?0) (<= d ?9))))
                                   (setq j (1+ j)))
                                 (push (cons :num (string-to-number (substring s i j))) res)
                                 (setq i (1- j))))
                           (setq cur (concat cur (string c)))))
                       (setq i (1+ i)))
                     (when (> (length cur) 0)
                       (push (cons :str (downcase cur)) res))
                     (nreverse res)))
                 (cmp (xa xb)
                   (cond
                    ((and (null xa) (null xb)) nil)
                    ((null xa) t)
                    ((null xb) nil)
                    (t
                     (let* ((a1 (car xa)) (b1 (car xb)))
                       (cond
                        ((and (eq (car a1) :num) (eq (car b1) :num))
                         (if (/= (cdr a1) (cdr b1))
                             (< (cdr a1) (cdr b1))
                           (cmp (cdr xa) (cdr xb))))
                        ((and (eq (car a1) :str) (eq (car b1) :str))
                         (let ((sa (cdr a1)) (sb (cdr b1)))
                           (if (string= sa sb)
                               (cmp (cdr xa) (cdr xb))
                             (string-lessp sa sb))))
                        ((eq (car a1) :num) t)
                        (t nil)))))))
              (cmp (chunks (or a "")) (chunks (or b ""))))))
         (in-subdir-p
          (lambda (rel)
            (and (stringp rel)
                 (not (string-prefix-p ".." rel))
                 (string-match-p "/" rel))))
         (basename-of
          (lambda (rel-or-path)
            (file-name-nondirectory (or rel-or-path ""))))
         (sorted-items
          (if (and (listp context-navigator-view--sorted-items)
                   (integerp context-navigator-view--sorted-gen)
                   (= gen context-navigator-view--sorted-gen)
                   (equal root context-navigator-view--sorted-root))
              context-navigator-view--sorted-items
            (let ((s (sort (copy-sequence (or items '()))
                           (lambda (a b)
                             (let* ((ra (funcall relpath-of a))
                                    (rb (funcall relpath-of b))
                                    (pa (and (stringp (context-navigator-item-path a))
                                             (context-navigator-item-path a)))
                                    (pb (and (stringp (context-navigator-item-path b))
                                             (context-navigator-item-path b)))
                                    (has-pa (and (stringp pa) (> (length pa) 0)))
                                    (has-pb (and (stringp pb) (> (length pb) 0)))
                                    (sa (and has-pa (funcall in-subdir-p ra)))
                                    (sb (and has-pb (funcall in-subdir-p rb))))
                               (cond
                                ;; Items with paths first; without paths at the bottom
                                ((and (not has-pa) (not has-pb))
                                 ;; both without paths -> natural by name
                                 (funcall natural-lessp
                                          (downcase (or (context-navigator-item-name a) ""))
                                          (downcase (or (context-navigator-item-name b) ""))))
                                ((and (not has-pa) has-pb) nil)
                                ((and has-pa (not has-pb)) t)
                                ;; Both have paths: subdirs first
                                ((and sa (not sb)) t)
                                ((and (not sa) sb) nil)
                                ;; Same group: natural sort by relpath (dir/base)
                                (t (funcall natural-lessp
                                            (downcase (or ra ""))
                                            (downcase (or rb ""))))))))))
              (setq context-navigator-view--sorted-items s
                    context-navigator-view--sorted-gen gen
                    context-navigator-view--sorted-root root)
              s)))
         (left-width (max 16 (min (- total-width 10) (floor (* 0.55 total-width)))))
         (base (let ((context-navigator-render--gptel-keys context-navigator-view--gptel-keys))
                 (context-navigator-render-build-lines sorted-items header
                                                       #'context-navigator-icons-for-item
                                                       left-width)))
         ;; Title/header is rendered inside the buffer (interactive, collapsible).
         (hl (context-navigator-view--title-line header))
         (sep "")
         (rest (cddr base))
         (up (let ((s (copy-sequence "..")))
               (add-text-properties 0 (length s)
                                    (list 'context-navigator-groups-up t
                                          'mouse-face 'highlight
                                          'help-echo (context-navigator-i18n :status-up-to-groups)
                                          'face 'shadow)
                                    s)
               s)))
    (list hl sep up rest)))

;;;###autoload
(defun context-navigator-view--status-text-at-point ()
  "Return status text for the current point:
- title line → show the header text (project[: group])
- item line → relative path to project root (or absolute when no root)
- \"..\" line → localized \"<to groups>\"
- footer action/toggle → their help-echo
- otherwise → empty string."
  (cond
   ;; On title/header line
   ((get-text-property (point) 'context-navigator-title)
    (or context-navigator-view--header ""))
   ;; On an item: show relative path (dirs + basename), or buffer name when no path
   ((get-text-property (point) 'context-navigator-item)
    (let* ((it (get-text-property (point) 'context-navigator-item))
           (p  (context-navigator-item-path it))
           (st (ignore-errors (context-navigator--state-get)))
           (root (and st (ignore-errors (context-navigator-state-last-project-root st)))))
      (cond
       ((and (stringp p) (> (length p) 0))
        (condition-case _err
            (if (stringp root)
                (file-relative-name (expand-file-name p)
                                    (file-name-as-directory (expand-file-name root)))
              (abbreviate-file-name p))
          (error (abbreviate-file-name p))))
       (t (or (context-navigator-item-name it) "")))))
   ;; On up line
   ((get-text-property (point) 'context-navigator-groups-up)
    (context-navigator-i18n :status-up-to-groups))
   ;; On footer action or header toggle: show help-echo
   ((or (get-text-property (point) 'context-navigator-action)
        (get-text-property (point) 'context-navigator-toggle))
    (or (get-text-property (point) 'help-echo) ""))
   (t "")))

;;;###autoload
(defun context-navigator-view--items-footer-lines (total-width)
  "Return footer lines for items view (controls moved to header-line).
- Show collapsible Stats block
- Add a compact \"[?] - menu\" hint at the very bottom (i18n)
- Keep Stats independent from the main title collapse"
  (let* (;; Ensure Stats do not depend on the main collapsed flag: bind it to nil during rendering.
         (raw-stats (and (fboundp 'context-navigator-stats-footer-lines)
                         (let ((context-navigator-view--collapsed-p nil))
                           (context-navigator-stats-footer-lines total-width))))
         ;; Post-process stats lines: ensure the header has an interactive keymap and hint.
         (stats-lines
          (and raw-stats
               (let ((first t))
                 (mapcar
                  (lambda (s)
                    (let* ((s (copy-sequence s))
                           ;; Treat the first line as header; otherwise consider it a header
                           ;; only if the stats-toggle property is present at position 0.
                           (is-header (or first
                                          (get-text-property 0 'context-navigator-stats-toggle s))))
                      (when is-header
                        (let ((km (make-sparse-keymap)))
                          ;; Inherit the main mode map so navigation keys (n/p, j/k, arrows) work here.
                          (when (and (boundp 'context-navigator-view-mode-map)
                                     (keymapp context-navigator-view-mode-map))
                            (set-keymap-parent km context-navigator-view-mode-map))
                          (define-key km [mouse-1] #'context-navigator-view-stats-toggle)
                          (define-key km (kbd "RET")       #'context-navigator-view-stats-toggle)
                          (define-key km (kbd "C-m")       #'context-navigator-view-stats-toggle)
                          (define-key km (kbd "TAB")       #'context-navigator-view-stats-toggle)
                          (define-key km (kbd "<tab>")     #'context-navigator-view-stats-toggle)
                          (define-key km [tab]             #'context-navigator-view-stats-toggle)
                          (define-key km (kbd "C-i")       #'context-navigator-view-stats-toggle)
                          (add-text-properties 0 (length s)
                                               (list 'mouse-face 'highlight
                                                     'help-echo "Click/TAB/RET — toggle stats"
                                                     'context-navigator-stats-toggle t
                                                     'keymap km
                                                     'local-map km)
                                               s))
                        (setq first nil))
                      (unless is-header
                        (add-text-properties 0 (length s)
                                             (list 'context-navigator-stats-line t)
                                             s))
                      s))
                  raw-stats)))))
    (let ((hint (propertize (context-navigator-i18n :menu-hint) 'face 'shadow)))
      (if stats-lines
          ;; Blank line, stats block, blank line, then hint
          (append (list "") stats-lines (list "" hint))
        ;; Only the hint with a blank line above
        (list "" hint)))))

;; Backward-compatibility: new canonical name is
;; `context-navigator-view--items-extra-lines'. Provide it as an alias that
;; delegates to the existing implementation and mark the old name obsolete.
(defalias 'context-navigator-view--items-extra-lines
  'context-navigator-view--items-footer-lines)
(make-obsolete 'context-navigator-view--items-footer-lines
               'context-navigator-view--items-extra-lines
               "0.2.1")

;;;###autoload
(defun context-navigator-view--render-items (state header total-width)
  "Render items view using STATE, HEADER and TOTAL-WIDTH.
Returns the list of lines that were rendered."
  (cl-destructuring-bind (hl _sep up rest)
      (context-navigator-view--items-base-lines state header total-width)
    (let* ((footer (context-navigator-view--items-extra-lines total-width))
           (body (append (list up) rest footer))
           (lines (if context-navigator-view--collapsed-p
                      ;; When collapsed, keep the Stats block visible (as a separate header below).
                      (append (list "" hl) footer)
                    (cons "" (cons hl body)))))
      (setq context-navigator-view--last-lines lines
            context-navigator-view--header header)
      (context-navigator-render-apply-to-buffer (current-buffer) lines)
      lines)))

(provide 'context-navigator-view-items)
;;; context-navigator-view-items.el ends here
