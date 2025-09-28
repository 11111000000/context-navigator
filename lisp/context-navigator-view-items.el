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
(require 'context-navigator-log)

;;;###autoload
(defun context-navigator-view--items-header-toggle-lines (total-width)
  "Return header toggle lines for items view wrapped to TOTAL-WIDTH."
  (when (fboundp 'context-navigator-view-controls-lines)
    (context-navigator-view-controls-lines total-width)))

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
         ;; Debug: diagnose duplicates/mismatches between model items and the cached sorted list
         (_ (ignore-errors
              (let* ((model-keys  (mapcar #'context-navigator-model-item-key (or items '())))
                     (sorted-keys (mapcar #'context-navigator-model-item-key (or sorted-items '())))
                     (mk (length model-keys))
                     (sk (length sorted-keys)))
                (context-navigator-debug :debug :ui
                                         "items-base-lines: model=%d sorted=%d gen=%s cache-gen=%s root=%s"
                                         mk sk gen context-navigator-view--sorted-gen root)
                (when (not (= mk sk))
                  (context-navigator-debug :warn :ui
                                           "len-mismatch: model-keys=%S, sorted-keys=%S"
                                           model-keys sorted-keys))
                (let* ((ht (make-hash-table :test 'equal))
                       (dups nil))
                  (dolist (k sorted-keys)
                    (puthash k (1+ (gethash k ht 0)) ht))
                  (maphash (lambda (k n) (when (> n 1) (push (cons k n) dups))) ht)
                  (when dups
                    (context-navigator-debug :warn :ui
                                             "duplicate keys in sorted list: %S" (nreverse dups)))))))
         (left-width (max 16 (min (- total-width 10) (floor (* 0.55 total-width)))))
         (item-lines (let ((context-navigator-render--gptel-keys context-navigator-view--gptel-keys))
                       (context-navigator-render-build-item-lines sorted-items
                                                                  #'context-navigator-icons-for-item
                                                                  left-width)))
         ;; Title/header is rendered inside the buffer (interactive, collapsible).
         (hl (context-navigator-view--title-line header))
         (sep "")
         (rest item-lines)
         (up (let ((s (copy-sequence "..")))
               (add-text-properties 0 (length s)
                                    (list 'context-navigator-groups-up t
                                          'context-navigator-interactive t
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
  (let* (;; Stats lines as a fully interactive block, independent from the main collapse flag.
         (stats-lines
          (and (fboundp 'context-navigator-stats-interactive-lines)
               (let ((context-navigator-view--collapsed-p nil))
                 (context-navigator-stats-interactive-lines
                  total-width
                  (and (boundp 'context-navigator-view-mode-map)
                       context-navigator-view-mode-map))))))
    (let ((hint (propertize (context-navigator-i18n :menu-hint) 'face 'shadow)))
      (if stats-lines
          ;; Blank line, stats block, blank line, then hint
          (append (list "") stats-lines (list "" hint))
        ;; Only the hint with a blank line above
        (list "" hint)))))



;;;###autoload
(defun context-navigator-view--render-items (state header total-width)
  "Render items view using STATE, HEADER and TOTAL-WIDTH.
Returns the list of lines that were rendered."
  (cl-destructuring-bind (hl _sep up rest)
      (context-navigator-view--items-base-lines state header total-width)
    (let* ((footer (context-navigator-view--items-footer-lines total-width))
           (body (append (list up) rest footer))
           (lines (if context-navigator-view--collapsed-p
                      ;; When collapsed, keep the Stats block visible (as a separate header below).
                      (append (list "" hl) footer)
                    (cons "" (cons hl body)))))
      (setq context-navigator-view--last-lines lines
            context-navigator-view--header header)
      (context-navigator-render-apply-to-buffer (current-buffer) lines)
      lines)))

(defun context-navigator-view-debug-dump-lines ()
  "Scan Navigator buffer for duplicate item lines and log their keys and counts.

Use M-x context-navigator-log-toggle to enable logs, then run this command
inside the Navigator buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (unless (eq major-mode 'context-navigator-view-mode)
      (user-error "Run inside the Context Navigator buffer"))
    (save-excursion
      (goto-char (point-min))
      (let ((line 1)
            (ht (make-hash-table :test 'equal)))
        (while (< (point) (point-max))
          (let* ((bol (line-beginning-position))
                 (eol (line-end-position))
                 (p (text-property-not-all bol eol 'context-navigator-item nil)))
            (when p
              (let* ((it (get-text-property p 'context-navigator-item))
                     (k (and it (context-navigator-model-item-key it))))
                (when k
                  (puthash k (1+ (gethash k ht 0)) ht)
                  (context-navigator-debug :trace :ui "ln=%d key=%s" line k)))))
          (setq line (1+ line))
          (forward-line 1))
        (let (dups)
          (maphash (lambda (k n) (when (> n 1) (push (cons k n) dups))) ht)
          (if dups
              (context-navigator-debug :warn :ui "buffer duplicate item-lines: %S" (nreverse dups))
            (context-navigator-debug :debug :ui "buffer: no duplicate item-lines")))))))

(provide 'context-navigator-view-items)
;;; context-navigator-view-items.el ends here
