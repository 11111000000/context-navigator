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
(defun context-navigator-view-items-header-lines (total-width)
  "Return header toggle lines for items view wrapped to TOTAL-WIDTH."
  (when (fboundp 'context-navigator-view-controls-lines)
    (context-navigator-view-controls-lines total-width)))

;; --- Items helpers (pure-ish) ------------------------------------------------

(defun context-navigator-view--items--nat-chunks (s)
  "Tokenize S into a list of chunks: (:num . N) and (:str . LOWERCASE-STR)."
  (let ((i 0) (n (length s)) (cur "") (res '()))
    (while (< i n)
      (let ((c (aref s i)))
        (if (and (>= c ?0) (<= c ?9))
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

(defun context-navigator-view--items--nat-chunks-lessp (xa xb)
  "Return non-nil when chunk list XA is less than chunk list XB in natural order."
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
          (context-navigator-view--items--nat-chunks-lessp (cdr xa) (cdr xb))))
       ((and (eq (car a1) :str) (eq (car b1) :str))
        (let ((sa (cdr a1)) (sb (cdr b1)))
          (if (string= sa sb)
              (context-navigator-view--items--nat-chunks-lessp (cdr xa) (cdr xb))
            (string-lessp sa sb))))
       ((eq (car a1) :num) t)
       (t nil))))))

(defun context-navigator-view--items--natural-lessp (a b)
  "Case-insensitive compare A and B with numeric chunk awareness."
  (context-navigator-view--items--nat-chunks-lessp
   (context-navigator-view--items--nat-chunks (or a ""))
   (context-navigator-view--items--nat-chunks (or b ""))))

(defun context-navigator-view--items--in-subdir-p (rel)
  "Return non-nil when REL is inside a subdirectory (not starting with ..)."
  (and (stringp rel)
       (not (string-prefix-p ".." rel))
       (string-match-p "/" rel)))

(defun context-navigator-view--items--ensure-relpath-cache (gen root)
  "Ensure relpath cache table is fresh for GEN/ROOT pair."
  (unless (and context-navigator-view--relpaths-hash
               (= gen (or context-navigator-view--sorted-gen -1))
               (equal root context-navigator-view--sorted-root))
    (setq context-navigator-view--relpaths-hash (make-hash-table :test 'equal))))

(defun context-navigator-view--items--relpath-of (it root)
  "Return relative path string for item IT to ROOT, updating cache."
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
          rel))))

(defun context-navigator-view--items--state-gen-root (state)
  "Return cons (GEN . ROOT) extracted from STATE (safe defaults)."
  (cons (or (and (context-navigator-state-p state)
                 (context-navigator-state-generation state))
            0)
        (and (context-navigator-state-p state)
             (context-navigator-state-last-project-root state))))

(defun context-navigator-view--items--sorted-cache-valid-p (gen root)
  "Return non-nil when sorted-items cache matches GEN/ROOT."
  (and (listp context-navigator-view--sorted-items)
       (integerp context-navigator-view--sorted-gen)
       (= gen context-navigator-view--sorted-gen)
       (equal root context-navigator-view--sorted-root)))

(defun context-navigator-view--items--sorted-set-cache (items gen root)
  "Update sorted-items cache with ITEMS, GEN and ROOT."
  (setq context-navigator-view--sorted-items items
        context-navigator-view--sorted-gen gen
        context-navigator-view--sorted-root root))

(defun context-navigator-view--items--item-has-path (it)
  "Return non-nil when item IT has a non-empty path."
  (let ((p (context-navigator-item-path it)))
    (and (stringp p) (> (length p) 0))))

(defun context-navigator-view--items--sort-keys (it root)
  "Return plist with keys for sorting IT relative to ROOT."
  (let* ((p   (context-navigator-item-path it))
         (has (and (stringp p) (> (length p) 0)))
         (rel (context-navigator-view--items--relpath-of it root))
         (sub (and has (context-navigator-view--items--in-subdir-p rel)))
         (nm  (downcase (or (context-navigator-item-name it) ""))))
    (list :has-path has :rel rel :subdir sub :name nm)))

(defun context-navigator-view--items--lessp (a b root)
  "Sort predicate for items A and B relative to ROOT."
  (let* ((ka (context-navigator-view--items--sort-keys a root))
         (kb (context-navigator-view--items--sort-keys b root))
         (has-pa (plist-get ka :has-path))
         (has-pb (plist-get kb :has-path))
         (sa (plist-get ka :subdir))
         (sb (plist-get kb :subdir))
         (ra (plist-get ka :rel))
         (rb (plist-get kb :rel)))
    (cond
     ;; Items without paths go to the bottom; among them sort by name naturally.
     ((and (not has-pa) (not has-pb))
      (context-navigator-view--items--natural-lessp
       (plist-get ka :name) (plist-get kb :name)))
     ((and (not has-pa) has-pb) nil)
     ((and has-pa (not has-pb)) t)
     ;; Both have paths: prefer ones in subdirs first
     ((and sa (not sb)) t)
     ((and (not sa) sb) nil)
     ;; Same group: natural sort by relpath
     (t (context-navigator-view--items--natural-lessp
         (downcase (or ra "")) (downcase (or rb "")))))))

(defun context-navigator-view--items--sort-items (items root)
  "Return a freshly sorted copy of ITEMS relative to ROOT."
  (sort (copy-sequence (or items '()))
        (lambda (a b) (context-navigator-view--items--lessp a b root))))

(defun context-navigator-view--items--sorted-debug-validate (items sorted gen root)
  "Emit debug diagnostics about ITEMS vs SORTED for GEN/ROOT."
  (ignore-errors
    (let* ((model-keys  (mapcar #'context-navigator-model-item-key (or items '())))
           (sorted-keys (mapcar #'context-navigator-model-item-key (or sorted '())))
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

(defun context-navigator-view--items--sorted (state items)
  "Return items sorted with stable cache keyed by STATE generation and ROOT.
Updates buffer-local caches:
- context-navigator-view--sorted-items
- context-navigator-view--sorted-gen
- context-navigator-view--sorted-root
Also maintains relpath cache per generation/root."
  (cl-destructuring-bind (gen . root)
      (context-navigator-view--items--state-gen-root state)
    (context-navigator-view--items--ensure-relpath-cache gen root)
    (if (context-navigator-view--items--sorted-cache-valid-p gen root)
        context-navigator-view--sorted-items
      (let ((s (context-navigator-view--items--sort-items items root)))
        (context-navigator-view--items--sorted-set-cache s gen root)
        (context-navigator-view--items--sorted-debug-validate items s gen root)
        s))))

(defun context-navigator-view--items--left-width (total-width)
  "Compute left column width from TOTAL-WIDTH, clamped to sane bounds."
  (max 16 (min (- total-width 10) (floor (* 0.55 total-width)))))

(defun context-navigator-view--items--title-line (header)
  "Build interactive title line for HEADER."
  (context-navigator-view--title-line header))

(defun context-navigator-view--items--up-line ()
  "\"..\" line that navigates up to groups."
  (let ((s (copy-sequence "..")))
    (add-text-properties 0 (length s)
                         (list 'context-navigator-groups-up t
                               'context-navigator-interactive t
                               'mouse-face 'highlight
                               'help-echo (context-navigator-i18n :status-up-to-groups)
                               'face 'shadow)
                         s)
    s))

(defun context-navigator-view--items--item-lines (sorted-items left-width)
  "Build propertized item lines from SORTED-ITEMS aligned to LEFT-WIDTH."
  (let ((context-navigator-render--gptel-keys context-navigator-view--gptel-keys))
    (context-navigator-render-build-item-lines sorted-items
                                               #'context-navigator-icons-for-item
                                               left-width)))

;;;###autoload
(defun context-navigator-view--items-base-lines (state header total-width)
  "Return a list: (hl sep up rest...) for items view base lines.
HL is the clickable [project[: group]] title line placed above \"..\".
SEP is currently empty (no extra separator in the buffer).
UP is the \"..\" line.
REST is a list of item lines."
  (let* ((items (context-navigator-state-items state))
         (sorted-items (context-navigator-view--items--sorted state items))
         (left-width (context-navigator-view--items--left-width total-width))
         (item-lines (context-navigator-view--items--item-lines sorted-items left-width))
         (hl (context-navigator-view--items--title-line header))
         (sep "")
         (rest item-lines)
         (up (context-navigator-view--items--up-line)))
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
(defun context-navigator-view-items-footer-lines (total-width)
  "Return footer lines for items view (no inline Stats; Stats shown in separate split)."
  (ignore total-width)
  (let ((hint (propertize (context-navigator-i18n :menu-hint) 'face 'shadow)))
    (list "" hint)))



;;;###autoload
(defun context-navigator-view-render-items (state header total-width)
  "Render items view using STATE and TOTAL-WIDTH (no inline title/stats in buffer).
Returns the list of lines that were rendered."
  (cl-destructuring-bind (_hl _sep up rest)
      (context-navigator-view--items-base-lines state header total-width)
    (let* ((footer (context-navigator-view-items-footer-lines total-width))
           (body (append (list up) rest footer))
           ;; No collapsible body and no inline title in the buffer: only content + footer.
           (lines (cons "" body)))
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
      (user-error "%s" (context-navigator-i18n :debug-run-in-nav-buffer)))
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
