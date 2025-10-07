;;; context-navigator-stats-split.el --- 5-line Stats split for Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Display a compact 5-line Stats buffer in a split below the Navigator sidebar.
;; - Toggles via header-line control [Σ] or key 's' in the Navigator buffer
;; - Auto-refreshes on model/project/groups events
;; - Closes automatically when the Navigator sidebar is closed

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-events)
(require 'context-navigator-stats)
(require 'context-navigator-core)
(require 'context-navigator-persist)
(require 'context-navigator-i18n)
(require 'context-navigator-util)

;; Cross-module helpers (optional; guard with fboundp at callsites)
(declare-function context-navigator-groups-split--visible-window "context-navigator-groups-split" ())
(declare-function context-navigator-groups-split--fit-window "context-navigator-groups-split" (win navw))

(defgroup context-navigator-stats-split nil
  "Split buffer for compact Stats view (5 lines)."
  :group 'context-navigator)

(defcustom context-navigator-stats-split-height 5
  "Height (in text lines) of the Stats split window."
  :type 'integer :group 'context-navigator-stats-split)

(defcustom context-navigator-stats-split-buffer-name "*Context Navigator Stats*"
  "Name of the Stats split buffer."
  :type 'string :group 'context-navigator-stats-split)

(defcustom context-navigator-stats-split-default-visible t
  "When non-nil, open the 5-line Stats split by default when opening Navigator."
  :type 'boolean :group 'context-navigator-stats-split)

(defcustom context-navigator-stats-split-aggregate-ttl 1.5
  "TTL (seconds) for cached aggregate stats across selected groups.
Within this time window, repeated renders with the same selection/root reuse
the cached aggregate without re-reading group files."
  :type 'number :group 'context-navigator-stats-split)

(defvar context-navigator-stats-split--subs nil
  "Event subscription tokens installed while Stats split is open.")

(defvar context-navigator-stats-split--buffer nil
  "Stats split buffer object (when created).")

;; Aggregate cache for groups view (fingerprint -> plist)
(defvar context-navigator-stats-split--agg-fp nil)
(defvar context-navigator-stats-split--agg-count 0)         ;; total items enabled (for header convenience)
(defvar context-navigator-stats-split--agg-nsel 0)          ;; number of selected groups
(defvar context-navigator-stats-split--agg-total 0)         ;; total items (all)
(defvar context-navigator-stats-split--agg-enabled 0)       ;; total items enabled
(defvar context-navigator-stats-split--agg-files-all 0)
(defvar context-navigator-stats-split--agg-buffers-all 0)
(defvar context-navigator-stats-split--agg-selections-all 0)
(defvar context-navigator-stats-split--agg-files-en 0)
(defvar context-navigator-stats-split--agg-buffers-en 0)
(defvar context-navigator-stats-split--agg-selections-en 0)
(defvar context-navigator-stats-split--agg-bytes-en 0)
(defvar context-navigator-stats-split--agg-bytes-all 0)
(defvar context-navigator-stats-split--agg-tokens-en 0)
(defvar context-navigator-stats-split--agg-tokens-all 0)
(defvar context-navigator-stats-split--agg-ts 0.0)

;; Auto-close hook for Stats split when Navigator sidebar window disappears
(defvar context-navigator-stats-split--wcch-on nil)

(defvar context-navigator-stats-split--refit-pending nil)

(defun context-navigator-stats-split--schedule-refit ()
  "Debounce refit of all Navigator splits after window layout changes."
  (unless context-navigator-stats-split--refit-pending
    (setq context-navigator-stats-split--refit-pending t)
    (run-at-time 0.02 nil
                 (lambda ()
                   (setq context-navigator-stats-split--refit-pending nil)
                   (ignore-errors (context-navigator-stats-split--refit-all))))))

(defun context-navigator-stats-split--maybe-autoclose ()
  "Close Stats split when Navigator sidebar window is no longer present.
Also schedule a refit of all splits to their content after any window layout change."
  (let ((stats-win (context-navigator-stats-split--visible-window))
        (nav-win (context-navigator-stats-split--nav-window)))
    (when (and (window-live-p stats-win)
               (not (window-live-p nav-win)))
      (context-navigator-stats-split-close)))
  (context-navigator-stats-split--schedule-refit))

(defun context-navigator-stats-split--sum-bytes (items)
  "Return cons (BYTES-ALL . BYTES-ENABLED) for ITEMS."
  (let ((all 0) (en 0))
    (dolist (it (or items '()))
      (let ((b (ignore-errors (context-navigator-stats--item-bytes it))))
        (setq all (+ all (or b 0)))
        (when (context-navigator-item-enabled it)
          (setq en (+ en (or b 0))))))
    (cons all en)))

(defun context-navigator-stats-split--tokens-from-bytes (bytes)
  "Approximate tokens count from BYTES using Stats setting."
  (let ((k (max 1e-6 (or context-navigator-stats-bytes-per-token 4.0))))
    (floor (/ (float bytes) k))))

(defun context-navigator-stats-split--nav-window ()
  "Return a Navigator window (sidebar or buffer-mode) on the current frame, or nil."
  (catch 'hit
    (dolist (w (window-list nil 'no-mini))
      (when (and (window-live-p w)
                 (let ((buf (window-buffer w)))
                   (or (memq (window-parameter w 'context-navigator-view) '(sidebar buffer))
                       (and (buffer-live-p buf)
                            (equal (buffer-name buf) "*context-navigator*")))))
        (throw 'hit w)))
    nil))

(defun context-navigator-stats-split--fit-window (win navw)
  "Fit WIN height to content, constrained by NAVW half-height and user preference.
Temporarily unlock window height to allow shrinking, then lock height so later
window rebalancing (e.g., when opening another split) won't inflate it."
  (when (and (window-live-p win) (window-live-p navw))
    (let* ((max-pref (max 1 (or context-navigator-stats-split-height 5)))
           (half     (max 1 (floor (/ (window-total-height navw) 2))))
           ;; Реальная высота контента буфера (в строках)
           (content  (with-current-buffer (window-buffer win)
                       (max 1 (count-lines (point-min) (point-max)))))
           (desired  (min content (min max-pref half)))
           (window-combination-resize t))
      ;; Allow resizing first, then fix height so other operations don't enlarge it.
      (set-window-parameter win 'window-size-fixed nil)
      (set-window-parameter win 'window-preserved-size nil)
      ;; Минимум 1 строка, максимум — desired
      (fit-window-to-buffer win desired 1)
      ;; Preserve final height and fix it to avoid later inflation.
      (set-window-parameter win 'window-preserved-size (cons t (window-total-height win)))
      (set-window-parameter win 'window-size-fixed 'height))))

(defun context-navigator-stats-split--ensure-buffer ()
  "Create or return the Stats buffer initialized to special-mode."
  (let ((buf (or (and (buffer-live-p context-navigator-stats-split--buffer)
                      context-navigator-stats-split--buffer)
                 (get-buffer-create context-navigator-stats-split-buffer-name))))
    (setq context-navigator-stats-split--buffer buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (setq-local buffer-read-only t)
      (setq-local truncate-lines t))
    buf))

(defun context-navigator-stats-split--visible-window ()
  "Return the window showing the Stats buffer, or nil."
  (catch 'hit
    (let ((buf (and (buffer-live-p context-navigator-stats-split--buffer)
                    context-navigator-stats-split--buffer)))
      (when buf
        (dolist (w (get-buffer-window-list buf nil t))
          (when (window-live-p w)
            (throw 'hit w)))))
    nil))

(defun context-navigator-stats-split-visible-p ()
  "Return non-nil when the Stats split is currently visible."
  (and (window-live-p (context-navigator-stats-split--visible-window)) t))

(defun context-navigator-stats-split--nav-buffer ()
  "Return Navigator buffer object or nil."
  (get-buffer "*context-navigator*"))

(defun context-navigator-stats-split--view-mode ()
  "Return current Navigator view mode symbol ('items or 'groups)."
  (let ((buf (context-navigator-stats-split--nav-buffer)))
    (if (not (buffer-live-p buf)) 'items
      (with-current-buffer buf
        (or (and (boundp 'context-navigator-view--mode)
                 context-navigator-view--mode)
            'items)))))

(defun context-navigator-stats-split--current-root ()
  "Return current project root from core state (string or nil)."
  (let* ((st (ignore-errors (context-navigator--state-get))))
    (and st (context-navigator-state-last-project-root st))))

(defun context-navigator-stats-split--selected-slugs (root)
  "Return list of selected group slugs for ROOT from state.el."
  (let* ((st (and (stringp root)
                  (ignore-errors (context-navigator-persist-state-load root)))))
    (let ((sel (and (listp st) (plist-member st :selected) (plist-get st :selected))))
      (if (listp sel) sel '()))))

(defun context-navigator-stats-split--fingerprint (root slugs)
  "Return a stable fingerprint string for ROOT and SLUGS."
  (format "%s|%s" (or root "~") (mapconcat #'identity (sort (copy-sequence (or slugs '())) #'string<) ",")))

(defun context-navigator-stats-split--kick-aggregate (root slugs)
  "Start async aggregation for ROOT/SLUGS; update cache on completion (with TTL)."
  (let* ((fp (context-navigator-stats-split--fingerprint root slugs))
         (now (float-time))
         (ttl (max 0.0 (or context-navigator-stats-split-aggregate-ttl 0))))
    ;; Skip recompute when fingerprint matches and TTL not expired.
    (when (or (not (and (stringp context-navigator-stats-split--agg-fp)
                        (string= context-navigator-stats-split--agg-fp fp)))
              (> (- now (or context-navigator-stats-split--agg-ts 0.0)) ttl))
      (setq context-navigator-stats-split--agg-fp fp
            context-navigator-stats-split--agg-ts now
            context-navigator-stats-split--agg-count 0
            context-navigator-stats-split--agg-nsel (length (or slugs '()))
            context-navigator-stats-split--agg-total 0
            context-navigator-stats-split--agg-enabled 0
            context-navigator-stats-split--agg-bytes-en 0
            context-navigator-stats-split--agg-bytes-all 0
            context-navigator-stats-split--agg-tokens-en 0
            context-navigator-stats-split--agg-tokens-all 0)
      (when (and (stringp root) (listp slugs) (> (length slugs) 0))
        (context-navigator-collect-items-for-groups-async
         root slugs
         (lambda (items)
           (let* ((all (or items '()))
                  (en (context-navigator--enabled-only all))
                  ;; per-type counts: all vs enabled
                  (files-all (cl-count-if (lambda (it) (eq (context-navigator-item-type it) 'file)) all))
                  (buffers-all (cl-count-if (lambda (it) (eq (context-navigator-item-type it) 'buffer)) all))
                  (sels-all (cl-count-if (lambda (it) (eq (context-navigator-item-type it) 'selection)) all))
                  (files-en (cl-count-if (lambda (it) (eq (context-navigator-item-type it) 'file)) en))
                  (buffers-en (cl-count-if (lambda (it) (eq (context-navigator-item-type it) 'buffer)) en))
                  (sels-en (cl-count-if (lambda (it) (eq (context-navigator-item-type it) 'selection)) en))
                  (bpair (context-navigator-stats-split--sum-bytes all))
                  (bytes-all (car bpair))
                  (bytes-en (cdr bpair))
                  (tok-all (context-navigator-stats-split--tokens-from-bytes bytes-all))
                  (tok-en (context-navigator-stats-split--tokens-from-bytes bytes-en)))
             (setq context-navigator-stats-split--agg-ts (float-time))
             (setq context-navigator-stats-split--agg-total (length all))
             (setq context-navigator-stats-split--agg-enabled (length en))
             (setq context-navigator-stats-split--agg-count (length en))
             (setq context-navigator-stats-split--agg-files-all files-all)
             (setq context-navigator-stats-split--agg-buffers-all buffers-all)
             (setq context-navigator-stats-split--agg-selections-all sels-all)
             (setq context-navigator-stats-split--agg-files-en files-en)
             (setq context-navigator-stats-split--agg-buffers-en buffers-en)
             (setq context-navigator-stats-split--agg-selections-en sels-en)
             (setq context-navigator-stats-split--agg-bytes-all bytes-all)
             (setq context-navigator-stats-split--agg-bytes-en bytes-en)
             (setq context-navigator-stats-split--agg-tokens-all tok-all)
             (setq context-navigator-stats-split--agg-tokens-en tok-en)
             (context-navigator-stats-split--render))))))))

(defun context-navigator-stats-split--groups-lines (total-width)
  "Return exactly 5 lines using unified Stats renderer (aggregate when MG ON)."
  (ignore total-width)
  (let* ((root (context-navigator-stats-split--current-root))
         (sel  (context-navigator-stats-split--selected-slugs root))
         (nsel (length sel)))
    (when (> nsel 0)
      (context-navigator-stats-split--kick-aggregate root sel))
    (let* ((en   (if (> nsel 0) context-navigator-stats-split--agg-enabled 0))
           (ben  (if (> nsel 0) context-navigator-stats-split--agg-bytes-en 0))
           (ten  (if (> nsel 0) context-navigator-stats-split--agg-tokens-en 0))
           (b-all (if (> nsel 0) context-navigator-stats-split--agg-bytes-all 0))
           (t-all (if (> nsel 0) context-navigator-stats-split--agg-tokens-all 0))
           (f-all (if (> nsel 0) context-navigator-stats-split--agg-files-all 0))
           (bufs-all (if (> nsel 0) context-navigator-stats-split--agg-buffers-all 0))
           (sels-all (if (> nsel 0) context-navigator-stats-split--agg-selections-all 0))
           (f-en (if (> nsel 0) context-navigator-stats-split--agg-files-en 0))
           (b-en (if (> nsel 0) context-navigator-stats-split--agg-buffers-en 0))
           (s-en (if (> nsel 0) context-navigator-stats-split--agg-selections-en 0))
           ;; Icons like in items footer
           (hdr-ico (or (context-navigator-stats--icon :header) ""))
           (cnt-ico (or (context-navigator-stats--icon :counts) ""))
           (siz-ico (or (context-navigator-stats--icon :size) ""))
           (tok-ico (or (context-navigator-stats--icon :tokens) ""))
           (ico-file (or (context-navigator-stats--icon :file) ""))
           (ico-buf  (or (context-navigator-stats--icon :buffer) ""))
           (ico-sel  (or (context-navigator-stats--icon :selection) ""))
           (arrow "▾")
           (lbl (context-navigator-i18n :stats))
           (hdr (format "%s %s %s: %d  ~%s  (~%d %s)"
                        arrow hdr-ico lbl
                        (max 0 (or en 0))
                        (context-navigator-human-size (max 0 (or ben 0)))
                        (max 0 (or ten 0))
                        (context-navigator-i18n :stats-tokens)))
           (row1 (format "   %s %s: %s %d (%d), %s %d (%d), %s %d (%d)"
                         cnt-ico
                         (context-navigator-i18n :stats-counts)
                         ico-file (max 0 (or f-all 0)) (max 0 (or f-en 0))
                         ico-buf  (max 0 (or bufs-all 0)) (max 0 (or b-en 0))
                         ico-sel  (max 0 (or sels-all 0)) (max 0 (or s-en 0))))
           (row2 (format "   %s %s: %s ~%s  /  %s ~%s"
                         siz-ico
                         (context-navigator-i18n :stats-size)
                         (context-navigator-i18n :enabled) (context-navigator-human-size (max 0 (or ben 0)))
                         (context-navigator-i18n :total)    (context-navigator-human-size (max 0 (or b-all 0)))))
           (row3 (format "   %s %s: %s %d  /  %s %d"
                         tok-ico
                         (context-navigator-i18n :stats-tokens)
                         (context-navigator-i18n :enabled) (max 0 (or ten 0))
                         (context-navigator-i18n :total)    (max 0 (or t-all 0)))))
      ;; Exactly 5 lines
      (list hdr row1 row2 row3 ""))))

(defun context-navigator-stats-split--items-lines (total-width)
  "Return exactly 5 lines for items view.
When MG is ON and selection non-empty, show aggregated unified stats; otherwise use shared compute."
  (let* ((tw (max 30 (or total-width 80)))
         (root (context-navigator-stats-split--current-root))
         (ps (and (stringp root) (ignore-errors (context-navigator-persist-state-load root))))
         (mg (and (listp ps) (plist-member ps :multi) (plist-get ps :multi)))
         (sel (and (listp ps) (plist-member ps :selected) (plist-get ps :selected)))
         (nsel (and (listp sel) (length sel))))
    (if (and mg (numberp nsel) (> nsel 0))
        (progn
          (context-navigator-stats-split--kick-aggregate root sel)
          (let* ((en   context-navigator-stats-split--agg-enabled)
                 (ben  context-navigator-stats-split--agg-bytes-en)
                 (ten  context-navigator-stats-split--agg-tokens-en)
                 (b-all context-navigator-stats-split--agg-bytes-all)
                 (t-all context-navigator-stats-split--agg-tokens-all)
                 (f-all context-navigator-stats-split--agg-files-all)
                 (bufs-all context-navigator-stats-split--agg-buffers-all)
                 (sels-all context-navigator-stats-split--agg-selections-all)
                 (f-en context-navigator-stats-split--agg-files-en)
                 (b-en context-navigator-stats-split--agg-buffers-en)
                 (s-en context-navigator-stats-split--agg-selections-en)
                 (hdr-ico (or (context-navigator-stats--icon :header) ""))
                 (cnt-ico (or (context-navigator-stats--icon :counts) ""))
                 (siz-ico (or (context-navigator-stats--icon :size) ""))
                 (tok-ico (or (context-navigator-stats--icon :tokens) ""))
                 (ico-file (or (context-navigator-stats--icon :file) ""))
                 (ico-buf  (or (context-navigator-stats--icon :buffer) ""))
                 (ico-sel  (or (context-navigator-stats--icon :selection) ""))
                 (arrow "▾")
                 (lbl (context-navigator-i18n :stats))
                 (hdr (format "%s %s %s: %d  ~%s  (~%d %s)"
                              arrow hdr-ico lbl
                              (max 0 (or en 0))
                              (context-navigator-human-size (max 0 (or ben 0)))
                              (max 0 (or ten 0))
                              (context-navigator-i18n :stats-tokens)))
                 (row1 (format "   %s %s: %s %d (%d), %s %d (%d), %s %d (%d)"
                               cnt-ico
                               (context-navigator-i18n :stats-counts)
                               ico-file (max 0 (or f-all 0)) (max 0 (or f-en 0))
                               ico-buf  (max 0 (or bufs-all 0)) (max 0 (or b-en 0))
                               ico-sel  (max 0 (or sels-all 0)) (max 0 (or s-en 0))))
                 (row2 (format "   %s %s: %s ~%s  /  %s ~%s"
                               siz-ico
                               (context-navigator-i18n :stats-size)
                               (context-navigator-i18n :enabled) (context-navigator-human-size (max 0 (or ben 0)))
                               (context-navigator-i18n :total)    (context-navigator-human-size (max 0 (or b-all 0)))))
                 (row3 (format "   %s %s: %s %d  /  %s %d"
                               tok-ico
                               (context-navigator-i18n :stats-tokens)
                               (context-navigator-i18n :enabled) (max 0 (or ten 0))
                               (context-navigator-i18n :total)    (max 0 (or t-all 0)))))
            (list hdr row1 row2 row3 "")))
      ;; Single-group: compute via shared stats
      (let* ((pl (ignore-errors (context-navigator-stats--compute-now)))
             (en    (or (plist-get pl :items-en) 0))
             (ben   (or (plist-get pl :bytes-en) 0))
             (ten   (or (plist-get pl :tokens-en) 0))
             (b-all (or (plist-get pl :bytes) 0))
             (t-all (or (plist-get pl :tokens) 0))
             (f-all (or (plist-get pl :files) 0))
             (bufs-all (or (plist-get pl :buffers) 0))
             (sels-all (or (plist-get pl :selections) 0))
             (f-en  (or (plist-get pl :files-en) 0))
             (b-en  (or (plist-get pl :buffers-en) 0))
             (s-en  (or (plist-get pl :selections-en) 0))
             (hdr-ico (or (context-navigator-stats--icon :header) ""))
             (cnt-ico (or (context-navigator-stats--icon :counts) ""))
             (siz-ico (or (context-navigator-stats--icon :size) ""))
             (tok-ico (or (context-navigator-stats--icon :tokens) ""))
             (ico-file (or (context-navigator-stats--icon :file) ""))
             (ico-buf  (or (context-navigator-stats--icon :buffer) ""))
             (ico-sel  (or (context-navigator-stats--icon :selection) ""))
             (arrow "▾")
             (lbl (context-navigator-i18n :stats))
             (hdr (format "%s %s %s: %d  ~%s  (~%d %s)"
                          arrow hdr-ico lbl
                          (max 0 en)
                          (context-navigator-human-size (max 0 ben))
                          (max 0 ten)
                          (context-navigator-i18n :stats-tokens)))
             (row1 (format "   %s %s: %s %d (%d), %s %d (%d), %s %d (%d)"
                           cnt-ico
                           (context-navigator-i18n :stats-counts)
                           ico-file (max 0 f-all) (max 0 f-en)
                           ico-buf  (max 0 bufs-all) (max 0 b-en)
                           ico-sel  (max 0 sels-all) (max 0 s-en)))
             (row2 (format "   %s %s: %s ~%s  /  %s ~%s"
                           siz-ico
                           (context-navigator-i18n :stats-size)
                           (context-navigator-i18n :enabled) (context-navigator-human-size (max 0 ben))
                           (context-navigator-i18n :total)    (context-navigator-human-size (max 0 b-all))))
             (row3 (format "   %s %s: %s %d  /  %s %d"
                           tok-ico
                           (context-navigator-i18n :stats-tokens)
                           (context-navigator-i18n :enabled) (max 0 ten)
                           (context-navigator-i18n :total)    (max 0 t-all))))
        (list hdr row1 row2 row3 "")))))

(defun context-navigator-stats-split--render-lines (total-width)
  "Return exactly 5 lines of Stats content for TOTAL-WIDTH columns.
- In items view: compute from current items (shared stats)
- In groups view: show aggregate summary for selected groups (dedup-enabled)"
  (pcase (context-navigator-stats-split--view-mode)
    ('groups (context-navigator-stats-split--groups-lines total-width))
    (_       (context-navigator-stats-split--items-lines total-width))))

(defun context-navigator-stats-split--render ()
  "Render 5-line Stats into the split buffer (no-op when invisible)."
  (let* ((w (context-navigator-stats-split--visible-window)))
    (when (window-live-p w)
      (let* ((buf (window-buffer w))
             (tw (max 30 (window-body-width w)))
             (lines (context-navigator-stats-split--render-lines tw)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (dolist (ln lines)
              (insert (or ln "") "\n"))))))))

(defun context-navigator-stats-split--install-subs ()
  "Subscribe to events that should refresh the Stats split (idempotent)."
  (unless context-navigator-stats-split--subs
    (setq context-navigator-stats-split--subs
          (list
           (context-navigator-events-subscribe
            :model-refreshed
            (lambda (&rest _)
              (when (context-navigator-stats-split-visible-p)
                (context-navigator-stats-invalidate)
                (context-navigator-stats-split--render))))
           (context-navigator-events-subscribe
            :project-switch
            (lambda (&rest _)
              (when (context-navigator-stats-split-visible-p)
                (context-navigator-stats-invalidate)
                (context-navigator-stats-split--render))))
           (context-navigator-events-subscribe
            :groups-list-updated
            (lambda (&rest _)
              (when (context-navigator-stats-split-visible-p)
                (context-navigator-stats-invalidate)
                (context-navigator-stats-split--render))))
           (context-navigator-events-subscribe
            :group-selection-changed
            (lambda (&rest _)
              (when (context-navigator-stats-split-visible-p)
                (context-navigator-stats-invalidate)
                (context-navigator-stats-split--render))))))))

(defun context-navigator-stats-split--remove-subs ()
  "Remove previously installed event subscriptions (idempotent)."
  (when context-navigator-stats-split--subs
    (mapc #'context-navigator-events-unsubscribe context-navigator-stats-split--subs)
    (setq context-navigator-stats-split--subs nil)))

(defun context-navigator-stats-split--notify-changed ()
  "Invalidate headerline cache and refresh Navigator view (if visible)."
  (let ((buf (get-buffer "*context-navigator*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local context-navigator-headerline--cache-key nil)
        (setq-local context-navigator-headerline--cache-str nil)
        (when (fboundp 'context-navigator-view--render-if-visible)
          (context-navigator-view--render-if-visible))))))

(defun context-navigator-stats-split--refit-all ()
  "Refit all Navigator splits (Stats and Groups) to content with half-window cap."
  (let ((navw (context-navigator-stats-split--nav-window)))
    (when (window-live-p navw)
      (when-let ((sw (context-navigator-stats-split--visible-window)))
        (context-navigator-stats-split--fit-window sw navw))
      (when (and (fboundp 'context-navigator-groups-split--visible-window)
                 (fboundp 'context-navigator-groups-split--fit-window))
        (let ((gw (ignore-errors (context-navigator-groups-split--visible-window))))
          (when (window-live-p gw)
            (ignore-errors (context-navigator-groups-split--fit-window gw navw))))))))

(defun context-navigator-stats-split--ensure-wcch ()
  "Ensure window-configuration-change-hook is installed once."
  (unless context-navigator-stats-split--wcch-on
    (add-hook 'window-configuration-change-hook
              #'context-navigator-stats-split--maybe-autoclose)
    (setq context-navigator-stats-split--wcch-on t)))

(defun context-navigator-stats-split--after-show (win)
  "Common post-show steps for WIN: subscriptions, hooks, render, fit, refit, notify."
  (when (window-live-p win)
    (context-navigator-stats-split--install-subs)
    (context-navigator-stats-split--ensure-wcch)
    (context-navigator-stats-split--render)
    (when-let ((navw (context-navigator-stats-split--nav-window)))
      (context-navigator-stats-split--fit-window win navw))
    ;; Refit on next tick to let layout settle.
    (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-stats-split--refit-all))))
    (context-navigator-stats-split--notify-changed))
  win)

(defun context-navigator-stats-split--open-fast-path (existing)
  "Refresh and refit EXISTING Stats window; return it."
  (context-navigator-stats-split--after-show existing))

(defun context-navigator-stats-split--create-window (navw buf)
  "Create the Stats window relative to NAVW showing BUF; return window."
  (let* ((target-height (max 1 (or context-navigator-stats-split-height 5)))
         (kind (window-parameter navw 'context-navigator-view)))
    (if (eq kind 'sidebar)
        ;; Sidebar: place Stats below Groups by giving it a lower priority (bigger slot)
        (let* ((side (or (window-parameter navw 'window-side) 'left))
               (params `((side . ,side)
                         (slot . 3)
                         (window-height . ,target-height))))
          (display-buffer-in-side-window buf params))
      ;; Buffer-mode: split below Groups window when present; otherwise below Navigator
      (let* ((base (catch 'hit
                     (dolist (w (window-list nil 'no-mini))
                       (when (and (window-live-p w)
                                  (window-parameter w 'context-navigator-groups))
                         (throw 'hit w)))
                     nil))
             (base (or base navw)))
        (or (condition-case _err
                (split-window base (- target-height) 'below)
              (error nil))
            (with-selected-window base
              (display-buffer-in-side-window
               buf `((side . bottom)
                     (slot . 0)
                     (window-height . ,target-height)))))))))

(defun context-navigator-stats-split--setup-window (win buf)
  "Finalize newly created WIN to display BUF and apply common steps."
  (when (window-live-p win)
    (set-window-buffer win buf)
    ;; Mark and dedicate stats window
    (set-window-parameter win 'context-navigator-stats t)
    (set-window-dedicated-p win t)
    (context-navigator-stats-split--after-show win)))

(defun context-navigator-stats-split--open-new (navw)
  "Create and show the Stats split relative to NAVW."
  (let* ((buf (context-navigator-stats-split--ensure-buffer))
         (win (context-navigator-stats-split--create-window navw buf)))
    (when (window-live-p win)
      (context-navigator-stats-split--setup-window win buf))))

;;;###autoload
(defun context-navigator-stats-split-open ()
  "Open the 5-line Stats split below the Navigator (sidebar or buffer-mode)."
  (interactive)
  (let ((navw (context-navigator-stats-split--nav-window)))
    (when (window-live-p navw)
      (let* ((inhibit-redisplay t)
             (window-combination-resize t)
             (existing (context-navigator-stats-split--visible-window)))
        (if (window-live-p existing)
            (context-navigator-stats-split--open-fast-path existing)
          (context-navigator-stats-split--open-new navw))))))

;;;###autoload
(defun context-navigator-stats-split-close ()
  "Close the Stats split if visible and remove event subscriptions."
  (interactive)
  (when-let* ((win (context-navigator-stats-split--visible-window)))
    (when (window-live-p win)
      (delete-window win)))
  (context-navigator-stats-split--remove-subs)
  (when context-navigator-stats-split--wcch-on
    (remove-hook 'window-configuration-change-hook
                 #'context-navigator-stats-split--maybe-autoclose)
    (setq context-navigator-stats-split--wcch-on nil))
  (context-navigator-stats-split--notify-changed)
  ;; Also refit after layout settles.
  (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-stats-split--refit-all))))
  t)

;;;###autoload
(defun context-navigator-stats-split-toggle ()
  "Toggle the 5-line Stats split below the Navigator sidebar."
  (interactive)
  (if (context-navigator-stats-split-visible-p)
      (context-navigator-stats-split-close)
    (context-navigator-stats-split-open)))

(provide 'context-navigator-stats-split)
;;; context-navigator-stats-split.el ends here
