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

(defun context-navigator-stats-split--maybe-autoclose ()
  "Close Stats split when Navigator sidebar window is no longer present."
  (let ((stats-win (context-navigator-stats-split--visible-window))
        (nav-win (context-navigator-stats-split--nav-window)))
    (when (and (window-live-p stats-win)
               (not (window-live-p nav-win)))
      (context-navigator-stats-split-close))))

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
  "Return a Navigator sidebar window on the current frame, or nil."
  (catch 'hit
    (dolist (w (window-list nil 'no-mini))
      (when (and (window-live-p w)
                 (eq (window-parameter w 'context-navigator-view) 'sidebar))
        (throw 'hit w)))
    nil))

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
When MG is ON and selection non-empty, show aggregated unified stats; otherwise reuse items footer."
  (let* ((tw (max 30 (or total-width 80)))
         ;; Detect MG and selection for current root
         (root (context-navigator-stats-split--current-root))
         (ps (and (stringp root) (ignore-errors (context-navigator-persist-state-load root))))
         (mg (and (listp ps) (plist-member ps :multi) (plist-get ps :multi)))
         (sel (and (listp ps) (plist-member ps :selected) (plist-get ps :selected)))
         (nsel (and (listp sel) (length sel))))
    (if (and mg (numberp nsel) (> nsel 0))
        ;; Aggregated unified stats (kick async if needed, then render from cache)
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
      ;; Default: reuse existing Stats footer (expanded)
      (let* ((context-navigator-stats--expanded-p t)
             (raw (or (ignore-errors (context-navigator-stats-footer-lines tw))
                      (list (propertize (context-navigator-i18n :stats) 'face 'shadow))))
             (lines (append raw nil)))
        (cond
         ((< (length lines) 5) (append lines (make-list (- 5 (length lines)) "")))
         ((> (length lines) 5) (cl-subseq lines 0 5))
         (t lines))))))

(defun context-navigator-stats-split--render-lines (total-width)
  "Return exactly 5 lines of Stats content for TOTAL-WIDTH columns.
- In items view: reuse existing Stats footer (expanded)
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

;;;###autoload
(defun context-navigator-stats-split-open ()
  "Open the 5-line Stats split below the Navigator sidebar."
  (interactive)
  (let ((navw (context-navigator-stats-split--nav-window)))
    (when (window-live-p navw)
      (let* ((buf (context-navigator-stats-split--ensure-buffer))
             (existing (context-navigator-stats-split--visible-window))
             (win (or existing
                      (let* ((side (or (window-parameter navw 'window-side) 'left))
                             (target-height (max 1 (or context-navigator-stats-split-height 5)))
                             (params (list (cons 'side side)
                                           (cons 'slot 1)
                                           (cons 'window-height target-height))))
                        (display-buffer-in-side-window buf params)))))
        (when (window-live-p win)
          (set-window-buffer win buf)
          ;; Make this a child window of the sidebar and keep it dedicated
          (set-window-parameter win 'context-navigator-stats t)
          (set-window-dedicated-p win t)
          ;; Install subscriptions and auto-close watcher
          (context-navigator-stats-split--install-subs)
          (unless context-navigator-stats-split--wcch-on
            (add-hook 'window-configuration-change-hook
                      #'context-navigator-stats-split--maybe-autoclose)
            (setq context-navigator-stats-split--wcch-on t))
          (context-navigator-stats-split--render)
          (context-navigator-stats-split--notify-changed)
          win)))))

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
