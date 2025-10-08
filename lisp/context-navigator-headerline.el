;;; context-navigator-headerline.el --- Header-line controls for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Puts Navigator control toggles and actions into the header-line of the
;; Navigator buffer. Enabled by default and configurable via:
;;   - context-navigator-view-headerline-enable
;;
;; The header-line shows only interactive control segments (toggles + actions)
;; produced by the controls module; the project/group title is rendered inside
;; the buffer itself (above the \"..\" line). Per-point status is displayed in
;; the modeline.
;;
;; This module avoids hard requires on the view module to prevent load cycles:
;; it declares the helper functions used and calls them when available.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)

(defgroup context-navigator-headerline nil
  "Header-line controls for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-view-headerline-enable t
  "When non-nil, show the Navigator title in the header-line."
  :type 'boolean :group 'context-navigator-headerline)

;; Declarations for byte-compiler friendliness (provided by context-navigator-view).
(declare-function context-navigator-view-controls-segments "context-navigator-view-controls" ())
(declare-function context-navigator-view--header "context-navigator-view" (state))
(declare-function context-navigator--state-get "context-navigator-core" ())
(declare-function context-navigator-state-last-project-root "context-navigator-core" (state))
(declare-function context-navigator-state-current-group-slug "context-navigator-core" (state))
(declare-function context-navigator-persist-state-load "context-navigator-persist" (root))
(declare-function context-navigator-persist-context-file "context-navigator-persist" (root slug))
(declare-function context-navigator-persist-group-enabled-count "context-navigator-persist" (file))
(declare-function context-navigator-title-fallback-line "context-navigator-view-title" (&optional mode))
(declare-function context-navigator-icons-for-indicator "context-navigator-icons" (state))

(defvar-local context-navigator-headerline--cache-key nil)
(defvar-local context-navigator-headerline--cache-str nil)

(defun context-navigator-headerline--mode-and-title ()
  "Return (MODE-SYM TITLE) for the current Navigator view."
  (let* ((mode-sym (and (boundp 'context-navigator-view--mode)
                        context-navigator-view--mode))
         (title (ignore-errors
                  (when (fboundp 'context-navigator-title-fallback-line)
                    (context-navigator-title-fallback-line mode-sym)))))
    (list mode-sym (if (stringp title) title ""))))

(defun context-navigator-headerline--root-and-slug ()
  "Return (ROOT SLUG) from the current Navigator state."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (ignore-errors (context-navigator-state-last-project-root st))))
         (slug (and st (ignore-errors (context-navigator-state-current-group-slug st)))))
    (list root slug)))

(defun context-navigator-headerline--mg-flag (_root)
  "Return non-nil when Multi-group is active for ROOT.
Avoid disk IO during redisplay: do not read state.el here."
  nil)

(defun context-navigator-headerline--counts (_root _slug mg)
  "Return (ENABLED TOTAL) from in-memory items; if MG, return 0 0.
Never read files during redisplay."
  (if mg
      (list 0 0)
    (let* ((st (ignore-errors (context-navigator--state-get)))
           (items (and st (ignore-errors (context-navigator-state-items st)))))
      (let* ((tot (length (or items '())))
             (en (and (listp items)
                      (cl-count-if (lambda (it)
                                     (and (context-navigator-item-p it)
                                          (context-navigator-item-enabled it)))
                                   items))))
        (list (or en 0) (or tot 0))))))

(defun context-navigator-headerline--state (mg en tot)
  "Derive tri-state symbol from MG and counts EN, TOT."
  (cond
   (mg 'ok)
   ((<= (or tot 0) 0) 'absent)
   ((<= (or en 0) 0) 'absent)
   ((< en tot) 'mismatch)
   (t 'ok)))

(defun context-navigator-headerline--lamp (state mg en tot)
  "Build colored lamp string for STATE. Adds hover with counts or MG note."
  (let ((lamp0
         (or
          (ignore-errors
            (if (fboundp 'context-navigator-icons-for-indicator)
                (context-navigator-icons-for-indicator state)
              ;; Fallback to text bullet with colors
              (let* ((color (pcase state
                              ('ok "green4") ('mismatch "goldenrod2") (_ "gray")))
                     (char  (if (eq state 'ok) "●" "○")))
                (propertize char 'face (list :foreground color :height 0.75)
                            'display '(raise 0.08)))))
          "")))
    (if (and (stringp lamp0) (> (length lamp0) 0))
        (propertize lamp0 'help-echo
                    (if mg
                        "Multi-group: aggregated apply"
                      (format "Enabled %d of %d" (or en 0) (or tot 0))))
      lamp0)))

(defun context-navigator-headerline--compose (mode-sym slug title lamp)
  "Insert LAMP before the group icon in items mode when group is active; else prefix."
  (cond
   ((and (eq mode-sym 'items) (stringp slug) (not (string-empty-p slug)))
    (let ((idx (string-match "  " title)))
      (if idx
          (concat (substring title 0 (+ idx 2)) lamp " " (substring title (+ idx 2)))
        (concat lamp " " title))))
   (t
    (concat lamp " " title))))

(defun context-navigator-headerline-format ()
  "Return header-line content for Navigator buffers (title with light caching).

Now renders the project/group title in the header-line. Controls are shown
inside the buffer at the top."
  (when (eq major-mode 'context-navigator-view-mode)
    (condition-case _safe
        (pcase-let* ((`(,mode-sym ,title) (context-navigator-headerline--mode-and-title))
                     (`(,root ,slug) (context-navigator-headerline--root-and-slug))
                     (mg (context-navigator-headerline--mg-flag root))
                     (`(,en ,tot) (context-navigator-headerline--counts root slug mg))
                     (state (context-navigator-headerline--state mg en tot))
                     (lamp (context-navigator-headerline--lamp state mg en tot))
                     (out (context-navigator-headerline--compose mode-sym slug title lamp))
                     (key (list mode-sym out state en tot)))
          (if (equal key context-navigator-headerline--cache-key)
              context-navigator-headerline--cache-str
            (setq context-navigator-headerline--cache-key key
                  context-navigator-headerline--cache-str out)
            out))
      ;; Robust fallback: never blank the header-line on errors.
      (let* ((fallback (ignore-errors
                         (when (fboundp 'context-navigator-title-fallback-line)
                           (context-navigator-title-fallback-line)))))
        (or (and (stringp fallback) fallback) "")))))

(defun context-navigator-headerline--apply (buffer)
  "Apply or remove header-line controls in BUFFER based on the feature flag."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq major-mode 'context-navigator-view-mode)
        (if context-navigator-view-headerline-enable
            (setq header-line-format '((:eval (context-navigator-headerline-format))))
          ;; Remove only if we had previously installed our own format.
          (when (equal header-line-format '((:eval (context-navigator-headerline-format))))
            (setq header-line-format nil)))
        (ignore-errors
          (context-navigator-debug :debug :ui "headerline applied: enabled=%s, format=%S"
                                   context-navigator-view-headerline-enable header-line-format))
        (force-mode-line-update t)))))

;; React to runtime toggling of the header-line feature.
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher
   'context-navigator-view-headerline-enable
   (lambda (_sym _newval _op _where)
     (dolist (buf (buffer-list))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (eq major-mode 'context-navigator-view-mode)
             (context-navigator-headerline--apply (current-buffer)))))))))

(provide 'context-navigator-headerline)
;;; context-navigator-headerline.el ends here
