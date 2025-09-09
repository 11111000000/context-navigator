;;; context-navigator-log.el --- Minimal logging for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Centralized logging facility:
;; - Single universal entry: (context-navigator-debug LEVEL [TOPIC] FMT &rest ARGS)
;; - Special log buffer: "*Context Navigator Log*"
;; - Toggle + levels (:error :warn :info :debug :trace), default :info when ON
;; - Auto-open on errors (no echo duplication), no autoscroll
;; - Trimming to max lines; optional persistent file append
;; - Subscribes to key events to produce concise logs
;;
;; Notes:
;; - Even when disabled, :error is still captured (and may auto-open the log).
;; - Use symbols/keywords for LEVEL and TOPIC; TOPIC is optional (defaults to :core).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-events)

(defgroup context-navigator-log nil
  "Logging settings for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-log-enabled nil
  "When non-nil, logging is enabled for levels >= `context-navigator-log-level'."
  :type 'boolean :group 'context-navigator-log)

(defcustom context-navigator-log-level :info
  "Minimal level to log when enabled. One of :error :warn :info :debug :trace."
  :type '(choice (const :error) (const :warn) (const :info) (const :debug) (const :trace))
  :group 'context-navigator-log)

(defcustom context-navigator-log-auto-open-on-error t
  "When non-nil, automatically open the log buffer on :error entries."
  :type 'boolean :group 'context-navigator-log)

(defcustom context-navigator-log-buffer-name "*Context Navigator Log*"
  "Name of the dedicated log buffer."
  :type 'string :group 'context-navigator-log)

(defcustom context-navigator-log-max-lines 5000
  "Maximum number of lines kept in the log buffer (older lines are trimmed)."
  :type 'integer :group 'context-navigator-log)

(defcustom context-navigator-log-truncate-length 800
  "Max length of formatted message payload before truncation (0 or nil to disable)."
  :type 'integer :group 'context-navigator-log)

(defcustom context-navigator-log-file-enable nil
  "When non-nil, also append each log line to `context-navigator-log-file'."
  :type 'boolean :group 'context-navigator-log)

(defcustom context-navigator-log-file nil
  "Absolute path to a file used for persistent logging when enabled."
  :type '(choice (const :tag "Disabled" nil)
                 (file :tag "File path"))
  :group 'context-navigator-log)

;; Faces for levels ------------------------------------------------------------

(defface context-navigator-log-error-face
  '((t :foreground "red3"))
  "Face for error log lines."
  :group 'context-navigator-log)

(defface context-navigator-log-warn-face
  '((t :foreground "orange3"))
  "Face for warning log lines."
  :group 'context-navigator-log)

(defface context-navigator-log-info-face
  '((t :foreground "gray70"))
  "Face for info log lines."
  :group 'context-navigator-log)

(defface context-navigator-log-debug-face
  '((t :foreground "gray55"))
  "Face for debug log lines."
  :group 'context-navigator-log)

(defface context-navigator-log-trace-face
  '((t :foreground "gray40"))
  "Face for trace log lines."
  :group 'context-navigator-log)

(defun context-navigator-log--face-for (lvl)
  "Return face symbol for LVL."
  (pcase lvl
    ((or :error 'error) 'context-navigator-log-error-face)
    ((or :warn  'warn)  'context-navigator-log-warn-face)
    ((or :info  'info)  'context-navigator-log-info-face)
    ((or :debug 'debug) 'context-navigator-log-debug-face)
    ((or :trace 'trace) 'context-navigator-log-trace-face)
    (_ 'context-navigator-log-info-face)))

;; Internal --------------------------------------------------------------------

(defvar context-navigator-log--subs nil
  "Tokens of event subscriptions installed by this module.")

(defun context-navigator-log--level->num (lvl)
  (pcase lvl
    ((or :error 'error) 0)
    ((or :warn  'warn)  1)
    ((or :info  'info)  2)
    ((or :debug 'debug) 3)
    ((or :trace 'trace) 4)
    (_ 2)))

(defun context-navigator-log--should-log-p (lvl)
  "Return non-nil when LVL should be logged.
When disabled, still capture :error."
  (or (and (not context-navigator-log-enabled)
           (<= (context-navigator-log--level->num lvl)
               (context-navigator-log--level->num :error)))
      (and context-navigator-log-enabled
           (<= (context-navigator-log--level->num lvl)
               (context-navigator-log--level->num (or context-navigator-log-level :info))))))

(defun context-navigator-log--ts ()
  "Return timestamp string HH:MM:SS.mmm."
  (let* ((time (current-time))
         (ms (nth 2 (decode-time time)))
         (fmt (format-time-string "%H:%M:%S" time)))
    (format "%s.%03d" fmt (floor (* 1000 (nth 2 (time-subtract time (seconds-to-time (float-time (format-time-string "%T" time))))))))))

(defun context-navigator-log--ensure-buffer ()
  "Create/initialize the log buffer."
  (let ((buf (get-buffer-create context-navigator-log-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (setq-local buffer-read-only t)
      (setq-local truncate-lines t))
    buf))

(defun context-navigator-log--trim-if-needed (buf)
  "Trim BUF to `context-navigator-log-max-lines' lines."
  (when (and (integerp context-navigator-log-max-lines)
             (> context-navigator-log-max-lines 0))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (let* ((total (max 0 (1- (line-number-at-pos)))) ;; ignore trailing newline
                 (overflow (max 0 (- total context-navigator-log-max-lines))))
            (when (> overflow 0)
              (goto-char (point-min))
              (forward-line overflow)
              (delete-region (point-min) (point)))))))))

(defun context-navigator-log--append-file (line)
  "Append LINE to the persistent log file when enabled."
  (when (and context-navigator-log-file-enable
             (stringp context-navigator-log-file)
             (not (string-empty-p context-navigator-log-file)))
    (condition-case _err
        (write-region (concat line "\n") nil context-navigator-log-file 'append 'silent)
      (error nil))))

(defun context-navigator-log--open-on-error (lvl)
  (when (and context-navigator-log-auto-open-on-error
             (<= (context-navigator-log--level->num lvl)
                 (context-navigator-log--level->num :error)))
    (ignore-errors (context-navigator-log-open))))

(defun context-navigator-log--maybe-trunc (s)
  (let ((n (or context-navigator-log-truncate-length 0)))
    (if (and (integerp n) (> n 0) (stringp s) (> (length s) n))
        (concat (substring s 0 (max 0 (- n 1))) "…")
      s)))

(defun context-navigator-log--format-line (lvl topic msg)
  (let* ((ts (format-time-string "%H:%M:%S.%3N"))
         (lv (upcase (substring (format "%s" lvl) 1))) ;; :info -> INFO
         (tp (if topic (format "%s" topic) "-")))
    (format "[%s][%s][%s] %s" ts lv tp (or msg ""))))

;; Public API ------------------------------------------------------------------

;;;###autoload
(defun context-navigator-log-toggle ()
  "Toggle logging enable flag."
  (interactive)
  (setq context-navigator-log-enabled (not context-navigator-log-enabled))
  (context-navigator-debug :info :log "logging %s (level=%s)"
                           (if context-navigator-log-enabled "ON" "OFF")
                           context-navigator-log-level))

;;;###autoload
(defun context-navigator-log-open ()
  "Open the log buffer in a window (do not select it)."
  (interactive)
  (let* ((buf (context-navigator-log--ensure-buffer)))
    (save-selected-window
      (display-buffer buf '((display-buffer-pop-up-window))))
    buf))

;;;###autoload
(defun context-navigator-log-clear ()
  "Clear the log buffer."
  (interactive)
  (when-let ((buf (get-buffer context-navigator-log-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))

;;;###autoload
(defun context-navigator-log-set-level (level)
  "Interactively set logging LEVEL (:error :warn :info :debug :trace)."
  (interactive
   (list (intern (completing-read "Log level: " '(":error" ":warn" ":info" ":debug" ":trace")
                                  nil t nil nil ":info"))))
  (setq context-navigator-log-level level)
  (context-navigator-debug :info :log "level set to %s" level))

;;;###autoload
(defun context-navigator-log-save-to-file (file)
  "Save current log buffer to FILE (overwrite)."
  (interactive "FSave log to file: ")
  (let ((buf (get-buffer context-navigator-log-buffer-name)))
    (if (not (buffer-live-p buf))
        (message "No log buffer")
      (with-current-buffer buf
        (write-region (point-min) (point-max) file nil 'silent)
        (message "Log saved to %s" (abbreviate-file-name file))))))

;;;###autoload
(defun context-navigator-log-toggle-file-persistence (&optional file)
  "Toggle persistent log file append.
When enabling and FILE is provided (or prompted), set it as destination."
  (interactive)
  (setq context-navigator-log-file-enable (not context-navigator-log-file-enable))
  (when (and context-navigator-log-file-enable
             (or (not (stringp context-navigator-log-file))
                 (string-empty-p context-navigator-log-file)))
    (setq context-navigator-log-file
          (or file (read-file-name "Persistent log file: " nil nil nil "context-navigator.log"))))
  (context-navigator-debug :info :log "file logging %s (%s)"
                           (if context-navigator-log-file-enable "ON" "OFF")
                           (or context-navigator-log-file "<unset>")))

;;;###autoload
(defun context-navigator-debug (&rest args)
  "Universal logging entry.

Usage patterns:
- (context-navigator-debug :level :topic \"fmt\" args…)
- (context-navigator-debug :level \"fmt\" args…)        ;; topic defaults to :core
- (context-navigator-debug \"fmt\" args…)               ;; level=:info, topic=:core

LEVEL is one of :error :warn :info :debug :trace.
TOPIC is a short symbol/keyword (e.g. :core, :persist, :gptel, :events)."
  (let* ((lvl (if (keywordp (car args)) (pop args) :info))
         (maybe (car args))
         (topic (if (and maybe (not (stringp maybe)))
                    (prog1 maybe (setq args (cdr args)))
                  :core))
         (fmt (or (car args) ""))
         (rest (cdr args)))
    (when (context-navigator-log--should-log-p lvl)
      (let* ((msg (condition-case _ (apply #'format fmt rest) (error (format "%s" fmt))))
             (msg (context-navigator-log--maybe-trunc msg))
             (line (context-navigator-log--format-line lvl topic msg))
             (buf (context-navigator-log--ensure-buffer)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (let ((start (point)))
                (insert line "\n")
                ;; Colorize only the [LEVEL] segment
                (let* ((m (string-match "\\[[^]]+\\]\\(\\[[^]]+\\]\\)" line))
                       (lb (and m (+ start (match-beginning 1))))
                       (le (and m (+ start (match-end 1)))))
                  (when (and lb le)
                    (add-text-properties lb le
                                         (list 'face (context-navigator-log--face-for lvl)))))))))
        (context-navigator-log--trim-if-needed buf)
        (context-navigator-log--append-file line)
        (context-navigator-log--open-on-error lvl)))))

;; Event subscriptions ---------------------------------------------------------

(defun context-navigator-log--install-subs ()
  "Subscribe to key events for automatic concise logs (idempotent)."
  (unless context-navigator-log--subs
    (push (context-navigator-events-subscribe
           :project-switch
           (lambda (root)
             (context-navigator-debug :info :events "project-switch → %s"
                                      (or root "~"))))
          context-navigator-log--subs)
    (push (context-navigator-events-subscribe
           :context-load-start
           (lambda (root)
             (context-navigator-debug :info :events "load-start %s" (or root "~"))))
          context-navigator-log--subs)
    (push (context-navigator-events-subscribe
           :context-load-step
           (lambda (root pos total)
             (when (and (integerp pos) (integerp total) (> total 0))
               (context-navigator-debug :debug :events "load-step %s %d/%d" (or root "~") pos total))))
          context-navigator-log--subs)
    (push (context-navigator-events-subscribe
           :context-load-done
           (lambda (root ok)
             (context-navigator-debug (if ok :info :error) :events
                                      "load-done %s (%s)" (or root "~")
                                      (if ok "ok" "failed"))))
          context-navigator-log--subs)
    (push (context-navigator-events-subscribe
           :groups-list-updated
           (lambda (_root groups)
             (let ((n (and (listp groups) (length groups))))
               (context-navigator-debug :debug :events "groups-list updated: %s" (or n 0)))))
          context-navigator-log--subs)
    (push (context-navigator-events-subscribe
           :group-switch-start
           (lambda (root slug)
             (context-navigator-debug :info :events "group-switch start: %s (%s)" (or slug "?") (or root "~"))))
          context-navigator-log--subs)
    (push (context-navigator-events-subscribe
           :group-switch-done
           (lambda (root slug ok)
             (context-navigator-debug (if ok :info :warn) :events
                                      "group-switch done: %s (%s) -> %s"
                                      (or slug "?") (or root "~") (if ok "ok" "failed"))))
          context-navigator-log--subs)
    (push (context-navigator-events-subscribe
           :gptel-change
           (lambda (&rest args)
             (context-navigator-debug :debug :gptel "gptel-change: %s"
                                      (mapconcat (lambda (x) (format "%s" x)) args " "))))
          context-navigator-log--subs)
    (push (context-navigator-events-subscribe
           :model-refreshed
           (lambda (&rest _)
             (context-navigator-debug :trace :model "model refreshed")))
          context-navigator-log--subs)))

(context-navigator-log--install-subs)

(provide 'context-navigator-log)
;;; context-navigator-log.el ends here
