;;; context-navigator-razor.el --- Occam filter for context items via LLM -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; "AI Occam's Razor" — safely minimizes the enabled context for the task
;; described in the current org document:
;; - Takes the active region (if any) or the whole org buffer
;; - Collects enabled group items (file/buffer/selection) with full content
;; - Warns about TRAMP and large payloads (configurable)
;; - Sends a cautious, low-cost prompt to the model (via gptel)
;; - Expects strict JSON {keep_keys:[...], rationale:"..."}; one retry on format error
;; - Applies: leaves enabled only the returned keys (disables the rest)
;; - If auto-push is on — applies to gptel immediately
;; - Maintains per-group history (Undo/Redo in the header-line)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-log)
(require 'context-navigator-i18n)

(defgroup context-navigator-razor nil
  "Occam (AI) context filter for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-razor-model "gpt-5-mini"
  "Default cheap model to use via gptel."
  :type 'string :group 'context-navigator-razor)

(defcustom context-navigator-razor-temperature 0.1
  "Temperature for the selection request."
  :type 'number :group 'context-navigator-razor)

(defcustom context-navigator-razor-max-output-tokens 256
  "Max tokens for the model response."
  :type 'integer :group 'context-navigator-razor)

(defcustom context-navigator-razor-timeout 45
  "Timeout (seconds) for the request."
  :type 'integer :group 'context-navigator-razor)

(defcustom context-navigator-razor-apply-immediately t
  "When non-nil, apply the result immediately (otherwise show preview confirmation)."
  :type 'boolean :group 'context-navigator-razor)

(defcustom context-navigator-razor-preview t
  "When non-nil, show a brief preview (counts) before applying."
  :type 'boolean :group 'context-navigator-razor)

(defcustom context-navigator-razor-remote-include t
  "Include TRAMP/remote files content (with a warning prompt)."
  :type 'boolean :group 'context-navigator-razor)

(defcustom context-navigator-razor-large-bytes-threshold 600000
  "Warn when payload exceeds this many bytes (org + items)."
  :type 'integer :group 'context-navigator-razor)

(defcustom context-navigator-razor-budget-tokens-limit 100000
  "Rough budget tokens limit (bytes/4). Used only for warnings."
  :type 'integer :group 'context-navigator-razor)

(defvaralias 'context-navigator-razor-undo-depth 'context-navigator-undo-depth)
(put 'context-navigator-razor-undo-depth 'obsolete-variable
     "Use `context-navigator-undo-depth' instead.")

(defcustom context-navigator-razor-strict-json t
  "When non-nil, enforce strict JSON and perform one retry if parsing fails."
  :type 'boolean :group 'context-navigator-razor)

;; History storage: group-key -> (:past list<items-snapshot> :future list<items-snapshot>)
(defvar context-navigator-razor--history (make-hash-table :test 'equal))

(defun context-navigator-razor--group-key ()
  "Return a stable key for current group (root + slug)."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (slug (and st (context-navigator-state-current-group-slug st))))
    (format "%s|%s" (or root "~") (or slug "<none>"))))

(defun context-navigator-razor--snapshot ()
  "Return a deep-ish snapshot (copy) of current items list."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st))))
    (mapcar (lambda (it)
              (context-navigator-item-create
               :type (context-navigator-item-type it)
               :name (context-navigator-item-name it)
               :path (context-navigator-item-path it)
               :buffer (context-navigator-item-buffer it)
               :beg (context-navigator-item-beg it)
               :end (context-navigator-item-end it)
               :size (context-navigator-item-size it)
               :enabled (context-navigator-item-enabled it)
               :meta (context-navigator-item-meta it)))
            (or items '()))))

(defun context-navigator-razor--history-get (key)
  (or (gethash key context-navigator-razor--history)
      (let ((cell (list :past '() :future '())))
        (puthash key cell context-navigator-razor--history)
        cell)))

(defun context-navigator-razor--history-push (key snapshot)
  "Push SNAPSHOT to past history for KEY, trim, and clear future."
  (let* ((cell (context-navigator-razor--history-get key))
         (past (plist-get cell :past)))
    (setq past (cons snapshot past))
    (when (> (length past) (max 1 context-navigator-razor-undo-depth))
      (setq past (cl-subseq past 0 context-navigator-razor-undo-depth)))
    (setf (plist-get cell :past) past)
    (setf (plist-get cell :future) '())
    (puthash key cell context-navigator-razor--history)))

(defun context-navigator-razor--history-undo (key)
  "Return previous snapshot and move current to future; nil when empty."
  (let* ((cell (context-navigator-razor--history-get key))
         (past (plist-get cell :past)))
    (when (consp past)
      (let* ((prev (car past))
             (rest (cdr past))
             (cur (context-navigator-razor--snapshot))
             (future (cons cur (plist-get cell :future))))
        (setf (plist-get cell :past) rest)
        (setf (plist-get cell :future) future)
        (puthash key cell context-navigator-razor--history)
        prev))))

(defun context-navigator-razor--history-redo (key)
  "Return next snapshot from future; move current to past; nil when empty."
  (let* ((cell (context-navigator-razor--history-get key))
         (future (plist-get cell :future)))
    (when (consp future)
      (let* ((next (car future))
             (rest (cdr future))
             (cur (context-navigator-razor--snapshot))
             (past (cons cur (plist-get cell :past))))
        (setf (plist-get cell :future) rest)
        (setf (plist-get cell :past) past)
        (puthash key cell context-navigator-razor--history)
        next))))

;;;###autoload
(defun context-navigator-razor-snapshot-push ()
  "Compatibility shim: delegate snapshot push to core history."
  (interactive)
  (if (fboundp 'context-navigator-snapshot-push)
      (context-navigator-snapshot-push)
    (message "History not available")))

(defun context-navigator-razor--apply-snapshot (snapshot)
  "Apply SNAPSHOT items to model and push to gptel when auto-push is ON."
  (ignore-errors (context-navigator-set-items (or snapshot '())))
  (when (and (boundp 'context-navigator--push-to-gptel)
             context-navigator--push-to-gptel)
    (ignore-errors (context-navigator-gptel-apply snapshot))))

;;;###autoload
(defun context-navigator-razor-undo ()
  "Compatibility shim: delegate to core undo."
  (interactive)
  (if (fboundp 'context-navigator-undo)
      (context-navigator-undo)
    (message "Nothing to undo")))

;;;###autoload
(defun context-navigator-razor-redo ()
  "Compatibility shim: delegate to core redo."
  (interactive)
  (if (fboundp 'context-navigator-redo)
      (context-navigator-redo)
    (message "Nothing to redo")))

(defun context-navigator-razor--human-size (bytes)
  (cond
   ((null bytes) "?")
   ((< bytes 1024) (format "%d B" bytes))
   ((< bytes 1048576) (format "%.1f KB" (/ bytes 1024.0)))
   ((< bytes 1073741824) (format "%.1f MB" (/ bytes 1048576.0)))
   (t (format "%.1f GB" (/ bytes 1073741824.0)))))

(defun context-navigator-razor--org-source ()
  "Return cons (TEXT . SRC-META) where SRC-META is plist (:region t|nil :remote N)."
  (let* ((has-region (use-region-p))
         (txt (buffer-substring-no-properties
               (if has-region (region-beginning) (point-min))
               (if has-region (region-end) (point-max))))
         (remote (and buffer-file-name (file-remote-p buffer-file-name))))
    (cons txt (list :region has-region :remote (and remote t)))))

(defun context-navigator-razor--read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun context-navigator-razor--read-buffer (buf)
  (with-current-buffer buf
    (buffer-substring-no-properties (point-min) (point-max))))

(defun context-navigator-razor--read-selection (buf beg end path)
  (cond
   ((and (bufferp buf) (buffer-live-p buf))
    (with-current-buffer buf
      (buffer-substring-no-properties (max (point-min) (min beg end))
                                      (min (point-max) (max beg end)))))
   ((and (stringp path) (file-exists-p path))
    ;; Fallback: open file and slice
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-substring-no-properties (max (point-min) (min beg end))
                                      (min (point-max) (max beg end)))))
   (t "")))

(defun context-navigator-razor--collect-enabled-items ()
  "Return list of plists: (:key :type :name :path :size :content :remote)."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st)))
         (enabled (cl-remove-if-not #'context-navigator-item-enabled items))
         (res '()))
    (dolist (it enabled)
      (let* ((type (context-navigator-item-type it))
             (name (context-navigator-item-name it))
             (path (context-navigator-item-path it))
             (buf  (context-navigator-item-buffer it))
             (beg  (context-navigator-item-beg it))
             (end  (context-navigator-item-end it))
             (key  (context-navigator-model-item-key it))
             (remote (and (stringp path) (file-remote-p path)))
             (content
              (pcase type
                ('file (and (stringp path) (file-exists-p path)
                            (context-navigator-razor--read-file path)))
                ('buffer (cond
                          ((and (bufferp buf) (buffer-live-p buf))
                           (context-navigator-razor--read-buffer buf))
                          ((and (stringp path) (file-exists-p path))
                           (context-navigator-razor--read-file path))
                          (t "")))
                ('selection (context-navigator-razor--read-selection buf beg end path))
                (_ "")))
             (size (and (stringp content) (string-bytes content))))
        (push (list :key key :type type :name name :path path
                    :size (or size 0) :content (or content "") :remote (and remote t))
              res)))
    (nreverse res)))

(defun context-navigator-razor--payload-bytes (org-text items)
  (+ (string-bytes (or org-text ""))
     (cl-loop for it in items sum (or (plist-get it :size) 0))))

(defun context-navigator-razor--warn-remote (items)
  (let* ((n (cl-count-if (lambda (pl) (plist-get pl :remote)) items)))
    (when (and (> n 0) context-navigator-razor-remote-include)
      (yes-or-no-p (format (context-navigator-i18n :razor-remote-warn) n)))))

(defun context-navigator-razor--warn-large (bytes)
  (when (and (integerp context-navigator-razor-large-bytes-threshold)
             (> bytes context-navigator-razor-large-bytes-threshold))
    (yes-or-no-p
     (format (context-navigator-i18n :razor-large-warn)
             (context-navigator-razor--human-size bytes)))))

(defun context-navigator-razor--system-prompt ()
  "Return cautious system prompt."
  "You are a cautious context selector. Choose the minimal but safe subset of items strictly necessary to solve the task described in the provided org document. When uncertain, prefer keeping an item rather than excluding it. Output strict JSON only.")

(defun context-navigator-razor--build-user (org-text items)
  "Build user message string with ORG-TEXT and ITEMS."
  (let ((sb (list)))
    (push "Task document (org):\n" sb)
    (push "=org\n" sb)
    (push org-text sb)
    (push "\n=\n\n" sb)
    (push "Context items (enabled):\n" sb)
    (dolist (pl items)
      (let ((key (plist-get pl :key))
            (type (plist-get pl :type))
            (name (plist-get pl :name))
            (path (plist-get pl :path))
            (size (plist-get pl :size))
            (content (plist-get pl :content)))
        (push (format "- key: %s\ntype: %s\nname: %s\npath: %s\nsize_bytes: %s\ncontent:\n" key type name (or path "") (or size 0)) sb)
        (push "=text\n" sb)
        (push content sb)
        (push "\n=\n\n" sb)))
    (push "Instruction: Return JSON ONLY of the form {\"keep_keys\": [\"<item-key>\", ...], \"rationale\": \"<optional short>\"}. If in doubt, include the item.\n" sb)
    (apply #'concat (nreverse sb))))

(defun context-navigator-razor--extract-json (s)
  "Try to extract a JSON object from S and parse it."
  (let* ((str (or s ""))
         (start (string-match "{" str)))
    (when start
      (let ((end nil) (lvl 0) (i start) (n (length str)))
        (while (and (< i n) (null end))
          (let ((ch (aref str i)))
            (cond
             ((= ch ?{) (setq lvl (1+ lvl)))
             ((= ch ?}) (setq lvl (1- lvl))
              (when (= lvl 0) (setq end i))))
            (setq i (1+ i))))
        (when (and end (>= end start))
          (json-parse-string (substring str start (1+ end))
                             :object-type 'plist :array-type 'list :null-object nil))))))

(defun context-navigator-razor--parse-keep (response)
  "Parse RESPONSE text to plist {:keep_keys [...]} with tolerant extraction."
  (condition-case _err
      (let ((jo (context-navigator-razor--extract-json response)))
        (when (plist-get jo :keep_keys)
          (list :keep_keys (plist-get jo :keep_keys)
                :rationale (plist-get jo :rationale))))
    (error nil)))

(defun context-navigator-razor--apply-keep (keep-keys)
  "Apply KEEP-KEYS: set enabled only for those keys; push undo snapshot; auto-push if enabled."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st)))
         (_idx (and items (context-navigator-model-build-index items)))
         (keys (and (listp keep-keys) keep-keys))
         (before (context-navigator-razor--snapshot))
         (gkey (context-navigator-razor--group-key)))
    ;; push history (moved to core)
    (ignore-errors (context-navigator-snapshot-push))
    ;; build new items with enabled flags according to keep
    (let ((set (and keys (make-hash-table :test 'equal))))
      (dolist (k keys) (puthash k t set))
      (let (new)
        (dolist (it items)
          (let* ((k (context-navigator-model-item-key it))
                 (en (and set (gethash k set))))
            (push (context-navigator-item-create
                   :type (context-navigator-item-type it)
                   :name (context-navigator-item-name it)
                   :path (context-navigator-item-path it)
                   :buffer (context-navigator-item-buffer it)
                   :beg (context-navigator-item-beg it)
                   :end (context-navigator-item-end it)
                   :size (context-navigator-item-size it)
                   :enabled (and en t)
                   :meta (context-navigator-item-meta it))
                  new)))
        (setq new (nreverse new))
        (ignore-errors (context-navigator-set-items new))
        (when (and (boundp 'context-navigator--push-to-gptel)
                   context-navigator--push-to-gptel)
          (ignore-errors (context-navigator-gptel-apply new)))
        (let* ((total (length (or items '())))
               (kept (length (or keys '()))))
          (ignore-errors
            (context-navigator-debug :info :razor
                                     "Applied keep set: kept=%d total=%d" kept total))
          (message (context-navigator-i18n :razor-done) kept total))))))

(defun context-navigator-razor--confirm-empty (keep)
  (when (or (null keep) (= (length keep) 0))
    (yes-or-no-p (context-navigator-i18n :razor-empty-confirm))))

(defun context-navigator-razor--preview-keep (keep total)
  (if (not context-navigator-razor-preview)
      t
    (yes-or-no-p
     (format (context-navigator-i18n :razor-preview-title)
             (length (or keep '())) total))))

(defun context-navigator-razor--gptel-opts (sys cb)
  "Build options plist for gptel with SYS and wrapped callback CB.

Note: gptel-request does not accept :model/:temperature/:max-tokens etc.
We set those via let-binding around the call. Here we only return
the supported keyword args for gptel-request."
  (list :system   sys
        :callback (lambda (response info)
                    (ignore info)
                    (let ((len (length (or response ""))))
                      (ignore-errors
                        (context-navigator-debug :debug :razor
                                                 "gptel callback: response length=%d" len)))
                    (funcall cb (or response "")))))

(defun context-navigator-razor--gptel-call (user opts)
  "Call gptel with USER text and OPTS plist. Return request object or nil.

OPTS must contain only supported gptel-request keywords (e.g., :system, :callback).
Model/temperature/max-tokens are let-bound via gptel variables."
  (unless (require 'gptel nil t)
    (user-error "gptel is not available"))
  (let* ((sys (plist-get opts :system))
         (cb  (plist-get opts :callback))
         (model (if (symbolp context-navigator-razor-model)
                    context-navigator-razor-model
                  (ignore-errors (intern context-navigator-razor-model)))))
    (ignore-errors
      (context-navigator-debug :info :razor
                               "Requesting model=%s temp=%.2f timeout=%ss max-out=%d"
                               (or model context-navigator-razor-model)
                               context-navigator-razor-temperature
                               context-navigator-razor-timeout
                               context-navigator-razor-max-output-tokens))
    (let ((gptel-model       (or model gptel-model))
          (gptel-temperature context-navigator-razor-temperature)
          (gptel-max-tokens  context-navigator-razor-max-output-tokens)
          (gptel-stream      nil))
      (gptel-request user :system sys :callback cb))))

(defun context-navigator-razor--call-model (sys user cb)
  "Compatibility wrapper: build OPTS from SYS/CB and call gptel with USER."
  (let* ((opts (context-navigator-razor--gptel-opts sys cb)))
    (context-navigator-razor--gptel-call user opts)))

(defun context-navigator-razor--collect-run-input ()
  "Collect input for the razor run and log initial metrics.
Return plist: (:org-text :items :total-enabled :bytes :remote-n)."
  (let* ((org-pair (context-navigator-razor--org-source))
         (org-text (car org-pair))
         (items (context-navigator-razor--collect-enabled-items))
         (total-enabled (length items))
         (bytes (context-navigator-razor--payload-bytes org-text items))
         (remote-n (cl-count-if (lambda (pl) (plist-get pl :remote)) items)))
    (ignore-errors
      (context-navigator-debug :info :razor
                               "Start: enabled=%d, payload=%d bytes (~%d tok), remotes=%d"
                               total-enabled bytes
                               (floor (/ bytes 4.0))
                               remote-n))
    (list :org-text org-text
          :items items
          :total-enabled total-enabled
          :bytes bytes
          :remote-n remote-n)))

(defun context-navigator-razor--early-abort-p (items total-enabled bytes remote-n)
  "Return non-nil when the run should abort; emits messages and logs."
  (cond
   ((= total-enabled 0)
    (ignore-errors (context-navigator-debug :info :razor "Abort: no enabled items"))
    (message "No enabled items in current group")
    t)
   ;; TRAMP confirm only when there are remote items
   ((and context-navigator-razor-remote-include
         (> remote-n 0)
         (not (context-navigator-razor--warn-remote items)))
    (ignore-errors
      (context-navigator-debug :warn :razor
                               "Abort: user declined sending remote items (n=%d)"
                               remote-n))
    (message (context-navigator-i18n :razor-abort-remote) remote-n)
    t)
   ;; Large payload confirm
   ((and (integerp context-navigator-razor-large-bytes-threshold)
         (> bytes context-navigator-razor-large-bytes-threshold)
         (not (context-navigator-razor--warn-large bytes)))
    (ignore-errors
      (context-navigator-debug :warn :razor
                               "Abort: user declined large payload ~%s"
                               (context-navigator-razor--human-size bytes)))
    (message (context-navigator-i18n :razor-abort-large)
             (context-navigator-razor--human-size bytes))
    t)
   (t nil)))

(defun context-navigator-razor--make-apply-callback (total-enabled user)
  "Build the gptel callback that parses and applies keep-set with retry logic."
  (let (apply-fn)
    (let ((retry-done nil))
      (setq apply-fn
            (lambda (raw)
              (let ((parsed (context-navigator-razor--parse-keep raw)))
                (if (and (listp parsed) (plist-get parsed :keep_keys))
                    (let* ((keep (plist-get parsed :keep_keys))
                           (klen (length (or keep '()))))
                      (ignore-errors
                        (context-navigator-debug :debug :razor
                                                 "Parsed keep_keys=%d" klen))
                      ;; confirm empty → preview → apply
                      (if (or (> klen 0) (context-navigator-razor--confirm-empty keep))
                          (let ((ok (context-navigator-razor--preview-keep keep total-enabled)))
                            (if ok
                                (context-navigator-razor--apply-keep keep)
                              (ignore-errors
                                (context-navigator-debug :info :razor "Abort: preview declined"))
                              (message "%s" (context-navigator-i18n :razor-abort-preview))))
                        (ignore-errors
                          (context-navigator-debug :info :razor "Abort: empty keep not confirmed"))
                        (message "%s" (context-navigator-i18n :razor-abort-empty))))
                  ;; parse error
                  (if (and context-navigator-razor-strict-json (not retry-done)
                           (yes-or-no-p (context-navigator-i18n :razor-parse-error)))
                      (progn
                        (setq retry-done t)
                        (ignore-errors
                          (context-navigator-debug :warn :razor "Retrying with strict JSON"))
                        (let* ((sys2 "Reply ONLY with strict JSON. No code fences, no markdown, no commentary.")
                               (user2 (concat user "\nReturn ONLY JSON.")))
                          (context-navigator-razor--call-model sys2 user2 apply-fn)))
                    (ignore-errors
                      (let* ((snippet (substring (or raw "") 0 (min (length (or raw "")) 256))))
                        (context-navigator-debug :error :razor
                                                 "Abort: parse failed; head=%S" snippet)))
                    (message "%s" (context-navigator-i18n :razor-abort-parse)))))))
      apply-fn)))

(defun context-navigator-razor--request (sys user total-enabled)
  "Send the request to the model with SYS/USER and handle the response."
  (let ((apply-fn (context-navigator-razor--make-apply-callback total-enabled user)))
    (context-navigator-razor--call-model sys user apply-fn)))

;;;###autoload
(defun context-navigator-razor-run ()
  "Run Occam filter: only active in org-mode buffers."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (ignore-errors (context-navigator-debug :warn :razor "Not in org-mode; abort"))
    (user-error "Occam filter is available only in org-mode buffers"))
  (let* ((input (context-navigator-razor--collect-run-input))
         (org-text (plist-get input :org-text))
         (items (plist-get input :items))
         (total-enabled (plist-get input :total-enabled))
         (bytes (plist-get input :bytes))
         (remote-n (plist-get input :remote-n)))
    (unless (context-navigator-razor--early-abort-p items total-enabled bytes remote-n)
      (message "%s" (context-navigator-i18n :razor-start))
      (let* ((sys (context-navigator-razor--system-prompt))
             (user (context-navigator-razor--build-user org-text items)))
        (context-navigator-razor--request sys user total-enabled)))))

(provide 'context-navigator-razor)
;;; context-navigator-razor.el ends here
