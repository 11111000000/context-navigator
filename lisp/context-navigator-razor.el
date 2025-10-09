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
;; - Maintains per-group history (Undo/Redo)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'json)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-log)
(require 'context-navigator-i18n)
(require 'context-navigator-util)
(require 'context-navigator-ui)


(defgroup context-navigator-razor nil
  "Occam (AI) context filter for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-razor-model "deepseek-chat"
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

(defcustom context-navigator-razor-strict-json nil
  "Deprecated. Use `context-navigator-razor-parse-mode' instead."
  :type 'boolean :group 'context-navigator-razor)
(put 'context-navigator-razor-strict-json 'obsolete-variable
     "Use `context-navigator-razor-parse-mode' set to 'json-only.")

(defcustom context-navigator-razor-parse-mode 'flex
  "Response parse mode:
- flex      : try JSON first; if absent, parse plain text identifiers (keys/paths/names)
- json-only : require strict JSON with one retry on parse error"
  :type '(choice (const flex) (const json-only))
  :group 'context-navigator-razor)

(defcustom context-navigator-razor-flex-allow-fuzzy nil
  "Enable cautious fuzzy matching in flex mode when no exact match exists and a single candidate is clear."
  :type 'boolean :group 'context-navigator-razor)

(defcustom context-navigator-razor-flex-accept-patterns
  (list
   ;; stable keys
   "^[[:lower:]]\\{3,\\}:[^[:space:]]\\{1,\\}$"
   ;; absolute paths (POSIX, Windows drive, TRAMP prefixes)
   "^\\(/\\|~\\|[A-Za-z]:[/\\\\]\\|/ssh:\\|/scp:\\|/smb:\\).+"
   ;; selection signature (path:beg-end) — avoid bare drive letter C:
   ".+[\\\\/].+:[0-9]+-[0-9]+$"
   ;; filenames with extension
   "^[^/\\\\[:space:]]+\\.[A-Za-z0-9._-]+$"
   )
  "Regex patterns (strings) to accept tokens in flex mode."
  :type '(repeat string) :group 'context-navigator-razor)

(defcustom context-navigator-razor-ambiguous-policy 'skip
  "What to do with ambiguous tokens (multiple matches):
- skip  : drop token
- first : take the first match (unsafe)"
  :type '(choice (const skip) (const first))
  :group 'context-navigator-razor)

(defvar context-navigator-razor--debug-resolve nil
  "When non-nil, `context-navigator-razor--resolve-token' returns (KEY . REASON) for logging.")

;; UI status for spinner
(defvar context-navigator-razor--running nil
  "Non-nil while Occam (razor) analysis is running.")

(defvar context-navigator-razor--spinner-index 0
  "Current spinner frame index for the Occam indicator.")

(defvar context-navigator-razor--spinner-timer nil
  "Timer used to animate the Occam spinner.")

(defcustom context-navigator-razor-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames used for the Occam spinner."
  :type '(repeat string)
  :group 'context-navigator-razor)

(defun context-navigator-razor-spinner-frame ()
  "Return current visual spinner frame string."
  (let* ((frames (or context-navigator-razor-spinner-frames
                     '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")))
         (len (max 1 (length frames))))
    (nth (mod (or context-navigator-razor--spinner-index 0) len) frames)))

(defun context-navigator-razor--schedule-controls-refresh ()
  "Request immediate controls refresh for the Navigator buffer (no debounce)."
  (let ((buf (get-buffer "*context-navigator*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Сброс кешей рендера, чтобы следующая перерисовка точно прошла
        (if (fboundp 'context-navigator-view--invalidate-render-caches)
            (context-navigator-view--invalidate-render-caches)
          (progn
            (setq-local context-navigator-render--last-hash nil)
            (setq-local context-navigator-view--last-render-key nil)))
        ;; Форсируем переоценку controls (:eval) даже если тело буфера не менялось
        (force-mode-line-update t)
        (when (fboundp 'context-navigator-view--render-if-visible)
          (context-navigator-view--render-if-visible))))))

(defun context-navigator-razor--notify-start ()
  "Mark razor run as started and start spinner animation."
  (setq context-navigator-razor--running t
        context-navigator-razor--spinner-index 0)
  (when (timerp context-navigator-razor--spinner-timer)
    (cancel-timer context-navigator-razor--spinner-timer))
  (setq context-navigator-razor--spinner-timer
        (run-at-time 0 0.12
                     (lambda ()
                       (setq context-navigator-razor--spinner-index
                             (1+ (or context-navigator-razor--spinner-index 0)))
                       (context-navigator-razor--schedule-controls-refresh)))))
(defun context-navigator-razor--notify-stop ()
  "Mark razor run as finished and stop spinner animation."
  (setq context-navigator-razor--running nil)
  (when (timerp context-navigator-razor--spinner-timer)
    (cancel-timer context-navigator-razor--spinner-timer))
  (setq context-navigator-razor--spinner-timer nil)
  (context-navigator-razor--schedule-controls-refresh))

(defun context-navigator-razor--absolute-p (s)
  "Return non-nil if S looks like an absolute path (POSIX, Windows drive, or TRAMP)."
  (and (stringp s)
       (or (file-name-absolute-p s)
           (string-match-p "\\`[A-Za-z]:[\\\\/]" s)
           (string-match-p "\\`/\\(ssh\\|scp\\|smb\\):" s))))

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
    (context-navigator-ui-info :history-not-available)))

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
    (context-navigator-ui-info :nothing-to-undo)))

;;;###autoload
(defun context-navigator-razor-redo ()
  "Compatibility shim: delegate to core redo."
  (interactive)
  (if (fboundp 'context-navigator-redo)
      (context-navigator-redo)
    (context-navigator-ui-info :nothing-to-redo)))



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
      (context-navigator-ui-ask :razor-remote-warn n))))

(defun context-navigator-razor--warn-large (bytes)
  (when (and (integerp context-navigator-razor-large-bytes-threshold)
             (> bytes context-navigator-razor-large-bytes-threshold))
    (context-navigator-ui-ask :razor-large-warn
                              (context-navigator-human-size bytes))))

(defun context-navigator-razor--system-prompt ()
  "Return cautious system prompt."
  "You are a cautious context selector. Choose the minimal but safe subset of items strictly necessary to solve the task described in the provided org document. When uncertain, prefer keeping an item rather than excluding it.")

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
    (push
     (if (eq context-navigator-razor-parse-mode 'json-only)
         "Instruction: Return JSON ONLY of the form {\"keep_keys\": [\"<item-key>\", ...], \"rationale\": \"<optional short>\"}. If in doubt, include the item.\n"
       "Instruction: List the items to KEEP, one per line. Prefer stable item KEYS (file:/buf:/sel:). If a key is unknown, write an absolute or project-relative path, or a unique filename or buffer name. No commentary. If in doubt, include the item.\n")
     sb)
    (apply #'concat (nreverse sb))))

(defun context-navigator-razor--extract-json (s)
  "Try to extract a JSON object from S and parse it. Logs detection and errors."
  (let* ((str (or s ""))
         (start (string-match "{" str)))
    (when start
      (let ((end nil) (lvl 0) (i start) (n (length str))))
      (while (and (< i n) (null end))
        (let ((ch (aref str i)))
          (cond
           ((= ch ?{) (setq lvl (1+ lvl)))
           ((= ch ?}) (setq lvl (1- lvl))
            (when (= lvl 0) (setq end i))))
          (setq i (1+ i))))
      (when (and end (>= end start))
        (let* ((json-str (substring str start (1+ end))))
          (ignore-errors
            (context-navigator-debug :trace :razor
                                     "extract-json: span=%d..%d len=%d"
                                     start end (length json-str)))
          (condition-case err
              (json-parse-string json-str
                                 :object-type 'plist :array-type 'list :null-object nil)
            (error
             (ignore-errors
               (context-navigator-debug :warn :razor
                                        "extract-json: parse error: %S" err))
             nil)))))))

(defun context-navigator-razor--parse-keep (response)
  "Parse RESPONSE text to plist {:keep_keys [...]} with tolerant extraction. Logs stats."
  (condition-case err
      (let ((jo (context-navigator-razor--extract-json response)))
        (when (plist-get jo :keep_keys)
          (let* ((ks (plist-get jo :keep_keys))
                 (cnt (and (listp ks) (length ks))))
            (ignore-errors
              (context-navigator-debug :debug :razor
                                       "json-only: keep_keys=%s rationale?=%s"
                                       (or cnt 0)
                                       (if (plist-member jo :rationale) "yes" "no")))
            (list :keep_keys ks :rationale (plist-get jo :rationale)))))
    (error
     (ignore-errors
       (context-navigator-debug :warn :razor "parse-keep: exception %S" err))
     nil)))

;; -------- Flex parsing helpers (tokens → keys) ------------------------------

(defun context-navigator-razor--strip-fences (s)
  "Remove common code fences/bullets from S (keep inner content)."
  (let* ((s (replace-regexp-in-string "^[ \t]*=[[:alpha:]]*\\s-*$" "" s))
         (s (replace-regexp-in-string "^[ \t]*=\\s-*$" "" s))
         (s (replace-regexp-in-string "^[ \t]*[-*+]\\s+" "" s))
         (s (replace-regexp-in-string "^[ \t]*[0-9]+[.)]\\s+" "" s)))
    s))

(defun context-navigator-razor--split-tokens (s)
  "Split S into candidate tokens with a tolerant heuristic.
- Split by newlines/commas and also by whitespace inside phrases
- Strip surrounding quotes/backticks/angle brackets for each token
- Drop prefixes like \"Key:\" / \"path:\" (but not file:/buf:/sel:)
- Trim trailing punctuation that is unlikely to be part of a name"
  (let* ((s (context-navigator-razor--strip-fences (or s "")))
         (chunks (split-string s "[\n,]" t))
         ;; Further split each chunk by whitespace to find inline identifiers like `file` foo.el
         (raw (apply #'append (mapcar (lambda (ln) (split-string ln "[ \t]+" t)) chunks))))
    (delq nil
          (mapcar
           (lambda (tkn)
             (let* ((tkn (string-trim tkn))
                    ;; strip surrounding quotes/backticks/angle brackets
                    (len (length tkn))
                    (c0  (and (>= len 1) (aref tkn 0)))
                    (c1  (and (>= len 1) (aref tkn (1- len))))
                    (tkn (if (and (>= len 2)
                                  (or (and (eq c0 ?\") (eq c1 ?\"))
                                      (and (eq c0 ?\') (eq c1 ?\'))
                                      (and (eq c0 ?`)  (eq c1 ?`))
                                      (and (eq c0 ?<)  (eq c1 ?>))))
                             (substring tkn 1 (1- len))
                           tkn))
                    ;; drop prefixes like "Key:" or "path:" (but not file:/buf:/sel:)
                    (tkn (replace-regexp-in-string
                          "\\`\\(keys?\\|paths?\\)\\s-*:\\s*" "" tkn t))
                    ;; trim trailing punctuation (keep Windows drive C: intact)
                    (tkn (replace-regexp-in-string "[,.;:)\\]]\\'" "" tkn))
                    (tkn (string-trim tkn)))
               (and (stringp tkn) (not (string-empty-p tkn)) tkn)))
           raw))))

(defun context-navigator-razor--token-acceptable-p (token)
  "Return non-nil when TOKEN matches any of accept patterns."
  (cl-some (lambda (re) (string-match-p re token))
           context-navigator-razor-flex-accept-patterns))

(defun context-navigator-razor--project-root ()
  (let* ((st (ignore-errors (context-navigator--state-get))))
    (and st (context-navigator-state-last-project-root st))))

(defun context-navigator-razor--build-lookup (items)
  "Build lookup maps from ITEMS. Return plist."
  (let* ((root (context-navigator-razor--project-root))
         (keys (make-hash-table :test 'equal))
         (abs (make-hash-table :test 'equal))
         (rel (make-hash-table :test 'equal))
         (base (make-hash-table :test 'equal))
         (bufn (make-hash-table :test 'equal))
         (sels (make-hash-table :test 'equal)))
    (dolist (it items)
      (let* ((key (context-navigator-model-item-key it))
             (p (context-navigator-item-path it))
             (nm (context-navigator-item-name it)))
        (when key (puthash key it keys))
        (when (and (stringp p) (not (string-empty-p p)))
          (puthash (expand-file-name p) it abs)
          (when (and (stringp root) (not (string-empty-p root)))
            (puthash (file-relative-name (expand-file-name p) (file-name-as-directory root)) it rel))
          (puthash (file-name-nondirectory p)
                   (cons it (gethash (file-name-nondirectory p) base)) base))
        (when (and (eq (context-navigator-item-type it) 'buffer) (stringp nm))
          (puthash nm (cons it (gethash nm bufn)) bufn))
        (when (eq (context-navigator-item-type it) 'selection)
          (let ((sig (format "%s:%s-%s"
                             (or p "") (or (context-navigator-item-beg it) 0) (or (context-navigator-item-end it) 0))))
            (puthash sig it sels)))))
    (list :keys keys :abs abs :rel rel :base base :buf bufn :sel sels :root root)))

(defun context-navigator-razor--resolve-token (token lookup)
  "Resolve TOKEN to a stable key using LOOKUP maps. When debug flag set, return (KEY . REASON)."
  (let* ((keys (plist-get lookup :keys))
         (abs  (plist-get lookup :abs))
         (rel  (plist-get lookup :rel))
         (base (plist-get lookup :base))
         (bufn (plist-get lookup :buf))
         (sels (plist-get lookup :sel))
         (root (plist-get lookup :root))
         (reason nil)
         (key nil))
    (cond
     ;; Stable key
     ((gethash token keys)
      (setq key (context-navigator-model-item-key (gethash token keys)))
      (setq reason "stable-key"))
     ;; Selection signature
     ((gethash token sels)
      (setq key (context-navigator-model-item-key (gethash token sels)))
      (setq reason "selection-sig"))
     ;; Absolute/relative path or names
     (t
      (let* ((tok token)
             (abs-path (cond
                        ((context-navigator-razor--absolute-p tok) (expand-file-name tok))
                        ((and (stringp root) (file-name-absolute-p (expand-file-name tok root)))
                         (expand-file-name tok root))
                        (t nil))))
        (cond
         ((and abs-path (gethash abs-path abs))
          (setq key (context-navigator-model-item-key (gethash abs-path abs)))
          (setq reason "abs-path"))
         ;; Relative path
         ((and (stringp root)
               (gethash (file-relative-name (expand-file-name token root) (file-name-as-directory root)) rel))
          (setq key (context-navigator-model-item-key
                     (gethash (file-relative-name (expand-file-name token root) (file-name-as-directory root)) rel)))
          (setq reason "rel-path"))
         ;; Unique basename
         ((let* ((lst (gethash (file-name-nondirectory token) base)))
            (and (listp lst) (= (length lst) 1)))
          (let ((it (car (gethash (file-name-nondirectory token) base))))
            (setq key (context-navigator-model-item-key it))
            (setq reason "basename-unique")))
         ;; Unique buffer name
         ((let* ((lst (gethash token bufn)))
            (and (listp lst) (= (length lst) 1)))
          (let ((it (car (gethash token bufn))))
            (setq key (context-navigator-model-item-key it))
            (setq reason "buffer-name-unique")))
         ;; Fuzzy (optional)
         ((and context-navigator-razor-flex-allow-fuzzy (fboundp 'string-distance))
          (let* ((candidates (append
                              (and base (gethash (file-name-nondirectory token) base))
                              (and bufn (gethash token bufn))))
                 (best nil) (bestd 999) (bestk nil))
            (dolist (it candidates)
              (let* ((nm (or (and (eq (context-navigator-item-type it) 'buffer)
                                  (context-navigator-item-name it))
                             (and (stringp (context-navigator-item-path it))
                                  (file-name-nondirectory (context-navigator-item-path it))))))
                (when (stringp nm)
                  (let ((d (ignore-errors (string-distance (downcase token) (downcase nm)))))
                    (when (and (numberp d) (< d bestd))
                      (setq bestd d best it bestk (context-navigator-model-item-key it)))))))
            (when (and bestk (<= bestd 3))
              (setq key bestk reason (format "fuzzy<=3(%d)" bestd)))))
         (t
          (setq key nil reason nil))))))
    (if context-navigator-razor--debug-resolve
        (cons key reason)
      key)))

(defun context-navigator-razor--parse-text-keep (response items)
  "Parse RESPONSE as plain text identifiers and resolve against ITEMS. Logs tokens and mapping."
  (let* ((raw-tokens (context-navigator-razor--split-tokens response))
         (tokens (seq-filter #'context-navigator-razor--token-acceptable-p raw-tokens))
         (lookup (context-navigator-razor--build-lookup items))
         (keep '())
         (unk 0) (matched 0) (amb 0)
         (mappings '()))
    (ignore-errors
      (context-navigator-debug :debug :razor
                               "flex: tokens raw=%d accepted=%d"
                               (length raw-tokens) (length tokens)))
    (let ((context-navigator-razor--debug-resolve t)
          (base (plist-get lookup :base))
          (bufn (plist-get lookup :buf)))
      (dolist (tkn tokens)
        (let* ((res (context-navigator-razor--resolve-token tkn lookup))
               (k   (and (consp res) (car res)))
               (why (and (consp res) (cdr res))))
          (cond
           ;; New unique match
           ((and (stringp k) (not (member k keep)))
            (push k keep)
            (setq matched (1+ matched))
            (push (list :token tkn :key k :why (or why "resolved")) mappings))
           ;; Unresolved: try to detect ambiguity by basename/buffer-name multiplicity
           ((null k)
            (let* ((base-lst (and base (gethash (file-name-nondirectory tkn) base)))
                   (buf-lst  (and bufn (gethash tkn bufn))))
              (cond
               ((and (listp base-lst) (> (length base-lst) 1))
                (setq amb (1+ amb))
                (push (list :token tkn :key nil :why "ambiguous-basename") mappings))
               ((and (listp buf-lst) (> (length buf-lst) 1))
                (setq amb (1+ amb))
                (push (list :token tkn :key nil :why "ambiguous-buffer") mappings))
               (t
                (setq unk (1+ unk))
                (push (list :token tkn :key nil :why "unresolved") mappings)))))
           ;; Duplicate match — уже в keep; логируем дублирование
           (t
            (push (list :token tkn :key (if (stringp k) k "<nil>") :why "duplicate") mappings))))))
    ;; Ограниченный предпросмотр сопоставлений в лог
    (ignore-errors
      (let* ((preview (cl-subseq (nreverse mappings) 0 (min 20 (length mappings))))
             (lines (mapcar (lambda (pl)
                              (format "  %s -> %s (%s)"
                                      (plist-get pl :token)
                                      (or (plist-get pl :key) "<nil>")
                                      (or (plist-get pl :why) "<nil>")))
                            preview)))
        (when (> (length lines) 0)
          (context-navigator-debug :trace :razor
                                   "flex: mapping preview:\n%s"
                                   (mapconcat #'identity lines "\n")))))
    (list :keep_keys (nreverse keep)
          :matched matched :unknown unk :ambiguous amb :total (length tokens))))

(defun context-navigator-razor--parse-response (response items)
  "Parse RESPONSE according to `context-navigator-razor-parse-mode'."
  (pcase context-navigator-razor-parse-mode
    ('json-only
     (context-navigator-razor--parse-keep response))
    (_
     ;; flex: try JSON first, then text
     (let ((j (context-navigator-razor--parse-keep response)))
       (if (plist-get j :keep_keys)
           (progn
             (ignore-errors (context-navigator-debug :debug :razor "flex: JSON detected in response; using it"))
             j)
         (let ((tres (context-navigator-razor--parse-text-keep response items)))
           (ignore-errors
             (context-navigator-debug :debug :razor
                                      "flex-parse stats: matched=%d unknown=%d ambiguous=%d total=%d"
                                      (plist-get tres :matched)
                                      (plist-get tres :unknown)
                                      (plist-get tres :ambiguous)
                                      (plist-get tres :total)))
           tres))))))

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
          (context-navigator-ui-info :razor-done kept total))))))

(defun context-navigator-razor--confirm-empty (keep)
  (when (or (null keep) (= (length keep) 0))
    (context-navigator-ui-ask :razor-empty-confirm)))

(defun context-navigator-razor--preview-keep (keep total)
  (if (not context-navigator-razor-preview)
      t
    (context-navigator-ui-ask :razor-preview-title
                              (length (or keep '()))
                              total)))

(defun context-navigator-razor--gptel-opts (sys cb)
  "Return minimal options plist for gptel. The actual wrapping is done in `context-navigator-razor--gptel-call'."
  (list :system sys :callback cb))

(defun context-navigator-razor--gptel-call (sys user cb)
  "Call gptel with SYS, USER and 1-arg CB (response-only). Return request object or nil."
  (unless (require 'gptel nil t)
    (context-navigator-ui-error :gptel-not-available))
  (let* ((model (if (symbolp context-navigator-razor-model)
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
      (gptel-request user
        :system sys
        :callback (lambda (response info)
                    (ignore info)
                    (let ((len (length (or response ""))))
                      (ignore-errors
                        (context-navigator-debug :debug :razor
                                                 "gptel callback: response length=%d" len)))
                    (funcall cb (or response "")))))))

(defun context-navigator-razor--call-model (sys user cb)
  "Call gptel with a wrapped 1-arg callback."
  (context-navigator-razor--gptel-call sys user cb))

(defun context-navigator-razor--collect-run-input ()
  "Collect input for the razor run and log initial metrics.
Return plist:
  (:org-text :items-pl :model-items :total-enabled :bytes :remote-n)."
  (let* ((org-pair (context-navigator-razor--org-source))
         (org-text (car org-pair))
         ;; Payload items (plists) with content for the prompt
         (items-pl (context-navigator-razor--collect-enabled-items))
         ;; Model items (context-navigator-item) currently enabled, for resolver
         (st (ignore-errors (context-navigator--state-get)))
         (all (and st (context-navigator-state-items st)))
         (model-items (and (listp all) (cl-remove-if-not #'context-navigator-item-enabled all)))
         (total-enabled (length items-pl))
         (bytes (context-navigator-razor--payload-bytes org-text items-pl))
         (remote-n (cl-count-if (lambda (pl) (plist-get pl :remote)) items-pl)))
    (ignore-errors
      (context-navigator-debug :info :razor
                               "Start: enabled=%d, payload=%d bytes (~%d tok), remotes=%d"
                               total-enabled bytes
                               (floor (/ bytes 4.0))
                               remote-n))
    (list :org-text org-text
          :items-pl items-pl
          :model-items model-items
          :total-enabled total-enabled
          :bytes bytes
          :remote-n remote-n)))

(defun context-navigator-razor--early-abort-p (items total-enabled bytes remote-n)
  "Return non-nil when the run should abort; emits messages and logs."
  (cond
   ((= total-enabled 0)
    (ignore-errors (context-navigator-debug :info :razor "Abort: no enabled items"))
    (context-navigator-ui-info :razor-no-enabled-items)
    t)
   ;; TRAMP confirm only when there are remote items
   ((and context-navigator-razor-remote-include
         (> remote-n 0)
         (not (context-navigator-razor--warn-remote items)))
    (ignore-errors
      (context-navigator-debug :warn :razor
                               "Abort: user declined sending remote items (n=%d)"
                               remote-n))
    (context-navigator-ui-warn :razor-abort-remote remote-n)
    t)
   ;; Large payload confirm
   ((and (integerp context-navigator-razor-large-bytes-threshold)
         (> bytes context-navigator-razor-large-bytes-threshold)
         (not (context-navigator-razor--warn-large bytes)))
    (ignore-errors
      (context-navigator-debug :warn :razor
                               "Abort: user declined large payload ~%s"
                               (context-navigator-human-size bytes)))
    (context-navigator-ui-warn :razor-abort-large
                               (context-navigator-human-size bytes))
    t)
   (t nil)))

(defun context-navigator-razor--maybe-apply-keep (keep total-enabled)
  "Confirm and apply KEEP set. Returns non-nil if applied, nil if aborted."
  (let* ((klen (length (or keep '()))))
    (ignore-errors
      (context-navigator-debug :debug :razor "Parsed keep_keys=%d" klen))
    (if (or (> klen 0) (context-navigator-razor--confirm-empty keep))
        (let ((ok (context-navigator-razor--preview-keep keep total-enabled)))
          (if ok
              (progn
                (context-navigator-razor--apply-keep keep)
                t)
            (ignore-errors
              (context-navigator-debug :info :razor "Abort: preview declined"))
            (context-navigator-ui-warn :razor-abort-preview)
            nil))
      (ignore-errors
        (context-navigator-debug :info :razor "Abort: empty keep not confirmed"))
      (context-navigator-ui-warn :razor-abort-empty)
      nil)))

(defun context-navigator-razor--retry-json-strict (user cb raw)
  "Retry with strict JSON instruction. Returns t if a new request was sent."
  (let* ((sys2 "Reply ONLY with strict JSON. No code fences, no markdown, no commentary.")
         (user2 (concat user "\nReturn ONLY JSON.")))
    (ignore-errors
      (context-navigator-debug :warn :razor "Retrying with strict JSON"))
    (context-navigator-razor--call-model sys2 user2 cb)
    t))

(defun context-navigator-razor--retry-flex-list (user cb)
  "Retry with newline-separated list-of-keys instruction. Returns t if sent."
  (let* ((sys2 "Return ONLY a newline-separated list of item KEYS (file:/buf:/sel:). No commentary.")
         (user2 (concat user "\nReturn ONLY a newline-separated list of item KEYS.")))
    (ignore-errors
      (context-navigator-debug :warn :razor "Retrying with newline-separated KEYS"))
    (context-navigator-razor--call-model sys2 user2 cb)
    t))

(defun context-navigator-razor--show-flex-stats (parsed)
  "Show flex-parse stats when available."
  (let* ((matched (and (listp parsed) (plist-get parsed :matched)))
         (unknown (and (listp parsed) (plist-get parsed :unknown)))
         (ambig   (and (listp parsed) (plist-get parsed :ambiguous)))
         (total   (and (listp parsed) (plist-get parsed :total))))
    (when (and total matched ambig unknown)
      (context-navigator-ui-info :razor-parse-flex-stats
                                 matched total ambig unknown))))

(defun context-navigator-razor--make-apply-callback (total-enabled user model-items)
  "Build the gptel callback that parses and applies keep-set with retry logic.
MODEL-ITEMS must be a list of `context-navigator-item' (enabled) for resolver."
  (let ((retry-done nil))
    (cl-labels
        ((cb (raw)
           (condition-case err
               (let ((parsed (context-navigator-razor--parse-response raw model-items)))
                 (if (and (listp parsed) (plist-get parsed :keep_keys))
                     (progn
                       (let ((keep (plist-get parsed :keep_keys)))
                         (context-navigator-razor--maybe-apply-keep keep total-enabled))
                       (context-navigator-razor--notify-stop))
                   (if (eq context-navigator-razor-parse-mode 'json-only)
                       (if (and (not retry-done)
                                (context-navigator-ui-ask :razor-parse-error))
                           (progn
                             (setq retry-done t)
                             (context-navigator-razor--retry-json-strict user #'cb raw))
                         (let* ((snippet (substring (or raw "") 0 (min (length (or raw "")) 256))))
                           (ignore-errors
                             (context-navigator-debug :error :razor
                                                      "Abort: parse failed; head=%S" snippet)))
                         (context-navigator-ui-warn :razor-abort-parse)
                         (context-navigator-razor--notify-stop))
                     (progn
                       (context-navigator-razor--show-flex-stats parsed)
                       (if (and (not retry-done)
                                (context-navigator-ui-ask :razor-retry))
                           (progn
                             (setq retry-done t)
                             (context-navigator-razor--retry-flex-list user #'cb))
                         (let* ((snippet (substring (or raw "") 0 (min (length (or raw "")) 256))))
                           (ignore-errors
                             (context-navigator-debug :error :razor
                                                      "Abort: flex-parse failed; head=%S" snippet)))
                         (context-navigator-ui-info :aborted)
                         (context-navigator-razor--notify-stop))))))
             (error
              (ignore-errors
                (context-navigator-debug :error :razor "Callback error: %S" err))
              (context-navigator-razor--notify-stop)
              (context-navigator-ui-warn :razor-error err)))))
      #'cb)))

(defun context-navigator-razor--request (sys user total-enabled model-items)
  "Send the request to the model with SYS/USER and handle the response."
  (let ((apply-fn (context-navigator-razor--make-apply-callback total-enabled user model-items)))
    (context-navigator-razor--call-model sys user apply-fn)))

;;;###autoload
(defun context-navigator-razor-run ()
  "Run Occam filter: only active in org-mode buffers."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (ignore-errors (context-navigator-debug :warn :razor "Not in org-mode; abort"))
    (context-navigator-ui-error :razor-only-org-mode))
  (let* ((input (context-navigator-razor--collect-run-input))
         (org-text (plist-get input :org-text))
         (items-pl (plist-get input :items-pl))
         (model-items (plist-get input :model-items))
         (total-enabled (plist-get input :total-enabled))
         (bytes (plist-get input :bytes))
         (remote-n (plist-get input :remote-n)))
    (unless (context-navigator-razor--early-abort-p items-pl total-enabled bytes remote-n)
      (context-navigator-ui-info :razor-start)
      (ignore-errors (context-navigator-debug :info :razor
                                              "parse-mode=%s fuzzy=%s"
                                              context-navigator-razor-parse-mode
                                              context-navigator-razor-flex-allow-fuzzy))
      (let* ((sys (context-navigator-razor--system-prompt))
             (user (context-navigator-razor--build-user org-text items-pl)))
        (context-navigator-razor--notify-start)
        (context-navigator-razor--request sys user total-enabled model-items)))))

(provide 'context-navigator-razor)
;;; context-navigator-razor.el ends here
