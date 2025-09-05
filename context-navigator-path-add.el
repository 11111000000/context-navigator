;;; context-navigator-path-add.el --- Add files by names/paths from text/minibuffer -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Minimal functional implementation of the spec:
;; - Extract path-like tokens from arbitrary text
;; - Resolve to project files (with index/TTL cache) or absolute/relative existing paths
;; - Apply filters (size, non-regular, symlink), TRAMP confirmation, count limit
;; - Abort on ambiguities, report unresolved; add resulting files to model and optionally push to gptel
;;
;; Small pure helpers, isolated side effects.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'rx)
(require 'project) ;; project-current / project-files
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-events)
(require 'context-navigator-i18n)

(defgroup context-navigator-path-add nil
  "Settings for adding files from names/paths."
  :group 'context-navigator)

(defcustom context-navigator-path-add-limit 50
  "Maximum number of files to add in a single operation."
  :type 'integer :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-index-cache-ttl 30.0
  "TTL (seconds) for project file index cache."
  :type 'number :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-case-sensitive 'on
  "Case sensitivity policy for basename matching: auto|on|off."
  :type '(choice (const auto) (const on) (const off))
  :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-ignore-gitignored t
  "Prefer sources that respect .gitignore (project.el/git)."
  :type 'boolean :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-exclude-dotdirs t
  "Exclude dot-directories (.*) from fallback recursion."
  :type 'boolean :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-fallback-exclude
  '("node_modules" "dist" "build" "target")
  "Directory names to exclude in fallback recursion."
  :type '(repeat string) :group 'context-navigator-path-add)

;; Reuse max file size if defined by transient module; declare to avoid require.
(defvar context-navigator-max-file-size (* 2 1024 1024)
  "Maximum file size (bytes) to include when adding files (fallback default).")

;; -----------------------------------------------------------------------------
;; Small helpers (pure where possible)

(defun context-navigator-path-add--now () (float-time))

(defun context-navigator-path-add--string-trim-quotes (s)
  "Strip surrounding quotes or angle brackets from S."
  (let* ((len (length s)))
    (cond
     ((and (>= len 2)
           (or (and (eq (aref s 0) ?\") (eq (aref s (1- len)) ?\"))
               (and (eq (aref s 0) ?\') (eq (aref s (1- len)) ?\'))
               (and (eq (aref s 0) ?<)  (eq (aref s (1- len)) ?>))))
      (substring s 1 (1- len)))
     (t s))))

(defun context-navigator-path-add--looks-like-url-p (s)
  "Return non-nil when S looks like a URL with scheme://."
  (string-match-p "^[[:alnum:]+.-]+://." s))

(defun context-navigator-path-add--strip-position-suffix (s)
  "Strip trailing :N or :A-B from S when appropriate (not for drive letter)."
  (let ((m (string-match "\\(:[0-9]+\\(-[0-9]+\\)?\\)\\'" s)))
    (if (not m)
        s
      (let ((prefix (substring s 0 m)))
        ;; Guard: do not treat 'C:' (drive) as position
        (if (string-match-p "\\`[A-Za-z]:\\'" prefix)
            s
          prefix)))))

(defun context-navigator-path-add--normalize-token (raw)
  "Normalize RAW path-like token or return nil when not acceptable."
  (let* ((s (string-trim raw)))
    (when (and (stringp s) (not (string-empty-p s)))
      (let* ((q (context-navigator-path-add--string-trim-quotes s)))
        (cond
         ((context-navigator-path-add--looks-like-url-p q) nil)
         (t
          (let* ((no-pos (context-navigator-path-add--strip-position-suffix q)))
            (string-trim no-pos))))))))

(defun context-navigator-path-add--case-fold-p ()
  "Return non-nil when basename match should be case-insensitive."
  (pcase context-navigator-path-add-case-sensitive
    ('on nil)
    ('off t)
    (_ ;; auto
     (memq system-type '(windows-nt ms-dos cygwin)))))

(defun context-navigator-path-add--basename= (a b)
  "Compare basenames A and B according to case policy."
  (if (context-navigator-path-add--case-fold-p)
      (string-equal (downcase a) (downcase b))
    (string-equal a b)))

(defun context-navigator-path-add--basename (p)
  (file-name-nondirectory p))

(defun context-navigator-path-add--has-dirsep-p (s)
  "Return non-nil when S contains a directory separator (/ or \\)."
  (string-match-p "[/\\]" s))

(defun context-navigator-path-add--has-extension-p (s)
  "Return non-nil when S has a filename extension (basename has a dot suffix)."
  (let ((ext (file-name-extension (file-name-nondirectory s))))
    (and (stringp ext) (not (string-empty-p ext)))))

(defun context-navigator-path-add--regular-file-p (p)
  (and (stringp p) (file-exists-p p) (file-regular-p p)))

(defun context-navigator-path-add--symlink-p (p)
  (condition-case _ (file-symlink-p p) (error nil)))

(defun context-navigator-path-add--file-size (p)
  (when (and (stringp p) (file-exists-p p))
    (let ((a (file-attributes p 'string)))
      (and a (file-attribute-size a)))))

(defun context-navigator-path-add--project-root ()
  "Resolve project root from core state or current buffer."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st))))
    (or root
        (ignore-errors
          (let ((proj (project-current nil)))
            (when proj (expand-file-name (project-root proj)))))
        default-directory)))

;; -----------------------------------------------------------------------------
;; Index cache

(defvar context-navigator-path-add--index-cache (make-hash-table :test 'equal)
  "Hash: root -> (timestamp . files) where files is list of absolute paths.")

(defun context-navigator-path-add--cache-get (root)
  (let ((cell (gethash root context-navigator-path-add--index-cache)))
    (when (and cell
               (numberp (car cell))
               (< (- (context-navigator-path-add--now) (car cell))
                  (max 0.0 context-navigator-path-add-index-cache-ttl)))
      (cdr cell))))

(defun context-navigator-path-add--cache-put (root files)
  (puthash root (cons (context-navigator-path-add--now) files)
           context-navigator-path-add--index-cache)
  files)

(defun context-navigator-path-add--cache-reset (&rest _)
  "Clear the whole index cache."
  (clrhash context-navigator-path-add--index-cache))

;; Reset cache on project/group switches
(context-navigator-events-subscribe :project-switch #'context-navigator-path-add--cache-reset)
(context-navigator-events-subscribe :group-switch-start #'context-navigator-path-add--cache-reset)

;; -----------------------------------------------------------------------------
;; Project file index (pure wrt returned list)

(defun context-navigator-path-add--project-files-project-el (root)
  "Use project.el to list project files, return absolute paths or nil."
  (when (and (fboundp 'project-current) (fboundp 'project-files))
    (let* ((default-directory root)
           (proj (ignore-errors (project-current nil))))
      (when proj
        (let ((lst (ignore-errors (project-files proj))))
          (when (listp lst)
            (mapcar (lambda (p) (expand-file-name p root)) lst)))))))

(defun context-navigator-path-add--project-files-git (root)
  "Use git ls-files to list repository files, return absolute paths or nil."
  (when (and (executable-find "git")
             (file-directory-p (expand-file-name ".git" root)))
    (let* ((default-directory root)
           (buf (generate-new-buffer " *cn-git-ls*"))
           (code (call-process "git" nil buf nil
                               "ls-files" "-co" "--exclude-standard")))
      (unwind-protect
          (when (and (integerp code) (= code 0))
            (with-current-buffer buf
              (goto-char (point-min))
              (let (acc)
                (while (not (eobp))
                  (let ((line (string-trim (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position)))))
                    (when (not (string-empty-p line))
                      (push (expand-file-name line root) acc)))
                  (forward-line 1))
                (nreverse acc))))
        (kill-buffer buf)))))

(defun context-navigator-path-add--fallback-dir-p-allowed (dir)
  "Return non-nil when DIR (basename) is allowed in fallback traversal."
  (let* ((bn (file-name-nondirectory (directory-file-name dir))))
    (and
     (or (not context-navigator-path-add-exclude-dotdirs)
         (not (string-prefix-p "." bn)))
     (not (member bn context-navigator-path-add-fallback-exclude)))))

(defun context-navigator-path-add--project-files-fallback (root)
  "Recursive directory listing (filtered). Return absolute files."
  (let ((res '()))
    (dolist (entry (directory-files root t nil t))
      (let ((bn (file-name-nondirectory entry)))
        (cond
         ((member bn '("." ".." ".git"))) ;; skip
         ((file-directory-p entry)
          (when (context-navigator-path-add--fallback-dir-p-allowed entry)
            (setq res (append res (context-navigator-path-add--project-files-fallback entry)))))
         ((file-regular-p entry)
          (push entry res)))))
    res))

(defun context-navigator-project-file-index (root)
  "Return list of absolute file paths for ROOT, with TTL cache."
  (let* ((abs-root (directory-file-name (expand-file-name root)))
         (remote (file-remote-p abs-root)))
    (or (context-navigator-path-add--cache-get abs-root)
        (context-navigator-path-add--cache-put
         abs-root
         (cond
          ;; avoid heavy scan on TRAMP roots
          (remote '())
          ((context-navigator-path-add--project-files-project-el abs-root))
          ((context-navigator-path-add--project-files-git abs-root))
          (t (context-navigator-path-add--project-files-fallback abs-root)))))))

;; -----------------------------------------------------------------------------
;; Token extraction

(defun context-navigator-path-add--token-acceptable-p (s)
  "Heuristic: accept S only if it really looks like a file path.
Rules:
- reject URLs
- accept absolute paths (POSIX/Windows/UNC)
- tokens with a directory separator are accepted when the last component is non-empty (no need for an extension)
- tokens without a directory separator are accepted when they either have an extension
  or match common extensionless filenames (Makefile, README, LICENSE, Dockerfile, etc.)
Everything else (e.g. trailing slash directories like 'app/', plain words) is rejected."
  (and (stringp s)
       (not (string-empty-p s))
       (not (context-navigator-path-add--looks-like-url-p s))
       (let ((bn (file-name-nondirectory s)))
         (or
          ;; Absolute paths
          (context-navigator-path-add--absolute-p s)
          ;; Relative/with dir separators: accept when basename is non-empty and not . or ..
          (and (context-navigator-path-add--has-dirsep-p s)
               (stringp bn)
               (not (string-empty-p bn))
               (not (member bn '("." ".."))))
          ;; Bare basenames: require extension OR well-known extensionless names
          (and (not (context-navigator-path-add--has-dirsep-p s))
               (or (context-navigator-path-add--has-extension-p s)
                   (let* ((bnu (downcase bn)))
                     (member bnu '("makefile" "readme" "license" "copying"
                                   "dockerfile" "vagrantfile" "gemfile" "rakefile"
                                   "procfile" "brewfile")))))))))

(defun context-navigator-extract-pathlike-tokens (text)
  "Extract path-like tokens from TEXT. Return list of normalized strings."
  (let ((re "\\(?:\"[^\"\n]+\"\\|'[^'\n]+'\\|<[^>\n]+>\\|[[:alnum:]_./\\\\:-]+\\)"))
    (let ((pos 0) (acc '()))
      (while (and (< pos (length text))
                  (string-match re text pos))
        (let* ((m0 (match-string 0 text)))
          (setq pos (match-end 0))
          (when (and (stringp m0) (not (string-empty-p (string-trim m0))))
            (let ((norm (context-navigator-path-add--normalize-token m0)))
              (when (and (stringp norm)
                         (not (string-empty-p norm))
                         (context-navigator-path-add--token-acceptable-p norm))
                (push norm acc))))))
      (nreverse (delete-dups acc)))))

;; -----------------------------------------------------------------------------
;; Resolution algorithm

(defun context-navigator-path-add--absolute-p (s)
  (or (file-name-absolute-p s)
      ;; Windows drive or UNC
      (string-match-p "\\`[A-Za-z]:[\\/]" s)
      (string-match-p "\\`\\\\\\\\[^\\/]+" s)))

(defun context-navigator-path-add--resolve-relative (root s)
  (expand-file-name s (or root default-directory)))

(defun context-navigator-path-add--resolve-candidates (tokens root index)
  "Return plist with resolution results before filtering by size/type.
:accepted list<abs-file>  — direct unique resolutions (abs/rel or unique index matches)
:ambiguous alist (token . matches)
:unresolved list<token>"
  (let* ((basename-map
          (let ((ht (make-hash-table :test (if (context-navigator-path-add--case-fold-p) 'equal 'equal))))
            (dolist (p index)
              (let ((bn (context-navigator-path-add--basename p)))
                (let* ((k (if (context-navigator-path-add--case-fold-p)
                              (downcase bn) bn))
                       (cur (gethash k ht)))
                  (puthash k (cons p cur) ht))))
            ht))
         (casefold (context-navigator-path-add--case-fold-p))
         (acc-accepted '())
         (acc-amb '())
         (acc-unres '()))
    (dolist (tok tokens)
      (cond
       ;; absolute
       ((context-navigator-path-add--absolute-p tok)
        (if (context-navigator-path-add--regular-file-p tok)
            (push (expand-file-name tok) acc-accepted)
          (push tok acc-unres)))
       ;; relative
       ((file-exists-p (context-navigator-path-add--resolve-relative root tok))
        (let ((p (context-navigator-path-add--resolve-relative root tok)))
          (if (context-navigator-path-add--regular-file-p p)
              (push (expand-file-name p) acc-accepted)
            (push tok acc-unres))))
       (t
        ;; search by basename in index
        (let* ((bn (context-navigator-path-add--basename tok)))
          (if (and (not (context-navigator-path-add--has-dirsep-p tok))
                   (not (context-navigator-path-add--has-extension-p tok)))
              ;; Bare name without extension: do not treat as a file
              (push tok acc-unres)
            (let* ((key (if (context-navigator-path-add--case-fold-p) (downcase bn) bn))
                   (hits (copy-sequence (or (gethash key basename-map) '()))))
              (cond
               ((= (length hits) 1)
                (push (car hits) acc-accepted))
               ((> (length hits) 1)
                (push (cons tok (cl-subseq hits 0 (min 10 (length hits)))) acc-amb))
               (t
                ;; No substring fallback — only full valid paths or unique basename matches.
                (push tok acc-unres)))))))))
    (list :accepted (nreverse (delete-dups acc-accepted))
          :ambiguous (nreverse acc-amb)
          :unresolved (nreverse acc-unres))))

(defun context-navigator-path-add--apply-filters (files)
  "Apply size/type/remote filters. Return plist:
:files L :skipped-too-big N :skipped-nonregular N :remote N"
  (let* ((limit (or context-navigator-max-file-size most-positive-fixnum))
         (kept '())
         (big 0) (nonreg 0) (remote 0))
    (dolist (f files)
      (cond
       ((or (not (stringp f))
            (not (file-exists-p f))
            (not (file-regular-p f))
            (context-navigator-path-add--symlink-p f))
        (setq nonreg (1+ nonreg)))
       (t
        (when (file-remote-p f) (setq remote (1+ remote)))
        (let ((sz (context-navigator-path-add--file-size f)))
          (if (and sz (> sz limit))
              (setq big (1+ big))
            (push f kept))))))
    (list :files (nreverse (delete-dups kept))
          :skipped-too-big big
          :skipped-nonregular nonreg
          :remote remote)))

(defun context-navigator-resolve-names->files (tokens root &rest _opts)
  "High-level resolution pipeline. Return plist:
:files L :ambiguous A :unresolved L :skipped-too-big N :skipped-nonregular N :remote N"
  (let* ((remote-root (and (stringp root) (file-remote-p root)))
         (index (if remote-root '() (context-navigator-project-file-index root)))
         (res (context-navigator-path-add--resolve-candidates tokens root index))
         (flt (context-navigator-path-add--apply-filters (plist-get res :accepted))))
    (list :files (plist-get flt :files)
          :ambiguous (plist-get res :ambiguous)
          :unresolved (plist-get res :unresolved)
          :skipped-too-big (plist-get flt :skipped-too-big)
          :skipped-nonregular (plist-get flt :skipped-nonregular)
          :remote (plist-get flt :remote))))

;; -----------------------------------------------------------------------------
;; Add to model + UX

(defun context-navigator-path-add--maybe-apply-to-gptel ()
  (when (and (boundp 'context-navigator--push-to-gptel)
             context-navigator--push-to-gptel)
    (let* ((st (context-navigator--state-get))
           (items (and st (context-navigator-state-items st))))
      (ignore-errors (context-navigator-gptel-apply (or items '()))))))

(cl-defun context-navigator-add-files-from-names (tokens &optional _interactive)
  "Resolve TOKENS relative to current root and add resulting files to model.
Handles ambiguities/unresolved/limits/remote confirmation. Returns plist result."
  (let* ((root (context-navigator-path-add--project-root)))
    (message "%s" (context-navigator-i18n :resolve-start))
    (let* ((res (context-navigator-resolve-names->files tokens root))
           (files (plist-get res :files))
           (amb (plist-get res :ambiguous))
           (unr (plist-get res :unresolved))
           (too-many (> (length files) (or context-navigator-path-add-limit 50)))
           (aborted nil)
           (result nil))
      (when (and (consp amb) (> (length amb) 0))
        (let* ((sample (mapconcat
                        (lambda (cell)
                          (format "%s → %d" (car cell) (length (cdr cell))))
                        (cl-subseq amb 0 (min 10 (length amb)))
                        ", ")))
          (message "%s %s" (context-navigator-i18n :ambiguous-found) sample))
        (setq result (plist-put (copy-sequence res) :aborted :ambiguous))
        (setq aborted t))
      (when (and (not aborted) (consp unr) (> (length unr) 0))
        (let ((sample (string-join (cl-subseq unr 0 (min 10 (length unr))) ", ")))
          (message "%s %s" (context-navigator-i18n :unresolved-found) sample)))
      (when (and (not aborted) too-many)
        (message (context-navigator-i18n :too-many) (length files) (or context-navigator-path-add-limit 50))
        (setq result (plist-put (copy-sequence res) :aborted :too-many))
        (setq aborted t))
      (when (and (not aborted)
                 (> (plist-get res :remote) 0)
                 (not (yes-or-no-p (format (context-navigator-i18n :remote-warning) (plist-get res :remote)))))
        (message "%s" (context-navigator-i18n :aborted))
        (setq result (plist-put (copy-sequence res) :aborted :remote))
        (setq aborted t))
      (if aborted
          result
        ;; Add files as items (side-effect isolated here)
        (let ((added 0))
          (dolist (p files)
            (when (and (stringp p) (file-exists-p p) (file-regular-p p))
              (let ((it (context-navigator-item-create
                         :type 'file
                         :name (file-name-nondirectory p)
                         :path (expand-file-name p)
                         :enabled t)))
                (ignore-errors (context-navigator-add-item it))
                (setq added (1+ added)))))
          (context-navigator-path-add--maybe-apply-to-gptel)
          (message (context-navigator-i18n :added-files) added)
          (plist-put (copy-sequence res) :added added))))))

;; -----------------------------------------------------------------------------
;; Interactive commands

;;;###autoload
(defun context-navigator-add-from-text ()
  "Extract path-like tokens from region or buffer and add resolved files."
  (interactive)
  (let* ((src (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max))))
         (tokens (context-navigator-extract-pathlike-tokens (or src ""))))
    (if (null tokens)
        (message "%s" (context-navigator-i18n :unresolved-found))
      (context-navigator-add-files-from-names tokens t))))

;;;###autoload
(defun context-navigator-add-from-minibuffer ()
  "Read names/paths from minibuffer (multi-line allowed) and add resolved files."
  (interactive)
  (let* ((prompt (format "%s: " (context-navigator-i18n :add-from-minibuf)))
         (input (read-from-minibuffer prompt nil nil nil nil nil t))
         (tokens (context-navigator-extract-pathlike-tokens (or input ""))))
    (if (null tokens)
        (message "%s" (context-navigator-i18n :unresolved-found))
      (context-navigator-add-files-from-names tokens t))))

(provide 'context-navigator-path-add)
;;; context-navigator-path-add.el ends here
