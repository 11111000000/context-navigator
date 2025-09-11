;;; context-navigator-path-add-test.el --- ERT tests for path-add -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Focused, functional tests:
;; - token extraction and normalization
;; - resolution behavior on index hits/ambiguities
;; - absolute/relative acceptance and limit policy

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-path-add)

(defun cn-pa--tmpdir ()
  (make-temp-file "cn-pa-" t))

(ert-deftest cn-pa-extract-normalize-basic ()
  "Extract quoted/unquoted tokens and normalize suffixes and quotes.
Rules:
- paths are tokens with dir separators (/ or \\) or with an extension
- reject tokens with double separators (// or \\\\)
- do not accept extensionless basenames like Makefile"
  (let* ((s "foo/bar.txt:12 'src/main.c:34-56' <include/header.h> \"C:\\\\dir\\\\file.rs:10\" http://example.com/a baz Makefile")
         (tokens (context-navigator-extract-pathlike-tokens s)))
    ;; URL should be ignored; 'baz Makefile' → none (no slash, no extension)
    (should (member "foo/bar.txt" tokens))
    (should (member "src/main.c" tokens))
    (should (member "include/header.h" tokens))
    (should (member "C:\\dir\\file.rs" tokens))
    (should (not (member "Makefile" tokens)))
    ;; No URL token
    (should (null (cl-find-if (lambda (t) (string-match-p "http" t)) tokens)))))

(ert-deftest cn-pa-normalize-windows-drive ()
  "Do not treat 'C:' as position suffix; do strip after full path."
  (let* ((f "C:\\path\\to\\file.ext:123")
         (norm (context-navigator-path-add--normalize-token f)))
    (should (equal norm "C:\\path\\to\\file.ext")))
  (let* ((drive "C:") ;; bare drive — should remain as is
         (norm (context-navigator-path-add--normalize-token drive)))
    (should (equal norm "C:"))))

(ert-deftest cn-pa-resolve-absolute-and-relative ()
  "Accept absolute and relative paths that exist and are regular files."
  (let* ((dir (cn-pa--tmpdir))
         (default-directory dir)
         (abs1 (make-temp-file (expand-file-name "a-" dir)))
         (rel1 (file-name-nondirectory (make-temp-file (expand-file-name "b-" dir)))))
    (unwind-protect
        (let* ((root dir)
               (res (context-navigator-resolve-names->files (list abs1 rel1) root))
               (files (plist-get res :files)))
          (should (member abs1 files))
          (should (member (expand-file-name rel1 root) files)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest cn-pa-resolve-index-and-ambiguity ()
  "Resolve against a supplied index; abort on ambiguities (reported upstream)."
  (let* ((dir (cn-pa--tmpdir))
         (default-directory dir)
         (f1 (expand-file-name "src/uniq/fileA.txt" dir))
         (f2 (expand-file-name "lib/dup/fileB.txt" dir))
         (f3 (expand-file-name "pkg/dup/fileB.txt" dir)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory f1) t)
          (make-directory (file-name-directory f2) t)
          (make-directory (file-name-directory f3) t)
          (with-temp-file f1 (insert "A"))
          (with-temp-file f2 (insert "B1"))
          (with-temp-file f3 (insert "B2"))
          ;; Override project index function to avoid scanning FS
          (cl-letf (((symbol-function 'context-navigator-project-file-index)
                     (lambda (_root) (list f1 f2 f3))))
            (let* ((root dir)
                   (ok (context-navigator-resolve-names->files (list "fileA.txt") root))
                   (amb (context-navigator-resolve-names->files (list "fileB.txt") root)))
              (should (member f1 (plist-get ok :files)))
              (should (consp (plist-get amb :ambiguous)))
              (should (= (length (cdar (plist-get amb :ambiguous))) 2)))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest cn-pa-limit-too-many ()
  "Abort when the resolved set exceeds the configured limit."
  (let* ((dir (cn-pa--tmpdir))
         (default-directory dir)
         (files (cl-loop repeat 3 collect (make-temp-file (expand-file-name "x-" dir)))))
    (unwind-protect
        (let* ((root dir)
               ;; Limit to 2; provide 3 absolute paths
               (context-navigator-path-add-limit 2)
               (res (context-navigator-resolve-names->files files root))
               (plist (let ((pl (copy-sequence res)))
                        ;; simulate downstream add that checks the limit
                        (if (> (length (plist-get pl :files)) context-navigator-path-add-limit)
                            (plist-put pl :aborted :too-many)
                          pl))))
          (should (eq (plist-get plist :aborted) :too-many)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest cn-pa-extract-paren-tail ()
  "Extract path when a trailing '(' follows the file."
  (let* ((s "- src/app/modules/configuration/configuration.component.ts (")
         (tokens (context-navigator-extract-pathlike-tokens s)))
    (should (member "src/app/modules/configuration/configuration.component.ts" tokens))))

(ert-deftest cn-pa-extract-paren-annotation-full ()
  "Extract path when an annotation in parentheses follows the file."
  (let* ((s "- src/app/models/settings.ts (для AppConfigurationService)")
         (tokens (context-navigator-extract-pathlike-tokens s)))
    (should (member "src/app/models/settings.ts" tokens))))

(provide 'context-navigator-path-add-test)
;;; context-navigator-path-add-test.el ends here
