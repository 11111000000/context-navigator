;;; context-navigator-mask-tests.el --- ERT tests for mask/glob support -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

(require 'context-navigator-add-paths)

(defun cn--with-temp-dir (fn)
  "Create a temporary directory, call FN with it, then delete recursively."
  (let* ((dir (make-temp-file "cn-mask-" t)))
    (unwind-protect
        (funcall fn dir)
      (ignore-errors (delete-directory dir t)))))

(defun cn--touch (file &optional content)
  "Create FILE and optionally write CONTENT."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (when content (insert content)))
  file)

(ert-deftest test-cn-mask-basic-root ()
  "Basic: \"*.el\" should match only .el files in root."
  (cn--with-temp-dir
   (lambda (dir)
     (let* ((default-directory (file-name-as-directory dir)))
       (cn--touch (expand-file-name "a.el" dir) "(ok)")
       (cn--touch (expand-file-name "b.txt" dir) "txt")
       (let* ((pl (context-navigator--mask-base "*.el"))
              (rx (context-navigator--glob-to-regexp (plist-get pl :rel-pattern)
                                                     context-navigator-mask-globstar))
              (cands (context-navigator--collect-candidates pl))
              (hits (context-navigator--filter-matches cands rx pl)))
         (should (= (length hits) 1))
         (should (string-suffix-p "/a.el" (car hits))))))))

(ert-deftest test-cn-mask-cwd-dot-slash ()
  "Relative: \"./*.el\" matches only current dir."
  (cn--with-temp-dir
   (lambda (dir)
     (let* ((default-directory (file-name-as-directory dir)))
       (cn--touch (expand-file-name "a.el" dir) "(ok)")
       (make-directory (expand-file-name "sub" dir) t)
       (cn--touch (expand-file-name "sub/c.el" dir) "(ok)")
       (let* ((pl (context-navigator--mask-base "./*.el"))
              (rx (context-navigator--glob-to-regexp (plist-get pl :rel-pattern)
                                                     context-navigator-mask-globstar))
              (cands (context-navigator--collect-candidates pl))
              (hits (context-navigator--filter-matches cands rx pl)))
         (should (= (length hits) 1))
         (should (string-suffix-p "/a.el" (car hits))))))))

(ert-deftest test-cn-mask-globstar ()
  "\"./**/*.el\" should traverse subdirs."
  (cn--with-temp-dir
   (lambda (dir)
     (let* ((default-directory (file-name-as-directory dir)))
       (cn--touch (expand-file-name "a.el" dir) "(ok)")
       (make-directory (expand-file-name "sub/deeper" dir) t)
       (cn--touch (expand-file-name "sub/deeper/c.el" dir) "(ok)")
       (let* ((pl (context-navigator--mask-base "./**/*.el"))
              (rx (context-navigator--glob-to-regexp (plist-get pl :rel-pattern)
                                                     context-navigator-mask-globstar))
              (cands (context-navigator--collect-candidates pl))
              (hits (sort (mapcar #'abbreviate-file-name
                                  (context-navigator--filter-matches cands rx pl))
                          #'string-lessp)))
         (should (= (length hits) 2))
         (should (string-suffix-p "/a.el" (nth 0 hits)))
         (should (string-match-p "/sub/deeper/c\\.el\\'" (nth 1 hits))))))))

(ert-deftest test-cn-mask-dotfiles-default-hidden ()
  "\"*.el\" must not include dotfiles by default."
  (cn--with-temp-dir
   (lambda (dir)
     (let* ((default-directory (file-name-as-directory dir)))
       (cn--touch (expand-file-name "a.el" dir) "(ok)")
       (cn--touch (expand-file-name ".hidden.el" dir) "(ok)")
       (let* ((pl (context-navigator--mask-base "*.el"))
              (rx (context-navigator--glob-to-regexp (plist-get pl :rel-pattern)
                                                     context-navigator-mask-globstar))
              (cands (context-navigator--collect-candidates pl))
              (hits (context-navigator--filter-matches cands rx pl)))
         (should (= (length hits) 1))
         (should (string-suffix-p "/a.el" (car hits))))))))

(ert-deftest test-cn-mask-dotfiles-explicit ()
  "\".*.el\" must include dotfiles."
  (cn--with-temp-dir
   (lambda (dir)
     (let* ((default-directory (file-name-as-directory dir)))
       (cn--touch (expand-file-name ".hidden.el" dir) "(ok)")
       (let* ((pl (context-navigator--mask-base ".*.el"))
              (rx (context-navigator--glob-to-regexp (plist-get pl :rel-pattern)
                                                     context-navigator-mask-globstar))
              (cands (context-navigator--collect-candidates pl))
              (hits (context-navigator--filter-matches cands rx pl)))
         (should (= (length hits) 1))
         (should (string-suffix-p "/.hidden.el" (car hits))))))))

(ert-deftest test-cn-mask-char-class ()
  "\"test-[a-z].el\" matches a single alpha suffix."
  (cn--with-temp-dir
   (lambda (dir)
     (let* ((default-directory (file-name-as-directory dir)))
       (cn--touch (expand-file-name "test-a.el" dir) "(ok)")
       (cn--touch (expand-file-name "test-1.el" dir) "(ok)")
       (let* ((pl (context-navigator--mask-base "test-[a-z].el"))
              (rx (context-navigator--glob-to-regexp (plist-get pl :rel-pattern)
                                                     context-navigator-mask-globstar))
              (cands (context-navigator--collect-candidates pl))
              (hits (context-navigator--filter-matches cands rx pl)))
         (should (= (length hits) 1))
         (should (string-suffix-p "/test-a.el" (car hits))))))))

(ert-deftest test-cn-mask-negated-class ()
  "\"foo[!0-9].el\" excludes digits in that position."
  (cn--with-temp-dir
   (lambda (dir)
     (let* ((default-directory (file-name-as-directory dir)))
       (cn--touch (expand-file-name "fooA.el" dir) "(ok)")
       (cn--touch (expand-file-name "foo7.el" dir) "(ok)")
       (let* ((pl (context-navigator--mask-base "foo[!0-9].el"))
              (rx (context-navigator--glob-to-regexp (plist-get pl :rel-pattern)
                                                     context-navigator-mask-globstar))
              (cands (context-navigator--collect-candidates pl))
              (hits (context-navigator--filter-matches cands rx pl)))
         (should (= (length hits) 1))
         (should (string-suffix-p "/fooA.el" (car hits))))))))

(provide 'context-navigator-mask-tests)
;;; context-navigator-mask-tests.el ends here
