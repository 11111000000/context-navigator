;;; context-navigator-mask-posix-class-test.el --- ERT tests for POSIX classes in masks -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

(require 'context-navigator-path-add)

(defun cn--pc-with-temp-dir (fn)
  "Create a temporary directory, call FN with it, then delete recursively."
  (let* ((dir (make-temp-file "cn-mask-pc-" t)))
    (unwind-protect
        (funcall fn dir)
      (ignore-errors (delete-directory dir t)))))

(defun cn--pc-touch (file &optional content)
  "Create FILE and optionally write CONTENT."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (when content (insert content)))
  file)

(ert-deftest test-cn-mask-posix-class-alpha ()
  "\"test-[[:alpha:]].el\" matches only single alpha letter."
  (cn--pc-with-temp-dir
   (lambda (dir)
     (let* ((default-directory (file-name-as-directory dir)))
       (cn--pc-touch (expand-file-name "test-a.el" dir) "(ok)")
       (cn--pc-touch (expand-file-name "test-1.el" dir) "(no)")
       (let* ((pl (context-navigator--mask-base "test-[[:alpha:]].el"))
              (rx (context-navigator--glob-to-regexp (plist-get pl :rel-pattern)
                                                     context-navigator-mask-globstar))
              (cands (context-navigator--collect-candidates pl))
              (hits (context-navigator--filter-matches cands rx pl)))
         (should (= (length hits) 1))
         (should (string-suffix-p "/test-a.el" (car hits))))))))

(ert-deftest test-cn-mask-posix-class-digit ()
  "\"file[[:digit:]].txt\" matches a single digit suffix."
  (cn--pc-with-temp-dir
   (lambda (dir)
     (let* ((default-directory (file-name-as-directory dir)))
       (cn--pc-touch (expand-file-name "file1.txt" dir) "(ok)")
       (cn--pc-touch (expand-file-name "fileA.txt" dir) "(no)")
       (let* ((pl (context-navigator--mask-base "file[[:digit:]].txt"))
              (rx (context-navigator--glob-to-regexp (plist-get pl :rel-pattern)
                                                     context-navigator-mask-globstar))
              (cands (context-navigator--collect-candidates pl))
              (hits (context-navigator--filter-matches cands rx pl)))
         (should (= (length hits) 1))
         (should (string-suffix-p "/file1.txt" (car hits))))))))

(provide 'context-navigator-mask-posix-class-test)
;;; context-navigator-mask-posix-class-test.el ends here
