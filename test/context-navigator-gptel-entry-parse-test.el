;;; context-navigator-gptel-entry-parse-test.el --- Tests for entry->item parsing -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)

(defun cn-test--temp-buffer (&optional name content file)
  "Create a live temp buffer with optional NAME, CONTENT and FILE.
Return the buffer (caller is responsible for killing it if needed)."
  (let ((buf (generate-new-buffer (or name "cn-test-tmp"))))
    (with-current-buffer buf
      (when content
        (erase-buffer)
        (insert content))
      (when file
        (setq-local buffer-file-name file)))
    buf))

(ert-deftest context-navigator-gptel-entry-parse-path-basic ()
  "String or (PATH . PROPS) should yield a single file item."
  (let* ((p1 "/tmp/cn-parse-a.txt")
         (res1 (context-navigator-gptel--entry->item p1)))
    (should (context-navigator-item-p res1))
    (should (eq (context-navigator-item-type res1) 'file))
    (should (equal (context-navigator-item-path res1) p1))
    (should (equal (context-navigator-item-name res1) "cn-parse-a.txt"))
    (should (context-navigator-item-enabled res1)))
  (let* ((p2 "/tmp/cn-parse-b.org")
         (entry (cons p2 '(:ignored t)))
         (res2 (context-navigator-gptel--entry->item entry)))
    (should (context-navigator-item-p res2))
    (should (eq (context-navigator-item-type res2) 'file))
    (should (equal (context-navigator-item-path res2) p2))
    (should (equal (context-navigator-item-name res2) "cn-parse-b.org"))))

(ert-deftest context-navigator-gptel-entry-parse-plist-file ()
  "Plist file form should be mapped to a file item."
  (let* ((p "/tmp/cn-parse-file.pl")
         (it (context-navigator-gptel--entry->item
              (list :type 'file :path p :name "Explicit"))))
    (should (context-navigator-item-p it))
    (should (eq (context-navigator-item-type it) 'file))
    (should (equal (context-navigator-item-path it) p))
    (should (equal (context-navigator-item-name it) "Explicit"))
    (should (context-navigator-item-enabled it))))

(ert-deftest context-navigator-gptel-entry-parse-plist-selection-no-path ()
  "Selection plist without path uses buffer-name in the title."
  (let* ((buf (cn-test--temp-buffer "cn-test-buf" "abcdef"))
         (entry (list :type 'selection :buffer buf :beg 2 :end 4))
         (it (context-navigator-gptel--entry->item entry)))
    (should (context-navigator-item-p it))
    (should (eq (context-navigator-item-type it) 'selection))
    (should (eq (context-navigator-item-buffer it) buf))
    (should (null (context-navigator-item-path it)))
    (should (equal (context-navigator-item-beg it) 2))
    (should (equal (context-navigator-item-end it) 4))
    ;; Name should be "<buffer-name>:beg-end"
    (should (string-match-p "^cn-test-buf:2-4$" (context-navigator-item-name it)))))

(ert-deftest context-navigator-gptel-entry-parse-plist-selection-with-path ()
  "Selection plist with path uses basename in the title."
  (let* ((p "/tmp/cn-parse-sel.txt")
         (it (context-navigator-gptel--entry->item
              (list :type 'selection :path p :beg 1 :end 3))))
    (should (context-navigator-item-p it))
    (should (eq (context-navigator-item-type it) 'selection))
    (should (equal (context-navigator-item-path it) p))
    (should (equal (context-navigator-item-beg it) 1))
    (should (equal (context-navigator-item-end it) 3))
    (should (equal (context-navigator-item-name it) "cn-parse-sel.txt:1-3"))))

(ert-deftest context-navigator-gptel-entry-parse-plist-buffer ()
  "Buffer plist yields a buffer item."
  (let* ((buf (cn-test--temp-buffer "cn-plist-buffer" "xyz"))
         (it (context-navigator-gptel--entry->item
              (list :type 'buffer :buffer buf :name "B"))))
    (should (context-navigator-item-p it))
    (should (eq (context-navigator-item-type it) 'buffer))
    (should (eq (context-navigator-item-buffer it) buf))
    (should (equal (context-navigator-item-name it) "B"))))

(ert-deftest context-navigator-gptel-entry-parse-buffer-regs-all ()
  "Entry (BUF . t) yields one buffer item."
  (let* ((buf (cn-test--temp-buffer "cn-buf-all" "hello"))
         (res (context-navigator-gptel--entry->item (cons buf t))))
    (should (listp res))
    (should (= (length res) 1))
    (let ((it (car res)))
      (should (context-navigator-item-p it))
      (should (eq (context-navigator-item-type it) 'buffer))
      (should (eq (context-navigator-item-buffer it) buf)))))

(ert-deftest context-navigator-gptel-entry-parse-buffer-regs-full-range-as-buffer ()
  "Entry (BUF . (pmin . pmax)) yields buffer item (not selection)."
  (let* ((buf (cn-test--temp-buffer "cn-buf-range" "abc"))
         pmin pmax res)
    (with-current-buffer buf
      (setq pmin (point-min)
            pmax (point-max)))
    (setq res (context-navigator-gptel--entry->item (cons buf (cons pmin pmax))))
    (should (listp res))
    (should (= (length res) 1))
    (let ((it (car res)))
      (should (eq (context-navigator-item-type it) 'buffer)))))

(ert-deftest context-navigator-gptel-entry-parse-buffer-selections ()
  "Multiple regions produce multiple selection items, preserving order."
  (let* ((buf (cn-test--temp-buffer "cn-buf-sels" "0123456789"))
         s1 e1 s2 e2 items)
    (with-current-buffer buf
      (setq s1 2 e1 4 s2 6 e2 9))
    ;; Use vector to test coerce and order preservation
    (setq items (context-navigator-gptel--entry->item
                 (cons buf (vector (cons s1 e1) (cons s2 e2)))))
    (should (listp items))
    (should (= (length items) 2))
    (let ((it1 (nth 0 items))
          (it2 (nth 1 items)))
      (should (eq (context-navigator-item-type it1) 'selection))
      (should (eq (context-navigator-item-type it2) 'selection))
      (should (equal (list (context-navigator-item-beg it1)
                           (context-navigator-item-end it1))
                     (list s1 e1)))
      (should (equal (list (context-navigator-item-beg it2)
                           (context-navigator-item-end it2))
                     (list s2 e2))))))

(ert-deftest context-navigator-gptel-entry-parse-buffer-overlay ()
  "Overlay region is recognized as a selection."
  (let* ((buf (cn-test--temp-buffer "cn-buf-ov" "abcdefg"))
         ov items)
    (with-current-buffer buf
      (setq ov (make-overlay 3 6)))
    (setq items (context-navigator-gptel--entry->item (cons buf (list ov))))
    (should (listp items))
    (should (= (length items) 1))
    (let ((it (car items)))
      (should (eq (context-navigator-item-type it) 'selection))
      (should (equal (list (context-navigator-item-beg it)
                           (context-navigator-item-end it))
                     (list 3 6))))))

(ert-deftest context-navigator-gptel-entry-parse-selection-name-with-buffer-file ()
  "When buffer has a file, selection title uses basename:beg-end."
  (let* ((file (expand-file-name "cn-test-file.txt" temporary-file-directory))
         (buf (cn-test--temp-buffer "cn-file-buf" "abcdef" file))
         (items (context-navigator-gptel--entry->item (cons buf (list (cons 2 5)))))
         (it (car items)))
    (should (eq (context-navigator-item-type it) 'selection))
    (should (equal (context-navigator-item-name it) "cn-test-file.txt:2-5"))
    (should (equal (context-navigator-item-path it) file))))

(ert-deftest context-navigator-gptel-entry-parse-plist-like-detection ()
  "Detect plist-like entries that specify :type."
  (should (context-navigator-gptel--plist-like-entry-p '(:type file :path "/a")))
  (should (context-navigator-gptel--plist-like-entry-p '(type file path "/a")))
  (should-not (context-navigator-gptel--plist-like-entry-p (cons (get-buffer-create "x") t)))
  (should-not (context-navigator-gptel--plist-like-entry-p "/a/b")))

(provide 'context-navigator-gptel-entry-parse-test)
;;; context-navigator-gptel-entry-parse-test.el ends here
