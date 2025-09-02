;;; context-navigator-fp-test.el --- Tests for FP helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'context-navigator-fp)

(ert-deftest context-navigator-fp/alist-set-order-and-immutability ()
  (let* ((orig '((a . 1) (b . 2)))
         (res1 (context-navigator-fp-alist-set orig 'b 22))
         (res2 (context-navigator-fp-alist-set orig 'c 3)))
    (should (equal orig '((a . 1) (b . 2))))
    (should (equal res1 '((a . 1) (b . 22))))
    (should (equal res2 '((a . 1) (b . 2) (c . 3))))))

(ert-deftest context-navigator-fp/unique-by-last-wins ()
  (let* ((seq '(a b a c b))
         (vals '((a . 1) (b . 2) (a . 3) (c . 4) (b . 5)))
         (res (context-navigator-fp-unique-by vals #'car)))
    (should (equal (mapcar #'car res) '(a c b)))
    (should (equal (mapcar #'cdr res) '(3 4 5)))))

(ert-deftest context-navigator-fp/diff-by-key-basic ()
  (let* ((old '((k . 1) (x . 9)))
         (new '((k . 2) (y . 7)))
         (diff (context-navigator-fp-diff-by-key old new #'car #'equal)))
    (should (equal (mapcar #'car (plist-get diff :add)) '(y)))
    (should (equal (mapcar #'car (plist-get diff :remove)) '(x)))
    (should (equal (mapcar #'car (plist-get diff :update)) '(k)))))

(provide 'context-navigator-fp-test)
;;; context-navigator-fp-test.el ends here
