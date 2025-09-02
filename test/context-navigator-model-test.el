;;; context-navigator-model-test.el --- Tests for pure model -*- lexical-binding: t; -*-

(require 'ert)
(require 'context-navigator-model)

(ert-deftest context-navigator-model/item-key-stability ()
  (let* ((fi (context-navigator-item-create :type 'file :path "/a/b.txt" :name "b.txt"))
         (bu (context-navigator-item-create :type 'buffer :path "/a/b.txt" :name "b.txt"))
         (se (context-navigator-item-create :type 'selection :path "/a/b.txt" :beg 1 :end 10)))
    (should (equal (context-navigator-model-item-key fi) "file:/a/b.txt"))
    (should (equal (context-navigator-model-item-key bu) "buf:b.txt:/a/b.txt"))
    (should (equal (context-navigator-model-item-key se) "sel:/a/b.txt:1-10"))))

(ert-deftest context-navigator-model/diff-detects-enabled-change ()
  (let* ((a (context-navigator-item-create :type 'file :path "/x" :enabled t :name "x"))
         (b (context-navigator-item-create :type 'file :path "/x" :enabled nil :name "x"))
         (diff (context-navigator-model-diff (list a) (list b))))
    (should (equal (mapcar #'context-navigator-model-item-key (plist-get diff :update))
                   (list (context-navigator-model-item-key b))))
    (should (null (plist-get diff :add)))
    (should (null (plist-get diff :remove)))))

(provide 'context-navigator-model-test)
;;; context-navigator-model-test.el ends here
