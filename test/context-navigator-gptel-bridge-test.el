;;; context-navigator-gptel-bridge-test.el --- Tests for gptel bridge -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)

(ert-deftest context-navigator-gptel/pull-converts-basic-entries ()
  (context-navigator-test-with-mocked-gptel
   (let* ((gptel--context (list
                           (list :type 'file :path "/tmp/a.txt")
                           (list :type 'selection :path "/tmp/b.txt" :beg 1 :end 5)
                           (list :type 'buffer :path "/tmp/c.txt"))))
     (let ((items (context-navigator-gptel-pull)))
       (should (= (length items) 3))
       (should (member "file:/tmp/a.txt" (mapcar #'context-navigator-model-item-key items)))
       (should (member "sel:/tmp/b.txt:1-5" (mapcar #'context-navigator-model-item-key items)))
       (should (member "buf:c.txt:/tmp/c.txt" (mapcar #'context-navigator-model-item-key items)))))))

;; apply should ignore disabled items on add
(ert-deftest context-navigator-gptel/apply-ignores-disabled-on-add ()
  (context-navigator-test-with-mocked-gptel
   (let* ((enabled (context-navigator-item-create :type 'file :path "/tmp/en.txt" :name "en" :enabled t))
          (disabled (context-navigator-item-create :type 'file :path "/tmp/dis.txt" :name "dis" :enabled nil))
          (added '()))
     ;; Intercept add-item to count how many are attempted to be added
     (cl-letf (((symbol-function 'context-navigator-gptel--add-item)
                (lambda (item) (push (context-navigator-model-item-key item) added) t))
               ;; Make sure capabilities are "available"
               ((symbol-function 'gptel-context-remove) (lambda (&rest _) nil))
               ((symbol-function 'gptel-context-add-file) (lambda (&rest _) nil)))
       (let ((res (context-navigator-gptel-apply (list enabled disabled))))
         (ignore res)
         ;; Only enabled item should be attempted to be added
         (should (equal (sort added #'string<)
                        (list (context-navigator-model-item-key enabled)))))))))

(provide 'context-navigator-gptel-bridge-test)
;;; context-navigator-gptel-bridge-test.el ends here
