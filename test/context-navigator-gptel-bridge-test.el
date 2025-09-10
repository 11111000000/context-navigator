;;; context-navigator-gptel-bridge-test.el --- Tests for gptel bridge -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)

(ert-deftest context-navigator-gptel/pull-converts-basic-entries ()
  "Bridge converts basic raw entries: file string and buffer region pair."
  (context-navigator-test-with-mocked-gptel
   (let* ((fa (make-temp-file "ctxnav-a-" nil ".txt"))
          (fc (make-temp-file "ctxnav-c-" nil ".txt"))
          (buf (find-file-noselect fc)))
     (unwind-protect
         (with-current-buffer buf
           (erase-buffer)
           (insert "hello world")
           (save-buffer)
           (let* ((beg (point-min))
                  (end (+ beg 5))
                  (raw (list
                        ;; file as string path
                        fa
                        ;; buffer with region (beg . end)
                        (cons buf (list (cons beg end))))))
             ;; Force bridge to use our raw list
             (cl-letf (((symbol-function 'context-navigator-gptel--context-list)
                        (lambda () raw)))
               (let* ((items (context-navigator-gptel-pull))
                      (keys  (mapcar #'context-navigator-model-item-key items)))
                 (should (>= (length items) 2))
                 (should (member (format "file:%s" fa) keys))
                 ;; selection key uses file path of the buffer's file
                 (should (member (format "sel:%s:%d-%d" fc beg end) keys))))))
       (when (buffer-live-p buf) (kill-buffer buf))
       (ignore-errors (delete-file fa))
       (ignore-errors (delete-file fc))))))

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
