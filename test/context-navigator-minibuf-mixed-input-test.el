;;; context-navigator-minibuf-mixed-input-test.el --- ERT test: mixed input should ignore masks -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

(require 'context-navigator-core)
(require 'context-navigator-path-add)

(defun cn-mi--with-temp-dir (fn)
  "Create a temporary directory, call FN with it, then delete recursively."
  (let* ((dir (make-temp-file "cn-mi-" t)))
    (unwind-protect
        (funcall fn dir)
      (ignore-errors (delete-directory dir t)))))

(defun cn-mi--touch (file &optional content)
  "Create FILE and optionally write CONTENT."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (when content (insert content)))
  file)

(ert-deftest cn-mb/mixed-input-ignores-mask ()
  "Mixed input: names + mask => process only explicit names, ignore masks."
  (cn-mi--with-temp-dir
   (lambda (dir)
     (let* ((default-directory (file-name-as-directory dir)))
       ;; Prepare files: foo.txt (should be added), bar.el (should be ignored because mask is ignored)
       (cn-mi--touch (expand-file-name "foo.txt" dir) "ok")
       (cn-mi--touch (expand-file-name "bar.el" dir) "should be ignored via mask")

       ;; Reset model state to avoid interference from previous tests
       (context-navigator-set-items '())

       ;; Stub minibuffer input and silence messages
       (cl-letf (((symbol-function 'read-from-minibuffer)
                  (lambda (&rest _args) "foo.txt *.el"))
                 ((symbol-function 'message)
                  (lambda (&rest _args) nil)))
         ;; Run command
         (context-navigator-add-from-minibuffer)
         ;; Assert: only foo.txt added
         (let* ((st (context-navigator--state-get))
                (items (and st (context-navigator-state-items st))))
           (should (listp items))
           (should (= (length items) 1))
           (let ((it (car items)))
             (should (eq (context-navigator-item-type it) 'file))
             (should (string-suffix-p "/foo.txt" (context-navigator-item-path it))))))))))

(provide 'context-navigator-minibuf-mixed-input-test)
;;; context-navigator-minibuf-mixed-input-test.el ends here
