;;; context-navigator-multifile-basic-test.el --- Basic tests for Multifile View -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'context-navigator)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-multifile)

(defun ctxnav-multifile-test--make-file (dir name content)
  "Create a temp file NAME in DIR with CONTENT. Return absolute path."
  (let* ((root (expand-file-name dir))
         (_ (make-directory root t))
         (path (expand-file-name name root)))
    (with-temp-file path
      (insert content))
    path))

(defun ctxnav-multifile-test--selection (path needle)
  "Return cons (BEG . END) of first occurrence of NEEDLE in PATH buffer."
  (let* ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (when (search-forward needle nil t)
          (cons (match-beginning 0) (match-end 0)))))))

(ert-deftest ctxnav-multifile/open-render-and-navigation ()
  "Open Multifile View, render section headers and navigate j/k."
  (let* ((tmp (make-temp-name (expand-file-name "ctxnav-mf-" temporary-file-directory)))
         (p1 (ctxnav-multifile-test--make-file tmp "a.txt" "alpha\nbeta\ngamma\n"))
         (p2 (ctxnav-multifile-test--make-file tmp "b.txt" "one\ntwo\nthree\n"))
         (it1 (context-navigator-item-create :type 'file :name "a.txt" :path p1 :enabled t))
         (it2 (context-navigator-item-create :type 'file :name "b.txt" :path p2 :enabled t)))
    (unwind-protect
        (progn
          (ignore-errors (context-navigator-set-items (list it1 it2)))
          (let ((win (context-navigator-multifile-open)))
            (should (window-live-p win))
            (let* ((buf (get-buffer "*Context Multifile View*")))
              (should (buffer-live-p buf))
              (with-current-buffer buf
                (should (eq major-mode 'context-navigator-multifile-mode))
                (goto-char (point-min))
                ;; Should have the title and two headers with file names
                (should (search-forward "Context Multifile View" nil t))
                (should (search-forward "a.txt" nil t))
                (should (search-forward "b.txt" nil t))
                ;; Navigation j/k between section headers should move point
                (goto-char (point-min))
                (context-navigator-multifile-next)
                (let ((p1 (point)))
                  (context-navigator-multifile-next)
                  (let ((p2 (point)))
                    (should (> p2 p1))
                    (context-navigator-multifile-prev)
                    (should (= (point) p1))))))))
      (ignore-errors (context-navigator-multifile-close)))))

(ert-deftest ctxnav-multifile/tramp-remote-preview-lazy ()
  "Remote path preview should be stubbed in lazy mode."
  (let* ((remote-path "/ssh:host:/var/tmp/remote.txt")
         (it (context-navigator-item-create :type 'file :name "remote.txt" :path remote-path :enabled t)))
    (let ((context-navigator-mf-remote-preview-mode 'lazy))
      (unwind-protect
          (progn
            (ignore-errors (context-navigator-set-items (list it)))
            (ignore-errors (context-navigator-multifile-open))
            (with-current-buffer "*Context Multifile View*"
              (goto-char (point-min))
              (should (search-forward "<remote preview (lazy): not reading file>" nil t))))
        (ignore-errors (context-navigator-multifile-close))))))

(provide 'context-navigator-multifile-basic-test)
;;; context-navigator-multifile-basic-test.el ends here
