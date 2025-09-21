;;; context-navigator-multifile-remote-and-selection-edge-test.el --- Remote preview and selection edge cases -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'context-navigator)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-multifile)

(defun ctxnav-mf2--tmpdir ()
  (make-temp-name (expand-file-name "ctxnav-mf2-" temporary-file-directory)))

(defun ctxnav-mf2--make-file (dir name content)
  (let* ((root (expand-file-name dir))
         (_ (make-directory root t))
         (path (expand-file-name name root)))
    (with-temp-file path
      (insert content))
    path))

(defun ctxnav-mf2--goto-header (needle)
  (goto-char (point-min))
  (re-search-forward (regexp-quote needle) nil t)
  (beginning-of-line))

(defun ctxnav-mf2--count-indirect ()
  (cl-count-if (lambda (b) (string-prefix-p "*cn-edit:" (buffer-name b)))
               (buffer-list)))

(ert-deftest ctxnav-multifile/remote-preview-strict-and-off ()
  "TRAMP remote file: off -> disabled stub; strict -> unreadable fallback."
  (let* ((remote "/ssh:host:/var/tmp/r.txt")
         (it (context-navigator-item-create :type 'file :name "r.txt" :path remote :enabled t)))
    (unwind-protect
        (progn
          ;; off -> disabled message
          (let ((context-navigator-mf-remote-preview-mode 'off))
            (context-navigator-set-items (list it))
            (context-navigator-multifile-open)
            (with-current-buffer "*Context Multifile View*"
              (goto-char (point-min))
              (should (search-forward "<remote preview disabled>" nil t))))
          (ignore-errors (context-navigator-multifile-close))
          ;; strict -> unreadable (no TRAMP session available in tests)
          (let ((context-navigator-mf-remote-preview-mode 'strict))
            (context-navigator-set-items (list it))
            (context-navigator-multifile-open)
            (with-current-buffer "*Context Multifile View*"
              (goto-char (point-min))
              (should (search-forward "<unreadable>" nil t)))))
      (ignore-errors (context-navigator-multifile-close)))))

(ert-deftest ctxnav-multifile/path-only-selection-edit ()
  "Selection with only :path (no live buffer) opens base via find-file-noselect and narrows."
  (let* ((dir (ctxnav-mf2--tmpdir))
         (p   (ctxnav-mf2--make-file dir "po.txt" "foo\nbar\nbaz\n"))
         ;; Reuse helper from basic test; it is loaded earlier by run-tests.
         (pair (ctxnav-multifile-test--selection p "bar"))
         (beg (car pair))
         (end (cdr pair))
         (sel (context-navigator-item-create :type 'selection :name "po.txt:bar"
                                             :path p :buffer nil :beg beg :end end :enabled t)))
    (unwind-protect
        (progn
          (context-navigator-set-items (list sel))
          (context-navigator-multifile-open)
          (with-current-buffer "*Context Multifile View*"
            (ctxnav-mf2--goto-header "po.txt")
            (context-navigator-multifile-edit))
          (let ((ind (cl-find-if (lambda (b) (string-prefix-p "*cn-edit:" (buffer-name b)))
                                 (buffer-list))))
            (should (buffer-live-p ind))
            (with-current-buffer ind
              (should (buffer-narrowed-p))
              (should (= (point-min) beg))
              (should (= (point-max) end))
              ;; Modify and save to ensure persistence to file
              (goto-char (point-max))
              (insert "X")
              (save-buffer)))
          (with-temp-buffer
            (insert-file-contents p)
            (should (search-forward "barX" nil t))))
      (ignore-errors (context-navigator-multifile-close))
      (ignore-errors (kill-buffer (get-file-buffer p))))))

(provide 'context-navigator-multifile-remote-and-selection-edge-test)
;;; context-navigator-multifile-remote-and-selection-edge-test.el ends here
