;;; context-navigator-multifile-filter-visit-cleanup-test.el --- Multifile View: filter/visit/cleanup tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'context-navigator)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-multifile)
(require 'context-navigator-gptel-bridge)

(defun ctxnav-mf--tmpdir ()
  (make-temp-name (expand-file-name "ctxnav-mf-" temporary-file-directory)))

(defun ctxnav-mf--make-file (dir name content)
  (let* ((root (expand-file-name dir))
         (_ (make-directory root t))
         (path (expand-file-name name root)))
    (with-temp-file path
      (insert content))
    path))

(defun ctxnav-mf--goto-header-containing (needle)
  "Move point to header line that contains NEEDLE."
  (goto-char (point-min))
  (re-search-forward (regexp-quote needle) nil t)
  (beginning-of-line))

(defun ctxnav-mf--count-indirect-edits ()
  "Count live indirect edit buffers."
  (cl-count-if (lambda (b) (string-prefix-p "*cn-edit:" (buffer-name b)))
               (buffer-list)))

(ert-deftest ctxnav-multifile/filter-enabled-only-toggle ()
  "Toggle filter (f): push ON does not enforce filter; user can toggle freely."
  (let* ((dir (ctxnav-mf--tmpdir))
         (pf (ctxnav-mf--make-file dir "f.txt" "one\n"))
         (it1 (context-navigator-item-create :type 'file :name "f-on"  :path pf :enabled t))
         (it2 (context-navigator-item-create :type 'file :name "f-off" :path pf :enabled nil)))
    (unwind-protect
        (progn
          (setq context-navigator--push-to-gptel nil) ;; push OFF
          (context-navigator-set-items (list it1 it2))
          (context-navigator-multifile-open)
          (with-current-buffer "*Context Multifile View*"
            ;; Initially both present
            (goto-char (point-min))
            (should (search-forward "f-on" nil t))
            (should (search-forward "f-off" nil t))
            ;; Toggle filter -> enabled-only, f-off should disappear
            (context-navigator-multifile-toggle-filter)
            (goto-char (point-min))
            (should (search-forward "f-on" nil t))
            (should (not (search-forward "f-off" nil t))))
          ;; With push ON: still can toggle back to show all
          (setq context-navigator--push-to-gptel t)
          (with-current-buffer "*Context Multifile View*"
            (context-navigator-multifile-toggle-filter) ;; flip back to 'all'
            (goto-char (point-min))
            (should (search-forward "f-on" nil t))
            (should (search-forward "f-off" nil t))))
      (ignore-errors (context-navigator-multifile-close)))))


(ert-deftest ctxnav-multifile/visit-file-and-header-tags ()
  "RET on file visits buffer; header shows [on]/[off] and clickable action labels."
  (let* ((dir (ctxnav-mf--tmpdir))
         (pa (ctxnav-mf--make-file dir "a.txt" "A\n"))
         (pb (ctxnav-mf--make-file dir "b.txt" "B\n"))
         (it-on  (context-navigator-item-create :type 'file :name "a.txt" :path pa :enabled t))
         (it-off (context-navigator-item-create :type 'file :name "b.txt" :path pb :enabled nil)))
    (unwind-protect
        (progn
          (setq context-navigator--push-to-gptel nil)
          (context-navigator-set-items (list it-on it-off))
          (context-navigator-multifile-open)
          (with-current-buffer "*Context Multifile View*"
            ;; Check [on]/[off] and actions present in headers
            (goto-char (point-min))
            (should (search-forward "a.txt" nil t))
            (should (search-backward "[on]" nil t))
            (should (search-forward " [visit]" nil t))
            (should (search-forward " [t]" nil t))
            (should (search-forward " [d]" nil t))
            (should (search-forward " [p]" nil t))
            (goto-char (point-min))
            (should (search-forward "b.txt" nil t))
            (should (search-backward "[off]" nil t))
            ;; Visit file a.txt
            (ctxnav-mf--goto-header-containing "a.txt")
            (context-navigator-multifile-visit))
          (should (buffer-live-p (get-file-buffer pa))))
      (ignore-errors (context-navigator-multifile-close))
      (ignore-errors (kill-buffer (get-file-buffer pa)))
      (ignore-errors (kill-buffer (get-file-buffer pb))))))

(ert-deftest ctxnav-multifile/delete-cleans-indirect-and-close-cleans-up ()
  "Deleting a selection kills its indirect buffer; closing Multifile kills all indirects."
  (let* ((dir (ctxnav-mf--tmpdir))
         (pf (ctxnav-mf--make-file dir "s.txt" "x\ny\nz\n"))
         (buf (find-file-noselect pf))
         posy posz)
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "y")
      (setq posy (match-beginning 0))
      (search-forward "z")
      (setq posz (match-beginning 0)))
    (let* ((sel (context-navigator-item-create :type 'selection :name "sel" :path pf :buffer buf :beg posy :end posz :enabled t)))
      (unwind-protect
          (progn
            (context-navigator-set-items (list sel))
            (context-navigator-multifile-open)
            (with-current-buffer "*Context Multifile View*"
              (ctxnav-mf--goto-header-containing "sel")
              ;; Open indirect
              (context-navigator-multifile-edit))
            (should (= (ctxnav-mf--count-indirect-edits) 1))
            ;; Delete selection -> indirect should be killed
            (with-current-buffer "*Context Multifile View*"
              (context-navigator-multifile-delete))
            (should (= (ctxnav-mf--count-indirect-edits) 0))
            ;; Re-open indirect, then close Multifile -> cleans up
            (with-current-buffer "*Context Multifile View*"
              (ctxnav-mf--goto-header-containing "sel")
              (context-navigator-multifile-edit))
            (should (= (ctxnav-mf--count-indirect-edits) 1))
            (context-navigator-multifile-close)
            (should (= (ctxnav-mf--count-indirect-edits) 0)))
        (ignore-errors (kill-buffer buf))
        (ignore-errors (context-navigator-multifile-close))))))

(ert-deftest ctxnav-multifile/edit-all-threshold-and-open ()
  "Edit-All honors threshold confirmation; opens all selections on confirm."
  (let* ((dir (ctxnav-mf--tmpdir))
         (pf (ctxnav-mf--make-file dir "e.txt" "a\nb\nc\nd\n"))
         (b (find-file-noselect pf))
         beg1 end1 beg2 end2
         (context-navigator-mf-open-all-threshold 1))
    (with-current-buffer b
      (goto-char (point-min))
      (search-forward "b")
      (setq beg1 (match-beginning 0) end1 (match-end 0))
      (search-forward "d")
      (setq beg2 (match-beginning 0) end2 (match-end 0)))
    (let* ((s1 (context-navigator-item-create :type 'selection :name "sb" :path pf :buffer b :beg beg1 :end end1 :enabled t))
           (s2 (context-navigator-item-create :type 'selection :name "sd" :path pf :buffer b :beg beg2 :end end2 :enabled t)))
      (unwind-protect
          (progn
            (context-navigator-set-items (list s1 s2))
            (context-navigator-multifile-open)
            (with-current-buffer "*Context Multifile View*"
              ;; Decline confirmation -> nothing opens
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
                (context-navigator-multifile-edit-all))
              (should (= (ctxnav-mf--count-indirect-edits) 0))
              ;; Accept confirmation -> both open
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (context-navigator-multifile-edit-all))
              (should (= (ctxnav-mf--count-indirect-edits) 2))))
        (ignore-errors (context-navigator-multifile-close))
        (ignore-errors (kill-buffer b))))))

(ert-deftest ctxnav-multifile/help-opens-transient ()
  "‘?’ in Multifile triggers transient/help entry."
  (let ((called nil))
    (cl-letf (((symbol-function 'context-navigator-view-transient)
               (lambda (&rest _) (setq called t) (message "TRANSIENT"))))
      (unwind-protect
          (progn
            (context-navigator-set-items '())
            (context-navigator-multifile-open)
            (with-current-buffer "*Context Multifile View*"
              (context-navigator-multifile-help))
            (should called))
        (ignore-errors (context-navigator-multifile-close))))))

(provide 'context-navigator-multifile-filter-visit-cleanup-test)
;;; context-navigator-multifile-filter-visit-cleanup-test.el ends here
