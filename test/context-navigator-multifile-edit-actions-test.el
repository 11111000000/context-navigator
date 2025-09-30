;;; context-navigator-multifile-edit-actions-test.el --- Edit and actions for Multifile View -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'context-navigator)
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-view-multifile)
(require 'context-navigator-gptel-bridge)

(defun ctxnav-mf--make-temp (name content)
  (let* ((dir (make-temp-name (expand-file-name "ctxnav-mf-" temporary-file-directory))))
    (make-directory dir t)
    (let ((p (expand-file-name name dir)))
      (with-temp-file p (insert content))
      p)))

(defun ctxnav-mf--goto-item-by-name (name)
  "Move point to header line containing NAME in Multifile buffer."
  (goto-char (point-min))
  (search-forward name nil t)
  (beginning-of-line))

(defun ctxnav-mf--goto-item (item)
  "Move point to the header line for ITEM in Multifile buffer."
  (let* ((key (context-navigator-model-item-key item))
         (pos (when (fboundp 'context-navigator-multifile--pos-for-key)
                (context-navigator-multifile--pos-for-key key))))
    (if (and pos (consp pos))
        (progn
          (goto-char (car pos))
          (beginning-of-line))
      (ctxnav-mf--goto-item-by-name (context-navigator-item-name item)))))

(ert-deftest ctxnav-multifile/edit-selection-narrow-and-save ()
  "Edit selection via indirect buffer with narrowing; save persists to file."
  (let* ((p (ctxnav-mf--make-temp "edit.txt" "hello\nworld\n"))
         (pair (ctxnav-multifile-test--selection p "world"))
         (beg (car pair))
         (end (cdr pair))
         (base (find-file-noselect p))
         (sel (context-navigator-item-create
               :type 'selection
               :name "edit.txt:world"
               :path p :buffer base :beg beg :end end :enabled t)))
    (unwind-protect
        (progn
          (ignore-errors (context-navigator-set-items (list sel)))
          (ignore-errors (context-navigator-multifile-open))
          (with-current-buffer "*Context Multifile View*"
            (ctxnav-mf--goto-item sel)
            (context-navigator-multifile-edit))
          ;; Find indirect buffer and verify narrowing
          (let* ((ind (cl-find-if (lambda (b)
                                    (string-prefix-p "*cn-edit:" (buffer-name b)))
                                  (buffer-list))))
            (should (buffer-live-p ind))
            (with-current-buffer ind
              (should (buffer-narrowed-p))
              (should (= (point-min) beg))
              (should (= (point-max) end))
              ;; Insert suffix and save
              (goto-char (point-max))
              (insert "!")
              (save-buffer)))
          ;; Validate file content changed
          (with-temp-buffer
            (insert-file-contents p)
            (should (search-forward "world!" nil t))))
      (ignore-errors (context-navigator-multifile-close))
      (ignore-errors (kill-buffer base)))))

(ert-deftest ctxnav-multifile/toggle-delete-push ()
  "Toggle enabled flag, delete item, and push (stubbed) for item at point."
  (let* ((p (ctxnav-mf--make-temp "a.txt" "a\n"))
         (it (context-navigator-item-create :type 'file :name "a.txt" :path p :enabled t)))
    (unwind-protect
        (progn
          (ignore-errors (context-navigator-set-items (list it)))
          (ignore-errors (context-navigator-multifile-open))
          (with-current-buffer "*Context Multifile View*"
            (ctxnav-mf--goto-item it)
            ;; Toggle -> should disable
            (context-navigator-multifile-toggle)
            (let* ((st (context-navigator--state-get))
                   (idx (context-navigator-state-index st))
                   (key (context-navigator-model-item-key it))
                   (it* (gethash key idx)))
              (should (not (null it*)))
              (should (eq (context-navigator-item-enabled it*) nil)))
            ;; Stub push to ensure it was invoked
            (let ((called nil))
              (cl-letf (((symbol-function 'context-navigator-gptel-toggle-one)
                         (lambda (_x) (setq called t) :added)))
                (ctxnav-mf--goto-item it)
                (context-navigator-multifile-push)
                (should called)))
            ;; Delete -> item removed from model
            (context-navigator-multifile-delete)
            (let* ((st2 (context-navigator--state-get))
                   (idx2 (context-navigator-state-index st2))
                   (key2 (context-navigator-model-item-key it)))
              (should (null (gethash key2 idx2)))))))
    (ignore-errors (context-navigator-multifile-close))))

(provide 'context-navigator-multifile-edit-actions-test)
;;; context-navigator-multifile-edit-actions-test.el ends here
