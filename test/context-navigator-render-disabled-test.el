;;; context-navigator-render-disabled-test.el --- Tests for disabled face rendering -*- lexical-binding: t; -*-

(require 'ert)
(require 'context-navigator-render)
(require 'context-navigator-model)

(defun ctxnav-render-test--line-has-disabled-face (line)
  "Return non-nil if LINE has context-navigator-disabled-face on any character."
  (let ((i 0) (n (length line)) (hit nil))
    (while (< i n)
      (let ((face (get-text-property i 'face line)))
        (when (or (eq face 'context-navigator-disabled-face)
                  (and (listp face) (memq 'context-navigator-disabled-face face)))
          (setq hit t)
          (setq i n)))
      (setq i (1+ i)))
    hit))

(ert-deftest ctxnav-render/disabled-items-use-face ()
  "Disabled items should render name with context-navigator-disabled-face (icon/indicator untouched)."
  (let* ((it (context-navigator-item-create :type 'file :path "/tmp/a" :name "a" :enabled nil))
         ;; No keys needed for this test
         (lines (context-navigator-render-build-lines (list it) "Hdr" nil 40))
         (line (nth 2 lines)))
    (should (stringp line))
    (should (ctxnav-render-test--line-has-disabled-face line))))

(provide 'context-navigator-render-disabled-test)
;;; context-navigator-render-disabled-test.el ends here
