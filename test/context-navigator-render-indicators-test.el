;;; context-navigator-render-indicators-test.el --- Tests for render indicators -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-render)
(require 'context-navigator-model)

(ert-deftest ctxnav-render/indicator-gray-when-no-keys ()
  "When gptel keys are absent, gray indicator ○ should be rendered (always-on indicators)."
  (let* ((item (context-navigator-item-create :type 'file :path "/tmp/a.txt" :name "a.txt" :enabled t))
         (context-navigator-render--gptel-keys nil)
         (lines (context-navigator-render-build-lines (list item) "Hdr" nil 32))
         (line (nth 2 lines))) ;; first item line
    (should (stringp line))
    ;; Expect gray ○ when keys snapshot is absent
    (should (string-match-p "○" line))
    ;; Should not show green ● in this case
    (should-not (string-match-p "●" line))))

(ert-deftest ctxnav-render/indicators-text-fallback-when-no-icons ()
  "With keys present and no icon provider, text bullets should be shown."
  (let* ((it (context-navigator-item-create :type 'file :path "/tmp/a.txt" :name "a.txt" :enabled t))
         ;; Present key
         (context-navigator-render--gptel-keys (list (context-navigator-model-item-key it)))
         ;; Ensure icon provider exists but returns nil (simulate all-the-icons missing)
         (provided (fboundp 'context-navigator-icons-for-indicator)))
    (cl-letf (((symbol-function 'context-navigator-icons-for-indicator)
               (lambda (_state) nil)))
      (let* ((lines (context-navigator-render-build-lines (list it) "Hdr" nil 32))
             (line (nth 2 lines)))
        (should (stringp line))
        ;; Expect some bullet present as text fallback
        (should (string-match-p "●" line))))))
(provide 'context-navigator-render-indicators-test)
;;; context-navigator-render-indicators-test.el ends here
