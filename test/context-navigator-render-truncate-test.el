;;; context-navigator-render-truncate-test.el --- Tests for name truncation -*- lexical-binding: t; -*-

(require 'ert)
(require 'context-navigator-render)
(require 'context-navigator-model)

(ert-deftest ctxnav-render/truncate-name-with-ellipsis ()
  "Long names should be truncated with ellipsis according to defcustom."
  (let* ((long-name (make-string 80 ?x))
         (context-navigator-render-truncate-name 20)
         (it (context-navigator-item-create :type 'file :path "/tmp/file" :name long-name :enabled t))
         (lines (context-navigator-render-build-item-lines (list it) nil 40))
         (line (car lines)))
    (should (stringp line))
    ;; Expect an ellipsis … in the rendered name
    (should (string-match-p "…" line))
    (should (< (length line) 80))))

(provide 'context-navigator-render-truncate-test)
;;; context-navigator-render-truncate-test.el ends here
