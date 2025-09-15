;;; context-navigator-view-controls-icons-test.el --- Tests for controls icons mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'context-navigator-view-controls)

(ert-deftest cn/controls-icons-no-brackets-when-available ()
  "When icon provider is available, first toggle (push) must not contain '['."
  (cl-letf (((symbol-function 'context-navigator-controls-icons-available-p) (lambda () t))
            ((symbol-function 'context-navigator-controls-icon)
             (lambda (&rest _args) "◎"))) ;; any single-glyph placeholder
    (let ((context-navigator-controls-style 'icons))
      (let* ((toggles (context-navigator-view-controls--build-toggles))
             (first (car toggles)))
        (should (stringp first))
        (should-not (string-match-p "\\[" first))))))

(ert-deftest cn/controls-fallback-brackets-when-icons-unavailable ()
  "When icons are unavailable, first toggle (push) should fall back to '[...]' compact label."
  (cl-letf (((symbol-function 'context-navigator-controls-icons-available-p) (lambda () nil))
            ((symbol-function 'context-navigator-controls-icon)
             (lambda (&rest _args) nil)))
    (let ((context-navigator-controls-style 'icons))
      (let* ((toggles (context-navigator-view-controls--build-toggles))
             (first (car toggles)))
        (should (stringp first))
        (should (string-match-p "\\[" first))))))
(provide 'context-navigator-view-controls-icons-test)
;;; context-navigator-view-controls-icons-test.el ends here
