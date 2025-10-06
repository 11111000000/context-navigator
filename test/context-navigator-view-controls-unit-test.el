;;; context-navigator-view-controls-unit-test.el --- Unit tests for controls (toolbar) -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-view-controls)
(require 'context-navigator-i18n)

(defun ctxnav--collect-prop-values (strings prop)
  "Collect non-nil PROP values from propertized STRINGS."
  (let (vals)
    (dolist (s strings)
      (when (stringp s)
        (let ((pos (text-property-not-all 0 (length s) prop nil s)))
          (when pos
            (push (get-text-property pos prop s) vals)))))
    (nreverse (delete-dups vals))))

(ert-deftest ctxnav-controls/segments-contain-expected-actions-and-toggles ()
  "Controls should expose expected action and toggle symbols as text properties."
  (let ((context-navigator-view-controls-style 'text)
        (context-navigator-language 'en))
    ;; Pretend gptel is available so push toggle has active help/keys
    (cl-letf (((symbol-function 'context-navigator-gptel-available-p) (lambda () t)))
      (let ((segs (context-navigator-view-controls-segments)))
        (should (and (listp segs) (> (length segs) 0)))
        (let* ((acts (ctxnav--collect-prop-values segs 'context-navigator-action))
               (tgls (ctxnav--collect-prop-values segs 'context-navigator-toggle)))
          ;; Required actions (clear-gptel button removed; covered by toggle-all)
          (dolist (sym '(push-now open-buffers close-buffers toggle-all-gptel undo redo))
            (should (member sym acts)))
          ;; Required toggles (auto-project toggle key replaced legacy 'auto)
          (dolist (sym '(push auto-project))
            (should (member sym tgls))))))))

(ert-deftest ctxnav-controls/wrap-lines-fit-width ()
  "controls-lines must wrap segments to fit TOTAL-WIDTH."
  (let ((context-navigator-view-controls-style 'text)
        (context-navigator-language 'en))
    (let ((lines (context-navigator-view-controls-lines 16)))
      (should (and (listp lines) (> (length lines) 0)))
      (dolist (ln lines)
        (should (<= (string-width ln) 16))))))


(provide 'context-navigator-view-controls-unit-test)
;;; context-navigator-view-controls-unit-test.el ends here
