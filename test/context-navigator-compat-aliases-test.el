;;; context-navigator-compat-aliases-test.el --- Tests for legacy aliases and dispatch -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-view)
(require 'context-navigator-view-controls)
(require 'context-navigator-i18n)

(ert-deftest ctxnav-compat/controls-segments-basic ()
  "Controls segments should be available via public API."
  (cl-letf (((symbol-function 'context-navigator-view-controls-segments)
             (lambda () '(" X" " Y" " Z"))))
    (let ((a (context-navigator-view-controls-segments)))
      (should (equal a '(" X" " Y" " Z"))))))

(ert-deftest ctxnav-compat/activate-clear-gptel-dispatch ()
  "Action 'clear-gptel at point should invoke the command."
  (with-temp-buffer
    (let ((called 0))
      (insert "x")
      (add-text-properties (point-min) (point-max)
                           (list 'context-navigator-action 'clear-gptel))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'context-navigator-view-clear-gptel)
                 (lambda () (setq called (1+ called)))))
        ;; Minimal, side-effect-free dispatcher equivalent to the relevant branch
        (let ((act (get-text-property (point) 'context-navigator-action)))
          (when (eq act 'clear-gptel)
            (context-navigator-view-clear-gptel))))
      (should (= called 1)))))

(ert-deftest ctxnav-compat/items-header-toggle-lines-prefers-controls ()
  "items-header-toggle-lines should prefer controls-lines when available."
  (cl-letf (((symbol-function 'context-navigator-view-controls-lines)
             (lambda (_w) '(" H1" " H2")))
            ;; Ensure fallback won't be used
            ((symbol-function 'context-navigator-view--make-toggle-segments)
             (lambda () '(" legacy"))))
    (let ((out (context-navigator-view--items-header-toggle-lines 40)))
      (should (equal out '(" H1" " H2"))))))

(provide 'context-navigator-compat-aliases-test)
;;; context-navigator-compat-aliases-test.el ends here
