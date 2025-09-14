;;; context-navigator-compat-aliases-test.el --- Tests for legacy aliases and dispatch -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-view)
(require 'context-navigator-view-controls)
(require 'context-navigator-i18n)

(ert-deftest ctxnav-compat/aliases-old-footer-controls ()
  "Legacy alias should delegate to new controls implementation."
  (cl-letf (((symbol-function 'context-navigator-view-controls-segments)
             (lambda () '(" X" " Y" " Z"))))
    (let ((a (context-navigator-view-controls-segments))
          (b (context-navigator-view--footer-control-segments)))
      (should (equal a b)))))

(ert-deftest ctxnav-compat/activate-clear-gptel-dispatch ()
  "RET on a segment with context-navigator-action 'clear-gptel should call the command."
  (with-temp-buffer
    (delay-mode-hooks (context-navigator-view-mode))
    (let* ((s " [∅]")
           (m (make-sparse-keymap))
           (called 0))
      (add-text-properties 0 (length s)
                           (list 'context-navigator-action 'clear-gptel
                                 'mouse-face 'highlight
                                 'keymap m 'local-map m)
                           s)
      (let ((inhibit-read-only t))
        (insert s))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'context-navigator-view-clear-gptel)
                 (lambda () (setq called (1+ called)))))
        (context-navigator-view-activate)
        (should (= called 1))))))

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
