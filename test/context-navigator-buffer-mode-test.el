;;; context-navigator-buffer-mode-test.el --- Tests for magit-like buffer mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-core)
(require 'context-navigator-view)

(defmacro ctxnav-buf-save-layout (&rest body)
  "Run BODY and restore window configuration afterwards."
  (declare (indent 0))
  `(save-window-excursion ,@body))

(defun ctxnav--navigator-buffer ()
  (get-buffer "*context-navigator*"))

(ert-deftest ctxnav-buffer/open-split-and-mark ()
  "With a single window, buffer-open should split and mark the window as 'buffer."
  (ctxnav-buf-save-layout
   (delete-other-windows)
   (let* ((context-navigator-display-mode 'buffer)
          (win0 (selected-window)))
     (should (= (count-windows) 1))
     (context-navigator-buffer-open)
     (should (= (count-windows) 2))
     (let* ((buf (ctxnav--navigator-buffer))
            (sel (selected-window)))
       (should (buffer-live-p buf))
       (should (eq (window-buffer sel) buf))
       ;; window parameter must be set to 'buffer (for balance protections/visit logic)
       (should (eq (window-parameter sel 'context-navigator-view) 'buffer))))))

(ert-deftest ctxnav-buffer/open-reuse-other-window ()
  "With two windows, buffer-open should reuse the other window and select it."
  (ctxnav-buf-save-layout
   (delete-other-windows)
   ;; Create a second window manually
   (let* ((w1 (selected-window))
          (w2 (split-window-right)))
     (with-selected-window w1
       (switch-to-buffer (get-buffer-create " ctxnav-test-a ")))
     (with-selected-window w2
       (switch-to-buffer (get-buffer-create " ctxnav-test-b ")))
     (select-window w1)
     (let ((before (count-windows)))
       (context-navigator-buffer-open)
       (should (= (count-windows) before))
       (let* ((buf (ctxnav--navigator-buffer))
              (sel (selected-window)))
         (should (buffer-live-p buf))
         ;; Selected window should now be the one we reused (was not selected initially)
         (should (not (eq sel w1)))
         (should (eq (window-buffer sel) buf))
         (should (eq (window-parameter sel 'context-navigator-view) 'buffer)))))))

(ert-deftest ctxnav-buffer/toggle-closes-on-current-frame ()
  "buffer-toggle should close only on the current frame."
  (ctxnav-buf-save-layout
   (delete-other-windows)
   (context-navigator-buffer-open)
   (should (get-buffer-window (ctxnav--navigator-buffer) 0))
   (context-navigator-buffer-toggle)
   (should-not (get-buffer-window (ctxnav--navigator-buffer) 0))))

(provide 'context-navigator-buffer-mode-test)
;;; context-navigator-buffer-mode-test.el ends here
