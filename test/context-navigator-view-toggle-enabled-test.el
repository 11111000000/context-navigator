;;; context-navigator-view-toggle-enabled-test.el --- Tests for sidebar toggle t (+autopush) -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-core)
(require 'context-navigator-render)
(require 'context-navigator-view)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)

(defun ctxnav-sidebar-test--render-single-item-buffer (item &optional keys)
  "Render single ITEM into a fresh buffer and return it. Optionally pass KEYS for indicators."
  (let ((buf (generate-new-buffer " *ctxnav-sidebar-test*")))
    (with-current-buffer buf
      (let ((context-navigator-render--gptel-keys keys))
        (let* ((lines (context-navigator-render-build-lines (list item) "Hdr" nil 40)))
          (context-navigator-render-apply-to-buffer (current-buffer) lines))))
    buf))

(defun ctxnav-sidebar-test--goto-first-item ()
  "Move point in current buffer to first item line."
  (let ((pos (text-property-not-all (point-min) (point-max) 'context-navigator-item nil)))
    (when pos (goto-char pos))
    pos))

(ert-deftest ctxnav-sidebar/toggle-enabled-autopush-removes-and-adds ()
  "With push=on, pressing t disables/enables in model and applies to gptel (remove/add)."
  (context-navigator-test-with-mocked-gptel
   (let* ((tmp (make-temp-file "ctxnav-a-"))
          (it (context-navigator-item-create :type 'file :path tmp :name (file-name-nondirectory tmp) :enabled t))
          ;; Seed model with one enabled item
          (_ (context-navigator-set-items (list it)))
          ;; Seed gptel with the same file (so initial pull sees presence)
          (_ (setq gptel--context (list (list :type 'file :path tmp))))
          (context-navigator--push-to-gptel t))
     (let* ((keys (list (context-navigator-model-item-key it)))
            (buf (ctxnav-sidebar-test--render-single-item-buffer it keys)))
       (unwind-protect
           (with-current-buffer buf
             (should (ctxnav-sidebar-test--goto-first-item))
             ;; Toggle to disabled -> expect removal from gptel
             (let ((before-calls (length gptel--calls)))
               (context-navigator-view-toggle-enabled)
               ;; Model: enabled=nil
               (let* ((st (context-navigator--state-get))
                      (idx (context-navigator-state-index st))
                      (key (context-navigator-model-item-key it)))
                 (should (not (context-navigator-item-enabled (gethash key idx)))))
               ;; gptel context should become empty (removed or reset to empty then add nothing)
               (should (= (length gptel--context) 0))
               ;; Some call to gptel happened
               (should (> (length gptel--calls) before-calls)))
             ;; Toggle back to enabled -> expect add to gptel
             (goto-char (point-min))
             (should (ctxnav-sidebar-test--goto-first-item))
             (let ((before (length gptel--context)))
               (context-navigator-view-toggle-enabled)
               (let* ((st2 (context-navigator--state-get))
                      (idx2 (context-navigator-state-index st2))
                      (key (context-navigator-model-item-key it)))
                 (should (context-navigator-item-enabled (gethash key idx2))))
               (should (= (length gptel--context) (1+ before)))
               (should (cl-find-if (lambda (pl) (equal (plist-get pl :path) tmp)) gptel--context))))
         (when (buffer-live-p buf) (kill-buffer buf))
         (ignore-errors (delete-file tmp)))))))

(ert-deftest ctxnav-sidebar/toggle-enabled-push-off-does-not-touch-gptel ()
  "With push=off, pressing t modifies model only; gptel context unchanged."
  (context-navigator-test-with-mocked-gptel
   (let* ((tmp (make-temp-file "ctxnav-b-"))
          (it (context-navigator-item-create :type 'file :path tmp :name (file-name-nondirectory tmp) :enabled t))
          (_ (context-navigator-set-items (list it)))
          ;; Seed gptel with the same file
          (_ (setq gptel--context (list (list :type 'file :path tmp))))
          (context-navigator--push-to-gptel nil))
     (let* ((buf (ctxnav-sidebar-test--render-single-item-buffer it nil)))
       (unwind-protect
           (with-current-buffer buf
             (should (ctxnav-sidebar-test--goto-first-item))
             (let ((calls-before (length gptel--calls))
                   (ctx-before (copy-sequence gptel--context)))
               (context-navigator-view-toggle-enabled)
               (let* ((st (context-navigator--state-get))
                      (idx (context-navigator-state-index st))
                      (key (context-navigator-model-item-key it)))
                 (should (not (context-navigator-item-enabled (gethash key idx)))))
               ;; No gptel changes
               (should (equal gptel--context ctx-before))
               (should (= (length gptel--calls) calls-before))))
         (when (buffer-live-p buf) (kill-buffer buf))
         (ignore-errors (delete-file tmp)))))))

(provide 'context-navigator-view-toggle-enabled-test)
;;; context-navigator-view-toggle-enabled-test.el ends here
