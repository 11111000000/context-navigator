;;; context-navigator-core-prune-test.el --- Tests for pruning dead buffers -*- lexical-binding: t; -*-

(require 'ert)
(require 'context-navigator-core)
(require 'context-navigator-model)

(ert-deftest ctxnav-core/prune-dead-buffer-items-on-set ()
  "set-items should remove buffer items with non-live buffers."
  (let* ((live (get-buffer-create "ctxnav-live"))
         (dead (generate-new-buffer "ctxnav-dead")))
    ;; Kill 'dead' so it is non-live
    (kill-buffer dead)
    (let* ((it-live (context-navigator-item-create :type 'buffer :name (buffer-name live) :buffer live :enabled t))
           (it-dead (context-navigator-item-create :type 'buffer :name "dead" :buffer dead :enabled t)))
      (unwind-protect
          (let* ((st (context-navigator-set-items (list it-live it-dead)))
                 (items (context-navigator-state-items st)))
            (should (= (length items) 1))
            (should (eq (context-navigator-item-buffer (car items)) live)))
        (when (buffer-live-p live) (kill-buffer live))))))

(provide 'context-navigator-core-prune-test)
;;; context-navigator-core-prune-test.el ends here
