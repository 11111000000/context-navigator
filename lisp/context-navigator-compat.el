;;; context-navigator-compat.el --- Small compatibility shims -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provide lightweight fallbacks for functions that may be missing on
;; some Emacs/CL builds used in CI or user setups.

;;; Code:

(require 'cl-lib)

;; Provide a minimal `cl-copy-struct' fallback when absent.
(unless (fboundp 'cl-copy-struct)
  (defun cl-copy-struct (obj)
    "Fallback compatibility for `cl-copy-struct'.
For known structs, return a shallow copy; for vectors use `copy-sequence';
otherwise try `copy-tree' as a best-effort generic copy."
    (cond
     ;; Context Navigator's state struct (avoid depending on core at load time)
     ((and (fboundp 'context-navigator-state-p)
           (ignore-errors (context-navigator-state-p obj))
           (fboundp 'context-navigator--state-copy))
      (context-navigator--state-copy obj))
     ((vectorp obj)
      (copy-sequence obj))
     (t
      (copy-tree obj)))))

(provide 'context-navigator-compat)
;;; context-navigator-compat.el ends here
