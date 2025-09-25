;;; context-navigator-util.el --- Small shared utilities -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Small, stable helpers shared across modules to reduce duplication.

;;; Code:

(require 'subr-x)

(defgroup context-navigator-util nil
  "Utility helpers for Context Navigator."
  :group 'context-navigator)

(defun context-navigator-human-size (bytes)
  "Return human-readable size for BYTES."
  (cond
   ((null bytes) "?")
   ((< bytes 1024) (format "%d B" bytes))
   ((< bytes (* 1024 1024)) (format "%.1f KB" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1f MB" (/ bytes 1048576.0)))
   (t (format "%.1f GB" (/ bytes 1073741824.0)))))

(provide 'context-navigator-util)
;;; context-navigator-util.el ends here
