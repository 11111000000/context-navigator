;;; context-navigator-test-helpers.el --- Test helpers for context-navigator  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Ensure project source is on load-path when running tests interactively
(add-to-list 'load-path (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(require 'context-navigator-events)

;; Declare specials so test stubs/timers see dynamic rebinding within tests.
(defvar gptel--calls nil)
(defvar gptel--context nil)

(defun context-navigator-test-wait (secs)
  "Wait up to SECS allowing timers/processes to run."
  (let ((end (+ (float-time) secs)))
    (while (< (float-time) end)
      (accept-process-output nil 0.01)
      (sit-for 0.01))))

(defmacro context-navigator-test-with-clean-events (&rest body)
  "Reset event bus/timers for the duration of BODY."
  `(unwind-protect
       (progn
         (context-navigator-events-reset)
         ,@body)
     (context-navigator-events-reset)))

(defmacro context-navigator-test-with-temp-dir (var &rest body)
  "Bind VAR to a fresh temp directory for BODY."
  (declare (indent 1))
  `(let* ((,var (make-temp-file "ctxnav-" t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defmacro context-navigator-test-with-mocked-gptel (&rest body)
  "Temporarily define minimal gptel API stubs and capture calls.
Binds dynamic vars:
- gptel--calls: list of (fn . args)
- gptel--context: mutable list representing current context"
  (declare (indent 0))
  `(let* ((gptel--calls '())
          (gptel--context '()))
     (cl-letf* (((symbol-function 'gptel-context-list)
                 (lambda () gptel--context))
                ((symbol-function 'gptel-context-add-file)
                 (lambda (path) (push (list :type 'file :path path) gptel--context)
                   (push (cons 'gptel-context-add-file (list path)) gptel--calls)))
                ((symbol-function 'gptel-context--add-region)
                 (lambda (buf beg end)
                   (let ((p (buffer-local-value 'buffer-file-name buf)))
                     (push (list :type 'selection :path p :beg beg :end end) gptel--context)
                     (push (cons 'gptel-context--add-region (list buf beg end)) gptel--calls))))
                ((symbol-function 'gptel-context-remove)
                 (lambda (path)
                   (setq gptel--context
                         (cl-remove-if (lambda (pl)
                                         (and (eq (plist-get pl :type) 'file)
                                              (equal (plist-get pl :path) path)))
                                       gptel--context))
                   (push (cons 'gptel-context-remove (list path)) gptel--calls)))
                ((symbol-function 'gptel-context-remove-all)
                 (lambda ()
                   (setq gptel--context '())
                   (push (cons 'gptel-context-remove-all nil) gptel--calls))))
       ,@body)))

(provide 'context-navigator-test-helpers)
;;; context-navigator-test-helpers.el ends here
