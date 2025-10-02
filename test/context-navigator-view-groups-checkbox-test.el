;;; context-navigator-view-groups-checkbox-test.el --- Tests for MG checkboxes in groups view -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'context-navigator-view)
(require 'context-navigator-view-groups)
(require 'context-navigator-core)
(require 'context-navigator-persist)

(defun cn-test--make-state (&optional root slug)
  (context-navigator--state-make
   :last-project-root (or root "/tmp")
   :current-group-slug (or slug "g1")))

(ert-deftest context-navigator-groups-mg-checkbox-fallback ()
  "MG: should render text checkboxes [x]/[ ] when all-the-icons is unavailable."
  (let* ((state (cn-test--make-state "/tmp" "g1"))
         (context-navigator-view--groups
          (list (list :slug "g1" :display "Group 1" :path "/tmp/.context/g1.el")))
         ;; Stub persist functions: MG=on, selected contains g1; counts enabled=1 total=2
         (orig-ai (and (fboundp 'all-the-icons-material) (symbol-function 'all-the-icons-material))))
    (unwind-protect
        (cl-letf (((symbol-function 'context-navigator-persist-state-load)
                   (lambda (_root) (list :version 1 :multi t :selected '("g1"))))
                  ((symbol-function 'context-navigator-persist-group-item-count)
                   (lambda (_file) 2))
                  ((symbol-function 'context-navigator-persist-group-enabled-count)
                   (lambda (_file) (cons 1 2))))
          ;; Ensure all-the-icons-material is not fboundp to trigger fallback
          (when (fboundp 'all-the-icons-material)
            (fmakunbound 'all-the-icons-material))
          (let* ((lines (context-navigator-view-groups-body-lines state))
                 (s (car lines)))
            (should (stringp s))
            (should (string-match-p "\\[x\\]" s))
            (should-not (string-match-p "●\\|◐\\|○" s)))) ;; circles hidden in MG
      ;; restore
      (when orig-ai
        (fset 'all-the-icons-material orig-ai)))))

(ert-deftest context-navigator-groups-mg-checkbox-with-icons ()
  "MG: should render graphic checkbox via all-the-icons when available."
  (let* ((state (cn-test--make-state "/tmp" "g1"))
         (context-navigator-view--groups
          (list (list :slug "g1" :display "Group 1" :path "/tmp/.context/g1.el"))))
    (cl-letf (((symbol-function 'context-navigator-persist-state-load)
               (lambda (_root) (list :version 1 :multi t :selected '("g1"))))
              ((symbol-function 'context-navigator-persist-group-item-count)
               (lambda (_file) 2))
              ((symbol-function 'context-navigator-persist-group-enabled-count)
               (lambda (_file) (cons 1 2)))
              ;; Provide a fake all-the-icons-material
              ((symbol-function 'all-the-icons-material)
               (lambda (name &rest _)
                 (cond
                  ((string= name "check_box") "CHK[on]")
                  ((string= name "check_box_outline_blank") "CHK[off]")
                  (t "CHK")))))
      (let* ((lines (context-navigator-view-groups-body-lines state))
             (s (car lines)))
        (should (stringp s))
        (should (string-match-p "CHK\\[on\\]" s)) ;; selected checkbox icon appears
        ;; circles hidden in MG
        (should-not (string-match-p "●\\|◐\\|○" s))))))

(ert-deftest context-navigator-groups-line-keymap-has-mg-keys ()
  "Group-line keymap must bind t/m/SPC to group-toggle-select."
  (let ((map (and (boundp 'context-navigator-view--group-line-keymap)
                  context-navigator-view--group-line-keymap)))
    (should (keymapp map))
    (should (eq (lookup-key map (kbd "t")) 'context-navigator-view-group-toggle-select))
    (should (eq (lookup-key map (kbd "m")) 'context-navigator-view-group-toggle-select))
    (should (eq (lookup-key map (kbd "SPC")) 'context-navigator-view-group-toggle-select))))

(provide 'context-navigator-view-groups-checkbox-test)
;;; context-navigator-view-groups-checkbox-test.el ends here
