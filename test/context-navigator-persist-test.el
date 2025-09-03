;;; context-navigator-persist-test.el --- Tests for persistence layer -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-test-helpers)
(require 'context-navigator-model)
(require 'context-navigator-persist)
(require 'context-navigator-events)

(ert-deftest context-navigator-persist/save-load-roundtrip ()
  (context-navigator-test-with-temp-dir dir
                                        (let* ((root dir)
                                               (context-navigator-dir-name ".context") ;; ensure relative within root
                                               (items (list
                                                       (context-navigator-item-create :type 'file :path (expand-file-name "a.txt" root) :name "a.txt" :enabled t)
                                                       (context-navigator-item-create :type 'buffer :path (expand-file-name "b.txt" root) :name "b.txt" :enabled nil)
                                                       (context-navigator-item-create :type 'selection :path (expand-file-name "c.txt" root) :beg 1 :end 10 :name "c.txt:1-10" :enabled t))))
                                          ;; create dummy files
                                          (dolist (p (list "a.txt" "b.txt" "c.txt"))
                                            (with-temp-file (expand-file-name p root) (insert "x")))
                                          (let ((file (context-navigator-persist-save items root)))
                                            (should (and (stringp file) (file-exists-p file))))
                                          (let ((got nil)
                                                (start 0)
                                                (steps '()))
                                            (context-navigator-events-subscribe :context-load-start (lambda (&rest _) (setq start (1+ start))))
                                            (context-navigator-events-subscribe :context-load-step (lambda (_root pos total) (push (list pos total) steps)))
                                            (context-navigator-persist-load-async
                                             root
                                             (lambda (its) (setq got its)))
                                            (context-navigator-test-wait 0.05)
                                            (should (= start 1))
                                            (should (listp got))
                                            ;; Compare by keys and enabled flags
                                            (let ((orig-keys (sort (mapcar #'context-navigator-model-item-key items) #'string<))
                                                  (got-keys  (sort (mapcar #'context-navigator-model-item-key got) #'string<)))
                                              (should (equal orig-keys got-keys)))
                                            (should (equal (mapcar #'context-navigator-item-enabled (sort got (lambda (a b) (string< (context-navigator-model-item-key a)
                                                                                                                                (context-navigator-model-item-key b)))))
                                                           (mapcar #'context-navigator-item-enabled (sort items (lambda (a b) (string< (context-navigator-model-item-key a)
                                                                                                                                  (context-navigator-model-item-key b)))))))))))

(ert-deftest context-navigator-persist/migrate-wraps-v2-like-list ()
  (let* ((v2 '((:type file :path "a" :enabled t)
               (:type buffer :path "b" :enabled nil))))
    (let ((v3 (context-navigator-persist-migrate-if-needed v2)))
      (should (eq (plist-get v3 :version) 3))
      (should (plist-get v3 :items))
      (should (= (length (plist-get v3 :items)) 2)))))

(ert-deftest context-navigator-persist/load-missing-file-calls-callback-empty ()
  (context-navigator-test-with-temp-dir dir
                                        (let* ((root dir)
                                               (file (context-navigator-persist-context-file root)))
                                          (ignore file) ;; ensure path computation
                                          (let ((got 'unset))
                                            (context-navigator-persist-load-async root (lambda (items) (setq got items)))
                                            (context-navigator-test-wait 0.02)
                                            (should (equal got nil))))))

(ert-deftest context-navigator-persist/list-groups-includes-state-mapping ()
  (context-navigator-test-with-temp-dir dir
                                        (let* ((root dir)
                                               (context-navigator-dir-name ".context"))
                                          ;; Save state with mapping but do not create any group files
                                          (context-navigator-persist-state-save root '(:version 1 :current "foo" :groups (("foo" . "Имя"))))
                                          (let* ((lst (context-navigator-persist-list-groups root))
                                                 (slugs (mapcar (lambda (pl) (plist-get pl :slug)) lst)))
                                            (should (member "foo" slugs))
                                            (let ((cell (cl-find "foo" lst :key (lambda (pl) (plist-get pl :slug)) :test #'equal)))
                                              (should (equal (plist-get cell :display) "Имя"))
                                              ;; mtime may be nil if file missing; path should be a proper path under dir
                                              (should (string-match-p (regexp-quote (file-name-as-directory (expand-file-name ".context" root)))
                                                                      (plist-get cell :path))))))))

(ert-deftest context-navigator-persist/slugify-basic-rules ()
  (let* ((s1 (context-navigator-persist-slugify "MyGroup"))
         (s2 (context-navigator-persist-slugify "Группа-тест 1!"))
         (s3 (context-navigator-persist-slugify (make-string 120 ?a))))
    (should (equal s1 "mygroup"))
    ;; No whitespace, only allowed chars, and lowercase
    (should (not (string-match-p "[[:space:]]" s2)))
    (should (equal s2 (downcase s2)))
    ;; Length capped at 100
    (should (<= (length s3) 100))))

(provide 'context-navigator-persist-test)
;;; context-navigator-persist-test.el ends here
