(defun clojure-test-for-style (namespace pathf style)
  "paramaterized version of clojure-test-for from clojure-mode.el"
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (funcall pathf (split-string namespace "\\."))))
    (format style
            (file-name-as-directory
             (locate-dominating-file buffer-file-name "src/"))
            (mapconcat 'identity segments "/"))))

(defun clojure-test-for (namespace)
  "hijacking from clojure-mode.el, I can't figure out advice arguments"
  (let ((new (clojure-test-for-style namespace 'identity "%stest/%s_test.clj")))
    (if (file-exists-p new)
        new
      (let ((old (clojure-test-for-style
                  namespace
                  (lambda (p)
                    (if (not (cdr p))
                        (cons "test" p)
                      (cons (car p)
                            (cons "test" (cdr p)))))
                  "%stest/%s.clj")))
        (if (file-exists-p old)
            old
          new)))))

(defun clojure-lint-file ()
  (interactive)
  (shell-command (concat "joker --lint " buffer-file-truename)))

(provide 'rc-clojure)
