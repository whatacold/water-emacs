;;; basics
(require 's) ; for e.g. writing query-replace-regexp expressions

;;; project
(defun w/project-try-local (dir)
  "Determine if DIR is a non-Git project."
  (catch 'ret
    (let ((pr-flags '((".emacs-project")
                      ("eglot-project.txt" "go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
                      ("Makefile" "README.org" "README.md"))))
      (dolist (current-level pr-flags)
        (dolist (f current-level)
          (when-let ((root (locate-dominating-file dir f)))
            (throw 'ret (cons 'transient root))))))))

(setq project-find-functions '(w/project-try-local project-try-vc))

;;; eglot
(setq eldoc-echo-area-use-multiline-p nil)

;;; python
(when w/python-venv
  (pyvenv-activate w/python-venv))

;;; modes
(electric-pair-mode)
(show-paren-mode)

