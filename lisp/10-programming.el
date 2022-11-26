;;; basics
(require 's) ; for e.g. writing query-replace-regexp expressions

;;; project
(defun w/project-try-local (dir)
  "Determine if DIR is a non-Git project."
  (catch 'ret
    (let ((pr-flags '((".emacs-project" ".git")
                      ("go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
                      )))
      (dolist (current-level pr-flags)
        (dolist (f current-level)
          (when-let ((root (locate-dominating-file dir f)))
            (throw 'ret (cons 'transient root))))))))

(setq project-find-functions '(w/project-try-local project-try-vc))

(defun w/project-rg (&optional specify-project-p)
  "Grep files using `rg' in the current project.

If `specify-project-p' is non-nil, prompt users to select a project."
  (interactive "P")
  (let* ((pr (project-current t))
         (root (if specify-project-p
                   (project-prompt-project-dir)
                 (project-root pr))))
    (counsel-rg "" root nil (format "Grep files in %s: " root))))

;;; web-mode
(require 'web-mode)
(setq web-mode-markup-indent-offset 2
      web-mode-code-indent-offset 2
      ;; M-x web-mode-set-content-type
      web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

;;; eglot
(setq eldoc-echo-area-use-multiline-p nil)

;;; C/C++
;; basically,
;; - etags, the one emacs supports by default, but lacks features
;; - ctags, used by vim
;; - gtags, generated by `gtags' and used by `global'
;; https://www.reddit.com/r/emacs/comments/3pni17/ctags_etags_or_gtags/
(gtags-mode)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              )))

;;; python
(when w/python-venv
  (pyvenv-activate w/python-venv))

;;; modes
(electric-pair-mode)
(show-paren-mode)

;;; elisp debugging
(defvar w/trace-subprocess-p nil
  "Whether to trace subprocess creating or not.")

(defun w/toggle-trace-subprocess ()
  "Toggle whether to trace subprocess creating."
  (interactive)
  (setq w/trace-subprocess-p (not w/trace-subprocess-p))
  (when w/trace-subprocess-p
    (message "Trace subprocess creating.")))

(defun w/quote-argument (arg)
  "Double quote ARG."
  (concat "\"" arg "\""))

(define-advice start-process (:before (name buffer program &rest program-args) trace)
  (when w/trace-subprocess-p
    (message "Trace start-process: name: %s, buffer: %s, program and args: %s %s"
             name buffer program
             (mapconcat #'w/quote-argument program-args " "))))

(define-advice shell-command-to-string (:before (command) trace)
  (when w/trace-subprocess-p
    (message "Trace shell-command-to-string: %s" command)))

(define-advice call-process (:before (program &optional infile destination display &rest args) trace)
  (when w/trace-subprocess-p
    (message "Trace call-process: %s %s"
             program
             (mapconcat #'w/quote-argument args " "))))

;;; misc
(defun w/urxvt ()
  "Open a new urxvt terminal based on `default-directory' of the current buffer."
  (interactive)
  (let ((urxvt "urxvt256c-ml"))
    (start-process urxvt nil urxvt "-cd" (expand-file-name "./"))))
