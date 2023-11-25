;;; basics
(require 's) ; for e.g. writing query-replace-regexp expressions

;;; programming modes
(add-hook 'prog-mode-hook (lambda ()
                            ;; C-x f to set the fill-column
                            (display-fill-column-indicator-mode)))

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
;; wechatapp project files
(add-to-list 'auto-mode-alist '("\\.wxss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.wxml\\'" . web-mode))

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

(defvar w/join-lines--last-separator ","
  "Keep the last used separator for `w/join-lines', a comma by default.")

(defun w/join-lines (&optional specify-separator)
  "Join lines in the active region by a separator, by default the last used.
Specify the separator by typing C-u before executing this command.

Note: it depends on s.el."
  (interactive "P")
  (require 's)
  (unless (region-active-p)
    (error "select a region of lines first."))
  (let* ((separator (if (not specify-separator)
                        w/join-lines--last-separator
                      (read-string "Separator: ")))
         (text (buffer-substring-no-properties
               (region-beginning)
               (region-end)))
         (lines (split-string text "\n"))
         (result (s-join separator lines)))
    (delete-region (region-beginning) (region-end))
    (insert result)
    (setq w/join-lines--last-separator separator)))

(defun w/join-every-n-lines (&optional specify-separator)
  "Join every N lines in the active region by a separator,
by default the last used.

Specify the separator by typing C-u before executing this
command.

Note: it depends on s.el."
  (interactive "P")
  (require 's)
  (unless (region-active-p)
    (error "select a region of all-lines first."))
  (let* ((n (string-to-number (read-string "N =: ")))
         (separator (if (not specify-separator)
                        w/join-lines--last-separator
                      (read-string "Separator: ")))
         (text (buffer-substring-no-properties
               (region-beginning)
               (region-end)))
         (all-lines (split-string text "\n"))
         n-lines
         result)
    (while all-lines
      (let (lines line)
        (dotimes (_ n)
          (when (setq line (pop all-lines))
            (push line lines)))
        (push (reverse lines) n-lines)))
    (setq result (mapconcat (lambda (lines)
                              (s-join separator lines))
                            (reverse n-lines) "\n"))
    (delete-region (region-beginning) (region-end))
    (insert result)
    (setq w/join-lines--last-separator separator)))
