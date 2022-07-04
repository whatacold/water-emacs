;;; company
(setq company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil ; keep candidates as-is
      company-minimum-prefix-length 2) ; e.g. 'fo' triggers company to start completion

(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(progn
     (company-statistics-mode) ; suggest candidates by stats

     (add-to-list 'company-backends #'company-c-headers)

     (when w/company-ispell-dict
       (setq company-ispell-dictionary (file-truename w/company-ispell-dict))
       (add-to-list 'company-backends #'company-ispell))

     (setq company-show-numbers t ; press M-number to choose candidate
           company-idle-delay 0.2
           company-require-match nil)))

;;; hippie-expand
(setq hippie-expand-try-functions-list
  '(try-complete-file-name-partially
    try-complete-file-name
    try-expand-all-abbrevs
    try-expand-list
    ;; try-expand-line
    try-expand-dabbrev
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-complete-lisp-symbol-partially
    try-complete-lisp-symbol))

;;; org-mode
(setq org-adapt-indentation nil) ; don't indent to outline node level

(defun w/org-mode-hook-setup ()
  (org-num-mode)

  (setq truncate-lines nil) ; display wrapped lines instead of truncated lines
  ;; (setq word-wrap t)
  )
(add-hook 'org-mode-hook #'w/org-mode-hook-setup)

;; (add-hook 'org-mode-hook #'valign-mode)

;;; yasnippet
(eval-after-load 'yasnippet
  '(yas-global-mode))

;; enable yasnippet based on the major mode
(dolist (hook '(prog-mode-hook
                text-mode-hook
                ;; below modes does NOT inherit from prog-mode
                cmake-mode-hook
                web-mode-hook
                scss-mode-hook))
  (add-hook hook #'yas-minor-mode))

;;; others
(require 'iedit) ; will bind C-; internally
(defalias #'cleanup-buffer #'whitespace-cleanup)

(defun w/insert-lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque sem mauris, aliquam vel interdum in, faucibus non libero. Asunt in anim uis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in anim id est laborum. Allamco laboris nisi ut aliquip ex ea commodo consequat."))
