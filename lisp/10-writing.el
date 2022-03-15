;;; company
(setq company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil ; keep candidates as-is
      company-minimum-prefix-length 2) ; e.g. 'fo' triggers company to start completion

(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(progn
     (company-statistics-mode) ; suggest candidates by stats

     (add-to-list 'company-backends 'company-c-headers)

     (setq company-show-numbers t ; press M-number to choose candidate
           company-idle-delay 0.2
           company-require-match nil)))


;;; org-mode
(setq org-adapt-indentation nil) ; don't indent to outline node level

(defun w/org-mode-hook-setup ()
  (org-num-mode)

  (setq truncate-lines nil) ; display wrapped lines instead of truncated lines
  ;; (setq word-wrap t)
  )
(add-hook 'org-mode-hook #'w/org-mode-hook-setup)

;;; others
(require 'iedit) ; will bind C-; internally
(defalias #'cleanup-buffer #'whitespace-cleanup)
