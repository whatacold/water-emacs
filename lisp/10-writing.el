;;;; company
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(progn
     (company-statistics-mode) ; suggest candidates by stats

     (add-to-list 'company-backends 'company-c-headers)

     (setq company-show-numbers t ; press M-number to choose candidate
           company-idle-delay 0.2
           company-require-match nil)))


;;; org-mode
(setq org-adapt-indentation nil)
