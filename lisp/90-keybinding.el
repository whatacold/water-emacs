;;;; key bindings
;;; built-in feature enhancements
(global-set-key (kbd "C-h f") #'counsel-describe-function)
(global-set-key (kbd "C-h v") #'counsel-describe-variable)
(global-set-key (kbd "C-s") #'swiper) ; isearch
(global-set-key (kbd "C-x C-b") #'ibuffer) ; list-buffers
(global-set-key (kbd "C-x b") #'ivy-switch-buffer) ; switch-to-buffer
(global-set-key (kbd "C-x C-f") #'counsel-find-file) ; find-file
(global-set-key (kbd "C-x o") #'ace-window) ; other-window
(global-set-key (kbd "M-/") #'hippie-expand) ; dabbrev-expand
(global-set-key (kbd "M-g g") #'avy-goto-line) ; goto-line
(global-set-key (kbd "M-x") #'counsel-M-x) ; execute-extended-command
(global-set-key (kbd "M-y") #'counsel-yank-pop) ; yank-pop

;;; other established & non C-c key bindings
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "C-c C-r") #'ivy-resume)

;;; key bindings following the convention
(global-set-key (kbd "C-c c") #'set-mark-command) ; for MS-Windows
(global-set-key [f8] #'compile)

;; searching
(global-set-key (kbd "C-c s i") #'counsel-rg)

;; jumping around
(global-set-key (kbd "C-c j c") #'avy-goto-char)
(global-set-key (kbd "C-c j o") #'ace-link)
(global-set-key (kbd "C-c j m") #'counsel-imenu)
(global-set-key (kbd "C-c j r") #'counsel-recentf)
(global-set-key (kbd "C-c j p") #'flymake-goto-prev-error)
(global-set-key (kbd "C-c j n") #'flymake-goto-next-error)

;; org-mode
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o l") #'org-store-link)
