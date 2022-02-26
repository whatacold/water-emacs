(load "~/.emacs.local.d/init-before" 'noerror)

;;;; early settings

;;; http proxy
(defcustom w/http-proxy nil
  "http proxy url without the scheme, e.g. 127.0.0.1:8000.")

(defcustom w/font-and-size "DejaVu Sans Mono-16"
  "The font and its size.")

;; For package.el
(when w/http-proxy
  (setq url-proxy-services `(("http" . ,w/http-proxy)
                             ("https" . ,w/http-proxy))))

;;;; tweak default settings

;;; separate session files
(setq recentf-save-file "~/.emacs.local.d/recentf")
(setq tramp-histfile-override "~/.emacs.local.d/.tramp_history")
(setq project-list-file "~/.emacs.local.d/projects")
(setq savehist-file "~/.emacs.local.d/history") ; minibuffer history
(setq bookmark-file "~/.emacs.local.d/bookmarks")
(setq company-statistics-file "~/.emacs.local.d/company-statistics-cache.el")
;; https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
(setq custom-file "~/.emacs.local.d/custom-set-variables.el")
(load custom-file 'noerror)

;;; UI
(load-theme 'leuven 'no-confirm)
;; font & font size
(add-to-list 'default-frame-alist
             `(font . ,w/font-and-size))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; packages
(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ))
(require 'package)
(package-initialize)

;;; install packages
(let (refreshed-p)
  (dolist (pkg '(s company company-statistics
                   scratch
                   exec-path-from-shell
                   ivy swiper counsel
                   yasnippet yasnippet-snippets
                   auto-yasnippet ; create live snippets and expand them
                   ivy-yasnippet
                   iedit
                   magit
                   quelpa
                   easy-kill

                   ;; UI
                   beacon ; for fun
                   cnfonts

                   ;; jumping around
                   avy ace-link ace-pinyin
                   ace-window

                   ;; reading
                   elfeed elfeed-org
                   hl-todo
                   highlight-symbol

                   ;; writing
                   easy-hugo
                   org-download

                   ;; programming
                   company-c-headers
                   paredit
                   tiny
                   cider
                   ggtags
                   hl-todo
                   emmet-mode

                   ;; others
                   expand-region
                   keycast
                   subed
                   ))
    (when (and (not (assoc pkg package-archive-contents))
               (not refreshed-p))
      (message "package %s not found in archive" pkg)
      (package-refresh-contents)
      (setq refreshed-p t))
    (unless (package-installed-p pkg)
      (package-install pkg))))

;;; others
;; don't auto-save and back up files
(setq auto-save-default nil
      make-backup-files nil)

;;;; customize built-in features

;;; settings
(setq history-length 8000
      savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
(savehist-mode)

;;;; key bindings
;;; built-in feature enhancements
(global-set-key (kbd "M-g g") #'avy-goto-line) ; goto-line
(global-set-key (kbd "C-x C-b") #'ibuffer) ; list-buffers
(global-set-key (kbd "M-/") #'hippie-expand) ; dabbrev-expand
(global-set-key (kbd "C-s") #'swiper) ; isearch
(global-set-key (kbd "C-x b") #'ivy-switch-buffer) ; switch-to-buffer
(global-set-key (kbd "M-x") #'counsel-M-x) ; execute-extended-command
(global-set-key (kbd "M-y") #'counsel-yank-pop) ; yank-pop
(global-set-key (kbd "C-h v") #'counsel-describe-variable)
(global-set-key (kbd "C-h f") #'counsel-describe-function)
(global-set-key (kbd "C-x o") #'ace-window) ; other-window

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

;;;; ending
(load "~/.emacs.local.d/init-after" 'noerror)
(message "have a nice day!")
