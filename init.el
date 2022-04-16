(load "~/.emacs.local.d/init-before" 'noerror)

;;;; early settings

;;; http proxy
(defcustom w/http-proxy nil
  "http proxy url without the scheme, e.g. 127.0.0.1:8000.")

(defcustom w/theme 'tsdh-light
  "The UI theme.")

(defcustom w/font-and-size "DejaVu Sans Mono-16"
  "The font and its size.")

(defcustom w/python-venv nil
  "The Python virtual environment.")

(defcustom w/yasnippet-dir "~/.emacs.local.d/snippets/"
  "Private yasnippet snippet dir.")

(defcustom w/company-ispell-dict nil
  "English dict file for company completion, one word per line.")

;; for package.el etc.
(when w/http-proxy
  (setq url-proxy-services `(("http" . ,w/http-proxy)
                             ("https" . ,w/http-proxy))))

;;;; tweak default settings

;;; separate session files
(unless (file-directory-p "~/.emacs.local.d/")
  (make-directory "~/.emacs.local.d/"))
(setq package-user-dir "~/.emacs.local.d/elpa")
(setq recentf-save-file "~/.emacs.local.d/recentf")
(setq tramp-histfile-override "~/.emacs.local.d/.tramp_history")
(setq project-list-file "~/.emacs.local.d/projects")
(setq savehist-file "~/.emacs.local.d/history") ; minibuffer history
(setq bookmark-file "~/.emacs.local.d/bookmarks")
(setq company-statistics-file "~/.emacs.local.d/company-statistics-cache.el")
(setq transient-history-file "~/.emacs.local.d/transient/history.el")
(setq auto-save-list-file-prefix "~/.emacs.local.d/auto-save-list/.saves-")
(setq smex-save-file "~/.emacs.local.d/.smex-items")
(setq eshell-history-file-name "~/.emacs.local.d/eshell/history")
(setq yas-snippet-dirs '(w/yasnippet-dir))
;; https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
(setq custom-file "~/.emacs.local.d/custom-set-variables.el")
(load custom-file 'noerror)

;;; UI
(load-theme w/theme 'no-confirm)
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
                   smex ivy swiper counsel ; ivy will use the command data of smex
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
                   delight ; for hiding specified minor modes; better that diminish

                   ;; jumping around
                   avy ace-link ace-pinyin
                   ace-window

                   ;; reading
                   elfeed elfeed-org
                   highlight-symbol

                   ;; writing
                   easy-hugo
                   org-download
                   valign               ; pixel-wise visual alignment of tables
		   markdown-mode
                   wgrep

                   ;; programming
                   company-c-headers
                   paredit
                   tiny
                   cider
                   ggtags
                   hl-todo
                   emmet-mode ; html
		   php-mode
		   csv-mode
		   eglot
		   pyvenv
		   go-mode
                   protobuf-mode
                   yaml-mode

                   ;; misc
                   expand-region
                   keycast
                   subed
		   which-key
                   docker
                   ))
    (when (and (not (assoc pkg package-archive-contents))
               (not refreshed-p))
      (message "package %s not found in archive" pkg)
      (package-refresh-contents)
      (setq refreshed-p t))
    (unless (package-installed-p pkg)
      (package-install pkg))))

;;; others
(fset 'yes-or-no-p 'y-or-n-p)
;; don't auto-save and back up files
(setq auto-save-default nil
      make-backup-files nil)

;;;; customize built-in features

;;; settings
(setq history-length 8000
      history-delete-duplicates t
      savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
(savehist-mode)

;;;; customize packages
(setq csv-align-max-width 100)

;;;; loading other settings
(let ((dir "~/.emacs.d/lisp"))
  (when (file-directory-p dir)
    (dolist (file (directory-files dir))
      (when (string-match-p "^[[:digit:]]+-.*\\.el$" file)
	(message "loading %s" (expand-file-name file dir))
	(load-file (expand-file-name file dir))))))

;;;; ending
(load "~/.emacs.local.d/init-after" 'noerror)
(message "have a nice day!")
