(load "~/.emacs.local.d/init-before" 'noerror)

;;;; early settings
;;; helpers
(defun w/terminal-p ()
  (not (display-graphic-p)))

(defalias 'w/gui-p #'display-graphic-p)

(defun w/windows-p()
  (eq system-type 'windows-nt))

(defun w/linux-p()
  (eq system-type 'gnu/linux))

(defun w/macos-p()
  (eq system-type 'darwin))

;;; http proxy
(defcustom w/http-proxy nil
  "http proxy url without the scheme, e.g. 127.0.0.1:8000.")

(defcustom w/theme 'tsdh-light
  "The UI theme.")

(defcustom w/font-size "16"
  "The font size.")

(defcustom w/default-font (cond
                           ((w/windows-p) ; Microsoft Windows
                            (when (member "Consolas" (font-family-list))
                              "Consolas"))
                           ((w/macos-p) ; macOS
                            (when (member "Menlo" (font-family-list))
                              "Menlo"))
                           ((w/linux-p) ; linux
                            (when (member "DejaVu Sans Mono" (font-family-list))
                              "DejaVu Sans Mono")))
  "The default font.")

(defcustom w/chinese-font (cond
                           ((w/windows-p)
                            (cond
                             ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                             ((member "Microsoft JhengHei" (font-family-list)) "Microsoft JhengHei")
                             ((member "SimHei" (font-family-list)) "SimHei")))
                           ((w/macos-p)
                            (cond
                             ((member "Hei" (font-family-list)) "Hei")
                             ((member "Heiti SC" (font-family-list)) "Heiti SC")
                             ((member "Heiti TC" (font-family-list)) "Heiti TC")))
                           ((w/linux-p)
                            (cond
                             ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei"))))
  "Chinese font.")

(defcustom w/symbol-font (cond
                          ((w/windows-p)
                           (cond
                            ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")))
                          ((w/macos-p)
                           (cond
                            ((member "Apple Symbols" (font-family-list)) "Apple Symbols")))
                          ((w/linux-p)
                           (cond
                            ((member "Symbola" (font-family-list)) "Symbola"))))
  "Symbol font, excluding emoji.")

(defcustom w/emoji-font (cond
                         ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
                         ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
                         ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
                         ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
                         ((member "Symbola" (font-family-list)) "Symbola"))
  "Emoji font.")

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

(defun w/toggle-proxy ()
  (interactive)
  (when w/http-proxy
    (setq url-proxy-services (if url-proxy-services
                                 nil
                               `(("http" . ,w/http-proxy)
                                 ("https" . ,w/http-proxy))))))

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
(setq image-dired-dir "~/.emacs.local.d/image-dired/")
(setq yas-snippet-dirs '(w/yasnippet-dir))
;; https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
(setq custom-file "~/.emacs.local.d/custom-set-variables.el")
(load custom-file 'noerror)

;;; UI
;; font & font size
;; http://xahlee.info/emacs/emacs/emacs_list_and_set_font.html
(defun w/set-default-font-size (font-size)
  (interactive "nFont size: ")
  (set-frame-font (format "%s-%s" w/default-font font-size) t t))

(w/set-default-font-size w/font-size)

(when (w/gui-p)
  (set-fontset-font t 'symbol w/symbol-font)
  (set-fontset-font t
                    ;; set font for emoji
                    ;; (if before emacs 28, should come after setting symbols.
                    ;; emacs 28 now has 'emoji . before, emoji is part of 'symbol)
                    (if (version< emacs-version "28.1")
                        '(#x1f300 . #x1fad0)
                      'emoji)
                    w/emoji-font)
  (set-fontset-font t 'han w/chinese-font))

(menu-bar-mode -1)
(when (w/gui-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defcustom w/package-archives '(
                                ("gnu" . "https://elpa.gnu.org/packages/")
                                ("melpa" . "https://melpa.org/packages/")
                                ("melpa-stable" . "http://stable.melpa.org/packages/")
                                ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                                ;; ("org" . "https://orgmode.org/elpa/")
                                )
  "default package archives.")

;;; packages
(setq package-archives w/package-archives)
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
                   hydra

                   ;; UI
                   beacon ; for fun
                   cnfonts
                   delight ; for hiding specified minor modes; better than diminish

                   ;; jumping around
                   avy ivy-avy ace-link ace-pinyin
                   ace-window

                   ;; reading
                   elfeed elfeed-org
                   highlight-symbol
                   outline-toc          ; for markdown, org, and outline modes
                   org-present

                   ;; writing
                   easy-hugo
                   org-download
                   ox-reveal
                   htmlize
                   ;; org-contrib
                   valign               ; pixel-wise visual alignment of tables
		   markdown-mode
                   wgrep
                   auctex
                   ;; sis                  ; smart input source

                   ;; programming
                   company-c-headers
                   paredit
                   tiny
                   cider
                   gtags-mode
                   hl-todo
                   typescript-mode
                   emmet-mode ; html
                   web-mode
		   php-mode
		   csv-mode
		   eglot
		   pyvenv
		   go-mode
                   protobuf-mode
                   yaml-mode

                   ;; misc
                   quelpa
                   expand-region
                   keycast
                   subed
		   which-key
                   docker
                   battery-notifier
                   org-anki
                   ))
    (when (and (not (assoc pkg package-archive-contents))
               (not refreshed-p))
      (message "package %s not found in archive" pkg)
      (package-refresh-contents)
      (setq refreshed-p t))
    (unless (package-installed-p pkg)
      (package-install pkg))))

;;;; install packages via quelpa
(unless (package-installed-p 'beancount)
  (quelpa '(beancount :repo "beancount/beancount-mode" :fetcher github)))

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
