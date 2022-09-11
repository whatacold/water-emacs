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
(setq-default org-download-image-dir "~/org/images/"
              org-download-heading-lvl nil)
(setq org-download-screenshot-method (if (eq system-type 'cygwin)
                                         "convert clipboard: %s"
                                       "gnome-screenshot -a -f %s"))

(eval-after-load 'org
  '(progn
     (require 'org-download)))

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

;;; smart-input-source
(sis-ism-lazyman-config "xkb:us::eng" "rime" 'ibus)

(defun w/sis--guess-context-by-prev-chars (backward-chars forward-chars)
  "Detect the context based on the 2 chars before the point.

It has a side effect of deleting the previous whitespace if
there is a whitespace/newline and a comma before the point."
  (when (and (>= (point) 3)
             sis-context-mode
             (memq major-mode '(org-mode)))
    (let ((prev (preceding-char))
          (pprev (char-before (1- (point)))))
      (cond
       ((and (or (char-equal ?  pprev) (char-equal 10 pprev)) ; a whitespace or newline
             (char-equal ?, prev))
        (delete-char -1)                ; side effect: delete the second whitespace
        'other)
       ((string-match-p "[[:ascii:]]" (char-to-string (preceding-char)))
        'english)
       (t 'other)))))

(setq sis-context-detectors '(w/sis--guess-context-by-prev-chars))

(setq sis-context-hooks '(post-command-hook)) ; may hurt performance

(sis-global-respect-mode t)
(sis-global-context-mode t)

;;; others
(require 'iedit) ; will bind C-; internally
(defalias #'cleanup-buffer #'whitespace-cleanup)

;; beancount
(add-to-list 'auto-mode-alist '("\\.beancount$" . beancount-mode))
(setq beancount-use-ido nil)
(add-to-list 'auto-mode-alist '("\\.bean$" . beancount-mode))
;; beancount dirty hacks
(defvar beancount-account-files nil
  "List of account files")

(defun w/beancount--collect-accounts-from-files (oldfun regex n)
  (let ((keys (funcall oldfun regex n))
        (hash (make-hash-table :test 'equal)))
    (dolist (key keys)
      (puthash key nil hash))
    ;; collect accounts from files
    (save-excursion
      (dolist (f beancount-account-files)
        (with-current-buffer (find-file-noselect f)
          (goto-char (point-min))
          (while (re-search-forward beancount-account-regexp nil t)
            (puthash (match-string-no-properties n) nil hash)))))
    (hash-table-keys hash)))

(advice-add #'beancount-collect
            :around #'w/beancount--collect-accounts-from-files
            '((name . "collect accounts from files as well")))

(defun w/insert-lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque sem mauris, aliquam vel interdum in, faucibus non libero. Asunt in anim uis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in anim id est laborum. Allamco laboris nisi ut aliquip ex ea commodo consequat."))
