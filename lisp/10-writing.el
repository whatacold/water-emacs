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

     (setq company-show-numbers t ; so that we can press M-number to choose a candidate
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
(setq org-adapt-indentation nil ; don't indent to outline node level
      org-blank-before-new-entry nil    ; https://emacs.stackexchange.com/questions/14629/org-mode-level-line-spacing
      org-startup-folded t
      org-edit-src-content-indentation 0)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (emacs-lisp . t)))
(setq-default org-download-image-dir "~/org/images/"
              org-download-heading-lvl nil)
(setq org-download-screenshot-method (if (eq system-type 'cygwin)
                                         "convert clipboard: %s"
                                       "gnome-screenshot -a -f %s"))

(eval-after-load 'org
  '(progn
     (require 'ol-man)                  ; link manpages from org
     (require 'org-download)))

(defun w/org-mode-hook-setup ()
  ;; (org-num-mode)
  (setq line-spacing 0.2)               ; https://wilkesley.org/~ian/xah/emacs/emacs_toggle_line_spacing.html
  (setq truncate-lines nil) ; display wrapped lines instead of truncated lines
  ;; (setq word-wrap t)
  )
(add-hook 'org-mode-hook #'w/org-mode-hook-setup)

(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
      org-reveal-theme "white")

;;; Emphasize content by dragging the mouse
(define-advice mouse-set-region (:after (click) org-highlight ())
  (when (and (derived-mode-p 'org-mode)
             (use-region-p))
    (let ((origin (buffer-substring (region-beginning) (region-end)))
          (emphasis-char "*"))
      (delete-region (region-beginning) (region-end))
      (insert emphasis-char origin emphasis-char))))
(defun w/org-anki-sync ()
  "Sync anki to the web."
  (interactive)
  (org-anki-connect-request '(("action" . "sync"))
                            (lambda (result) (message "org anki sync succ"))
                            nil))

(run-with-idle-timer 120 'repeat #'w/org-anki-sync)

;; (add-hook 'org-mode-hook #'valign-mode)

;;; hugo blogging
(defun w/hugo--collect-tags ()
  "Collect hugo tags from the org files in the current dir.

Note that it only extracts tags from lines like the below:
#+tags[]: Emacs Org-mode"
  (interactive)
  (let ((files (directory-files-recursively default-directory "\\.org$")))
    (let ((source (with-temp-buffer
		    (while files
                      (when (file-exists-p (car files))
                        (insert-file-contents (car files)))
		      (pop files))
		    (buffer-string))))
      (save-match-data
	(let ((pos 0)
	      matches)
	  (while (string-match "^#\\+[Tt]ags\\[\\]: \\(.+?\\)$" source pos)
	    (push (match-string 1 source) matches)
	    (setq pos (match-end 0)))
          (sort
	   (delete-dups
	    (delete "" (split-string
			(replace-regexp-in-string "[\"\']" " "
						  (replace-regexp-in-string
						   "[,()]" ""
						   (format "%s" matches)))
			" ")))
           (lambda (a b)
             (string< (downcase a) (downcase b)))))))))

(defun w/hugo-select-tags ()
  "Select tags for the current hugo post."
  (interactive)
  (ivy-read "Insert tags: "
            (w/hugo--collect-tags)
            :action
            (lambda (tag)
              (insert (if (char-equal (preceding-char) 32)
                          ""
                        " ")
                      tag))))

(defun w/hugo-update-lastmod ()
  "Update the `lastmod' value for a hugo org-mode buffer."
  (interactive)
  (ignore-errors
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (search-forward-regexp "^#\\+lastmod: ")
        (delete-region (point) (line-end-position))
        (insert (w/hugo-current-time))))))

(defun w/hugo-current-time ()
  "Get timestamp for hugo."
  (let ((tz (format-time-string "%z")))
    (insert (format-time-string "%Y-%m-%dT%T")
            (substring tz 0 3) ":" (substring tz 3 5))))

(add-hook 'before-save-hook #'w/hugo-update-lastmod)

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
(when (w/gui-p)
  (cond
   ((w/windows-p)
    (sis-ism-lazyman-config nil t 'w32))
   ((w/macos-p)
    (sis-ism-lazyman-config "com.apple.keylayout.US" "com.sogou.inputmethod.sogou.pinyin"))
   ((w/linux-p)
    (sis-ism-lazyman-config "xkb:us::eng" "rime" 'ibus))))

(defun w/sis--guess-context-by-prev-chars (backward-chars forward-chars)
  "Detect the context based on the 2 chars before the point.

It has a side effect of deleting the previous whitespace if
there is a whitespace/newline and a comma before the point."
  (when (and (>= (point) 3)
             sis-context-mode)
    (let ((prev (preceding-char))
          (pprev (char-before (1- (point)))))
      (cond
       ((and (or (char-equal ?  pprev) (char-equal 10 pprev)) ; a whitespace or newline
             (char-equal ?, prev))
        (delete-char -1)                ; side effect
        'other)
       ((string-match-p "[[:ascii:]]" (char-to-string (preceding-char)))
        'english)
       (t 'other)))))

(setq sis-context-detectors '(w/sis--guess-context-by-prev-chars))

(setq sis-context-hooks '(post-command-hook)) ; may hurt performance

(when (w/gui-p)
  (sis-global-respect-mode t)
  (sis-global-context-mode t))

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
