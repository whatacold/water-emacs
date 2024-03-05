;;; company for completion
(setq company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil ; keep candidates as-is
      ;; The dict is stolen from Chen Bin's emacs.d repo
      company-ispell-dictionary (expand-file-name
                                 "~/.emacs.d/data/misc/english-words.txt")
      company-show-numbers t ; so that we can press M-number to choose a candidate
      company-require-match nil
      company-transformers '(company-sort-prefer-same-case-prefix)
      company-backends '(company-bbdb
                         company-capf
                         company-files
                         (company-dabbrev company-ispell))
      company-idle-delay 0.8            ; beware company-ispell may make it laggy
      company-minimum-prefix-length 2) ; e.g. 'fo' triggers company to start completion

(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(progn
     (company-statistics-mode) ; suggest candidates based on stats
     ;; HACK put company-sort-prefer-same-case-prefix at last, so as to
     ;; make the same prefix candidates show first
     (progn
       (setq company-transformers (delq #'company-sort-prefer-same-case-prefix
                                        company-transformers))
       (add-to-list 'company-transformers
                    #'company-sort-prefer-same-case-prefix 'append))))

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
      org-startup-folded 'showeverything ; better with desktop
      org-src-window-setup 'current-window ; use the current window to edit the src block
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

;; ;;; Emphasize content by dragging the mouse
;; (define-advice mouse-set-region (:after (click) org-highlight ())
;;   (when (and (derived-mode-p 'org-mode)
;;              (use-region-p))
;;     (let ((origin (buffer-substring (region-beginning) (region-end)))
;;           (emphasis-char "*"))
;;       (delete-region (region-beginning) (region-end))
;;       (insert emphasis-char origin emphasis-char))))

;;; org-anki
;; skip empty headings
(setq org-anki-skip-function
      (lambda ()
        (when (string-blank-p (substring-no-properties (org-get-entry)))
          (org-element-property :end (org-element-at-point)))))
(defun w/org-anki-sync-subentries ()
  "Sync all subentries of the current entry at point.

Also respect `org-anki-skip-function'."
  (interactive)
  (save-excursion
    (let* ((next-level (1+ (org-current-level)))
           (notes (org-map-entries (lambda ()
                          (when (and t
                                     (= next-level (org-element-property :level (org-element-at-point))))
                            (org-anki--note-at-point)
                            ))
                        nil
                        'tree
                        org-anki-skip-function)))
      (org-anki--sync-notes (seq-filter (lambda (note) note)
                                        notes)))))

(defun w/org-anki-sync ()
  "Sync anki to the web."
  (interactive)
  (org-anki-connect-request '(("action" . "sync"))
                            (lambda (result) (message "org anki sync succ"))
                            nil))

;;; mpv
(defun org-mpv-complete-link (&optional arg)
  (replace-regexp-in-string
   "file:" "mpv:"
   (org-file-complete-link arg)
   t t))
(org-link-set-parameters "mpv"
                         :follow #'mpv-play
                         :complete #'org-mpv-complete-link)
;; below doesn't work
;; (add-hook 'org-open-at-point-functions #'mpv-seek-to-position-at-point)


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

(require 'ivy-yasnippet)
(defvar ivy-yasnippet--history nil)
;; HACK better to use :preselect, so that if the default is not
;; something you want, you dont need to delete them at first.  Also
;; consider this senario, the user type foo for the current session,
;; and select foob from two candidates fooa and foob, we should handle
;; it correctly so that foob is preselected next time.
(defun ivy-yasnippet ()
  "Read a snippet name from the minibuffer and expand it at point.
The completion is done using `ivy-read'.

In the minibuffer, each time selection changes, the selected
snippet is temporarily expanded at point for preview.

If text before point matches snippet key of any candidate, that
candidate will be initially selected, unless variable
`ivy-yasnippet-expand-keys' is set to nil."
  (interactive)
  (barf-if-buffer-read-only)
  (unless yas-minor-mode
    (error "`yas-minor-mode' not enabled in current buffer"))
  (let* ((ivy-yasnippet--buffer (current-buffer))

	 (ivy-yasnippet--region
	  (if (region-active-p)
	      (cons (region-beginning) (region-end))
	    (cons (point) (point))))
	 (ivy-yasnippet--region-contents
	  (buffer-substring (car ivy-yasnippet--region)
			    (cdr ivy-yasnippet--region)))

	 (key-info (yas--templates-for-key-at-point))
	 (ivy-yasnippet--key
	  (and key-info
	       (buffer-substring (cadr key-info) (cl-caddr key-info))))
	 (templates-for-key-at-point (mapcar #'cdr (car key-info)))
	 (ivy-yasnippet--key-deleted nil)
	 (ivy-yasnippet--should-delete-key
	  (memq ivy-yasnippet-expand-keys '(always smart)))

	 (ivy-yasnippet--template-alist
	  (mapcar (lambda (template)
		    (cons (yas--template-name template) template))
		  (yas--all-templates (yas--get-snippet-tables))))

	 (modified-flag (buffer-modified-p))

	 candidates selection)
    (let ((buffer-undo-list t))
      (setq candidates
	    (-map #'car
		  (-flatten
		   (-map (-partial #'-sort (lambda (a b)
					     (string-lessp (car a) (car b))))
			 (-separate
			  (-lambda ((_ . template))
			    (memq template templates-for-key-at-point))
			  ivy-yasnippet--template-alist)))))

      (unwind-protect
	  (let ((buffer-read-only t))
	    (ivy-read "Choose a snippet: " candidates
		      :require-match
		      (not ivy-yasnippet-create-snippet-if-not-matched)
                      :history 'ivy-yasnippet--history
                      :initial-input (when ivy-yasnippet--history
                                       (car ivy-yasnippet--history))
		      :update-fn #'ivy-yasnippet--update-fn
		      :action (lambda (candidate) (setq selection candidate))
		      :preselect
		      (when ivy-yasnippet--key
			(-find-index
			 (lambda (x)
			   (string-equal
			    ivy-yasnippet--key
			    (yas--template-key
			     (ivy-yasnippet--lookup-template x))))
			 candidates))
		      :caller 'ivy-yasnippet))
	(ivy-yasnippet--revert)
	(set-buffer-modified-p modified-flag)))
    (when selection
      (let ((template (ivy-yasnippet--lookup-template selection)))
	(if template
	    (ivy-yasnippet--expand-template template)
	  (setq template ivy-yasnippet-new-snippet)
	  (yas-new-snippet t)
	  (when (derived-mode-p 'snippet-mode)
	    (yas-expand-snippet
	     template nil nil
	     `((name ,selection)
	       (yas-selected-text
		,ivy-yasnippet--region-contents)))))))))

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

;; FIXME make respect mode for C-x b not work
;; (setq sis-context-detectors '(w/sis--guess-context-by-prev-chars))
;; (setq sis-context-hooks '(post-command-hook)) ; may hurt performance

;; (when (w/gui-p)
;;   (sis-global-respect-mode t)
;;   (sis-global-context-mode t))

;;; others
(require 'iedit) ; will bind C-; internally
(defalias #'cleanup-buffer #'whitespace-cleanup)

(defun w/insert-lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque sem mauris, aliquam vel interdum in, faucibus non libero. Asunt in anim uis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in anim id est laborum. Allamco laboris nisi ut aliquip ex ea commodo consequat."))

(defun w/insert-xhs-emoji ()
  "Insert an xhs (小红书) specific emoji."
  (interactive)
  (ivy-read "Pick an xhs emoji: "
            ;; the conses are initials for pinyin and shuangpin
            '(("wx [微笑R]" . "[微笑R]")
              ("hx [害羞R]" . "[害羞R]")
              ("sw uw [失望R]" . "[失望R]")
              ("hy [汗颜R]" . "[汗颜R]")
              ("wa [哇R]" . "[哇R]")
              ("hnc [喝奶茶R]" . "[喝奶茶R]")
              ("zp [自拍R]" . "[自拍R]")
              ("tx [偷笑R]" . "[偷笑R]")
              ("fw [飞吻R]" . "[飞吻R]")
              ("sh uh [石化R]" . "[石化R]")
              ("xk [笑哭R]" . "[笑哭R]")
              ("z [赞R]" . "[赞R]")
              ("azgc avgi [暗中观察R]" . "[暗中观察R]")
              ("mb [买爆R]" . "[买爆R]")
              ("ss [色色R]" . "[色色R]")
              ("sq [生气R]" . "[生气R]")
              ("rk [哭惹R]" . "[哭惹R]")
              ("mmd [萌萌哒R]" . "[萌萌哒R]")
              ("xy [斜眼R]" . "[斜眼R]")
              ("kl [可怜R]" . "[可怜R]")
              ("bs bu [鄙视R]" . "[鄙视R]")
              ("zm [皱眉R]" . "[皱眉R]")
              ("zk vk [抓狂R]" . "[抓狂R]")
              ("wl [捂脸R]" . "[捂脸R]")
              ("pd [派对R]" . "[派对R]")
              ("bj [吧唧R]" . "[吧唧R]")
              ("jk [惊恐R]" . "[惊恐R]")
              ("zj [再见R]" . "[再见R]")
              ("tq [叹气R]" . "[叹气R]")
              ("sj uj [睡觉R]" . "[睡觉R]")
              ("dy [得意R]" . "[得意R]")
              ("cg ig [吃瓜R]" . "[吃瓜R]")
              ("fq [扶墙R]" . "[扶墙R]")
              ("hjs hju [黄金薯R]" . "[黄金薯R]")
              ("tst tuz [吐舌头H]" . "[吐舌头H]")
              ("cl il [扯脸H]" . "[扯脸H]")
              ("[doge]" . "[doge]")
              ("hs hu [红书R]" . "[红书R]")
              ("qwm [请文明R]" . "[请文明R]")
              ("qyh [请友好R]" . "[请友好R]")
              ("bq [抱拳R]" . "[抱拳R]")
              ("dz [点赞R]" . "[点赞R]"))
            :require-match t
            :action (lambda (pair)
                      (insert (cdr pair)))))
