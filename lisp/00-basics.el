;;; basics
;; enable these
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq initial-scratch-message nil)
(setq kill-ring-max 1000)
(setq-default indent-tabs-mode nil) ; no TAB
;; https://www.reddit.com/r/emacs/comments/fts7ap/command_attempted_to_use_minibuffer_while_in/
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; auto save files
;; (defvar w/auto-save--timer nil)
;;
;; (defun w/auto-save-enable ()
;;     "Save all unsaved filed when being idle."
;;   (interactive)
;;   (w/auto-save-disable)
;;   (setq w/auto-save--timer
;; 	(run-with-idle-timer
;; 	 3
;; 	 'repeat
;; 	 (lambda ()
;; 	   (save-some-buffers t)))))
;;
;; (defun w/auto-save-disable ()
;;     "Disable auto-saving files."
;;   (interactive)
;;   (when w/auto-save--timer
;;     (cancel-timer w/auto-save--timer)))
;;
;; (w/auto-save-enable)

;; built-in auto save files
(setq auto-save-visited-interval 2)
(auto-save-visited-mode)

(setq set-mark-command-repeat-pop t) ; C-u SPC SPC goes to the last last mark

;;; coding system
(prefer-coding-system 'chinese-gbk)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "\\.sh\\'" 'unix)

;;; windows
;; ace-window
(setq aw-scope 'frame)

(advice-add #'ace-window :around
            (lambda (oldfun &rest arg)
              "Prefixed with three C-u's, kill the buffer of that window."
              (if (/= 64 (car arg))
                  (apply oldfun arg)
                (save-window-excursion
                  (apply oldfun arg)
                  (kill-buffer))))
            '((name . kill-buffer)))

;;; mode line
;; not show vc info
(setq vc-handled-backends nil) ; so that `vc-mode' is nil for any buffer

;; hide minor modes from the mode line
(require 'delight)
(delight '((ivy-mode nil "ivy")
           (eldoc-mode nil "eldoc")
           (which-key-mode nil "which-key")
           (yas-minor-mode nil "yasnippet")
           (company-mode nil "company")
           (auto-revert-mode nil "autorevert")
           ;; org-mode minor modes
           (org-num-mode nil "org-num")
           (valign-mode nil "valign")))

;;; dired
(setq dired-dwim-target t)

;;; ivy
;; better performance on everything (especially windows), ivy-0.10.0 required
;; @see https://github.com/abo-abo/swiper/issues/1218
(setq ivy-dynamic-exhibit-delay-ms 250)
(ivy-mode) ; so that dired, project-find-file works using the ivy style
(add-to-list 'ivy-more-chars-alist '(counsel-ag . 2))
(add-to-list 'ivy-more-chars-alist '(counsel-rg . 2))

;; Press C-p and Enter to select current input as candidate
;; https://oremacs.com/2017/11/30/ivy-0.10.0/
(setq ivy-use-selectable-prompt t)

;;; magit
(setq magit-diff-refine-hunk t)

;;; others
(defun w/duplicate-line()
  "Duplicate the current line."
  (interactive)
  (let ((line-text (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
    (save-excursion
      (move-end-of-line 1)
      (open-line 1)
      (forward-char)
      (insert line-text))
    (next-line)))

(setq recentf-max-saved-items 10000
      recentf-auto-cleanup 'never)
(recentf-mode)

;; https://oremacs.com/2015/01/17/setting-up-ediff/
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)
