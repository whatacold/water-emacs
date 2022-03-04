;;; basics
(setq kill-ring-max 1000)
(setq-default indent-tabs-mode nil) ; no TAB

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

;;; dired
(setq dired-dwim-target t)

;;; ivy
;; better performance on everything (especially windows), ivy-0.10.0 required
;; @see https://github.com/abo-abo/swiper/issues/1218
(setq ivy-dynamic-exhibit-delay-ms 250)

;; Press C-p and Enter to select current input as candidate
;; https://oremacs.com/2017/11/30/ivy-0.10.0/
(setq ivy-use-selectable-prompt t)

;;; magit
(setq magit-diff-refine-hunk t)

;;; others
(defun w/duplicate-line()
  "Duplicate the current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(setq recentf-max-saved-items 10000
      recentf-auto-cleanup 'never)
