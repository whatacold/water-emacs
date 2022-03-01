;;; auto save files
(defvar w/auto-save--timer nil)

(defun w/auto-save-enable ()
    "Save all unsaved filed when being idle."
  (interactive)
  (w/auto-save-disable)
  (setq w/auto-save--timer
	(run-with-idle-timer
	 3
	 'repeat
	 (lambda ()
	   (save-some-buffers t)))))

(defun w/auto-save-disable ()
    "Disable auto-saving files."
  (interactive)
  (when w/auto-save--timer
    (cancel-timer w/auto-save--timer)))

(w/auto-save-enable)

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

(setq recentf-max-saved-items 10000)
