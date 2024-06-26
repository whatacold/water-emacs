(defhydra hydra-subed-shadowing (global-map "<f5>")
  "Shadowing in subed"
  ("SPC" subed-mpv-toggle-pause "play/pause")
  ("l" subed-toggle-loop-over-current-subtitle "loop or not")
  ("n" subed-forward-subtitle-text "next")
  ("p" subed-backward-subtitle-text "prev"))

(winner-mode)

(defhydra hydra (global-map "C-c w")
  "Navigate the time machine of the window layout"
  ("p" winner-undo "Previous layout")
  ("n" winner-redo "Next layout"))

(defvar winner-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") 'winner-undo)
    (define-key map (kbd "<right>") 'winner-redo)
    map)
  "Keymap to repeat `winner' key sequences.  Used in `repeat-mode'.")
(put 'winner-undo 'repeat-map 'winner-repeat-map)
(put 'winner-redo 'repeat-map 'winner-repeat-map)

(setq gtags-mode-lighter nil)

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

;;; extend built-in keybindings
(global-set-key (kbd "C-x 8 h") #'w/insert-xhs-emoji) ; C-x 8 x doesn't work, why?

;;; unusual key bindings
;; searching
;; workaround to distinguish `C-i' from 'TAB'
;; https://stackoverflow.com/a/11319885/910978
;; (define-key input-decode-map (kbd "C-i") (kbd "H-i")) ; C-i == TAB by default
;; (global-set-key (kbd "H-i") #'counsel-rg)

;;; other established & non C-c key bindings,
;;; explore global-map to find some inspirations
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "C-\\") #'counsel-rg)
(global-set-key (kbd "C-z") #'set-mark-command) ; for MS-Windows

(global-set-key (kbd "C-c i r") #'ivy-resume)
;; (global-set-key (kbd "C-c i i") #'counsel-rg)

;;; key bindings following the C-c convention
(global-set-key (kbd "C-c b r") #'w/rename-this-file-and-buffer)
(global-set-key (kbd "C-c d") #'w/duplicate-line)
(global-set-key (kbd "C-c h") #'symbol-overlay-put)
(global-set-key (kbd "C-c s y") #'ivy-yasnippet)
(global-set-key (kbd "C-c s s") #'scratch)
(global-set-key (kbd "C-c t u") #'w/urxvt)
(global-set-key [f8] #'compile)

;; jumping around
(global-set-key (kbd "C-c j c") #'avy-goto-char)
(global-set-key (kbd "C-c j j") #'avy-goto-word-1)
(global-set-key (kbd "C-c j o") #'ace-link)
(global-set-key (kbd "C-c j m") #'counsel-imenu)
(global-set-key (kbd "C-c j r") #'counsel-recentf)
(global-set-key (kbd "C-c j p") #'flymake-goto-prev-error)
(global-set-key (kbd "C-c j n") #'flymake-goto-next-error)
(global-set-key (kbd "C-c j t") #'ff-find-other-file)

(global-set-key (kbd "C-c k") #'easy-kill)

;; org-mode
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o i") #'org-download-image)
(global-set-key (kbd "C-c o y") #'org-download-clipboard) ; y as in yank/C-y

;; project.el
(define-key project-prefix-map "i" #'w/project-rg)
