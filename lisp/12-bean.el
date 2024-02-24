;;;; beancount stuffs

;;; beancount hacks
(defcustom w/beancount-account-files nil
  "List of account files")

(defun w/beancount--collect-accounts-from-files (oldfun regex n)
  (let ((keys (funcall oldfun regex n))
        (hash (make-hash-table :test 'equal)))
    (dolist (key keys)
      (puthash key nil hash))
    ;; collect accounts from files
    (save-excursion
      (dolist (f w/beancount-account-files)
        (with-current-buffer (find-file-noselect f)
          (goto-char (point-min))
          (while (re-search-forward beancount-account-regexp nil t)
            (puthash (match-string-no-properties n) nil hash)))))
    (hash-table-keys hash)))

(advice-add #'beancount-collect
            :around #'w/beancount--collect-accounts-from-files
            '((name . "collect accounts from files as well")))

(defcustom w/beancount-tags-file nil
  "The file contains all tags, one per line.")

(defvar w/beancount--tags nil)

(defcustom w/beancount-links-file nil
  "The file contains all links, one per line.")

(defvar w/beancount--links nil)

(defun w/beancount-insert-tag ()
  "Insert a tag."
  (interactive)
  (unless w/beancount-tags-file
    (error "Please set `w/beancount-tags-file' first"))
  (insert (ivy-read "Select one:"
                    (w/read-lines w/beancount-tags-file))))

(defun w/beancount-insert-link ()
  "Insert a link."
  (interactive)
  (unless w/beancount-links-file
    (error "Please set `w/beancount-links-file' first"))
  (insert (ivy-read "Select one:"
                    (w/read-lines w/beancount-links-file))))

;;; beancount package settings
;; TODO make below work
;; (use-package beancount
;;   :init
;;   (setq beancount-use-ido nil
;;         beancount-mode-map-prefix [(control c) (b)])
;;   :config
;;   (define-key beancount-mode-map (kbd "C-c b T") #'w/beancount-insert-tag)
;;   (define-key beancount-mode-map (kbd "C-c b L") #'w/beancount-insert-link)
;;   :mode "\\.bean$"
;;   :mode "\\.beancount$"
;;   :hook (beancount-mode outline-minor-mode))

(add-to-list 'auto-mode-alist '("\\.beancount$" . beancount-mode))
(add-to-list 'auto-mode-alist '("\\.bean$" . beancount-mode))
(setq beancount-use-ido nil
      beancount-mode-map-prefix [(control c) (b)])
(add-hook 'beancount-mode-hook #'outline-minor-mode)
(eval-after-load 'beancount
  '(progn
     (define-key beancount-mode-map (kbd "C-c b T") #'w/beancount-insert-tag)
     (define-key beancount-mode-map (kbd "C-c b L") #'w/beancount-insert-link)))
