;;; eglot
(setq eldoc-echo-area-use-multiline-p nil)

(define-key eglot-mode-map (kbd "C-c <tab>") #'company-complete) ; initiate the completion manually
(define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename)

;;; python
(when w/python-venv
  (pyvenv-activate w/python-venv))
