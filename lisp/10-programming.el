;;; eglot
(setq eldoc-echo-area-use-multiline-p nil)

;;; python
(when w/python-venv
  (pyvenv-activate w/python-venv))
