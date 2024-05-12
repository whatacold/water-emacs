(defun w/java-mode-setup ()
  (setq-local tab-width 2))

(add-hook 'java-mode-hook #'w/java-mode-setup)
