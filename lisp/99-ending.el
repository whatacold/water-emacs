;;; things should be at the end
;; hide minor modes from the mode line
(require 'diminish)
(dolist (mode '(eldoc-mode which-key-mode ivy-mode yas-minor-mode
                           company-mode))
  (diminish mode))
