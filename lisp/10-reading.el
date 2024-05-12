(use-package pdf-tools
  ;; :ensure t
  :pin manual ; don't reinstall when package updates
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; (setq-default pdf-view-display-size 'fit-page)
  ;; ;; automatically annotate highlights
  ;; (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install 'no-query)
  (add-hook 'pdf-view-mode-hook #'pdf-view-themed-minor-mode)
  ;; use normal isearch as swiper doesn't work here
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (require 'pdf-occur))

;; part of pdf-tools
(use-package pdf-view
  :after (pdf-tools)
    :bind (:map pdf-view-mode-map
                ([remap scroll-up-command] . #'pdf-view-scroll-up-or-next-page)
                ([remap scroll-down-command] . #'pdf-view-scroll-down-or-previous-page)))

(use-package org-noter
  :init (setq ; org-noter-default-notes-file-names "xxx"
         org-noter-notes-search-path (list w/pdf-outline-export-dir)
         org-noter-doc-split-fraction '(0.6 . 0.5))
  :ensure t)

(use-package w3m
  :init
  (setq w3m-use-favicon nil))
