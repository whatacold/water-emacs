(use-package pdf-tools
  ;; :ensure t
  :pin manual ; don't reinstall when package updates
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; (setq-default pdf-view-display-size 'fit-page)
  ;; ;; automatically annotate highlights
  ;; (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install 'no-query)
  (add-hook 'pdf-view-mode-hook #'pdf-view-themed-minor-mode)
  ;; use normal isearch as swiper doesn't work here
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (require 'pdf-occur))

(use-package org-noter
  :init (setq ; org-noter-default-notes-file-names "xxx"
         org-noter-notes-search-path (list w/pdf-outline-export-dir)
         org-noter-doc-split-fraction '(0.6 . 0.5))
  :ensure t)

(use-package w3m
  :init
  (setq w3m-use-favicon nil))
