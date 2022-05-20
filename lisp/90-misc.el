;;; keycast
(setq keycast-log-format "%-18K%C%R\n"
      keycast-log-buffer-name "*keycast by @whatacold*"
      keycast-remove-tail-elements nil
      keycast-log-frame-alist '((inhibit-switch-frame . t)
                                (pop-up-frame-parameters . ((font . "DejaVu Sans Mono-12")
                                                            (minibuffer . nil)))))
(eval-after-load 'keycast
  '(progn
     (push '(self-insert-command nil nil) keycast-substitute-alist)))


;;; which-key displays the key bindings following your currently entered incomplete command (a prefix) in a popup
(setq which-key-allow-imprecise-window-fit t) ; performance
(which-key-mode)

;;; misc
;;;; save the recent file list
(run-with-timer 600 600
                (lambda ()
                  (recentf-save-list)))

;; midnight
(require 'midnight)
(add-hook 'midnight-hook #'recentf-save-list)
(midnight-mode) ; (clean-buffer-list) automatically

(battery-notifier-mode)
