;; -*- lexical-binding: t -*-
;;; tune built-in features
(setq messages-buffer-max-lines 10000)

(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(setq httpd-port 8000)

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

(use-package emacs-everywhere
  :ensure t
  :init
  (defun w/emacs-everywhere-workaround-gnome-frame ()
    "Workaround a problem in GNOME and/or Emacs."
    ;; even so, the width parameter doesn't take effect!
    (modify-frame-parameters (selected-frame) emacs-everywhere-frame-parameters))

  (defun w/emacs-everywhere-copy-org-buffer ()
    "Copy the current buffer."
    (copy-region-as-kill (point-min) (point-max)))

  ;; workaround the problem that alt+tab won't work after auto paste
  (setq emacs-everywhere-paste-command nil
        emacs-everywhere-final-hooks '(w/emacs-everywhere-copy-org-buffer
                                       emacs-everywhere-convert-org-to-gfm
                                       emacs-everywhere-remove-trailing-whitespace))
  (add-to-list 'emacs-everywhere-frame-parameters '(fullscreen . nil))
  (add-to-list 'emacs-everywhere-frame-parameters '(width . 80))
  (add-to-list 'emacs-everywhere-frame-parameters '(height . 50))
  (add-to-list 'emacs-everywhere-markdown-windows "Emacs China")

  (add-hook 'emacs-everywhere-mode-hook #'auto-fill-mode)
  (add-hook 'emacs-everywhere-init-hooks #'w/emacs-everywhere-workaround-gnome-frame)
  ;; (add-hook 'emacs-everywhere-final-hooks
  ;;           (lambda ()
  ;;             (emacs-everywhere-remove-trailing-whitespace))
  ;;           'append)
  )

(use-package bbdb
  :init
  (setq bbdb-file "~/org/bbdb.eld"))

(use-package org-clock
  :init
  (setq org-clock-sound t))

(defun w/set-selective-display-current-indent ()
  "Turn on the selective display so as to hide lines whose indentation > point.

See `set-selective-display' for docs."
  (interactive)
  (let ((indent (- (point) (line-beginning-position))))
    (set-selective-display (1+ indent))))

;;; which-key displays the key bindings following your currently entered incomplete command (a prefix) in a popup
(setq which-key-allow-imprecise-window-fit t) ; performance
(which-key-mode)

;;; save the recent file list
(run-with-timer 600 600
                (lambda ()
                  (recentf-save-list)
                  (dired-recent-save-list)))

(defcustom w/do-things-when-idle-hooks nil
  "Things to do when Emacs is idle.")

(defun w/do-things-when-idle ()
  (run-hooks 'w/do-things-when-idle-hooks))

(add-hook 'w/do-things-when-idle-hooks
          (lambda ()
            (with-current-buffer (find-file-noselect "~/org/gtd/projects.org")
              (org-update-statistics-cookies 'all)
              (save-buffer))))

(run-with-idle-timer 1800 'repeat #'w/do-things-when-idle)

;;; midnight
(require 'midnight)
(add-hook 'midnight-hook #'recentf-save-list)
(add-hook 'midnight-hook #'w/do-things-when-idle)
(midnight-mode) ; (clean-buffer-list) automatically

;;; subed
(setq subed-loop-seconds-before 0
      subed-loop-seconds-after 0
      subed-auto-play-media nil
      subed-default-subtitle-length 3500)

;;; OS helpers
(when (w/windows-p)
  (setq visible-bell t))           ; mute the annoying bell

(when (and w/enable-battery-notifier-p
           (w/i3-p))
  (require 'battery)
  (when battery-status-function
    (battery-notifier-mode)))

(defun w/adjust-screen-brightness (inc)
  "Adjust the screen brightness by INC*10%.

INC may be passed as a numeric prefix argument.

The actual adjustment made depends on the final component of the
key-binding used to invoke the command, with all modifiers removed:

   +, =   Increase the height of the default face by one step
   -      Decrease the height of the default face by one step

After adjusting, continue to read input events and further adjust
the screen brightness as long as the input event read
\(with all modifiers removed) is one of the above characters.
"
  (interactive "p")
  (let ((ev last-command-event)
	(echo-keystrokes nil))
    (let* ((base (event-basic-type ev))
           (step (* inc 10))
           (cmd (format "brightnessctl set %s%d%%%s" ; +10% or 10%-
                        (if (/= ?- base)
                            "+"         ; increase it by default
                          "")
                        step
                        (if (= ?- base) "-" ""))))
      (shell-command cmd)

      (message "Use +,- for further adjustment by %d%%" step)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (dolist (mods '(() (control)))
           (dolist (key '(?- ?+ ?=)) ;; = is often unshifted +.
             (define-key map (vector (append mods (list key)))
               (lambda () (interactive)
                 (w/adjust-screen-brightness (abs inc))))))
         map)))))

;;; org-present
(defun w/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(add-hook 'org-present-after-navigate-functions
          #'w/org-present-prepare-slide)
;; (setq org-present-after-navigate-functions nil)

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; elfeed
(use-package elfeed
  :init
  (setq elfeed-curl-timeout 10
        elfeed-search-filter "+unread"))

(use-package elfeed-org
  :init
  (setq rmh-elfeed-org-files (list "~/org/elfeed-feeds.org"))
  (elfeed-org))

;;; pdf-tools
;; https://github.com/politza/pdf-tools/issues/338#issuecomment-447214217
(defun w/pdf-outline-export-to-org ()
  "Export the outline of current pdf to an org mode file.

Also take a look at `org-noter-create-skeleton'."
  (interactive)
  (let* ((pdf-buffer (current-buffer))
         (filename (file-name-sans-extension (buffer-name pdf-buffer)))
         (org-filename (concat filename ".org"))
         (outline-info (pdf-info-outline pdf-buffer)))
    (if (not outline-info)
        (error "No outline.")
      (with-temp-buffer
        (org-mode)
        (insert (concat "#+TITLE: " filename "\n\n"))
        (dolist (item outline-info)
          (let ((title (assoc-default 'title item))
                (page (assoc-default 'page item))
                (level (assoc-default 'depth item)))
            (insert (format
                     "%s %s\n"
                     (make-string level ?*)
                     title))))
        (write-file (concat (or w/pdf-outline-export-dir default-directory)
                            org-filename)
                    'prompt-confirm)))))
