;; -*- lexical-binding: t -*-

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
                        (if (or (= ?+ base)
                                (= ?= base))
                            "+"
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
