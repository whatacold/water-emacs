;; following https://www.labri.fr/perso/nrougier/GTD/index.html
(require 'org)
(require 'org-capture)

;; %? the point
;; %U inactive timestamps
(setq org-capture-templates
      `(("i" "Inbox" entry  (file "inbox.org")
         ,(concat "* TODO %?\n"
                  ;; TODO how to put the timestamp in the property drawer?
                  "Entered at %U"))
        ("t" "Test Features" entry (file "inbox.org"))
        ("@" "Inbox [mu4e]" entry (file "inbox.org")
         ,(concat "* TODO Reply to \"%a\" %?\n"
                  "Entered at %U"))))

(setq org-directory "~/org/gtd/"
      org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)"))
      org-log-done 'time

      org-agenda-hide-tags-regexp "."   ; hide all
      org-agenda-breadcrumbs-separator " > "
      org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c"))
      org-agenda-files (list "inbox.org"
                             "agenda.org"
                             "projects.org")
      org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n"))))))

      org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)"))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

;; (define-key global-map (kbd "C-c c") #'org-capture)
;; (define-key global-map (kbd "C-c a") 'org-agenda)

;; agenda
;; (setq org-agenda-files '("/a" "/b"))  ; set the agenda files

;; (eval-after-load 'org-agenda
;;   '(progn
;;      (add-to-list 'org-agenda-prefix-format '(agenda . "%b%?-12t% s"))))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

;; org-mobile for MobileOrg
(setq org-mobile-files '("~/org/mobileorg.org" ; the capture file whose name cannot be customized
                         org-agenda-files) ; files to be staged, should be in absolute path
      org-mobile-inbox-for-pull "~/org/mobileorg-from-mobile.org"
      org-mobile-directory "~/mobileorg/") ; the staging area
