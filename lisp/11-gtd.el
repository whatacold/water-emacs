;; following https://www.labri.fr/perso/nrougier/GTD/index.html
(require 'org)
(require 'org-capture)

;; %? the point
;; %U inactive timestamps
(setq org-capture-templates
      `(("i" "Inbox" entry  (file "~/org/gtd/inbox.org")
         ,(concat "* TODO %?\n"
                  ;; TODO how to put the timestamp in the property drawer?
                  "Captured at %U"))
        ("t" "Test Features" entry (file "~/org/gtd/inbox.org"))
        ("@" "Inbox [mu4e]" entry (file "~/org/gtd/inbox.org")
         ,(concat "* TODO Reply to \"%a\" %?\n"
                  "Captured at %U"))))

;; XXX don't change org-directory, or mobileorg will result in confict
;; after syncing back a modified org file in a sub-dir.
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "WAIT(w)"
                  "ASSG(a)"             ; assigned
                  "|" "DONE(d)"
                  ;; canceled
                  "CNCL(c)"))
      org-log-done 'time

      org-agenda-hide-tags-regexp "."   ; hide all
      org-agenda-breadcrumbs-separator " > "
      org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c"))
      org-agenda-files (list "~/org/gtd/inbox.org"
                             "~/org/gtd/agenda.org"
                             "~/org/gtd/projects.org")
      ;; https://stackoverflow.com/questions/32423127/how-to-view-the-next-days-in-org-modes-agenda
      org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo '("DONE" "CNCL")))
                   (org-agenda-overriding-header "Deadlines")))
          (agenda nil
                  ((org-agenda-span 4)  ; at least show next monday's agenda when it's friday
                   (org-deadline-warning-days 0)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo '("DONE" "CNCL")))
                   ;; should also include tasks having deadline
                   ;; (org-agenda-overriding-header "Week Agenda Excluding Deadlines")
                   ))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "NEXT Tasks")))
          (tags-todo "inbox"            ; search for tag inbox?
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "Inbox")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "Completed today")))
          (agenda nil
                 ((org-agenda-start-day "-14d")
                  (org-agenda-span 14)
                  (org-agenda-start-on-weekday 1)
                  (org-agenda-start-with-log-mode '(closed))
                  (org-agenda-skip-function
                   '(org-agenda-skip-entry-if 'nottodo '("DONE")))
                  (org-agenda-overriding-header "Completed in last week")))
          (tags "project"
                ((org-use-tag-inheritance nil)
                 (org-agenda-overriding-header "All projects"))))))

      org-clock-idle-time 10
      org-clock-mode-line-total 'current

      org-refile-targets
      ;; only Tasks are concerned
      '(("~/org/gtd/projects.org" . (:regexp . "\\(?:\\(?:Task\\)s\\)")))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

;; org-mobile for MobileOrg
(setq org-mobile-files '("~/org/mobileorg.org" ; the capture file whose name cannot be customized
                         org-agenda-files) ; files to be staged, should be in absolute path
      org-mobile-inbox-for-pull "~/org/gtd/inbox.org"
      org-mobile-directory "~/tmp/mobileorg-staging-area/")

(defun w/org-mobile-sync-query ()
  "Query user when killing Emacs about whether files have been sync'ed.

This function is added to `kill-emacs-query-functions'."
  (yes-or-no-p "Have org mobiles files been sync'ed? "))

(add-hook 'kill-emacs-query-functions #'w/org-mobile-sync-query)

;;; misc
;; ivy and refile, see https://emacs.stackexchange.com/questions/38841/counsel-m-x-always-shows
(dolist (key '(org-refile org-capture-refile org-agenda-refile))
  (setq ivy-initial-inputs-alist (assq-delete-all key ivy-initial-inputs-alist)))
