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
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)"))
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
      org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo '("DONE")))
                   (org-agenda-overriding-header "Deadlines")))
          (agenda nil
                  ((org-agenda-span 3)
                   (org-deadline-warning-days 0)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo '("DONE")))
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
          (tags "project"
                ((org-use-tag-inheritance nil)
                 (org-agenda-overriding-header "All projects"))))))

      org-refile-targets
      ;; only Tasks are concerned
      '(("projects.org" . (:regexp . "\\(?:\\(?:Task\\)s\\)")))
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

;;; misc
;; ivy and refile, see https://emacs.stackexchange.com/questions/38841/counsel-m-x-always-shows
(dolist (key '(org-refile org-capture-refile org-agenda-refile))
  (setq ivy-initial-inputs-alist (assq-delete-all key ivy-initial-inputs-alist)))
