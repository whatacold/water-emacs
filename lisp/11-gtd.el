;; following https://www.labri.fr/perso/nrougier/GTD/index.html
(require 'org)

(setq org-directory "~/org/gtd/"
      org-agenda-files (list "inbox.org"))

;; %? the point
;; %U inactive timestamps
(setq org-capture-templates
      `(("i" "Inbox" entry  (file "inbox.org")
         ,(concat "* TODO %?\n"
                  ;; TODO how to put the timestamp in the property drawer?
                  "/Entered on/ %U"))
        ("@" "Inbox [mu4e]" entry (file "inbox.org")
         ,(concat "* TODO Reply to \"%a\" %?\n"
                  "/Entered on/ %U"))))

(define-key global-map (kbd "C-c c") #'org-capture)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; agenda
;; (setq org-agenda-files '("/a" "/b"))  ; set the agenda files
(eval-after-load 'org-agenda
  '(progn
     (add-to-list 'org-agenda-prefix-format '(agenda . "%b%?-12t% s"))))
(setq org-agenda-breadcrumbs-separator " > ")

;; org-mobile for MobileOrg
(setq org-mobile-files '("~/org/mobileorg.org" ; the capture file whose name cannot be customized
                         org-agenda-files) ; files to be staged, should be in absolute path
      org-mobile-inbox-for-pull "~/org/mobileorg-from-mobile.org"
      org-mobile-directory "~/mobileorg/") ; the staging area
