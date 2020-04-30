(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?E)

(setq org-agenda-custom-commands
      '(
        ("f" "Focus"
         ((tags-todo "Milestone")
          (tags-todo "Priority")
          (tags-todo "Practice")
          (tags-todo "Meeting+LEVEL<=2")
          (tags-todo "+PRIORITY<=\"C\"-Practice-Priority-Milestone")
          ))
        ("P" "Planning"
         ((tags-todo "Milestone")
          (tags-todo "Priority")
          (tags-todo "Practice")
          (tags-todo "Meeting+LEVEL<=2")
          (tags-todo "-Practice-Priority-Milestone")
          ))
        ("W" "Weekly review"
         agenda ""
         ((org-agenda-span 9)
          (org-agenda-start-day "-9d")
          (org-agenda-start-with-log-mode t)
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if 'nottodo 'done))
          ))
        ))

;; Users should set the following in local/local.el
;; (setq org-agenda-files (list "~/Documents/notes/todo"))
;; (setq org-default-notes-file "~/Documents/notes/todo/todo.org")
;; (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capture
;;
;; Default capture template.  Add more options for project specific
;; tasks below.

(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/notes/todo/todo.org" "Tasks")
         "* TODO %?\n")
        ))


(provide 'agenda)
