;; Bookmarks can be tagged as project/task/scope
(defvar jnk-bookmarks-project
  "Defines the current project used for bookmark tags.")
(defvar jnk-bookmarks-task
  "Defines the current task used for bookmark tags.")
(defvar jnk-bookmarks-scope
  "Defines the current scope used for bookmark tags and cycling.")


(setq jnk-bookmarks-project "project")
(setq jnk-bookmarks-task    "task")
(setq jnk-bookmarks-scope   "scope")

(defun project-task-scope-tag ()
  (concat jnk-bookmarks-project "/"
	  jnk-bookmarks-task "/"
	  jnk-bookmarks-scope))

(defun jnk-bookmarks-scope-new-bookmark ()
  (interactive)
  (bookmark-set)
  (setq bookmark (car bookmark-alist))
  (bmkp-add-tags bookmark (list (concat jnk-bookmarks-project "/"
					jnk-bookmarks-task "/"
					jnk-bookmarks-scope)))
  (jnk-bookmarks-scope-bookmark-list)
  )


(defun jnk-bookmarks-task-new-bookmark ()
  (interactive)
  (bookmark-set)
  (setq bookmark (car bookmark-alist))
  (bmkp-add-tags bookmark (list (concat jnk-bookmarks-project "/"
					jnk-bookmarks-task)))
  )

(defun jnk-bookmarks-project-new-bookmark ()
  (interactive)
  (bookmark-set)
  (setq bookmark (car bookmark-alist))
  (bmkp-add-tags bookmark (list jnk-bookmarks-project)))


(defun jnk-bookmarks-scope-bookmark-list ()
  (interactive)
  (let (bookmark-list-buffer (get-buffer "*Bookmark List*"))
       (if (not bookmark-list-buffer)
	   (progn
	     (bookmark-bmenu-list)
	     (setq bookmark-list-buffer (get-buffer "*Bookmark List*"))))
       (with-current-buffer bookmark-list-buffer
	   (let ((bmkp-bmenu-filter-pattern (project-task-scope-tag)))
	     (bmkp-bmenu-filter-alist-by-tags-regexp)
	     (bmkp-choose-navlist-from-bookmark-list "CURRENT *Bookmark List*")))))

(defun jnk-bookmarks-project-set (project)
  (interactive "MProject: ")
  (setq jnk-bookmarks-project project)
  (jnk-bookmarks-scope-bookmark-list))

(defun jnk-bookmarks-task-set (task)
  (interactive "MTask: ")
  (setq jnk-bookmarks-task task)
  (jnk-bookmarks-scope-bookmark-list))

(defun jnk-bookmarks-scope-set (scope)
  (interactive "MScope: ")
  (setq jnk-bookmarks-scope scope)
  (jnk-bookmarks-scope-bookmark-list))

(provide 'jnk-bookmarks)
