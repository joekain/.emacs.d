;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counsel worker functions

(defun counsel-file-list--cands (file)
    (save-current-buffer
      (set-buffer (find-file-noselect file))
      (split-string
       (buffer-substring-no-properties 1 (point-max)))
      )
    )

(defun counsel-file-list--1 (file  &optional initial-input)
  "Find file from a list of files."
  (interactive)
  (ivy-read "Find file: " (counsel-file-list--cands file)
            :initial-input initial-input
            :history 'counsel-file-list-history
            :action (lambda (file)
                      (with-ivy-window
                        (when file
                          (find-file file))))
            :caller 'counsel-file-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User function
;;
;; This is really just an example.  It reads the list of files from
;; /tmp/filelist.  Users should create there own file lists for their
;; projects and then write a function like the one below.
;; Name your function after your project and use the project filelist.

(defun counsel-example-filelist (&optional initial-input)
  "Find file listed in /tmp/filelist"
  (interactive)
  (counsel-file-list--1 "/tmp/filelist"))

(provide 'file-list)
