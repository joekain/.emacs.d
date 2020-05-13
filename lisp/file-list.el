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
;; This is useful as is but can also be used as an example.  Users
;; should set counsel-open-from-filelist-filelist to configure this
;; for their needs.  Alternatively, write a similar function with your
;; own list.

(defvar counsel-open-from-filelist-filelist "/tmp/filelist"
  "Set this variable to point to a file listing a set of files.
counsel-open-from-filelist will select from this list."
  )

(defun counsel-open-from-filelist (&optional initial-input)
  (interactive)
  (counsel-file-list--1 counsel-open-from-filelist-filelist))

(provide 'file-list)
