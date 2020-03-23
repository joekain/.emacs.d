(defun jnk-dashboard-file-contents (file)
   (split-string
    (with-current-buffer (find-file-noselect file)
      (buffer-string)
      ) "\n")
   )

(defun jnk-dashboard-insert-example (list-size)
  (dashboard-insert-section
   "Example:"
  (jnk-dashboard-file-contents "~/example")
   list-size
   "e"
   nil
   (abbreviate-file-name el))
  )

;; (add-to-list 'dashboard-item-generators  '(example . jnk-dashboard-insert-example))
;; (add-to-list 'dashboard-items '(example) t)

(provide 'jnk-dashboard)
