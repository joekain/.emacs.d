
(defun nav-bm-init ()
  (setq nav-mark-file nil)
  (setq org-capture-templates
	(append
	 '(("n" "Nav" entry (file+headline nav-mark-file "Navigation Chart")
	    "* [[%l][%i]] :navbm:"))
	org-capture-templates)))

(defun nav-bm-set-file (buf)
  "Set the file/buffer used to source the Navigational Chart"
  (interactive "*bBuffer: ")
  (setq nav-mark-file (buffer-file-name (get-buffer buf))))

(defun nav-bm-capture (&optional description)
  "Capture using the Nav template"
  (interactive (list
                (read-string (format "Link Description (%s): "
				     (thing-at-point 'symbol))
                             nil nil (thing-at-point 'symbol))))
  (org-capture-string description "n"))

(defun nav-bm-get-links ()
  "Return a list of all nav-marks"
  (org-ql-select nav-mark-file
    `(tags "navbm")
    :action '(org-get-heading t t)))

(defun counsel-nav-bm (&optional initial-input)
  "Navigate to any nav-mark."
  (interactive)
  (ivy-read "Navigate: " (nav-bm-get-links)
	    :initial-input initial-input
	    :action (lambda (headline)
		      (with-ivy-window
			(when headline
			  (org-open-link-from-string headline))))
	    :caller 'counsel-nav-bm))

(defhydra hydra-nav-bm ()
  "Navigation Commands"
  ("f" nav-bm-set-file "Set Navigational Chart file" :exit t)
  ("m" nav-bm-capture "Save a Nav Mark" :exit t)
  ("g" counsel-nav-bm "Go to a Nav Mark" :exit t))

(provide 'nav-bm)
