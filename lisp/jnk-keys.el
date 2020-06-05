(defhydra jnk-keys-move-region (:color pink :hint nil)
  "
Movement                                        Region
_w_: beginning-of-line   _g_: goto-line             ^ ^: set-mark          _;_ comment
_e_: end-of-line         _1_: beginning-of-buffer   _k_: kill-region       _,_ comment/duplicate
_f_: forward-word        _2_: end-of-buffer         _l_: copy-region       _o_ org table
_a_: backward-word       _v_: forward-sexp          _u_: undo              _h_ eval
_d_: expand-region       _c_: backward-sexp         _p_: query-replace     _n_ fill
_s_: search              _r_: search backward       _j_: extended-command  _._ deactivate
_x_: exchange            ^ ^                        _i_: duplicate         _y_ yank
"
  ("w" crux-move-beginning-of-line)
  ("e" move-end-of-line)
  ("f" forward-word)
  ("a" backward-word)
  ("d" er/expand-region)
  ("s" jnk-keys-isearch-forward-wrapper)
  ("x" exchange-point-and-mark)

  ("W" crux-move-beginning-of-line :exit t)
  ("E" move-end-of-line :exit t)
  ("F" forward-word :exit t)
  ("A" backward-word :exit t)
  ("D" er/expand-region :exit t)
  ("S" isearch-forward :exit t)
  ("X" exchange-point-and-mark :exit t)

  ("g" goto-line)
  ("1" beginning-of-buffer)
  ("2" end-of-buffer)
  ("c" backward-sexp)
  ("v" forward-sexp)
  ("r" jnk-keys-isearch-backward-wrapper)

  ("G" goto-line :exit t)
  ("1" beginning-of-buffer :exit t)
  ("2" end-of-buffer :exit t)
  ("C" backward-sexp :exit t)
  ("V" forward-sexp :exit t)
  ("R" isearch-backward :exit t)

  ("3" scroll-down-command)
  ("4" scroll-up-command)
  ("#" scroll-down-command :exit t)
  ("$" scroll-up-command :exit t)

  ;; t
  ;; b
  ;; z

  ("<SPC>" set-mark-command)
  ("k" kill-region)
  ("l" copy-region-as-kill)
  ("u" undo)
  ("p" query-replace)
  ("j" execute-extended-command)
  ("i" crux-duplicate-current-line-or-region)

  ("K" kill-region :exit t)
  ("L" copy-region-as-kill :exit t)
  ("U" undo :exit t)
  ("P" query-replace :exit t)
  ("J" execute-extended-command :exit t)
  ("I" crux-duplicate-current-line-or-region :exit t)

  (";" comment-dwim)
  ("," crux-duplicate-and-comment-current-line-or-region)
  ("o" org-table-create-or-convert-from-region)
  ("h" eval-region)
  ("n" fill-region)
  ("." keyboard-quit)  ;; deactivates the mark
  ("y" jnk-keys-return-and-yank)

  (":" comment-dwim :exit t)
  ("<" crux-duplicate-and-comment-current-line-or-region :exit t)
  ("O" org-table-create-or-convert-from-region :exit t)
  ("H" eval-region :exit t)
  ("N" fill-region :exit t)
  ("Y" jnk-keys-return-and-yank :exit t)

  ("q" nil)
  ("Q" jnk-keys-return :exit t)

  ;; b
  ;; m
  ;; /
  )

(defvar jnk-keys-marker)

(defun jnk-keys-move-region-wrapper ()
  "A wrapper around the jnk-keys-move-region hydra that saves the
   initial location.  The location can be restored by the hydra
   if desired."
  (interactive)

  (setq jnk-keys-marker (point-marker))
  (jnk-keys-move-region/body)
  )

(defun jnk-keys-return ()
  (interactive)
  (let ((buf  (marker-buffer jnk-keys-marker)))
    (switch-to-buffer buf)(goto-char jnk-keys-marker)))

(defun jnk-keys-return-and-yank ()
  (interactive)
  (if (use-region-p)
      (progn
	(copy-region-as-kill (region-beginning) (region-end))
	(jnk-keys-return)
	(yank))))

(defun jnk-keys-isearch-forward-wrapper (&optional REGEXP-P NO-RECURSIVE-EDIT)
  "A wrapper around isearch-forward that invokes the hydra when
done.  isearch-forward exits the hydra, use this wrapper to 'stay
in' the hydra."
  (interactive)
  (isearch-forward REGEXP-P NO-RECURSIVE-EDIT)
  (jnk-keys-move-region/body))

(defun jnk-keys-isearch-backward-wrapper (&optional REGEXP-P NO-RECURSIVE-EDIT)
  "A wrapper around isearch-forward that invokes the hydra when
done.  isearch-forward exits the hydra, use this wrapper to 'stay
in' the hydra."
  (interactive)
  (isearch-backward REGEXP-P NO-RECURSIVE-EDIT)
  (jnk-keys-move-region/body))


(defhydra jnk-keys-file-buffer (:color teal :hint nil)
  "
_f_: find-file     _s_: save-buffer  _o_: ff-get-other-file  _b_: switch-buffer
_r_: recent file   _k_: kill-buffer  ^ ^                     _l_: list-buffers

"

  ("f" find-file)
  ("o" ff-get-other-file)
  ("r" crux-recentf-find-file)
  ("s" save-buffer)
  ("b" switch-to-buffer)
  ("k" kill-current-buffer)
  ("l" ibuffer)
  ("q" nil)
  )


(defhydra jnk-keys-windows-frames (:color teal :hint nil)
  "
_2_: split window  _1_: delete-other-windows  _f_: new-frame
^ ^                _0_: delete-window         _d_: delete-frame
"
  ("2" split-window-below)
  ("1" delete-other-windows)
  ("0" delete-window)
  ("f" new-frame)
  ("d" delete-frame))


(defhydra jnk-keys-org (:color pink :hint nil)
  "
Heading: sfed   Subtree: jlik   _a_: archive  _u_: fold up     _r_: return
_n_: insert       _m_: ins-close    _c_: closeup  _v_: close-edit  _t_: todo
"
  ("s" org-metaleft)
  ("f" org-metaright)
  ("e" org-shiftmetaup)
  ("d" org-shiftmetadown)
  ("r" jnk-keys-return)
  ("S" org-metaleft :exit t)
  ("F" org-metaright :exit t)
  ("E" org-shiftmetaup :exit t)
  ("D" org-shiftmetadown :exit t)

  ("j" org-shiftmetaleft)
  ("l" org-shiftmetaright)
  ("i" org-metaup)
  ("k" org-metadown)
  ("J" org-shiftmetaleft :exit t)
  ("L" org-shiftmetaright :exit t)
  ("I" org-metaup :exit t)
  ("K" org-metadown :exit t)

  ("a" org-archive-subtree)
  ("A" org-archive-subtree :exit t)

  ("u" jnk-keys-org-fold-up)
  ("n" jnk-keys-org-insert-next)
  ("m" jnk-keys-org-insert-next-and-close)
  ("c" jnk-keys-org-close-up)
  ("v" jnk-keys-org-close-and-edit)
  ("t" org-todo)

  ("U" jnk-keys-org-fold-up :exit t)
  ("N" jnk-keys-org-insert-next :exit t)
  ("M" jnk-keys-org-insert-next-and-close :exit t)
  ("C" jnk-keys-org-close-up :exit t)
  ("V" jnk-keys-org-close-and-edit :exit t)
  ("T" org-todo :exit t)

  ("q" nil)
  ("Q" jnk-keys-return :exit t)
  )

(defun jnk-keys-org-wrapper ()
  "A wrapper around the jnk-keys-org hydra that saves the initial
   location.  The location can be restored by the hydra if
   desired."
  (interactive)

  (setq jnk-keys-marker (point-marker))
  (jnk-keys-org/body)
  )

(defun jnk-keys-org-fold-up ()
  (interactive)
  (outline-hide-subtree)
  (outline-previous-visible-heading 1)
  )

(defun jnk-keys-org-insert-next ()
  (interactive)
  (outline-hide-subtree)
  (org-insert-heading-respect-content)
  )

(defun jnk-keys-org-insert-next-and-close ()
  (interactive)
  (jnk-keys-org-close-up)
  (org-insert-heading-respect-content)
  )

(defun jnk-keys-org-close-up ()
  (interactive)
  (outline-hide-subtree)
  (if (not (org-at-heading-p))
      (outline-previous-visible-heading 1)
    )
  (if (not (org-entry-is-done-p))
      (let ((org-use-fast-todo-selection nil))
	(org-todo))
      )
  )


(defun jnk-keys-org-close-and-edit ()
  (interactive)
  (outline-previous-visible-heading 1)
  (if (org-get-todo-state)
      (progn
	(let ((org-use-fast-todo-selection nil))
	  (org-todo))
	(forward-symbol 1))
    )
  (forward-symbol 1)
  (forward-char 1)
  )

(defhydra jnk-keys-quick-access (:color blue :hint nil)
  "
_s_: save  _u_: undo    _;_: comment    _p_: query  _S_: Set Chart  _k_ kill line
_f_: fill  _r_: redo    _/_: complete   _d_: rg     _m_: bookmark   _n_ spell
_c_: cap   _j_: recent  _b_: buffer     _x_: xref   _g_: goto mark  _a_ rg cwd
_o_: open  ^ ^          ^ ^             _R_: Region _F_: Files / Buffers
"
  ("s" save-buffer)
  ("f" fill-paragraph)
  ("c" org-capture)
  ("o" counsel-open-from-filelist)
  ("j" crux-recentf-find-file)
  ("u" undo-fu-only-undo :color pink)
  ("r" undo-fu-only-redo :color pink)
  ("U" undo-fu-only-undo)
  ("R" undo-fu-only-redo)
  (";" comment-dwim)
  ("/" dabbrev-expand :color pink)
  ("?" dabbrev-expand)
  ("b" switch-to-buffer)
  ("p" query-replace)
  ("d" deadgrep)
  ("x" xref-find-definitions)
  ("S" nav-bm-set-file)
  ("m" nav-bm-capture)
  ("g" counsel-nav-bm)
  ("W" jnk-keys-windows-frames/body)
  ("R" jnk-keys-move-region-wrapper)
  ("F" jnk-keys-file-buffer/body)
  ("k" crux-kill-whole-line :color pink)
  ("K" crux-kill-whole-line)
  ("n" flyspell-correct-wrapper :color pink)
  ("N" flyspell-correct-wrapper)
  ("a" rg-cwd)
  ("q" nil :exit t)
  )

(defun rg-cwd ()
  (interactive)
  (let ((deadgrep-project-root-function (lambda () default-directory)))
    (call-interactively 'deadgrep))
  )


(defhydra jnk-keys-compile (:color pink :hint nil)
  "
_i_: prev  _k_: next error  _c_: compile  _g_: recompile _f_: kill
"
  ("i" previous-error)
  ("k" next-error)
  ("c" compile :color blue)
  ("g" recompile :color blue)
  ("f" kill-compilation :color blue)
  ("q" nil)
  )

(defvar jnk-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-d") 'jnk-keys-move-region-wrapper)
    (define-key map (kbd "M-n") 'jnk-keys-quick-access/body)
    (define-key map (kbd "M-c") 'jnk-keys-compile/body)
    map)
  "jnk-keys-minor-mode keymap.")

(define-minor-mode jnk-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " jnk-keys")

(provide 'jnk-keys)
