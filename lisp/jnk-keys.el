(defhydra jnk-keys-move-region (:color pink :hint nil :timeout 2)
  "
Movement                                        Region
_a_: beginning-of-line   _g_: goto-line             ^ ^: set-mark          _;_ comment
_e_: end-of-line         _z_: beginning-of-buffer   _k_: kill-region       _,_ comment/duplicate
_f_: forward-word        _c_: end-of-buffer         _l_: copy-region       _o_ org table
_b_: backward-word       _t_: forward-sexp          _u_: undo              _h_ eval
_x_: expand-region       _w_: backward-sexp         _p_: query-replace     _n_ fill
_s_: search              _'_: duplicate             _j_: extended-command  _._ deactivate
"
  ("a" crux-move-beginning-of-line)
  ("e" move-end-of-line)
  ("f" forward-word)
  ("b" backward-word)
  ("x" er/expand-region)
  ("s" swiper-isearch)

  ("A" crux-move-beginning-of-line :exit t)
  ("E" move-end-of-line :exit t)
  ("F" forward-word :exit t)
  ("B" backward-word :exit t)
  ("X" er/expand-region :exit t)
  ("S" swiper-isearch)

  ("g" goto-line)
  ("v" avy-goto-char-timer)
  ("z" beginning-of-buffer)
  ("c" end-of-buffer)
  ("w" backward-sexp)
  ("t" forward-sexp)

  ("G" goto-line :exit t)
  ("V" avy-goto-char-timer :exit t)
  ("Z" beginning-of-buffer :exit t)
  ("C" end-of-buffer :exit t)
  ("W" backward-sexp :exit t)
  ("T" forward-sexp :exit t)

  ("r" scroll-down-command)
  ("v" scroll-up-command)
  ("R" scroll-down-command :exit t)
  ("V" scroll-up-command :exit t)
  ;; d


  ("<SPC>" set-mark-command)
  ("k" kill-region)
  ("l" copy-region-as-kill)
  ("u" undo)
  ("p" query-replace)
  ("j" execute-extended-command)

  ("K" kill-region :exit t)
  ("L" copy-region-as-kill :exit t)
  ("U" undo :exit t)
  ("P" query-replace :exit t)
  ("J" execute-extended-command :exit t)

  (";" comment-dwim)
  ("," crux-duplicate-and-comment-current-line-or-region)
  ("o" org-table-create-or-convert-from-region)
  ("h" eval-region)
  ("n" fill-region)
  ("." keyboard-quit)  ;; deactivates the mark
  ("'" crux-duplicate-current-line-or-region)

  (":" comment-dwim :exit t)
  ("<" crux-duplicate-and-comment-current-line-or-region :exit t)
  ("O" org-table-create-or-convert-from-region :exit t)
  ("H" eval-region :exit t)
  ("N" fill-region :exit t)
  ("\"" crux-duplicate-current-line-or-region :exit t)

  ("q" nil)
  ("Q" jnk-keys-return :exit t)
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


(defhydra jnk-keys-file-buffer (:color teal :hint nil :timeout 2)
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


(defhydra jnk-keys-windows-frames (:color teal :hint nil :timeout 2)
  "
_2_: split window  _1_: delete-other-windows  _f_: new-frame
^ ^                _0_: delete-window         _d_: delete-frame
"
  ("2" split-window-below)
  ("1" delete-other-windows)
  ("0" delete-window)
  ("f" new-frame)
  ("d" delete-frame))


(defhydra jnk-keys-org (:color pink :hint nil :timeout 2)
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

(defhydra jnk-keys-quick-access (:color blue :hint nil :timeout 2)
  "
_s_: save  _u_: undo  _;_: comment    _p_: query  _S_: Set Chart  _k_ kill line
_f_: fill  _r_: redo  _/_: complete   _d_: rg     _m_: bookmark   _n_ spell
_c_: cap   _v_ tree   _b_: buffer     _x_: xref   _g_: goto mark  _a_ rg cwd
_o_: open  _W_: Win   _R_: Region     _F_: Files / Buffers
"
  ("s" save-buffer)
  ("f" fill-paragraph)
  ("c" org-capture)
  ("o" counsel-open-from-filelist)
  ("u" undo-fu-only-undo :color pink)
  ("r" undo-fu-only-redo :color pink)
  ("U" undo-fu-only-undo)
  ("R" undo-fu-only-redo)
  ("v" undo-tree-visualize)
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
_i_: prev  _j_: next error  _c_: compile  _g_: recompile
"
  ("i" previous-error)
  ("j" previous-error)
  ("c" compile)
  ("g" recompile)
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
