(defhydra jnk-keys-move-region (:color pink :hint nil)
  "
Movement                                        Region
_a_: beginning-of-line   _g_: goto-line             ^ ^: set-mark          _;_ comment
_e_: end-of-line         _z_: beginning-of-buffer   _k_: kill-region       _n_ fill
_f_: forward-word        _c_: end-of-buffer         _l_: copy-region       _t_ table
_b_: backward-word       _t_: forward-sexp          _u_: undo
_x_: expand-region       _w_: backward-sexp         _p_: query-replace
_s_: search              ^ ^                        _j_: extended-command
"
  ("a" move-beginning-of-line)
  ("e" move-end-of-line)
  ("f" forward-word)
  ("b" backward-word)
  ("x" er/expand-region)
  ("s" swiper-isearch)

  ("A" move-beginning-of-line :exit t)
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

  ;; d
  ;; r
  ;; v

  ("<SPC>" set-mark-command)
  ("k" kill-region)
  ("l" copy-region-as-kill)
  ("u" undo)
  ("p" query-replace)
  ("j" counsel-M-x)

  ("K" kill-region :exit t)
  ("L" copy-region-as-kill :exit t)
  ("U" undo :exit t)
  ("P" query-replace :exit t)
  ("J" counsel-M-x :exit t)

  (";" comment-dwim)
  ("n" fill-region)
  ("t" org-table-create-or-convert-from-region)

  (":" comment-dwim :exit t)
  ("N" fill-region :exit t)
  ("T" org-table-create-or-convert-from-region :exit t)

  ("q" nil)
  ("Q" jnk-keys-move-region-return :exit t)
  )

(defun jnk-keys-move-region-wrapper ()
  "A wrapper around the jnk-keys-move-region hydra that saves the
   initial location.  The location can be restored by the hydra
   if desired."
  (interactive)
  (ignore-errors
    (bookmark-set "jnk-keys-bookmark"))
  (jnk-keys-move-region/body)
  )

(defun jnk-keys-move-region-return ()
  (interactive)
  (bookmark-jump "jnk-keys-bookmark"))


(defhydra jnk-keys-file-buffer (:color teal :hint nil)
  "
_f_: find-file     _s_: save-buffer  _o_: ff-get-other-file  _b_: switch-buffer
_r_: recent file   _k_: kill-buffer  ^ ^                     _l_: list-buffers

"

  ("f" find-file)
  ("o" ff-get-other-file)
  ("r" counsel-recentf)
  ("s" save-buffer)
  ("b" persp-ivy-switch-buffer)
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
Heading: sfed   Subtree: jlik   _a_: archive
"
  ("s" org-metaleft)
  ("f" org-metaright)
  ("e" org-shiftmetaup)
  ("d" org-shiftmetadown)
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

  ("q" nil))


(defhydra jnk-keys-quick-access (:color blue :hint nil)
  "
_s_: save  _u_: undo  _;_:  comment   _q_: query  _S_: Set Chart
_f_: fill  _r_: redo  _/_:  complete  _d_: rg     _m_: bookmark
_c_: cap   _v_ tree   _b_: buffer     _x_: xref   _g_: goto meark
_o_: open  _W_: Windows               _F_: Files / Buffers
"

  ("s" save-buffer)
  ("f" fill-paragraph)
  ("c" org-capture)
  ("o" counsel-open-from-filelist)
  ("u" undo)
  ("r" redo)
  ("v" undo-tree-visualize)
  (";" comment-dwim)
  ("/" dabbrev-expand)
  ("b" counsel-switch-buffer)
  ("q" query-replace)
  ("d" deadgrep)
  ("x" xref-find-definitions)
  ("S" nav-bm-set-file)
  ("m" nav-bm-capture)
  ("g" counsel-nav-bm)
  ("W" jnk-keys-windows-frames/body)
  ("F" jnk-keys-file-buffer/body)
  )

(provide 'jnk-keys)
