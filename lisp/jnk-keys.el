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
  ("s" consult-line)
  ("x" exchange-point-and-mark)

  ("W" crux-move-beginning-of-line :exit t)
  ("E" move-end-of-line :exit t)
  ("F" forward-word :exit t)
  ("A" backward-word :exit t)
  ("D" er/expand-region :exit t)
  ("S" consult-line :exit t)
  ("X" exchange-point-and-mark :exit t)

  ("g" goto-line)
  ("1" beginning-of-buffer)
  ("2" end-of-buffer)
  ("c" backward-sexp)
  ("v" forward-sexp)

  ("G" goto-line :exit t)
  ("!" beginning-of-buffer :exit t)
  ("@" end-of-buffer :exit t)
  ("C" backward-sexp :exit t)
  ("V" forward-sexp :exit t)

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

  ("h" org-insert-heading-respect-content :exit t)  ;; default to exit
  ("j" org-shiftmetaleft)
  ("l" org-shiftmetaright)
  ("i" org-metaup)
  ("k" org-metadown)
  ("H" org-insert-heading-respect-content)
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
_s_: save  _u_: undo    _;_: comment    _p_: query  _k_ kill line
_f_: fill  _r_: redo    _/_: complete   _d_: rg     _n_ spell
_c_: cap   _j_: recent  _b_: buffer     _x_: xref   _a_ rg cwd
_o_: open  _y_: yank    ^ ^             _R_: Region _F_: Files / Buffers
"
  ("s" save-buffer)
  ("f" unfill-toggle)
  ("c" org-capture)
  ("o" magit-find-file-completing-read)
  ("j" consult-recent-file)
  ("u" undo-fu-only-undo :color pink)
  ("r" undo-fu-only-redo :color pink)
  ("U" undo-fu-only-undo)
  ("R" undo-fu-only-redo)
  ("y" consult-yank)
  (";" comment-dwim)
  ("/" dabbrev-expand :color pink)
  ("?" dabbrev-expand)
  ("b" consult-buffer)
  ("p" query-replace)
  ("d" deadgrep)
  ("x" xref-find-definitions)
  ("W" jnk-keys-windows-frames/body)
  ("R" jnk-keys-move-region-wrapper)
  ("F" jnk-keys-file-buffer/body)
  ("k" crux-kill-whole-line :color pink)
  ("K" crux-kill-whole-line)
  ("n" jnk-keys-flyspell-correct-rapid)
  ("a" rg-cwd)
  ("q" nil :exit t)
  )

(defun rg-cwd ()
  (interactive)
  (let ((deadgrep-project-root-function (lambda () default-directory)))
    (call-interactively 'deadgrep))
  )

(defun jnk-keys-flyspell-correct-rapid ()
  (interactive)
  (let ((current-prefix-arg '(4)))  ; C-u to enable rapid-mode
    (call-interactively 'flyspell-correct-wrapper)))


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


(defhydra jnk-keys-dumb-jump (:color blue :hint nil)
  "
_g_: go   _b_: back  _l_: look  _x_: go external  _p_: go prompt
"
  ("g" dumb-jump-go)
  ("b" dumb-jump-back)
  ("l" dumb-jump-quick-look)
  ("x" dumb-jump-go-prefer-external)
  ("p" dumb-jump-go-prompt)
  ("q" nil)
  )

(defhydra jnk-keys-bookmarks (:color amaranth :hint nil)
  "
Insert: _p_: project  _t_: task  _s_: scope   _i_: no tag  ║  _q_/_<return>_: accept
Filter: _P_: project  _T_: task  _S_: scope   ^ ^: cycle   ║  _<ESC>_: cancel
"
  ("<SPC>" bmkp-cycle)
  ("i" bmkp-bookmark-set-confirms-overwrite :exit t)
  ("p" jnk-bookmarks-project-new-bookmark :exit t)
  ("t" jnk-bookmarks-task-new-bookmark :exit t)
  ("s" jnk-bookmarks-scope-new-bookmark :exit t)
  ("P" jnk-bookmarks-project-set)
  ("T" jnk-bookmarks-task-set)
  ("S" jnk-bookmarks-scope-set)
  ("<ESC>" jnk-keys-return :exit t)
  ("q" nil)
  ("<return>" nil))

(defun jnk-keys-bookmarks-wrapper ()
  "A wrapper around the jnk-keys-bookmarks hydra that saves the
   initial location.  The location can be restored by the hydra
   if desired."
  (interactive)

  (setq jnk-keys-marker (point-marker))
  ;; Regenerate the list so we can just start cycling
  (jnk-bookmarks-scope-bookmark-list)
  (jnk-keys-bookmarks/body))



(defhydra jnk-keys-history (:color amaranth :hint nil)
  "
History:
^ ^: cycle  _i_: insert  _k_: kill_<ESC>_: cancel  _q_/_<return>_: accept history
"
  ("<SPC>" jnk-keys-cycle-history)
  ("i" history-add-history :exit t)
  ("k" history-kill-histories :exit t)
  ("<ESC>" jnk-keys-return :exit t)
  ("q" nil)
  ("<return>" nil))

(defun jnk-keys-cycle-history ()
  (interactive)
  (history-do
    (when history-stack
      (setq history-index (+ history-index -1))
      (cond
       ((< history-index 0)
	(setq history-index (1- (length history-stack)))))
      (history-use-current-history)
      (message (history-histories-string)))))

(defun jnk-keys-history-wrapper ()
  "A wrapper around the jnk-keys-history hydra that saves the
   initial location.  The location can be restored by the hydra
   if desired."
  (interactive)

  (setq jnk-keys-marker (point-marker))
  (jnk-keys-history/body)
  )

(defhydra jnk-keys-deadgrep (:color pink :hint nil)
  "
_i_: prev  _k_: next match _d_: deadgrep  _g_: restart _f_: kill
"
  ("i" jnk-keys-deadgrep-prev)
  ("k" jnk-keys-deadgrep-next)
  ("d" deadgrep)
  ("g" deadgrep-restart)
  ("f" deadgrep-kill-process :color blue)
  ("q" nil))

(defun jnk-keys-deadgrep-next ()
  (interactive)
  (let ((last-results-buf (car-safe (deadgrep--buffers))))
    (with-current-buffer last-results-buf
      (deadgrep-forward-match)
      (deadgrep-visit-result))))

(defun jnk-keys-deadgrep-prev ()
  (interactive)
  (let ((last-results-buf (car-safe (deadgrep--buffers))))
    (with-current-buffer last-results-buf
      (deadgrep-backward-match)
      (deadgrep-visit-result))))

(defvar jnk-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-d") 'jnk-keys-move-region-wrapper)
    (define-key map (kbd "M-j") 'jnk-keys-quick-access/body)
    (define-key map (kbd "M-c") 'jnk-keys-compile/body)
    (define-key map (kbd "M-SPC") 'jnk-keys-bookmarks-wrapper)
    map)
  "jnk-keys-minor-mode keymap.")

(define-minor-mode jnk-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " jnk-keys")

(provide 'jnk-keys)
