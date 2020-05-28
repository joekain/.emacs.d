;; Swap alt and command keys on mac.
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal stuff
(unless (display-graphic-p)
  (require 'mwheel)
  (require 'mouse)
  (xterm-mouse-mode t)
  (mouse-wheel-mode t)
  (global-set-key [mouse-4] 'scroll-down-line)
  (global-set-key [mouse-5] 'scroll-up-line)
  )

(load-theme 'jnk-monokai-white t)

;; Bootstrap straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Enable and configure packages

(use-package key-chord
  :straight t)

(use-package use-package-chords
  :straight t
  :config (key-chord-mode -1))

(use-package prescient
  :straight t
  :config
  (prescient-persist-mode t))

;; ivy/counsel - not primary selection
;; Used for a few special functions and in some of my own functions.
(use-package counsel
  :straight t
  :config
    (setq ivy-use-virtual-buffers t
	  enable-recursive-minibuffers t
	  ivy-initial-inputs-alist nil)
    (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
    (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done))

(use-package ivy-prescient
  :straight t
  :after counsel
  :config
    (prescient-persist-mode t)
    (ivy-prescient-mode 1))

;; Selectrum used as primary selection
(use-package selectrum
  :straight t
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :straight t
  :config (selectrum-prescient-mode 1))

(use-package orderless
  :straight t
  :config
  (setq selectrum-refine-candidates-function #'orderless-filter
	selectrum-highlight-candidates-function #'orderless-highlight-matches)
  :custom (completion-styles '(orderless)))

(use-package recentf
  :straight t
  :config
    (setq recentf-max-saved-items 500
	  recentf-max-menu-items 15
	  recentf-exclude '("^/var/folders\\.*"
			    "COMMIT_EDITMSG\\'"
			    ".*-autoloads\\.el\\'"
			    "[/\\]\\.elpa/"
			    )
	  )
    (recentf-mode +1))

(use-package ace-window
  :straight t
  :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(use-package org
  :straight t
  :config
  (add-hook 'org-mode-hook
	    (lambda ()
	      (when (version<= "26.0.50" emacs-version )
		(display-line-numbers-mode -1))
	      (setq org-log-done 'time)))
  (setq org-startup-folded t)
  )

(use-package org-ql
  :straight t)

(use-package yasnippet
  :straight t
  :config (yas-global-mode 1))

(use-package dumb-jump
  :straight t
  :config (setq dumb-jump-selector 'completing-read))

(use-package keyfreq
  :straight t
  :config
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1))

(use-package deadgrep
  :straight t)

(use-package smartparens-config
  :straight smartparens
  :config
    (smartparens-global-mode t)
    (show-smartparens-global-mode t))

(use-package magit
  :straight t)

(use-package avy
  :straight t)

(tool-bar-mode -1)

(setq-default show-trailing-whitespace t)

(use-package major-mode-hydra
  :straight t)

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook))

(use-package undo-tree
  :straight t
  :config (global-undo-tree-mode -1))

(use-package ws-butler
  :straight t
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'org-mode-hook #'ws-butler-mode)
  )

(use-package clang-format+
  :straight t
  :config (add-hook 'c-mode-common-hook #'clang-format+-mode))

(use-package ctrlf
  :straight t
  :config (ctrlf-mode -1))

(use-package perspective
  :straight t
  :config (persp-mode -1))  ;; Disabled for now, try it out manually

(use-package expand-region
  :straight t)

(use-package crux
  :straight t
  :bind ("C-c f" . crux-recentf-find-file))

(use-package abbrev
  :custom
  (abbrev-file-name (expand-file-name ".abbrev_defs" user-emacs-directory))
  (abbrev-mode 1)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package flyspell
  :custom
  (flyspell-abbrev-p t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :config
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'text-mode-hook #'turn-on-flyspell)
  (add-hook 'org-mode-hook  #'turn-on-flyspell))

(use-package flyspell-correct
  :straight t
  :after flyspell)

(use-package company
  :straight t
  :config
  (setq
   company-dabbrev-downcase nil
   company-idle-delay 0.2
   company-minimum-prefix-length 1
   company-selection-wrap-around t
   company-tooltip-flip-when-above t
   company-tooltip-align-annotations t)
  (company-tng-configure-default)
  (global-company-mode 1))

(use-package company-prescient
  :straight t
  :config (company-prescient-mode 1))

;; (use-package company-fuzzy
;;   :straight t
;;   :config
;;   (global-company-fuzzy-mode 1))

(use-package savehist
  :config (savehist-mode 1))

;; (use-package literate-calc-mode
;;   :straight t
;;   :config
;;     (add-hook 'org-mode-hook #'literate-calc-minor-mode))

(use-package undo-fu
  :straight t
  :bind ("C-x u" . undo-fu-only-undo))

(use-package keycast
  :straight t)

(use-package anaconda-mode
  :straight t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package company-anaconda
  :straight t
  :after company
  :config
  (add-to-list 'company-backends 'company-anaconda))

;; Modules in lisp/ directory
(add-to-list 'load-path "~/.emacs.d/lisp")
(use-package agenda)

(use-package file-list)

(use-package nav-bm
  :config (nav-bm-init))

(use-package jnk-dashboard)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package kill-current-buffer
  :config (global-set-key (kbd "C-x k") 'kill-current-buffer))

(use-package jnk-keys
  :config (jnk-keys-minor-mode 1)
  :bind (:map org-mode-map
	      ("M-h" . #'jnk-keys-org-wrapper)))


;; Allow local customization in local/local.el
(add-to-list 'load-path "~/.emacs.d/local")
(load "local")

(setq custom-file "~/.emacs.d/custom.el")
