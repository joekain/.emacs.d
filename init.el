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

;; Package setup from https://dev.to/huytd/emacs-from-scratch-1cg6
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; Enable and configure packages
(use-package counsel
  :ensure t
  :config
    (setq ivy-use-virtual-buffers t
	  enable-recursive-minibuffers t
	  ivy-initial-inputs-alist nil)
    (ivy-mode 1)
    (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
    (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
    (global-set-key (kbd "M-j") 'counsel-M-x))

(use-package prescient)
(use-package ivy-prescient
  :ensure t
  :after counsel
  :config
    (prescient-persist-mode t)
    (ivy-prescient-mode t))

(use-package key-chord
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(use-package recentf
  :ensure t
  :config
    (setq recentf-max-saved-items 500
	  recentf-max-menu-items 15
	  ;; disable recentf-cleanup on Emacs start, because it can cause
	  ;; problems with remote files
	  recentf-auto-cleanup 'never)
    (recentf-mode +1)
  :bind ("C-c f" . counsel-recentf))

(use-package ace-window
  :ensure t
  :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  :bind ("C-x o" . ace-window))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(use-package org
  :ensure t
  :config (add-hook 'org-mode-hook
		    (lambda ()
		      (when (version<= "26.0.50" emacs-version )
			(display-line-numbers-mode -1))
		      (setq org-log-done 'time))))

(use-package org-ql
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package dumb-jump
  :ensure t
  :config (setq dumb-jump-selector 'ivy))

(use-package keyfreq
  :ensure t
  :config
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1))

(use-package deadgrep
  :ensure t)

(use-package smartparens-config
  :ensure smartparens
  :config
    (smartparens-global-mode t)
    (show-smartparens-global-mode t))

(use-package magit
  :ensure t)

(use-package avy
  :ensure t)

(tool-bar-mode -1)

(setq-default show-trailing-whitespace t)

;; Local config should set
;;     (setq deft-directory "<notes>")
(use-package deft
  :ensure t)

(use-package major-mode-hydra
  :ensure t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

(use-package ws-butler
  :ensure t
  :config (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package clang-format+
  :ensure t
  :config (add-hook 'c-mode-common-hook #'clang-format+-mode))

(use-package ctrlf
  :ensure t
  :config (ctrlf-mode -1))

(use-package perspective
  :ensure t
  :config (persp-mode -1))  ;; Disabled for now, try it out manually

(use-package expand-region
  :ensure t)

(use-package crux
  :ensure t)

(use-package abbrev
  :ensure nil
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
  (flyspell-mode 1))

(use-package flyspell-correct
  :ensure t
  :after flyspell)

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell-correct)

;; Modules in lisp/ directory
(add-to-list 'load-path "~/.emacs.d/lisp")
(use-package agenda
  :ensure nil)

(use-package file-list
  :ensure nil)

(use-package nav-bm
  :ensure nil
  :config (nav-bm-init))

(use-package jnk-dashboard
  :ensure nil)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package kill-current-buffer
  :ensure nil
  :config (global-set-key (kbd "C-x k") 'kill-current-buffer))

(use-package jnk-keys
  :ensure nil
  :bind
  ("M-d" . #'jnk-keys-move-region-wrapper)
  ("M-n" . #'jnk-keys-quick-access/body)
  ("M-h" . nil)
  (:map org-mode-map
	("M-h" . #'jnk-keys-org/body))
  )

;; Allow local customization in local/local.el
(add-to-list 'load-path "~/.emacs.d/local")
(load "local")

(setq custom-file "~/.emacs.d/custom.el")
