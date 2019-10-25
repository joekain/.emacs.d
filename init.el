;; Swap alt and command keys on mac.
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  )

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
    (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done))

(use-package prescient)
(use-package ivy-prescient
  :ensure t
  :after counsel
  :config
    (prescient-persist-mode t)
    (ivy-prescient-mode t))

(use-package key-chord
  :ensure t
  :config
    (key-chord-define-global "jj" 'counsel-M-x)
    )


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
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("C-x o" . ace-window))

(use-package linum
  :ensure t
  :config
    (global-linum-mode 1)
    (add-hook 'org-mode-hook (lambda () (linum-mode -1))))

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

(use-package hydra
  :ensure t
  :config (key-chord-define-global "vv"
	   (defhydra hydra-quick ()
	      "Quick Access Commands"
	      ("q" query-replace "query-replace")
	      ("d" deadgrep "deadgrep")
	      ("f" fill-paragraph "fill-paragraph")
	      ("m" make-frame "make-frame"))))


(use-package smartparens-config
  :ensure smartparens
  :config
    (smartparens-global-mode t)
    (show-smartparens-global-mode t))

(use-package magit
  :ensure t)

(tool-bar-mode -1)

(setq-default show-trailing-whitespace t)


;; Allow local customization in local/local.el
(add-to-list 'load-path "~/.emacs.d/local")
(load "local")

(setq custom-file "~/.emacs.d/custom.el")
