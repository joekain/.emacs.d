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
    (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  :bind
    ("M-x" . counsel-M-x)
    ("C-x C-f)" . counsel-find-file))

(use-package ivy-prescient
  :ensure t
  :init (setq prescient-persist-mode t))

(use-package key-chord
  :ensure t
  :config
    (key-chord-mode 1)
    (key-chord-define-global "jj" 'counsel-M-x)
    (key-chord-define-global "qq" 'fill-paragraph)
    (key-chord-define-global "hh" 'deadgrep)
  )

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
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("C-x o" . ace-window))

(use-package linum
  :ensure t
  :init (global-linum-mode 1))

(use-package yassnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package dumb-jump
  :ensure t
  :init (setq dumb-jump-selector 'ivy))

(use-package keyfreq
  :ensure t
  :init
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1))

(setq custom-file "~/.emacs.d/custom.el")
