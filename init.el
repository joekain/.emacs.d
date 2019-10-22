
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
(use-package ivy
  :ensure t
  :init
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
  :bind
    ("M-x" . counsel-M-x)
    ("C-x C-f)" . counsel-find-file))

(use-package ivy-prescient)

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


(setq custom-file "~/.emacs.d/custom.el")
