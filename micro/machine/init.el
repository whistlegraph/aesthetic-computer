;; Aesthetic Computer Emacs Configuration, 2024.3.13.12.51

(setq inhibit-startup-screen t) ;; Disable startup message.
(setq eshell-banner-message "") ;; No eshell banner.

;; (load-theme 'wombat t) ;; Set a dark theme.
(setq initial-scratch-message nil) ;; Empty scratch buffer message.
(global-display-line-numbers-mode) ;; Always show line numbers.

(menu-bar-mode -1) ;; Disable the menu bar.

(when (window-system)
  (tool-bar-mode -1) ;; Disable the tool bar.
  (fringe-mode 0) ;; Disable fringe indicators.
  (scroll-bar-mode -1)) ;; Disable scroll bar.

(setq-default line-spacing 0)
(xterm-mouse-mode 1)
(defun track-mouse (e))
(setq mouse-sel-mode t)
(setq ring-bell-function 'ignore) ;; Ignore scroll bell.

;; Set-up a better backup directory.
(defvar my-backup-directory "~/.emacs.d/backups/")
(unless (file-exists-p my-backup-directory)
  (make-directory my-backup-directory t))
(setq backup-directory-alist `(("." . ,my-backup-directory)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq vc-follow-symlinks t)

;; Initialize package sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install and configure use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Evil mode configuration
(use-package evil
  :config
  (evil-mode 1)
  (setq-default evil-shift-width 2))

(use-package s :ensure t) ;; `dockerfile-mode` depends on `s`.
(use-package dockerfile-mode :ensure t) ;; Dockerfile support.
(use-package fish-mode :ensure t) ;; Fish shell syntax.
;; (use-package gptel :ensure t) ;; ChatGPT / LLM support. 
(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (getenv "OPENAI_API_KEY")))))
;; ^ Set via `set -Ux OPENAI_API_KEY "your_api_key_here"` in fish shell.

;; Prettier-js configuration
(use-package prettier-js
  :hook (js-mode . prettier-js-mode)
  :bind ("C-c p" . prettier-js))

;; Add more use-package blocks for other packages as needed

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode)) ;; Support mjs files.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(gptel prettier-js evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
