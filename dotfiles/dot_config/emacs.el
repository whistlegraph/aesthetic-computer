;; Aesthetic Computer Emacs Configuration, 2024.3.13.12.51

;; Open emacs maximized and with undecorated window.
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(undecorated . t))

(tab-bar-mode t) ;; Enable the tab bar mode.
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-hints t)
(setq tab-bar-format '(;; tab-bar-format-history
                       tab-bar-format-tabs
                       tab-bar-separator))
(setq tab-bar-close-button-show nil)
;; (setq tab-line-tab-max-width 20) ; Adjust the number as needed

;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(tab-bar ((t (:height 1.0)))))

(setq inhibit-startup-screen t) ;; Disable startup message.
(setq eshell-banner-message "") ;; No eshell banner.

(setq initial-scratch-message nil) ;; Empty scratch buffer message.

(global-display-line-numbers-mode) ;; Always show line numbers.

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode)) ;; Support mjs files.

(defun disable-line-numbers-in-modes ()
  "Disable line numbers in eshell and vterm."
  (when (or (derived-mode-p 'eshell-mode)
            (derived-mode-p 'vterm-mode))
    (display-line-numbers-mode -1)))

;; Adding hooks for both eshell and vterm
(add-hook 'eshell-mode-hook 'disable-line-numbers-in-modes)
(add-hook 'vterm-mode-hook 'disable-line-numbers-in-modes)

;; Set the default shell for Windows to use bash on WSL. 
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:/Windows/System32/bash.exe")
  (setq shell-file-name explicit-shell-file-name)
  (setenv "SHELL" shell-file-name)
  (add-to-list 'exec-path "C:/Windows/System32")
  )

;; Only show emergency warnings.
(add-hook 'after-init-hook
          (lambda ()
            (setq warning-minimum-level :emergency)))

(when (window-system)
  (fringe-mode 0) ;; Disable fringe indicators.
  (scroll-bar-mode -1)) ;; Disable scroll bar.

(menu-bar-mode -1) ;; Disable the menu bar.
(tool-bar-mode -1) ;; Disable the tool bar.

;; (setq-default line-spacing 0)
(xterm-mouse-mode 1)
(defun track-mouse (e))
(setq mouse-sel-mode t)
(setq ring-bell-function 'ignore) ;; Ignore scroll bell.

;; Set-up a better backup directory.
(defvar my-backup-directory "~/.emacs.d/backups/")
(unless (file-exists-p my-backup-directory)
  (make-directory my-backup-directory t))
(setq backup-directory-alist `(("." . ,my-backup-directory)))

;; Set-up a directory for auto-save files.
(defvar my-auto-save-directory "~/.emacs.d/auto-saves/")
(unless (file-exists-p my-auto-save-directory)
  (make-directory my-auto-save-directory t))
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-directory t)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq vc-follow-symlinks t)

(global-set-key (kbd "M-z") 'toggle-truncate-lines) ;; Line truncation. 
(add-hook 'after-init-hook (lambda () (setq-default truncate-lines t)))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; 🪄 Packages
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (package-initialize)

;; Install and configure use-package
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (setq use-package-always-ensure t)

(setq package-enable-at-startup nil)

;; Add 'straight package manager.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
		    "straight/repos/straight.el/bootstrap.el"
			  (or (bound-and-true-p straight-base-dir)
					  user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	      (url-retrieve-synchronously
		     "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
				 'silent 'inhibit-cookies)
		  (goto-char (point-max))
			(eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

;; (use-package eat)

(use-package helm ;; Add helm: https://github.com/emacs-helm/helm/wiki#from-melpa  
  ;; :straight t
  :config
  (setq helm-M-x-fuzzy-match t) ;; Optional: Fuzzy match for M-x
  (setq helm-mode-fuzzy-match t) ;; Optional: Fuzzy match for helm-mode
  (setq helm-split-window-in-side-p t) ;; Optional: Have helm open in current window.
  (setq helm-ff-fuzzy-matching t) ;; Enable fuzzy matching for file and buffer names
  (helm-mode 1))

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-p") #'project-find-file) ;; C-p everywhere

(defun my/helm-find-files-directory-handler ()
  "Open helm-find-files if Emacs is started with a directory."
  (when (and command-line-args-left (file-directory-p (car command-line-args-left)))
    (helm-find-files-1 (car command-line-args-left))
    (setq command-line-args-left nil))
  nil)

(add-to-list 'command-line-functions 'my/helm-find-files-directory-handler)

(use-package s) ;; `dockerfile-mode` depends on `s`.
(use-package dockerfile-mode) ;; Dockerfile support.
(use-package fish-mode) ;; Fish shell syntax.
;; (use-package gptel) ;; ChatGPT / LLM support. 
(use-package chatgpt-shell
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (getenv "OPENAI_API_KEY")))))
;; ^ Set via `set -Ux OPENAI_API_KEY "your_api_key_here"` in fish shell.

;; Prettier-js configuration
(use-package prettier-js
  :hook (js-mode . prettier-js-mode)
  :bind ("C-c p" . prettier-js))

;; Use good clipboard system in terminal mode.
(use-package clipetty
 :ensure t
 :hook (after-init . global-clipetty-mode))

;; Evil mode configuration
(use-package evil
  :config
  (evil-mode 1)
  (setq-default evil-shift-width 2)
  ;; override C-p in evil mode
  (dolist (state '(normal insert visual motion emacs))
    (evil-define-key state 'global (kbd "C-p") 'project-find-file)))

(unless (display-graphic-p)
        (use-package evil-terminal-cursor-changer)
        (require 'evil-terminal-cursor-changer)
        (evil-terminal-cursor-changer-activate))

(use-package restart-emacs) ;; Fully restart emacs: https://github.com/iqbalansari/restart-emacs 
(setq restart-emacs-restore-frames t)
(global-set-key (kbd "C-c C-r") 'restart-emacs)

(global-set-key (kbd "C-c C-o") 'browse-url-at-point) ;; Open url.

(use-package auto-dark)
(setq auto-dark-dark-theme 'wombat
      auto-dark-light-theme 'whiteboard)
(auto-dark-mode t)

;; (use-package burly)

;; This package breaks terminal rendering :(
;; (use-package gruvbox-theme)
;; (load-theme 'whiteboard t) ;; Set a theme.

;; 🫀 Aesthetic Computer Layouts

;; Function to open eshell and run redis-server
(defun aesthetic ()
  "Open eshell and run redis-server."
  (interactive)
  (eshell)
  (insert "python3 -m http.server 8888")
  (eshell-send-input))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vterm evil-terminal-cursor-changer restart-emacs prettier-js helm fish-mode evil dockerfile-mode clipetty chatgpt-shell)))

;; (desktop-save-mode 1)
;; (setq desktop-save 'if-exists)
;; (setq desktop-dirname "~/.emacs.d/desktop/")

;; fedora: sudo dnf install cmake libtool libvterm
;; windows:  choco install cmake --installargs 'ADD_CMAKE_TO_PATH=System'
(use-package vterm)

;; (defun open-fish-or-eshell-if-no-file ()
;;   "Open vterm with fish shell or eshell if no file is specified in the arguments."
;;   (unless (or (not command-line-args-left)      ;; If there are no arguments left.
;;               (cdr command-line-args-left))     ;; Or if there's more than one argument.
;;     (let ((fish-path (executable-find "fish")))
;;       (if fish-path
;;           (vterm fish-path "fish")
;;         (eshell)))))
;; 
;; (add-hook 'emacs-startup-hook 'open-fish-or-eshell-if-no-file)

;; (defun disable-evil-in-vterm ()
;;  (evil-local-mode -1))

;; (add-hook 'vterm-mode-hook 'disable-evil-in-vterm)

;; Kill any active processes when quitting emacs.
(setq confirm-kill-processes nil)

(defun aesthetic-internal ()
  "Run aesthetic servers in a docker container that's running emacs."
  (interactive)
  ;; Open a terminal.
  (vterm)
  (vterm-send-string (format "ac-site\n" cmd))
  (rename-buffer (format "vterm-site" cmd) t)
)

(defun aesthetic-backend ()
  "Run npm commands in vterm, each in a new tab named after the command. Use 'prompt' for 'shell' and 'url' in split panes, and 'stripe' for 'stripe-print' and 'stripe-ticket'."
  (interactive)
  ;; Define the directory path
  (let ((directory-path "~/Desktop/code/aesthetic-computer/micro")
        (commands '("shell" "site" "session" "redis" "edge" "stripe-print" "stripe-ticket"))
        prompt-tab-created stripe-tab-created)
    ;; Iterate over the commands
    (tab-rename "source")
    (find-file "~/Desktop/code/aesthetic-computer/README.txt")
    (dolist (cmd commands)
      (cond
       ;; For 'shell' and 'url', split the 'prompt' tab
       ;;((or (string= cmd "shell") (string= cmd "url"))
       ;; (unless prompt-tab-created
       ;;   (tab-new)
       ;;   (tab-rename "prompt")
       ;;   (setq prompt-tab-created t))
       ;; (when (string= cmd "url")
       ;;   ;;(split-window-right)
       ;;   (split-window-below)
       ;;   (other-window 1))
       ;; (let ((default-directory directory-path))
       ;;   ;; Open a new vterm and send the command
       ;;   (vterm)
       ;;   (vterm-send-string (format "npm run %s\n" cmd))
       ;;   (rename-buffer (format "vterm-%s" cmd) t)))
       ;; For 'stripe-print' and 'stripe-ticket', split the 'stripe' tab vertically
       ((or (string= cmd "stripe-print") (string= cmd "stripe-ticket"))
        (unless stripe-tab-created
          (tab-new)
          (tab-rename "stripe")
          (setq stripe-tab-created t))
        (when (string= cmd "stripe-ticket")
          (split-window-below)
          (other-window 1))
        (let ((default-directory directory-path))
          ;; Open a new vterm and send the command
          (vterm)
          (vterm-send-string (format "npm run %s\n" cmd))
          (rename-buffer (format "vterm-%s" cmd) t)))
       ;; For other commands, create new tabs
       (t
        (tab-new)
        (tab-rename (format "%s" cmd))
        (let ((default-directory directory-path))
          ;; Open a new vterm and send the command
          (vterm)
          (vterm-send-string (format "npm run %s\n" cmd))
          (rename-buffer (format "vterm-%s" cmd) t)))))
    )
  ;; Switch to the tab named "scratch"
  (let ((tabs (tab-bar-tabs)))
    (dolist (tab tabs)
      (when (string= (alist-get 'name tab) "shell")
        (tab-bar-switch-to-tab (alist-get 'name tab))))))
