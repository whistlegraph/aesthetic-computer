;; Aesthetic Computer Emacs Configuration, 2024.3.13.12.51

;; Open emacs maximized and with undecorated window.
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(undecorated . t))

(when (window-system)
  (load-theme 'wombat t))

;; Only show emergency warnings.
(setq warning-minimum-level :emergency)

(tab-bar-mode t) ;; Enable the tab bar mode.
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-hints t)
(setq tab-bar-format '(;; tab-bar-format-history
                       tab-bar-format-tabs
                       tab-bar-separator))
(setq tab-bar-close-button-show nil)
;; (setq tab-line-tab-max-width 20) ; Adjust the number as needed

(setq scroll-step 1)

(setq-default display-fill-column-indicator-column 80) ;; Vertical guide-line.
(add-hook 'prog-mode-hook (lambda () (display-fill-column-indicator-mode 1)))

;; (custom-set-faces
;;  '(fill-column-indicator ((t (:foreground "yellow")))))
;;  (electric-pair-local-mode 1))

;; Enable electric-pair mode in prog modes.
(defun enable-electric-pairs ()
  (setq-local electric-pair-pairs '((?\{ . ?\}) (?\( . ?\)) (?\[ . ?\]) (?' . ?') (?` . ?`)))
  (electric-pair-local-mode 1))
(add-hook 'prog-mode-hook 'enable-electric-pairs)

;; Auto-save support while editing files derived from prog-mode or in text-mode;
(auto-save-visited-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun auto-save-buffer ()
  (when (or (derived-mode-p 'prog-mode)
            (derived-mode-p 'text-mode))
    (save-buffer)))

(when (window-system)
  (add-hook 'window-configuration-change-hook
	    (lambda ()
	      (unless (minibuffer-window-active-p (minibuffer-window))
		(auto-save-buffer))))
  (add-hook 'focus-out-hook 'auto-save-buffer))

(custom-set-faces
 '(tab-bar ((t (:height 1.0)))))

(setq inhibit-startup-screen t) ;; Disable startup message.
(setq eshell-banner-message "") ;; No eshell banner.

(setq initial-scratch-message nil) ;; Empty scratch buffer message.

(global-display-line-numbers-mode) ;; Always show line numbers.

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode)) ;; Support mjs files.

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

(when (window-system)
  (if (display-graphic-p)
    (scroll-bar-mode -1)
    (fringe-mode 0)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(select-frame frame)
		(when (display-graphic-p)
		  (fringe-mode 0)
		  (scroll-bar-mode -1))))))

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

;; Set-up a directory for lock files
(defvar my-lockfiles-directory "~/.emacs.d/lockfiles/")
(unless (file-exists-p my-lockfiles-directory)
  (make-directory my-lockfiles-directory t))
(setq lock-file-name-transforms `((".*" ,my-lockfiles-directory t)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq vc-follow-symlinks t)

(global-set-key (kbd "M-z") 'toggle-truncate-lines) ;; Line truncation.
(add-hook 'after-init-hook (lambda () (setq-default truncate-lines t)))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; 🌳 Tree-Sitter

;; (setq treesit-language-source-alist
;;       '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;         (html "https://github.com/tree-sitter/tree-sitter-html")
;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;         (json "https://github.com/tree-sitter/tree-sitter-json")
;;         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;         (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;         (fish "https://github.com/ram02z/tree-sitter-fish")
;;         ;;(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;         ))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

;; (require 'tree-sitter-langs)
;; (add-to-list 'tree-sitter-major-mode-language-alist '(js-mode . javascript))

;; 🪄 Packages
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (package-initialize)
;;
;; ;; Install and configure use-package
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (setq use-package-always-ensure t)
;;
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

(when (window-system)
  (use-package auto-dark)
  (setq auto-dark-dark-theme 'wombat
	auto-dark-light-theme 'whiteboard)
  (auto-dark-mode t))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; code-folding via tree-sitter
(use-package ts-fold
 :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
 :hook (tree-sitter-mode . ts-fold-mode))

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
(global-set-key (kbd "C-x C-p") #'project-find-file)

(defun my/helm-find-files-directory-handler ()
  "Open helm-find-files if Emacs is started with a directory."
  (when (and command-line-args-left (file-directory-p (car command-line-args-left)))
    (helm-find-files-1 (car command-line-args-left))
    (setq command-line-args-left nil))
  nil)

(add-to-list 'command-line-functions 'my/helm-find-files-directory-handler)

(use-package s) ;; `dockerfile-mode` depends on `s`.
(use-package dockerfile-mode) ;; Dockerfile support.
;; (use-package fish-mode) ;; Fish shell syntax.
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

;; fedora: sudo dnf install cmake libtool libvterm
;; windows: choco install cmake --installargs 'ADD_CMAKE_TO_PATH=System'
(use-package vterm)
(setq vterm-always-compile-module t)

(use-package eat)
;; (setq vterm-shell "/usr/bin/fish") ;; Use fish as the default vterm shell.

;; (desktop-save-mode 1)
;; (setq desktop-save 'if-exists)
;; (setq desktop-dirname "~/.emacs.d/desktop/")

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
  ;; (eat)
  ;; (eat-line-send-input "echo 'hi'")
  ;; (rename-buffer "eat-site")
)

(defun aesthetic-backend ()
  "Run npm commands in eat, each in a new tab named after the command. Use 'prompt' for 'shell' and 'url' in split panes, and 'stripe' for 'stripe-print' and 'stripe-ticket'."
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
          (rename-buffer (format "%s" cmd) t)))
       ;; For all other commands, create new tabs.
       (t
        (tab-new)
        (tab-rename (format "%s" cmd))
        (let ((default-directory directory-path))
          ;; Open a new terminal and send the command
          (vterm)
          (vterm-send-string (format "npm run %s\n" cmd))
          (rename-buffer (format "%s" cmd) t)))))
    )
  ;; Switch to the tab named "scratch"
  (let ((tabs (tab-bar-tabs)))
    (dolist (tab tabs)
      (when (string= (alist-get 'name tab) "shell")
        (tab-bar-switch-to-tab (alist-get 'name tab))))))
