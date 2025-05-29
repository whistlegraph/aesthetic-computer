;; Aesthetic Computer Emacs Configuration, 2024.3.13.12.51

(setq x-gtk-use-system-tooltips nil)

;; Open emacs maximized and with undecorated window.
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; (when (window-system)
;;  (load-theme 'wombat t))

;; (load-theme 'wombat t)
(setq mac-option-modifier 'meta)

(global-set-key [C-down-mouse-1] 'ignore)

;; Only show emergency warnings.
(setq warning-minimum-level :emergency)

(tab-bar-mode t) ;; Enable the tab bar mode.
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-hints t)
(setq tab-bar-format '(;; tab-bar-format-history
                       tab-bar-format-tabs
                       tab-bar-separator))
(setq tab-bar-close-button-show nil)
(setq tab-line-tab-max-width 20) ; Adjust the number as needed

(setq scroll-step 1)

(global-auto-revert-mode 1) ;; Always keep buffers up to date with disk.

;;(setq display-buffer-alist
;;      '((".*" . (display-buffer-reuse-window display-buffer-below-selected))))

(setq-default display-fill-column-indicator-column 80) ;; Vertical guide-line.
(add-hook 'prog-mode-hook (lambda () (display-fill-column-indicator-mode 1)))

;; Adding hooks for eat & eshell.
(add-hook 'eshell-mode-hook 'disable-line-numbers-in-modes)
(add-hook 'eat-mode-hook 'disable-line-numbers-in-modes)
(add-hook 'image-mode-hook 'disable-line-numbers-in-modes)

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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(fill-column-indicator ((t (:foreground "yellow"))))
 ;; '(origami-fold-face ((t (:inherit magenta :weight bold))))
 '(tab-bar ((t (:height 1.0))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :inverse-video t)))))
 ;;'(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "black")))))


;; (defun windows-custom-set-faces ()
;;  (when (eq system-type 'windows-nt)
    ;; Add your face remappings here. For example:
;;  (add-to-list 'face-remapping-alist '(tab-bar-tab (:background "orange"))))

;; (add-hook 'after-init-hook 'windows-custom-set-faces)

(setq inhibit-startup-screen t) ;; Disable startup message.
(setq eshell-banner-message "") ;; No eshell banner.

(setq initial-scratch-message nil) ;; Empty scratch buffer message.

(global-display-line-numbers-mode) ;; Always show line numbers.

(defun disable-line-numbers-in-modes ()
  "Disable line numbers in eshell and vterm."
  (when (or (derived-mode-p 'eshell-mode)
            (derived-mode-p 'eat-mode))
    (display-line-numbers-mode -1)))

;; Set the default shell for Windows to use bash on WSL.
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:/Windows/System32/bash.exe")
  (setq shell-file-name explicit-shell-file-name)
  (setenv "SHELL" shell-file-name)
  (add-to-list 'exec-path "C:/Windows/System32")
  )

;; Scroll with trackpad on my thinkpad.
(global-set-key (kbd "<mouse-4>") (lambda ()
                                   (interactive)
                                   (scroll-down 1)))

(global-set-key (kbd "<mouse-5>") (lambda ()
                                   (interactive)
                                   (scroll-up 1)))

(defun my-tab-next ()
  "Run eat handler after switching to the next tab."
  (interactive)
  (eat-tab-change #'tab-next))

(defun my-tab-previous ()
  "Run eat handler after switching to the previous tab."
  (interactive)
  (eat-tab-change #'tab-previous))

(global-set-key (kbd "C-x <right>") 'my-tab-next)
(global-set-key (kbd "C-x <left>") 'my-tab-previous)
;; For running in Windows Terminal via WSL2 I needed to add this to the JSON:
;; {
;;   "command": {
;;     "action": "sendInput",
;;     "input": "\u001b[27;6;9~"
;;   },
;;   "keys": "ctrl+shift+tab"
;; },
;; {
;;   "command": {
;;     "action": "sendInput",
;;     "input": "\u001b[27;5;9~"
;;   },
;;   "keys": "ctrl+tab"
;; }

(if (display-graphic-p)
    (when (display-graphic-p)
      (scroll-bar-mode -1)
      (fringe-mode 0)))

;; (when (not (display-graphic-p))
;; ;; set internal border width for tui
;; (setq default-frame-alist
;;   (append default-frame-alist '((internal-border-width . 10))))
;; ;; Set fringes for TUI
;; (setq-default left-fringe-width  10)
;; (setq-default right-fringe-width 10))

(setq-default scroll-bar-mode 'right)

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
(global-set-key (kbd "C-c z") 'toggle-truncate-lines) ;; Alternate...

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
;;  (package-install 'use-package))
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

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

;; Add org-mode
(use-package org)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
;; (setq org-startup-with-inline-images t)

;; TODO: Try this.
;; (defun create-and-link-org-file (filename)
;;   "Create a new org file and insert a link to it."
;;   (interactive "sEnter filename: ")
;;   (let ((filepath (concat filename ".org")))
;;     (find-file filepath)
;;     (write-file filepath)
;;     (switch-to-buffer (other-buffer))
;;     (insert (format "[[file:%s][%s]]" filepath filename))))

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;;(when (window-system)
;; (if (display-graphic-p)
;;  (use-package auto-dark)
;;  (setq auto-dark-dark-theme 'wombat
;;	auto-dark-light-theme 'whiteboard)
;;  (auto-dark-mode t)
;;  )
;;)

;; (if (display-graphic-p)
;;   (add-hook 'after-make-frame-functions
;; 	    (lambda (frame)
;; 	      (select-frame frame)
;; 	      (when (display-graphic-p)
;; 		(fringe-mode 0)
;; 		(scroll-bar-mode -1)))))

;; fedora: sudo dnf install cmake libtool libvterm
;; windows: choco install cmake --installargs 'ADD_CMAKE_TO_PATH=System'
;;(use-package vterm
	     ;; Update the module automatically:
;;	     :straight (:post-build ((let ((vterm-always-compile-module t))
;;				       (require 'vterm))))
;;	     :config
	     ;; Disable the highlighting of the current line
	     ;; for the virtual terminal:
;;	     (add-hook 'vterm-mode-hook 'disable-line-numbers-in-modes)
;;	     (setq vterm-shell "/usr/bin/fish")) ;; Use fish as the default vterm shell.

(use-package yascroll :config (global-yascroll-bar-mode 1))

;; (use-package mlscroll
;;   :config
;;   (setq mlscroll-shortfun-min-width 11) ;truncate which-func, for default mode-line-format's
;;   (mlscroll-mode 1))

;; 🌳 Tree-Sitter
;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :custom
  (treesit-auto-install t) ;; or 'prompt
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . typescript-ts-mode)) ;; Support mjs files.

;; (use-package helm ;; Add helm: https://github.com/emacs-helm/helm/wiki#from-melpa
;;       ;; :straight t
;;       :config
;;       (setq helm-M-x-fuzzy-match t) ;; Optional: Fuzzy match for M-x
;;       (setq helm-mode-fuzzy-match t) ;; Optional: Fuzzy match for helm-mode
;;       (setq helm-split-window-in-side-p t) ;; Optional: Have helm open in current window.
;;       (setq helm-ff-fuzzy-matching t) ;; Enable fuzzy matching for file and buffer names
;;       (helm-mode 1))

;; (global-set-key (kbd "M-x") #'helm-M-x)
;; (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-p") #'project-find-file) ;; C-p everywhere
(global-set-key (kbd "C-x C-p") #'project-find-file)
(global-set-key (kbd "M-p") #'project-find-file)

;; (defun my/helm-find-files-directory-handler ()
;;   "Open helm-find-files if Emacs is started with a directory."
;;   (when (and command-line-args-left (file-directory-p (car command-line-args-left)))
;;     (helm-find-files-1 (car command-line-args-left))
;;     (setq command-line-args-left nil))
;;   nil)

;; (add-to-list 'command-line-functions 'my/helm-find-files-directory-handler)

(use-package lsp-mode
  :config
  (setq lsp-auto-install-server t)
  (setq lsp-warn-no-matched-clients nil)
  ;; Disable Copilot language server
  (setq lsp-disabled-clients '(copilot-ls))
  ;; Or alternatively, remove copilot from the server list entirely
  (delete 'copilot-ls lsp-language-id-configuration))
(add-hook 'prog-mode-hook #'lsp)

(use-package origami :hook (after-init . global-origami-mode))

;; (use-package dap-mode
;;	     :config
;;	     (dap-auto-configure-mode)
;;	     :bind
;;	     (("<f7>" . dap-step-in)
;;	      ("<f8>" . dap-next)
;;	      ("<f9>" . dap-continue)))

(setq js-indent-level 2)

;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package s) ;; `dockerfile-mode` depends on `s`.
(use-package dockerfile-mode) ;; Dockerfile support.
(use-package fish-mode) ;; Fish shell syntax.

;; (use-package gptel) ;; ChatGPT / LLM support.
;; (setq gptel-api-key (getenv "OPENAI_API_KEY"))
;; Optional: set `gptel-api-key` to the API key.
;; (use-package chatgpt-shell
;;  :custom
;;  ((chatgpt-shell-openai-key
;;    (lambda ()
;;      (getenv "OPENAI_API_KEY")))))
;; ^ Set via `set -Ux OPENAI_API_KEY "your_api_key_here"` in fish shell.

;; Prettier-js configuration
(use-package prettier-js
  :hook (js-mode . prettier-js-mode)
  :bind ("C-c p" . prettier-js))

;; Use good clipboard system in terminal mode.
;; (use-package clipetty :hook (after-init . global-clipetty-mode))

;; Use xclip for system clipboard.
(use-package xclip :config (xclip-mode 1))

(defun my/xclip-set-selection (orig-fun type data)
  "Send clipboard to host from Emacs using a fish shell function, then run original xclip."
  (let ((clipboard-command (format "fish -c 'clipboard %s'" (shell-quote-argument data))))
    (start-process-shell-command "clipboard" nil clipboard-command))
  (funcall orig-fun type data))

(advice-add 'xclip-set-selection :around #'my/xclip-set-selection)

;; Evil mode configuration
(use-package evil
     :config
     (evil-mode 1)
     (setq-default evil-shift-width 2)
     ;; override C-p in evil mode
     (dolist (state '(normal insert visual motion emacs))
       (evil-define-key state 'global (kbd "C-p") 'project-find-file)))

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

(defun my/evil-quit ()
  "Enhanced quit command for Evil mode in Emacs.
   - Closes the current window unless it's the last window in the tab.
   - Kills the buffer if it's the last window showing it.
   - Closes the tab if it's the last window in the tab, unless it's the last tab.
   - Never quits Emacs."
  (interactive)
  (let ((current-buffer (current-buffer))
        (is-last-tab (eq 1 (length (tab-bar-tabs))))
        (is-last-window (eq 1 (length (window-list)))))

    ;; If it's not the last window in the tab, delete the window
    (unless is-last-window
      (delete-window))

    ;; Kill the buffer if it's not displayed elsewhere
    (unless (delq (selected-window) (get-buffer-window-list current-buffer nil t))
      (kill-buffer current-buffer))

    ;; If it's the last window in the tab but not the last tab, close the tab
    ;; (when (and is-last-window (not is-last-tab))
    ;;   (tab-bar-close-tab)))
)
  )

;; Bind the function to :q in Evil mode
(with-eval-after-load 'evil
  (evil-ex-define-cmd "q[uit]" 'my/evil-quit))

(unless (display-graphic-p)
(use-package evil-terminal-cursor-changer)
(require 'evil-terminal-cursor-changer)
(evil-terminal-cursor-changer-activate))

(use-package restart-emacs) ;; Fully restart emacs: https://github.com/iqbalansari/restart-emacs
(setq restart-emacs-restore-frames t)

(global-set-key (kbd "C-c C-r") 'restart-emacs)
(global-set-key (kbd "C-c C-o") 'browse-url-at-point) ;; Open url.

;; (use-package burly)

;; (use-package eglot :hook (web-mode . eglot-ensure))

;; (add-hook 'web-mode-hook 'eglot-ensure) ;; enable eglot for web mode automatically

(use-package eat)
(setq-default eat-shell "/usr/bin/fish")
(setq-default eat-term-name "xterm-256color")

;; This package breaks terminal rendering :(
;; (use-package gruvbox-theme)
;; (load-theme 'whiteboard t) ;; Set a theme.

;; Auto open as splits.
;; https://discourse.doomemacs.org/t/open-selected-completion-candidate-in-a-split/2525/8
;; (defun cust/vsplit-file-open (f)
;;   (let ((evil-vsplit-window-right t))
;;     (+evil/window-vsplit-and-follow)
;;     (find-file f)))
;;
;; (defun cust/split-file-open (f)
;;   (let ((evil-split-window-below t))
;;     (+evil/window-split-and-follow)
;;     (find-file f)))
;;
;; (map! :after embark
;;       :map embark-file-map
;;       "V" #'cust/vsplit-file-open
;;       "X" #'cust/split-file-open)
(defun eat-tab-change (original-fun &rest args)
  (interactive)
  (message "eat-tab-change triggered")  ; Output to *Messages* buffer
  (apply original-fun args)
  (walk-windows (lambda (window)
                  (with-current-buffer (window-buffer window)
                    (when (eq major-mode 'eat-mode)
                      (end-of-buffer)
                      (evil-insert-state))))
               nil 'visible))

(advice-add 'tab-bar-select-tab :around #'eat-tab-change)

(defun kill-eat-processes ()
  "Kill processes associated with eat terminal buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'eat-mode)
        (eat-kill-process)))))

(add-hook 'kill-emacs-hook #'kill-eat-processes)

;; 🫀 Aesthetic Computer Layouts

;; Function to open eshell and run redis-server
(defun aesthetic ()
  "Open eshell and run redis-server."
  (interactive)
  (eshell)
  (insert "python3 -m http.server 8888")
  (eshell-send-input))

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
;; (evil-local-mode -1))

;; (add-hook 'vterm-mode-hook 'disable-evil-in-vterm)


;; (defun aesthetic-internal ()
;;  "Run aesthetic servers in a docker container that's running emacs."
;;  (interactive)
;;  (eat "fish -c 'npm run site'")
;;  (with-current-buffer "*eat*" (rename-buffer "eat-site" t)))

(setq confirm-kill-processes nil) ;; Avoid confirmation for killing processes

;; Bind the shutdown function to a key (e.g., C-c C-x for example purposes)
;; (global-set-key (kbd "C-c C-x") 'shutdown-emacs-server)

;; Auto-scroll eat buffers in all windows
(add-hook 'eat-mode-hook
          (lambda ()
            (setq-local comint-scroll-to-bottom-on-output       t
                        comint-show-maximum-output            t
                        comint-move-point-for-output          t
                        window-point-insertion-type           t)
            (add-hook 'comint-output-filter-functions
                      #'comint-postoutput-scroll-to-bottom
                      nil t)))

(defun aesthetic-backend (target-tab)
  "Run npm commands in eat, each in a new tab named after the command.
Creates split tabs for 'status' (url + tunnel), 'stripe' (stripe-print + stripe-ticket), and 'chat' (chat-system + chat-sotce + chat-clock)."
  (interactive)
  (let ((directory-path "~/aesthetic-computer")
        (commands '("site" "session" "redis" "servers" "kidlisp"))
        (emoji-for-command
         '(("source" . "📂") ("status" . "📡") ("url" . "🌐") ("tunnel" . "🚇")
           ("site" . "📰") ("session" . "🔒") ("redis" . "🔄")
           ("stripe-print" . "💳 🖨️") ("stripe-ticket" . "💳🎫")
           ("servers" . "🤖") ("chat-system" . "🧭") ("chat-sotce" . "🪷") ("chat-clock" . "🕑")
           ("kidlisp" . "🐍"))))

    ;; Clean up unwanted buffers
    (dolist (bufname '("*scratch*" "*Messages*" "*straight-process*" "*async-native-comp*"))
      (when-let ((buf (get-buffer bufname)))
        (dolist (win (get-buffer-window-list buf nil t))
          (with-selected-window win
            (switch-to-buffer (other-buffer buf t))))
        (kill-buffer buf)))

    ;; Rename current tab and show TODO
    (tab-rename "📂 source")
    (find-file "~/aesthetic-computer/TODO.txt")

    (cl-labels
        ((run-split-tab (tab-name &rest cmds)
           (tab-new)
           (tab-rename tab-name)
           (let ((default-directory directory-path))
             (delete-other-windows)
             (let ((first t))
               (dolist (cmd cmds)
                 (unless first (split-window-right) (other-window 1))
                 (setq first nil)
                 (eat (format "fish -c 'ac-%s'" cmd))
                 (with-current-buffer "*eat*"
                   (rename-buffer
                    (format "%s-%s"
                            (cdr (assoc cmd emoji-for-command))
                            cmd) t))
                 (goto-char (point-max)))
               (balance-windows)
               (other-window 1)))))

      ;; primary splits
      (run-split-tab "📡 status"     "url"      "tunnel")
      (run-split-tab "💳 stripe"     "stripe-print" "stripe-ticket")
      (run-split-tab "💬 chat"       "chat-system"  "chat-sotce" "chat-clock")

      ;; individual tabs
      (dolist (cmd commands)
        (tab-new)
        (tab-rename (format "%s %s"
                            (cdr (assoc cmd emoji-for-command))
                            cmd))
        (let ((default-directory directory-path))
          (eat (format "fish -c 'ac-%s'" cmd))
          (with-current-buffer "*eat*"
            (rename-buffer
             (format "%s-%s"
                     (cdr (assoc cmd emoji-for-command))
                     cmd) t))
          (goto-char (point-max))
          (dolist (win (get-buffer-window-list (current-buffer) nil t))
            (set-window-point win (point-max))))))

    ;; switch to requested tab
    (let ((tab-emoji (cdr (assoc target-tab emoji-for-command))))
      (if tab-emoji
          (tab-bar-switch-to-tab (format "%s %s" tab-emoji target-tab))
        (message "No such tab: %s" target-tab)))))
