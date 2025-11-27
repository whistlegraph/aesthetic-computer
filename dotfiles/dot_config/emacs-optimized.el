;; Aesthetic Computer Emacs Configuration (Optimized), 2025.11.17
;; Performance-focused configuration with deferred compilation and lazy loading

;;; ===================================================================
;;; PERFORMANCE OPTIMIZATIONS (Applied Early)
;;; ===================================================================

;; Debug logging
(defvar ac-debug-log-file "/tmp/emacs-debug.log")
(defun ac-debug-log (message)
  (with-temp-buffer
    (insert (format "[%s] %s\n" (format-time-string "%Y-%m-%d %H:%M:%S.%3N") message))
    (append-to-file (point-min) (point-max) ac-debug-log-file)))

(ac-debug-log "=== Starting Optimized Emacs Configuration ===")

;; CRITICAL: Native compilation settings (prevents blocking compiles)
(ac-debug-log "Configuring native compilation for optimal performance...")
(setq native-comp-speed 2                        ; Balance speed vs optimization
      native-comp-async-report-warnings-errors nil ; Silent async compilation
      native-comp-deferred-compilation t         ; Defer compilation to idle time
      native-comp-always-compile nil             ; Don't force compile everything
      comp-deferred-compilation t                ; Enable deferred compilation
      comp-async-compilation t)                  ; Compile in background

;; Increase GC threshold during startup (restore later)
(defvar ac--original-gc-cons-threshold gc-cons-threshold)
(defvar ac--original-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum  ; ~800MB before GC runs
      gc-cons-percentage 0.6                  ; GC when heap grows 60%
      file-name-handler-alist nil)            ; Disable file handlers during init

;; Restore GC settings after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (ac-debug-log "Restoring GC thresholds post-startup")
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB (reasonable for work)
                  gc-cons-percentage 0.1
                  file-name-handler-alist ac--original-file-name-handler-alist)
            ;; Run GC when idle for 5 seconds
            (run-with-idle-timer 5 t #'garbage-collect)))

;; Reduce startup noise
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      warning-minimum-level :emergency)

;; Misc performance settings
(setq x-gtk-use-system-tooltips nil
      mac-option-modifier 'meta
      read-process-output-max (* 1024 1024))  ; 1MB read chunks (faster subprocess I/O)

(ac-debug-log "Performance settings applied")

;;; ===================================================================
;;; UI SETTINGS (Fast, No Package Dependencies)
;;; ===================================================================

(ac-debug-log "Configuring UI...")

;; Disable GUI cruft immediately
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (display-graphic-p)
  (fringe-mode 0))

;; Global keybindings (no modifier conflicts)
(global-set-key [C-down-mouse-1] 'ignore)
(global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-z") 'toggle-truncate-lines)
(global-set-key (kbd "C-c z") 'toggle-truncate-lines)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-p") #'project-find-file)
(global-set-key (kbd "C-x C-p") #'project-find-file)
(global-set-key (kbd "M-p") #'project-find-file)

;; Tab bar
(tab-bar-mode t)
(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-hints t
      tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
      tab-bar-close-button-show nil
      tab-line-tab-max-width 20)

;; Tab navigation
(global-set-key (kbd "C-x <right>") 'tab-next)
(global-set-key (kbd "C-x <left>") 'tab-previous)

;; Display settings
(setq scroll-step 1
      scroll-conservatively 10000)
(global-auto-revert-mode 1)
(setq auto-revert-interval 5
      auto-revert-verbose nil)  ; Silent auto-revert

;; Line numbers (deferred to prog-mode)
(add-hook 'prog-mode-hook 
          (lambda () 
            (display-line-numbers-mode 1)
            (setq-default display-fill-column-indicator-column 80)
            (display-fill-column-indicator-mode 1)))

;; Disable line numbers in specific modes
(dolist (mode '(eshell-mode-hook eat-mode-hook image-mode-hook term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; Electric pairs (lightweight, no package)
(electric-pair-mode 1)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local electric-pair-pairs 
                        '((?\{ . ?\}) (?\( . ?\)) (?\[ . ?\]) (?' . ?') (?` . ?`)))))

;; Auto-save and backups (organized directories)
(auto-save-visited-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defvar my-backup-directory "~/.emacs.d/backups/")
(defvar my-auto-save-directory "~/.emacs.d/auto-saves/")
(defvar my-lockfiles-directory "~/.emacs.d/lockfiles/")

(dolist (dir (list my-backup-directory my-auto-save-directory my-lockfiles-directory))
  (unless (file-exists-p dir) (make-directory dir t)))

(setq backup-directory-alist `(("." . ,my-backup-directory))
      auto-save-file-name-transforms `((".*" ,my-auto-save-directory t))
      lock-file-name-transforms `((".*" ,my-lockfiles-directory t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Throttled auto-save on focus-out (GUI only)
(when (window-system)
  (defvar auto-save-throttle-timer nil)
  (defun auto-save-buffer ()
    (when (or (derived-mode-p 'prog-mode)
              (derived-mode-p 'text-mode))
      (save-buffer)))
  
  (defun throttled-auto-save ()
    (when auto-save-throttle-timer
      (cancel-timer auto-save-throttle-timer))
    (setq auto-save-throttle-timer
          (run-with-timer 3 nil #'auto-save-buffer)))
  
  (add-hook 'focus-out-hook 'auto-save-buffer))

;; Editor behavior
(setq-default indent-tabs-mode nil
              tab-width 2
              truncate-lines t)
(setq vc-follow-symlinks t
      js-indent-level 2
      confirm-kill-processes nil)

;; Mouse and bell
(xterm-mouse-mode 1)
(defun track-mouse (e))
(setq mouse-sel-mode t
      ring-bell-function 'ignore)

;; Custom faces
(custom-set-faces
 '(tab-bar ((t (:height 1.0))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :inverse-video t)))))

(ac-debug-log "UI configured")

;;; ===================================================================
;;; PACKAGE MANAGEMENT (Straight.el with optimizations)
;;; ===================================================================

(ac-debug-log "Bootstrapping straight.el...")
(setq package-enable-at-startup nil)

;; Straight.el optimizations
(setq straight-use-package-by-default t
      straight-vc-git-default-clone-depth 1           ; Shallow clones
      straight-check-for-modifications '(check-on-save) ; Only check on save
      straight-cache-autoloads t                       ; Cache autoloads
      straight-disable-compile nil)                    ; Allow compilation

;; Bootstrap straight.el (with timeout and error handling)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (ac-debug-log "Downloading straight.el bootstrap...")
    (condition-case err
        (with-timeout (15 (error "Bootstrap download timed out"))
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
      (error 
       (ac-debug-log (format "Failed to bootstrap straight.el: %s" err))
       (message "Failed to bootstrap straight.el: %s" err))))
  (when (file-exists-p bootstrap-file)
    (ac-debug-log "Loading straight.el bootstrap")
    (load bootstrap-file nil 'nomessage)))

(ac-debug-log "Straight.el ready")

;;; ===================================================================
;;; ESSENTIAL PACKAGES (Loaded Early)
;;; ===================================================================

;; use-package for cleaner config
(straight-use-package 'use-package)

;; Completion framework (vertico is lightweight)
(ac-debug-log "Loading vertico...")
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode))

;; Enhanced completion
(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)
        enable-recursive-minibuffers t
        read-extended-command-predicate #'command-completion-default-include-p)
  
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(ac-debug-log "Core completion packages loaded")

;;; ===================================================================
;;; DEFERRED PACKAGES (Load on demand or after idle)
;;; ===================================================================

;; Defer heavy/optional packages to after-init or idle time
(defun ac--load-deferred-packages ()
  "Load non-essential packages after startup completes."
  (ac-debug-log "Loading deferred packages...")
  
  ;; Visual enhancements (low priority)
  (use-package yascroll 
    :defer t
    :config (global-yascroll-bar-mode 1))
  
  (use-package origami 
    :defer t
    :hook (prog-mode . origami-mode))
  
  ;; Utilities
  (use-package s :defer t)
  (use-package xclip 
    :defer t
    :config 
    (xclip-mode 1)
    
    ;; Custom clipboard integration
    (defun my/xclip-set-selection (orig-fun type data)
      (let ((clipboard-command (format "fish -c 'clipboard %s'" (shell-quote-argument data))))
        (start-process-shell-command "clipboard" nil clipboard-command))
      (funcall orig-fun type data))
    
    (advice-add 'xclip-set-selection :around #'my/xclip-set-selection))
  
  ;; Mode-specific packages
  (use-package dockerfile-mode :defer t)
  (use-package fish-mode :defer t)
  (use-package prettier-js
    :defer t
    :hook (js-mode . prettier-js-mode)
    :bind (:map js-mode-map ("C-c p" . prettier-js)))
  
  ;; Terminal emulator (load on demand)
  (use-package eat
    :defer t
    :init
    (setq eat-shell "/usr/bin/fish"
          eat-term-name "xterm-256color")
    :config
    (add-hook 'eat-mode-hook
              (lambda ()
                (setq-local comint-scroll-to-bottom-on-output t
                            comint-show-maximum-output t
                            comint-move-point-for-output t
                            window-point-insertion-type t)
                (add-hook 'comint-output-filter-functions
                          #'comint-postoutput-scroll-to-bottom
                          nil t))))
  
  ;; Utilities
  (use-package restart-emacs
    :defer t
    :bind (("C-c C-r" . restart-emacs))
    :config
    (setq restart-emacs-restore-frames t))
  
  (global-set-key (kbd "C-c C-o") 'browse-url-at-point)
  
  ;; Evil mode (load after idle to avoid blocking)
  (use-package evil
    :defer 1  ; Load after 1 second idle
    :init
    (setq evil-want-keybinding nil  ; For evil-collection compatibility
          evil-shift-width 2
          evil-undo-system 'undo-redo)
    :config
    (evil-mode 1)
    
    ;; Preserve C-p for project-find-file in all states
    (dolist (state '(normal insert visual motion emacs))
      (evil-define-key state 'global (kbd "C-p") 'project-find-file))
    
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    
    ;; Custom quit behavior
    (defun my/evil-quit ()
      (interactive)
      (let ((current-buffer (current-buffer))
            (is-last-tab (eq 1 (length (tab-bar-tabs))))
            (is-last-window (eq 1 (length (window-list)))))
        
        (unless is-last-window
          (delete-window))
        
        (unless (delq (selected-window) (get-buffer-window-list current-buffer nil t))
          (kill-buffer current-buffer))
        
        (when (and is-last-window (not is-last-tab))
          (tab-bar-close-tab))))
    
    (with-eval-after-load 'evil
      (evil-ex-define-cmd "q[uit]" 'my/evil-quit)))
  
  ;; Evil terminal cursor (terminal only)
  (unless (display-graphic-p)
    (use-package evil-terminal-cursor-changer
      :after evil
      :config
      (evil-terminal-cursor-changer-activate)))
  
  (ac-debug-log "Deferred packages loaded"))

;; Schedule deferred loading after init
(add-hook 'emacs-startup-hook #'ac--load-deferred-packages)

;; Kill eat processes on exit
(defun kill-eat-processes ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'eat-mode)
        (eat-kill-process)))))

(add-hook 'kill-emacs-hook #'kill-eat-processes)

;;; ===================================================================
;;; AESTHETIC COMPUTER FUNCTIONS
;;; ===================================================================

(defun aesthetic ()
  "Start aesthetic computer dev server."
  (interactive)
  (eshell)
  (insert "python3 -m http.server 8888")
  (eshell-send-input))

;; Main backend function (optimized for speed)
(defun aesthetic-backend (target-tab)
  "Initialize aesthetic computer backend tabs.
TARGET-TAB: which tab to switch to after setup."
  (interactive)
  (ac-debug-log (format "Starting aesthetic-backend with target-tab: %s" target-tab))
  
  ;; Ensure eat is loaded before we use it
  (require 'eat nil t)
  
  ;; Set environment variable for fish
  (setenv "INSIDE_EMACS" "t")
  
  ;; Watchdog timer
  (run-with-timer 30 nil
                  (lambda ()
                    (ac-debug-log "WARNING: aesthetic-backend running 30+ seconds")
                    (message "⚠️ Backend init slow. Check /tmp/emacs-debug.log")))
  
  (let ((directory-path "~/aesthetic-computer")
        (emoji-for-command
         '(("code" . "📂") ("status" . "📡") ("url" . "⚡") 
           ("tunnel" . "🚇") ("agent" . "⚡") ("stripe-print" . "💳")
           ("stripe-ticket" . "🎫") ("chat-system" . "🤖") ("chat-sotce" . "🧠")
           ("chat-clock" . "⏰") ("site" . "🌐") ("session" . "📋")
           ("redis" . "🔴") ("bookmarks" . "🔖") ("kidlisp" . "🧪")
           ("oven" . "🔥") ("artery" . "🩸"))))
    
    ;; Clean up buffers (with error handling)
    (condition-case nil
        (dolist (bufname '("*scratch*" "*Messages*" "*straight-process*" "*async-native-compile-log*"))
          (when-let ((buf (get-buffer bufname)))
            (dolist (win (get-buffer-window-list buf nil t))
              (with-selected-window win
                (switch-to-buffer (other-buffer buf t))))
            (kill-buffer buf)))
      (error nil))
    
    ;; Initialize first tab - Artery TUI
    (tab-rename "artery")
    (let ((default-directory directory-path))
      (eat "fish -c 'ac-artery'")
      (when (get-buffer "*eat*")
        (with-current-buffer "*eat*"
          (rename-buffer "🩸-artery" t))))
    
    ;; Create code tab with agent
    (tab-new)
    (tab-rename "code")
    (let ((default-directory directory-path))
      (eat "fish -c 'ac-agent'")
      (when (get-buffer "*eat*")
        (with-current-buffer "*eat*"
          (rename-buffer "⚡-agent" t))))
    
    ;; Helper to create split tabs
    (defun create-split-tab (tab-name commands)
      (ac-debug-log (format "Creating tab: %s with commands: %s" tab-name commands))
      (condition-case err
          (progn
            (tab-new)
            (tab-rename tab-name)
            (let ((default-directory directory-path))
              (delete-other-windows)
              
              (let* ((window-height (window-height))
                     (window-width (window-width))
                     (min-height-per-window 8)
                     (min-width-per-window 40)
                     (max-vertical-splits (/ window-height min-height-per-window))
                     (use-horizontal (< max-vertical-splits (length commands)))
                     (first t))
                
                (dolist (cmd commands)
                  (unless first
                    (condition-case split-err
                        (if use-horizontal
                            (progn (split-window-right) (other-window 1))
                          (progn (split-window-below) (other-window 1)))
                      (error
                       (condition-case nil
                           (if use-horizontal
                               (progn (split-window-below) (other-window 1))
                             (progn (split-window-right) (other-window 1)))
                         (error
                          (message "Warning: Cannot split for %s" cmd))))))
                  (setq first nil)
                  
                  (let ((actual-cmd (if (string= cmd "bookmarks") "servers" cmd)))
                    (ac-debug-log (format "Running: ac-%s" actual-cmd))
                    (eat (format "fish -c 'ac-%s'" actual-cmd)))
                  
                  (when (get-buffer "*eat*")
                    (with-current-buffer "*eat*"
                      (rename-buffer
                       (format "%s-%s"
                               (or (cdr (assoc cmd emoji-for-command)) "🔧")
                               cmd) t)))
                  (goto-char (point-max)))
                
                (balance-windows)
                (other-window 1))))
        (error (message "Error creating tab %s: %s" tab-name err))))
    
    ;; Create all tabs
    (create-split-tab "status"   '("url" "tunnel"))
    (create-split-tab "stripe"   '("stripe-print" "stripe-ticket"))
    (create-split-tab "chat"     '("chat-system" "chat-sotce" "chat-clock"))
    (create-split-tab "web 1/2"  '("site" "session"))
    (create-split-tab "web 2/2"  '("redis" "bookmarks" "oven"))
    (create-split-tab "tests"    '("kidlisp"))
    
    ;; Switch to target tab
    (condition-case nil
        (if (member target-tab '("artery" "code" "status" "stripe" "chat" "web 1/2" "web 2/2" "tests"))
            (tab-bar-switch-to-tab target-tab)
          (message "No such tab: %s" target-tab))
      (error nil))))

;;; ===================================================================
;;; STARTUP COMPLETE
;;; ===================================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            (ac-debug-log "=== Optimized Emacs Configuration Load Complete ===")
            (message "Emacs ready (startup: %.2fs)"
                     (float-time (time-subtract (current-time) before-init-time)))
            ;; Auto-run aesthetic-backend after ensuring eat is loaded
            (run-with-idle-timer 2 nil
                                 (lambda ()
                                   (ac-debug-log "Auto-running aesthetic-backend")
                                   ;; Force load eat before running aesthetic-backend
                                   (require 'eat)
                                   (aesthetic-backend "code")))))

;; End of configuration
