;; Aesthetic Computer Emacs Configuration, 2024.3.13.12.51

;; Performance settings
(setq native-comp-async-report-warnings-errors nil
      native-comp-deferred-compilation nil
      x-gtk-use-system-tooltips nil
      mac-option-modifier 'meta
      warning-minimum-level :emergency)

(global-set-key [C-down-mouse-1] 'ignore)

;; Tab bar
(tab-bar-mode t)
(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-hints t
      tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
      tab-bar-close-button-show nil
      tab-line-tab-max-width 20)

;; Display settings
(setq scroll-step 1)
(global-auto-revert-mode 1)
(setq auto-revert-interval 5)
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook (lambda () (display-fill-column-indicator-mode 1)))

;; Line numbers
(defun disable-line-numbers-in-modes ()
  (when (or (derived-mode-p 'eshell-mode)
            (derived-mode-p 'eat-mode))
    (display-line-numbers-mode -1)))

(add-hook 'eshell-mode-hook 'disable-line-numbers-in-modes)
(add-hook 'eat-mode-hook 'disable-line-numbers-in-modes)
(add-hook 'image-mode-hook 'disable-line-numbers-in-modes)

;; Electric pairs
(defun enable-electric-pairs ()
  (setq-local electric-pair-pairs 
              '((?\{ . ?\}) (?\( . ?\)) (?\[ . ?\]) (?' . ?') (?` . ?`)))
  (electric-pair-local-mode 1))
(add-hook 'prog-mode-hook 'enable-electric-pairs)

;; Auto-save
(auto-save-visited-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun auto-save-buffer ()
  (when (or (derived-mode-p 'prog-mode)
            (derived-mode-p 'text-mode))
    (save-buffer)))

(when (window-system)
  (defvar auto-save-throttle-timer nil)
  (defun throttled-auto-save ()
    (when auto-save-throttle-timer
      (cancel-timer auto-save-throttle-timer))
    (setq auto-save-throttle-timer
          (run-with-timer 3 nil #'auto-save-buffer)))
  (add-hook 'focus-out-hook 'auto-save-buffer))

;; Faces
(custom-set-faces
 '(tab-bar ((t (:height 1.0))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :inverse-video t)))))

;; UI settings
(setq inhibit-startup-screen t
      eshell-banner-message ""
      initial-scratch-message nil)
(global-display-line-numbers-mode)

;; Windows WSL
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:/Windows/System32/bash.exe"
        shell-file-name explicit-shell-file-name)
  (setenv "SHELL" shell-file-name)
  (add-to-list 'exec-path "C:/Windows/System32"))

;; Mouse scrolling
(global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 1)))

;; Tab navigation
(defun my-tab-next ()
  (interactive)
  (eat-tab-change #'tab-next))

(defun my-tab-previous ()
  (interactive)
  (eat-tab-change #'tab-previous))

(global-set-key (kbd "C-x <right>") 'my-tab-next)
(global-set-key (kbd "C-x <left>") 'my-tab-previous)

;; GUI settings
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (fringe-mode 0))
(setq-default scroll-bar-mode 'right)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Mouse and bell
(xterm-mouse-mode 1)
(defun track-mouse (e))
(setq mouse-sel-mode t
      ring-bell-function 'ignore)

;; Backup directories
(defvar my-backup-directory "~/.emacs.d/backups/")
(unless (file-exists-p my-backup-directory)
  (make-directory my-backup-directory t))
(setq backup-directory-alist `(("." . ,my-backup-directory)))

(defvar my-auto-save-directory "~/.emacs.d/auto-saves/")
(unless (file-exists-p my-auto-save-directory)
  (make-directory my-auto-save-directory t))
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-directory t)))

(defvar my-lockfiles-directory "~/.emacs.d/lockfiles/")
(unless (file-exists-p my-lockfiles-directory)
  (make-directory my-lockfiles-directory t))
(setq lock-file-name-transforms `((".*" ,my-lockfiles-directory t)))

;; Editor behavior
(setq-default indent-tabs-mode nil
              tab-width 2
              truncate-lines t)
(setq vc-follow-symlinks t)

(global-set-key (kbd "M-z") 'toggle-truncate-lines)
(global-set-key (kbd "C-c z") 'toggle-truncate-lines)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(add-hook 'after-init-hook (lambda () (setq-default truncate-lines t)))

;; Package management
(setq package-enable-at-startup nil)

;; Straight.el bootstrap
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

;; Completion
(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

;; Org-mode
(use-package org)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

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
        enable-recursive-minibuffers t)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;; ───────────────────────────────────────────────────────────────────
;;; DISABLED/OPTIONAL PACKAGES
;;; ───────────────────────────────────────────────────────────────────

;; Auto dark theme (commented out)
;; (when (window-system)
;;   (use-package auto-dark)
;;   (setq auto-dark-dark-theme 'wombat
;;         auto-dark-light-theme 'whiteboard)
;;   (auto-dark-mode t))

;; Frame-specific configuration (commented out)
;; (when (display-graphic-p)
;;   (add-hook 'after-make-frame-functions
;;             (lambda (frame)
;; Active packages
(use-package yascroll 
  :config 
  (global-yascroll-bar-mode 1))

(use-package origami 
  :hook (after-init . global-origami-mode))

(global-set-key (kbd "C-p") #'project-find-file)
(global-set-key (kbd "C-x C-p") #'project-find-file)
(global-set-key (kbd "M-p") #'project-find-file)

(setq js-indent-level 2)

(use-package s)
(use-package dockerfile-mode)
(use-package fish-mode)

(use-package prettier-js
  :hook (js-mode . prettier-js-mode)
  :bind ("C-c p" . prettier-js))

(use-package xclip 
  :config 
  (xclip-mode 1))

(defun my/xclip-set-selection (orig-fun type data)
  (let ((clipboard-command (format "fish -c 'clipboard %s'" (shell-quote-argument data))))
    (start-process-shell-command "clipboard" nil clipboard-command))
  (funcall orig-fun type data))

(advice-add 'xclip-set-selection :around #'my/xclip-set-selection)

;; Evil mode
(use-package evil
  :config
  (evil-mode 1)
  (setq-default evil-shift-width 2)
  
  (dolist (state '(normal insert visual motion emacs))
    (evil-define-key state 'global (kbd "C-p") 'project-find-file)))

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

(defun my/evil-quit ()
  (interactive)
  (let ((current-buffer (current-buffer))
        (is-last-tab (eq 1 (length (tab-bar-tabs))))
        (is-last-window (eq 1 (length (window-list)))))

    ;; Close window if not the last window in tab
    (unless is-last-window
      (delete-window))

    ;; Kill buffer if not displayed elsewhere
    (unless (delq (selected-window) (get-buffer-window-list current-buffer nil t))
      (kill-buffer current-buffer))

    ;; Optional: Close tab if last window but not last tab
    ;; (when (and is-last-window (not is-last-tab))
    (if is-last-window
        (if is-last-tab
            (message "Cannot close last window of last tab")
          (tab-bar-close-tab))
        (delete-window))))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "q[uit]" 'my/evil-quit))

(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

;; Utilities
(use-package restart-emacs)
(setq restart-emacs-restore-frames t)

(global-set-key (kbd "C-c C-r") 'restart-emacs)
(global-set-key (kbd "C-c C-o") 'browse-url-at-point)

;; Eat terminal
(use-package eat)
(setq-default eat-shell "/usr/bin/fish"
              eat-term-name "xterm-256color")

;; Eat tab integration
(defun eat-tab-change (original-fun &rest args)
  (interactive)
  (apply original-fun args)
  
  (run-with-idle-timer 0.5 nil
    (lambda ()
      (when (and (selected-frame) (frame-live-p (selected-frame)))
        (condition-case err
            (let ((current-tab-windows (window-list (selected-frame) 'never)))
              (dolist (window current-tab-windows)
                (when (and (windowp window) (window-live-p window))
                  (with-current-buffer (window-buffer window)
                    (when (eq major-mode 'eat-mode)
                      (with-selected-window window
                        (end-of-buffer)
                        (evil-insert-state)))))))
          (error (message "Error in eat-tab-change timer: %s" err)))))))

(with-eval-after-load 'eat
  (advice-add 'tab-bar-select-tab :around #'eat-tab-change))

(defun kill-eat-processes ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'eat-mode)
        (eat-kill-process)))))

(add-hook 'kill-emacs-hook #'kill-eat-processes)

;; Aesthetic functions
(defun aesthetic ()
  (interactive)
  (eshell)
  (insert "python3 -m http.server 8888")
  (eshell-send-input))

(setq confirm-kill-processes nil)

(add-hook 'eat-mode-hook
          (lambda ()
            (setq-local comint-scroll-to-bottom-on-output t
                        comint-show-maximum-output t
                        comint-move-point-for-output t
                        window-point-insertion-type t)
            (add-hook 'comint-output-filter-functions
                      #'comint-postoutput-scroll-to-bottom
                      nil t)))

;; Main backend function
(defun aesthetic-backend (target-tab)
  (interactive)
  
  (let ((directory-path "~/aesthetic-computer")
        (commands '()) ; Individual tab commands (currently empty)
        (emoji-for-command
         '(("code" . "📂") ("source" . "📂") ("status" . "📡") 
           ("url" . "🌐") ("tunnel" . "🚇") ("site" . "📰") 
           ("session" . "🔒") ("redis" . "🔄") ("stripe" . "💳") 
           ("chat" . "💬") ("stripe-print" . "💳 🖨️") 
           ("stripe-ticket" . "💳🎫") ("web 1/2" . "🌐") 
           ("web 2/2" . "🌐") ("servers" . "🤖") ("bookmarks" . "🔖") 
           ("chat-system" . "🧭") ("chat-sotce" . "🪷") 
           ("chat-clock" . "🕑") ("tests" . "🧪") ("web" . "🌐"))))

    ;; Clean up unwanted buffers before starting
    (dolist (bufname '("*scratch*" "*Messages*" "*straight-process*" "*async-native-comp*"))
      (when-let ((buf (get-buffer bufname)))
        (dolist (win (get-buffer-window-list buf nil t))
          (with-selected-window win
            (switch-to-buffer (other-buffer buf t))))
        (kill-buffer buf)))

    ;; Initialize the first tab as "code" with ac-agent terminal
    (tab-rename "code")
    (let ((default-directory directory-path))
      (eat "fish -c 'ac-agent'"))

    ;; Define helper function for creating split tabs
    (cl-labels
        ((run-split-tab (tab-name &rest cmds)
           "Create a new tab with multiple commands in split windows."
           (tab-new)
           (tab-rename tab-name)
           (let ((default-directory directory-path))
             (delete-other-windows)
             (let ((first t))
               (dolist (cmd cmds)
                 ;; Create splits for all commands except the first
                 (unless first 
                   (if (member tab-name '("status" "stripe" "chat" "web 1/2" "web 2/2"))
                       (split-window-below)  ; Horizontal splits for these tabs
                     (split-window-right))   ; Vertical splits for others
                   (other-window 1))
                 (setq first nil)
                 
                 ;; Handle command name mapping (bookmarks -> servers)
                 (let ((actual-cmd (if (string= cmd "bookmarks") "servers" cmd)))
                   (eat (format "fish -c 'ac-%s'" actual-cmd)))
                 
                 ;; Rename buffer with emoji and command name
                 (with-current-buffer "*eat*"
                   (rename-buffer
                    (format "%s-%s"
                            (cdr (assoc cmd emoji-for-command))
                            cmd) t))
                 (goto-char (point-max)))
               
               ;; Balance window sizes and move to first window
               (balance-windows)
               (other-window 1)))))

      ;; Create all the split tabs
      (run-split-tab "status"   "url" "tunnel")
      (run-split-tab "stripe"   "stripe-print" "stripe-ticket")
      (run-split-tab "chat"     "chat-system" "chat-sotce" "chat-clock")
      (run-split-tab "web 1/2"  "site" "session")
      (run-split-tab "web 2/2"  "redis" "bookmarks")
      (run-split-tab "tests"    "kidlisp")

      ;; Create individual tabs (currently none, but structure is ready)
      (dolist (cmd commands)
        (tab-new)
        (tab-rename cmd)
        (let ((default-directory directory-path))
          (let ((actual-cmd (if (string= cmd "tests") "kidlisp" cmd)))
            (eat (format "fish -c 'ac-%s'" actual-cmd)))
          (with-current-buffer "*eat*"
            (rename-buffer
             (format "%s-%s"
                     (cdr (assoc cmd emoji-for-command))
                     cmd) t))
          (goto-char (point-max))
          (dolist (win (get-buffer-window-list (current-buffer) nil t))
            (set-window-point win (point-max))))))

    ;; Switch to the requested tab if it exists
    (let ((tab-emoji (cdr (assoc target-tab emoji-for-command))))
      (if tab-emoji
          (tab-bar-switch-to-tab target-tab)
        (message "No such tab: %s" target-tab)))))

;;; ===================================================================
;;; END OF AESTHETIC COMPUTER EMACS CONFIGURATION
;;; ===================================================================
