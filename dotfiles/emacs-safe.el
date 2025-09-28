;; Safe Aesthetic Computer Emacs Configuration for debugging

;; Performance settings
(setq native-comp-async-report-warnings-errors nil
      native-comp-deferred-compilation nil
      x-gtk-use-system-tooltips nil
      mac-option-modifier 'meta
      warning-minimum-level :emergency)

;; Basic UI settings
(setq inhibit-startup-screen t
      eshell-banner-message ""
      initial-scratch-message nil)
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Tab bar
(tab-bar-mode t)
(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-hints t
      tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
      tab-bar-close-button-show nil)

;; Basic editor behavior
(setq-default indent-tabs-mode nil
              tab-width 2
              truncate-lines t)

(global-set-key (kbd "M-z") 'toggle-truncate-lines)

;; Package management - minimal setup
(setq package-enable-at-startup nil)

;; Minimal straight.el bootstrap (with shorter timeout)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (condition-case err
        (with-timeout (10 (error "Bootstrap download timed out"))
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
      (error (message "Failed to bootstrap straight.el: %s" err))))
  (when (file-exists-p bootstrap-file)
    (load bootstrap-file nil 'nomessage)))

(setq straight-use-package-by-default t
      straight-vc-git-default-clone-depth 1)

;; Essential packages only
(use-package fish-mode)

;; Safe aesthetic function that doesn't auto-start everything
(defun aesthetic-backend-safe (target-tab)
  "Safe version that only creates tabs without auto-starting processes"
  (interactive)
  (message "Use 'M-x aesthetic-backend-start' to manually start all services")
  (tab-rename "code"))

;; Function to manually start services when ready
(defun aesthetic-backend-start ()
  "Manually start all aesthetic computer services"
  (interactive)
  (when (y-or-n-p "Start all AC services? This will spawn many processes: ")
    (load-file "/workspaces/aesthetic-computer/dotfiles/dot_config/emacs.el")
    (aesthetic-backend "code")))

(message "Safe emacs config loaded. Use 'M-x aesthetic-backend-start' to start services.")
