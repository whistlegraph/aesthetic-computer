(setq inhibit-startup-screen t) ;; Disable startup message.
(setq eshell-banner-message "") ;; No eshell banner.

;; (load-theme 'wombat t) ;; Set a dark theme.
(setq-default line-spacing 0)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'fringe-mode) (fringe-mode 0))

;; Set up mouse in terminal.
(defun track-mouse (e))
(setq mouse-sel-mode t)
(xterm-mouse-mode 1)

(setq ring-bell-function 'ignore) ;; Ignore scroll bell.

;; Check if evil is installed, if not, set up package.el for MELPA and install evil.
(require 'package)
(unless (package-installed-p 'tree-sitter-lands)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'evil)
  ;; Requires these node packages: prettier typescript-language-server javascript-typescript-langserver
  (package-install 'prettier-js)
  (package-install 'lsp-mode)
  (package-install 'typescript-mode)
  (package-install 'tree-sitter)
  (package-install 'tree-sitter-langs))

(require 'evil) ;; Enable evil.
(evil-mode 1)

;; Enable JavaScript support.
(require 'tree-sitter)
(require 'tree-sitter-langs)
(add-hook 'js-mode-hook 'prettier-js-mode) ;; Enable prettier-js.
(add-hook 'js-mode-hook #'tree-sitter-hl-mode)
(add-hook 'js-mode-hook #'lsp) ;; Enable lsp on js.
(add-hook 'js-mode-hook #'lsp-deferred) ;; Enable lsp on js.

(global-display-line-numbers-mode t) ;; Line numbers everywhere.
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; Or only when programming.

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode)) ;; Support mjs files.
