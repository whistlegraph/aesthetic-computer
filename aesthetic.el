;; aesthetic-emacs, 24.03.06.23.07
;; Runs the aesthetic computer backend in emacs, for a nice dashboard.

;; 📚 Notes
;; for installing on fedora 39, do: `sudo dnf copr enable stevenlin/emacs-pgtk-nativecomp`
;; also a tiling window manager may be preferred `sudo dnf install gnome-shell-extension-forge`

;; ✍️ Basic Emacs Configuration
(setq explicit-shell-file-name "/usr/share/fish")

;; (setq default-frame-alist '((undecorated . t))) ;; Hide title bar.

(setq initial-frame-alist
      '((width . 90)   ; Width set in characters
        (height . 45))) ; Height set in lines

(setq inhibit-startup-screen t) ;; Disable startup message.
(setq eshell-banner-message "") ;; No eshell banner.

(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono-10")
(setq-default line-spacing 0)

(menu-bar-mode -1) ;; Disable the menu bar.
(tool-bar-mode -1) ;; Disable the tool bar.
(load-theme 'wombat t) ;; Set a dark theme.

(setq-default truncate-lines t) ;; Disable line wrapping by default.

(fringe-mode 0) ;; Disable fringe indicators.
(scroll-bar-mode -1) ;; Disable scroll bar.

(xterm-mouse-mode 1)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; 🤖 Aesthetic Servers
(defun start-eshell-with-command (buffer-name command)
  "Start eshell in a buffer named BUFFER-NAME and run the specified COMMAND."
  (let ((eshell-buffer (generate-new-buffer-name buffer-name)))
    (eshell t)  ;; Start eshell in a new buffer
    (rename-buffer eshell-buffer)  ;; Rename the eshell buffer
    (insert command)  ;; Insert the command
    (eshell-send-input)))  ;; Send the command to eshell

(defun start-multiplex-environment ()
  "Start a multiplexed environment with predefined commands in eshell."
  ;; Start the first eshell and run the first command
  (start-eshell-with-command "server" "npm run aesthetic-emacs-top")

  ;; Create a new window and start the second eshell
  (split-window-below)  ;; Split the window
  (other-window 1)  ;; Move focus to the new window
  (start-eshell-with-command "session" "npm run aesthetic-emacs-bottom"))

;; Execute the function on startup
(add-hook 'emacs-startup-hook 'start-multiplex-environment)
