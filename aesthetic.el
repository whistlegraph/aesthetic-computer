(defun start-multiplex-environment ()
  "Start a multiplexed environment with predefined commands."

  ;; Disable startup message
  (setq inhibit-startup-screen t)

  ;; Start the first terminal and run the first command
  (ansi-term "/bin/bash")
  (term-send-raw-string "npm run aesthetic -- --exclude server:session\n")

  ;; Create a new window and start the second terminal
  (split-window-below)
  (other-window 1)
  (ansi-term "/bin/bash")
  (term-send-raw-string "npm run server:session\n"))

;; Execute the function on startup
(add-hook 'emacs-startup-hook 'start-multiplex-environment)