;;; artery.el --- Aesthetic Computer Artery Dashboard for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Whistlegraph
;; Author: Aesthetic Computer
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, multimedia, daw

;;; Commentary:
;; Emacs-native dashboard for Aesthetic Computer's artery system.
;; Replaces artery-tui with native Emacs buffers.
;;
;; Buffers:
;;   *artery*        - Main dashboard with status
;;   *artery-daw*    - DAW CDP log stream
;;   *artery-server* - AC server output
;;
;; Usage:
;;   M-x artery-start    - Start artery dashboard
;;   M-x artery-stop     - Stop artery processes
;;   C-c a s             - Show status
;;   C-c a d             - Toggle DAW log buffer
;;   C-c a e             - Eval in DAW

;;; Code:

(require 'ansi-color)

;;; --- Customization ---

(defgroup artery nil
  "Aesthetic Computer Artery integration."
  :group 'tools
  :prefix "artery-")

(defcustom artery-daw-host "host.docker.internal"
  "Host for DAW CDP connection."
  :type 'string
  :group 'artery)

(defcustom artery-daw-port 9229
  "Port for DAW CDP connection."
  :type 'integer
  :group 'artery)

(defcustom artery-server-url "https://localhost:8888"
  "URL for AC development server."
  :type 'string
  :group 'artery)

(defcustom artery-refresh-interval 0.5
  "Seconds between dashboard refreshes."
  :type 'number
  :group 'artery)

;;; --- State ---

(defvar artery--state nil
  "Current artery state plist.
Keys: :bpm :playing :piece :server-status :daw-connected")

(defvar artery--daw-log-buffer "🩸-daw-log"
  "Buffer name for DAW logs.")

(defvar artery--dashboard-buffer "🩸-artery"
  "Buffer name for main dashboard.")

(defvar artery--server-buffer "🩸-server"
  "Buffer name for server output.")

(defvar artery--bridge-buffer "🩸-bridge"
  "Buffer name for bridge process output.")

(defvar artery--refresh-timer nil
  "Timer for dashboard refresh.")

(defvar artery--process nil
  "The artery-emacs.mjs bridge process.")

;;; --- Faces ---

(defface artery-header-face
  '((t :inherit font-lock-keyword-face :height 1.3 :weight bold))
  "Face for artery dashboard header."
  :group 'artery)

(defface artery-status-ok-face
  '((t :inherit success :weight bold))
  "Face for OK status."
  :group 'artery)

(defface artery-status-error-face
  '((t :inherit error :weight bold))
  "Face for error status."
  :group 'artery)

(defface artery-bpm-face
  '((t :inherit font-lock-constant-face :weight bold :height 1.2))
  "Face for BPM display."
  :group 'artery)

(defface artery-transport-playing-face
  '((t :inherit success :weight bold))
  "Face for playing transport."
  :group 'artery)

(defface artery-transport-stopped-face
  '((t :inherit shadow))
  "Face for stopped transport."
  :group 'artery)

(defface artery-log-timestamp-face
  '((t :inherit font-lock-comment-face))
  "Face for log timestamps."
  :group 'artery)

(defface artery-log-type-face
  '((t :inherit font-lock-type-face))
  "Face for log message types."
  :group 'artery)

;;; --- Buffer Setup ---

(defun artery--create-buffers ()
  "Create artery buffers if they don't exist."
  (unless (get-buffer artery--dashboard-buffer)
    (with-current-buffer (get-buffer-create artery--dashboard-buffer)
      (artery-dashboard-mode)))
  (unless (get-buffer artery--daw-log-buffer)
    (with-current-buffer (get-buffer-create artery--daw-log-buffer)
      (artery-log-mode)))
  (unless (get-buffer artery--server-buffer)
    (with-current-buffer (get-buffer-create artery--server-buffer)
      (artery-log-mode))))

(defun artery--kill-buffers ()
  "Kill artery buffers."
  (dolist (buf (list artery--dashboard-buffer 
                     artery--daw-log-buffer 
                     artery--server-buffer))
    (when (get-buffer buf)
      (kill-buffer buf))))

;;; --- Dashboard Rendering ---

(defun artery--render-dashboard ()
  "Render the main dashboard buffer."
  (let ((buf (get-buffer artery--dashboard-buffer)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (pos (point)))
          (erase-buffer)
          (artery--insert-header)
          (insert "\n")
          (artery--insert-daw-status)
          (insert "\n")
          (artery--insert-server-status)
          (insert "\n")
          (artery--insert-keybindings)
          (goto-char (min pos (point-max))))))))

(defun artery--insert-header ()
  "Insert dashboard header."
  (insert (propertize "🎨 Artery Dashboard" 'face 'artery-header-face))
  (insert "\n")
  (insert (propertize (format-time-string "  %Y-%m-%d %H:%M:%S UTC" (current-time) t)
                      'face 'font-lock-comment-face))
  (insert "\n"))

(defun artery--insert-daw-status ()
  "Insert DAW status section."
  (let ((bpm (plist-get artery--state :bpm))
        (playing (plist-get artery--state :playing))
        (connected (plist-get artery--state :daw-connected))
        (piece (plist-get artery--state :piece)))
    (insert (propertize "DAW" 'face 'bold) "\n")
    (insert "  ")
    (if connected
        (progn
          (insert (propertize "●" 'face 'artery-status-ok-face) " Connected")
          (insert "  ")
          (insert (propertize (format "%d BPM" (or bpm 120)) 'face 'artery-bpm-face))
          (insert "  ")
          (if playing
              (insert (propertize "▶ Playing" 'face 'artery-transport-playing-face))
            (insert (propertize "■ Stopped" 'face 'artery-transport-stopped-face))))
      (insert (propertize "○" 'face 'artery-status-error-face) " Disconnected"))
    (insert "\n")
    (when piece
      (insert (format "  Piece: %s\n" piece)))))

(defun artery--insert-server-status ()
  "Insert server status section."
  (let ((status (plist-get artery--state :server-status)))
    (insert (propertize "Server" 'face 'bold) "\n")
    (insert "  ")
    (pcase status
      ('running (insert (propertize "●" 'face 'artery-status-ok-face) 
                        (format " %s" artery-server-url)))
      ('error (insert (propertize "●" 'face 'artery-status-error-face) " Error"))
      (_ (insert (propertize "○" 'face 'shadow) " Unknown")))
    (insert "\n")))

(defun artery--insert-keybindings ()
  "Insert keybindings help section."
  (insert "\n")
  (insert (propertize "Keybindings" 'face 'bold) "\n")
  (insert "  [d] DAW logs   [s] Refresh   [p] Load piece\n")
  (insert "  [e] Eval DAW   [r] Restart   [q] Quit\n"))

;;; --- Log Buffer ---

(defun artery-append-daw-log (timestamp type message)
  "Append a log entry to the DAW log buffer.
TIMESTAMP is the time string, TYPE is the log type, MESSAGE is the content."
  (let ((buf (get-buffer artery--daw-log-buffer)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (at-end (= (point) (point-max))))
          (goto-char (point-max))
          (insert (propertize timestamp 'face 'artery-log-timestamp-face))
          (insert " ")
          (insert (propertize (format "[%s]" type) 'face 'artery-log-type-face))
          (insert " ")
          (insert message)
          (insert "\n")
          ;; Auto-scroll if at end
          (when at-end
            (dolist (win (get-buffer-window-list buf nil t))
              (with-selected-window win
                (goto-char (point-max))
                (recenter -1)))))))))

(defun artery-clear-daw-log ()
  "Clear the DAW log buffer."
  (interactive)
  (let ((buf (get-buffer artery--daw-log-buffer)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer))))))

;;; --- State Updates ---

(defun artery-update-state (key value)
  "Update artery state KEY to VALUE and refresh dashboard."
  (setq artery--state (plist-put artery--state key value))
  (artery--render-dashboard))

(defun artery-update-daw-state (bpm playing)
  "Update DAW state with BPM and PLAYING status."
  (setq artery--state 
        (plist-put (plist-put artery--state :bpm bpm) :playing playing))
  (artery--render-dashboard))

;;; --- Process Management ---

(defun artery--start-bridge ()
  "Start the artery-emacs.mjs bridge process."
  (when (and artery--process (process-live-p artery--process))
    (kill-process artery--process))
  (let ((default-directory (expand-file-name "artery" 
                            (or (getenv "AC_ROOT") 
                                "/workspaces/aesthetic-computer"))))
    (setq artery--process
          (make-process
           :name "artery-bridge"
           :buffer artery--bridge-buffer
           :command '("node" "artery-emacs.mjs")
           :filter #'artery--process-filter
           :sentinel #'artery--process-sentinel))))

(defun artery--stop-bridge ()
  "Stop the artery bridge process."
  (when (and artery--process (process-live-p artery--process))
    (kill-process artery--process))
  (setq artery--process nil))

(defun artery--process-filter (proc output)
  "Process filter for bridge OUTPUT from PROC."
  ;; Parse JSON lines from the bridge
  (dolist (line (split-string output "\n" t))
    (condition-case err
        (let* ((json (json-parse-string line :object-type 'plist))
               (type (plist-get json :type)))
          (pcase type
            ("daw-state"
             (artery-update-daw-state 
              (plist-get json :bpm)
              (plist-get json :playing)))
            ("daw-log"
             (artery-append-daw-log
              (plist-get json :timestamp)
              (plist-get json :logType)
              (plist-get json :message)))
            ("daw-connected"
             (artery-update-state :daw-connected t))
            ("daw-disconnected"
             (artery-update-state :daw-connected nil))
            ("server-status"
             (artery-update-state :server-status 
                                  (intern (plist-get json :status))))))
      (error (message "artery: Failed to parse: %s" line)))))

(defun artery--process-sentinel (proc event)
  "Process sentinel for PROC with EVENT."
  (message "artery-bridge: %s" (string-trim event))
  (artery-update-state :daw-connected nil))

;;; --- Interactive Commands ---

;;;###autoload
(defun artery-start ()
  "Start the artery dashboard."
  (interactive)
  (artery--create-buffers)
  (setq artery--state '(:bpm 120 :playing nil :daw-connected nil :server-status unknown))
  (artery--render-dashboard)
  (artery--start-bridge)
  ;; Set up refresh timer
  (when artery--refresh-timer
    (cancel-timer artery--refresh-timer))
  (setq artery--refresh-timer
        (run-with-timer artery-refresh-interval artery-refresh-interval
                        #'artery--render-dashboard))
  (switch-to-buffer artery--dashboard-buffer)
  (message "Artery started"))

;;;###autoload
(defun artery-stop ()
  "Stop artery and clean up."
  (interactive)
  (artery--stop-bridge)
  (when artery--refresh-timer
    (cancel-timer artery--refresh-timer)
    (setq artery--refresh-timer nil))
  (artery--kill-buffers)
  (setq artery--state nil)
  (message "Artery stopped"))

(defun artery-show-daw-log ()
  "Show the DAW log buffer."
  (interactive)
  (let ((buf (get-buffer artery--daw-log-buffer)))
    (if buf
        (pop-to-buffer buf)
      (message "DAW log buffer not available"))))

(defun artery-refresh ()
  "Refresh the dashboard."
  (interactive)
  (artery--render-dashboard)
  (message "Dashboard refreshed"))

(defun artery-eval-in-daw (code)
  "Evaluate CODE in the DAW jweb~ context."
  (interactive "sEval in DAW: ")
  (if artery--process
      (process-send-string artery--process 
                           (concat (json-encode `(:type "eval" :code ,code)) "\n"))
    (message "Artery bridge not running")))

(defun artery-eval-region-in-daw (start end)
  "Evaluate region from START to END in DAW."
  (interactive "r")
  (artery-eval-in-daw (buffer-substring-no-properties start end)))

;;; --- Major Modes ---

(defvar artery-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'artery-show-daw-log)
    (define-key map "s" #'artery-refresh)
    (define-key map "e" #'artery-eval-in-daw)
    (define-key map "r" #'artery-start)
    (define-key map "q" #'artery-stop)
    (define-key map "g" #'artery-refresh)
    map)
  "Keymap for `artery-dashboard-mode'.")

(define-derived-mode artery-dashboard-mode special-mode "Artery"
  "Major mode for the artery dashboard buffer."
  (setq-local revert-buffer-function (lambda (_ignore-auto _noconfirm) 
                                        (artery--render-dashboard)))
  (setq buffer-read-only t))

(defvar artery-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'artery-clear-daw-log)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `artery-log-mode'.")

(define-derived-mode artery-log-mode special-mode "Artery-Log"
  "Major mode for artery log buffers."
  (setq buffer-read-only t)
  (setq-local truncate-lines t))

;;; --- Global Keybindings ---

;;;###autoload
(defvar artery-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'artery-start)
    (define-key map "q" #'artery-stop)
    (define-key map "d" #'artery-show-daw-log)
    (define-key map "e" #'artery-eval-in-daw)
    (define-key map "r" #'artery-refresh)
    map)
  "Keymap for artery commands, usually bound to `C-c a'.")

;;;###autoload
(global-set-key (kbd "C-c a") artery-command-map)

(provide 'artery)
;;; artery.el ends here
