;; -*- lexical-binding: t; -*-
;; Aesthetic Computer Emacs Configuration, 2024.3.13.12.51

;; Debug logging - persistent location that survives container rebuilds
(defvar ac-debug-log-file "/workspaces/aesthetic-computer/.emacs-logs/emacs-debug.log")
(defvar ac-perf-log-file "/workspaces/aesthetic-computer/.emacs-logs/emacs-perf.log")
(defvar ac-profile-log-file "/workspaces/aesthetic-computer/.emacs-logs/emacs-profile.log")
(defvar ac--startup-time (current-time))
(defvar ac--last-perf-time (current-time))

(defun ac-debug-log (message)
  "Log a debug message with timestamp."
  (with-temp-buffer
    (insert (format "[%s] %s\n" (format-time-string "%Y-%m-%d %H:%M:%S.%3N") message))
    (append-to-file (point-min) (point-max) ac-debug-log-file)))

(defun ac-perf-log (label)
  "Log performance timing since startup and since last checkpoint."
  (let* ((now (current-time))
         (total-ms (round (* 1000 (float-time (time-subtract now ac--startup-time)))))
         (delta-ms (round (* 1000 (float-time (time-subtract now ac--last-perf-time))))))
    (setq ac--last-perf-time now)
    (with-temp-buffer
      (insert (format "[%s] PERF: %s | +%dms (total: %dms)\n" 
                      (format-time-string "%Y-%m-%d %H:%M:%S.%3N")
                      label delta-ms total-ms))
      (append-to-file (point-min) (point-max) ac-perf-log-file))))

(defun ac-perf-session-start ()
  "Mark the start of a new Emacs session in perf log."
  (with-temp-buffer
    (insert (format "\n=== NEW SESSION: %s ===\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (append-to-file (point-min) (point-max) ac-perf-log-file)))

;;; ===================================================================
;;; PROFILING & DIAGNOSTICS
;;; ===================================================================

(defvar ac--profiling-active nil "Whether CPU profiling is currently running.")

(defun ac-profile-start ()
  "Start CPU profiler and log to file."
  (interactive)
  (unless ac--profiling-active
    (profiler-start 'cpu)
    (setq ac--profiling-active t)
    (ac-debug-log "PROFILER: Started CPU profiling")))

(defun ac-profile-stop ()
  "Stop profiler and write report to log file."
  (interactive)
  (when ac--profiling-active
    (let ((report (profiler-report-cpu)))
      (with-temp-buffer
        (insert (format "\n=== PROFILE REPORT: %s ===\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        (profiler-report)
        (append-to-file (point-min) (point-max) ac-profile-log-file)))
    (profiler-stop)
    (setq ac--profiling-active nil)
    (ac-debug-log "PROFILER: Stopped, report written")))

(defun ac-profile-report ()
  "Show profiler report interactively."
  (interactive)
  (profiler-report))

(defun ac-diagnose-eat ()
  "Diagnose all eat terminal buffers - log their state."
  (interactive)
  (let ((eat-buffers (seq-filter (lambda (b) (with-current-buffer b (eq major-mode 'eat-mode)))
                                  (buffer-list))))
    (ac-debug-log (format "DIAGNOSE: Found %d eat buffers" (length eat-buffers)))
    (dolist (buf eat-buffers)
      (with-current-buffer buf
        (let* ((proc (get-buffer-process buf))
               (proc-status (if proc (process-status proc) 'no-process))
               (buf-size (buffer-size)))
          (ac-debug-log (format "  BUFFER: %s | size=%d | proc=%s"
                                (buffer-name) buf-size proc-status)))))
    (message "Diagnosed %d eat buffers - see debug log" (length eat-buffers))))

(defun ac-diagnose-timers ()
  "Log all active timers."
  (interactive)
  (ac-debug-log (format "DIAGNOSE: %d active timers" (length timer-list)))
  (dolist (timer timer-list)
    (ac-debug-log (format "  TIMER: %s repeat=%s" 
                          (timer--function timer)
                          (timer--repeat-delay timer))))
  (message "Logged %d timers" (length timer-list)))

(defun ac-diagnose-processes ()
  "Log all active processes."
  (interactive)
  (let ((procs (process-list)))
    (ac-debug-log (format "DIAGNOSE: %d active processes" (length procs)))
    (dolist (proc procs)
      (ac-debug-log (format "  PROC: %s | status=%s | buffer=%s"
                            (process-name proc)
                            (process-status proc)
                            (if (process-buffer proc) (buffer-name (process-buffer proc)) "none"))))
    (message "Logged %d processes" (length procs))))

(defun ac-diagnose-all ()
  "Run all diagnostics."
  (interactive)
  (ac-debug-log "=== FULL DIAGNOSTIC START ===")
  (ac-diagnose-eat)
  (ac-diagnose-timers)
  (ac-diagnose-processes)
  (ac-debug-log "=== FULL DIAGNOSTIC END ===")
  (message "Full diagnostic complete - see .emacs-logs/emacs-debug.log"))

;; Keybindings for diagnostics
(global-set-key (kbd "C-c d e") 'ac-diagnose-eat)
(global-set-key (kbd "C-c d t") 'ac-diagnose-timers)
(global-set-key (kbd "C-c d p") 'ac-diagnose-processes)
(global-set-key (kbd "C-c d a") 'ac-diagnose-all)
(global-set-key (kbd "C-c d s") 'ac-profile-start)
(global-set-key (kbd "C-c d x") 'ac-profile-stop)
(global-set-key (kbd "C-c d r") 'ac-profile-report)

;;; --- MCP State Awareness Functions ---
;; These functions provide structured state info for MCP tools

(defun ac-mcp-get-state ()
  "Return comprehensive emacs state as JSON-friendly alist for MCP tools.
Call this to get awareness of current buffers, tabs, and processes."
  (let* ((current-buf (current-buffer))
         (current-buf-name (buffer-name current-buf))
         (current-tab (alist-get 'name (tab-bar--current-tab)))
         (all-tabs (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))
         (eat-buffers 
          (seq-filter (lambda (b) 
                        (with-current-buffer b (eq major-mode 'eat-mode)))
                      (buffer-list)))
         (eat-info
          (mapcar (lambda (buf)
                    (with-current-buffer buf
                      (let* ((proc (get-buffer-process buf))
                             (proc-status (if proc 
                                              (symbol-name (process-status proc)) 
                                            "no-process")))
                        (list (cons 'name (buffer-name buf))
                              (cons 'process proc-status)
                              (cons 'alive (and proc (process-live-p proc)))))))
                  eat-buffers)))
    (list (cons 'currentBuffer current-buf-name)
          (cons 'currentTab current-tab)
          (cons 'allTabs all-tabs)
          (cons 'eatBuffers eat-info))))

(defun ac-mcp-get-buffer-state (buffer-name)
  "Get detailed state of a specific buffer for MCP tools.
Returns nil if buffer doesn't exist, otherwise returns alist with:
- exists: always t
- mode: major mode name  
- process: process status or nil
- alive: t if process is live
- modified: t if buffer has unsaved changes
- size: buffer character count"
  (if-let ((buf (get-buffer buffer-name)))
      (with-current-buffer buf
        (let* ((proc (get-buffer-process buf))
               (proc-status (when proc (symbol-name (process-status proc)))))
          (list (cons 'exists t)
                (cons 'mode (symbol-name major-mode))
                (cons 'process proc-status)
                (cons 'alive (and proc (process-live-p proc)))
                (cons 'modified (buffer-modified-p))
                (cons 'size (buffer-size)))))
    nil))

(defun ac-mcp-get-eat-status (buffer-name)
  "Get status of an eat terminal buffer.
Returns: 'running, 'exited, 'not-eat, or 'not-found"
  (if-let ((buf (get-buffer buffer-name)))
      (with-current-buffer buf
        (if (eq major-mode 'eat-mode)
            (let ((proc (get-buffer-process buf)))
              (cond
               ((not proc) 'exited)
               ((process-live-p proc) 'running)
               (t 'exited)))
          'not-eat))
    'not-found))

(defun ac-mcp-format-state ()
  "Return emacs state as formatted string for MCP tools."
  (let* ((state (ac-mcp-get-state))
         (current-buf (alist-get 'currentBuffer state))
         (current-tab (alist-get 'currentTab state))
         (tabs (alist-get 'allTabs state))
         (eats (alist-get 'eatBuffers state)))
    (format "Tab: %s | Buffer: %s | Tabs: [%s] | Terminals: %s"
            current-tab
            current-buf
            (mapconcat #'identity tabs ", ")
            (mapconcat (lambda (e) 
                         (format "%s(%s)" 
                                 (alist-get 'name e)
                                 (if (alist-get 'alive e) "●" "○")))
                       eats ", "))))

(defun ac-mcp-ensure-buffer-ready (buffer-name &optional restart-fn)
  "Ensure buffer exists and is usable. Returns status symbol.
If buffer has dead process and RESTART-FN is provided, calls it.
Returns: 'ready, 'restarted, 'dead, or 'not-found"
  (if-let ((buf (get-buffer buffer-name)))
      (with-current-buffer buf
        (if (eq major-mode 'eat-mode)
            (let ((proc (get-buffer-process buf)))
              (cond
               ((and proc (process-live-p proc)) 'ready)
               (restart-fn 
                (funcall restart-fn)
                'restarted)
               (t 'dead)))
          'ready))
    'not-found))

;;; --- Copilot Completion Notification ---
;; Flash current terminal when Copilot response is done (called via MCP)

(defvar ac-notify-flash-active nil "Whether notification flash is currently active.")
(defvar ac-notify-flash-timer nil "Timer for notification flashing.")
(defvar ac-notify-flash-count 0 "Number of flash cycles completed.")
(defvar ac-notify-original-bg nil "Original background color before flash.")
(defvar ac-notify-flash-colors
  '("#1a3a1a"   ; dark green
    "#3a1a3a"   ; dark magenta
    "#1a1a3a"   ; dark blue
    "#3a3a1a"   ; dark yellow
    "#3a1a1a"   ; dark red
    "#1a3a3a")  ; dark cyan
  "Colors to cycle through during notification flash.")

(defun ac-notify-stop-flash ()
  "Stop the notification flash and restore original background."
  (interactive)
  (when ac-notify-flash-timer
    (cancel-timer ac-notify-flash-timer)
    (setq ac-notify-flash-timer nil))
  (setq ac-notify-flash-active nil)
  (setq ac-notify-flash-count 0)
  ;; Restore original background
  (when ac-notify-original-bg
    (set-face-background 'default ac-notify-original-bg)
    (setq ac-notify-original-bg nil))
  ;; Force redisplay
  (redisplay t))

(defun ac-notify-do-flash ()
  "Perform one flash cycle with rainbow colors."
  (when ac-notify-flash-active
    (setq ac-notify-flash-count (1+ ac-notify-flash-count))
    ;; Alternate between flash colors and original
    (if (= (mod ac-notify-flash-count 2) 1)
        ;; Pick a color from the list, cycling through
        (let* ((color-idx (mod (/ ac-notify-flash-count 2) (length ac-notify-flash-colors)))
               (flash-color (nth color-idx ac-notify-flash-colors)))
          (set-face-background 'default flash-color))
      (set-face-background 'default ac-notify-original-bg))
    (redisplay t)
    ;; Continue flashing
    (setq ac-notify-flash-timer
          (run-at-time 0.25 nil #'ac-notify-do-flash))))

;; 🎯 Task State Management (for VS Code status bar integration)
(defvar ac-task-state-file "/tmp/aesthetic-task-state.json"
  "Path to the task state file that VS Code watches.")

(defun ac-task-set (status &optional label progress)
  "Set the task state. STATUS can be: idle, working, done, error.
LABEL is optional description text.
PROGRESS is optional percentage (0-100).
VS Code status bar will update to reflect this state."
  (let* ((timestamp (truncate (* 1000 (float-time))))
         (state `((status . ,status)
                  (label . ,(or label status))
                  (progress . ,(or progress (if (equal status "done") 100 0)))
                  (timestamp . ,timestamp))))
    (with-temp-file ac-task-state-file
      (insert (json-encode state)))
    (message "🎯 Task: %s - %s" status (or label ""))))

(defun ac-task-working (&optional label)
  "Set task status to working (yellow, spinning icon)."
  (interactive "sTask label: ")
  (ac-task-set "working" (or label "Working...")))

(defun ac-task-done (&optional label)
  "Set task status to done (green, flash effect)."
  (interactive "sTask label: ")
  (ac-task-set "done" (or label "Done!")))

(defun ac-task-error (&optional label)
  "Set task status to error (red)."
  (interactive "sTask label: ")
  (ac-task-set "error" (or label "Error")))

(defun ac-task-idle ()
  "Clear task status (back to idle)."
  (interactive)
  (ac-task-set "idle" ""))

(defun ac-task-clear ()
  "Remove the task state file entirely."
  (interactive)
  (when (file-exists-p ac-task-state-file)
    (delete-file ac-task-state-file)
    (message "🎯 Task state cleared")))

(defun ac-notify-done (&optional message)
  "Notify completion by flashing the current buffer background.
Flash continues until user clicks or presses a key.
Called by MCP tools at the end of each response.
Also updates VS Code task status bar to 'done'."
  (interactive)
  (let ((msg (or message "Done")))
    ;; Also signal artery-tui (for when viewing that tab)
    (with-temp-file "/tmp/ac-copilot-done"
      (insert "done"))
    ;; Update VS Code task status bar
    (ac-task-done msg)
    ;; Start flashing in current buffer
    (unless ac-notify-flash-active
      (setq ac-notify-original-bg (face-background 'default nil t))
      (setq ac-notify-flash-active t)
      (setq ac-notify-flash-count 0)
      (ac-notify-do-flash)
      ;; Stop flash on any input (mouse or keyboard)
      (add-hook 'pre-command-hook #'ac-notify-stop-flash))
    (message "🔔 %s" msg)))

(ac-perf-session-start)
(ac-debug-log "=== Starting Emacs Configuration Load ===")
(ac-perf-log "Config load started")

;; Performance settings
(ac-debug-log "Setting performance options...")
(setq native-comp-async-report-warnings-errors nil
      native-comp-deferred-compilation nil
      native-comp-jit-compilation nil           ; Disable JIT native compilation
      native-comp-async-jobs-number 1           ; Limit async jobs if any do run
      native-comp-async-query-on-exit nil       ; Don't query on exit
      comp-async-report-warnings-errors nil     ; Suppress comp warnings
      comp-deferred-compilation nil             ; Also disable via comp- prefix
      x-gtk-use-system-tooltips nil
      mac-option-modifier 'meta
      warning-minimum-level :emergency
      ;; Terminal/process output throttling
      read-process-output-max (* 16 1024)       ; 16KB chunks instead of 64KB
      process-adaptive-read-buffering t         ; Let emacs batch reads
      redisplay-dont-pause nil                  ; Allow redisplay interruption
      fast-but-imprecise-scrolling t            ; Faster scroll in terminals
      bidi-inhibit-bpa t                        ; Disable bidi for terminals
      bidi-paragraph-direction 'left-to-right)  ; No bidi scanning

;; Kill any runaway native comp processes on startup
(when (fboundp 'native-comp-available-p)
  (advice-add 'native-compile-async :override
              (lambda (&rest _) nil)))          ; Completely disable async native comp

(ac-debug-log "Performance settings loaded")
(ac-perf-log "Performance settings loaded")

(global-set-key [C-down-mouse-1] 'ignore)

;; Tab bar
(tab-bar-mode t)
(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-hints t
      tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
      tab-bar-close-button-show nil
      tab-line-tab-max-width 20)

;; Custom colored tab bar - each tab gets its own background color
(defun ac-tab-bar-tab-name-format (tab i)
  "Format TAB name with custom colors based on tab name."
  (let* ((current-p (eq (car tab) 'current-tab))
         (tab-name (alist-get 'name tab))
         ;; Define colors for each tab type (bg . fg)
         (tab-colors '(("artery"  . ("#8B0000" . "#FFFFFF"))  ; Dark red
                       ("status"  . ("#006400" . "#FFFFFF"))  ; Dark green  
                       ("stripe"  . ("#4B0082" . "#FFFFFF"))  ; Indigo
                       ("chat"    . ("#00008B" . "#FFFFFF"))  ; Dark blue
                       ("web 1/2" . ("#008B8B" . "#FFFFFF"))  ; Dark cyan
                       ("web 2/2" . ("#2F4F4F" . "#FFFFFF"))  ; Dark slate gray
                       ("tests"   . ("#8B4513" . "#FFFFFF"))  ; Saddle brown
                       ("views"   . ("#556B2F" . "#FFFFFF"))  ; Dark olive green
                       ("llm"     . ("#5B2C83" . "#FFFFFF"))  ; Deep purple
                       ("crash"   . ("#5A1B1B" . "#FFFFFF"))  ; Dark maroon
                       ("fishy"   . ("#005F73" . "#FFFFFF")))) ; Deep teal
         (colors (or (cdr (assoc tab-name tab-colors))
                     '("#333333" . "#FFFFFF")))  ; Default dark gray
         (bg-color (car colors))
         (fg-color (cdr colors))
         ;; Make current tab brighter
         (face-spec (if current-p
                        `(:background ,bg-color :foreground ,fg-color :weight bold :box (:line-width 2 :color "#FFFFFF"))
                      `(:background ,bg-color :foreground ,fg-color :weight normal))))
    (propertize (format " %d:%s " i tab-name) 'face face-spec)))

(setq tab-bar-tab-name-format-function #'ac-tab-bar-tab-name-format)

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

(defun ac-optimize-eat-terminal ()
  "Disable heavy modes in eat terminal buffers for better performance."
  ;; Disable line numbers
  (display-line-numbers-mode -1)
  ;; Disable evil-mode (use emacs keybindings in terminals)
  (when (and (boundp 'evil-local-mode) (fboundp 'evil-local-mode))
    (evil-local-mode -1))
  ;; Disable origami (code folding not needed in terminal)
  (when (and (boundp 'origami-mode) (fboundp 'origami-mode))
    (origami-mode -1))
  ;; Disable yascroll (scrollbar causes rendering overhead)
  (when (and (boundp 'yascroll-bar-mode) (fboundp 'yascroll-bar-mode))
    (yascroll-bar-mode -1))
  ;; Disable fill column indicator
  (when (fboundp 'display-fill-column-indicator-mode)
    (display-fill-column-indicator-mode -1)))

;; Auto-scroll eat terminals to bottom on new output
(defun ac-eat-auto-scroll ()
  "Scroll eat buffer to bottom when new output arrives (if not actively viewing history)."
  (when (derived-mode-p 'eat-mode)
    ;; Only auto-scroll if we're near the bottom already (within ~5 lines)
    ;; This allows users to scroll up and read without being yanked back
    (let ((window (get-buffer-window (current-buffer))))
      (when window
        (with-selected-window window
          (let* ((point-max (point-max))
                 (window-end-pos (window-end nil t))
                 (near-bottom (>= window-end-pos (- point-max 200))))
            (when near-bottom
              (goto-char point-max)
              (recenter -1))))))))

;; Add auto-scroll to eat-update-hook
(with-eval-after-load 'eat
  (add-hook 'eat-update-hook #'ac-eat-auto-scroll))

(add-hook 'eshell-mode-hook 'disable-line-numbers-in-modes)
(add-hook 'eat-mode-hook 'disable-line-numbers-in-modes)
(add-hook 'eat-mode-hook 'ac-optimize-eat-terminal)
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
  (tab-next))

(defun my-tab-previous ()
  (interactive)
  (tab-previous))

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

;; Straight.el bootstrap with timeout
(ac-debug-log "Starting straight.el bootstrap")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (ac-debug-log "Bootstrap file not found, downloading...")
    (condition-case err
        (with-timeout (10 (error "Bootstrap download timed out"))  ; Reduced timeout
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
      (error (ac-debug-log (format "Failed to bootstrap straight.el: %s" err))
             (message "Failed to bootstrap straight.el: %s" err))))
  (when (file-exists-p bootstrap-file)
    (ac-debug-log "Loading bootstrap file")
    (load bootstrap-file nil 'nomessage)))

(setq straight-use-package-by-default t
      straight-vc-git-default-clone-depth 1   ; Shallow clones for faster setup
      straight-check-for-modifications '(check-on-save find-when-checking))

;; Auto-update packages in background (daemon only, not on client connect)
(defun ac-update-packages-async ()
  "Update all packages asynchronously in background subprocess."
  (ac-debug-log "Checking for package updates in background...")
  (let ((default-directory user-emacs-directory))
    ;; Run in a subprocess so it doesn't block anything
    (start-process "straight-update" "*straight-update*"
                   "emacs" "--batch" "-l" user-init-file
                   "--eval" "(progn (straight-pull-all) (straight-rebuild-all))")))

;; Manual update command
(defun ac-update-packages ()
  "Manually update all packages now."
  (interactive)
  (message "🔄 Updating packages...")
  (straight-pull-all)
  (straight-rebuild-all)
  (message "✅ All packages updated"))

;; Only check for updates during daemon startup, not on every client connection
;; This prevents UI blocking when connecting emacsclient
(when (daemonp)
  (run-with-timer 60 nil #'ac-update-packages-async))

(ac-debug-log "Straight.el configured, starting package loads")
(ac-perf-log "Straight.el configured")

;; Completion
(ac-debug-log "Loading vertico")
(use-package vertico
  :init
  (vertico-mode))
(ac-debug-log "Vertico loaded")
(ac-perf-log "Vertico loaded")

(use-package savehist
  :init
  (savehist-mode))

;; Org-mode (temporarily disabled due to cloning issues)
;; (condition-case nil
;;     (progn
;;       (use-package org
;;         :defer t)
;;       (global-set-key (kbd "C-c l") #'org-store-link)
;;       (global-set-key (kbd "C-c a") #'org-agenda)
;;       (global-set-key (kbd "C-c c") #'org-capture))
;;   (error (message "Failed to load org-mode, continuing without it...")))

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

;; NOTE: origami.el has a defface bug that calls (face-attribute 'highlight :background)
;; at load time, which returns 'unspecified' in daemon mode (no frame).
;; Defer loading until after a frame exists.
(use-package origami 
  :defer t
  :hook (server-after-make-frame . global-origami-mode))

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

;; Eat terminal - with performance optimizations for many terminals
(use-package eat)
(setq-default eat-shell "/usr/bin/fish"
              eat-term-name "xterm-256color")

;; Filter out unhandled OSC sequences (10/11/12 = fg/bg/cursor color)
;; These are sent by apps like Copilot CLI but eat doesn't handle them
(defun ac--strip-osc-color-sequences (string)
  "Remove OSC 10/11/12 sequences that eat cannot process."
  ;; Simple pattern: ]10/11/12;... until whitespace (Copilot CLI sends malformed OSC)
  (replace-regexp-in-string "]1[012];[^[:space:]]+" "" string))


(defun ac--filter-eat-output (orig-fn process string)
  "Advice to filter unhandled OSC sequences before eat processes them."
  (funcall orig-fn process (ac--strip-osc-color-sequences string)))

(advice-add 'eat--filter :around #'ac--filter-eat-output)

;; Performance: Aggressively limit eat overhead for 16+ terminals
(setq eat-enable-directory-tracking nil      ; Reduces overhead
      eat-enable-shell-command-history nil   ; We use fish history
      eat-enable-shell-prompt-annotation nil ; Reduces processing
      eat-show-title-on-mode-line nil        ; We name buffers ourselves
      eat-term-scrollback-size 8192          ; 8KB - minimal scrollback
      eat-enable-auto-line-mode nil          ; Keep in semi-char mode
      eat-minimum-latency 0.1                ; Batch output updates (100ms)
      eat-maximum-latency 0.2                ; Max batch delay (200ms)
      eat-enable-blinking-text nil           ; Disable blinking
      eat-enable-mouse nil)                  ; Disable mouse support

;; Disable evil mode in artery buffer
;; REMOVED: buffer-list-update-hook was causing performance issues
;; The hook fired constantly with 16+ terminals causing CPU spike
;; Just use eat-mode-hook with timer instead

;; Use eat-mode-hook with a timer to catch post-rename
(add-hook 'eat-mode-hook
          (lambda ()
            (run-with-timer 0.5 nil
              (lambda ()
                (when (and (buffer-live-p (current-buffer))
                           (string-match-p "artery" (buffer-name)))
                  (with-current-buffer (current-buffer)
                    (when (and (boundp 'evil-mode) evil-mode)
                      (evil-emacs-state))))))))

;; Eat tab integration (temporarily disabled for debugging)
;; (defun eat-tab-change (original-fun &rest args)
;;   (interactive)
;;   (let ((result (apply original-fun args)))
;;     (run-with-idle-timer 0.1 nil
;;       (lambda ()
;;         (when (and (selected-frame) (frame-live-p (selected-frame)))
;;           (condition-case err
;;               (let ((current-tab-windows (window-list (selected-frame) 'never)))
;;                 (dolist (window current-tab-windows)
;;                   (when (and (windowp window) (window-live-p window))
;;                     (with-current-buffer (window-buffer window)
;;                       (when (eq major-mode 'eat-mode)
;;                         (with-selected-window window
;;                           (end-of-buffer)
;;                           (when (bound-and-true-p evil-mode)
;;                             (evil-insert-state))))))))
;;             (error nil)))))
;;     result))

;; (with-eval-after-load 'eat
;;   (advice-add 'tab-bar-select-tab :around #'eat-tab-change))

(defun kill-eat-processes ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'eat-mode)
        (eat-kill-process)))))

(add-hook 'kill-emacs-hook #'kill-eat-processes)

(defun ac-restart-fishy ()
  "Restart fish process in the fishy buffer if it died."
  (interactive)
  (if-let ((buf (get-buffer "🐟-fishy")))
      (with-current-buffer buf
        (when (get-buffer-process buf)
          (delete-process (get-buffer-process buf)))
        (eat-exec buf "fishy" "/usr/bin/fish" nil nil)
        (message "✅ Fish restarted in 🐟-fishy"))
    (message "No 🐟-fishy buffer found")))

(defun ac-restart-site ()
  "Restart the site dev server (npm run site) in the 🌐-site buffer."
  (interactive)
  (if-let ((buf (get-buffer "🌐-site")))
      (with-current-buffer buf
        (when (get-buffer-process buf)
          (delete-process (get-buffer-process buf)))
        (let ((default-directory ac--directory-path))
          (eat-exec buf "🌐-site" "/usr/bin/fish" nil '("-c" "ac-site")))
        (message "✅ Site dev server restarted"))
    (message "No 🌐-site buffer found - try switching to 'web 1/2' tab first")))

(defun ac-restart-session ()
  "Restart the session server in the 📋-session buffer."
  (interactive)
  (if-let ((buf (get-buffer "📋-session")))
      (with-current-buffer buf
        (when (get-buffer-process buf)
          (delete-process (get-buffer-process buf)))
        (let ((default-directory ac--directory-path))
          (eat-exec buf "📋-session" "/usr/bin/fish" nil '("-c" "ac-session")))
        (message "✅ Session server restarted"))
    (message "No 📋-session buffer found")))

(defun ac-restart-redis ()
  "Restart redis in the 🔴-redis buffer."
  (interactive)
  (if-let ((buf (get-buffer "🔴-redis")))
      (with-current-buffer buf
        (when (get-buffer-process buf)
          (delete-process (get-buffer-process buf)))
        (let ((default-directory ac--directory-path))
          (eat-exec buf "🔴-redis" "/usr/bin/fish" nil '("-c" "ac-redis")))
        (message "✅ Redis restarted"))
    (message "No 🔴-redis buffer found")))

(defun ac-restart-kidlisp ()
  "Restart kidlisp test watcher in the 🧪-kidlisp buffer."
  (interactive)
  (if-let ((buf (get-buffer "🧪-kidlisp")))
      (with-current-buffer buf
        (when (get-buffer-process buf)
          (delete-process (get-buffer-process buf)))
        (let ((default-directory ac--directory-path))
          (eat-exec buf "🧪-kidlisp" "/usr/bin/fish" nil '("-c" "ac-kidlisp")))
        (message "✅ KidLisp test watcher restarted"))
    (message "No 🧪-kidlisp buffer found")))

(defun ac-restart-buffer (buffer-name command)
  "Generic restart function for any AC terminal buffer.
BUFFER-NAME is the full buffer name (e.g., \"🌐-site\").
COMMAND is the ac- fish command to run (e.g., \"site\")."
  (if-let ((buf (get-buffer buffer-name)))
      (with-current-buffer buf
        (when (get-buffer-process buf)
          (delete-process (get-buffer-process buf)))
        (let ((default-directory ac--directory-path))
          (eat-exec buf buffer-name "/usr/bin/fish" nil 
                    (list "-c" (format "ac-%s" command))))
        (message "✅ %s restarted" buffer-name))
    (message "Buffer %s not found" buffer-name)))

(defun ac--fishy-sentinel (process event)
  "Auto-restart fishy when process exits."
  (when (and (string-match-p "\\(finished\\|exited\\|killed\\)" event)
             (buffer-live-p (process-buffer process))
             (string= (buffer-name (process-buffer process)) "🐟-fishy"))
    (run-with-timer 0.1 nil #'ac-restart-fishy)))

;; Hook into eat to watch fishy process
(add-hook 'eat-mode-hook
          (lambda ()
            (when (string= (buffer-name) "🐟-fishy")
              (run-with-timer 0.5 nil
                (lambda ()
                  (when-let ((proc (get-buffer-process "🐟-fishy")))
                    (set-process-sentinel proc #'ac--fishy-sentinel)))))))

;;; --- Aesthetic Computer Restart Functions ---

(defvar ac--tab-names '("artery" "status" "stripe" "chat" "web 1/2" "web 2/2" "tests")
  "List of all AC tab names.")

(defvar ac--buffer-prefixes '("🩸" "📡" "⚡" "🚇" "💳" "🎫" "🤖" "🧠" "⏰" "🌐" "📋" "🔴" "🔖" "🧪" "🔥" "📦")
  "Emoji prefixes used for AC buffers.")

(defun ac-kill-all ()
  "Kill all AC processes, buffers, and tabs. Use before ac-restart."
  (interactive)
  (message "🛑 Killing all AC processes and buffers...")
  
  ;; 1. Stop artery bridge if running
  (when (and (boundp 'artery--process) artery--process (process-live-p artery--process))
    (condition-case nil (artery-stop) (error nil)))
  
  ;; 2. Kill all eat processes first (sends SIGTERM)
  (kill-eat-processes)
  
  ;; 3. Kill all AC emoji-prefixed buffers
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (cl-some (lambda (prefix) (string-prefix-p prefix name)) ac--buffer-prefixes)
        (condition-case nil
            (progn
              ;; Kill any process attached to buffer
              (when (get-buffer-process buffer)
                (delete-process (get-buffer-process buffer)))
              (kill-buffer buffer))
          (error nil)))))
  
  ;; 4. Kill artery-specific buffers
  (dolist (bufname '("🩸-artery" "🩸-daw-log" "🩸-bridge" "🩸-server"
                     "*artery*" "*artery-daw*" "*artery-bridge*" "*artery-server*"))
    (when-let ((buf (get-buffer bufname)))
      (condition-case nil
          (progn
            (when (get-buffer-process buf)
              (delete-process (get-buffer-process buf)))
            (kill-buffer buf))
        (error nil))))
  
  ;; 5. Close all AC tabs except current, then close that
  (let ((current-tab (alist-get 'name (tab-bar--current-tab))))
    ;; Close all other tabs first
    (dolist (tab-name ac--tab-names)
      (unless (string= tab-name current-tab)
        (condition-case nil
            (tab-bar-close-tab-by-name tab-name)
          (error nil))))
    ;; Try to close current tab (might fail if it's the last one)
    (when (member current-tab ac--tab-names)
      (condition-case nil
          (tab-bar-close-tab)
        (error nil))))
  
  ;; 6. Ensure we have at least scratch buffer
  (unless (get-buffer "*scratch*")
    (get-buffer-create "*scratch*"))
  (switch-to-buffer "*scratch*")
  
  (message "✅ All AC processes and buffers killed. Run M-x ac-restart to start fresh."))

(defun ac-restart (&optional target-tab)
  "Restart all AC tabs and processes from scratch.
Optional TARGET-TAB specifies which tab to land on (default: artery)."
  (interactive)
  (let ((target (or target-tab "artery")))
    (message "🔄 Restarting Aesthetic Computer...")
    ;; Kill everything first
    (ac-kill-all)
    ;; Wait a moment for processes to fully terminate
    (run-with-timer 1 nil
                    (lambda (tgt)
                      (message "🚀 Starting AC backend...")
                      (aesthetic-backend tgt))
                    target)))

(defun ac-restart-artery ()
  "Restart just the artery tab/process."
  (interactive)
  (message "🔄 Restarting artery...")
  
  ;; Stop artery bridge
  (when (and (boundp 'artery--process) artery--process)
    (condition-case nil (artery-stop) (error nil)))
  
  ;; Kill artery buffers
  (dolist (bufname '("🩸-artery" "🩸-daw-log" "🩸-bridge"))
    (when-let ((buf (get-buffer bufname)))
      (when (get-buffer-process buf)
        (delete-process (get-buffer-process buf)))
      (kill-buffer buf)))
  
  ;; Recreate artery tab
  (condition-case nil (tab-bar-close-tab-by-name "artery") (error nil))
  
  (run-with-timer 0.5 nil
                  (lambda ()
                    (tab-new)
                    (tab-rename "artery")
                    (let ((default-directory ac--directory-path))
                      (eat "fish -c 'ac-artery-dev'")
                      (when (get-buffer "*eat*")
                        (with-current-buffer "*eat*"
                          (rename-buffer "🩸-artery" t)
                          (eat-semi-char-mode)
                          (when (and (boundp 'evil-mode) evil-mode)
                            (evil-emacs-state)))))
                    (message "✅ Artery restarted"))))

;; Keybindings for restart functions
(global-set-key (kbd "C-c C-k") 'ac-kill-all)
(global-set-key (kbd "C-c C-S-r") 'ac-restart)

;; Aesthetic functions
(defun aesthetic ()
  (interactive)
  (eshell)
  (insert "python3 -m http.server 8888")
  (eshell-send-input))

(setq confirm-kill-processes nil)

;; DISABLED: These hooks were causing CPU spikes with 16+ terminals
;; Each terminal output triggers filter functions, overwhelming emacs
;; (add-hook 'eat-mode-hook
;;           (lambda ()
;;             (setq-local comint-scroll-to-bottom-on-output t
;;                         comint-show-maximum-output t
;;                         comint-move-point-for-output t
;;                         window-point-insertion-type t)
;;             (add-hook 'comint-output-filter-functions
;;                       #'comint-postoutput-scroll-to-bottom
;;                       nil t)))

;; Main backend function
(defvar ac--directory-path "~/aesthetic-computer")
(defvar ac--emoji-for-command
  '(("artery" . "🩸") ("status" . "📡") ("url" . "⚡") 
    ("tunnel" . "🚇") ("agent" . "⚡") ("stripe-print" . "💳")
    ("stripe-ticket" . "🎫") ("chat-system" . "🤖") ("chat-sotce" . "🧠")
    ("chat-clock" . "⏰") ("site" . "🌐") ("session" . "📋")
    ("redis" . "🔴") ("bookmarks" . "🔖") ("kidlisp" . "🧪")
    ("oven" . "🔥") ("media" . "📦") ("llm" . "🤖") ("top" . "📊")
    ("crash-diary" . "💥") ("views" . "🖼️")))

(defun ac--tab-exists-p (tab-name)
  "Check if a tab with TAB-NAME already exists."
  (cl-find tab-name (tab-bar-tabs) 
           :key (lambda (tab) (alist-get 'name tab))
           :test #'string=))

(defun ac--buffer-exists-p (buffer-name)
  "Check if a buffer with BUFFER-NAME already exists and has a live process."
  (when-let ((buf (get-buffer buffer-name)))
    (and (buffer-live-p buf)
         (or (get-buffer-process buf)
             (> (buffer-size buf) 0)))))

(defun ac--create-split-tab (tab-name commands)
  "Create a new tab with split windows running the specified commands.
Skips creation if tab already exists."
  ;; Guard: Skip if tab already exists
  (when (ac--tab-exists-p tab-name)
    (ac-debug-log (format "Tab '%s' already exists, skipping creation" tab-name))
    (cl-return-from ac--create-split-tab nil))
  
  (ac-debug-log (format "Creating tab: %s with commands: %s" tab-name commands))
  (ac-perf-log (format "Creating tab: %s" tab-name))
  (condition-case err
      (progn
        (tab-new)
        (tab-rename tab-name)
        (let ((default-directory ac--directory-path))
          (delete-other-windows)
          
          ;; Calculate layout
          (let* ((window-height (window-height))
                 (min-height-per-window 8)
                 (max-vertical-splits (/ window-height min-height-per-window))
                 (use-horizontal (< max-vertical-splits (length commands)))
                 (cmd-index 0))
            
            ;; Create windows and terminals - use eat-exec to avoid blocking
            (dolist (cmd commands)
              (unless (= cmd-index 0)
                (condition-case nil
                    (if use-horizontal
                        (progn (split-window-right) (other-window 1))
                      (progn (split-window-below) (other-window 1)))
                  (error nil)))
              
              (let ((actual-cmd (cond
                                  ((string= cmd "bookmarks") "servers")
                                  ((string= cmd "llm") "llm-continue")  ; Auto-resume last session
                                  (t cmd)))
                    (buf-name (format "%s-%s"
                                      (or (cdr (assoc cmd ac--emoji-for-command)) "🔧")
                                      cmd)))
                (ac-debug-log (format "Running command: ac-%s" actual-cmd))
                ;; Create eat buffer directly without blocking
                (let ((buf (generate-new-buffer buf-name)))
                  (with-current-buffer buf
                    (eat-mode)
                    (eat-exec buf buf-name "/usr/bin/fish" nil
                              (list "-c" (format "ac-%s" actual-cmd))))
                  (switch-to-buffer buf)
                  ;; Yield to let Emacs process events (prevents freeze)
                  (redisplay t)))
              
              (setq cmd-index (1+ cmd-index)))
            
            (balance-windows)
            (other-window 1))))
    (error (message "Error creating tab %s: %s" tab-name err))))

(defvar ac--startup-lock-file "/tmp/emacs-backend-startup.lock"
  "Lock file indicating aesthetic-backend is starting (crash monitor will be gentle).")

(defun aesthetic-backend (target-tab)
  (interactive)
  (setq target-tab (or target-tab "artery"))
  
  ;; Prevent double-invocation race condition
  (when ac--backend-started
    (ac-debug-log (format "aesthetic-backend already started, just switching to tab: %s" target-tab))
    ;; Just switch to the requested tab if backend already running
    (run-with-timer 0.5 nil
                    (lambda (target)
                      (condition-case nil
                          (when (member target '("artery" "fishy" "status" "stripe" "chat" "web 1/2" "web 2/2" "tests" "llm" "top" "views"))
                            (tab-bar-switch-to-tab target))
                        (error nil)))
                    target-tab)
    (cl-return-from aesthetic-backend nil))
  (setq ac--backend-started t)
  
  ;; Create startup lock file to tell crash monitor we're starting up
  ;; This prevents false "unresponsive" detection during heavy init
  (with-temp-file ac--startup-lock-file
    (insert (format "%s" (float-time))))
  (ac-debug-log "Created startup lock file - crash monitor will wait")
  
  (ac-debug-log (format "Starting aesthetic-backend with target-tab: %s" target-tab))
  (ac-perf-log "aesthetic-backend started")
  
  ;; Set environment variable to tell fish it's running inside Emacs
  (setenv "AC_EMACS_MODE" "t")
  
  ;; Prevent OSC color query responses from leaking (shows as "11;rgb:..." text)
  (setenv "COLORFGBG" "15;0")
  ;; NOTE: Disabled the bash watchdog - it may contribute to instability
  ;; The fish-based crash monitor in config.fish handles recovery instead
  ;; (let ((watchdog-script (expand-file-name "monitor-emacs.sh" ac--directory-path)))
  ;;   (when (file-exists-p watchdog-script)
  ;;     (ac-debug-log "Starting emacs watchdog...")
  ;;     (start-process "emacs-watchdog" nil "bash" watchdog-script)
  ;;     (ac-debug-log "Emacs watchdog started")))
  
  ;; Set up a watchdog timer to detect hangs (30 seconds)
  (run-with-timer 30 nil
                  (lambda ()
                    (ac-debug-log "WARNING: aesthetic-backend has been running for 30+ seconds")
                    (message "⚠️ Backend initialization taking longer than expected. Check .emacs-logs/")))

  ;; Clean up unwanted buffers before starting (with error handling)
    (condition-case nil
        (dolist (bufname '("*scratch*" "*Messages*" "*straight-process*" "*async-native-comp*"))
          (when-let ((buf (get-buffer bufname)))
            (dolist (win (get-buffer-window-list buf nil t))
              (with-selected-window win
                (switch-to-buffer (other-buffer buf t))))
            (kill-buffer buf)))
      (error nil))

    ;; Helper to check if a tab with a given name exists
    (defun ac--tab-exists-p (name)
      "Return t if a tab named NAME exists."
      (seq-find (lambda (tab) (string= name (alist-get 'name tab)))
                (tab-bar-tabs)))

    ;; Initialize the first tab as "artery" with artery CLI (dev mode with hot-reload)
    ;; Only create if the tab doesn't already exist
    (unless (ac--tab-exists-p "artery")
      (tab-rename "artery")
      (let ((default-directory ac--directory-path)
            (buf (or (get-buffer "🩸-artery")
                     (generate-new-buffer "🩸-artery"))))
        (with-current-buffer buf
          (unless (eq major-mode 'eat-mode)
            (eat-mode)
            (eat-exec buf "🩸-artery" "/usr/bin/fish" nil '("-c" "ac-artery-dev")))
          ;; Enable semi-char mode so TUI gets individual keypresses
          (eat-semi-char-mode)
          ;; Disable evil mode for artery - use emacs state
          (when (and (boundp 'evil-mode) evil-mode)
            (evil-emacs-state)))
        (switch-to-buffer buf)
        (redisplay t)))  ; Yield to prevent freeze

    ;; Create fishy tab (plain fish shell for quick terminal access via LLM)
    ;; Only create if the tab doesn't already exist
    (unless (ac--tab-exists-p "fishy")
      (tab-new)
      (tab-rename "fishy")
      (let ((default-directory ac--directory-path)
            (buf (or (get-buffer "🐟-fishy")
                     (generate-new-buffer "🐟-fishy"))))
        (with-current-buffer buf
          (unless (eq major-mode 'eat-mode)
            (eat-mode)
            (eat-exec buf "🐟-fishy" "/usr/bin/fish" nil nil))
          (eat-semi-char-mode)
          (when (and (boundp 'evil-mode) evil-mode)
            (evil-emacs-state)))
        (switch-to-buffer buf)
        (redisplay t)))

    ;; Create all the split tabs with staggered delays to prevent terminal garbling
    (let ((delay 0.5)
          (tab-specs '(("status"   ("url" "tunnel"))
                       ("stripe"   ("stripe-print" "stripe-ticket"))
                       ("chat"     ("chat-system" "chat-sotce" "chat-clock"))
                       ("web 1/2"  ("site" "session"))
                       ("web 2/2"  ("redis" "bookmarks" "oven" "media"))
                       ("tests"    ("kidlisp"))
                       ("views"    ("views"))  ; Extension views dev server (localhost:5555)
                       ("llm"      ("llm"))
                       ("top"      ("top"))
                       ("crash"    ("crash-diary")))))  ; Crash log viewer tab
      (dolist (tab-spec tab-specs)
        (let ((tab-name (car tab-spec))
              (commands (cadr tab-spec))
              (current-delay delay))
          (run-with-timer current-delay nil
                          (lambda (name cmds)
                            (ac--create-split-tab name cmds)
                            (redisplay t))
                          tab-name commands)
          (setq delay (+ delay 0.5)))))  ; 300ms between each tab

    ;; Switch to the requested tab (after all tabs created - 10 tabs * 0.3s = 3s + buffer)
    (run-with-timer 6.0 nil
                    (lambda (target)
                      (condition-case nil
                          (if (member target '("artery" "fishy" "status" "stripe" "chat" "web 1/2" "web 2/2" "tests" "llm" "top" "crash" "views"))
                              (progn
                                (tab-bar-switch-to-tab target)
                                ;; Also refresh the buffer to ensure it's scrolled properly
                                (when (eq major-mode 'eat-mode)
                                  (goto-char (point-max)))
                                (redisplay t))
                            (message "No such tab: %s" target))
                        (error nil)))
                    target-tab)

    ;; Start CDP tunnel in background after tabs are created
    (run-with-timer 8.0 nil #'ac-start-cdp-tunnel-async)
    
    ;; Remove startup lock file to signal we're done initializing
    ;; Give a bit more time for everything to settle
    (run-with-timer 10.0 nil
                    (lambda ()
                      (when (file-exists-p ac--startup-lock-file)
                        (delete-file ac--startup-lock-file)
                        (ac-debug-log "Removed startup lock - crash monitor now active")
                        (ac-perf-log "aesthetic-backend initialization complete")))))

(defun ac-start-cdp-tunnel-async ()
  "Start CDP tunnel to VS Code host in background (non-blocking)."
  (ac-debug-log "Starting CDP tunnel to host...")
  (let ((proc (start-process-shell-command 
               "cdp-tunnel" nil
               "fish -c 'ac-cdp-tunnel' >/dev/null 2>&1")))
    (when proc
      (set-process-sentinel 
       proc
       (lambda (p e)
         (if (= 0 (process-exit-status p))
             (ac-debug-log "CDP tunnel established")
           (ac-debug-log (format "CDP tunnel failed: %s" e))))))))

;;; ===================================================================
;;; AUTO-START AESTHETIC-BACKEND ON FIRST TERMINAL FRAME
;;; ===================================================================

(defvar ac--backend-started nil
  "Flag to ensure aesthetic-backend only runs once.")

(defun ac--maybe-start-backend (frame)
  "Start aesthetic-backend when first terminal frame connects."
  (when (and (not ac--backend-started)
             (not (display-graphic-p frame))  ; Terminal frame
             (frame-live-p frame))
    ;; DON'T set flag here - let aesthetic-backend set it when it actually runs
    (ac-debug-log "First terminal frame detected, scheduling aesthetic-backend")
    ;; Delay to ensure frame is fully ready
    (run-with-timer 1 nil
                    (lambda ()
                      (condition-case err
                          (aesthetic-backend "artery")
                        (error (message "Error starting aesthetic-backend: %s" err)))))))

;; Auto-start full backend on first terminal frame
(add-hook 'after-make-frame-functions #'ac--maybe-start-backend)

(defun aesthetic-backend-minimal ()
  "Minimal version - start artery and fishy only. Use M-x aesthetic-backend for all tabs."
  (interactive)
  (ac-debug-log "Starting aesthetic-backend-minimal")
  (setenv "AC_EMACS_MODE" "t")
  
  ;; Start with artery in the first tab
  (tab-rename "artery")
  (let ((default-directory ac--directory-path))
    (eat "fish -c 'ac-artery-dev'")
    (when (get-buffer "*eat*")
      (with-current-buffer "*eat*"
        (rename-buffer "🩸-artery" t)
        (eat-semi-char-mode))))
  
  ;; Add fishy tab after 2 seconds (only if not already created)
  (run-with-timer 2 nil
    (lambda ()
      (condition-case nil
          (unless (get-buffer "🐟-fishy")
            (tab-new)
            (tab-rename "fishy")
            (let ((default-directory ac--directory-path))
              (eat "fish")
              (when (get-buffer "*eat*")
                (with-current-buffer "*eat*"
                  (rename-buffer "🐟-fishy" t)
                  (eat-semi-char-mode))))
            ;; Switch back to artery
            (run-with-timer 1 nil (lambda () (tab-bar-switch-to-tab "artery"))))
        (error nil))))
  
  (message "🚀 Artery started! Use M-x aesthetic-backend-full to load all tabs.")
  (ac-debug-log "aesthetic-backend-minimal complete"))

(defun aesthetic-backend-full ()
  "Load all remaining tabs (status, stripe, chat, web, tests, views, llm, top)."
  (interactive)
  (message "Loading additional tabs...")
  (dolist (tab-spec '(("status"   ("url" "tunnel"))
                      ("stripe"   ("stripe-print" "stripe-ticket"))
                      ("chat"     ("chat-system" "chat-sotce" "chat-clock"))
                      ("web 1/2"  ("site" "session"))
                      ("web 2/2"  ("redis" "bookmarks" "oven" "media"))
                      ("tests"    ("kidlisp"))
                      ("views"    ("views"))
                      ("llm"      ("llm"))
                      ("top"      ("top"))))
    (let ((tab-name (car tab-spec))
          (commands (cadr tab-spec)))
      (condition-case err
          (progn
            (ac--create-split-tab tab-name commands)
            (message "Created tab: %s" tab-name))
        (error (message "Error creating tab %s: %s" tab-name err)))))
  (message "All tabs loaded!"))

;;; ===================================================================
;;; END OF AESTHETIC COMPUTER EMACS CONFIGURATION
;;; ===================================================================

(ac-debug-log "=== Emacs Configuration Load Complete ===")
(ac-perf-log "Config load complete")
