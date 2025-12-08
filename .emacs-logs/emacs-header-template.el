;; -*- lexical-binding: t; -*-
;; Aesthetic Computer Emacs Configuration, 2024.3.13.12.51

;; Debug logging - persistent location that survives container rebuilds
(defvar ac-debug-log-file "/workspaces/aesthetic-computer/.emacs-logs/emacs-debug.log")
(defvar ac-perf-log-file "/workspaces/aesthetic-computer/.emacs-logs/emacs-perf.log")
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

(ac-perf-session-start)
(ac-debug-log "=== Starting Emacs Configuration Load ===")
(ac-perf-log "Config load started")
