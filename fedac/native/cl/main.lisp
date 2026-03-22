;;; Main entry point — AC Native OS (Common Lisp edition)

(in-package :ac-native)

(defvar *running* t "Main loop flag.")

(defun compute-pixel-scale (display-w)
  "Compute pixel scale targeting ~300px wide."
  (let ((target (max 1 (min 16 (floor display-w 300)))))
    ;; Prefer clean divisors within ±3
    (loop for delta from 0 to 3 do
      (let ((s (+ target delta)))
        (when (and (>= s 1) (<= s 16)
                   (zerop (mod display-w s)))
          (return-from compute-pixel-scale s)))
      (let ((s (- target delta)))
        (when (and (>= s 1)
                   (zerop (mod display-w s)))
          (return-from compute-pixel-scale s))))
    target))

(defun mount-minimal-fs ()
  "Mount essential pseudo-filesystems (PID 1 only)."
  (ac-native.syscalls:sys-mount "proc" "/proc" "proc")
  (ac-native.syscalls:sys-mount "sysfs" "/sys" "sysfs")
  (ac-native.syscalls:sys-mount "devtmpfs" "/dev" "devtmpfs")
  (ac-log "mounted proc, sysfs, devtmpfs~%"))

(defun main ()
  "AC Native OS entry point."
  (ac-log "~%═══════════════════════════════════════~%")
  (ac-log "  AC Native OS (Common Lisp)~%")
  (ac-log "  SBCL ~A~%" (lisp-implementation-version))
  (ac-log "═══════════════════════════════════════~%~%")

  ;; PID 1 duties
  (when (= (ac-native.syscalls:sys-getpid) 1)
    (mount-minimal-fs))

  ;; Init display
  (let ((display (ac-native.drm:drm-init)))
    (unless display
      (ac-log "FATAL: no display~%")
      (return-from main 1))

    (let* ((dw (ac-native.drm:display-width display))
           (dh (ac-native.drm:display-height display))
           (scale (compute-pixel-scale dw))
           (sw (floor dw scale))
           (sh (floor dh scale))
           (screen (fb-create sw sh))
           (graph (make-graph :fb screen :screen screen)))

      (ac-log "display: ~Dx~D  scale: ~D  screen: ~Dx~D~%"
              dw dh scale sw sh)

      ;; Main loop
      (setf *running* t)
      (unwind-protect
          (loop while *running* do
            ;; TODO: input-poll
            ;; TODO: js-call-act
            ;; TODO: js-call-sim

            ;; Demo: cycle background color
            (let* ((t-ms (monotonic-time-ms))
                   (r (floor (+ 128 (* 127 (sin (* t-ms 0.001))))))
                   (g (floor (+ 128 (* 127 (sin (* t-ms 0.0013))))))
                   (b (floor (+ 128 (* 127 (sin (* t-ms 0.0017)))))))
              (graph-wipe graph (make-color :r r :g g :b b))
              ;; Draw a box in the center
              (graph-ink graph (make-color :r 255 :g 255 :b 255 :a 200))
              (graph-box graph (- (floor sw 2) 20) (- (floor sh 2) 20) 40 40)
              ;; Draw a circle
              (graph-ink graph (make-color :r 255 :g 100 :b 50))
              (graph-circle graph (floor sw 2) (floor sh 2) 15))

            ;; Present
            (ac-native.drm:drm-present display screen scale)
            (frame-sync-60fps))

        ;; Cleanup
        (fb-destroy screen)
        (ac-native.drm:drm-destroy display)
        (ac-log "shutdown complete~%")))))
