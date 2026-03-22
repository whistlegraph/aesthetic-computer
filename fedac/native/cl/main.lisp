;;; Main entry point — AC Native OS (Common Lisp edition)
;;; First boot: just show a colored screen to prove DRM works

(in-package :ac-native)

(defvar *running* t "Main loop flag.")

(defun compute-pixel-scale (display-w)
  "Compute pixel scale targeting ~300px wide."
  (let ((target (max 1 (min 16 (floor display-w 300)))))
    (loop for delta from 0 to 3 do
      (let ((s (+ target delta)))
        (when (and (>= s 1) (<= s 16) (zerop (mod display-w s)))
          (return-from compute-pixel-scale s)))
      (let ((s (- target delta)))
        (when (and (>= s 1) (zerop (mod display-w s)))
          (return-from compute-pixel-scale s))))
    target))

(defun main ()
  "AC Native OS entry point."
  ;; Log to stderr (visible on serial console)
  (format *error-output* "~%════════════════════════════════════~%")
  (format *error-output* "  AC Native OS (Common Lisp)~%")
  (format *error-output* "  SBCL ~A~%" (lisp-implementation-version))
  (format *error-output* "════════════════════════════════════~%~%")
  (force-output *error-output*)

  ;; Try to init display
  (let ((display (handler-case (ac-native.drm:drm-init)
                   (error (e)
                     (format *error-output* "[cl] DRM init error: ~A~%" e)
                     (force-output *error-output*)
                     nil))))
    (unless display
      (format *error-output* "[cl] FATAL: no display — sleeping 30s~%")
      (force-output *error-output*)
      (sleep 30)
      (return-from main 1))

    (let* ((dw (ac-native.drm:display-width display))
           (dh (ac-native.drm:display-height display))
           (scale (compute-pixel-scale dw))
           (sw (floor dw scale))
           (sh (floor dh scale))
           (screen (fb-create sw sh))
           (graph (make-graph :fb screen :screen screen)))

      (format *error-output* "[cl] display: ~Dx~D  scale: ~D  screen: ~Dx~D~%"
              dw dh scale sw sh)
      (force-output *error-output*)

      ;; Main loop — just cycle colors
      (setf *running* t)
      (let ((frame 0))
        (unwind-protect
            (loop while *running* do
              (incf frame)
              ;; Cycle background
              (let* ((t-val (* frame 0.02))
                     (r (floor (+ 40 (* 40 (sin t-val)))))
                     (g (floor (+ 20 (* 20 (sin (* t-val 1.3))))))
                     (b (floor (+ 80 (* 80 (sin (* t-val 0.7)))))))
                (graph-wipe graph (make-color :r r :g g :b b))

                ;; White box in center
                (graph-ink graph (make-color :r 255 :g 255 :b 255 :a 200))
                (graph-box graph (- (floor sw 2) 30) (- (floor sh 2) 30) 60 60)

                ;; Orange circle
                (graph-ink graph (make-color :r 255 :g 140 :b 50))
                (graph-circle graph (floor sw 2) (floor sh 2) 20))

              ;; Present
              (ac-native.drm:drm-present display screen scale)
              (frame-sync-60fps)

              ;; Exit after 10 seconds (for testing)
              (when (> frame 600)
                (setf *running* nil)))

          ;; Cleanup
          (fb-destroy screen)
          (ac-native.drm:drm-destroy display)
          (format *error-output* "[cl] shutdown~%")
          (force-output *error-output*))))))
