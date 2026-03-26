;;; kidlisp-runner.lisp — Run KidLisp pieces in the CL main loop

(in-package :ac-native.kidlisp)

(defun run-kidlisp-piece (source &key (label "$code"))
  "Run a KidLisp piece with full DRM graphics, audio, and input.
SOURCE is the KidLisp source code string."
  (format *error-output* "~%════════════════════════════════════~%")
  (format *error-output* "  KidLisp (~A)~%" label)
  (format *error-output* "  SBCL ~A~%" (lisp-implementation-version))
  (format *error-output* "════════════════════════════════════~%~%")
  (force-output *error-output*)

  ;; Parse the source
  (let ((ast (handler-case (kidlisp-parse source)
               (error (e)
                 (format *error-output* "[kidlisp] Parse error: ~A~%" e)
                 (force-output *error-output*)
                 (return-from run-kidlisp-piece)))))
    (format *error-output* "[kidlisp] Parsed ~A: ~A top-level forms~%"
            label (if (and (listp ast) (string= (first ast) "progn"))
                      (1- (length ast))
                      1))
    (force-output *error-output*)

    ;; Initialize display
    (let ((display (ac-native.drm:drm-init)))
      (unless display
        (format *error-output* "[kidlisp] DRM init failed~%")
        (return-from run-kidlisp-piece))

      (let* ((dw (ac-native.drm:display-width display))
             (dh (ac-native.drm:display-height display))
             (scale (cond ((>= (min dw dh) 1440) 6)
                          ((>= (min dw dh) 1080) 4)
                          ((>= (min dw dh) 720) 3)
                          (t 2)))
             (sw (floor dw scale))
             (sh (floor dh scale))
             (screen (ac-native.framebuffer:fb-create sw sh))
             (graph (ac-native.graph:graph-create screen))
             (input (ac-native.input:input-init dw dh scale))
             (audio (ac-native.audio:audio-init)))

        (format *error-output* "[kidlisp] ~Dx~D scale:~D -> ~Dx~D~%" dw dh scale sw sh)
        (force-output *error-output*)

        ;; Create KidLisp instance
        (let ((inst (make-kidlisp-instance
                     :ast ast
                     :graph graph
                     :screen-w sw
                     :screen-h sh
                     :audio audio)))

          ;; Main loop
          (unwind-protect
              (let ((running t))
                (loop while running do
                  ;; Input
                  (ac-native.input:input-poll input)
                  (dotimes (i (ac-native.input:input-event-count input))
                    (let ((ev (ac-native.input:input-event input i)))
                      (when (and (= (ac-native.input:event-type ev) 1)  ; keyboard down
                                 (= (ac-native.input:event-code ev) 1)) ; ESC
                        (setf running nil))))

                  ;; Evaluate one frame
                  (handler-case
                      (kidlisp-frame inst)
                    (error (e)
                      (format *error-output* "[kidlisp] Runtime error: ~A~%" e)
                      (force-output *error-output*)))

                  ;; Present
                  (ac-native.drm:drm-present display screen scale)

                  ;; 60fps sync
                  (sleep 1/60)))

            ;; Cleanup
            (when audio (ac-native.audio:audio-destroy audio))
            (ac-native.input:input-destroy input)
            (ac-native.framebuffer:fb-destroy screen)
            (ac-native.drm:drm-destroy display)
            (format *error-output* "[kidlisp] Exited~%")
            (force-output *error-output*)))))))
