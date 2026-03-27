;;; waves.lisp — Sine wave visualization with sound

(defpackage :piece.waves
  (:use :cl))
(in-package :piece.waves)

(defvar *frame* 0)

(defun paint (graph screen-w screen-h audio)
  "Paint layered sine waves with matching audio."
  (incf *frame*)
  (let ((t-sec (/ *frame* 60.0)))
    ;; Dark blue-black background
    (ac-native.graph:graph-wipe graph
      (ac-native.color:make-color :r 2 :g 2 :b 12 :a 255))
    ;; Draw 5 layered sine waves
    (dotimes (layer 5)
      (let* ((freq-mult (+ 1.0 (* layer 0.7)))
             (amplitude (* (- screen-h 20) (/ 0.15 (1+ (* layer 0.3)))))
             (y-offset (+ (floor screen-h 2) (* (- layer 2) 20)))
             (phase (* t-sec freq-mult 0.8))
             (r (min 255 (round (* 50 (1+ layer)))))
             (g (min 255 (round (* 30 (- 5 layer)))))
             (b (min 255 (+ 100 (* 30 layer)))))
        (ac-native.graph:graph-ink graph
          (ac-native.color:make-color :r r :g g :b b :a 200))
        ;; Draw wave as connected line segments
        (let ((prev-y y-offset))
          (dotimes (x screen-w)
            (let* ((nx (/ (float x) screen-w))
                   (y (+ y-offset
                         (round (* amplitude
                                   (sin (+ phase (* nx freq-mult 2 pi)))))))
                   (cy (max 0 (min (1- screen-h) y))))
              (when (> x 0)
                (ac-native.graph:graph-line graph (1- x) prev-y x cy))
              (setf prev-y cy))))))
    ;; Audio: play a chord based on the wave frequencies
    (when (and audio (zerop (mod *frame* 60)))
      (dotimes (layer 3)
        (let ((freq (* 220 (+ 1.0 (* layer 0.5)))))
          (ac-native.audio:audio-synth audio 0 freq 0.06 0.8 0.01 0.3
                                        (- (* layer 0.3) 0.3)))))))
