;;; spirals.lisp — Rotating spiral pattern
;;; Uses the ac-native CL graphics API directly.

(defpackage :piece.spirals
  (:use :cl))
(in-package :piece.spirals)

(defvar *frame* 0)

(defun paint (graph screen-w screen-h audio)
  "Paint one frame of the spiral pattern."
  (incf *frame*)
  (let* ((cx (floor screen-w 2))
         (cy (floor screen-h 2))
         (t-sec (/ *frame* 60.0))
         (arms 5)
         (points-per-arm 200))
    ;; Dark background with subtle fade
    (ac-native.graph:graph-wipe graph
      (ac-native.color:make-color :r 5 :g 5 :b 15 :a 255))
    ;; Draw spiral arms
    (dotimes (arm arms)
      (let ((arm-offset (* arm (/ (* 2 pi) arms))))
        (dotimes (i points-per-arm)
          (let* ((r (* (/ (float i) points-per-arm) (min cx cy) 0.9))
                 (angle (+ arm-offset
                           (* (/ (float i) points-per-arm) 4 pi)
                           (* t-sec 0.5)))
                 (x (+ cx (round (* r (sin (+ angle 1.5708))))))
                 (y (+ cy (round (* r (sin angle)))))
                 ;; Color cycles with position and time
                 (hue (mod (+ (* i 2) (* *frame* 3) (* arm 50)) 360))
                 (bright (max 80 (- 255 (round (* i 0.8))))))
            (when (and (>= x 0) (< x screen-w) (>= y 0) (< y screen-h))
              (ac-native.graph:graph-ink graph
                (ac-native.color:make-color
                 :r (min 255 (+ 60 (round (* bright (max 0 (sin (* hue 0.0174)))))))
                 :g (min 255 (+ 40 (round (* bright (max 0 (sin (* (+ hue 120) 0.0174)))))))
                 :b (min 255 (+ 80 (round (* bright (max 0 (sin (* (+ hue 240) 0.0174)))))))
                 :a 255))
              (ac-native.graph:graph-plot graph x y))))))
    ;; Occasional tone based on frame
    (when (and audio (zerop (mod *frame* 30)))
      (let ((freq (+ 200 (* 400 (sin (* t-sec 0.3))))))
        (ac-native.audio:audio-synth audio 0 freq 0.08 0.3 0.005 0.07 0.0)))))
