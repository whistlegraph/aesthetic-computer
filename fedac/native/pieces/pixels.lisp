;;; pixels.lisp — Animated pixel noise with color drift

(defpackage :piece.pixels
  (:use :cl))
(in-package :piece.pixels)

(defvar *frame* 0)
(defvar *rng* (make-random-state t))

(defun paint (graph screen-w screen-h audio)
  "Paint one frame of pixel noise."
  (incf *frame*)
  ;; Don't wipe — let pixels accumulate and drift
  (let* ((density (+ 50 (round (* 200 (abs (sin (/ *frame* 120.0)))))))
         (hue-base (mod (* *frame* 2) 360)))
    (dotimes (i density)
      (let* ((x (random screen-w *rng*))
             (y (random screen-h *rng*))
             (hue (mod (+ hue-base (random 60 *rng*)) 360))
             (r (min 255 (max 0 (round (* 255 (max 0 (sin (* hue 0.0174))))))))
             (g (min 255 (max 0 (round (* 255 (max 0 (sin (* (+ hue 120) 0.0174))))))))
             (b (min 255 (max 0 (round (* 255 (max 0 (sin (* (+ hue 240) 0.0174)))))))))
        (ac-native.graph:graph-ink graph
          (ac-native.color:make-color :r r :g g :b b :a 255))
        (ac-native.graph:graph-plot graph x y)))
    ;; Sonify: tone based on average hue
    (when (and audio (zerop (mod *frame* 15)))
      (let ((freq (+ 100 (* 3 hue-base))))
        (ac-native.audio:audio-synth audio 0 freq 0.04 0.2 0.002 0.04 0.0)))))
