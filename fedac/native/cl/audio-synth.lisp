;;; Audio synthesis — oscillators, envelopes

(in-package :ac-native.audio)

(defconstant +twopi+ (* 2.0d0 pi))

(declaim (inline osc-sine osc-square osc-triangle osc-sawtooth))

(defun osc-sine (phase)
  (declare (optimize (speed 3) (safety 0)) (type double-float phase))
  (sin phase))

(defun osc-square (phase)
  (declare (optimize (speed 3) (safety 0)) (type double-float phase))
  (if (< (mod phase +twopi+) pi) 1.0d0 -1.0d0))

(defun osc-triangle (phase)
  (declare (optimize (speed 3) (safety 0)) (type double-float phase))
  (let ((p (mod (/ phase +twopi+) 1.0d0)))
    (if (< p 0.5d0)
        (- (* 4.0d0 p) 1.0d0)
        (- 3.0d0 (* 4.0d0 p)))))

(defun osc-sawtooth (phase)
  (declare (optimize (speed 3) (safety 0)) (type double-float phase))
  (- (* 2.0d0 (mod (/ phase +twopi+) 1.0d0)) 1.0d0))

(defstruct voice
  "A synthesizer voice."
  (active nil :type boolean)
  (osc-type 0 :type fixnum)  ; 0=sine 1=square 2=tri 3=saw 4=noise
  (phase 0.0d0 :type double-float)
  (phase-inc 0.0d0 :type double-float)
  (volume 0.7d0 :type double-float)
  (pan 0.0d0 :type double-float)
  (attack 0.01d0 :type double-float)
  (decay 0.1d0 :type double-float)
  (duration 0.0d0 :type double-float)  ; 0 = infinite
  (elapsed 0.0d0 :type double-float)
  (fade 1.0d0 :type double-float)
  (id 0 :type fixnum))

(defun generate-sample (v)
  "Generate one sample from voice V."
  (declare (optimize (speed 3) (safety 0))
           (type voice v))
  (case (voice-osc-type v)
    (0 (osc-sine (voice-phase v)))
    (1 (osc-square (voice-phase v)))
    (2 (osc-triangle (voice-phase v)))
    (3 (osc-sawtooth (voice-phase v)))
    (4 (- (random 2.0d0) 1.0d0))  ; white noise
    (t 0.0d0)))
