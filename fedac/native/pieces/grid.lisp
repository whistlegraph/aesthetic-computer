;;; grid.lisp — Cellular automata grid with sonification

(defpackage :piece.grid
  (:use :cl))
(in-package :piece.grid)

(defvar *frame* 0)
(defvar *rng* (make-random-state t))
(defvar *cells* nil)
(defvar *cols* 0)
(defvar *rows* 0)

(defun init-grid (cols rows)
  (setf *cols* cols *rows* rows)
  (setf *cells* (make-array (* cols rows) :initial-element 0))
  ;; Random seed
  (dotimes (i (* cols rows))
    (when (< (random 100 *rng*) 30)
      (setf (aref *cells* i) (1+ (random 5 *rng*))))))

(defun cell-at (x y)
  (if (and (>= x 0) (< x *cols*) (>= y 0) (< y *rows*))
      (aref *cells* (+ (* y *cols*) x))
      0))

(defun step-grid ()
  "Simple rule: each cell averages neighbors, with random mutation."
  (let ((new (make-array (* *cols* *rows*) :initial-element 0)))
    (dotimes (y *rows*)
      (dotimes (x *cols*)
        (let ((sum (+ (cell-at (1- x) y) (cell-at (1+ x) y)
                      (cell-at x (1- y)) (cell-at x (1+ y)))))
          (setf (aref new (+ (* y *cols*) x))
                (mod (+ (floor sum 2)
                        (if (< (random 100 *rng*) 3) (random 6 *rng*) 0))
                     6)))))
    (setf *cells* new)))

(defun paint (graph screen-w screen-h audio)
  "Paint the cellular automata grid."
  (incf *frame*)
  (when (null *cells*)
    (init-grid (floor screen-w 4) (floor screen-h 4)))
  ;; Step every 4 frames
  (when (zerop (mod *frame* 4))
    (step-grid))
  ;; Draw
  (ac-native.graph:graph-wipe graph
    (ac-native.color:make-color :r 0 :g 0 :b 0 :a 255))
  (let ((cell-w 4) (cell-h 4)
        (active-count 0))
    (dotimes (y *rows*)
      (dotimes (x *cols*)
        (let ((v (cell-at x y)))
          (when (> v 0)
            (incf active-count)
            (let ((colors #((40 200 80) (200 60 60) (60 60 220)
                            (200 200 40) (180 60 200))))
              (let ((c (aref colors (min (1- (length colors)) (1- v)))))
                (ac-native.graph:graph-ink graph
                  (ac-native.color:make-color
                   :r (first c) :g (second c) :b (third c) :a 220))
                (ac-native.graph:graph-box graph
                  (* x cell-w) (* y cell-h) (1- cell-w) (1- cell-h) 1)))))))
    ;; Sonify: tone based on active cell count
    (when (and audio (zerop (mod *frame* 20)))
      (let ((freq (+ 100 (* 2 active-count))))
        (ac-native.audio:audio-synth audio 0 freq 0.05 0.3 0.003 0.06 0.0)))))
