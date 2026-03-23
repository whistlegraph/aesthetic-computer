;;; Graphics primitives — wipe, ink, plot, line, box, circle

(in-package :ac-native.graph)

(defstruct graph
  "Immediate-mode 2D graphics context."
  (fb nil)     ; ac-native.framebuffer:framebuffer
  (screen nil) ; ac-native.framebuffer:framebuffer
  (ink-color (make-color :r 255 :g 255 :b 255 :a 255)))

(defun graph-wipe (g color)
  "Clear the screen with COLOR."
  (fb-clear (graph-fb g) (color-pack-argb32 color)))

(defun graph-ink (g color)
  "Set the current drawing color."
  (setf (graph-ink-color g) color))

(defun graph-plot (g x y)
  "Plot a single pixel at (X, Y) using the current ink."
  (let ((c (graph-ink-color g)))
    (if (= (color-a c) 255)
        (fb-put-pixel (graph-fb g) x y (color-pack-argb32 c))
        (fb-blend-pixel (graph-fb g) x y c))))

(defun graph-line (g x0 y0 x1 y1)
  "Draw a line from (X0,Y0) to (X1,Y1) using Bresenham's algorithm."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum x0 y0 x1 y1))
  (let ((dx (abs (- x1 x0)))
        (dy (- (abs (- y1 y0))))
        (sx (if (< x0 x1) 1 -1))
        (sy (if (< y0 y1) 1 -1)))
    (let ((err (+ dx dy))
          (x x0) (y y0))
      (declare (type fixnum err x y))
      (loop
        (graph-plot g x y)
        (when (and (= x x1) (= y y1)) (return))
        (let ((e2 (* 2 err)))
          (when (>= e2 dy)
            (incf err dy)
            (incf x sx))
          (when (<= e2 dx)
            (incf err dx)
            (incf y sy)))))))

(defun graph-box (g x y w h &optional (filled t))
  "Draw a rectangle. If FILLED, fill it; otherwise draw outline."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum x y w h))
  (let ((c (graph-ink-color g))
        (fb (graph-fb g)))
    (if filled
        ;; Filled box — hot path, optimize
        (let ((c32 (color-pack-argb32 c))
              (opaque (= (color-a c) 255)))
          (dotimes (row h)
            (let ((py (+ y row)))
              (when (and (>= py 0) (< py (fb-height fb)))
                (dotimes (col w)
                  (let ((px (+ x col)))
                    (when (and (>= px 0) (< px (fb-width fb)))
                      (if opaque
                          (fb-put-pixel fb px py c32)
                          (fb-blend-pixel fb px py c)))))))))
        ;; Outline
        (progn
          (graph-line g x y (+ x w -1) y)
          (graph-line g x (+ y h -1) (+ x w -1) (+ y h -1))
          (graph-line g x y x (+ y h -1))
          (graph-line g (+ x w -1) y (+ x w -1) (+ y h -1))))))

(defun graph-circle (g cx cy r &optional (filled t))
  "Draw a circle using the midpoint algorithm."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum cx cy r))
  (if filled
      ;; Filled circle — draw horizontal spans
      (let ((x 0) (y r) (d (- 1 r)))
        (declare (type fixnum x y d))
        (loop while (<= x y) do
          (graph-line g (- cx x) (+ cy y) (+ cx x) (+ cy y))
          (graph-line g (- cx x) (- cy y) (+ cx x) (- cy y))
          (graph-line g (- cx y) (+ cy x) (+ cx y) (+ cy x))
          (graph-line g (- cx y) (- cy x) (+ cx y) (- cy x))
          (incf x)
          (if (< d 0)
              (incf d (+ (* 2 x) 1))
              (progn (decf y) (incf d (+ (* 2 (- x y)) 1))))))
      ;; Outline circle
      (let ((x 0) (y r) (d (- 1 r)))
        (declare (type fixnum x y d))
        (loop while (<= x y) do
          (graph-plot g (+ cx x) (+ cy y))
          (graph-plot g (- cx x) (+ cy y))
          (graph-plot g (+ cx x) (- cy y))
          (graph-plot g (- cx x) (- cy y))
          (graph-plot g (+ cx y) (+ cy x))
          (graph-plot g (- cx y) (+ cy x))
          (graph-plot g (+ cx y) (- cy x))
          (graph-plot g (- cx y) (- cy x))
          (incf x)
          (if (< d 0)
              (incf d (+ (* 2 x) 1))
              (progn (decf y) (incf d (+ (* 2 (- x y)) 1))))))))
