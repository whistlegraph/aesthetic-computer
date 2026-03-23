;;; Font rendering — 6x10 bitmap font

(in-package :ac-native.font)

(defun font-init ()
  "Load font data. Called once at startup."
  ;; Data is statically defined in font-data.lisp
  )

(defun font-draw (graph text x y &key (size 1))
  "Draw TEXT at (X, Y) using the 6x10 bitmap font, scaled by SIZE."
  (declare (type fixnum x y size))
  (let ((cx x))
    (loop for ch across text do
      (let ((idx (- (char-code ch) 32)))
        (when (and (>= idx 0) (< idx 95))
          (dotimes (row +font-h+)
            (let ((byte (aref *font-6x10* idx row)))
              (dotimes (col +font-w+)
                (when (logbitp (- 7 col) byte)
                  (if (= size 1)
                      (graph-plot graph (+ cx col) (+ y row))
                      ;; Scaled: draw size x size block per pixel
                      (dotimes (dy size)
                        (dotimes (dx size)
                          (graph-plot graph
                                     (+ cx (* col size) dx)
                                     (+ y (* row size) dy)))))))))))
      (incf cx (* +font-w+ size)))))

(defun font-measure (text &key (size 1))
  "Return the pixel width of TEXT in the 6x10 font."
  (* (length text) +font-w+ size))
