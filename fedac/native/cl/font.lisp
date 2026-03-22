;;; Font rendering — bitmap font draw/measure

(in-package :ac-native.font)

(defun font-init ()
  "Load font data. Called once at startup."
  ;; TODO: populate *font-8x8* from header data
  )

(defun font-draw (graph text x y &key (size 1))
  "Draw TEXT at (X, Y) using the 8x8 bitmap font."
  (declare (ignore graph text x y size))
  ;; TODO: implement
  )

(defun font-measure (text &key (size 1))
  "Return the pixel width of TEXT in the 8x8 font."
  (declare (ignore size))
  (* (length text) 8))

(defun font-draw-matrix (graph text x y size)
  "Draw TEXT using the MatrixChunky8 font."
  (declare (ignore graph text x y size))
  ;; TODO: implement
  )

(defun font-measure-matrix (text size)
  "Return pixel width of TEXT in MatrixChunky8."
  (declare (ignore size))
  (* (length text) 10))
