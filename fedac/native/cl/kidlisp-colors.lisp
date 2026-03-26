;;; kidlisp-colors.lisp — Color table + rainbow/zebra for KidLisp

(in-package :ac-native.kidlisp)

(defvar *colors* (make-hash-table :test #'equal))

(macrolet ((defcolors (&rest pairs)
             `(progn
                ,@(loop for (name r g b) on pairs by #'cddddr
                        collect `(setf (gethash ,name *colors*) (list ,r ,g ,b))))))
  (defcolors
    "red" 255 0 0       "green" 0 128 0       "blue" 0 0 255
    "white" 255 255 255 "black" 0 0 0
    "cyan" 0 255 255    "magenta" 255 0 255   "yellow" 255 255 0
    "orange" 255 165 0  "pink" 255 192 203    "purple" 128 0 128
    "lime" 0 255 0      "aqua" 0 255 255      "navy" 0 0 128
    "maroon" 128 0 0    "olive" 128 128 0     "teal" 0 128 128
    "silver" 192 192 192 "gray" 128 128 128   "grey" 128 128 128
    "coral" 255 127 80  "salmon" 250 128 114
    "gold" 255 215 0    "indigo" 75 0 130     "violet" 238 130 238
    "crimson" 220 20 60 "turquoise" 64 224 208))

(defun hsv-to-rgb (h s v)
  "Convert HSV (h=0-360, s=0-1, v=0-1) to (r g b) in 0-255."
  (let* ((c (* v s))
         (x (* c (- 1.0 (abs (- (mod (/ h 60.0) 2.0) 1.0)))))
         (m (- v c))
         (r1 0.0) (g1 0.0) (b1 0.0))
    (cond ((< h 60)  (setf r1 c g1 x))
          ((< h 120) (setf r1 x g1 c))
          ((< h 180) (setf g1 c b1 x))
          ((< h 240) (setf g1 x b1 c))
          ((< h 300) (setf r1 x b1 c))
          (t         (setf r1 c b1 x)))
    (list (round (* (+ r1 m) 255))
          (round (* (+ g1 m) 255))
          (round (* (+ b1 m) 255)))))

(defun rainbow-color (frame &optional (speed 1.0))
  "Generate a cycling rainbow color based on frame count."
  (hsv-to-rgb (mod (* frame speed 2.0) 360.0) 1.0 1.0))

(defun zebra-color (frame)
  "Alternating black/white."
  (if (evenp frame) '(255 255 255) '(0 0 0)))

(defun color-name-p (name)
  "Is NAME a known color name, rainbow, or zebra?"
  (or (gethash name *colors*)
      (string= name "rainbow")
      (string= name "zebra")))

(defun resolve-color (args frame)
  "Resolve KidLisp color arguments to (r g b).
ARGS can be: a color name string, (r g b) numbers, 'rainbow', 'zebra',
or a single grayscale number."
  (cond
    ((null args) '(255 255 255))
    ;; Single string: color name
    ((and (= (length args) 1) (stringp (first args)))
     (let ((name (first args)))
       (cond ((string= name "rainbow") (rainbow-color frame))
             ((string= name "zebra") (zebra-color frame))
             ((gethash name *colors*) (gethash name *colors*))
             (t '(255 255 255)))))
    ;; Single number: grayscale
    ((and (= (length args) 1) (numberp (first args)))
     (let ((v (max 0 (min 255 (round (first args))))))
       (list v v v)))
    ;; Three numbers: r g b
    ((and (>= (length args) 3)
          (numberp (first args))
          (numberp (second args))
          (numberp (third args)))
     (list (max 0 (min 255 (round (first args))))
           (max 0 (min 255 (round (second args))))
           (max 0 (min 255 (round (third args))))))
    ;; Four numbers: r g b a (ignore alpha for now)
    ((and (>= (length args) 4)
          (numberp (first args)))
     (list (max 0 (min 255 (round (first args))))
           (max 0 (min 255 (round (second args))))
           (max 0 (min 255 (round (third args))))))
    ;; Fallback
    (t '(255 255 255))))
