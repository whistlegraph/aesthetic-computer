;;; kidlisp-builtins.lisp — Built-in function table for KidLisp
;;; Each builtin is (lambda (instance evaluated-args) ...)

(in-package :ac-native.kidlisp)

(defvar *builtins* (make-hash-table :test #'equal))

(defmacro defbuiltin (name params &body body)
  `(setf (gethash ,name *builtins*)
         (lambda ,params ,@body)))

;;; ── Graphics ──

(defbuiltin "wipe" (inst args)
  (let ((rgb (resolve-color args (kidlisp-instance-frame-count inst))))
    (when (kidlisp-instance-graph inst)
      (ac-native.graph:graph-wipe
       (kidlisp-instance-graph inst)
       (ac-native.color:make-color
        :r (first rgb) :g (second rgb) :b (third rgb) :a 255)))))

(defbuiltin "ink" (inst args)
  (let ((rgb (resolve-color args (kidlisp-instance-frame-count inst))))
    (setf (kidlisp-instance-ink-r inst) (first rgb)
          (kidlisp-instance-ink-g inst) (second rgb)
          (kidlisp-instance-ink-b inst) (third rgb))
    (when (kidlisp-instance-graph inst)
      (ac-native.graph:graph-ink
       (kidlisp-instance-graph inst)
       (ac-native.color:make-color
        :r (first rgb) :g (second rgb) :b (third rgb) :a 255)))))

(defbuiltin "line" (inst args)
  (when (and (kidlisp-instance-graph inst) (>= (length args) 4))
    (ac-native.graph:graph-line
     (kidlisp-instance-graph inst)
     (round (or (nth 0 args) 0)) (round (or (nth 1 args) 0))
     (round (or (nth 2 args) 0)) (round (or (nth 3 args) 0)))))

(defbuiltin "box" (inst args)
  (when (and (kidlisp-instance-graph inst) (>= (length args) 4))
    (ac-native.graph:graph-box
     (kidlisp-instance-graph inst)
     (round (or (nth 0 args) 0)) (round (or (nth 1 args) 0))
     (round (or (nth 2 args) 0)) (round (or (nth 3 args) 0))
     1)))

(defbuiltin "circle" (inst args)
  (when (and (kidlisp-instance-graph inst) (>= (length args) 3))
    (ac-native.graph:graph-circle
     (kidlisp-instance-graph inst)
     (round (or (nth 0 args) 0)) (round (or (nth 1 args) 0))
     (round (or (nth 2 args) 0)))))

(defbuiltin "plot" (inst args)
  (when (and (kidlisp-instance-graph inst) (>= (length args) 2))
    (ac-native.graph:graph-plot
     (kidlisp-instance-graph inst)
     (round (or (nth 0 args) 0)) (round (or (nth 1 args) 0)))))

;;; ── Pixel transforms ──

(defbuiltin "scroll" (inst args)
  (when (kidlisp-instance-graph inst)
    (let ((dx (round (or (nth 0 args) 0)))
          (dy (round (or (nth 1 args) 0)))
          (fb (ac-native.graph:graph-fb (kidlisp-instance-graph inst))))
      (when fb
        (ac-native.framebuffer:fb-scroll fb dx dy)))))

(defbuiltin "zoom" (inst args)
  (when (kidlisp-instance-graph inst)
    (let ((factor (or (nth 0 args) 1.0))
          (fb (ac-native.graph:graph-fb (kidlisp-instance-graph inst))))
      (when (and fb (numberp factor))
        (ac-native.framebuffer:fb-zoom fb factor)))))

(defbuiltin "spin" (inst args)
  (when (kidlisp-instance-graph inst)
    (let ((angle (or (nth 0 args) 0.0))
          (fb (ac-native.graph:graph-fb (kidlisp-instance-graph inst))))
      (when (and fb (numberp angle))
        (ac-native.framebuffer:fb-spin fb angle)))))

(defbuiltin "contrast" (inst args)
  (when (kidlisp-instance-graph inst)
    (let ((factor (or (nth 0 args) 1.0))
          (fb (ac-native.graph:graph-fb (kidlisp-instance-graph inst))))
      (when (and fb (numberp factor))
        (ac-native.framebuffer:fb-contrast fb factor)))))

;;; ── Math ──

(defbuiltin "+" (inst args)
  (declare (ignore inst))
  (apply #'+ (remove-if-not #'numberp args)))

(defbuiltin "-" (inst args)
  (declare (ignore inst))
  (if (= (length args) 1)
      (- (first args))
      (apply #'- (remove-if-not #'numberp args))))

(defbuiltin "*" (inst args)
  (declare (ignore inst))
  (apply #'* (remove-if-not #'numberp args)))

(defbuiltin "/" (inst args)
  (declare (ignore inst))
  (if (and (>= (length args) 2) (not (zerop (second args))))
      (/ (float (first args)) (float (second args)))
      0))

(defbuiltin "%" (inst args)
  (declare (ignore inst))
  (if (and (>= (length args) 2) (not (zerop (second args))))
      (mod (first args) (second args))
      0))
(setf (gethash "mod" *builtins*) (gethash "%" *builtins*))

(defbuiltin "sin" (inst args)
  (declare (ignore inst))
  (when (numberp (first args)) (sin (float (first args)))))

(defbuiltin "cos" (inst args)
  (declare (ignore inst))
  (when (numberp (first args)) (cos (float (first args)))))

(defbuiltin "abs" (inst args)
  (declare (ignore inst))
  (when (numberp (first args)) (abs (first args))))

(defbuiltin "floor" (inst args)
  (declare (ignore inst))
  (when (numberp (first args)) (floor (first args))))

(defbuiltin "ceil" (inst args)
  (declare (ignore inst))
  (when (numberp (first args)) (ceiling (first args))))

(defbuiltin "round" (inst args)
  (declare (ignore inst))
  (when (numberp (first args)) (round (first args))))

(defbuiltin "sqrt" (inst args)
  (declare (ignore inst))
  (when (and (numberp (first args)) (>= (first args) 0))
    (sqrt (float (first args)))))

(defbuiltin "min" (inst args)
  (declare (ignore inst))
  (apply #'min (remove-if-not #'numberp args)))

(defbuiltin "max" (inst args)
  (declare (ignore inst))
  (apply #'max (remove-if-not #'numberp args)))

(defbuiltin "pow" (inst args)
  (declare (ignore inst))
  (when (and (numberp (first args)) (numberp (second args)))
    (expt (float (first args)) (float (second args)))))

(defbuiltin "random" (inst args)
  (let ((n (or (first args) 1)))
    (when (numberp n)
      (if (integerp n)
          (random n (kidlisp-instance-random-state inst))
          (* n (random 1.0 (kidlisp-instance-random-state inst)))))))

(defbuiltin "wiggle" (inst args)
  (let ((n (or (first args) 1)))
    (when (numberp n)
      (random n (kidlisp-instance-random-state inst)))))

;;; ── System ──

(defbuiltin "width" (inst args)
  (declare (ignore args))
  (kidlisp-instance-screen-w inst))

(defbuiltin "height" (inst args)
  (declare (ignore args))
  (kidlisp-instance-screen-h inst))

(defbuiltin "frame" (inst args)
  (declare (ignore args))
  (kidlisp-instance-frame-count inst))

;;; ── Audio (stubs for now) ──

(defbuiltin "tone" (inst args)
  (declare (ignore inst args))
  nil)

(defbuiltin "overtone" (inst args)
  (declare (ignore inst args))
  nil)

(defbuiltin "melody" (inst args)
  (declare (ignore inst args))
  nil)

;;; ── Output ──

(defbuiltin "log" (inst args)
  (declare (ignore inst))
  (format *error-output* "[kidlisp] ~{~A ~}~%" args)
  (force-output *error-output*))

(defbuiltin "print" (inst args)
  (declare (ignore inst))
  (format *error-output* "[kidlisp] ~{~A ~}~%" args)
  (force-output *error-output*))
