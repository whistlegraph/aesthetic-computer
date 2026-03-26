;;; kidlisp-package.lisp — Package definition for CL-native KidLisp evaluator

(defpackage :ac-native.kidlisp
  (:use :cl)
  (:export #:make-kidlisp-instance
           #:kidlisp-parse
           #:kidlisp-evaluate
           #:kidlisp-frame
           #:run-kidlisp-piece
           #:resolve-color))
