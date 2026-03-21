; This file is old now and was
; meant for getting sbcl running in a unikernel.

;(declaim (optimize (debug 3)))  ; Set the debugging level
;(sb-ext:enable-debugger)        ; Enable the SBCL debugger

(format t "Hello!~%")

(load "quicklisp.lisp")

(cond
 ((probe-file "./quicklisp/setup.lisp") ; Condition
  (format t "Setup found...~%")
  (load "./quicklisp/setup.lisp")       ; True branch
  (format t "Quicklisp setup loaded.~%"))
 (t                                    ; Else branch
  (format t "Quicklisp setup not found.~%")
  (quicklisp-quickstart:install :path "./quicklisp")
  (format t "Installed quicklisp...~%")))

(ql:quickload "hunchentoot")

(format t "Loaded hunchentoot!")

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8083))