(declaim (optimize (debug 3)))  ; Set the debugging level
(sb-ext:enable-debugger)        ; Enable the SBCL debugger

(format t "Hello, SBCL!~%")

;;(handler-case
;;    (progn
      (load "quicklisp.lisp")
      (format t "Loaded quicklisp...~%")
      
      ;(quicklisp-quickstart:install)
      (quicklisp-quickstart:install :path ".quicklisp/")

      (format t "Installed quicklisp...~%")

      ;; Uncomment if needed
      ;; (load "quicklisp/setup.lisp")

      ; (ql:quickload "hunchentoot")
;;      (format t "Loaded hunchentoot!~%"))
;;  (error (e)
;;    (format t "An error occurred: ~A~%" e)))  ; Print any caught error

 
;(load "quicklisp.lisp")
;(format t "Loaded quicklisp...")

;(quicklisp-quickstart:install)

;; (load "quicklisp/setup.lisp")


;(format t "Installed quicklisp...")

;(ql:quickload "hunchentoot")

;(format t "Loaded hunchentoot!")

;(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8083))

;(defun hello-world-handler (request)
;  (declare (ignore request))
;  (setf (hunchentoot:content-type*) "text/plain")
;  (setf (hunchentoot:return-code*) 200)
;  (format nil "Hello, World!"))
;
;(defun start-server ()
;  (let ((dispatcher (hunchentoot:create-prefix-dispatcher "/" 'hello-world-handler)))
;    (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor 
;                                      :port 8083
;                                      :dispatcher dispatcher))))

;(start-server)
