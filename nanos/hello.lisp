(format t "Hello, SBCL!~%")
 
(load "quicklisp.lisp")

(quicklisp-quickstart:install)

(ql:quickload "hunchentoot")

(defun hello-world-handler (request)
  (declare (ignore request))
  (setf (hunchentoot:content-type*) "text/plain")
  (setf (hunchentoot:return-code*) 200)
  (format nil "Hello, World!"))

(defun start-server ()
  (let ((dispatcher (hunchentoot:create-prefix-dispatcher "/" 'hello-world-handler)))
    (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor 
                                      :port 8083
                                      :dispatcher dispatcher))))

(start-server)