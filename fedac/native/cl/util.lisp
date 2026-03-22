;;; Utility functions — logging, timing

(in-package :ac-native.util)

(defvar *log-stream* *error-output*
  "Where ac-log writes. Rebound to a file stream when USB is mounted.")

(defvar *log-file* nil "Open file stream for USB log, or nil.")

(defun ac-log (fmt &rest args)
  "Log a message to stderr and optionally to USB log file."
  (let ((msg (apply #'format nil fmt args)))
    (write-string msg *error-output*)
    (force-output *error-output*)
    (when *log-file*
      (write-string msg *log-file*)
      (force-output *log-file*))))

(defun open-usb-log (path)
  "Open a log file on the USB drive (append mode)."
  (setf *log-file* (open path :direction :output
                               :if-exists :append
                               :if-does-not-exist :create)))

(defun close-usb-log ()
  (when *log-file*
    (close *log-file*)
    (setf *log-file* nil)))

;;; Monotonic clock

(cffi:defcfun ("clock_gettime" %clock-gettime) :int
  (clk-id :int)
  (tp :pointer))

(defconstant +clock-monotonic+ 1)

(defun monotonic-time-ms ()
  "Return monotonic time in milliseconds as a double-float."
  (cffi:with-foreign-object (ts '(:struct timespec))
    (%clock-gettime +clock-monotonic+ ts)
    (let ((sec (cffi:foreign-slot-value ts '(:struct timespec) 'tv-sec))
          (nsec (cffi:foreign-slot-value ts '(:struct timespec) 'tv-nsec)))
      (+ (* sec 1000.0d0) (/ nsec 1000000.0d0)))))

(cffi:defcstruct timespec
  (tv-sec :long)
  (tv-nsec :long))

(cffi:defcfun ("clock_nanosleep" %clock-nanosleep) :int
  (clk-id :int)
  (flags :int)
  (request :pointer)
  (remain :pointer))

(defconstant +timer-abstime+ 1)

(defvar *frame-target-ns* (floor 1000000000 60)
  "Nanoseconds per frame at 60fps.")

(defvar *next-frame-sec* 0)
(defvar *next-frame-nsec* 0)

(defun frame-sync-60fps ()
  "Sleep until the next 60fps frame boundary. Call once per frame."
  (incf *next-frame-nsec* *frame-target-ns*)
  (when (>= *next-frame-nsec* 1000000000)
    (decf *next-frame-nsec* 1000000000)
    (incf *next-frame-sec*))
  ;; Initialize on first call
  (when (zerop *next-frame-sec*)
    (cffi:with-foreign-object (ts '(:struct timespec))
      (%clock-gettime +clock-monotonic+ ts)
      (setf *next-frame-sec*
            (cffi:foreign-slot-value ts '(:struct timespec) 'tv-sec))
      (setf *next-frame-nsec*
            (cffi:foreign-slot-value ts '(:struct timespec) 'tv-nsec))
      (return-from frame-sync-60fps)))
  ;; Sleep until target time
  (cffi:with-foreign-object (ts '(:struct timespec))
    (setf (cffi:foreign-slot-value ts '(:struct timespec) 'tv-sec) *next-frame-sec*)
    (setf (cffi:foreign-slot-value ts '(:struct timespec) 'tv-nsec) *next-frame-nsec*)
    (%clock-nanosleep +clock-monotonic+ +timer-abstime+ ts (cffi:null-pointer))))
