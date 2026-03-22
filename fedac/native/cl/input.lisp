;;; Input — evdev keyboard/mouse/touch

(in-package :ac-native.input)

(defstruct ac-input
  "Input subsystem state."
  (fds (make-array 16 :element-type 'fixnum :initial-element -1))
  (fd-count 0 :type fixnum)
  (display-w 0 :type fixnum)
  (display-h 0 :type fixnum)
  (pixel-scale 1 :type fixnum))

(defstruct event
  "An input event."
  (type :none :type keyword)  ; :key-down :key-up :touch :draw :lift
  (key nil)                    ; key name string
  (code 0 :type fixnum)        ; raw keycode
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defun input-init (display-w display-h pixel-scale)
  "Open all /dev/input/event* devices."
  (let ((input (make-ac-input :display-w display-w :display-h display-h
                               :pixel-scale pixel-scale)))
    ;; Scan /dev/input/ for event devices
    (loop for i from 0 to 15
          for path = (format nil "/dev/input/event~D" i)
          for fd = (ac-native.syscalls:sys-open path
                     (logior ac-native.syscalls:+o-rdonly+
                             ac-native.syscalls:+o-nonblock+)
                     0)
          when (>= fd 0) do
            (setf (aref (ac-input-fds input) (ac-input-fd-count input)) fd)
            (incf (ac-input-fd-count input))
            (ac-native.util:ac-log "[input] opened ~A (fd ~D)~%" path fd))
    (ac-native.util:ac-log "[input] ~D event devices~%" (ac-input-fd-count input))
    input))

(defun input-poll (input)
  "Poll all input devices, return a list of events."
  (let ((events nil))
    (dotimes (i (ac-input-fd-count input))
      (let ((fd (aref (ac-input-fds input) i)))
        (cffi:with-foreign-object (ev '(:struct input-event))
          (loop for n = (ac-native.syscalls:sys-read
                         fd ev (cffi:foreign-type-size '(:struct input-event)))
                while (= n (cffi:foreign-type-size '(:struct input-event))) do
                  (let ((type (cffi:foreign-slot-value ev '(:struct input-event) 'type))
                        (code (cffi:foreign-slot-value ev '(:struct input-event) 'code))
                        (value (cffi:foreign-slot-value ev '(:struct input-event) 'value)))
                    (when (= type +ev-key+)
                      (push (make-event
                             :type (if (plusp value) :key-down :key-up)
                             :code code)
                            events)))))))
    (nreverse events)))

(defun input-destroy (input)
  "Close all input fds."
  (when input
    (dotimes (i (ac-input-fd-count input))
      (let ((fd (aref (ac-input-fds input) i)))
        (when (>= fd 0)
          (ac-native.syscalls:sys-close fd))))))
