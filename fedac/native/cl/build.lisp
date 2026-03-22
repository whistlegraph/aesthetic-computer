;;; Build script — produce standalone ac-native binary

(in-package :ac-native.build)

(defun build (&optional (output "ac-native"))
  "Save a standalone executable."
  (format t "Building AC Native OS (Common Lisp)...~%")
  (format t "  Output: ~A~%" output)
  (sb-ext:save-lisp-and-die output
    :toplevel #'ac-native:main
    :executable t
    :compression t
    :purify t))
