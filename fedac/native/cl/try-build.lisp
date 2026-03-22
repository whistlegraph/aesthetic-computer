;;; Quick test: load the system and report errors
(load "/opt/quicklisp/setup.lisp")
(require :asdf)
(push #P"/repo/fedac/native/cl/" asdf:*central-registry*)
(handler-case
    (progn
      (asdf:load-system :ac-native)
      (format t "~&=== SYSTEM LOADED OK ===~%"))
  (error (e)
    (format t "~&=== LOAD ERROR ===~%~A~%" e)
    (sb-ext:exit :code 1)))
