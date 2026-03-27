;;; notepat.lisp — Common Lisp notepat piece
;;; This is a marker file that tells the C runtime to hand off to /ac-swank
;;; which runs the native CL notepat (defined in cl/main.lisp).
;;;
;;; To run: type "notepat.lisp" at the prompt, or select it from the list.
;;; The C binary will exec /ac-swank --piece notepat, which launches
;;; the full CL notepat with DRM graphics, audio, and Swank REPL.
