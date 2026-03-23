;;; AC Native OS — Common Lisp Edition
;;; ASDF system definition

(defsystem "ac-native"
  :description "Aesthetic Computer Native OS runtime"
  :version "0.1.0"
  :author "Aesthetic Computer"
  :license "MIT"
  :depends-on ("cffi" "bordeaux-threads" "alexandria" "swank")
  :serial t
  :components
  ((:file "packages")
   ;; Core infrastructure
   (:file "util")
   (:file "syscalls")
   ;; Display
   (:file "color")
   (:file "framebuffer")
   (:file "drm-constants")
   (:file "drm-display")
   ;; Graphics
   (:file "graph")
   (:file "font-data")
   (:file "font")
   ;; Input
   (:file "input-keycodes")
   (:file "input")
   ;; Audio
   (:file "alsa-bindings")
   (:file "audio-synth")
   (:file "audio")
   ;; Main
   (:file "config")
   (:file "main")
   ;; Build
   (:file "build")))
