;;; Font bitmap data — placeholder, will be generated from C headers

(in-package :ac-native.font)

;; 8x8 font — 95 printable ASCII characters (space through ~)
;; Each character is 8 bytes (8 rows of 8 pixels)
;; TODO: port from fedac/native/src/font-8x8.h
(defvar *font-8x8* (make-array (* 95 8) :element-type '(unsigned-byte 8) :initial-element 0))
