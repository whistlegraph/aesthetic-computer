;;; Color — ARGB32 pixel operations

(in-package :ac-native.color)

(defstruct (color (:constructor make-color (&key (r 0) (g 0) (b 0) (a 255))))
  (r 0 :type (unsigned-byte 8))
  (g 0 :type (unsigned-byte 8))
  (b 0 :type (unsigned-byte 8))
  (a 255 :type (unsigned-byte 8)))

(declaim (inline color-pack-argb32))
(defun color-pack-argb32 (c)
  "Pack a color struct into a 32-bit ARGB value."
  (declare (optimize (speed 3) (safety 0)))
  (logior (ash (color-a c) 24)
          (ash (color-r c) 16)
          (ash (color-g c) 8)
          (color-b c)))

(declaim (inline blend-channel))
(defun blend-channel (src dst alpha)
  "Blend a single channel: src over dst with alpha [0..255]."
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 8) src dst alpha))
  (the (unsigned-byte 8)
       (ash (+ (* src alpha) (* dst (- 255 alpha)) 128) -8)))

(defun color-blend (src dst)
  "Alpha-blend src over dst, return packed ARGB32."
  (declare (optimize (speed 3) (safety 0)))
  (let ((sa (color-a src)))
    (if (= sa 255)
        (color-pack-argb32 src)
        (if (zerop sa)
            (color-pack-argb32 dst)
            (logior
             #xFF000000
             (ash (blend-channel (color-r src) (color-r dst) sa) 16)
             (ash (blend-channel (color-g src) (color-g dst) sa) 8)
             (blend-channel (color-b src) (color-b dst) sa))))))
