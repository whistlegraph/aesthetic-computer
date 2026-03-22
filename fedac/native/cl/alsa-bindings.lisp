;;; ALSA PCM bindings via CFFI

(in-package :ac-native.alsa)

(cffi:define-foreign-library libasound
  (:unix "libasound.so.2"))

(cffi:use-foreign-library libasound)

;;; PCM
(cffi:defcfun ("snd_pcm_open" pcm-open%) :int
  (pcm :pointer) (name :string) (stream :int) (mode :int))

(defun pcm-open (name &key (stream 0) (mode 0))
  "Open an ALSA PCM device. Returns (values pcm-handle error-code)."
  (cffi:with-foreign-object (handle :pointer)
    (let ((err (pcm-open% handle name stream mode)))
      (values (if (zerop err) (cffi:mem-ref handle :pointer) nil) err))))

(cffi:defcfun ("snd_pcm_close" pcm-close) :int (pcm :pointer))
(cffi:defcfun ("snd_pcm_prepare" pcm-prepare) :int (pcm :pointer))
(cffi:defcfun ("snd_pcm_writei" pcm-writei) :long
  (pcm :pointer) (buffer :pointer) (size :unsigned-long))
(cffi:defcfun ("snd_pcm_recover" pcm-recover) :int
  (pcm :pointer) (err :int) (silent :int))

;;; Simplified setup
(cffi:defcfun ("snd_pcm_set_params" pcm-set-params) :int
  (pcm :pointer) (format :int) (access :int) (channels :unsigned-int)
  (rate :unsigned-int) (soft-resample :int) (latency :unsigned-int))

;;; Constants
(defconstant +snd-pcm-stream-playback+ 0)
(defconstant +snd-pcm-stream-capture+ 1)
(defconstant +snd-pcm-format-s16-le+ 2)
(defconstant +snd-pcm-format-float-le+ 14)
(defconstant +snd-pcm-access-rw-interleaved+ 3)

;;; Error strings
(cffi:defcfun ("snd_strerror" snd-strerror) :string (errnum :int))
