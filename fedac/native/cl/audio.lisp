;;; Audio subsystem — ALSA output + voice mixing

(in-package :ac-native.audio)

(defconstant +max-voices+ 32)
(defconstant +sample-rate+ 48000)

(defstruct ac-audio
  "Audio subsystem state."
  (pcm nil)  ; ALSA PCM handle
  (voices (make-array +max-voices+ :initial-element nil))
  (thread nil)
  (running nil :type boolean)
  (next-id 1 :type fixnum)
  (lock (bordeaux-threads:make-lock "audio")))

(defun audio-init ()
  "Initialize ALSA audio output and start the mixing thread."
  (multiple-value-bind (pcm err) (ac-native.alsa:pcm-open "default")
    (unless pcm
      (ac-native.util:ac-log "[audio] failed to open: ~A~%"
                              (ac-native.alsa:snd-strerror err))
      ;; Try hw:0,0
      (multiple-value-setq (pcm err) (ac-native.alsa:pcm-open "hw:0,0"))
      (unless pcm
        (ac-native.util:ac-log "[audio] hw:0,0 also failed~%")
        (return-from audio-init nil)))

    ;; Set params: S16_LE, stereo, 48kHz, 50ms latency
    (let ((ret (ac-native.alsa:pcm-set-params
                pcm
                ac-native.alsa:+snd-pcm-format-s16-le+
                ac-native.alsa:+snd-pcm-access-rw-interleaved+
                2 +sample-rate+ 1 50000)))
      (unless (zerop ret)
        (ac-native.util:ac-log "[audio] set-params failed: ~A~%"
                                (ac-native.alsa:snd-strerror ret))
        (ac-native.alsa:pcm-close pcm)
        (return-from audio-init nil)))

    (let ((audio (make-ac-audio :pcm pcm :running t)))
      ;; Init voice slots
      (dotimes (i +max-voices+)
        (setf (aref (ac-audio-voices audio) i) (make-voice)))

      ;; Start audio thread
      (setf (ac-audio-thread audio)
            (bordeaux-threads:make-thread
             (lambda () (audio-thread-fn audio))
             :name "audio"))

      (ac-native.util:ac-log "[audio] OK (48kHz stereo)~%")
      audio)))

(defun audio-thread-fn (audio)
  "Audio mixing thread — fills PCM buffer."
  (let* ((period-frames 256)
         (buf-samples (* period-frames 2))  ; stereo
         (buf (cffi:foreign-alloc :int16 :count buf-samples)))
    (unwind-protect
        (loop while (ac-audio-running audio) do
          ;; Mix all active voices
          (dotimes (f period-frames)
            (let ((mix-l 0.0d0) (mix-r 0.0d0))
              (bordeaux-threads:with-lock-held ((ac-audio-lock audio))
                (dotimes (i +max-voices+)
                  (let ((v (aref (ac-audio-voices audio) i)))
                    (when (voice-active v)
                      (let* ((s (generate-sample v))
                             (vol (* (voice-volume v) (voice-fade v)))
                             (l-gain (if (<= (voice-pan v) 0) 1.0d0
                                         (- 1.0d0 (* (voice-pan v) 0.6d0))))
                             (r-gain (if (>= (voice-pan v) 0) 1.0d0
                                         (+ 1.0d0 (* (voice-pan v) 0.6d0)))))
                        (incf mix-l (* s vol l-gain))
                        (incf mix-r (* s vol r-gain))
                        ;; Advance phase
                        (incf (voice-phase v) (voice-phase-inc v))
                        (incf (voice-elapsed v) (/ 1.0d0 +sample-rate+))
                        ;; Check duration
                        (when (and (> (voice-duration v) 0)
                                   (> (voice-elapsed v) (voice-duration v)))
                          (setf (voice-active v) nil)))))))
              ;; Clamp and write S16LE stereo
              (let ((sl (max -32767 (min 32767 (round (* mix-l 32767)))))
                    (sr (max -32767 (min 32767 (round (* mix-r 32767))))))
                (setf (cffi:mem-aref buf :int16 (* f 2)) sl)
                (setf (cffi:mem-aref buf :int16 (1+ (* f 2))) sr))))
          ;; Write to ALSA
          (let ((ret (ac-native.alsa:pcm-writei (ac-audio-pcm audio) buf period-frames)))
            (when (< ret 0)
              (ac-native.alsa:pcm-recover (ac-audio-pcm audio) (truncate ret) 0))))
      ;; Cleanup
      (cffi:foreign-free buf))))

(defun audio-synth (audio &key (type 0) (tone 440.0) (volume 0.7) (duration 0)
                            (pan 0.0) (attack 0.01) (decay 0.1))
  "Start a synth voice. Returns voice ID."
  (bordeaux-threads:with-lock-held ((ac-audio-lock audio))
    ;; Find free slot
    (let ((slot (dotimes (i +max-voices+ 0)
                 (unless (voice-active (aref (ac-audio-voices audio) i))
                   (return i))))
          (id (incf (ac-audio-next-id audio))))
      (let ((v (aref (ac-audio-voices audio) slot)))
        (setf (voice-active v) t
              (voice-osc-type v) type
              (voice-phase v) 0.0d0
              (voice-phase-inc v) (/ (* +twopi+ (coerce tone 'double-float)) +sample-rate+)
              (voice-volume v) (coerce volume 'double-float)
              (voice-pan v) (coerce pan 'double-float)
              (voice-attack v) (coerce attack 'double-float)
              (voice-decay v) (coerce decay 'double-float)
              (voice-duration v) (coerce duration 'double-float)
              (voice-elapsed v) 0.0d0
              (voice-fade v) 1.0d0
              (voice-id v) id))
      id)))

(defun audio-synth-kill (audio id)
  "Kill a synth voice by ID."
  (bordeaux-threads:with-lock-held ((ac-audio-lock audio))
    (dotimes (i +max-voices+)
      (let ((v (aref (ac-audio-voices audio) i)))
        (when (and (voice-active v) (= (voice-id v) id))
          (setf (voice-active v) nil)
          (return))))))

(defun audio-destroy (audio)
  "Shutdown audio."
  (when audio
    (setf (ac-audio-running audio) nil)
    (when (ac-audio-thread audio)
      (bordeaux-threads:join-thread (ac-audio-thread audio)))
    (when (ac-audio-pcm audio)
      (ac-native.alsa:pcm-close (ac-audio-pcm audio)))
    (ac-native.util:ac-log "[audio] destroyed~%")))
