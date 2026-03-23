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

(defun unmute-all-mixers (card-name)
  "Open the ALSA mixer for CARD-NAME, unmute all playback switches, set volumes to max."
  (ac-native.util:ac-log "[audio] unmuting mixer: ~A~%" card-name)
  (cffi:with-foreign-object (mixer-ptr :pointer)
    (when (zerop (cffi:foreign-funcall "snd_mixer_open"
                   :pointer mixer-ptr :int 0 :int))
      (let ((mixer (cffi:mem-ref mixer-ptr :pointer)))
        (cffi:foreign-funcall "snd_mixer_attach"
          :pointer mixer :string card-name :int)
        (cffi:foreign-funcall "snd_mixer_selem_register"
          :pointer mixer :pointer (cffi:null-pointer) :pointer (cffi:null-pointer) :int)
        (cffi:foreign-funcall "snd_mixer_load" :pointer mixer :int)
        ;; Iterate all elements
        (loop for elem = (cffi:foreign-funcall "snd_mixer_first_elem"
                           :pointer mixer :pointer)
                then (cffi:foreign-funcall "snd_mixer_elem_next"
                       :pointer elem :pointer)
              until (cffi:null-pointer-p elem) do
          (let ((name (cffi:foreign-funcall "snd_mixer_selem_get_name"
                        :pointer elem :string)))
            (when (not (zerop (cffi:foreign-funcall "snd_mixer_selem_is_active"
                                :pointer elem :int)))
              ;; Unmute playback switch
              (when (not (zerop (cffi:foreign-funcall "snd_mixer_selem_has_playback_switch"
                                  :pointer elem :int)))
                (cffi:foreign-funcall "snd_mixer_selem_set_playback_switch_all"
                  :pointer elem :int 1 :int)
                (ac-native.util:ac-log "[audio] unmuted: ~A~%" name))
              ;; Set volume to max
              (when (not (zerop (cffi:foreign-funcall "snd_mixer_selem_has_playback_volume"
                                  :pointer elem :int)))
                (cffi:with-foreign-objects ((minv :long) (maxv :long))
                  (cffi:foreign-funcall "snd_mixer_selem_get_playback_volume_range"
                    :pointer elem :pointer minv :pointer maxv :int)
                  (let ((mx (cffi:mem-ref maxv :long)))
                    (cffi:foreign-funcall "snd_mixer_selem_set_playback_volume_all"
                      :pointer elem :long mx :int)
                    (ac-native.util:ac-log "[audio] volume ~A: ~D~%" name mx)))))))
        (cffi:foreign-funcall "snd_mixer_close" :pointer mixer :int)))))

(defun audio-init ()
  "Initialize ALSA audio output and start the mixing thread."
  ;; Try multiple devices with retries (HDA codec may not be probed yet)
  (let ((devices '("hw:0,0" "hw:1,0" "hw:0,1" "hw:1,1"
                   "plughw:0,0" "plughw:1,0" "default"))
        (pcm nil)
        (card-idx 0))
    (loop for attempt from 0 below 5
          until pcm do
      (when (> attempt 0)
        (ac-native.util:ac-log "[audio] retry ~D/4 — waiting 2s...~%" attempt)
        (sleep 2))
      (dolist (dev devices)
        (multiple-value-bind (handle err) (ac-native.alsa:pcm-open dev)
          (when handle
            (ac-native.util:ac-log "[audio] opened: ~A (attempt ~D)~%" dev attempt)
            (setf pcm handle)
            ;; Extract card index from device name
            (let ((pos (position #\: dev)))
              (when pos
                (let ((ch (char dev (1+ pos))))
                  (when (digit-char-p ch)
                    (setf card-idx (- (char-code ch) (char-code #\0)))))))
            (return))
          (when (= attempt 0)
            (ac-native.util:ac-log "[audio] ~A: ~A~%" dev
                                    (ac-native.alsa:snd-strerror err))))))

    (unless pcm
      (ac-native.util:ac-log "[audio] no ALSA device found~%")
      (return-from audio-init nil))

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

    ;; Unmute all mixer controls and set volumes to max
    (handler-case
        (unmute-all-mixers (format nil "hw:~D" card-idx))
      (error (e)
        (ac-native.util:ac-log "[audio] mixer error: ~A~%" e)))

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
