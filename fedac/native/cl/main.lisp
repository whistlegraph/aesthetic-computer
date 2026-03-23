;;; Notepat — AC Native OS musical keyboard instrument (Common Lisp)
;;; Port of fedac/native/pieces/notepat.mjs core functionality

(in-package :ac-native)

(defvar *running* t "Main loop flag.")

;;; ── Pixel scale ──

(defun compute-pixel-scale (display-w)
  "Compute pixel scale targeting ~200px wide (bigger pixels)."
  (let ((target (max 1 (min 16 (floor display-w 200)))))
    (loop for delta from 0 to 3 do
      (let ((s (+ target delta)))
        (when (and (>= s 1) (<= s 16) (zerop (mod display-w s)))
          (return-from compute-pixel-scale s)))
      (let ((s (- target delta)))
        (when (and (>= s 1) (zerop (mod display-w s)))
          (return-from compute-pixel-scale s))))
    target))

;;; ── Music theory ──

(defvar *chromatic* #("c" "c#" "d" "d#" "e" "f" "f#" "g" "g#" "a" "a#" "b"))

(defun note-to-freq (note-name octave)
  "Convert note name and octave to frequency in Hz.
   A4 = 440Hz. Uses equal temperament."
  (let ((idx (position note-name *chromatic* :test #'string=)))
    (if idx
        (* 440.0d0 (expt 2.0d0 (+ (- octave 4) (/ (- idx 9) 12.0d0))))
        440.0d0)))

;;; ── Note colors (chromatic rainbow) ──

(defvar *note-colors*
  '(("c"  255  30  30)    ; red
    ("c#" 255  80   0)    ; red-orange
    ("d"  255 150   0)    ; orange
    ("d#" 200 200   0)    ; yellow-green
    ("e"  230 220   0)    ; yellow
    ("f"   30 200  30)    ; green
    ("f#"   0 200 180)    ; teal
    ("g"   30 100 255)    ; blue
    ("g#"  80  50 255)    ; indigo
    ("a"  140  30 220)    ; purple
    ("a#" 200  30 150)    ; magenta
    ("b"  200  50 255)))  ; violet

(defun note-color-rgb (note-name)
  "Return (r g b) list for a note name."
  (let ((entry (assoc note-name *note-colors* :test #'string=)))
    (if entry (cdr entry) '(80 80 80))))

;;; ── Keyboard mapping: evdev keycode → (note-name . octave-offset) ──

(defvar *key-note-map* (make-hash-table :test 'eql))

(defun init-key-note-map ()
  "Populate keycode → note mapping (QWERTY layout matching JS notepat)."
  (clrhash *key-note-map*)
  ;; Lower octave naturals
  (setf (gethash ac-native.input:+key-c+ *key-note-map*) '("c" . 0))
  (setf (gethash ac-native.input:+key-d+ *key-note-map*) '("d" . 0))
  (setf (gethash ac-native.input:+key-e+ *key-note-map*) '("e" . 0))
  (setf (gethash ac-native.input:+key-f+ *key-note-map*) '("f" . 0))
  (setf (gethash ac-native.input:+key-g+ *key-note-map*) '("g" . 0))
  (setf (gethash ac-native.input:+key-a+ *key-note-map*) '("a" . 0))
  (setf (gethash ac-native.input:+key-b+ *key-note-map*) '("b" . 0))
  ;; Lower octave sharps
  (setf (gethash ac-native.input:+key-v+ *key-note-map*) '("c#" . 0))
  (setf (gethash ac-native.input:+key-s+ *key-note-map*) '("d#" . 0))
  (setf (gethash ac-native.input:+key-w+ *key-note-map*) '("f#" . 0))
  (setf (gethash ac-native.input:+key-r+ *key-note-map*) '("g#" . 0))
  (setf (gethash ac-native.input:+key-q+ *key-note-map*) '("a#" . 0))
  ;; Upper octave naturals
  (setf (gethash ac-native.input:+key-h+ *key-note-map*) '("c" . 1))
  (setf (gethash ac-native.input:+key-i+ *key-note-map*) '("d" . 1))
  (setf (gethash ac-native.input:+key-j+ *key-note-map*) '("e" . 1))
  (setf (gethash ac-native.input:+key-k+ *key-note-map*) '("f" . 1))
  (setf (gethash ac-native.input:+key-l+ *key-note-map*) '("g" . 1))
  (setf (gethash ac-native.input:+key-m+ *key-note-map*) '("a" . 1))
  (setf (gethash ac-native.input:+key-n+ *key-note-map*) '("b" . 1))
  ;; Upper octave sharps
  (setf (gethash ac-native.input:+key-t+ *key-note-map*) '("c#" . 1))
  (setf (gethash ac-native.input:+key-y+ *key-note-map*) '("d#" . 1))
  (setf (gethash ac-native.input:+key-u+ *key-note-map*) '("f#" . 1))
  (setf (gethash ac-native.input:+key-o+ *key-note-map*) '("g#" . 1))
  (setf (gethash ac-native.input:+key-p+ *key-note-map*) '("a#" . 1))
  ;; Extension: +2 octave
  (setf (gethash ac-native.input:+key-semicolon+ *key-note-map*) '("c" . 2))
  (setf (gethash ac-native.input:+key-apostrophe+ *key-note-map*) '("c#" . 2))
  (setf (gethash ac-native.input:+key-rightbrace+ *key-note-map*) '("d" . 2))
  ;; Sub-octave
  (setf (gethash ac-native.input:+key-z+ *key-note-map*) '("a#" . -1))
  (setf (gethash ac-native.input:+key-x+ *key-note-map*) '("b" . -1)))

;;; ── Wave types ──

(defvar *wave-names* #("sine" "triangle" "sawtooth" "square" "noise"))
(defvar *wave-index* 0)
(defvar *octave* 4)

;;; ── Active voices and trails ──

(defvar *active-voices* (make-hash-table :test 'eql)
  "Keycode → voice-id for currently held keys.")

(defvar *active-notes* (make-hash-table :test 'eql)
  "Keycode → (note-name . actual-octave) for currently held keys.")

(defvar *trails* (make-hash-table :test 'equal)
  "note-name → brightness (1.0 → 0.0) for recently released notes.")

;;; ── Background color state ──

(defvar *bg-r* 20)
(defvar *bg-g* 20)
(defvar *bg-b* 25)

;;; ── FPS tracking ──

(defvar *fps-display* 0)
(defvar *fps-accum* 0.0d0)
(defvar *fps-samples* 0)
(defvar *fps-last-time* 0.0d0)

;;; ── ESC triple-press ──

(defvar *esc-count* 0)
(defvar *esc-last-frame* 0)

;;; ── Main ──

(defun main ()
  "AC Native OS entry point — boots directly into notepat."
  (format *error-output* "~%════════════════════════════════════~%")
  (format *error-output* "  notepat (Common Lisp)~%")
  (format *error-output* "  SBCL ~A~%" (lisp-implementation-version))
  (format *error-output* "════════════════════════════════════~%~%")
  (force-output *error-output*)

  ;; Init key map
  (init-key-note-map)

  ;; Init display
  (let ((display (handler-case (ac-native.drm:drm-init)
                   (error (e)
                     (format *error-output* "[notepat] DRM error: ~A~%" e)
                     (force-output *error-output*)
                     nil))))
    (unless display
      (format *error-output* "[notepat] FATAL: no display~%")
      (force-output *error-output*)
      (sleep 30)
      (return-from main 1))

    (let* ((dw (ac-native.drm:display-width display))
           (dh (ac-native.drm:display-height display))
           (scale (compute-pixel-scale dw))
           (sw (floor dw scale))
           (sh (floor dh scale))
           (screen (fb-create sw sh))
           (graph (make-graph :fb screen :screen screen))
           (input (ac-native.input:input-init dw dh scale))
           (audio (ac-native.audio:audio-init))
           (frame 0))

      (format *error-output* "[notepat] ~Dx~D scale:~D → ~Dx~D~%"
              dw dh scale sw sh)
      (format *error-output* "[notepat] audio: ~A~%"
              (if audio "OK" "FAILED"))
      (force-output *error-output*)

      ;; Font init
      (font-init)

      ;; Main loop
      (setf *running* t)
      (unwind-protect
          (loop while *running* do
            (incf frame)

            ;; FPS tracking
            (let ((now (monotonic-time-ms)))
              (when (> *fps-last-time* 0.0d0)
                (incf *fps-accum* (- now *fps-last-time*))
                (incf *fps-samples*)
                (when (>= *fps-samples* 30)
                  (setf *fps-display* (round (/ 30000.0d0 *fps-accum*)))
                  (setf *fps-accum* 0.0d0 *fps-samples* 0)))
              (setf *fps-last-time* now))

            ;; ── Input ──
            (dolist (ev (ac-native.input:input-poll input))
              (let ((type (ac-native.input:event-type ev))
                    (code (ac-native.input:event-code ev)))

                ;; ── Key down ──
                (when (eq type :key-down)
                  ;; ESC: triple-press to quit
                  (when (= code ac-native.input:+key-esc+)
                    (if (> (- frame *esc-last-frame*) 90)
                        (setf *esc-count* 0))
                    (incf *esc-count*)
                    (setf *esc-last-frame* frame)
                    (when (and audio (< *esc-count* 3))
                      (audio-synth audio :type 3 ; square
                                   :tone (if (= *esc-count* 1) 440.0d0 660.0d0)
                                   :duration 0.08d0 :volume 0.15d0
                                   :attack 0.002d0 :decay 0.06d0))
                    (when (>= *esc-count* 3)
                      (setf *running* nil)))

                  ;; Power button
                  (when (= code ac-native.input:+key-power+)
                    (setf *running* nil))

                  ;; Number keys: set octave
                  (when (and (>= code ac-native.input:+key-1+)
                             (<= code ac-native.input:+key-9+))
                    (setf *octave* (1+ (- code ac-native.input:+key-1+))))

                  ;; Arrow up/down: octave
                  (when (= code ac-native.input:+key-up+)
                    (setf *octave* (min 9 (1+ *octave*))))
                  (when (= code ac-native.input:+key-down+)
                    (setf *octave* (max 1 (1- *octave*))))

                  ;; Tab: cycle wave type
                  (when (= code ac-native.input:+key-tab+)
                    (setf *wave-index* (mod (1+ *wave-index*) 5))
                    ;; Confirmation blip
                    (when audio
                      (let ((tones #(660.0d0 550.0d0 440.0d0 330.0d0 220.0d0)))
                        (audio-synth audio :type *wave-index*
                                     :tone (aref tones *wave-index*)
                                     :duration 0.07d0 :volume 0.18d0
                                     :attack 0.002d0 :decay 0.06d0))))

                  ;; Note keys
                  (let ((mapping (gethash code *key-note-map*)))
                    (when (and mapping
                               (not (gethash code *active-voices*))
                               audio)
                      (let* ((note-name (car mapping))
                             (oct-delta (cdr mapping))
                             (actual-octave (+ *octave* oct-delta))
                             (freq (note-to-freq note-name actual-octave))
                             ;; Pan: lower notes left, higher notes right
                             (idx (position note-name *chromatic* :test #'string=))
                             (semitones (+ (* (- actual-octave 4) 12) (or idx 0)))
                             (pan (max -0.8d0 (min 0.8d0 (/ (- semitones 12) 15.0d0))))
                             (voice-id (audio-synth audio
                                                    :type *wave-index*
                                                    :tone freq
                                                    :volume 0.7d0
                                                    :duration 0  ; sustain
                                                    :attack 0.005d0
                                                    :decay 0.1d0
                                                    :pan pan)))
                        (setf (gethash code *active-voices*) voice-id)
                        (setf (gethash code *active-notes*)
                              (cons note-name actual-octave))))))

                ;; ── Key up ──
                (when (eq type :key-up)
                  (let ((voice-id (gethash code *active-voices*)))
                    (when (and voice-id audio)
                      (audio-synth-kill audio voice-id)
                      (remhash code *active-voices*)
                      ;; Start trail
                      (let ((note-info (gethash code *active-notes*)))
                        (when note-info
                          (setf (gethash note-info *trails*) 1.0)
                          (remhash code *active-notes*))))))))

            ;; ── Trail decay ──
            (let ((dead nil))
              (maphash (lambda (note val)
                         (let ((new-val (- val 0.025)))
                           (if (<= new-val 0.0)
                               (push note dead)
                               (setf (gethash note *trails*) new-val))))
                       *trails*)
              (dolist (n dead) (remhash n *trails*)))

            ;; ── Compute background color from active notes ──
            (let ((n (hash-table-count *active-notes*)))
              (if (> n 0)
                  (let ((tr 0) (tg 0) (tb 0))
                    (maphash (lambda (code note-info)
                               (declare (ignore code))
                               (let ((rgb (note-color-rgb (car note-info))))
                                 (incf tr (first rgb))
                                 (incf tg (second rgb))
                                 (incf tb (third rgb))))
                             *active-notes*)
                    ;; Lerp toward target (darkened)
                    (let ((target-r (floor (* (floor tr n) 35) 100))
                          (target-g (floor (* (floor tg n) 35) 100))
                          (target-b (floor (* (floor tb n) 35) 100)))
                      (setf *bg-r* (+ *bg-r* (floor (- target-r *bg-r*) 4)))
                      (setf *bg-g* (+ *bg-g* (floor (- target-g *bg-g*) 4)))
                      (setf *bg-b* (+ *bg-b* (floor (- target-b *bg-b*) 4)))))
                  ;; Decay to dark
                  (progn
                    (setf *bg-r* (+ *bg-r* (floor (- 20 *bg-r*) 8)))
                    (setf *bg-g* (+ *bg-g* (floor (- 20 *bg-g*) 8)))
                    (setf *bg-b* (+ *bg-b* (floor (- 25 *bg-b*) 8))))))

            ;; ── Paint ──
            (graph-wipe graph (make-color :r *bg-r* :g *bg-g* :b *bg-b*))

            ;; Draw trails — horizontal bars per note+octave
            (maphash (lambda (trail-key val)
                       ;; trail-key is (note-name . octave)
                       (let* ((note-name (car trail-key))
                              (oct (cdr trail-key))
                              (rgb (note-color-rgb note-name))
                              (note-idx (or (position note-name *chromatic* :test #'string=) 0))
                              ;; Unique Y per note+octave: semitone index relative to octave 1
                              (semi (+ (* (- oct 1) 12) note-idx))
                              (total-semitones (* 9 12))  ; octaves 1-9
                              (bar-h (max 2 (floor (- sh 30) total-semitones)))
                              (bar-y (+ 14 (floor (* semi (- sh 30)) total-semitones)))
                              (bar-w (max 1 (floor (* val sw))))
                              (bar-x (floor (- sw bar-w) 2))
                              (alpha (max 1 (min 255 (floor (* val 200))))))
                         (graph-ink graph (make-color :r (first rgb)
                                                     :g (second rgb)
                                                     :b (third rgb)
                                                     :a alpha))
                         (graph-box graph bar-x bar-y bar-w bar-h)))
                     *trails*)

            ;; Draw active note indicators — bright bars
            (maphash (lambda (code note-info)
                       (declare (ignore code))
                       (let* ((note-name (car note-info))
                              (oct (cdr note-info))
                              (rgb (note-color-rgb note-name))
                              (note-idx (or (position note-name *chromatic* :test #'string=) 0))
                              (semi (+ (* (- oct 1) 12) note-idx))
                              (total-semitones (* 9 12))
                              (bar-h (max 2 (floor (- sh 30) total-semitones)))
                              (bar-y (+ 14 (floor (* semi (- sh 30)) total-semitones))))
                         (graph-ink graph (make-color :r (min 255 (+ (first rgb) 40))
                                                     :g (min 255 (+ (second rgb) 40))
                                                     :b (min 255 (+ (third rgb) 40))
                                                     :a 220))
                         (graph-box graph 0 bar-y sw bar-h)))
                     *active-notes*)

            ;; Status bar (bottom)
            (let ((status (format nil "~A  OCT:~D  ~Dfps"
                                  (aref *wave-names* *wave-index*)
                                  *octave*
                                  *fps-display*)))
              (graph-ink graph (make-color :r 180 :g 180 :b 180 :a 200))
              (font-draw graph status 3 (- sh 12)))

            ;; "notepat" title (top-left, dim)
            (graph-ink graph (make-color :r 100 :g 100 :b 110 :a 150))
            (font-draw graph "notepat" 3 3)

            ;; Active voice count (top-right)
            (let ((vc (hash-table-count *active-voices*)))
              (when (> vc 0)
                (let ((txt (format nil "~D" vc)))
                  (graph-ink graph (make-color :r 200 :g 200 :b 200 :a 180))
                  (font-draw graph txt (- sw (* (length txt) 6) 3) 3))))

            ;; ── Present ──
            (ac-native.drm:drm-present display screen scale)
            (frame-sync-60fps))

        ;; ── Cleanup ──
        ;; Kill all active voices
        (when audio
          (maphash (lambda (code voice-id)
                     (declare (ignore code))
                     (audio-synth-kill audio voice-id))
                   *active-voices*)
          (audio-destroy audio))
        (ac-native.input:input-destroy input)
        (fb-destroy screen)
        (ac-native.drm:drm-destroy display)
        (format *error-output* "[notepat] shutdown~%")
        (force-output *error-output*)))))
