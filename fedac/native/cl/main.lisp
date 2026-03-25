;;; Notepat — AC Native OS musical keyboard instrument (Common Lisp)
;;; Port of fedac/native/pieces/notepat.mjs

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
  "Convert note name and octave to frequency in Hz. A4 = 440Hz."
  (let ((idx (position note-name *chromatic* :test #'string=)))
    (if idx
        (* 440.0d0 (expt 2.0d0 (+ (- octave 4) (/ (- idx 9) 12.0d0))))
        440.0d0)))

(defun note-is-sharp-p (note-name)
  (search "#" note-name))

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
  (flet ((m (key note off) (setf (gethash key *key-note-map*) (cons note off))))
    ;; Lower octave naturals
    (m ac-native.input:+key-c+ "c" 0) (m ac-native.input:+key-d+ "d" 0)
    (m ac-native.input:+key-e+ "e" 0) (m ac-native.input:+key-f+ "f" 0)
    (m ac-native.input:+key-g+ "g" 0) (m ac-native.input:+key-a+ "a" 0)
    (m ac-native.input:+key-b+ "b" 0)
    ;; Lower octave sharps
    (m ac-native.input:+key-v+ "c#" 0) (m ac-native.input:+key-s+ "d#" 0)
    (m ac-native.input:+key-w+ "f#" 0) (m ac-native.input:+key-r+ "g#" 0)
    (m ac-native.input:+key-q+ "a#" 0)
    ;; Upper octave naturals
    (m ac-native.input:+key-h+ "c" 1) (m ac-native.input:+key-i+ "d" 1)
    (m ac-native.input:+key-j+ "e" 1) (m ac-native.input:+key-k+ "f" 1)
    (m ac-native.input:+key-l+ "g" 1) (m ac-native.input:+key-m+ "a" 1)
    (m ac-native.input:+key-n+ "b" 1)
    ;; Upper octave sharps
    (m ac-native.input:+key-t+ "c#" 1) (m ac-native.input:+key-y+ "d#" 1)
    (m ac-native.input:+key-u+ "f#" 1) (m ac-native.input:+key-o+ "g#" 1)
    (m ac-native.input:+key-p+ "a#" 1)
    ;; Extension +2
    (m ac-native.input:+key-semicolon+ "c" 2)
    (m ac-native.input:+key-apostrophe+ "c#" 2)
    (m ac-native.input:+key-rightbrace+ "d" 2)
    ;; Sub-octave
    (m ac-native.input:+key-z+ "a#" -1) (m ac-native.input:+key-x+ "b" -1)))

;;; ── State ──

(defvar *wave-names* #("sine" "triangle" "sawtooth" "square" "noise"))
(defvar *wave-index* 0)
(defvar *octave* 4)
(defvar *quick-mode* nil "Short attack/release for staccato play.")

;; Active voices and trails
(defvar *active-voices* (make-hash-table :test 'eql))
(defvar *active-notes* (make-hash-table :test 'eql))
(defvar *trails* (make-hash-table :test 'equal))

;; Background color
(defvar *bg-r* 20) (defvar *bg-g* 20) (defvar *bg-b* 25)

;; FPS
(defvar *fps-display* 0)
(defvar *fps-accum* 0.0d0)
(defvar *fps-samples* 0)
(defvar *fps-last-time* 0.0d0)

;; ESC triple-press
(defvar *esc-count* 0)
(defvar *esc-last-frame* 0)

;; Metronome
(defvar *metronome-on* nil)
(defvar *metronome-bpm* 120)
(defvar *metronome-last-beat* -1)
(defvar *metronome-flash* 0.0 "Visual flash intensity 0-1, decays per frame.")
(defvar *metronome-phase* 0.0 "Pendulum swing -1..1.")

;; Identity
(defvar *boot-handle* nil "Handle from config, set during boot splash.")

;; Native notepat runtime state (defvar to survive unwind-protect after save-lisp-and-die)
(defvar *np-screen* nil)
(defvar *np-graph* nil)
(defvar *np-input* nil)
(defvar *np-audio* nil)
(defvar *np-frame* 0)

;; Network
(defvar *ip-address* "")

(defun refresh-ip ()
  (handler-case
      (let ((output (with-output-to-string (s)
                      (sb-ext:run-program "/sbin/ip" '("-4" "-o" "addr" "show")
                        :output s :error nil))))
        (dolist (line (uiop:split-string output :separator '(#\Newline)))
          (when (and (search "inet " line) (not (search "127.0.0.1" line)))
            (let* ((inet-pos (search "inet " line))
                   (ip-start (+ inet-pos 5))
                   (slash-pos (position #\/ line :start ip-start)))
              (when slash-pos
                (setf *ip-address* (subseq line ip-start slash-pos))
                (return))))))
    (error () nil)))

;;; ── Helpers ──

(defun kill-all-voices (audio)
  "Kill all active voices (on octave/wave change)."
  (when audio
    (maphash (lambda (code voice-id)
               (declare (ignore code))
               (audio-synth-kill audio voice-id))
             *active-voices*))
  (clrhash *active-voices*)
  (clrhash *active-notes*))

;;; ── Build metadata ──

(defvar *build-name* "dev" "Build name from /etc/ac-build.")
(defvar *build-variant* "c" "Build variant: c or cl.")

(defun load-build-metadata ()
  "Read /etc/ac-build: line 1=name, 2=hash, 3=timestamp, 4=variant."
  (handler-case
      (when (probe-file "/etc/ac-build")
        (with-open-file (s "/etc/ac-build" :direction :input)
          (let ((name (read-line s nil nil))
                (hash (read-line s nil nil))
                (ts (read-line s nil nil))
                (variant (read-line s nil nil)))
            (declare (ignore hash ts))
            (when name (setf *build-name* name))
            (when variant (setf *build-variant* variant)))))
    (error () nil)))

;;; ── Boot splash ──

(defun time-greeting ()
  "Return time-of-day greeting string."
  (let ((hour (nth-value 2 (get-decoded-time))))
    (cond ((and (>= hour 5) (< hour 12)) "good morning")
          ((and (>= hour 12) (< hour 17)) "good afternoon")
          (t "good evening"))))

;;; ── JS Piece Runner ──

(defun main-js (piece-path)
  "Run a .mjs piece via the QuickJS bridge."
  (format *error-output* "~%════════════════════════════════════~%")
  (format *error-output* "  AC Native OS (Common Lisp + QuickJS)~%")
  (format *error-output* "  SBCL ~A~%" (lisp-implementation-version))
  (format *error-output* "  piece: ~A~%" piece-path)
  (format *error-output* "════════════════════════════════════~%~%")
  (force-output *error-output*)

  (let ((display (handler-case (ac-native.drm:drm-init)
                   (error (e)
                     (format *error-output* "[js] DRM error: ~A~%" e)
                     (force-output *error-output*) nil))))
    (unless display
      (format *error-output* "[js] FATAL: no display~%")
      (force-output *error-output*) (sleep 30) (return-from main-js 1))

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

      (font-init)

      ;; Initialize QuickJS bridge
      (handler-case
          (js-init graph screen audio sw sh)
        (error (e)
          (format *error-output* "[js] JS-INIT ERROR: ~A~%" e)
          (force-output *error-output*)
          (when audio (audio-destroy audio))
          (ac-native.input:input-destroy input)
          (fb-destroy screen)
          (ac-native.drm:drm-destroy display)
          (setf *js-fallback* t)
        (return-from main-js (main))))

      ;; Load the piece
      (unless (js-load-piece piece-path)
        (format *error-output* "[js] Failed to load piece, falling back to native notepat~%")
        (force-output *error-output*)
        (js-destroy)
        ;; Cleanup and fall through to native
        (when audio (audio-destroy audio))
        (ac-native.input:input-destroy input)
        (fb-destroy screen)
        (ac-native.drm:drm-destroy display)
        (setf *js-fallback* t)
        (return-from main-js (main)))

      ;; Call boot
      (js-boot)

      ;; Start Swank
      (handler-case
          (progn
            (setf swank::*communication-style* :spawn)
            (swank:create-server :port 4005 :dont-close t)
            (format *error-output* "[js] Swank on :4005~%")
            (force-output *error-output*))
        (error (e)
          (format *error-output* "[js] Swank failed: ~A~%" e)
          (force-output *error-output*)))

      ;; Main loop
      (setf *running* t)
      (unwind-protect
          (loop while *running* do
            (incf frame)

            ;; Input → act
            (dolist (ev (ac-native.input:input-poll input))
              (let ((type (ac-native.input:event-type ev))
                    (code (ac-native.input:event-code ev))
                    (key (ac-native.input:event-key ev)))
                ;; ESC triple-press to quit
                (when (and (eq type :key-down) (= code ac-native.input:+key-esc+))
                  (when (> (- frame *esc-last-frame*) 90) (setf *esc-count* 0))
                  (incf *esc-count*)
                  (setf *esc-last-frame* frame)
                  (when (>= *esc-count* 3) (setf *running* nil)))
                ;; Power button
                (when (and (eq type :key-down) (= code ac-native.input:+key-power+))
                  (setf *running* nil))
                ;; Pass to JS
                (js-act (if (eq type :key-down) 1 0)
                        (or key "") code)))

            ;; sim
            (js-sim)

            ;; paint
            (js-paint frame)

            ;; Check for jump or poweroff from JS
            (when ac-native.js-bridge::*poweroff-requested*
              (setf *running* nil))
            (when ac-native.js-bridge::*jump-target*
              ;; TODO: reload piece
              (format *error-output* "[js] jump to ~A (not yet implemented)~%"
                      ac-native.js-bridge::*jump-target*)
              (force-output *error-output*)
              (setf ac-native.js-bridge::*jump-target* nil))

            ;; Present
            (ac-native.drm:drm-present display screen scale)
            (frame-sync-60fps))

        ;; Cleanup
        (js-destroy)
        (when audio (audio-destroy audio))
        (ac-native.input:input-destroy input)
        (fb-destroy screen)
        (ac-native.drm:drm-destroy display)
        (format *error-output* "[js] shutdown~%")
        (force-output *error-output*)))))

;;; ── Main ──

(defvar *js-fallback* nil "Set to T when falling back from JS to prevent re-entry loop.")

(defun main ()
  "AC Native OS entry point. Runs .mjs pieces via QuickJS or native CL notepat."
  ;; Run .mjs pieces via QuickJS bridge
  (unless *js-fallback*
    (let ((args (uiop:command-line-arguments)))
      (when args
        (let ((piece-path (first args)))
          (when (and piece-path (search ".mjs" piece-path))
            (return-from main (main-js piece-path)))))))
  (setf *js-fallback* nil)

  ;; No .mjs piece specified — fall back to native CL notepat
  (format *error-output* "~%════════════════════════════════════~%")
  (format *error-output* "  notepat (Common Lisp)~%")
  (format *error-output* "  SBCL ~A~%" (lisp-implementation-version))
  (format *error-output* "════════════════════════════════════~%~%")
  (force-output *error-output*)

  (init-key-note-map)

  (let ((display (handler-case (ac-native.drm:drm-init)
                   (error (e)
                     (format *error-output* "[notepat] DRM error: ~A~%" e)
                     (force-output *error-output*) nil))))
    (unless display
      (format *error-output* "[notepat] FATAL: no display~%")
      (force-output *error-output*) (sleep 30) (return-from main 1))

    (let* ((dw (ac-native.drm:display-width display))
           (dh (ac-native.drm:display-height display))
           (scale (compute-pixel-scale dw))
           (sw (floor dw scale))
           (sh (floor dh scale)))
      (setf *np-screen* (fb-create sw sh)
            *np-graph* (make-graph :fb *np-screen* :screen *np-screen*)
            *np-input* (ac-native.input:input-init dw dh scale)
            *np-audio* (ac-native.audio:audio-init)
            *np-frame* 0)

      (format *error-output* "[notepat] ~Dx~D scale:~D → ~Dx~D~%"
              dw dh scale sw sh)
      (format *error-output* "[notepat] audio: ~A~%"
              (if *np-audio* "OK" "FAILED"))
      (force-output *error-output*)

      (font-init)
      (load-build-metadata)

      ;; ── Boot splash ──
      (let* ((cfg (ac-native.config:load-config))
             (handle (ac-native.config:config-handle cfg))
             (has-handle (and handle (string/= handle "") (string/= handle "unknown")))
             (greeting (time-greeting))
             (splash-start (monotonic-time-ms)))
        ;; Write tokens
        (handler-case (ac-native.config:write-device-tokens cfg)
          (error (e)
            (format *error-output* "[notepat] token write error: ~A~%" e)
            (force-output *error-output*)))
        ;; Store handle for status display
        (setf *boot-handle* (if has-handle handle nil))
        ;; Show splash for 3 seconds or until keypress
        (loop while (< (- (monotonic-time-ms) splash-start) 3000) do
          (let ((events (ac-native.input:input-poll *np-input*)))
            (when (some (lambda (ev) (eq (ac-native.input:event-type ev) :key-down)) events)
              (return)))
          ;; Paint splash
          (graph-wipe *np-graph* (make-color :r 10 :g 12 :b 18))
          (let ((cy (floor sh 3)))
            (if has-handle
                (progn
                  ;; Greeting
                  (graph-ink *np-graph* (make-color :r 140 :g 160 :b 200 :a 220))
                  (font-draw *np-graph* greeting
                             (- (floor sw 2) (floor (font-measure greeting) 2)) cy)
                  ;; @handle
                  (let ((htxt (format nil "@~A" handle)))
                    (graph-ink *np-graph* (make-color :r 80 :g 255 :b 140 :a 255))
                    (font-draw *np-graph* htxt
                               (- (floor sw 2) (floor (font-measure htxt) 2)) (+ cy 14)))
                  ;; Subtitle
                  (graph-ink *np-graph* (make-color :r 80 :g 80 :b 100 :a 160))
                  (font-draw *np-graph* "aesthetic.computer"
                             (- (floor sw 2) (floor (font-measure "aesthetic.computer") 2)) (+ cy 32)))
                (progn
                  ;; No handle: just show name
                  (graph-ink *np-graph* (make-color :r 80 :g 255 :b 140 :a 255))
                  (font-draw *np-graph* "aesthetic.computer"
                             (- (floor sw 2) (floor (font-measure "aesthetic.computer") 2)) cy)
                  (graph-ink *np-graph* (make-color :r 140 :g 160 :b 200 :a 180))
                  (font-draw *np-graph* "notepat"
                             (- (floor sw 2) (floor (font-measure "notepat") 2)) (+ cy 18)))))
            ;; Build name (bottom center)
            (graph-ink *np-graph* (make-color :r 60 :g 60 :b 80 :a 120))
            (font-draw *np-graph* *build-name*
                       (- (floor sw 2) (floor (font-measure *build-name*) 2)) (- sh 20))
            ;; LISP tag (top right) when CL variant
            (when (string= *build-variant* "cl")
              (graph-ink *np-graph* (make-color :r 255 :g 200 :b 80 :a 200))
              (font-draw *np-graph* "LISP" (- sw (font-measure "LISP") 6) 6)))
          (ac-native.drm:drm-present display *np-screen* scale)
          (frame-sync-60fps)))

      ;; Start Swank server for remote REPL
      (handler-case
          (progn
            (setf swank::*communication-style* :spawn)
            (swank:create-server :port 4005 :dont-close t)
            (format *error-output* "[notepat] Swank on :4005~%")
            (force-output *error-output*))
        (error (e)
          (format *error-output* "[notepat] Swank failed: ~A~%" e)
          (force-output *error-output*)))

      ;; Main loop
      (setf *running* t)
      (unwind-protect
          (loop while *running* do
            (incf *np-frame*)

            ;; FPS
            (let ((now (monotonic-time-ms)))
              (when (> *fps-last-time* 0.0d0)
                (incf *fps-accum* (- now *fps-last-time*))
                (incf *fps-samples*)
                (when (>= *fps-samples* 30)
                  (setf *fps-display* (round (/ 30000.0d0 *fps-accum*)))
                  (setf *fps-accum* 0.0d0 *fps-samples* 0)))
              (setf *fps-last-time* now))

            ;; ── Metronome tick ──
            (when (and *metronome-on* (> *metronome-bpm* 0) *np-audio*)
              (let* ((now-ms (monotonic-time-ms))
                     (ms-per-beat (/ 60000.0d0 *metronome-bpm*))
                     (beat-number (floor now-ms ms-per-beat))
                     ;; Pendulum: sinusoidal swing over 2-beat period
                     (beat-phase (/ (mod now-ms (* ms-per-beat 2)) (* ms-per-beat 2))))
                (setf *metronome-phase* (sin (* beat-phase pi 2.0d0)))
                (when (/= beat-number *metronome-last-beat*)
                  (setf *metronome-last-beat* beat-number)
                  (setf *metronome-flash* 1.0)
                  (let ((downbeat (zerop (mod beat-number 4))))
                    (audio-synth *np-audio* :type 3  ; square
                                 :tone (if downbeat 1200.0d0 800.0d0)
                                 :duration 0.03d0
                                 :volume (if downbeat 0.4d0 0.25d0)
                                 :attack 0.001d0 :decay 0.02d0)))))

            ;; Decay metronome flash
            (when (> *metronome-flash* 0.0)
              (decf *metronome-flash* 0.15)
              (when (< *metronome-flash* 0.0) (setf *metronome-flash* 0.0)))

            ;; ── Input ──
            (dolist (ev (ac-native.input:input-poll *np-input*))
              (let ((type (ac-native.input:event-type ev))
                    (code (ac-native.input:event-code ev)))

                (when (eq type :key-down)
                  ;; ESC: triple-press to quit
                  (when (= code ac-native.input:+key-esc+)
                    (when (> (- *np-frame* *esc-last-frame*) 90) (setf *esc-count* 0))
                    (incf *esc-count*)
                    (setf *esc-last-frame* *np-frame*)
                    (when (and *np-audio* (< *esc-count* 3))
                      (audio-synth *np-audio* :type 3
                                   :tone (if (= *esc-count* 1) 440.0d0 660.0d0)
                                   :duration 0.08d0 :volume 0.15d0
                                   :attack 0.002d0 :decay 0.06d0))
                    (when (>= *esc-count* 3) (setf *running* nil)))

                  ;; Power
                  (when (= code ac-native.input:+key-power+) (setf *running* nil))

                  ;; Shift: toggle quick mode
                  (when (= code 42)  ; KEY_LEFTSHIFT
                    (setf *quick-mode* (not *quick-mode*)))

                  ;; Space: toggle metronome
                  (when (= code ac-native.input:+key-space+)
                    (setf *metronome-on* (not *metronome-on*))
                    (when *metronome-on*
                      (setf *metronome-last-beat* -1)))

                  ;; Minus / Equal: BPM control
                  (when (= code ac-native.input:+key-minus+)
                    (setf *metronome-bpm* (max 20 (- *metronome-bpm* 5))))
                  (when (= code ac-native.input:+key-equal+)
                    (setf *metronome-bpm* (min 300 (+ *metronome-bpm* 5))))

                  ;; Number keys: set octave (kills active voices)
                  (when (and (>= code ac-native.input:+key-1+)
                             (<= code ac-native.input:+key-9+))
                    (let ((new-oct (1+ (- code ac-native.input:+key-1+))))
                      (unless (= new-oct *octave*)
                        (kill-all-voices *np-audio*)
                        (setf *octave* new-oct))))

                  ;; Arrow up/down: octave
                  (when (= code ac-native.input:+key-up+)
                    (when (< *octave* 9)
                      (kill-all-voices *np-audio*)
                      (incf *octave*)))
                  (when (= code ac-native.input:+key-down+)
                    (when (> *octave* 1)
                      (kill-all-voices *np-audio*)
                      (decf *octave*)))

                  ;; Tab / Arrow left/right: cycle wave type
                  (when (or (= code ac-native.input:+key-tab+)
                            (= code ac-native.input:+key-right+))
                    (kill-all-voices *np-audio*)
                    (setf *wave-index* (mod (1+ *wave-index*) 5))
                    (when *np-audio*
                      (let ((tones #(660.0d0 550.0d0 440.0d0 330.0d0 220.0d0)))
                        (audio-synth *np-audio* :type *wave-index*
                                     :tone (aref tones *wave-index*)
                                     :duration 0.07d0 :volume 0.18d0
                                     :attack 0.002d0 :decay 0.06d0))))
                  (when (= code ac-native.input:+key-left+)
                    (kill-all-voices *np-audio*)
                    (setf *wave-index* (mod (+ *wave-index* 4) 5))
                    (when *np-audio*
                      (let ((tones #(660.0d0 550.0d0 440.0d0 330.0d0 220.0d0)))
                        (audio-synth *np-audio* :type *wave-index*
                                     :tone (aref tones *wave-index*)
                                     :duration 0.07d0 :volume 0.18d0
                                     :attack 0.002d0 :decay 0.06d0))))

                  ;; Note keys
                  (let ((mapping (gethash code *key-note-map*)))
                    (when (and mapping (not (gethash code *active-voices*)) *np-audio*)
                      (let* ((note-name (car mapping))
                             (oct-delta (cdr mapping))
                             (actual-octave (+ *octave* oct-delta))
                             (freq (note-to-freq note-name actual-octave))
                             (idx (position note-name *chromatic* :test #'string=))
                             (semitones (+ (* (- actual-octave 4) 12) (or idx 0)))
                             (pan (max -0.8d0 (min 0.8d0 (/ (- semitones 12) 15.0d0))))
                             (attack (if *quick-mode* 0.002d0 0.005d0))
                             (voice-id (audio-synth *np-audio*
                                                    :type *wave-index*
                                                    :tone freq
                                                    :volume 0.7d0
                                                    :duration 0
                                                    :attack attack
                                                    :decay 0.1d0
                                                    :pan pan)))
                        (setf (gethash code *active-voices*) voice-id)
                        (setf (gethash code *active-notes*)
                              (cons note-name actual-octave))))))

                ;; Key up
                (when (eq type :key-up)
                  (let ((voice-id (gethash code *active-voices*)))
                    (when (and voice-id *np-audio*)
                      (audio-synth-kill *np-audio* voice-id)
                      (remhash code *active-voices*)
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

            ;; ── Background color from active notes ──
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
                    (let ((target-r (floor (* (floor tr n) 35) 100))
                          (target-g (floor (* (floor tg n) 35) 100))
                          (target-b (floor (* (floor tb n) 35) 100)))
                      (setf *bg-r* (+ *bg-r* (floor (- target-r *bg-r*) 4)))
                      (setf *bg-g* (+ *bg-g* (floor (- target-g *bg-g*) 4)))
                      (setf *bg-b* (+ *bg-b* (floor (- target-b *bg-b*) 4)))))
                  (progn
                    (setf *bg-r* (+ *bg-r* (floor (- 20 *bg-r*) 8)))
                    (setf *bg-g* (+ *bg-g* (floor (- 20 *bg-g*) 8)))
                    (setf *bg-b* (+ *bg-b* (floor (- 25 *bg-b*) 8))))))

            ;; Metronome flash brightens background
            (when (> *metronome-flash* 0.0)
              (let ((boost (floor (* *metronome-flash* 40))))
                (setf *bg-r* (min 255 (+ *bg-r* boost)))
                (setf *bg-g* (min 255 (+ *bg-g* boost)))
                (setf *bg-b* (min 255 (+ *bg-b* boost)))))

            ;; ══════════════ PAINT ══════════════
            (graph-wipe *np-graph* (make-color :r *bg-r* :g *bg-g* :b *bg-b*))

            ;; ── Trails ──
            (maphash (lambda (trail-key val)
                       (let* ((note-name (car trail-key))
                              (oct (cdr trail-key))
                              (rgb (note-color-rgb note-name))
                              (note-idx (or (position note-name *chromatic* :test #'string=) 0))
                              (semi (+ (* (- oct 1) 12) note-idx))
                              (total-semitones (* 9 12))
                              (bar-h (max 2 (floor (- sh 30) total-semitones)))
                              (bar-y (+ 14 (floor (* semi (- sh 30)) total-semitones)))
                              (bar-w (max 1 (floor (* val sw))))
                              (bar-x (floor (- sw bar-w) 2))
                              (alpha (max 1 (min 255 (floor (* val 200))))))
                         (graph-ink *np-graph* (make-color :r (first rgb)
                                                     :g (second rgb)
                                                     :b (third rgb)
                                                     :a alpha))
                         (graph-box *np-graph* bar-x bar-y bar-w bar-h)))
                     *trails*)

            ;; ── Active note bars ──
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
                         (graph-ink *np-graph* (make-color :r (min 255 (+ (first rgb) 40))
                                                     :g (min 255 (+ (second rgb) 40))
                                                     :b (min 255 (+ (third rgb) 40))
                                                     :a 220))
                         (graph-box *np-graph* 0 bar-y sw bar-h)))
                     *active-notes*)

            ;; ── Metronome pendulum ──
            (when *metronome-on*
              (let* ((cx (floor sw 2))
                     (cy (- sh 24))
                     (arm-len (min 20 (floor sh 8)))
                     (bx (+ cx (floor (* *metronome-phase* arm-len))))
                     (bright (floor (* *metronome-flash* 255))))
                ;; Arm line
                (graph-ink *np-graph* (make-color :r 180 :g 180 :b 180 :a 120))
                (graph-line *np-graph* cx cy bx (- cy arm-len))
                ;; Bob
                (graph-ink *np-graph* (make-color :r (min 255 (+ 180 bright))
                                            :g (min 255 (+ 100 bright))
                                            :b 60 :a 220))
                (graph-circle *np-graph* bx (- cy arm-len) 3)))

            ;; ── Wave type indicators (bottom bar) ──
            (let* ((bar-y (- sh 14))
                   (btn-w (max 12 (floor sw 6)))
                   (gap 2)
                   (total-w (+ (* 5 btn-w) (* 4 gap)))
                   (start-x (floor (- sw total-w) 2)))
              (dotimes (i 5)
                (let* ((bx (+ start-x (* i (+ btn-w gap))))
                       (selected (= i *wave-index*))
                       (col (if selected
                                (make-color :r 255 :g 255 :b 255 :a 200)
                                (make-color :r 100 :g 100 :b 110 :a 140))))
                  ;; Button background
                  (if selected
                      (progn
                        (graph-ink *np-graph* (make-color :r 60 :g 50 :b 80 :a 200))
                        (graph-box *np-graph* bx bar-y btn-w 12))
                      (progn
                        (graph-ink *np-graph* (make-color :r 30 :g 28 :b 35 :a 150))
                        (graph-box *np-graph* bx bar-y btn-w 12)))
                  ;; Wave name (abbreviated to 3 chars)
                  (graph-ink *np-graph* col)
                  (let ((abbr (subseq (aref *wave-names* i) 0 (min 3 (length (aref *wave-names* i))))))
                    (font-draw *np-graph* abbr
                               (+ bx (floor (- btn-w (* (length abbr) 6)) 2))
                               (+ bar-y 2))))))

            ;; ── Status text ──
            ;; Top-left: piece name + mode indicators
            (graph-ink *np-graph* (make-color :r 100 :g 100 :b 110 :a 150))
            (font-draw *np-graph* "notepat" 3 3)

            ;; Quick mode indicator
            (when *quick-mode*
              (graph-ink *np-graph* (make-color :r 255 :g 200 :b 50 :a 200))
              (font-draw *np-graph* "Q" (+ 3 (* 8 6)) 3))

            ;; Handle (top-left, after piece name)
            (when *boot-handle*
              (let ((htxt (format nil "@~A" *boot-handle*)))
                (graph-ink *np-graph* (make-color :r 80 :g 255 :b 140 :a 140))
                (font-draw *np-graph* htxt (+ 3 (* (if *quick-mode* 10 8) 6)) 3)))

            ;; Octave (top-left, below title)
            (graph-ink *np-graph* (make-color :r 160 :g 160 :b 170 :a 180))
            (font-draw *np-graph* (format nil "OCT ~D" *octave*) 3 14)

            ;; Metronome BPM (if on)
            (when *metronome-on*
              (graph-ink *np-graph* (make-color :r 180 :g 140 :b 60 :a 200))
              (font-draw *np-graph* (format nil "~DBPM" *metronome-bpm*)
                         (+ 3 (* 7 6)) 14))

            ;; FPS (top-right)
            (let ((fps-txt (format nil "~D" *fps-display*)))
              (graph-ink *np-graph* (make-color :r 80 :g 80 :b 90 :a 120))
              (font-draw *np-graph* fps-txt (- sw (* (length fps-txt) 6) 3) 3))

            ;; Voice count (top-right, below FPS)
            (let ((vc (hash-table-count *active-voices*)))
              (when (> vc 0)
                (let ((txt (format nil "~Dv" vc)))
                  (graph-ink *np-graph* (make-color :r 200 :g 200 :b 200 :a 180))
                  (font-draw *np-graph* txt (- sw (* (length txt) 6) 3) 14))))

            ;; IP + Swank (top center)
            (when (> (length *ip-address*) 0)
              (let ((txt (format nil "~A:4005" *ip-address*)))
                (graph-ink *np-graph* (make-color :r 60 :g 180 :b 60 :a 160))
                (font-draw *np-graph* txt (- (floor sw 2) (floor (font-measure txt) 2)) 3)))

            ;; Refresh IP every ~5 seconds
            (when (zerop (mod *np-frame* 300)) (refresh-ip))

            ;; ── Present ──
            (ac-native.drm:drm-present display *np-screen* scale)
            (frame-sync-60fps))

        ;; ── Cleanup ──
        (when *np-audio* (ignore-errors (kill-all-voices *np-audio*)))
        (when *np-audio* (ignore-errors (audio-destroy *np-audio*)))
        (when *np-input* (ignore-errors (ac-native.input:input-destroy *np-input*)))
        (when *np-screen* (ignore-errors (fb-destroy *np-screen*)))
        (ignore-errors (ac-native.drm:drm-destroy display))
        (format *error-output* "[notepat] shutdown~%")
        (force-output *error-output*))))
