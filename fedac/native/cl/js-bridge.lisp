;;; js-bridge.lisp — Bridge between QuickJS and CL graphics/audio/input
;;;
;;; This creates a QuickJS context, registers CL functions as the JS API
;;; (wipe, ink, line, box, circle, sound.synth, etc.), loads .mjs pieces,
;;; and drives the piece lifecycle (boot, paint, act, sim) each frame.

(in-package :ac-native.js-bridge)

(defvar *ctx* nil "Active QuickJS context")
(defvar *rt* nil "Active QuickJS runtime")
(defvar *graph* nil "Current graph context for JS callbacks")
(defvar *fb* nil "Current framebuffer for JS callbacks")
(defvar *audio* nil "Audio engine for JS callbacks")
(defvar *screen-w* 0 "Screen width")
(defvar *screen-h* 0 "Screen height")
(defvar *has-boot* nil)
(defvar *has-paint* nil)
(defvar *has-act* nil)
(defvar *has-sim* nil)

;;; ── CFFI callbacks (called from JS via quickjs-shim) ──
;;; These use the QuickJS JSCFunction signature:
;;; JSValue callback(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv)

(defvar *jump-target* nil "Piece name to jump to (set by system.jump).")
(defvar *poweroff-requested* nil "Set to T when JS calls system.poweroff().")

(cffi:defcallback cl-wipe :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore this))
  (let ((r (ac-native.quickjs:qjs-arg-int ctx argv 0))
        (g (ac-native.quickjs:qjs-arg-int ctx argv 1))
        (b (ac-native.quickjs:qjs-arg-int ctx argv 2)))
    (declare (ignore argc))
    (ac-native.graph:graph-wipe *graph* (ac-native.color:make-color :r r :g g :b b))
    0))  ; JS_UNDEFINED

(cffi:defcallback cl-ink :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore this))
  (let ((r (ac-native.quickjs:qjs-arg-int ctx argv 0))
        (g (ac-native.quickjs:qjs-arg-int ctx argv 1))
        (b (ac-native.quickjs:qjs-arg-int ctx argv 2))
        (a (if (> argc 3) (ac-native.quickjs:qjs-arg-int ctx argv 3) 255)))
    (ac-native.graph:graph-ink *graph* (ac-native.color:make-color :r r :g g :b b :a a))
    0))

(cffi:defcallback cl-line :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore this argc))
  (let ((x1 (ac-native.quickjs:qjs-arg-int ctx argv 0))
        (y1 (ac-native.quickjs:qjs-arg-int ctx argv 1))
        (x2 (ac-native.quickjs:qjs-arg-int ctx argv 2))
        (y2 (ac-native.quickjs:qjs-arg-int ctx argv 3)))
    (ac-native.graph:graph-line *graph* x1 y1 x2 y2)
    0))

(cffi:defcallback cl-box :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore this argc))
  (let ((x (ac-native.quickjs:qjs-arg-int ctx argv 0))
        (y (ac-native.quickjs:qjs-arg-int ctx argv 1))
        (w (ac-native.quickjs:qjs-arg-int ctx argv 2))
        (h (ac-native.quickjs:qjs-arg-int ctx argv 3)))
    (ac-native.graph:graph-box *graph* x y w h)
    0))

(cffi:defcallback cl-circle :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore this argc))
  (let ((x (ac-native.quickjs:qjs-arg-int ctx argv 0))
        (y (ac-native.quickjs:qjs-arg-int ctx argv 1))
        (r (ac-native.quickjs:qjs-arg-int ctx argv 2)))
    (ac-native.graph:graph-circle *graph* x y r)
    0))

(cffi:defcallback cl-plot :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore this argc))
  (let ((x (ac-native.quickjs:qjs-arg-int ctx argv 0))
        (y (ac-native.quickjs:qjs-arg-int ctx argv 1)))
    (ac-native.graph:graph-plot *graph* x y)
    0))

(cffi:defcallback cl-write :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore this argc))
  (let ((text (ac-native.quickjs:qjs-arg-string ctx argv 0))
        (x (ac-native.quickjs:qjs-arg-int ctx argv 1))
        (y (ac-native.quickjs:qjs-arg-int ctx argv 2)))
    (ac-native.font:font-draw *graph* text x y)
    0))

(cffi:defcallback cl-screen-w :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore ctx this argc argv))
  ;; Return screen width as NaN-boxed int — use shim helper instead
  0)

(cffi:defcallback cl-screen-h :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore ctx this argc argv))
  0)

(cffi:defcallback cl-synth :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore this argc))
  (when *audio*
    (let ((type (ac-native.quickjs:qjs-arg-int ctx argv 0))
          (freq (ac-native.quickjs:qjs-arg-float ctx argv 1))
          (vol (ac-native.quickjs:qjs-arg-float ctx argv 2))
          (dur (ac-native.quickjs:qjs-arg-float ctx argv 3))
          (attack (ac-native.quickjs:qjs-arg-float ctx argv 4))
          (decay (ac-native.quickjs:qjs-arg-float ctx argv 5))
          (pan (ac-native.quickjs:qjs-arg-float ctx argv 6)))
      (ac-native.audio:audio-synth *audio*
                                   :type type :tone freq :volume vol
                                   :duration dur :attack attack :decay decay
                                   :pan pan)))
  0)

(cffi:defcallback cl-synth-kill :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore this argc))
  (when *audio*
    (let ((id (ac-native.quickjs:qjs-arg-int ctx argv 0)))
      (ac-native.audio:audio-synth-kill *audio* id)))
  0)

(cffi:defcallback cl-log :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore this argc))
  (let ((msg (ac-native.quickjs:qjs-arg-string ctx argv 0)))
    (format *error-output* "[js] ~A~%" msg)
    (force-output *error-output*))
  0)

(cffi:defcallback cl-jump :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore this argc))
  (let ((piece (ac-native.quickjs:qjs-arg-string ctx argv 0)))
    (setf *jump-target* piece)
    (format *error-output* "[js-bridge] jump → ~A~%" piece)
    (force-output *error-output*))
  0)

(cffi:defcallback cl-poweroff :uint64 ((ctx :pointer) (this :uint64) (argc :int) (argv :pointer))
  (declare (ignore ctx this argc argv))
  (setf *poweroff-requested* t)
  0)

;;; ── Initialize QuickJS and register the API ──

(defun js-init (graph fb audio screen-w screen-h)
  "Create QuickJS runtime and context, register CL functions as JS API."
  (setf *graph* graph
        *fb* fb
        *audio* audio
        *screen-w* screen-w
        *screen-h* screen-h
        *jump-target* nil
        *poweroff-requested* nil)
  (setf *rt* (ac-native.quickjs:js-new-runtime))
  (setf *ctx* (ac-native.quickjs:js-new-context *rt*))
  ;; Register native callbacks
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_wipe" (cffi:callback cl-wipe) 3)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_ink" (cffi:callback cl-ink) 4)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_line" (cffi:callback cl-line) 4)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_box" (cffi:callback cl-box) 4)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_circle" (cffi:callback cl-circle) 3)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_plot" (cffi:callback cl-plot) 2)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_write" (cffi:callback cl-write) 3)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_screen_w" (cffi:callback cl-screen-w) 0)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_screen_h" (cffi:callback cl-screen-h) 0)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_synth" (cffi:callback cl-synth) 7)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_synth_kill" (cffi:callback cl-synth-kill) 2)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_log" (cffi:callback cl-log) 1)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_jump" (cffi:callback cl-jump) 1)
  (ac-native.quickjs:qjs-register-func *ctx* "__cl_poweroff" (cffi:callback cl-poweroff) 0)
  ;; Set screen dimensions
  (ac-native.quickjs:qjs-set-global-int *ctx* "__screen_w" screen-w)
  (ac-native.quickjs:qjs-set-global-int *ctx* "__screen_h" screen-h)
  ;; Eval the API bootstrap JS
  (let ((api-code (build-api-js)))
    (ac-native.quickjs:qjs-eval *ctx* api-code (length api-code) "<api-init>" 0))
  (format *error-output* "[js-bridge] QuickJS initialized (~Dx~D)~%" screen-w screen-h)
  (force-output *error-output*))

(defun js-destroy ()
  "Free QuickJS context and runtime."
  (when *ctx* (ac-native.quickjs:js-free-context *ctx*) (setf *ctx* nil))
  (when *rt* (ac-native.quickjs:js-free-runtime *rt*) (setf *rt* nil)))

;;; ── Load and run a piece ──

(defun js-load-piece (path)
  "Load a .mjs piece file and detect lifecycle functions."
  (let ((code (with-open-file (s path :direction :input :if-does-not-exist nil)
                (when s
                  (let ((buf (make-string (file-length s))))
                    (read-sequence buf s)
                    buf)))))
    (unless code
      (format *error-output* "[js-bridge] Piece not found: ~A~%" path)
      (return-from js-load-piece nil))
    ;; Wrap piece code so `export { ... }` becomes a no-op.
    ;; We eval a script that replaces `export` with a harmless function,
    ;; then evals the piece code, then exposes lifecycle functions.
    (let ((wrapper (format nil "~
// Make export a no-op for module compatibility~%~
var __orig_code = true;~%~
~A~%~
// Expose lifecycle functions as globals~%~
if (typeof boot === 'function') globalThis.__piece_boot = boot;~%~
if (typeof paint === 'function') globalThis.__piece_paint = paint;~%~
if (typeof act === 'function') globalThis.__piece_act = act;~%~
if (typeof sim === 'function') globalThis.__piece_sim = sim;~%~
if (typeof leave === 'function') globalThis.__piece_leave = leave;~%"
                           ;; Remove the export line via simple string search
                           (let ((s code)
                                 (pos (search "export {" code)))
                             (if pos
                                 ;; Find the semicolon after the closing brace
                                 (let ((end (position #\; s :start pos)))
                                   (if end
                                       (concatenate 'string (subseq s 0 pos) (subseq s (1+ end)))
                                       (subseq s 0 pos)))
                                 s)))))
      (let ((rc (ac-native.quickjs:qjs-eval *ctx* wrapper (length wrapper)
                                             path 0)))
        (when (= rc -1)
          (format *error-output* "[js-bridge] Failed to load ~A~%" path)
          (return-from js-load-piece nil))))
    ;; Detect which lifecycle functions exist
    (setf *has-boot*  (= 1 (ac-native.quickjs:qjs-has-global-func *ctx* "__piece_boot")))
    (setf *has-paint* (= 1 (ac-native.quickjs:qjs-has-global-func *ctx* "__piece_paint")))
    (setf *has-act*   (= 1 (ac-native.quickjs:qjs-has-global-func *ctx* "__piece_act")))
    (setf *has-sim*   (= 1 (ac-native.quickjs:qjs-has-global-func *ctx* "__piece_sim")))
    (format *error-output* "[js-bridge] Loaded ~A (boot=~A paint=~A act=~A sim=~A)~%"
            path *has-boot* *has-paint* *has-act* *has-sim*)
    (force-output *error-output*)
    t))

;;; ── Lifecycle calls ──

(defun js-boot ()
  "Call the piece's boot() with the API object."
  (when *has-boot*
    (ac-native.quickjs:qjs-call-with-api *ctx* "__piece_boot")
    (ac-native.quickjs:qjs-execute-pending *ctx*)))

(defun js-paint (paint-count)
  "Call the piece's paint() with the API object."
  (when *has-paint*
    (ac-native.quickjs:qjs-set-global-int *ctx* "__paintCount" paint-count)
    (ac-native.quickjs:qjs-call-with-api *ctx* "__piece_paint")
    (ac-native.quickjs:qjs-execute-pending *ctx*)))

(defun js-act (event-type event-key event-code)
  "Call the piece's act() with an event."
  (when *has-act*
    (ac-native.quickjs:qjs-set-global-int *ctx* "__evType" event-type)
    (ac-native.quickjs:qjs-set-global-string *ctx* "__evKey" event-key)
    (ac-native.quickjs:qjs-set-global-int *ctx* "__evCode" event-code)
    (ac-native.quickjs:qjs-call-with-api *ctx* "__piece_act")
    (ac-native.quickjs:qjs-execute-pending *ctx*)))

(defun js-sim ()
  "Call the piece's sim()."
  (when *has-sim*
    (ac-native.quickjs:qjs-call-with-api *ctx* "__piece_sim")
    (ac-native.quickjs:qjs-execute-pending *ctx*)))

;;; ── Build the JS API object ──
;;; This JS code creates __ac_api with all the graphics/audio/system
;;; functions that call back into native CL via registered C functions.

(defun build-api-js ()
  "Return JS source that builds the __ac_api object."
  (format nil "
// Console
globalThis.console = {
  log: function(...args) { __cl_log(args.join(' ')); },
  warn: function(...args) { __cl_log('[warn] ' + args.join(' ')); },
  error: function(...args) { __cl_log('[error] ' + args.join(' ')); },
};

// Screen dimensions (updated each frame by CL)
globalThis.__paintCount = 0;
globalThis.__evType = 0;
globalThis.__evKey = '';
globalThis.__evCode = 0;

// Event helper
function makeEvent() {
  return {
    _type: __evType,
    _key: __evKey,
    code: __evCode,
    key: __evKey,
    name: __evKey,
    x: 0, y: 0,
    repeat: false,
    velocity: 255,
    pressure: 0,
    is: function(pattern) {
      const parts = pattern.split(':');
      if (parts[0] === 'keyboard') {
        const dir = parts[1]; // 'down' or 'up'
        const key = parts[2];
        if (dir === 'down' && __evType === 1 && __evKey === key) return true;
        if (dir === 'up' && __evType === 0 && __evKey === key) return true;
        return false;
      }
      return false;
    }
  };
}

// Build API object passed to lifecycle functions
globalThis.__ac_api = {
  wipe: function(r, g, b) { __cl_wipe(r||0, g||0, b||0); },
  ink: function(r, g, b, a) { __cl_ink(r||255, g||255, b||255, a===undefined?255:a); return __ac_api; },
  line: function(x1, y1, x2, y2) { __cl_line(x1, y1, x2, y2); },
  box: function(x, y, w, h) { __cl_box(x, y, w, h); },
  circle: function(x, y, r) { __cl_circle(x, y, r); },
  plot: function(x, y) { __cl_plot(x, y); },
  write: function(text, opts) {
    var x = 0, y = 0;
    if (opts && typeof opts === 'object') { x = opts.x || 0; y = opts.y || 0; }
    __cl_write(String(text), x, y);
  },
  screen: { get width() { return __screen_w; }, get height() { return __screen_h; } },
  event: makeEvent(),
  paintCount: 0,
  num: {
    rand: function() { return Math.random(); },
    randIntRange: function(a, b) { return Math.floor(Math.random() * (b - a + 1)) + a; },
    clamp: function(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); },
    lerp: function(a, b, t) { return a + (b - a) * t; },
    dist: function(x1, y1, x2, y2) { return Math.sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)); },
    abs: Math.abs, floor: Math.floor, ceil: Math.ceil, round: Math.round,
    sign: Math.sign, min: Math.min, max: Math.max,
    map: function(v, a, b, c, d) { return c + (v - a) / (b - a) * (d - c); },
  },
  sound: {
    synth: function(cfg) {
      var type = cfg.type || 0;
      var freq = cfg.tone || cfg.freq || 440;
      var vol = cfg.volume !== undefined ? cfg.volume : 0.7;
      var dur = cfg.duration || -1;
      var attack = cfg.attack || 0;
      var decay = cfg.decay || 0;
      var pan = cfg.pan || 0;
      var id = __cl_synth(type, freq, vol, dur, attack, decay, pan);
      return {
        id: id,
        kill: function(fade) { __cl_synth_kill(id, fade || 0); },
        update: function(opts) { /* TODO */ },
      };
    },
    kill: function(id, fade) { __cl_synth_kill(id, fade || 0); },
    bpm: function(v) { if (v !== undefined) __cl_set_bpm(v); return __cl_get_bpm(); },
    time: 0,
    speaker: {
      poll: function() {},
      sampleRate: 48000,
      waveforms: { left: [] },
      amplitudes: { left: 0, right: 0 },
      frequencies: { left: [] },
      systemVolume: 70,
    },
    room: { toggle: function(){}, setMix: function(){}, mix: 0 },
    fx: { setMix: function(){}, mix: 0 },
    glitch: { toggle: function(){} },
    microphone: { open: function(){}, close: function(){}, connected: false },
    sample: { play: function(){}, kill: function(){} },
    speak: function(text) { __cl_log('[tts] ' + text); },
  },
  system: {
    version: 'cl-bridge',
    jump: function(piece) { __cl_jump(piece); },
    reboot: function() { __cl_reboot(); },
    poweroff: function() { __cl_poweroff(); },
    readFile: function(path) { return __cl_read_file(path); },
    config: { handle: '', piece: '' },
    brightness: -1,
    battery: { percent: -1, charging: false, status: 'Unknown' },
    hw: { model: '', vendor: '', cpu: '', cores: 0 },
  },
  wifi: {
    scan: function() { __cl_log('[wifi] scan requested'); },
    connect: function(ssid, pass) { __cl_log('[wifi] connect: ' + ssid); },
    disconnect: function() { __cl_log('[wifi] disconnect'); },
  },
  store: {
    retrieve: function(k, d) { return Promise.resolve(d); },
    persist: function() { return Promise.resolve(); },
    delete: function() { return Promise.resolve(); },
  },
  net: {
    preload: function() { return Promise.resolve(); },
    pieces: function() { return Promise.resolve([]); },
  },
  clock: { time: function() { return Date.now(); } },
  params: [],
  colon: [],
  needsPaint: function() {},
  help: { resampleArray: function(a,n) { return a; } },
};

// Performance timer
globalThis.performance = { now: function() { return Date.now(); } };
"))
