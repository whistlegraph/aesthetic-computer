// notepat-native.mjs — Traditional notepat for ac-native bare metal

// setTimeout polyfill — QuickJS on ac-native does not provide setTimeout.
// Pending callbacks are queued here and flushed each tick from sim() via
// __tickPendingTimeouts(). Delays are in milliseconds and match browser
// semantics closely enough for the short audio/UI schedules this piece uses.
const __pendingTimeouts = [];
function setTimeout(fn, delayMs) {
  if (typeof fn !== "function") return 0;
  const at = Date.now() + (Number(delayMs) || 0);
  __pendingTimeouts.push({ fn, at });
  return __pendingTimeouts.length;
}
function __tickPendingTimeouts() {
  if (__pendingTimeouts.length === 0) return;
  const now = Date.now();
  for (let i = 0; i < __pendingTimeouts.length; ) {
    const entry = __pendingTimeouts[i];
    if (now >= entry.at) {
      __pendingTimeouts.splice(i, 1);
      try { entry.fn(); } catch (e) { console.error("setTimeout cb error:", e); }
    } else {
      i++;
    }
  }
}

let sounds = {};
let trail = {};
let frame = 0;
let escCount = 0;
let escLastFrame = 0;
let fpsLastTime = 0;
let fpsDisplay = 0;
let fpsAccum = 0;
let fpsSamples = 0;
let octave = 4;
let wave = "sine";
let waveIndex = 0;
let quickMode = false;
const wavetypes = ["sine", "triangle", "sawtooth", "square", "composite", "noise", "whistle", "sample"];
let sampleLoaded = false;  // true when sample buffer has data (default or recorded)
let lastLoadedSample = null;  // track which sample object is currently loaded
let recording = false;     // true while holding REC
let recPointerId = null;   // touch pointer currently holding REC button
let recStartTime = 0;      // Date.now() when recording started
const MAX_REC_SECS = 10;   // matches AUDIO_MAX_SAMPLE_SECS
const SAMPLE_BASE_FREQ = 261.63; // C4 — base pitch for sample playback

// Per-key sample bank: End key arms, tone key records to that key only
let sampleBank = {};       // key -> { data: Float32Array, len: number, rate: number }
let globalSample = null;   // { data: Float32Array, len: number, rate: number } — Home recording
let endArmed = false;      // true while End key is held (arm per-key recording)
let perKeyRecording = null; // key currently recording in per-key mode

// Hold/latch: F11 (green pickup) engages, F10 (red hangup) clears
// While engaged, any new notes auto-latch (sustain on key release)
let recitalMode = false; // F12: hide all UI, show only colored backdrops
let helpPanel = false;   // Meta/Win key: show keyboard shortcut help overlay
let holdActive = false;      // true when hold is engaged
let heldKeys = new Set();    // keys that are latched (won't stop on key-up)
// Flourish mode: while F11 is physically held (after the initial engage),
// new notes are NOT added to heldKeys so you can play melodic riffs over
// the latched chord without sticking every new note.
let f11Held = false;

// Effective pitch shift blended by FX mix (0% fx = no pitch shift)
function effectivePitchShift() {
  return pitchShift * fxMix;
}

// Metronome state
let metronomeEnabled = false;
let metronomeBPM = 120;
let metronomeBeatCount = 0;
let metronomeFlash = 0;
let metronomeVisualPhase = 0;
let metronomePendulumAngle = 0;  // swinging pendulum animation (-1..1)
let metronomePendulumDir = 1;

// Clock time sync
let clockOffset = 0;       // ms offset added to Date.now() for accurate time
let clockSynced = false;   // true once we have a server time sync
let clockSyncFrame = 0;    // frame counter for periodic resync
function syncedNow() { return Date.now() + clockOffset; }

// Echo (room) mix — controlled by trackpad X / slider
let echoMix = 0;
let echoDragging = false;
let pitchDragging = false;
let fxDragging = false;

// FX chain dry/wet mix (0=dry, 1=fully wet)
let fxMix = 1;
let volDragging = false;
let brtDragging = false;

// Pitch shift — controlled by trackpad Y / slider
let pitchShift = 0; // -1 to +1, 0 = no shift
let lastAppliedPitch = 0; // last pitch actually sent to synths (throttle)

// Trackpad FX control (\ toggles on/off)
let trackpadFX = false;

// Dark mode: auto based on LA time (7pm-7am)
// UEFI clock is UTC; LA is UTC-7 (PDT) or UTC-8 (PST)
// DST: second Sunday of March 2am → first Sunday of November 2am
function getLAOffset() {
  const now = new Date();
  const y = now.getUTCFullYear(), m = now.getUTCMonth();
  if (m > 2 && m < 10) return 7; // Apr-Oct: always PDT
  if (m < 2 || m > 10) return 8; // Jan-Feb, Dec: always PST
  // March (m=2): DST starts second Sunday at 2am UTC-8 (10am UTC)
  if (m === 2) {
    const d1 = new Date(Date.UTC(y, 2, 1)).getUTCDay(); // day-of-week of Mar 1
    const secondSun = 8 + (7 - d1) % 7; // date of second Sunday
    const dstStart = Date.UTC(y, 2, secondSun, 10); // 2am PST = 10am UTC
    return now.getTime() >= dstStart ? 7 : 8;
  }
  // November (m=10): DST ends first Sunday at 2am UTC-7 (9am UTC)
  const d1 = new Date(Date.UTC(y, 10, 1)).getUTCDay();
  const firstSun = 1 + (7 - d1) % 7;
  const dstEnd = Date.UTC(y, 10, firstSun, 9); // 2am PDT = 9am UTC
  return now.getTime() < dstEnd ? 7 : 8;
}
function getLAHour() {
  const now = new Date();
  return (now.getUTCHours() - getLAOffset() + 24) % 24;
}
function isDark() {
  const h = getLAHour();
  return h >= 20 || h < 7; // dark after 8pm, light after 7am (LA time)
}
let dark = isDark(); // auto: dark after 7pm LA time, light before

// Background color — average of active notes, lerped
let bgColor = [0, 0, 0];
let bgTarget = dark ? [20, 20, 25] : [240, 238, 232];

// Cached sound API ref (for leave)
let soundAPI = null;
let systemAPI = null;

// Tablet mode tracking
let lastTabletMode = null;
let usbMidiRecent = [];
let usbMidiNextRefreshFrame = 0;
let usbMidiTypecSignature = "";
let udpMidiBroadcast = false;
let udpMidiNextHeartbeatFrame = 0;


// OS update panel state machine
// states: "idle" | "checking" | "up-to-date" | "available" | "downloading" | "flashing" | "rebooting"
const OS_BASE_URL = "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com/os/";
const OS_VERSION_URL = OS_BASE_URL + "native-notepat-latest.version";
const OS_VMLINUZ_URL = OS_BASE_URL + "native-notepat-latest.vmlinuz";
const OS_VMLINUZ_BYTES = 37_000_000; // ~35MB (actual size from releases.json)
const OS_AUTO_CHECK_INTERVAL_FRAMES = 36_000; // 10 minutes @ 60fps
const OS_AUTO_RETRY_FRAMES = 900;             // 15 seconds on transient network errors
// Screen state machine — only one screen renders at a time
// "notepat" = pads/instrument, "os" = update panel, "wifi" = network picker
let activeScreen = "notepat";
let osState = "idle";
let osCurrentVersion = "";   // set from system.version on boot
let osRemoteVersion = "";    // fetched from remote
let osProgress = 0;          // 0.0-1.0
let osError = "";            // error message if any
let osFetchPending = false;  // true while waiting for version fetchResult (manual panel)
let osCheckFrame = 0;        // frame when we started the version fetch
let osUpdatePingVersion = ""; // last version we pinged to the user
let flashTargetIdx = 0;      // index into system.flashTargets

// Auto-update: background check on WiFi connect, silent download+flash+reboot
// states: "idle" | "checking" | "downloading" | "flashing" | "rebooting"
let autoUpdate = {
  state: "idle",
  fetchPending: false,  // true = fetchResult belongs to auto-update version check
  rebootFrame: 0,
  nextCheckFrame: 0,
  availableVersion: "",
  lastError: "",
};

// WiFi UI state
let wifiSelectedIdx = -1;
let wifiPassword = "";
let wifiPasswordMode = false;  // true = fullscreen password entry
let shiftHeld = false;

// AC chat: latest message fetched after WiFi connects
let acMsg = null;            // { from, text } once loaded
let lastSpokenMsgKey = "";   // "from:text" of last TTS'd message (avoid repeats on reconnect)
let wsStatus = "";           // "connecting" | "connected" | "error" | ""
let wsConnectGrace = 0;      // frames to wait before declaring error (race-condition guard)
let wsReconnectTimer = 0;   // frames until next reconnect attempt
let chatMuted = false;       // mute TTS for incoming chat messages
let wifiWasConnected = false;
let wifiConnectFrame = -9999; // frame when WiFi last connected (cooldown guard)
let lastBatPercent = -1;     // for battery change TTS

// Auto-connect: try "aesthetic.computer" hotspot when not connected
const AC_SSID = "aesthetic.computer";
const AC_PASS = "aesthetic.computer";
// Additional networks loaded from /mnt/wifi_creds.json
let autoConnectFrame = 0;    // counts frames; try every ~5s (300 frames)
let autoConnectBlink = 0;    // blink counter for antenna icon while polling
let autoConnectTry = 0;      // increments each attempt
let connectStartFrame = 0;   // frame when current connect began (for timeout)

// Saved WiFi credentials — persisted to /mnt/wifi_creds.json (USB EFI partition)
const CREDS_PATH = "/mnt/wifi_creds.json";
let savedCreds = [];         // [{ssid, pass}, ...] loaded at boot

// === DJ DECK (ported from dj.mjs) ===
// Integrated turntable playback so notepat can layer melody/percussion
// over a backing track, sync tempo, and scratch — all in one piece.
const DJ_AUDIO_EXTS = new Set(["mp3", "wav", "flac", "ogg", "aac", "m4a", "opus", "wma"]);
let djFiles = [];            // [{path, name}] discovered on USB
let djTrackIdx = 0;
let djMounted = false;
let djUsbConnected = false;
let djLastUsbCheckFrame = 0;
let djMessage = "";
let djMessageFrame = 0;
let djAngle = 0;             // visual spin angle (not drawn big, just for strip)
let djDragging = false;      // true while user is scratching the deck strip
let djDragWasPlaying = false;
let djDragLastX = 0;
let djScratchSpeed = 0;
// Tap-tempo state for syncing notepat metronome to the current track
let djTapTimes = [];         // last few tap timestamps (ms)
let djDerivedBPM = 0;        // 0 = not yet tapped, otherwise tapped BPM
// Cached deck strip geometry from paint() for hit-testing in act()
let djStrip = null;          // { x, y, w, h, btnPlay, btnPrev, btnNext, btnScan, btnTap }

function djIsAudio(name) {
  if (name.startsWith(".")) return false;
  const dot = name.lastIndexOf(".");
  return dot >= 0 && DJ_AUDIO_EXTS.has(name.slice(dot + 1).toLowerCase());
}

function djScanDir(system, path, results, depth) {
  if (depth > 4) return;
  const listing = system?.listDir?.(path);
  if (!listing) return;
  for (const f of listing) {
    const full = path + "/" + f.name;
    if (f.isDir && !f.name.startsWith(".")) djScanDir(system, full, results, depth + 1);
    else if (djIsAudio(f.name)) results.push({ path: full, name: f.name });
  }
}

function djScan(system, sound) {
  djFiles = [];
  for (const d of ["/media", "/mnt/samples", "/mnt"]) djScanDir(system, d, djFiles, 0);
  djFiles.sort((a, b) => a.name.localeCompare(b.name));
  djMsg(djFiles.length > 0 ? `${djFiles.length} tracks` : "no tracks found");
  sound?.speak?.(djFiles.length > 0 ? `${djFiles.length} tracks` : "no tracks");
}

function djLoadTrack(sound) {
  if (djFiles.length === 0) return;
  if (djTrackIdx >= djFiles.length) djTrackIdx = 0;
  if (djTrackIdx < 0) djTrackIdx = djFiles.length - 1;
  const f = djFiles[djTrackIdx];
  const ok = sound?.deck?.load?.(0, f.path);
  if (ok) {
    const name = f.name.replace(/\.[^.]+$/, "");
    djMsg(name);
    sound?.speak?.(name);
    sound.deck.play(0);
  } else {
    djMsg("failed: " + f.name);
  }
}

function djMsg(t) { djMessage = t; djMessageFrame = frame; }

function djFmt(s) {
  if (!s || s < 0) return "0:00";
  return `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, "0")}`;
}

function djTogglePlay(sound) {
  const dk = sound?.deck;
  const d = dk?.decks?.[0];
  if (!d?.loaded) return;
  if (d.playing) { dk.pause(0); djMsg("paused"); }
  else { dk.play(0); djMsg("playing"); }
}

// Tap-tempo: each call records a timestamp; BPM = 60000 / avg interval across
// the last 4 intervals. Tapping in time with the deck track sets djDerivedBPM
// and also updates notepat's metronomeBPM so the click track lines up.
function djTapTempo() {
  const now = Date.now();
  // Reset if the tap gap is too big (>2s means a fresh tap sequence)
  if (djTapTimes.length > 0 && now - djTapTimes[djTapTimes.length - 1] > 2000) {
    djTapTimes = [];
  }
  djTapTimes.push(now);
  if (djTapTimes.length > 5) djTapTimes.shift();
  if (djTapTimes.length >= 2) {
    let sum = 0;
    for (let i = 1; i < djTapTimes.length; i++) sum += djTapTimes[i] - djTapTimes[i - 1];
    const avg = sum / (djTapTimes.length - 1);
    if (avg > 150 && avg < 2000) {
      djDerivedBPM = Math.round(60000 / avg);
      metronomeBPM = Math.max(20, Math.min(300, djDerivedBPM));
      metronomeBeatCount = Math.floor(syncedNow() / (60000 / metronomeBPM));
      djMsg(`${djDerivedBPM} bpm`);
    }
  }
}

// US-QWERTY shift map for bare-metal text input (no OS layout handling)
const SHIFT_MAP = {
  "1":"!","2":"@","3":"#","4":"$","5":"%","6":"^","7":"&","8":"*","9":"(","0":")",
  "-":"_","=":"+","[":"{","]":"}",";":":","'":'"',",":"<",".":">","/":"?","\\":"|","`":"~",
};
let wifiCursorBlink = 0;       // cursor blink counter
// Touch-note state (for clickable grid buttons)
let touchNotes = {};  // pointer id -> { key, note, octave }
// Hover / cursor position (updated from touch + draw events)
let hoverX = -1, hoverY = -1;

// Chromatic note order per octave
const CHROMATIC = ["c","c#","d","d#","e","f","f#","g","g#","a","a#","b"];

// Keyboard mapping matching notepat.mjs NOTE_TO_KEYBOARD_KEY
const NOTE_TO_KEY = {
  "c": "c", "c#": "v", "d": "d", "d#": "s", "e": "e", "f": "f",
  "f#": "w", "g": "g", "g#": "r", "a": "a", "a#": "q", "b": "b",
  "+c": "h", "+c#": "t", "+d": "i", "+d#": "y", "+e": "j", "+f": "k",
  "+f#": "u", "+g": "l", "+g#": "o", "+a": "m", "+a#": "p", "+b": "n",
  "++c": ";", "++c#": "'", "++d": "]",
  "-a#": "z", "-b": "x",
};

const KEY_TO_NOTE = {};
for (const [note, key] of Object.entries(NOTE_TO_KEY)) {
  KEY_TO_NOTE[key] = note;
}

function parseNote(name) {
  if (name.startsWith("++")) return [name.slice(2), 2];
  if (name.startsWith("+")) return [name.slice(1), 1];
  if (name.startsWith("-")) return [name.slice(1), -1];
  return [name, 0];
}

const LEFT_GRID = [
  ["c", "c#", "d", "d#"],
  ["e", "f", "f#", "g"],
  ["g#", "a", "a#", "b"],
];
const RIGHT_GRID = [
  ["+c", "+c#", "+d", "+d#"],
  ["+e", "+f", "+f#", "+g"],
  ["+g#", "+a", "+a#", "+b"],
];

const NOTE_COLORS = {
  c: [255, 30, 30], "c#": [255, 80, 0],
  d: [255, 150, 0], "d#": [200, 200, 0],
  e: [230, 220, 0],
  f: [30, 200, 30], "f#": [0, 200, 180],
  g: [30, 100, 255], "g#": [80, 50, 255],
  a: [140, 30, 220], "a#": [200, 30, 150],
  b: [200, 50, 255],
};

// === PERCUSSION LAYOUT ===
// Pgup toggles drum kit for the left octave grid, Pgdn for the right.
// When active, the 12 notes in that grid become drum hits with their own
// synth recipes and optional per-drum recorded samples.
let percussionLeft = false;
let percussionRight = false;

// Per-octave master volume. Two vertical sliders (left of the left grid,
// right of the right grid) scale every note / drum played on that side so
// you can balance melody-vs-percussion or left-chord-vs-right-melody in
// real time without touching the global fx/echo sliders. 0.0 = silent,
// 1.0 = unity (no boost), values >1 just pass through since drums already
// run hot.
let leftMasterVol = 1.0;
let rightMasterVol = 1.0;
let leftVolDragging = false;
let rightVolDragging = false;

// Returns the master volume for the grid that produced a note, based on the
// gridOffset we pass through from parseNote / hitTestGrid (0 = left, 1 = right).
function masterForSide(gridOffset) {
  return gridOffset === 1 ? rightMasterVol : leftMasterVol;
}
let percussionNotice = null;      // { text, until } — transient visual flash
let percussionSampleBank = {};    // drumName -> { data: Float32Array, len, rate }

// Natural notes = 7 basic drums; sharps = 5 accent percussion.
const PERCUSSION_NAMES = {
  c: "kick", d: "snare", e: "clap", f: "snap",
  g: "hat-c", a: "hat-o", b: "ride",
  "c#": "crash", "d#": "splash",
  "f#": "cowbell", "g#": "block", "a#": "tambo",
};

// 3-char display labels shown on the drum pads in place of note names.
const PERCUSSION_LABELS = {
  c: "BAS", d: "SNR", e: "CLP", f: "SNP",
  g: "HHC", a: "HHO", b: "RDE",
  "c#": "CRS", "d#": "SPL",
  "f#": "CBL", "g#": "BLK", "a#": "TMB",
};

// Metallic / earthy colors to visually distinguish drum pads from melodic keys.
const PERCUSSION_COLORS = {
  c: [220, 90, 40],      // kick — deep orange
  d: [220, 180, 110],    // snare — tan
  e: [240, 220, 130],    // clap — pale yellow
  f: [220, 240, 140],    // snap — yellow-green
  g: [120, 220, 180],    // closed hat — mint
  a: [120, 200, 240],    // open hat — cyan
  b: [180, 180, 230],    // ride — silver-blue
  "c#": [220, 150, 240], // crash — lavender
  "d#": [240, 160, 220], // splash — pink
  "f#": [200, 150, 80],  // cowbell — brass
  "g#": [190, 120, 70],  // block — wood brown
  "a#": [230, 210, 170], // tambourine — sandy
};

// Return drum name if the given (letter, grid offset) is a live drum pad, else null.
// offset 0 = left grid, 1 = right grid. Low/high extras (z, x, ;, ', ]) stay melodic.
function percussionDrumFor(letter, offset) {
  if (!PERCUSSION_NAMES[letter]) return null;
  if (offset === 0 && percussionLeft) return PERCUSSION_NAMES[letter];
  if (offset === 1 && percussionRight) return PERCUSSION_NAMES[letter];
  return null;
}

// Fire a drum hit from short auto-stopping synth voices. No cleanup
// needed on key-up because every voice uses a finite duration.
function playPercussion(sound, letter, volume = 1.0, pan = 0, pitchFactor = 1.0) {
  if (!sound?.synth) return;
  // Cap at 2.2 so drums can push voice volumes above 1.0. The native audio
  // mixer auto-divides by total voice weight (audio.c mix_divisor), which
  // means a drum hit with vol=0.9 sharing the mix with three 0.7 sustained
  // melody voices gets crushed to ~0.3 — basically invisible. Pushing drum
  // voice volumes to ~1.6-2.0 puts them at ~2x the ratio of the melodies
  // even under heavy polyphony, so they cut through like real drums.
  const v = Math.max(0.1, Math.min(2.2, volume));
  const pf = Math.max(0.25, Math.min(4, pitchFactor)); // clamp to ±2 octaves
  switch (letter) {
    case "c": // kick — 808/909-style with beater click, body punch, sub boom, and warm tail
      // 1. Beater click — very brief high transient, makes the ear hear "drum" not "tone"
      sound.synth({ type: "triangle", tone: 1800 * pf, duration: 0.007, volume: 1.1 * v, attack: 0.0003, decay: 0.006, pan });
      sound.synth({ type: "noise", tone: 4000 * pf, duration: 0.005, volume: 0.7 * v, attack: 0.0003, decay: 0.004, pan });
      // 2. Body punch — short mid-low sine for the "thump" (fast linear decay simulates pitch drop)
      sound.synth({ type: "sine", tone: 140 * pf, duration: 0.03, volume: 1.4 * v, attack: 0.0005, decay: 0.028, pan });
      // 3. Sub boom — long low fundamental at ~45 Hz, the felt bass weight. This one runs HOT so
      //    it dominates the mix even against sustained melody voices (see auto-mixer math in audio.c).
      sound.synth({ type: "sine", tone: 45 * pf, duration: 0.5, volume: 2.0 * v, attack: 0.001, decay: 0.49, pan });
      // 4. Warm low-mid tail — fills the 60-80 Hz gap so the kick doesn't feel hollow
      sound.synth({ type: "sine", tone: 72 * pf, duration: 0.18, volume: 1.0 * v, attack: 0.002, decay: 0.17, pan });
      break;
    case "d": // snare
      sound.synth({ type: "noise", tone: 2200 * pf, duration: 0.12, volume: 0.55 * v, attack: 0.001, decay: 0.11, pan });
      sound.synth({ type: "triangle", tone: 220 * pf, duration: 0.1, volume: 0.4 * v, attack: 0.001, decay: 0.09, pan });
      sound.synth({ type: "square", tone: 180 * pf, duration: 0.05, volume: 0.2 * v, attack: 0.001, decay: 0.045, pan });
      break;
    case "e": // clap — staggered noise bursts for classic handclap texture
      sound.synth({ type: "noise", tone: 1400 * pf, duration: 0.02, volume: 0.45 * v, attack: 0.0005, decay: 0.018, pan });
      setTimeout(() => sound.synth({ type: "noise", tone: 1450 * pf, duration: 0.02, volume: 0.4 * v, attack: 0.0005, decay: 0.018, pan }), 10);
      setTimeout(() => sound.synth({ type: "noise", tone: 1550 * pf, duration: 0.02, volume: 0.35 * v, attack: 0.0005, decay: 0.018, pan }), 22);
      setTimeout(() => sound.synth({ type: "noise", tone: 1600 * pf, duration: 0.1, volume: 0.3 * v, attack: 0.001, decay: 0.09, pan }), 34);
      break;
    case "f": // snap — finger snap click + short body
      sound.synth({ type: "noise", tone: 3200 * pf, duration: 0.015, volume: 0.45 * v, attack: 0.0003, decay: 0.014, pan });
      sound.synth({ type: "square", tone: 1800 * pf, duration: 0.02, volume: 0.22 * v, attack: 0.0005, decay: 0.018, pan });
      sound.synth({ type: "triangle", tone: 2400 * pf, duration: 0.025, volume: 0.18 * v, attack: 0.0005, decay: 0.022, pan });
      break;
    case "g": // closed hi-hat
      sound.synth({ type: "noise", tone: 7000 * pf, duration: 0.04, volume: 0.35 * v, attack: 0.0005, decay: 0.035, pan });
      sound.synth({ type: "noise", tone: 5000 * pf, duration: 0.04, volume: 0.2 * v, attack: 0.0005, decay: 0.035, pan });
      break;
    case "a": // open hi-hat
      sound.synth({ type: "noise", tone: 6500 * pf, duration: 0.28, volume: 0.3 * v, attack: 0.001, decay: 0.27, pan });
      sound.synth({ type: "noise", tone: 4800 * pf, duration: 0.2, volume: 0.18 * v, attack: 0.001, decay: 0.19, pan });
      break;
    case "b": // ride
      sound.synth({ type: "noise", tone: 4200 * pf, duration: 0.4, volume: 0.28 * v, attack: 0.001, decay: 0.38, pan });
      sound.synth({ type: "square", tone: 3100 * pf, duration: 0.12, volume: 0.1 * v, attack: 0.001, decay: 0.11, pan });
      sound.synth({ type: "square", tone: 4600 * pf, duration: 0.1, volume: 0.08 * v, attack: 0.001, decay: 0.09, pan });
      break;
    case "c#": // crash
      sound.synth({ type: "noise", tone: 3500 * pf, duration: 0.55, volume: 0.42 * v, attack: 0.001, decay: 0.53, pan });
      sound.synth({ type: "noise", tone: 6500 * pf, duration: 0.45, volume: 0.28 * v, attack: 0.002, decay: 0.43, pan });
      sound.synth({ type: "square", tone: 4200 * pf, duration: 0.2, volume: 0.08 * v, attack: 0.001, decay: 0.19, pan });
      break;
    case "d#": // splash
      sound.synth({ type: "noise", tone: 5500 * pf, duration: 0.3, volume: 0.38 * v, attack: 0.001, decay: 0.29, pan });
      sound.synth({ type: "noise", tone: 8500 * pf, duration: 0.22, volume: 0.25 * v, attack: 0.001, decay: 0.21, pan });
      break;
    case "f#": // cowbell — detuned square pair
      sound.synth({ type: "square", tone: 810 * pf, duration: 0.12, volume: 0.22 * v, attack: 0.001, decay: 0.11, pan });
      sound.synth({ type: "square", tone: 540 * pf, duration: 0.15, volume: 0.18 * v, attack: 0.001, decay: 0.14, pan });
      break;
    case "g#": // wood block
      sound.synth({ type: "triangle", tone: 900 * pf, duration: 0.06, volume: 0.35 * v, attack: 0.001, decay: 0.05, pan });
      sound.synth({ type: "square", tone: 1800 * pf, duration: 0.03, volume: 0.14 * v, attack: 0.0005, decay: 0.025, pan });
      break;
    case "a#": // tambourine
      sound.synth({ type: "noise", tone: 7000 * pf, duration: 0.15, volume: 0.3 * v, attack: 0.001, decay: 0.14, pan });
      sound.synth({ type: "noise", tone: 4500 * pf, duration: 0.1, volume: 0.18 * v, attack: 0.001, decay: 0.09, pan });
      sound.synth({ type: "square", tone: 6500 * pf, duration: 0.05, volume: 0.1 * v, attack: 0.001, decay: 0.045, pan });
      break;
  }
}

function flashPercussionNotice(text) {
  percussionNotice = { text, until: frame + 120 }; // ~2 seconds at 60fps
}

function noteToFreq(note, oct) {
  const idx = CHROMATIC.indexOf(note);
  if (idx < 0) return 440;
  return 440 * Math.pow(2, (oct - 4) + (idx - 9) / 12);
}

function noteToMidiNumber(note, oct) {
  const idx = CHROMATIC.indexOf(note);
  if (idx < 0) return 60;
  return Math.max(0, Math.min(127, (oct + 1) * 12 + idx));
}

function velocityToMidi(velocity) {
  const v = Number.isFinite(velocity) ? velocity : 1;
  return Math.max(1, Math.min(127, Math.round(v * 127)));
}

function midiNoteLabel(note, oct) {
  return note.toUpperCase() + oct;
}

function pushUsbMidiRecent(prefix, note, oct) {
  usbMidiRecent.unshift({ text: prefix + midiNoteLabel(note, oct), until: frame + 180 });
  if (usbMidiRecent.length > 6) usbMidiRecent.length = 6;
}

function readUsbMidiStatus(system) {
  try {
    return system?.usbMidi?.status?.() || system?.usbMidi || { enabled: false, active: false, reason: "uninitialized" };
  } catch (_) {
    return system?.usbMidi || { enabled: false, active: false, reason: "uninitialized" };
  }
}

function activeUsbMidiNotes() {
  return Object.values(sounds)
    .filter((entry) => entry?.midiNote !== undefined)
    .sort((a, b) => (a.midiNote || 0) - (b.midiNote || 0))
    .map((entry) => midiNoteLabel(entry.note, entry.octave));
}

function usbMidiStatusText(status) {
  const notes = activeUsbMidiNotes();
  const recent = usbMidiRecent
    .filter((entry) => entry.until > frame)
    .map((entry) => entry.text);

  if (status?.active) {
    if (notes.length > 0) return "USB MIDI ON " + notes.slice(0, 3).join(",");
    if (recent.length > 0) return "USB MIDI ON " + recent.slice(0, 2).join(" ");
    return "USB MIDI ON";
  }
  return "USB MIDI OFF";
}

function loadUdpMidiConfig(system) {
  try {
    const raw = system?.readFile?.("/mnt/config.json");
    if (!raw) {
      udpMidiBroadcast = false;
      return;
    }
    const cfg = JSON.parse(raw);
    udpMidiBroadcast = cfg.udpMidiBroadcast === true || cfg.udpMidiBroadcast === "true";
  } catch (_) {
    udpMidiBroadcast = false;
  }
}

function sendUdpMidiEvent(system, event, midiNote, velocity, channel = 0) {
  if (!udpMidiBroadcast || !system?.udp?.connected) return;
  system?.udp?.sendMidi?.(event, midiNote, velocity, channel, "notepat");
}

function maybeSendUdpMidiHeartbeat(system) {
  if (!udpMidiBroadcast || !system?.udp?.connected) return;
  if (frame < udpMidiNextHeartbeatFrame) return;
  system?.udp?.sendMidiHeartbeat?.("notepat");
  udpMidiNextHeartbeatFrame = frame + 300;
}

function udpMidiRelayStatusText(system) {
  if (!udpMidiBroadcast) return "";
  const handle = system?.udp?.handle || system?.config?.handle || "";
  if (system?.udp?.connected) return handle ? "relay @" + handle : "relay on";
  return handle ? "relay ...@" + handle : "relay ...";
}

function rememberSound(key, entry, system, velocity = 1) {
  if (!entry) return;
  entry.midiNote = noteToMidiNumber(entry.note, entry.octave);
  entry.midiChannel = 0;
  sounds[key] = entry;
  // Auto-add to hold if F11 latch is active, UNLESS the F11 key is currently
  // physically held down — in that "flourish" mode we want to play melodic
  // riffs over the already-latched chord without sticking the new notes.
  if (holdActive && !f11Held) heldKeys.add(key);
  system?.usbMidi?.noteOn?.(entry.midiNote, velocityToMidi(velocity), entry.midiChannel);
  sendUdpMidiEvent(system, "note_on", entry.midiNote, velocityToMidi(velocity), entry.midiChannel);
  pushUsbMidiRecent(">", entry.note, entry.octave);
}

function stopSoundKey(key, sound, system, fade = 0.08) {
  const entry = sounds[key];
  if (!entry) return;
  if (entry.compositeVoices) {
    for (const voice of entry.compositeVoices) sound?.kill?.(voice, fade);
  } else {
    sound?.kill?.(entry.synth || entry, fade);
  }
  if (entry.midiNote !== undefined) {
    system?.usbMidi?.noteOff?.(entry.midiNote, 0, entry.midiChannel || 0);
    sendUdpMidiEvent(system, "note_off", entry.midiNote, 0, entry.midiChannel || 0);
    pushUsbMidiRecent("<", entry.note, entry.octave);
  }
  delete sounds[key];
}

function stopAllSounds(sound, system, fade = 0.08) {
  heldKeys.clear();
  holdActive = false;
  for (const key of Object.keys(sounds)) stopSoundKey(key, sound, system, fade);
  touchNotes = {};
  system?.usbMidi?.allNotesOff?.(0);
  sounds = {};
}

function noteColor(n) { return NOTE_COLORS[n] || [80, 80, 80]; }

// Hit-test a touch point against the note grid
function hitTestGrid(x, y, gi) {
  const grids = [
    { grid: LEFT_GRID, startX: gi.leftX, octOffset: 0 },
    { grid: RIGHT_GRID, startX: gi.rightX, octOffset: 0 },
  ];
  for (const { grid, startX, octOffset } of grids) {
    for (let r = 0; r < 3; r++) {
      for (let c = 0; c < 4; c++) {
        const bx = startX + c * (gi.btnW + gi.gap);
        const by = gi.gridTop + r * (gi.btnH + gi.gap);
        if (x >= bx && x < bx + gi.btnW && y >= by && y < by + gi.btnH) {
          const noteName = grid[r][c];
          const key = NOTE_TO_KEY[noteName];
          const [letter, off] = parseNote(noteName);
          return { key, letter, octave: octave + off + octOffset, gridOffset: off };
        }
      }
    }
  }
  return null;
}

// Play a short identifying blip in the new wave type when switching
function playWaveSound(sound, waveType) {
  if (!sound?.synth) return;
  if (waveType === "sample") {
    // Short percussive click for sample mode
    sound.synth({ type: "noise", tone: 800, duration: 0.03, volume: 0.12, attack: 0.001, decay: 0.025, pan: 0 });
    return;
  }
  const tones = { sine: 660, triangle: 550, sawtooth: 440, square: 330, noise: 220, whistle: 880 };
  sound.synth({
    type: waveType === "noise" ? "noise" : waveType,
    tone: tones[waveType] || 440,
    duration: 0.07, volume: 0.18,
    attack: 0.002, decay: 0.06, pan: 0,
  });
}

function stopSampleRecording(sound, reason = "stop") {
  if (!recording) return 0;
  recording = false;
  recPointerId = null;
  const len = sound?.microphone?.cut?.() || 0;
  sampleLoaded = len > 0;
  const mic = sound?.microphone || {};
  const secs = (mic.sampleRate > 0 && len > 0) ? (len / mic.sampleRate).toFixed(2) : "0.00";
  console.log(`[sample] ${reason}: len=${len} rate=${mic.sampleRate || 0} secs=${secs} err=${mic.lastError || ""}`);
  return len;
}

function setWave(nextWave, sound) {
  if (!nextWave || wave === nextWave) return;
  const prev = wave;
  if (prev === "sample" && recording) stopSampleRecording(sound, "wave-exit");
  wave = nextWave;
  waveIndex = wavetypes.indexOf(nextWave);
  if (waveIndex < 0) waveIndex = 0;
  // Announce wave type
  sound?.speak?.(nextWave === "composite" ? "composite" : nextWave);

  if (wave === "sample") {
    const mic = sound?.microphone || {};
    sampleLoaded = (mic.sampleLength || 0) > 0;
    // Open hot-mic so device stays ready — recording is instant after this.
    // Always call open(); C side is idempotent if already hot.
    sound?.microphone?.open?.();
    playWaveSound(sound, wave);
    console.log(`[sample] wave-enter: loaded=${sampleLoaded} len=${mic.sampleLength || 0} rate=${mic.sampleRate || 0} connected=${!!mic.connected} hot=${!!mic.hot} device=${mic.device || "none"} err=${mic.lastError || ""}`);
  } else {
    // Close hot-mic when leaving sample mode to free the device
    if (prev === "sample") sound?.microphone?.close?.();
    playWaveSound(sound, wave);
  }
}

function scheduleAutoUpdateCheck(delayFrames = 0) {
  autoUpdate.state = "checking";
  autoUpdate.fetchPending = false;
  autoUpdate.nextCheckFrame = frame + Math.max(0, delayFrames | 0);
}

// Version format is "<git-hash>-YYYY-MM-DDTHH:MM".
// We only treat remote as update when its timestamp is strictly newer.
function parseVersionStamp(version) {
  const raw = (version || "").trim();
  if (!raw) return { raw: "", stamp: "", ms: NaN };
  const m = raw.match(/(\d{4}-\d{2}-\d{2}T\d{2}:\d{2})$/);
  if (!m) return { raw, stamp: "", ms: NaN };
  const stamp = m[1];
  const ms = Date.parse(stamp + ":00Z");
  return { raw, stamp, ms };
}

function isRemoteVersionNewer(remoteVersion, localVersion) {
  const remote = parseVersionStamp(remoteVersion);
  const local = parseVersionStamp(localVersion);
  if (!isNaN(remote.ms) && !isNaN(local.ms)) return remote.ms > local.ms;
  if (!isNaN(remote.ms) && isNaN(local.ms)) return true;
  return false; // conservative: unknown formats do not trigger "update available"
}

function notifyUpdateAvailable(sound, version) {
  if (!version || osUpdatePingVersion === version) return;
  osUpdatePingVersion = version;
  sound?.synth?.({ type: "sine", tone: 988, duration: 0.08, volume: 0.16, attack: 0.002, decay: 0.06 });
  sound?.synth?.({ type: "sine", tone: 1319, duration: 0.12, volume: 0.14, attack: 0.01, decay: 0.08 });
  sound?.speak?.("os update available");
}

function startAutoUpdateDownload(system) {
  autoUpdate.state = "downloading";
  autoUpdate.lastError = "";
  osProgress = 0;
  system.fetchBinary?.(OS_VMLINUZ_URL, "/tmp/vmlinuz.new", OS_VMLINUZ_BYTES);
}

function boot({ wipe, system, sound }) {
  wipe(0);
  soundAPI = sound;
  systemAPI = system;
  loadUdpMidiConfig(system);
  udpMidiNextHeartbeatFrame = 0;
  const mic = sound?.microphone || null;
  if (mic && (mic.sampleLength || 0) > 0) {
    sampleLoaded = true;
    console.log(`[sample] boot: preloaded len=${mic.sampleLength} rate=${mic.sampleRate || 0} device=${mic.device || "none"}`);
  }
  // Load saved credentials from USB EFI partition
  if (system?.readFile) {
    try {
      const raw = system.readFile(CREDS_PATH);
      if (raw) savedCreds = JSON.parse(raw);
    } catch (_) {}
  }
  // Capture current version for OS panel
  osCurrentVersion = system?.version || "unknown";

  // Mount music USB + scan for deck tracks. If a track is already loaded
  // in the global deck (e.g. resumed from dj.mjs), leave it be; otherwise
  // auto-load the first discovered track so users can jam immediately.
  djMounted = system?.mountMusic?.() || false;
  djUsbConnected = djMounted;
  djScan(system, sound);
  const dk0 = sound?.deck?.decks?.[0];
  if (dk0?.loaded) {
    djMsg("resumed");
  } else if (djFiles.length > 0) {
    djLoadTrack(sound);
  }
}

function act({ event: e, sound, wifi, system }) {
  soundAPI = sound;
  systemAPI = system;
  // Track shift state before any other handling
  if (e.is("keyboard:down") && e.key?.toLowerCase() === "shift") shiftHeld = true;
  if (e.is("keyboard:up") && e.key?.toLowerCase() === "shift") shiftHeld = false;

  // WiFi password input mode — fullscreen, capture all keyboard
  if (wifiPasswordMode && e.is("keyboard:down")) {
    const key = e.key?.toLowerCase();
    if (key === "escape") { wifiPasswordMode = false; wifiPassword = ""; wifiSelectedIdx = -1; return; }
    if (key === "enter") {
      if (wifi && wifiSelectedIdx >= 0) {
        const nets = wifi.networks || [];
        const net = nets[wifiSelectedIdx];
        if (net) {
          wifi.connect(net.ssid, wifiPassword);
          // Save credentials persistently (skip duplicates)
          const already = savedCreds.find((c) => c.ssid === net.ssid);
          if (!already) savedCreds.push({ ssid: net.ssid, pass: wifiPassword });
          else already.pass = wifiPassword;
        }
      }
      wifiPasswordMode = false;
      activeScreen = "notepat";
      wifiSelectedIdx = -1;
      return;
    }
    if (key === "backspace") { wifiPassword = wifiPassword.slice(0, -1); return; }
    if (key.length === 1) {
      wifiPassword += shiftHeld ? (SHIFT_MAP[key] ?? key.toUpperCase()) : key;
      return;
    }
    return;
  }

  // WiFi password mode — block touch from reaching note grid
  if (wifiPasswordMode && (e.is("touch") || e.is("draw") || e.is("lift"))) {
    return;
  }

  if (e.is("keyboard:down")) {
    const key = e.key?.toLowerCase();
    if (!key) return;
    // Note: we do NOT alias media-key scancodes (audiomute, audiovolumedown,
    // brightnessdown, etc.) to F-keys anymore. Different laptops place those
    // media functions on different top-row positions (e.g. X1 Nano puts mute
    // on F1 and volume-down on F2), so aliasing them to f10/f11/f12 caused
    // F1/F2 to incorrectly trigger hold/latch. Users can still reach
    // F1-F12 by holding Fn. If a specific machine needs bare-key access we'll
    // remap in input.c, not here.

    // === DJ DECK KEYS ===
    // F1 play/pause, F2 next, F3 prev, F4 scan+mount USB. Also [ = speed−,
    // ` = reset speed. Tap tempo on `,` (unused elsewhere in notepat).
    if (key === "f1") { djTogglePlay(sound); return; }
    if (key === "f2") { djTrackIdx++; djLoadTrack(sound); return; }
    if (key === "f3") { djTrackIdx = (djTrackIdx - 1 + Math.max(1, djFiles.length)) % Math.max(1, djFiles.length); djLoadTrack(sound); return; }
    if (key === "f4") {
      djMounted = system?.mountMusic?.() || false;
      djUsbConnected = djMounted;
      djScan(system, sound);
      if (djFiles.length > 0) { djTrackIdx = 0; djLoadTrack(sound); }
      return;
    }
    if (key === "[") {
      const d = sound?.deck?.decks?.[0];
      if (d?.loaded) {
        const s = Math.max(-2, (d.speed || 1) - 0.05);
        sound.deck.setSpeed(0, s);
        djMsg(`${s.toFixed(2)}x`);
      }
      return;
    }
    if (key === "`") {
      if (sound?.deck?.decks?.[0]?.loaded) {
        sound.deck.setSpeed(0, 1);
        djMsg("1.00x");
      }
      return;
    }
    if (key === ",") { djTapTempo(); return; }

    if (key === "escape" && activeScreen === "wifi") { activeScreen = "notepat"; return; }
    if (key === "escape" && activeScreen === "os") { activeScreen = "notepat"; osState = "idle"; return; }
    if (key === "escape" && activeScreen === "notepat") {
      // Triple-escape to exit: 3 presses within 90 frames (~1.5s)
      if (frame - escLastFrame > 90) escCount = 0;
      escCount++;
      escLastFrame = frame;
      if (escCount === 1) {
        // First escape: low warning tone + spoken count
        const beep = sound?.synth?.("sine", { tone: 220, duration: 0.15, volume: 0.18, attack: 0.005, decay: 0.14 });
        if (beep) setTimeout(() => sound?.kill?.(beep, 0.02), 150);
        sound?.speak?.("one");
        return;
      }
      if (escCount === 2) {
        // Second escape: rising warning, two notes
        const a = sound?.synth?.("sine", { tone: 330, duration: 0.1, volume: 0.2, attack: 0.005, decay: 0.09 });
        if (a) setTimeout(() => sound?.kill?.(a, 0.02), 100);
        setTimeout(() => {
          const b = sound?.synth?.("sine", { tone: 440, duration: 0.1, volume: 0.2, attack: 0.005, decay: 0.09 });
          if (b) setTimeout(() => sound?.kill?.(b, 0.02), 100);
        }, 80);
        sound?.speak?.("two");
        return;
      }
      // Third escape — descending exit chord + spoken "exit"
      const c1 = sound?.synth?.("sine", { tone: 660, duration: 0.12, volume: 0.22, attack: 0.005, decay: 0.11 });
      if (c1) setTimeout(() => sound?.kill?.(c1, 0.02), 120);
      setTimeout(() => {
        const c2 = sound?.synth?.("sine", { tone: 523, duration: 0.12, volume: 0.22, attack: 0.005, decay: 0.11 });
        if (c2) setTimeout(() => sound?.kill?.(c2, 0.02), 120);
      }, 80);
      setTimeout(() => {
        const c3 = sound?.synth?.("sine", { tone: 392, duration: 0.18, volume: 0.25, attack: 0.005, decay: 0.17 });
        if (c3) setTimeout(() => sound?.kill?.(c3, 0.02), 180);
      }, 160);
      sound?.speak?.("exit");
      escCount = 0;
      stopAllSounds(sound, system, 0.05);
      system?.jump?.("prompt");
      return;
    }
    if (key === "shift") { quickMode = !quickMode; return; }
    if (key === "tab") {
      const idx = (waveIndex + 1) % wavetypes.length;
      setWave(wavetypes[idx], sound);
      return;
    }
    if (key === "space") {
      // Kick drum — short sine burst with pitch drop
      if (sound && sound.synth) {
        sound.synth({ type: "sine", tone: 150, duration: 0.15, volume: 0.9, attack: 0.001, decay: 0.14, pan: 0.0 });
        sound.synth({ type: "sine", tone: 60, duration: 0.2, volume: 0.7, attack: 0.001, decay: 0.19, pan: 0.0 });
      }
      return;
    }
    // F11 (green phone pickup): Engage hold/latch
    // - Press snapshots currently-playing keys into heldKeys + enables hold
    // - While physically held, f11Held suppresses auto-latch so new notes
    //   become flourishes over the held chord instead of sticking
    if (key === "f11") {
      // Only re-snapshot on the first press (ignore autorepeat while held)
      if (!f11Held) {
        heldKeys = new Set(Object.keys(sounds));
        holdActive = true;
        sound?.synth?.({ type: "sine", tone: 660, duration: 0.06, volume: 0.12, attack: 0.002, decay: 0.05 });
      }
      f11Held = true;
      return;
    }
    // F10 (red phone hangup): Clear hold — stop all held notes
    if (key === "f10") {
      if (holdActive) {
        for (const k of heldKeys) stopSoundKey(k, sound, system, 0.08);
        heldKeys.clear();
        holdActive = false;
        sound?.synth?.({ type: "sine", tone: 330, duration: 0.06, volume: 0.12, attack: 0.002, decay: 0.05 });
      }
      return;
    }
    // F9: metronome toggle (no Fn key required)
    if (key === "f9") {
      metronomeEnabled = !metronomeEnabled;
      if (metronomeEnabled) {
        metronomeBeatCount = Math.floor(syncedNow() / (60000 / metronomeBPM));
      }
      return;
    }
    // PgUp: toggle percussion layout on the LEFT octave grid
    if (key === "pageup") {
      percussionLeft = !percussionLeft;
      const label = percussionLeft ? "percussion left on" : "percussion left off";
      flashPercussionNotice(percussionLeft ? "◀ DRUMS ON" : "◀ drums off");
      sound?.speak?.(label);
      // Rising / falling two-tone feedback
      sound?.synth?.({ type: "triangle", tone: percussionLeft ? 440 : 660, duration: 0.08, volume: 0.18, attack: 0.002, decay: 0.07, pan: -0.6 });
      setTimeout(() => sound?.synth?.({
        type: "triangle", tone: percussionLeft ? 660 : 440,
        duration: 0.1, volume: 0.18, attack: 0.002, decay: 0.09, pan: -0.6,
      }), 70);
      if (percussionLeft) playPercussion(sound, "c", 1.6, -0.4);
      return;
    }
    // PgDn: toggle percussion layout on the RIGHT octave grid
    if (key === "pagedown") {
      percussionRight = !percussionRight;
      const label = percussionRight ? "percussion right on" : "percussion right off";
      flashPercussionNotice(percussionRight ? "DRUMS ON ▶" : "drums off ▶");
      sound?.speak?.(label);
      sound?.synth?.({ type: "triangle", tone: percussionRight ? 440 : 660, duration: 0.08, volume: 0.18, attack: 0.002, decay: 0.07, pan: 0.6 });
      setTimeout(() => sound?.synth?.({
        type: "triangle", tone: percussionRight ? 660 : 440,
        duration: 0.1, volume: 0.18, attack: 0.002, decay: 0.09, pan: 0.6,
      }), 70);
      if (percussionRight) playPercussion(sound, "c", 1.6, 0.4);
      return;
    }
    // F12 (star key): recital mode — hide UI, show only colored backdrops
    if (key === "f12") {
      recitalMode = !recitalMode;
      sound?.synth?.({
        type: "sine",
        tone: recitalMode ? 880 : 440,
        duration: 0.12, volume: 0.18, attack: 0.005, decay: 0.11
      });
      return;
    }
    // Meta/Win key: toggle keyboard shortcut help panel
    if (key === "meta") {
      helpPanel = !helpPanel;
      sound?.synth?.({
        type: "sine",
        tone: helpPanel ? 660 : 440,
        duration: 0.08, volume: 0.15, attack: 0.005, decay: 0.07
      });
      return;
    }
    if (key >= "1" && key <= "9") { octave = parseInt(key); return; }
    if (key === "arrowup") { octave = Math.min(9, octave + 1); return; }
    if (key === "arrowdown") { octave = Math.max(1, octave - 1); return; }
    // Home key: hold to record GLOBAL sample
    if (key === "home" && wave === "sample" && !recording && !perKeyRecording) {
      const ok = !!sound?.microphone?.rec?.();
      recording = ok;
      recPointerId = null;
      if (ok) recStartTime = Date.now();
      console.log(`[mic] rec-home: ok=${ok}`);
      return;
    }
    // End key: arm per-key recording mode
    if (key === "end" && wave === "sample") {
      endArmed = true;
      console.log(`[sample-bank] armed for per-key recording`);
      return;
    }
    // Delete key: clear all per-key samples, reset to global/default
    if (key === "delete" && wave === "sample") {
      sampleBank = {};
      console.log(`[sample-bank] cleared all per-key samples`);
      // Restore global sample if we have one
      if (globalSample) {
        sound.sample.loadData(globalSample.data, globalSample.rate);
        sampleLoaded = true;
      }
      sound.synth({ type: "noise", tone: 200, duration: 0.1, volume: 0.15, attack: 0.001, decay: 0.08 });
      return;
    }
    if (key === "arrowleft") {
      const idx = (waveIndex - 1 + wavetypes.length) % wavetypes.length;
      setWave(wavetypes[idx], sound);
      return;
    }
    if (key === "arrowright") {
      const idx = (waveIndex + 1) % wavetypes.length;
      setWave(wavetypes[idx], sound);
      return;
    }
    if (key === "\\") {
      trackpadFX = !trackpadFX;
      sound.synth({
        type: "sine", tone: trackpadFX ? 880 : 440,
        duration: 0.06, volume: 0.2,
        attack: 0.002, decay: 0.05, pan: 0,
      });
      // FX on: bright chord; FX off: hollow low note
      if (trackpadFX) sound?.synth({ type: "triangle", tone: 1320, duration: 0.08, volume: 0.15, attack: 0.002, decay: 0.06 });
      return;
    }
    if (key === "-") {
      metronomeBPM = Math.max(20, metronomeBPM - 5);
      metronomeBeatCount = Math.floor(syncedNow() / (60000 / metronomeBPM));
      return;
    }
    if (key === "=") {
      metronomeBPM = Math.min(300, metronomeBPM + 5);
      metronomeBeatCount = Math.floor(syncedNow() / (60000 / metronomeBPM));
      return;
    }

    const noteName = KEY_TO_NOTE[key];
    // Per-key recording: End armed + tone key = record to that key
    if (noteName && endArmed && wave === "sample" && !perKeyRecording && !recording) {
      const ok = !!sound?.microphone?.rec?.();
      if (ok) {
        perKeyRecording = key;
        recStartTime = Date.now();
        console.log(`[sample-bank] recording to key '${key}' (${noteName})`);
      }
      return; // Suppress note playback while recording
    }
    // If this key is currently recording in per-key mode, suppress playback
    if (perKeyRecording === key) return;
    if (noteName && !sounds[key]) {
      const [letter, offset] = parseNote(noteName);
      const noteOctave = octave + offset;
      const freq = noteToFreq(letter, noteOctave);
      const semitones = (noteOctave - 4) * 12 + CHROMATIC.indexOf(letter);
      const pan = Math.max(-0.8, Math.min(0.8, (semitones - 12) / 15));

      // Start at current pressure (or full for digital keys)
      const velocity = e.pressure > 0 ? e.pressure : 1.0;
      // Per-side master mix — scales every note and drum on this grid.
      const master = masterForSide(offset);
      const baseVol = 0.15 + velocity * 0.55;  // un-mastered; sim() re-applies master live
      const vol = baseVol * master;
      const playFreq = freq * Math.pow(2, effectivePitchShift());

      // === PERCUSSION MODE ===
      // If this grid side has percussion enabled, play a drum hit instead
      // of a melodic tone. Drums are one-shots so we don't register them
      // in `sounds[key]`; the visual flash is driven purely by `trail`.
      const drumName = percussionDrumFor(letter, offset);
      if (drumName) {
        // Per-drum sampling: End armed + drum key = record to this drum slot.
        if (wave === "sample" && endArmed && !perKeyRecording && !recording) {
          const ok = !!sound?.microphone?.rec?.();
          if (ok) {
            perKeyRecording = key;
            recStartTime = Date.now();
            console.log(`[perc-bank] recording to drum '${drumName}' (key '${key}')`);
          }
          return;
        }
        if (perKeyRecording === key) return;

        // Push drums well above unity so they cut through the mix. The
        // mixer divides by total voice weight, so at velocity 1.0 we want
        // a drum-layer peak near ~1.8 so that against 3 sustained
        // melodic voices (~2.1 weight) the kick still lands at ~0.5 vs
        // each melody at ~0.17 — roughly 3× prominence. Then scale the
        // whole thing by the per-side master so the user can balance L/R.
        const drumVol = (1.0 + velocity * 0.8) * master;
        const bankSample = percussionSampleBank[drumName];
        if (wave === "sample" && bankSample) {
          if (bankSample !== lastLoadedSample) {
            sound.sample.loadData(bankSample.data, bankSample.rate);
            lastLoadedSample = bankSample;
          }
          sound.sample.play({
            tone: SAMPLE_BASE_FREQ * Math.pow(2, effectivePitchShift()),
            base: SAMPLE_BASE_FREQ,
            volume: drumVol, pan, loop: false,
          });
        } else {
          playPercussion(sound, letter, drumVol, pan, Math.pow(2, effectivePitchShift()));
        }
        trail[key] = { note: letter, octave: noteOctave, brightness: velocity };
        return;
      }

      if (wave === "sample" && (sampleLoaded || sampleBank[key])) {
        // Only reload sample data if switching to a different sample
        const targetSample = sampleBank[key] || globalSample;
        if (targetSample && targetSample !== lastLoadedSample) {
          sound.sample.loadData(targetSample.data, targetSample.rate);
          lastLoadedSample = targetSample;
        }
        const smp = sound.sample.play({
          tone: playFreq, base: SAMPLE_BASE_FREQ, volume: vol, pan, loop: true,
        });
        if (smp) {
          rememberSound(key, { synth: smp, note: letter, octave: noteOctave, baseFreq: freq, isSample: true, gridOffset: offset, baseVol }, system, velocity);
        } else {
          const synth = sound.synth({
            type: "sine", tone: playFreq, duration: Infinity,
            volume: vol, attack: quickMode ? 0.002 : 0.005, decay: 0.1, pan,
          });
          rememberSound(key, { synth, note: letter, octave: noteOctave, baseFreq: freq, gridOffset: offset, baseVol }, system, velocity);
        }
      } else {
        const synth = sound.synth({
          type: wave, tone: playFreq,
          duration: Infinity,
          volume: vol, attack: quickMode ? 0.002 : 0.005,
          decay: 0.1, pan: pan,
        });
        rememberSound(key, { synth, note: letter, octave: noteOctave, baseFreq: freq, gridOffset: offset, baseVol }, system, velocity);
      }
      trail[key] = { note: letter, octave: noteOctave, brightness: velocity };
    }
  }

  if (e.is("keyboard:up")) {
    const key = e.key?.toLowerCase();
    if (!key) return;
    // F11 release — exit flourish mode (new notes will auto-latch again)
    if (key === "f11") { f11Held = false; return; }
    // Home key release: stop global recording + save to global sample
    if (key === "home" && recording && recPointerId === null) {
      stopSampleRecording(sound, "home-lift");
      // Save global sample data for bank restore
      const data = sound.sample.getData?.();
      if (data && data.length > 0) {
        globalSample = { data: new Float32Array(data), len: data.length, rate: sound.microphone?.sampleRate || 48000 };
        lastLoadedSample = null;  // force reload on next key press
        console.log(`[sample-bank] global sample saved (${data.length} samples)`);
      }
      return;
    }
    // End key release: disarm per-key recording
    if (key === "end") {
      endArmed = false;
      console.log(`[sample-bank] disarmed`);
      return;
    }
    // Per-key recording stop: tone key released while recording to it
    if (perKeyRecording && key === perKeyRecording) {
      const len = sound?.microphone?.cut?.() || 0;
      if (len > 0) {
        const data = sound.sample.getData?.();
        if (data && data.length > 0) {
          const rate = sound.microphone?.sampleRate || 48000;
          // If this key was recorded while its grid side had percussion on,
          // save into the drum bank instead of the melodic sample bank.
          const recNoteName = KEY_TO_NOTE[key];
          const [recLetter, recOffset] = recNoteName ? parseNote(recNoteName) : ["", 0];
          const recDrum = recLetter ? percussionDrumFor(recLetter, recOffset) : null;
          if (recDrum) {
            percussionSampleBank[recDrum] = { data: new Float32Array(data), len: data.length, rate };
            console.log(`[perc-bank] saved ${data.length} samples to drum '${recDrum}'`);
          } else {
            sampleBank[key] = { data: new Float32Array(data), len: data.length, rate };
            console.log(`[sample-bank] saved ${data.length} samples to key '${key}'`);
          }
          sampleLoaded = true;
          // Confirmation beep
          sound.synth({ type: "sine", tone: 660, duration: 0.05, volume: 0.15, attack: 0.002, decay: 0.04 });
        }
      }
      perKeyRecording = null;
      return;
    }
    if (sounds[key]) {
      // Don't stop held/latched notes on key release
      if (heldKeys.has(key)) return;
      stopSoundKey(key, sound, system, quickMode ? 0.02 : 0.08);
    }
  }

  // Touch interactions
  if (e.is("touch")) {
    const y = e.pointer?.y ?? e.y ?? 0;
    const x = e.pointer?.x ?? e.x ?? 0;
    const w = globalThis.__screenW || 320;
    const h = globalThis.__screenH || 200;
    const pid = e.pointer?.id ?? 0;
    hoverX = x; hoverY = y;

    // "notepat.com" label: no longer tappable (use triple-escape to exit)

    // OS panel touch handling
    if (activeScreen === "os" && y > 14) {
      if (osState === "available") {
        // Cycle target button
        const cb = globalThis.__osCycleBtn;
        if (cb && y >= cb.y && y <= cb.y + cb.h && x >= cb.x && x <= cb.x + cb.w) {
          const targets = system?.flashTargets || [];
          flashTargetIdx = (flashTargetIdx + 1) % Math.max(1, targets.length);
          sound?.synth({ type: "sine", tone: 440, duration: 0.04, volume: 0.12, attack: 0.002, decay: 0.03 });
          return;
        }
        // "update now" button
        const ub = globalThis.__osUpdateBtn;
        if (ub && y >= ub.y && y <= ub.y + ub.h && x >= ub.x && x <= ub.x + ub.w) {
          const targets = system?.flashTargets || [];
          const tgt = targets[flashTargetIdx];
          const device = tgt?.device || undefined;
          console.log(`[os] manual download: ${OS_VMLINUZ_URL} -> /tmp/vmlinuz.new target=${device || "auto"}`);
          globalThis.__osFlashDevice = device;
          osState = "downloading";
          osProgress = 0;
          system?.fetchBinary?.(
            OS_VMLINUZ_URL,
            "/tmp/vmlinuz.new",
            OS_VMLINUZ_BYTES
          );
        }
      }
      return; // don't fall through to note grid
    }

    // Volume bar drag
    const vb = globalThis.__volBar;
    if (y < 16 && vb && x >= vb.x && x <= vb.x + vb.w) {
      volDragging = true;
      const pct = Math.max(0, Math.min(1, (x - vb.barX) / vb.barW));
      const target = Math.round(pct * 20); // 0-20 steps
      const cur = Math.round((sound?.speaker?.systemVolume ?? 50) / 5);
      const diff = target - cur;
      if (diff !== 0) system?.volumeAdjust?.(diff > 0 ? 1 : -1);
      return;
    }
    // Brightness bar drag
    const bb = globalThis.__brtBar;
    if (y < 16 && bb && x >= bb.x && x <= bb.x + bb.w) {
      brtDragging = true;
      const pct = Math.max(0, Math.min(1, (x - bb.barX) / bb.barW));
      const target = Math.round(pct * 20);
      const cur = Math.round((system?.brightness ?? 50) / 5);
      const diff = target - cur;
      if (diff !== 0) system?.brightnessAdjust?.(diff > 0 ? 1 : -1);
      return;
    }

    // WiFi fullscreen network list clicks (merged list)
    // First tap: select network. Second tap on same: connect.
    if (activeScreen === "wifi" && !wifiPasswordMode) {
      const merged = globalThis.__wifiMergedList || [];
      const rowH = 16;
      const listY = 44;
      const clickedRow = Math.floor((y - listY) / rowH);
      if (clickedRow >= 0 && clickedRow < merged.length) {
        const entry = merged[clickedRow];
        if (entry.type === "separator") return; // ignore separator clicks

        // First tap — just select
        if (wifiSelectedIdx !== clickedRow) {
          wifiSelectedIdx = clickedRow;
          return;
        }

        // Second tap on same row — connect
        if (entry.type === "saved") {
          wifi?.connect(entry.ssid, entry.pass);
          activeScreen = "notepat";
        } else if (entry.type === "scan") {
          const nets = wifi?.networks || [];
          const net = nets[entry.idx];
          const savedCred = savedCreds.find((c) => c.ssid === net.ssid) ||
            (net.ssid === AC_SSID ? { pass: AC_PASS } : null) ||
            null;
          if (savedCred) {
            wifi?.connect(net.ssid, savedCred.pass);
            activeScreen = "notepat";
          } else if (net.encrypted) {
            wifiPassword = "";
            wifiPasswordMode = true;
          } else {
            wifi?.connect(net.ssid, "");
            if (!savedCreds.find((c) => c.ssid === net.ssid))
              savedCreds.push({ ssid: net.ssid, pass: "" });
            activeScreen = "notepat";
          }
        }
      }
      return;
    }

    // FX mix slider zone (y 15-26)
    if (y >= 15 && y < 27) {
      fxDragging = true;
      fxMix = Math.max(0, Math.min(1, x / w));
      sound?.fx?.setMix?.(fxMix);
      return;
    }
    // Echo slider zone (y 27-38)
    if (y >= 27 && y < 39) {
      echoDragging = true;
      echoMix = Math.max(0, Math.min(1, x / w));
      sound?.room?.setMix?.(echoMix);
      return;
    }
    // Pitch slider zone (y 39-50)
    if (y >= 39 && y < 51) {
      pitchDragging = true;
      pitchShift = Math.max(-1, Math.min(1, (x / w) * 2 - 1));
      const factor = Math.pow(2, effectivePitchShift());
      for (const k of Object.keys(sounds)) {
        const s = sounds[k];
        if (s && s.synth && s.baseFreq) {
          if (s.isSample) s.synth.update({ tone: s.baseFreq * factor, base: SAMPLE_BASE_FREQ });
          else s.synth.update({ tone: s.baseFreq * factor });
        }
      }
      return;
    }
    // Per-side master volume sliders (vertical bars flanking the grids)
    {
      const ls = globalThis.__leftMasterSlider;
      const rs = globalThis.__rightMasterSlider;
      if (ls && x >= ls.x && x < ls.x + ls.w && y >= ls.y && y < ls.y + ls.h) {
        leftVolDragging = true;
        leftMasterVol = Math.max(0, Math.min(1, 1 - (y - ls.y) / ls.h));
        return;
      }
      if (rs && x >= rs.x && x < rs.x + rs.w && y >= rs.y && y < rs.y + rs.h) {
        rightVolDragging = true;
        rightMasterVol = Math.max(0, Math.min(1, 1 - (y - rs.y) / rs.h));
        return;
      }
    }
    // DJ deck strip: play/pause icon + scratch pad
    const ds = globalThis.__djStrip;
    if (ds && y >= ds.y && y < ds.y + ds.h) {
      if (x < ds.iconW) {
        // Tap play/pause icon
        djTogglePlay(sound);
      } else {
        // Begin scratch drag
        const dk = sound?.deck;
        const d = dk?.decks?.[0];
        if (d?.loaded) {
          djDragging = true;
          djDragWasPlaying = d.playing;
          djDragLastX = x;
          djScratchSpeed = 0;
          if (!d.playing) dk.play(0);
        }
      }
      return;
    }

    // Wave type buttons (y 51-64)
    const wb = globalThis.__waveButtons;
    if (wb && y >= wb.y && y < wb.y + wb.h) {
      if (x >= wb.octX) {
        if (wave === "sample") {
          // REC button — hold to record
          const ok = !!sound?.microphone?.rec?.();
          recording = ok;
          recPointerId = ok ? pid : null;
          if (ok) recStartTime = Date.now();
          const mic = sound?.microphone || {};
          console.log(`[mic] rec-touch: ok=${ok} connected=${!!mic.connected} device=${mic.device || "none"} err=${mic.lastError || ""}`);
          if (!ok) sound?.synth?.({ type: "sine", tone: 220, duration: 0.08, volume: 0.16, attack: 0.002, decay: 0.06 });
        } else {
          // Octave button — tap to increment
          octave = (octave % 6) + 1;
        }
      } else {
        const idx = Math.min(wavetypes.length - 1, Math.floor(x / wb.btnW));
        setWave(wavetypes[idx], sound);
      }
      return;
    }

    // Grid button tap — check if touch lands on a note button
    const gridInfo = globalThis.__gridInfo;
    if (gridInfo) {
      const hitNote = hitTestGrid(x, y, gridInfo);
      if (hitNote && !sounds[hitNote.key]) {
        const freq = noteToFreq(hitNote.letter, hitNote.octave);
        const semitones = (hitNote.octave - 4) * 12 + CHROMATIC.indexOf(hitNote.letter);
        const pan = Math.max(-0.8, Math.min(0.8, (semitones - 12) / 15));
        const playFreq = freq * Math.pow(2, effectivePitchShift());
        // Percussion pad tap: fire drum (or drum sample) and flash trail.
        const touchDrum = percussionDrumFor(hitNote.letter, hitNote.gridOffset);
        if (touchDrum) {
          const bankSample = percussionSampleBank[touchDrum];
          if (wave === "sample" && bankSample) {
            if (bankSample !== lastLoadedSample) {
              sound.sample.loadData(bankSample.data, bankSample.rate);
              lastLoadedSample = bankSample;
            }
            sound.sample.play({
              tone: SAMPLE_BASE_FREQ * Math.pow(2, effectivePitchShift()),
              base: SAMPLE_BASE_FREQ,
              volume: 1.6, pan, loop: false,
            });
          } else {
            playPercussion(sound, hitNote.letter, 1.8, pan, Math.pow(2, effectivePitchShift()));
          }
          trail[hitNote.key] = { note: hitNote.letter, octave: hitNote.octave, brightness: 1.0 };
          touchNotes[pid] = { key: hitNote.key };
          return;
        }
        let synth;
        if (wave === "sample" && sampleLoaded) {
          const smp = sound.sample.play({
            tone: playFreq, base: SAMPLE_BASE_FREQ, volume: 0.5, pan, loop: true,
          });
          if (smp) {
            synth = smp;
            rememberSound(hitNote.key, { synth, note: hitNote.letter, octave: hitNote.octave, baseFreq: freq, isSample: true }, system, 1);
          }
        } else if (wave === "composite") {
          // Rich layered pad: 5 detuned oscillators
          const detune = () => Math.floor(Math.random() * 13) - 6;
          const a = sound.synth({ type: "sine", tone: playFreq, duration: Infinity, volume: 0.5, attack: 0.003, decay: 0.9, pan });
          const b = sound.synth({ type: "sine", tone: playFreq + 9 + detune(), duration: Infinity, volume: 0.17, attack: 0.003, decay: 0.9, pan });
          const c = sound.synth({ type: "sawtooth", tone: playFreq + detune(), duration: 0.15 + Math.random() * 0.05, volume: 0.01, attack: 0.005, decay: 0.1, pan });
          const d = sound.synth({ type: "triangle", tone: playFreq + 8 + detune(), duration: Infinity, volume: 0.016, attack: 0.999, decay: 0.9, pan });
          const e2 = sound.synth({ type: "square", tone: playFreq + detune(), duration: Infinity, volume: 0.008, attack: 0.05, decay: 0.9, pan });
          synth = a; // primary handle for kill
          rememberSound(hitNote.key, {
            synth, note: hitNote.letter, octave: hitNote.octave, baseFreq: freq,
            compositeVoices: [a, b, c, d, e2],
          }, system, 1);
        } else {
          synth = sound.synth({
            type: wave, tone: playFreq, duration: Infinity,
            volume: 0.5, attack: 0.005, decay: 0.1, pan,
          });
          rememberSound(hitNote.key, { synth, note: hitNote.letter, octave: hitNote.octave, baseFreq: freq }, system, 1);
        }
        if (!sounds[hitNote.key]) {
          synth = sound.synth({
            type: "sine", tone: playFreq, duration: Infinity,
            volume: 0.5, attack: 0.005, decay: 0.1, pan,
          });
          rememberSound(hitNote.key, { synth, note: hitNote.letter, octave: hitNote.octave, baseFreq: freq }, system, 1);
        }
        trail[hitNote.key] = { note: hitNote.letter, octave: hitNote.octave, brightness: 1.0 };
        touchNotes[pid] = { key: hitNote.key };
        return;
      }
    }
  }
  if (e.is("draw")) {
    const y = e.pointer?.y ?? e.y ?? 0;
    const x = e.pointer?.x ?? e.x ?? 0;
    const w = globalThis.__screenW || 320;
    const pid = e.pointer?.id ?? 0;
    hoverX = x; hoverY = y;
    // Per-side master volume slider drag
    if (leftVolDragging) {
      const ls = globalThis.__leftMasterSlider;
      if (ls) {
        leftMasterVol = Math.max(0, Math.min(1, 1 - (y - ls.y) / ls.h));
      }
    }
    if (rightVolDragging) {
      const rs = globalThis.__rightMasterSlider;
      if (rs) {
        rightMasterVol = Math.max(0, Math.min(1, 1 - (y - rs.y) / rs.h));
      }
    }
    // Deck scratch: horizontal drag velocity → deck speed.
    // Positive dx forwards, negative dx backwards. Clamped to a reasonable range.
    if (djDragging) {
      const dk = sound?.deck;
      if (dk?.decks?.[0]?.loaded) {
        const dx = x - djDragLastX;
        // 1 px ≈ 0.08 speed units; clamp to [-3, 3]
        djScratchSpeed = Math.max(-3, Math.min(3, dx * 0.08));
        dk.setSpeed(0, djScratchSpeed);
      }
      djDragLastX = x;
    }
    if (fxDragging) {
      fxMix = Math.max(0, Math.min(1, x / w));
      sound?.fx?.setMix?.(fxMix);
    }
    if (echoDragging) {
      echoMix = Math.max(0, Math.min(1, x / w));
      sound?.room?.setMix?.(echoMix);
    }
    if (pitchDragging) {
      pitchShift = Math.max(-1, Math.min(1, (x / w) * 2 - 1));
      const factor = Math.pow(2, effectivePitchShift());
      for (const k of Object.keys(sounds)) {
        const s = sounds[k];
        if (s && s.synth && s.baseFreq) {
          if (s.isSample) s.synth.update({ tone: s.baseFreq * factor, base: SAMPLE_BASE_FREQ });
          else s.synth.update({ tone: s.baseFreq * factor });
        }
      }
    }
    if (volDragging) {
      const vb = globalThis.__volBar;
      if (vb) {
        const pct = Math.max(0, Math.min(1, (x - vb.barX) / vb.barW));
        const target = Math.round(pct * 20);
        const cur = Math.round((sound?.speaker?.systemVolume ?? 50) / 5);
        const diff = target - cur;
        if (diff !== 0) system?.volumeAdjust?.(diff > 0 ? 1 : -1);
      }
    }
    if (brtDragging) {
      const bb = globalThis.__brtBar;
      if (bb) {
        const pct = Math.max(0, Math.min(1, (x - bb.barX) / bb.barW));
        const target = Math.round(pct * 20);
        const cur = Math.round((system?.brightness ?? 50) / 5);
        const diff = target - cur;
        if (diff !== 0) system?.brightnessAdjust?.(diff > 0 ? 1 : -1);
      }
    }
    // Grid rollover: dragging across note buttons triggers the new one
    const gridInfo = globalThis.__gridInfo;
    if (gridInfo && touchNotes[pid] !== undefined) {
      const hitNote = hitTestGrid(x, y, gridInfo);
      if (hitNote && hitNote.key && hitNote.key !== touchNotes[pid]?.key) {
        // Release current note
        const oldKey = touchNotes[pid].key;
        stopSoundKey(oldKey, sound, system, 0.02);
        // Trigger new note
        if (!sounds[hitNote.key]) {
          const freq = noteToFreq(hitNote.letter, hitNote.octave);
          const semitones = (hitNote.octave - 4) * 12 + CHROMATIC.indexOf(hitNote.letter);
          const pan = Math.max(-0.8, Math.min(0.8, (semitones - 12) / 15));
          const playFreq = freq * Math.pow(2, effectivePitchShift());
          // Drag-to-drum: drum pads fire as one-shots on rollover too.
          const rollDrum = percussionDrumFor(hitNote.letter, hitNote.gridOffset);
          if (rollDrum) {
            const bankSample = percussionSampleBank[rollDrum];
            if (wave === "sample" && bankSample) {
              if (bankSample !== lastLoadedSample) {
                sound.sample.loadData(bankSample.data, bankSample.rate);
                lastLoadedSample = bankSample;
              }
              sound.sample.play({
                tone: SAMPLE_BASE_FREQ * Math.pow(2, effectivePitchShift()),
                base: SAMPLE_BASE_FREQ,
                volume: 1.6, pan, loop: false,
              });
            } else {
              playPercussion(sound, hitNote.letter, 1.8, pan, Math.pow(2, effectivePitchShift()));
            }
            trail[hitNote.key] = { note: hitNote.letter, octave: hitNote.octave, brightness: 1.0 };
            touchNotes[pid] = { key: hitNote.key };
            return;
          }
          let synth;
          if (wave === "sample" && sampleLoaded) {
            const smp = sound.sample.play({
              tone: playFreq, base: SAMPLE_BASE_FREQ, volume: 0.5, pan, loop: true,
            });
            if (smp) {
              synth = smp;
              rememberSound(hitNote.key, { synth, note: hitNote.letter, octave: hitNote.octave, baseFreq: freq, isSample: true }, system, 1);
            }
          } else {
            synth = sound.synth({
              type: wave, tone: playFreq,
              duration: Infinity, volume: 0.5, attack: 0.005, decay: 0.1, pan,
            });
            rememberSound(hitNote.key, { synth, note: hitNote.letter, octave: hitNote.octave, baseFreq: freq }, system, 1);
          }
          if (!sounds[hitNote.key]) {
            synth = sound.synth({
              type: "sine", tone: playFreq,
              duration: Infinity, volume: 0.5, attack: 0.005, decay: 0.1, pan,
            });
            rememberSound(hitNote.key, { synth, note: hitNote.letter, octave: hitNote.octave, baseFreq: freq }, system, 1);
          }
          trail[hitNote.key] = { note: hitNote.letter, octave: hitNote.octave, brightness: 1.0 };
          touchNotes[pid] = { key: hitNote.key };
        }
      }
    }
  }
  if (e.is("lift")) {
    const pid = e.pointer?.id ?? 0;
    // Stop REC only when the REC-holding pointer is released.
    if (recording && (recPointerId === null || recPointerId === pid)) {
      stopSampleRecording(sound, "touch-lift");
    }
    // Deck scratch release — restore normal speed and resume or pause.
    if (djDragging) {
      const dk = sound?.deck;
      if (dk?.decks?.[0]?.loaded) {
        dk.setSpeed(0, 1);
        if (djDragWasPlaying) dk.play(0);
        else dk.pause(0);
      }
      djDragging = false;
      djScratchSpeed = 0;
    }
    if (leftVolDragging) leftVolDragging = false;
    if (rightVolDragging) rightVolDragging = false;
    if (fxDragging) fxDragging = false;
    if (echoDragging) echoDragging = false;
    if (pitchDragging) pitchDragging = false;
    if (volDragging) volDragging = false;
    if (brtDragging) brtDragging = false;
    // Release touch-triggered note
    if (touchNotes[pid]) {
      const key = touchNotes[pid].key;
      stopSoundKey(key, sound, system, 0.08);
      delete touchNotes[pid];
    }
  }
}

function paint({ wipe, ink, box, line, write, screen, sound, system, trackpad, pressures, wifi }) {
  frame++;
  usbMidiRecent = usbMidiRecent.filter((entry) => entry.until > frame);
  // FPS tracking
  const now = performance.now();
  if (fpsLastTime > 0) {
    fpsAccum += now - fpsLastTime;
    fpsSamples++;
    if (fpsSamples >= 30) {
      fpsDisplay = Math.round(30000 / fpsAccum);
      fpsAccum = 0;
      fpsSamples = 0;
    }
  }
  fpsLastTime = now;
  const activeCount = Object.keys(sounds).length;
  const w = screen.width;
  const h = screen.height;
  const CH = 6; // char width at size 1 (font_1 = 6x10)
  const mic = sound?.microphone || {};
  let usbMidiStatus = readUsbMidiStatus(system);
  sampleLoaded = (mic.sampleLength || 0) > 0;
  globalThis.__screenW = w; // expose for act()
  globalThis.__screenH = h;

  const typecSignature = (system?.typec || [])
    .map((port) => port.port + ":" + port.powerRole + ":" + port.dataRole)
    .join("|");
  const typecChanged = typecSignature !== usbMidiTypecSignature;
  if (typecChanged) {
    usbMidiTypecSignature = typecSignature;
  }

  if (usbMidiStatus?.enabled && !usbMidiStatus?.active && frame >= usbMidiNextRefreshFrame) {
    const typecDevice = (system?.typec || []).some((port) => port.dataRole === "device");
    if (typecChanged && typecDevice) {
      usbMidiStatus = system?.usbMidi?.refresh?.() || usbMidiStatus;
      usbMidiNextRefreshFrame = frame + 180;
    }
  }

  // Trackpad FX: X = echo, Y = pitch shift (when enabled via \)
  if (trackpadFX && trackpad) {
    if (trackpad.dx !== 0) {
      echoMix = Math.max(0, Math.min(1, echoMix + (trackpad.dx * 3) / w));
      if (frame % 3 === 0) sound?.room?.setMix?.(echoMix);
    }
    if (trackpad.dy !== 0) {
      pitchShift = Math.max(-1, Math.min(1, pitchShift - trackpad.dy / h));
    }
    // Apply pitch shift to active voices — throttled to every 4th frame
    // and only when pitch actually changed
    const ep = effectivePitchShift();
    if (frame % 4 === 0 && Math.abs(ep - lastAppliedPitch) > 0.001) {
      lastAppliedPitch = ep;
      const factor = Math.pow(2, ep); // ±1 octave
      for (const key of Object.keys(sounds)) {
        const s = sounds[key];
        if (s && s.synth && s.baseFreq) {
          if (s.isSample) {
            s.synth.update({ tone: s.baseFreq * factor, base: SAMPLE_BASE_FREQ });
          } else {
            s.synth.update({ tone: s.baseFreq * factor });
          }
        }
      }
    }
  }

  // Metronome tick (clock-synced)
  if (metronomeEnabled && metronomeBPM > 0) {
    const now = syncedNow();
    const msPerBeat = 60000 / metronomeBPM;
    const beatNumber = Math.floor(now / msPerBeat);
    // Pendulum: smooth sinusoidal swing synced to beat phase (2-beat period)
    const beatPhase = (now % (msPerBeat * 2)) / (msPerBeat * 2);
    metronomePendulumAngle = Math.sin(beatPhase * Math.PI * 2);
    if (beatNumber !== metronomeBeatCount) {
      metronomeBeatCount = beatNumber;
      metronomeVisualPhase = 1.0;
      metronomeFlash = 1.0;
      const isDownbeat = (beatNumber % 4) === 0;
      sound.synth({
        type: "square", tone: isDownbeat ? 1200 : 800,
        duration: 0.03, volume: isDownbeat ? 0.4 : 0.25,
        attack: 0.001, decay: 0.02, pan: 0,
      });
    }
    if (metronomeVisualPhase > 0) metronomeVisualPhase = Math.max(0, metronomeVisualPhase - 0.08);
    if (metronomeFlash > 0) metronomeFlash = Math.max(0, metronomeFlash - 0.15);
  }

  // Re-check dark mode every ~10 seconds
  // dark mode is always on

  // Theme: dark after 7pm LA, light before
  const FG = dark ? 220 : 40;
  const FG_DIM = dark ? 140 : 100;
  const FG_MUTED = dark ? 80 : 150;
  const BAR_BG = dark ? [35, 20, 30] : [225, 220, 215];
  const BAR_BORDER = dark ? [55, 35, 45] : [200, 195, 190];
  const PAD_NORMAL = dark ? [28, 28, 30] : [250, 248, 244];
  const PAD_SHARP = dark ? [18, 18, 20] : [235, 232, 228];
  const PAD_OUTLINE = dark ? [50, 50, 55] : [210, 205, 200];

  // Compute background color from active notes — full color flash
  const activeKeys = Object.keys(sounds);
  if (activeKeys.length > 0) {
    let r = 0, g = 0, b = 0;
    for (const key of activeKeys) {
      const noteName = KEY_TO_NOTE[key];
      if (noteName) {
        const [letter] = parseNote(noteName);
        const nc = noteColor(letter);
        r += nc[0]; g += nc[1]; b += nc[2];
      }
    }
    const n = activeKeys.length;
    // Saturate: boost toward max channel, clamp to 255
    const avg = [r / n, g / n, b / n];
    const mx = Math.max(avg[0], avg[1], avg[2], 1);
    const sat = 255 / mx; // scale so brightest channel hits 255
    bgTarget = [
      Math.min(255, Math.floor(avg[0] * sat)),
      Math.min(255, Math.floor(avg[1] * sat)),
      Math.min(255, Math.floor(avg[2] * sat)),
    ];
    // Snap on instantly (blink)
    bgColor[0] = bgTarget[0];
    bgColor[1] = bgTarget[1];
    bgColor[2] = bgTarget[2];
  } else {
    bgTarget = dark ? [0, 0, 0] : [255, 255, 255];
    // Fade smoothly
    bgColor[0] += (bgTarget[0] - bgColor[0]) * 0.25;
    bgColor[1] += (bgTarget[1] - bgColor[1]) * 0.25;
    bgColor[2] += (bgTarget[2] - bgColor[2]) * 0.25;
  }

  wipe(Math.round(bgColor[0]), Math.round(bgColor[1]), Math.round(bgColor[2]));

  // Tablet mode change detection — beep + log
  if (system.tabletMode !== lastTabletMode && lastTabletMode !== null) {
    if (system.tabletMode) {
      sound?.synth({ type: "sine", tone: 880, duration: 0.12, volume: 0.2, attack: 0.005, decay: 0.1 });
      sound?.synth({ type: "sine", tone: 1320, duration: 0.12, volume: 0.15, attack: 0.005, decay: 0.1 });
    } else {
      sound?.synth({ type: "sine", tone: 1320, duration: 0.12, volume: 0.2, attack: 0.005, decay: 0.1 });
      sound?.synth({ type: "sine", tone: 880, duration: 0.12, volume: 0.15, attack: 0.005, decay: 0.1 });
    }
    system.writeFile?.("/mnt/tablet-mode.log",
      `${new Date().toISOString()} tablet=${system.tabletMode ? "on" : "off"}\n`);
  }
  lastTabletMode = system.tabletMode;

  // Tablet mode or Recital mode (F12): solid color only, no UI
  if (system.tabletMode || recitalMode) return;

  // Metronome flash border
  if (metronomeFlash > 0 && metronomeEnabled) {
    const fa = Math.floor(metronomeFlash * 120);
    const fw = Math.max(2, Math.floor(5 * metronomeFlash));
    const db = (metronomeBeatCount % 4) === 0;
    if (db) ink(255, 100, 100, fa); else ink(180, 200, 255, fa);
    box(0, 0, w, fw, true);
    box(0, h - fw, w, fw, true);
    box(0, 0, fw, h, true);
    box(w - fw, 0, fw, h, true);
  }

  // === STATUS BAR ===
  const topBarH = 14;
  const barY = 2;

  ink(BAR_BG[0], BAR_BG[1], BAR_BG[2]);
  box(0, 0, w, topBarH, true);
  ink(BAR_BORDER[0], BAR_BORDER[1], BAR_BORDER[2]);
  line(0, topBarH - 1, w, topBarH - 1);

  // Reserve space for right-side indicators so left status text never overlaps.
  const reserveBatPct = system?.battery?.percent ?? -1;
  const reserveSysBrt = system?.brightness ?? -1;
  const reserveNow = new Date(syncedNow());
  const reserveLaMs = reserveNow.getTime() - getLAOffset() * 3600000;
  const reserveLaDate = new Date(reserveLaMs);
  const reserveHh = reserveLaDate.getUTCHours();
  const reserveMm = reserveLaDate.getUTCMinutes();
  const reserveSs = reserveLaDate.getUTCSeconds();
  const reserveAmpm = reserveHh >= 12 ? "p" : "a";
  const reserveH12 = reserveHh % 12 || 12;
  const reserveTime = reserveH12 + ":" + (reserveMm < 10 ? "0" : "") + reserveMm +
    ":" + (reserveSs < 10 ? "0" : "") + reserveSs + reserveAmpm;
  let statusRightReserve = reserveTime.length * CH + 4 + 20 + 2 + 3 * CH; // time + vol
  if (reserveBatPct >= 0) statusRightReserve += 14 + (String(reserveBatPct).length + 1) * CH + 6;
  if (reserveSysBrt >= 0) statusRightReserve += 4 + 16 + 2 + 3 * CH;
  const statusRightLimit = Math.max(80, w - statusRightReserve - 8);

  // Left: "notepat.com" — clickable, jumps to prompt piece
  const npHovered = hoverX >= 0 && hoverX <= 46 && hoverY < topBarH;
  if (npHovered) { ink(255, 255, 255, 20); box(0, 0, 48, topBarH, true); }
  ink(FG, FG, FG, npHovered ? 255 : 200);
  write("notepat", { x: 2, y: barY, size: 1, font: "matrix" });
  ink(dark ? 200 : 180, dark ? 100 : 60, dark ? 140 : 120, npHovered ? 255 : 200);
  const dotComX = 2 + 7 * 4;
  write(".com", { x: dotComX, y: barY, size: 1, font: "matrix" });
  globalThis.__npBtn = { w: 48, h: topBarH };

  // Status area after notepat.com — auto-update indicator only (minimal)
  let statusX = dotComX + 4 * 4 + 6;
  const statusWrite = (text, r, g, b, a = 255) => {
    if (!text) return false;
    const width = text.length * 4;
    if (statusX + width > statusRightLimit) return false;
    ink(r, g, b, a);
    write(text, { x: statusX, y: barY, size: 1, font: "matrix" });
    statusX += width + 2;
    return true;
  };

  // Auto-update status (subtle — download/flash progress only)
  if (autoUpdate.state === "downloading") {
    const pct = Math.round((osProgress || 0) * 100);
    statusWrite("os " + pct + "%", 80, 180, 100, 180);
  } else if (autoUpdate.state === "flashing") {
    const ap = system.flashPhase ?? 0;
    const at = ap === 3 ? "verify" : "flash";
    statusWrite(at, ap === 3 ? 100 : 255, ap === 3 ? 200 : 160, ap === 3 ? 255 : 60, 200);
  } else if (autoUpdate.state === "ready") {
    statusWrite("reboot?", 100, 220, 100, 220);
  }

  const usbMidiText = usbMidiStatusText(usbMidiStatus);
  if (usbMidiStatus?.active) {
    statusWrite(usbMidiText, 100, 220, 140, 220);
  } else if (usbMidiStatus?.enabled) {
    statusWrite(usbMidiText, 255, 190, 80, 220);
  } else {
    statusWrite(usbMidiText, FG_DIM, FG_DIM, FG_DIM, 200);
  }

  const relayText = udpMidiRelayStatusText(system);
  if (relayText) {
    statusWrite(
      relayText,
      system?.udp?.connected ? 80 : 255,
      system?.udp?.connected ? 180 : 180,
      system?.udp?.connected ? 255 : 90,
      210
    );
  }

  // Metronome indicator (pendulum) in status bar — shown when enabled
  if (metronomeEnabled) {
    const bpmLabel = metronomeBPM + "b";
    const metNeed = bpmLabel.length * 4 + 3 + 14;
    if (statusX + metNeed <= statusRightLimit) {
      const metX = statusX;
      ink(dark ? 160 : 100, dark ? 160 : 100, dark ? 170 : 110);
      write(bpmLabel, { x: metX, y: barY, size: 1, font: "matrix" });
      statusX += bpmLabel.length * 4 + 3;
      const px = statusX + 5;
      const pvY = barY + 1;
      const armLen = 8;
      const angle = metronomePendulumAngle * 0.45;
      const bobX = Math.round(px + Math.sin(angle) * armLen);
      const bobY = Math.round(pvY + Math.cos(angle) * armLen);
      const db = (metronomeBeatCount % 4) === 0;
      const fa = Math.floor(metronomeVisualPhase * 255);
      if (fa > 0) ink(db ? 255 : 180, db ? 100 : 180, db ? 100 : 255, fa);
      else ink(dark ? 120 : 150, dark ? 130 : 150, dark ? 140 : 160);
      line(px, pvY, bobX, bobY);
      box(bobX - 1, bobY - 1, 3, 3, true);
      statusX += 14;
    }
  }

  // OS update panel (fullscreen, like WiFi panel)
  if (activeScreen === "os") {
    const panelY = topBarH;
    const panelH = h - panelY;
    ink(dark ? 12 : 240, dark ? 16 : 235, dark ? 24 : 230, 240);
    box(0, panelY, w, panelH, true);
    ink(dark ? 40 : 180, dark ? 60 : 160, dark ? 80 : 140);
    line(0, panelY, w, panelY);

    const pad = 10;
    const midY = Math.floor(panelY + panelH * 0.35);
    const maxVerChars = Math.floor((w - pad * 2) / 6) - 10; // fit label + version

    // Title — centered
    ink(dark ? 200 : 40, dark ? 220 : 60, dark ? 200 : 80);
    const title = "ac/native";
    write(title, { x: pad, y: panelY + 8, size: 1, font: "matrix" });

    // Connection status
    if (!wifi?.connected) {
      ink(200, 80, 80);
      write("offline", { x: pad, y: panelY + 22, size: 1, font: "font_1" });
      ink(dark ? 100 : 120, dark ? 80 : 100, dark ? 80 : 100);
      write("connect wifi first", { x: pad, y: panelY + 34, size: 1, font: "font_1" });
    } else {
      // Current version
      ink(dark ? 100 : 100, dark ? 110 : 110, dark ? 100 : 100);
      write("current", { x: pad, y: panelY + 22, size: 1, font: "font_1" });
      ink(dark ? 180 : 40, dark ? 180 : 40, dark ? 180 : 40);
      const curVer = (osCurrentVersion || "unknown").slice(0, maxVerChars);
      write(curVer, { x: pad, y: panelY + 34, size: 1, font: "font_1" });
    }

    const stateY = panelY + 50;
    if (osState === "checking") {
      ink(200, 200, 80);
      write("checking...", { x: pad, y: stateY, size: 1, font: "font_1" });
    } else if (osState === "up-to-date") {
      ink(dark ? 100 : 100, dark ? 110 : 110, dark ? 100 : 100);
      write("available", { x: pad, y: stateY, size: 1, font: "font_1" });
      ink(80, 200, 120);
      write(osRemoteVersion.slice(0, maxVerChars), { x: pad, y: stateY + 12, size: 1, font: "font_1" });
      ink(80, 200, 120);
      write("up to date!", { x: pad, y: stateY + 28, size: 1, font: "font_1" });
    } else if (osState === "available") {
      ink(dark ? 100 : 100, dark ? 110 : 110, dark ? 100 : 100);
      write("available", { x: pad, y: stateY, size: 1, font: "font_1" });
      ink(255, 200, 60);
      write(osRemoteVersion.slice(0, maxVerChars), { x: pad, y: stateY + 12, size: 1, font: "font_1" });
      // Flash target selector
      const targets = system.flashTargets || [];
      if (targets.length > 0) {
        if (flashTargetIdx >= targets.length) flashTargetIdx = 0;
        const tgt = targets[flashTargetIdx];
        const tgtLabel = (tgt?.label || "?") + " (" + (tgt?.device || "?") + ")";
        const isBoot = (tgt?.device === system.bootDevice);
        ink(dark ? 80 : 120, dark ? 100 : 100, dark ? 120 : 80);
        write("target:", { x: pad, y: stateY + 28, size: 1, font: "font_1" });
        ink(isBoot ? 80 : 200, isBoot ? 200 : 200, isBoot ? 255 : 80);
        write(tgtLabel, { x: pad + 48, y: stateY + 28, size: 1, font: "font_1" });
        if (isBoot) {
          ink(60, 140, 200);
          write("(current boot)", { x: pad, y: stateY + 40, size: 1, font: "font_1" });
        }
        // Cycle button if multiple targets
        if (targets.length > 1) {
          ink(120, 120, 140);
          const cycX = w - pad - 36;
          box(cycX, stateY + 26, 32, 14, true);
          ink(220, 220, 240);
          write("next", { x: cycX + 4, y: stateY + 29, size: 1, font: "font_1" });
          globalThis.__osCycleBtn = { x: cycX, y: stateY + 26, w: 32, h: 14 };
        } else {
          globalThis.__osCycleBtn = null;
        }
      }
      // Update button — centered, proportional
      const btnW = Math.min(120, w - pad * 4);
      const btnX = Math.floor((w - btnW) / 2);
      const btnY = stateY + (targets.length > 0 ? 56 : 34);
      ink(60, 180, 100);
      box(btnX, btnY, btnW, 18, true);
      ink(dark ? 220 : 240, dark ? 255 : 255, dark ? 220 : 240);
      write("update now", { x: btnX + Math.floor((btnW - 60) / 2), y: btnY + 4, size: 1, font: "font_1" });
      globalThis.__osUpdateBtn = { x: btnX, y: btnY, w: btnW, h: 18 };
    } else if (osState === "downloading") {
      ink(dark ? 120 : 80, dark ? 140 : 100, dark ? 120 : 80);
      write("downloading...", { x: pad, y: stateY, size: 1, font: "font_1" });
      // Progress bar — full width with padding
      const barX = pad, barW2 = w - pad * 2, barH2 = 8, barY2 = stateY + 16;
      ink(dark ? 30 : 200, dark ? 40 : 190, dark ? 50 : 180);
      box(barX, barY2, barW2, barH2, true);
      ink(60, 180, 100);
      box(barX, barY2, Math.round(barW2 * (osProgress || 0)), barH2, true);
      ink(dark ? 160 : 80);
      write(Math.round((osProgress || 0) * 100) + "%", { x: pad, y: barY2 + 12, size: 1, font: "font_1" });
    } else if (osState === "flashing") {
      const phase = system.flashPhase ?? 0;
      const phaseText = phase === 1 ? "writing EFI..."
                      : phase === 2 ? "syncing to disk..."
                      : phase === 3 ? "verifying..."
                      : phase === 4 ? "done"
                      : "preparing...";
      const phaseColor = phase === 3 ? [100, 200, 255] : [255, 160, 60];
      ink(...phaseColor);
      write(phaseText, { x: pad, y: stateY, size: 1, font: "font_1" });
      ink(dark ? 100 : 80);
      const flashDev = globalThis.__osFlashDevice || system.bootDevice || "?";
      write("→ " + flashDev, { x: pad, y: stateY + 14, size: 1, font: "font_1" });
      ink(dark ? 140 : 80);
      write("do not power off", { x: pad, y: stateY + 26, size: 1, font: "font_1" });
    } else if (osState === "rebooting") {
      const vb = system.flashVerifiedBytes ?? 0;
      const mb = (vb / 1048576).toFixed(1);
      ink(80, 255, 120);
      write(`verified ${mb}MB`, { x: pad, y: stateY, size: 1, font: "font_1" });
      ink(200, 100, 60);
      write("rebooting...", { x: pad, y: stateY + 14, size: 1, font: "font_1" });
    } else if (osState === "error") {
      ink(220, 80, 80);
      const errTxt = ("error: " + osError).slice(0, Math.floor((w - pad * 2) / 6));
      write(errTxt, { x: pad, y: stateY, size: 1, font: "font_1" });
      ink(dark ? 120 : 80);
      write("tap os to retry", { x: pad, y: stateY + 14, size: 1, font: "font_1" });
    }

    // Dismiss hint — bottom
    ink(dark ? 60 : 160, dark ? 80 : 150, dark ? 60 : 140);
    write("tap os to close", { x: pad, y: h - 12, size: 1, font: "font_1" });
  }

  // Center status bar: note count is enough, settings go below
  const centerX = Math.floor(w / 2);

  // TTS + WebSocket connect on WiFi connect transition (with 10s cooldown)
  if (wifi?.connected && !wifiWasConnected && (frame - wifiConnectFrame) > 600) {
    wifiConnectFrame = frame;
    // WiFi connect chord: perfect fifth (G5 + D6)
    sound?.synth({ type: "sine", tone: 784, duration: 0.18, volume: 0.22, attack: 0.005, decay: 0.15 });
    sound?.synth({ type: "sine", tone: 1175, duration: 0.18, volume: 0.16, attack: 0.005, decay: 0.15 });
    autoConnectTry = 0;
    wsStatus = "connecting";
    wsConnectGrace = 180; // ~3s grace before declaring error
    system.ws?.connect("wss://chat-system.aesthetic.computer/");
    // Connect raw UDP for fairy co-presence
    system.udp?.connect("session-server.aesthetic.computer", 10010);
    udpMidiNextHeartbeatFrame = frame + 30;
    if (system?.writeFile && savedCreds.length > 0) {
      system.writeFile(CREDS_PATH, JSON.stringify(savedCreds));
    }
    // Kick off clock sync fetch (only if fetch slot is free)
    clockSynced = false;
    clockSyncFrame = 0;
    if (!system.fetchPending) {
      system.fetch?.("https://aesthetic.computer/api/clock");
    }
    // Kick off update check shortly after connect (background ping + auto-update path).
    scheduleAutoUpdateCheck(300);
    // Start SSH daemon for remote access
    if (!system.sshStarted) system.startSSH?.();
  }
  wifiWasConnected = !!wifi?.connected;

  if (!wifi?.connected && autoUpdate.state === "checking") {
    autoUpdate.fetchPending = false;
    autoUpdate.state = "idle";
  }

  // Background update check schedule while connected.
  if (wifi?.connected
      && autoUpdate.state === "checking"
      && !autoUpdate.fetchPending
      && !osFetchPending
      && frame >= autoUpdate.nextCheckFrame
      && !system.fetchPending) {
    autoUpdate.fetchPending = true;
    system.fetch?.(OS_VERSION_URL);
  }

  // HTTP error routing for single fetch slot.
  if (system.fetchError) {
    const err = (system.fetchError || "request failed").trim().slice(0, 120);
    if (osFetchPending) {
      osFetchPending = false;
      osError = err;
      osState = "error";
    } else if (autoUpdate.fetchPending) {
      autoUpdate.fetchPending = false;
      autoUpdate.lastError = err;
      autoUpdate.state = "checking";
      autoUpdate.nextCheckFrame = frame + OS_AUTO_RETRY_FRAMES;
    }
  }

  // Poll for fetch result — priority: manual OS panel > auto-update > clock sync
  if (system.fetchResult) {
    const fetchText = system.fetchResult.trim();
    if (osFetchPending) {
      // Manual OS panel version check
      osFetchPending = false;
      const ver = fetchText;
      if (ver && ver.length > 0 && ver.length < 128) {
        const newer = isRemoteVersionNewer(ver, osCurrentVersion);
        osRemoteVersion = ver;
        autoUpdate.availableVersion = newer ? ver : "";
        osState = newer ? "available" : "up-to-date";
        console.log(`[os] manual-check local=${osCurrentVersion} remote=${ver} newer=${newer}`);
        if (newer) notifyUpdateAvailable(sound, ver);
      } else {
        osError = "version check failed";
        osState = "error";
      }
    } else if (autoUpdate.fetchPending) {
      // Background auto-update version check
      autoUpdate.fetchPending = false;
      const ver = fetchText;
      const newer = isRemoteVersionNewer(ver, osCurrentVersion);
      console.log(`[os] auto-check local=${osCurrentVersion} remote=${ver} newer=${newer}`);
      if (ver && ver.length > 0 && ver.length < 128 && newer) {
        autoUpdate.availableVersion = ver;
        osRemoteVersion = ver;
        notifyUpdateAvailable(sound, ver);
        // Don't auto-download — user must manually trigger via OS panel
        autoUpdate.state = "idle";
      } else {
        autoUpdate.availableVersion = "";
        autoUpdate.state = "checking";
        autoUpdate.nextCheckFrame = frame + OS_AUTO_CHECK_INTERVAL_FRAMES;
      }
    } else {
      // Clock sync result
      const t1 = Date.now();
      try {
        const serverTime = new Date(fetchText).getTime();
        if (!isNaN(serverTime)) {
          const newOffset = serverTime - t1;
          clockOffset += (newOffset - clockOffset) * 0.5;
          clockSynced = true;
        }
      } catch (e) { /* ignore parse errors */ }
    }
  }


  // Periodic re-sync every ~10 min (at 60fps: 36000 frames)
  clockSyncFrame++;
  if (wifi?.connected && clockSyncFrame % 36000 === 0
      && !osFetchPending && !autoUpdate.fetchPending && !system.fetchPending) {
    system.fetch?.("https://aesthetic.computer/api/clock");
  }

  // Background auto-update state machine (runs independently of OS panel)
  if (autoUpdate.state === "downloading") {
    osProgress = system.fetchBinaryProgress ?? osProgress;
    if (system.fetchBinaryDone) {
      if (system.fetchBinaryOk) {
        console.log("[os] auto-download complete, starting flash");
        autoUpdate.state = "flashing";
        system.flashUpdate?.("/tmp/vmlinuz.new"); // auto-detects boot device
      } else {
        console.log("[os] auto-download FAILED");
        autoUpdate.lastError = "download failed";
        autoUpdate.state = "checking";
        autoUpdate.nextCheckFrame = frame + OS_AUTO_RETRY_FRAMES;
      }
    }
  }
  if (autoUpdate.state === "flashing") {
    if (system.flashDone) {
      if (system.flashOk) {
        // Don't auto-reboot — just mark as ready. User must manually reboot via OS panel.
        autoUpdate.state = "ready";
        autoUpdate.lastError = "";
        console.log("[os] auto-update flashed successfully, awaiting manual reboot");
      } else {
        console.log("[os] auto-flash FAILED");
        autoUpdate.lastError = "flash failed";
        autoUpdate.state = "checking";
        autoUpdate.nextCheckFrame = frame + OS_AUTO_RETRY_FRAMES;
      }
    }
  }

  // OS update panel state machine (manual, only when panel open)
  if (activeScreen === "os") {
    if (osState === "checking" && !wifi?.connected) {
      osState = "error";
      osError = "no wifi connection";
    } else if (osState === "checking" && !osFetchPending) {
      // Cancel any in-flight auto-update fetch so we can use the fetch slot
      if (autoUpdate.fetchPending) {
        system.fetchCancel?.();
        autoUpdate.fetchPending = false;
        autoUpdate.state = "checking";
        autoUpdate.nextCheckFrame = frame + OS_AUTO_RETRY_FRAMES;
      }
      // Wait for C-side fetch slot to be free before issuing
      if (!system.fetchPending) {
        osFetchPending = true;
        osCheckFrame = frame;
        osError = "";
        system.fetch?.(OS_VERSION_URL);
      }
    }
    // Timeout: if checking for > 10s (600 frames), give up
    if (osState === "checking" && osFetchPending && frame - osCheckFrame > 600) {
      system.fetchCancel?.();
      osFetchPending = false;
      osState = "error";
      osError = "request timed out";
    }
    if (osState === "downloading") {
      osProgress = system.fetchBinaryProgress ?? osProgress;
      if (system.fetchBinaryDone) {
        if (system.fetchBinaryOk) {
          const dev = globalThis.__osFlashDevice;
          console.log(`[os] manual download complete, flashing to ${dev || "auto"}`);
          osState = "flashing";
          if (dev) {
            system.flashUpdate?.("/tmp/vmlinuz.new", dev);
          } else {
            system.flashUpdate?.("/tmp/vmlinuz.new");
          }
        } else {
          console.log("[os] manual download FAILED");
          osError = "download failed";
          osState = "error";
        }
      }
    }
    if (osState === "flashing") {
      if (system.flashDone) {
        if (system.flashOk) {
          console.log("[os] manual flash complete! ready to reboot");
          osState = "rebooting";
          globalThis.__osRebootFrame = frame + 180; // 3s at 60fps
        } else {
          console.log("[os] manual flash FAILED");
          osError = "flash failed";
          osState = "error";
        }
      }
    }
    if (osState === "rebooting" && globalThis.__osRebootFrame && frame >= globalThis.__osRebootFrame) {
      console.log("[os] rebooting now!");
      system.reboot?.();
    }
  }

  // Track WS connection status + auto-reconnect when server drops
  if (wsConnectGrace > 0) wsConnectGrace--;
  if (wsReconnectTimer > 0) wsReconnectTimer--;
  if (system.ws?.connecting) { /* don't overwrite wsStatus here — preserve "reconnecting" */ wsConnectGrace = 0; }
  else if (system.ws?.connected && wsStatus !== "connected") {
    const wasFirstConnect = (wsStatus !== "reconnecting" && wsStatus !== "connected");
    wsStatus = "connected";
    wsConnectGrace = 0;
    // Chat connected ding — only on very first connect, never on reconnects
    if (wasFirstConnect && !globalThis.__wsEverConnected) {
      globalThis.__wsEverConnected = true;
      sound?.synth({ type: "sine", tone: 1047, duration: 0.1, volume: 0.18, attack: 0.003, decay: 0.08 });
      sound?.synth({ type: "sine", tone: 1319, duration: 0.1, volume: 0.14, attack: 0.02, decay: 0.08 });
    }
  } else if (!system.ws?.connected && !system.ws?.connecting && wsConnectGrace === 0) {
    if (wsStatus === "connecting") wsStatus = "error";
    // Auto-reconnect 2s after drop (server sends history then closes)
    if (wifi?.connected && wsReconnectTimer === 0 && wsStatus !== "connecting" && wsStatus !== "reconnecting") {
      wsReconnectTimer = 120; // ~2s
      wsStatus = "reconnecting";
      wsConnectGrace = 180;
      system.ws?.connect("wss://chat-system.aesthetic.computer/");
    }
  }

  // Process incoming WebSocket chat messages (real-time)
  const wsMsgs = system.ws?.messages;
  if (wsMsgs?.length) {
    for (const raw of wsMsgs) {
      try {
        const msg = JSON.parse(raw);
        // content may be a string (needs parsing) or already an object
        const parseContent = (c) => (typeof c === "string" ? JSON.parse(c) : c);
        if (msg.type === "connected") {
          wsStatus = "connected"; // lock in even if server closes right after
          const content = parseContent(msg.content);
          const msgs = content?.messages || [];
          console.log("ws connected: " + msgs.length + " messages");
          const last = msgs.slice(-1)[0];
          if (last) console.log("last msg: from=" + last.from + " text=" + (last.text || "").slice(0, 40));
          if (last?.from && last?.text) {
            const msgKey = last.from + ":" + last.text;
            acMsg = { from: last.from, text: last.text };
            // Only TTS on first connect, not on reconnects with the same last message
            if (!chatMuted && msgKey !== lastSpokenMsgKey) {
              lastSpokenMsgKey = msgKey;
              sound?.speak(last.from + ": " + last.text);
            }
          }
        } else if (msg.type === "message") {
          const m = parseContent(msg.content);
          if (m?.from && m?.text) {
            acMsg = { from: m.from, text: m.text };
            if (!chatMuted) sound?.speak(m.from + ": " + m.text);
          }
        } else if (msg.from && msg.text) {
          acMsg = { from: msg.from, text: msg.text };
          if (!chatMuted) sound?.speak(msg.from + ": " + msg.text);
        }
      } catch (err) { console.log("ws parse error: " + err); }
    }
  }

  // Auto-connect: scan, pick strongest known network, timeout after 5s
  // Disabled when WiFi panel is open (user is manually picking)
  autoConnectFrame++;
  autoConnectBlink = (autoConnectBlink + 1) % 60;
  const autoConnectEnabled = activeScreen !== "wifi";

  // All known credentials
  const knownCreds = [
    { ssid: AC_SSID, pass: AC_PASS },
    ...savedCreds.filter((c) => c.ssid !== AC_SSID),
  ];
  const knownSSIDs = new Set(knownCreds.map((c) => c.ssid));

  const isConnecting = wifi && !wifi.connected &&
    (wifi.state === 3 /* CONNECTING */ || wifi.state === 4);
  const isIdle = wifi && !wifi.connected &&
    wifi.state !== 3 && wifi.state !== 4;

  // Timeout: if connecting for > 180 frames (~3s), disconnect and retry (silent)
  if (autoConnectEnabled && isConnecting && frame - connectStartFrame > 180) {
    wifi.disconnect?.();
    autoConnectFrame = -30; // brief delay before next attempt
  }

  if (autoConnectEnabled && isIdle) {
    // Trigger scan every ~30s (1800 frames) — iw scan causes 65ms+ frame
    // drops so keep it infrequent. Immediate on first frame only.
    if (autoConnectFrame <= 1 || autoConnectFrame % 1800 === 0) {
      wifi.scan?.();
    }
    // 60 frames (~1s) after scan: pick strongest known network from scan results
    if (autoConnectFrame % 120 === 60) {
      const nets = wifi.networks || [];
      const matches = nets
        .filter((n) => n.ssid && knownSSIDs.has(n.ssid))
        .sort((a, b) => b.signal - a.signal); // strongest first

      autoConnectTry++;
      if (matches.length > 0) {
        const best = matches[0];
        const cred = knownCreds.find((c) => c.ssid === best.ssid);
        wifi.connect(cred.ssid, cred.pass);
        connectStartFrame = frame;
      }
      // No known networks in scan — just wait for next scan cycle (no blind attempts)
    }
  }

  // Right section: battery | time | vol
  let rx = w - 2; // right edge cursor (builds right to left)

  // Battery icon + percentage
  const bat = system?.battery;
  const batPct = bat?.percent ?? -1;
  // Battery sound: 5% increments normally, every 1% below 10%
  // Lower battery = higher pitch, louder, longer, more voices (more urgent)
  {
    const batThreshold = batPct <= 10 ? 1 : 5;
    if (batPct >= 0 && lastBatPercent >= 0 && Math.abs(batPct - lastBatPercent) >= batThreshold) {
      const charging = bat?.charging;
      if (charging) {
        // Charging: gentle ascending major triad
        sound?.synth({ type: "sine", tone: 523, duration: 0.15, volume: 0.12, attack: 0.01, decay: 0.12 });
        sound?.synth({ type: "sine", tone: 659, duration: 0.15, volume: 0.10, attack: 0.03, decay: 0.12 });
        sound?.synth({ type: "sine", tone: 784, duration: 0.15, volume: 0.08, attack: 0.05, decay: 0.10 });
      } else {
        // Draining: pitch rises as battery drops (100%→220Hz, 0%→1760Hz)
        const urgency = 1 - batPct / 100; // 0 at full, 1 at empty
        const base = 220 + urgency * 1540;
        const vol = 0.08 + urgency * 0.32; // 0.08 at full, 0.40 at empty
        const dur = 0.08 + urgency * 0.30; // short at full, long at empty
        // Root + minor third + tritone (increasingly dissonant as battery drops)
        sound?.synth({ type: "sine", tone: base, duration: dur, volume: vol, attack: 0.005, decay: dur * 0.8 });
        sound?.synth({ type: "triangle", tone: base * 1.2, duration: dur * 0.9, volume: vol * 0.6, attack: 0.01, decay: dur * 0.7 });
        sound?.synth({ type: "sine", tone: base * 1.414, duration: dur * 0.7, volume: vol * 0.4, attack: 0.02, decay: dur * 0.5 });
        // Below 20%: add a 4th voice — high square pulse
        if (batPct < 20) {
          sound?.synth({ type: "square", tone: base * 2, duration: dur * 0.5, volume: vol * 0.25, attack: 0.001, decay: dur * 0.4 });
        }
        // Below 10%: add a 5th voice — sub-bass throb
        if (batPct < 10) {
          sound?.synth({ type: "sawtooth", tone: base * 0.5, duration: dur * 1.2, volume: vol * 0.5, attack: 0.005, decay: dur });
        }
        // At 5% or below: alarm cluster
        if (batPct <= 5) {
          sound?.synth({ type: "square", tone: base * 3, duration: 0.04, volume: 0.3, attack: 0.001, decay: 0.03 });
          sound?.synth({ type: "sine", tone: base * 2.5, duration: 0.06, volume: 0.2, attack: 0.001, decay: 0.05 });
        }
      }
      lastBatPercent = batPct;
    } else if (batPct >= 0 && lastBatPercent < 0) {
      lastBatPercent = batPct; // initial read, no sound
    }
  }
  if (batPct >= 0) {
    const batW = 12, batH = 6;
    rx -= batW + 2;
    const batX = rx;
    // Below 10%: rapid blink (faster as battery drops). Hide icon on blink-off frames.
    const batCritical = batPct <= 10 && !bat?.charging;
    const blinkRate = batCritical ? Math.max(4, batPct * 2) : 0; // frames per blink cycle
    const batVisible = !batCritical || (frame % blinkRate < blinkRate / 2);
    if (batVisible) {
      ink(dark ? 90 : 150, dark ? 90 : 150, dark ? 90 : 150);
      box(batX, barY, batW, batH, "outline");
      box(batX + batW, barY + 2, 2, 2, true);
      const fillW = Math.max(0, Math.floor(batPct * (batW - 2) / 100));
      if (batPct <= 10) ink(255, 0, 0);
      else if (batPct <= 20) ink(220, 30, 30);
      else if (batPct <= 50) ink(200, 160, 0);
      else ink(50, 160, 50);
      if (fillW > 0) box(batX + 1, barY + 1, fillW, batH - 2, true);
      // Charging indicator: yellow bolt inside battery
      if (bat?.charging) {
        ink(255, 220, 40);
        box(batX + 4, barY + 1, 2, 2, true);
        box(batX + 3, barY + 3, 2, 2, true);
        box(batX + 5, barY + 2, 1, 1, true);
      }
    }
    const pctStr = (bat?.charging ? "+" : "") + batPct + "%";
    rx -= pctStr.length * CH + 2;
    if (batCritical && !batVisible) {
      ink(255, 0, 0); // red flash on text during blink-off
    } else {
      ink(FG_DIM, FG_DIM, FG_DIM);
    }
    write(pctStr, { x: rx, y: barY, size: 1 });
    rx -= 4; // separator space
  }

  // Time (LA) — uses syncedNow() for accuracy; shows green dot when synced
  {
    const now = new Date(syncedNow());
    const laMs = now.getTime() - getLAOffset() * 3600000;
    const laDate = new Date(laMs);
    const hh = laDate.getUTCHours();
    const mm = laDate.getUTCMinutes();
    const ss = laDate.getUTCSeconds();
    const ampm = hh >= 12 ? "p" : "a";
    const h12 = hh % 12 || 12;
    const timeStr = h12 + ":" + (mm < 10 ? "0" : "") + mm + ":" + (ss < 10 ? "0" : "") + ss + ampm;
    rx -= timeStr.length * CH;
    // Draw clock with green colons when synced
    let cx = rx;
    for (let ci = 0; ci < timeStr.length; ci++) {
      const ch = timeStr[ci];
      if (ch === ":" && clockSynced) {
        ink(60, 220, 80);
      } else {
        ink(FG_DIM, FG_DIM, FG_DIM);
      }
      write(ch, { x: cx, y: barY, size: 1 });
      cx += CH;
    }
    rx -= 4;
  }

  // Volume bar
  const sysVol = sound?.speaker?.systemVolume ?? 100;
  {
    const volW = 20, volH = 3;
    rx -= volW;
    const volBarX = rx;
    ink(dark ? 45 : 220, dark ? 45 : 220, dark ? 50 : 225);
    box(rx, barY + 2, volW, volH, true);
    const fillV = Math.floor(sysVol * volW / 100);
    if (fillV > 0) { ink(dark ? 150 : 80, dark ? 150 : 80, dark ? 150 : 80); box(rx, barY + 2, fillV, volH, true); }
    rx -= 2;
    ink(FG_MUTED, FG_MUTED, FG_MUTED);
    rx -= 3 * CH;
    write("vol", { x: rx, y: barY, size: 1 });
    // Store hit zone for mouse interaction (label + bar)
    globalThis.__volBar = { x: rx, w: volBarX + volW - rx, barX: volBarX, barW: volW };
  }

  // Brightness bar
  const sysBrt = system?.brightness ?? -1;
  if (sysBrt >= 0) {
    rx -= 4;
    const brtW = 16, brtH = 3;
    rx -= brtW;
    const brtBarX = rx;
    ink(dark ? 45 : 220, dark ? 45 : 220, dark ? 50 : 225);
    box(rx, barY + 2, brtW, brtH, true);
    const fillB = Math.floor(sysBrt * brtW / 100);
    if (fillB > 0) { ink(dark ? 180 : 60, dark ? 160 : 60, dark ? 80 : 30); box(rx, barY + 2, fillB, brtH, true); }
    rx -= 2;
    ink(FG_MUTED, FG_MUTED, FG_MUTED);
    rx -= 3 * CH;
    write("brt", { x: rx, y: barY, size: 1 });
    globalThis.__brtBar = { x: rx, w: brtBarX + brtW - rx, barX: brtBarX, barW: brtW };
  }

  // FPS counter (left of brightness)
  if (fpsDisplay > 0) {
    rx -= 4;
    const fpsStr = fpsDisplay + "";
    const fpsColor = fpsDisplay >= 55 ? (dark ? 80 : 180) : (fpsDisplay >= 30 ? (dark ? 200 : 180) : 255);
    const fpsG = fpsDisplay >= 55 ? (dark ? 180 : 180) : (fpsDisplay >= 30 ? (dark ? 160 : 120) : 60);
    ink(fpsColor, fpsG, dark ? 80 : 80);
    rx -= fpsStr.length * CH;
    write(fpsStr, { x: rx, y: barY, size: 1 });
  }

  // === NOTEPAT SCREEN: pads, waveform, echo slider ===
  if (activeScreen === "notepat") {

  // === SPLIT GRID: 4x3 left (bottom-left) + 4x3 right (bottom-right) ===
  const gap = 0;  // marginless — buttons touch for rollover clicking
  const cols = 4, rows = 3;
  // Reserve ~8px on each edge for the per-side master volume sliders that
  // sit to the left of the left grid and right of the right grid.
  const masterSliderW = 7;
  const margin = masterSliderW + 2;

  // Small buttons: ~30% of screen height, anchored to bottom corners
  const maxGridH = Math.floor(h * 0.30);
  const btnH = Math.floor(maxGridH / rows);
  const maxBtnW = Math.floor((w / 2 - margin * 2) / cols);
  const btnW = Math.min(maxBtnW, btnH * 2);

  const gridW = cols * btnW;
  const gridH = rows * btnH;
  const gridTop = h - gridH - margin - 8; // anchor near bottom, leave room for echo bar

  const leftX = margin;
  const rightX = w - gridW - margin;

  // Waveform visualizer bars only in lanes above pad grids (not full-screen).
  const wf = sound?.speaker?.waveforms?.left;
  if (wf && activeCount > 0) {
    const wfTop = topBarH + 1;
    const wfBottom = Math.max(wfTop + 6, gridTop - 2);
    const wfH = wfBottom - wfTop;
    if (wfH > 2) {
      const wfBottomY = wfTop + wfH;
      const amp = Math.floor(wfH * 0.9);
      const barsPerSide = 24;
      const laneW = gridW;
      const barW = Math.max(1, Math.floor(laneW / barsPerSide));
      const vGap = 1;

      const drawLane = (x0, phase) => {
        // Single background tint per lane (not per-bar — avoids alpha overdraw)
        ink(dark ? 30 : 220, dark ? 10 : 200, dark ? 50 : 180, 36);
        box(x0, wfTop, laneW, wfH, true);

        for (let i = 0; i < barsPerSide; i++) {
          const t = (i / barsPerSide) * 0.5 + phase;
          const si = Math.min((wf.length || 1) - 1, Math.floor(t * (wf.length || 1)));
          const sample = Math.abs(wf[si] || 0);
          const barH = Math.max(2, Math.round(sample * amp));
          const barTop = wfBottomY - barH;
          const bx = x0 + i * barW;
          const bw = Math.max(1, Math.min(barW - vGap, x0 + laneW - bx));
          if (bw <= 0 || bx >= x0 + laneW) break;

          // Opaque bar fill (no alpha — fast path)
          ink(dark ? 0 : 200, dark ? 200 : 40, dark ? 255 : 0);
          box(bx, barTop, bw, barH, true);
          ink(dark ? 255 : 255, dark ? 60 : 0, dark ? 180 : 100);
          box(bx, barTop, bw, 2, true);
        }
      };

      drawLane(leftX, 0.0);
      drawLane(rightX, 0.5);
    }
  }

  // Expose grid layout for touch hit-testing in act()
  globalThis.__gridInfo = { leftX, rightX, gridTop, btnW, btnH, gap };

  function drawGrid(grid, startX, octOffset, side) {
    const isPerc = (side === "left" && percussionLeft) || (side === "right" && percussionRight);
    for (let r = 0; r < rows; r++) {
      for (let c = 0; c < cols; c++) {
        const noteName = grid[r][c];
        const [letter, off] = parseNote(noteName);
        const noteOctave = octave + off + octOffset;
        const key = NOTE_TO_KEY[noteName];
        const isActive = key && sounds[key] !== undefined;
        const trailInfo = key && trail[key];
        const sharp = letter.includes("#");
        const drumActive = isPerc && !!PERCUSSION_NAMES[letter];
        const nc = drumActive ? (PERCUSSION_COLORS[letter] || noteColor(letter)) : noteColor(letter);

        const x = startX + c * (btnW + gap);
        const y = gridTop + r * (btnH + gap);
        const isHovered = hoverX >= x && hoverX < x + btnW && hoverY >= y && hoverY < y + btnH;

        if (isActive) {
          ink(nc[0], nc[1], nc[2]);
          box(x, y, btnW, btnH, true);
          ink(255, 255, 255);
        } else if (trailInfo && trailInfo.brightness > 0.05) {
          const b = trailInfo.brightness;
          if (dark) {
            ink(Math.floor(nc[0] * b * 0.4),
                Math.floor(nc[1] * b * 0.4),
                Math.floor(nc[2] * b * 0.4));
          } else {
            ink(Math.floor(255 - (255 - nc[0]) * b * 0.4),
                Math.floor(255 - (255 - nc[1]) * b * 0.4),
                Math.floor(255 - (255 - nc[2]) * b * 0.4));
          }
          box(x, y, btnW, btnH, true);
          ink(dark ? 200 : 40, dark ? 200 : 40, dark ? 200 : 40);
        } else {
          // Idle: note color — sharps are darker (black keys)
          const sharpDim = sharp ? 0.15 : 0.35;
          const sharpDimL = sharp ? 0.08 : 0.3;
          if (dark) {
            ink(Math.floor(nc[0] * sharpDim) + (sharp ? 8 : 20),
                Math.floor(nc[1] * sharpDim) + (sharp ? 8 : 20),
                Math.floor(nc[2] * sharpDim) + (sharp ? 10 : 22));
          } else {
            ink(Math.floor(255 - (255 - nc[0]) * sharpDimL) - (sharp ? 60 : 0),
                Math.floor(255 - (255 - nc[1]) * sharpDimL) - (sharp ? 60 : 0),
                Math.floor(255 - (255 - nc[2]) * sharpDimL) - (sharp ? 60 : 0));
          }
          box(x, y, btnW, btnH, true);
          const outDim = sharp ? 0.25 : 0.5;
          if (dark) {
            ink(Math.floor(nc[0] * outDim) + (sharp ? 15 : 30),
                Math.floor(nc[1] * outDim) + (sharp ? 15 : 30),
                Math.floor(nc[2] * outDim) + (sharp ? 18 : 33));
          } else {
            ink(Math.floor(255 - (255 - nc[0]) * 0.45) - (sharp ? 40 : 0),
                Math.floor(255 - (255 - nc[1]) * 0.45) - (sharp ? 40 : 0),
                Math.floor(255 - (255 - nc[2]) * 0.45) - (sharp ? 40 : 0));
          }
          box(x, y, btnW, btnH, "outline");
          if (isHovered) {
            ink(255, 255, 255, 30);
            box(x, y, btnW, btnH, true);
          }
          const fg = dark ? (sharp ? 120 : 190) : (sharp ? 100 : 50);
          ink(fg, fg, fg);
        }

        const label = key ? key.toUpperCase() : "";
        write(label, { x: x + 2, y: y + 2, size: 1, font: "font_1" });

        if (btnH > 12) {
          if (isActive) ink(255, 255, 255, 180);
          else { const sl = dark ? (sharp ? 80 : 120) : (sharp ? 160 : 110); ink(sl, sl, sl); }
          const bottomLabel = drumActive
            ? (PERCUSSION_LABELS[letter] || (letter + noteOctave))
            : (letter + noteOctave);
          write(bottomLabel, { x: x + 2, y: y + btnH - 12, size: 1, font: "font_1" });
        }

        // Pressure bar — fills from bottom of pad proportional to analog pressure
        if (isActive && pressures && key && pressures[key] !== undefined) {
          const p = pressures[key];
          const barH = Math.floor(p * (btnH - 2));
          if (barH > 0) {
            ink(255, 255, 255, 120);
            box(x + 1, y + btnH - 1 - barH, btnW - 2, barH, true);
          }
        }
      }
    }
  }

  drawGrid(LEFT_GRID, leftX, 0, "left");
  drawGrid(RIGHT_GRID, rightX, 0, "right");

  // === PER-SIDE MASTER VOLUME SLIDERS ===
  // Thin vertical bars flanking each grid. Drag to set the master volume
  // for that side's notes and drums. Fill rises from the bottom so it
  // feels like a level meter at rest.
  const drawVSlider = (sx, sy, sw, sh, value, active, dragging, accent) => {
    // Track background
    ink(dark ? 24 : 226, dark ? 24 : 226, dark ? 30 : 232);
    box(sx, sy, sw, sh, true);
    // Fill (from bottom up)
    const fillH = Math.max(0, Math.floor(sh * Math.max(0, Math.min(1, value))));
    if (fillH > 0) {
      ink(accent[0], accent[1], accent[2], dragging ? 255 : 210);
      box(sx, sy + sh - fillH, sw, fillH, true);
    }
    // Outline
    ink(dark ? 70 : 160, dark ? 70 : 160, dark ? 85 : 170, active ? 255 : 180);
    box(sx, sy, sw, sh, "outline");
    // Unity tick mark
    const unityY = sy + Math.floor(sh * (1 - 1.0));
    if (unityY >= sy && unityY < sy + sh) {
      ink(dark ? 140 : 100, dark ? 140 : 100, dark ? 150 : 110, 160);
      box(sx, unityY, sw, 1, true);
    }
  };
  const leftSliderX = Math.max(0, leftX - masterSliderW - 1);
  const rightSliderX = Math.min(w - masterSliderW, rightX + (cols * btnW) + 1);
  const leftAccent = [60, 200, 180];   // teal
  const rightAccent = [220, 140, 90];  // warm amber
  drawVSlider(leftSliderX, gridTop, masterSliderW, gridH, leftMasterVol,
              hoverX >= leftSliderX && hoverX < leftSliderX + masterSliderW && hoverY >= gridTop && hoverY < gridTop + gridH,
              leftVolDragging, leftAccent);
  drawVSlider(rightSliderX, gridTop, masterSliderW, gridH, rightMasterVol,
              hoverX >= rightSliderX && hoverX < rightSliderX + masterSliderW && hoverY >= gridTop && hoverY < gridTop + gridH,
              rightVolDragging, rightAccent);
  // Expose for act() hit-testing
  globalThis.__leftMasterSlider = { x: leftSliderX, y: gridTop, w: masterSliderW, h: gridH };
  globalThis.__rightMasterSlider = { x: rightSliderX, y: gridTop, w: masterSliderW, h: gridH };

  // Transient percussion toggle notice (shown near top of grid)
  if (percussionNotice && frame < percussionNotice.until) {
    const dark2 = isDark();
    const remaining = percussionNotice.until - frame;
    const alpha = Math.min(255, Math.round(remaining * 3));
    const txt = percussionNotice.text;
    const tw = Math.max(1, txt.length) * 6 + 8;
    const tx = Math.floor((w - tw) / 2);
    const ty = gridTop + 2;
    ink(0, 0, 0, Math.floor(alpha * 0.6));
    box(tx - 1, ty - 1, tw + 2, 12, true);
    ink(dark2 ? 40 : 255, dark2 ? 40 : 255, dark2 ? 55 : 255, Math.floor(alpha * 0.9));
    box(tx, ty, tw, 10, true);
    ink(dark2 ? 240 : 20, dark2 ? 220 : 20, dark2 ? 180 : 60, alpha);
    box(tx, ty, tw, 10, "outline");
    ink(dark2 ? 240 : 30, dark2 ? 230 : 30, dark2 ? 200 : 60, alpha);
    write(txt, { x: tx + 4, y: ty + 1, size: 1, font: "font_1" });
  } else if (percussionNotice) {
    percussionNotice = null;
  }

  // === SLIDERS: fx mix, echo, and pitch (directly under status bar) ===
  const settingsY = topBarH;

  // FX mix slider (dry/wet for entire FX chain)
  {
    const sliderY = settingsY;
    const sliderH = 12;
    const fxHovered = hoverY >= sliderY && hoverY < sliderY + sliderH;
    ink(dark ? (fxHovered ? 40 : 25) : (fxHovered ? 220 : 235),
        dark ? (fxHovered ? 40 : 25) : (fxHovered ? 220 : 235),
        dark ? (fxHovered ? 45 : 28) : (fxHovered ? 225 : 238));
    box(0, sliderY, w, sliderH, true);
    const fillW = Math.floor(fxMix * w);
    if (fillW > 0) {
      ink(120, 200, 80, 180);
      box(0, sliderY, fillW, sliderH, true);
    }
    if (fxMix > 0.005) {
      const knobX = Math.max(1, Math.min(w - 3, Math.floor(fxMix * w)));
      ink(180, 255, 120, 220);
      box(knobX - 1, sliderY, 3, sliderH, true);
    }
    ink(dark ? 90 : 160, dark ? 90 : 160, dark ? 100 : 170);
    const fxStr = "fx " + Math.round(fxMix * 100) + "%";
    write(fxStr, { x: 2, y: sliderY + 2, size: 1, font: "font_1" });
  }

  // Echo slider
  {
    const sliderY = settingsY + 12;
    const sliderH = 12;
    const echoHovered = hoverY >= sliderY && hoverY < sliderY + sliderH;
    ink(dark ? (echoHovered ? 40 : 25) : (echoHovered ? 220 : 235),
        dark ? (echoHovered ? 40 : 25) : (echoHovered ? 220 : 235),
        dark ? (echoHovered ? 45 : 28) : (echoHovered ? 225 : 238));
    box(0, sliderY, w, sliderH, true);
    const fillW = Math.floor(echoMix * w);
    if (fillW > 0) {
      ink(80, 120, 220, trackpadFX ? 240 : 180);
      box(0, sliderY, fillW, sliderH, true);
    }
    if (echoMix > 0.005) {
      const knobX = Math.max(1, Math.min(w - 3, Math.floor(echoMix * w)));
      ink(140, 180, 255, 220);
      box(knobX - 1, sliderY, 3, sliderH, true);
    }
    ink(dark ? 90 : 160, dark ? 90 : 160, dark ? 100 : 170);
    const echoStr = "echo " + Math.round(echoMix * 100) + "%";
    write(echoStr, { x: 2, y: sliderY + 2, size: 1, font: "font_1" });
    if (trackpadFX) {
      ink(120, 220, 120);
      write("\\", { x: w - 8, y: sliderY + 2, size: 1, font: "font_1" });
    }
  }

  // Pitch shift slider
  {
    const sliderY = settingsY + 24;
    const sliderH = 12;
    const pitchHovered = hoverY >= sliderY && hoverY < sliderY + sliderH;
    ink(dark ? (pitchHovered ? 40 : 25) : (pitchHovered ? 220 : 235),
        dark ? (pitchHovered ? 40 : 25) : (pitchHovered ? 220 : 235),
        dark ? (pitchHovered ? 45 : 28) : (pitchHovered ? 225 : 238));
    box(0, sliderY, w, sliderH, true);
    const centerX = Math.floor(w / 2);
    ink(dark ? 40 : 220, dark ? 40 : 220, dark ? 45 : 225);
    box(centerX, sliderY, 1, sliderH, true);
    const pitchX = Math.floor((pitchShift + 1) / 2 * w);
    if (Math.abs(pitchShift) > 0.005) {
      ink(200, 100, 160, trackpadFX ? 240 : 180);
      const fx = Math.min(centerX, pitchX);
      const fw = Math.abs(pitchX - centerX);
      box(fx, sliderY, fw, sliderH, true);
    }
    if (Math.abs(pitchShift) > 0.005) {
      ink(255, 140, 180, 220);
      box(Math.max(1, Math.min(w - 3, pitchX)) - 1, sliderY, 3, sliderH, true);
    }
    ink(dark ? 90 : 160, dark ? 90 : 160, dark ? 100 : 170);
    const cents = Math.round(pitchShift * 1200);
    const pitchStr = "pitch " + (cents >= 0 ? "+" : "") + cents + "c";
    write(pitchStr, { x: 2, y: sliderY + 2, size: 1, font: "font_1" });
  }

  // === WAVE TYPE BUTTONS (below sliders, modular GUI) ===
  {
    const waveRowY = settingsY + 36;
    const waveRowH = 14;
    const waveLabels = ["sine", "tri", "saw", "square", "cmp", "noise", "whist", "sample"];
    const octBtnW = 22;                           // octave button on right
    const waveAreaW = w - octBtnW - 1;
    const btnW2 = Math.floor(waveAreaW / wavetypes.length);

    // Wave type colors: sin=blue, tri=green, saw=orange, sq=purple, ns=red, smp=cyan
    const WAVE_COLORS = [
      [60, 120, 255],  // sine — blue
      [60, 200, 80],   // triangle — green
      [255, 150, 40],  // sawtooth — orange
      [160, 80, 220],  // square — purple
      [255, 180, 220], // composite — pink
      [220, 60, 60],   // noise — red
      [220, 220, 150], // whistle — bone
      [40, 200, 200],  // sample — cyan
    ];

    // Draw each wave button
    for (let i = 0; i < wavetypes.length; i++) {
      const bx = i * btnW2;
      const isActive = wave === wavetypes[i];
      const isHov = hoverX >= bx && hoverX < bx + btnW2 && hoverY >= waveRowY && hoverY < waveRowY + waveRowH;
      const wc = WAVE_COLORS[i];
      if (isActive) {
        ink(dark ? Math.floor(wc[0] * 0.4) + 20 : Math.floor(255 - (255 - wc[0]) * 0.3),
            dark ? Math.floor(wc[1] * 0.4) + 20 : Math.floor(255 - (255 - wc[1]) * 0.3),
            dark ? Math.floor(wc[2] * 0.4) + 20 : Math.floor(255 - (255 - wc[2]) * 0.3));
        box(bx, waveRowY, btnW2, waveRowH, true);
        ink(dark ? 240 : 20, dark ? 250 : 20, dark ? 255 : 30);
      } else if (isHov) {
        ink(dark ? Math.floor(wc[0] * 0.2) + 15 : Math.floor(255 - (255 - wc[0]) * 0.15),
            dark ? Math.floor(wc[1] * 0.2) + 15 : Math.floor(255 - (255 - wc[1]) * 0.15),
            dark ? Math.floor(wc[2] * 0.2) + 15 : Math.floor(255 - (255 - wc[2]) * 0.15));
        box(bx, waveRowY, btnW2, waveRowH, true);
        ink(dark ? 180 : 70, dark ? 180 : 70, dark ? 190 : 80);
      } else {
        ink(dark ? Math.floor(wc[0] * 0.18) + 15 : Math.floor(255 - (255 - wc[0]) * 0.12),
            dark ? Math.floor(wc[1] * 0.18) + 15 : Math.floor(255 - (255 - wc[1]) * 0.12),
            dark ? Math.floor(wc[2] * 0.18) + 15 : Math.floor(255 - (255 - wc[2]) * 0.12));
        box(bx, waveRowY, btnW2, waveRowH, true);
        ink(dark ? Math.floor(wc[0] * 0.55) + 60 : Math.floor(wc[0] * 0.7) + 50,
            dark ? Math.floor(wc[1] * 0.55) + 60 : Math.floor(wc[1] * 0.7) + 50,
            dark ? Math.floor(wc[2] * 0.55) + 60 : Math.floor(wc[2] * 0.7) + 50);
      }
      // Color accent strip at bottom of each button
      ink(dark ? Math.floor(wc[0] * 0.6) : wc[0],
          dark ? Math.floor(wc[1] * 0.6) : wc[1],
          dark ? Math.floor(wc[2] * 0.6) : wc[2],
          isActive ? 200 : 80);
      box(bx, waveRowY + waveRowH - 2, btnW2, 2, true);
      // Separator line between buttons
      if (i > 0) {
        ink(dark ? 40 : 190, dark ? 40 : 190, dark ? 45 : 195);
        box(bx, waveRowY, 1, waveRowH, true);
      }
      // Label
      const lx = bx + Math.floor((btnW2 - waveLabels[i].length * 6) / 2);
      write(waveLabels[i], { x: lx, y: waveRowY + 3, size: 1, font: "font_1" });
    }

    // Octave / REC button (right side)
    const obx = w - octBtnW;
    const octHov = hoverX >= obx && hoverY >= waveRowY && hoverY < waveRowY + waveRowH;
    if (wave === "sample" && recording) {
      // Recording — red pulse with countdown bar
      const recElapsed = (Date.now() - recStartTime) / 1000;
      const recRemain = Math.max(0, MAX_REC_SECS - recElapsed);
      const recFrac = recRemain / MAX_REC_SECS;
      ink(200, 30, 30);
      box(obx, waveRowY, octBtnW, waveRowH, true);
      // Countdown bar (shrinks from full width as time runs out)
      const barH = 2;
      const barW = Math.round((octBtnW - 2) * recFrac);
      ink(255, 80, 80);
      box(obx + 1, waveRowY + waveRowH - barH - 1, barW, barH, true);
      ink(255, 255, 255);
      write(recRemain.toFixed(1), { x: obx + 2, y: waveRowY + 2, size: 1, font: "font_1" });
    } else if (wave === "sample") {
      // REC button (idle)
      ink(dark ? (octHov ? 80 : 50) : (octHov ? 180 : 210),
          dark ? (octHov ? 30 : 20) : (octHov ? 180 : 210),
          dark ? (octHov ? 30 : 20) : (octHov ? 185 : 215));
      box(obx, waveRowY, octBtnW, waveRowH, true);
      ink(dark ? 40 : 190, dark ? 40 : 190, dark ? 45 : 195);
      box(obx, waveRowY, 1, waveRowH, true);
      ink(dark ? 220 : 180, dark ? 80 : 60, dark ? 80 : 60);
      write("REC", { x: obx + 2, y: waveRowY + 3, size: 1, font: "font_1" });
    } else {
      ink(dark ? (octHov ? 50 : 28) : (octHov ? 200 : 225),
          dark ? (octHov ? 50 : 28) : (octHov ? 200 : 225),
          dark ? (octHov ? 55 : 32) : (octHov ? 205 : 230));
      box(obx, waveRowY, octBtnW, waveRowH, true);
      ink(dark ? 40 : 190, dark ? 40 : 190, dark ? 45 : 195);
      box(obx, waveRowY, 1, waveRowH, true);
      ink(dark ? 140 : 100, dark ? 140 : 100, dark ? 150 : 110);
      write("o:" + octave, { x: obx + 3, y: waveRowY + 3, size: 1, font: "font_1" });
      // HOLD indicator (drawn to the LEFT of the octave button)
      if (holdActive) {
        const holdW = 30;
        const hbx = obx - holdW - 2;
        ink(dark ? 80 : 180, dark ? 40 : 100, dark ? 40 : 100);
        box(hbx, waveRowY, holdW, waveRowH, true);
        ink(255, dark ? 180 : 60, dark ? 120 : 40);
        write("HOLD", { x: hbx + 2, y: waveRowY + 3, size: 1, font: "font_1" });
      }
    }

    // Mic level meter in sample mode — compact multi-segment bar next to REC.
    if (wave === "sample") {
      const infoY = waveRowY + waveRowH + 1;
      const level = Math.max(0, Math.min(1, mic.level || 0));
      const len = mic.sampleLength || 0;
      const rate = mic.sampleRate || 0;

      // Compact label: duration when loaded, nothing otherwise
      if (len > 0 && rate > 0) {
        ink(dark ? 120 : 100, dark ? 120 : 100, dark ? 135 : 115);
        write((len / rate).toFixed(1) + "s", { x: 2, y: infoY + 1, size: 1, font: "font_1" });
      }

      // Multi-segment level meter (spans most of the row)
      const meterX = (len > 0 && rate > 0) ? 30 : 2;
      const meterW = obx - meterX - 2;
      const meterH = 7;
      const meterY = infoY + 1;
      const segments = 16;
      const segW = Math.max(2, Math.floor((meterW - segments + 1) / segments));
      const gap = 1;
      const lit = Math.round(level * segments);
      for (let s = 0; s < segments; s++) {
        const sx = meterX + s * (segW + gap);
        if (s < lit) {
          // Green → yellow → red gradient
          if (s < segments * 0.6) ink(60, 200, 80);
          else if (s < segments * 0.85) ink(220, 200, 40);
          else ink(255, 60, 60);
        } else {
          ink(dark ? 30 : 215, dark ? 30 : 215, dark ? 35 : 220);
        }
        box(sx, meterY, segW, meterH, true);
      }
    }

    // Expose hit zones for act()
    globalThis.__waveButtons = { y: waveRowY, h: waveRowH, btnW: btnW2, octX: obx, octW: octBtnW };

  }

  // === DJ DECK STRIP ===
  // One compact line below the wave buttons. Shows play state, track name,
  // position/duration, speed, BPM. Left icon = play/pause button. Right
  // portion is a scratch pad (drag to setSpeed(0, v), release to resume).
  {
    const dk0 = soundAPI?.deck?.decks?.[0] || {};
    const stripY = settingsY + 50; // directly below waveRow (36) + waveRowH (14)
    const stripH = 16;
    const bgR = dark ? 22 : 232, bgG = dark ? 22 : 232, bgB = dark ? 30 : 236;
    ink(bgR, bgG, bgB); box(0, stripY, w, stripH, true);
    ink(dark ? 60 : 180, dark ? 60 : 180, dark ? 70 : 190);
    line(0, stripY + stripH - 1, w, stripY + stripH - 1);

    // Play / pause icon (leftmost square)
    const iconW = 16;
    const playHov = hoverX >= 0 && hoverX < iconW && hoverY >= stripY && hoverY < stripY + stripH;
    if (dk0.playing) ink(80, 200, 120, playHov ? 255 : 200);
    else ink(dark ? 120 : 100, dark ? 120 : 100, dark ? 130 : 110, playHov ? 255 : 180);
    box(1, stripY + 1, iconW - 2, stripH - 2, true);
    ink(dark ? 20 : 240, dark ? 20 : 240, dark ? 25 : 245);
    if (dk0.playing) {
      // pause icon: two vertical bars
      box(5, stripY + 4, 2, stripH - 8, true);
      box(9, stripY + 4, 2, stripH - 8, true);
    } else {
      // play icon: triangle approximated with 3 rows
      for (let yo = 0; yo < 8; yo++) {
        const rowW = 8 - Math.abs(yo - 3) * 2;
        if (rowW > 0) box(5, stripY + 4 + yo, rowW, 1, true);
      }
    }

    // Right side: scratch pad zone (dragged to scratch) — subtle hatch pattern
    const scratchX = iconW + 2;
    const scratchW = w - scratchX;
    const scratchHov = !playHov && hoverY >= stripY && hoverY < stripY + stripH;
    if (djDragging) {
      ink(200, 120, 60, 150);
      box(scratchX, stripY + 1, scratchW, stripH - 2, true);
    } else if (scratchHov) {
      ink(dark ? 35 : 220, dark ? 35 : 220, dark ? 45 : 228);
      box(scratchX, stripY + 1, scratchW, stripH - 2, true);
    }

    // Text: title / pos / dur / speed / BPM
    const title = dk0.loaded ? (dk0.title || "?").replace(/\.[^.]+$/, "") : (djFiles.length > 0 ? "tap scan" : "no usb");
    const pos = djFmt(dk0.position || 0);
    const dur = djFmt(dk0.duration || 0);
    const spd = `${(dk0.speed || 1).toFixed(2)}x`;
    const bpmStr = djDerivedBPM > 0 ? `${djDerivedBPM}bpm` : "--bpm";
    // Left text: title (truncated)
    const maxTitleChars = Math.max(4, Math.floor((scratchW - 100) / 6));
    const shortTitle = title.length > maxTitleChars ? title.slice(0, maxTitleChars - 1) + "…" : title;
    ink(dark ? 220 : 40, dark ? 220 : 40, dark ? 230 : 60);
    write(shortTitle, { x: scratchX + 4, y: stripY + 4, size: 1, font: "font_1" });
    // Right-side meta: pos/dur spd bpm
    const right = `${pos}/${dur} ${spd} ${bpmStr}`;
    const rightX = w - right.length * 6 - 4;
    ink(dark ? 140 : 90, dark ? 150 : 90, dark ? 170 : 110);
    write(right, { x: rightX, y: stripY + 4, size: 1, font: "font_1" });
    // Progress bar (thin) at bottom of strip
    const prog = dk0.duration > 0 ? (dk0.position / dk0.duration) : 0;
    ink(dark ? 60 : 190, dark ? 80 : 200, dark ? 120 : 210, 200);
    box(scratchX, stripY + stripH - 2, Math.max(1, Math.floor(scratchW * prog)), 1, true);

    // Transient deck message (fades out)
    if (djMessage && frame - djMessageFrame < 120) {
      const a = Math.max(0, 255 - Math.floor((frame - djMessageFrame) * 2.5));
      ink(220, 180, 80, a);
      write(djMessage.slice(0, 28), { x: scratchX + 4, y: stripY - 10, size: 1, font: "font_1" });
    }

    // Expose strip layout for act() hit-testing
    globalThis.__djStrip = { x: 0, y: stripY, w, h: stripH, iconW, scratchX, scratchW };
  }

  } // end activeScreen === "notepat"

  // WiFi fullscreen password entry
  if (wifiPasswordMode && wifi) {
    wifiCursorBlink++;
    const nets = wifi.networks || [];
    const net = nets[wifiSelectedIdx];
    const ssid = net ? net.ssid : "?";

    // Fullscreen dark overlay
    ink(dark ? 10 : 240, dark ? 10 : 240, dark ? 15 : 245, 250);
    box(0, 0, w, h, true);

    // Title: network name in matrix chunky (centered estimate: ~9px per char at scale 2)
    ink(FG, FG, FG);
    const titleX = Math.max(10, (w - ssid.length * 18) / 2);
    write(ssid, { x: titleX, y: h / 2 - 50, size: 2, font: "matrix" });

    // "enter password" label
    ink(FG_DIM, FG_DIM, FG_DIM);
    write("enter password:", { x: 20, y: h / 2 - 24, size: 2, font: "font_1" });

    // Password field — visible text with blinking cursor
    const cursor = (wifiCursorBlink % 60) < 35 ? "|" : "";
    const pwDisplay = wifiPassword + cursor;
    ink(FG, FG, FG);

    // Background box for password field
    ink(dark ? 25 : 235, dark ? 25 : 235, dark ? 30 : 240);
    box(18, h / 2 - 6, w - 36, 18, true);
    ink(dark ? 70 : 180, dark ? 70 : 180, dark ? 80 : 190);
    box(18, h / 2 - 6, w - 36, 18, "outline");

    // Password text (visible!)
    ink(FG, FG, FG);
    write(pwDisplay, { x: 22, y: h / 2 - 2, size: 1, font: "font_1" });

    // Instructions
    ink(FG_MUTED, FG_MUTED, FG_MUTED);
    write("Enter: connect    Esc: cancel", { x: 20, y: h / 2 + 22, size: 1, font: "font_1" });

    // Show connection status if connecting
    if (wifi.state === 3) { // WIFI_STATE_CONNECTING
      ink(200, 200, 80);
      write("connecting...", { x: 20, y: h / 2 + 40, size: 1, font: "font_1" });
    } else if (wifi.state === 5) { // WIFI_STATE_FAILED
      ink(220, 80, 80);
      write("failed: " + (wifi.status || "?"), { x: 20, y: h / 2 + 40, size: 1, font: "font_1" });
    }
  }

  // WiFi fullscreen network chooser
  else if (activeScreen === "wifi" && wifi) {
    // Fullscreen dark overlay
    ink(dark ? 10 : 240, dark ? 10 : 240, dark ? 15 : 245, 250);
    box(0, 0, w, h, true);

    // Title
    ink(FG, FG, FG);
    write("WiFi Networks", { x: 20, y: 12, size: 2, font: "matrix" });

    // Status + interface info
    ink(FG_DIM, FG_DIM, FG_DIM);
    const wifiStatusStr = (wifi.status || "scanning...") + (wifi.iface ? " [" + wifi.iface + "]" : "");
    write(wifiStatusStr, { x: 20, y: 30, size: 1, font: "font_1" });

    // Build merged network list: scanned first (deduplicated), then saved/preset not in scan
    const rawNets = wifi.networks || [];
    // Deduplicate by SSID — keep the entry with strongest signal
    const ssidBest = new Map();
    for (const n of rawNets) {
      if (!n.ssid) continue;
      const prev = ssidBest.get(n.ssid);
      if (!prev || n.signal > prev.signal) ssidBest.set(n.ssid, n);
    }
    const scannedNets = [...ssidBest.values()].sort((a, b) => b.signal - a.signal);
    const scannedSSIDs = new Set(scannedNets.map((n) => n.ssid));
    const allSaved = [
      { ssid: AC_SSID, pass: AC_PASS },
      ...savedCreds.filter((c) => c.ssid !== AC_SSID),
    ];
    // Saved networks not currently visible in scan
    const offlineSaved = allSaved.filter((c) => !scannedSSIDs.has(c.ssid));

    const rowH = 16;
    const listY = 44;
    const totalRows = scannedNets.length + (offlineSaved.length > 0 ? 1 + offlineSaved.length : 0);
    const maxRows = Math.min(totalRows, Math.floor((h - listY - 30) / rowH));
    // Store merged list for touch handler
    globalThis.__wifiMergedList = [];

    let row = 0;
    // Scanned networks
    for (let i = 0; i < scannedNets.length && row < maxRows; i++, row++) {
      const net = scannedNets[i];
      const ry = listY + row * rowH;
      const isSaved = allSaved.find((c) => c.ssid === net.ssid);

      const isSelected = row === wifiSelectedIdx;
      if (isSelected) {
        ink(dark ? 40 : 210, dark ? 55 : 215, dark ? 80 : 230);
        box(10, ry, w - 20, rowH, true);
      }

      // Signal bars
      const bars = net.signal > -50 ? 4 : net.signal > -60 ? 3 : net.signal > -70 ? 2 : 1;
      for (let b = 0; b < 4; b++) {
        if (b < bars) ink(80, 200, 80);
        else ink(dark ? 40 : 220, dark ? 40 : 220, dark ? 45 : 225);
        box(16 + b * 4, ry + 10 - (b + 1) * 2, 3, (b + 1) * 2, true);
      }

      // SSID name (green tint if saved/known)
      if (isSaved) ink(100, 220, 100);
      else ink(FG, FG, FG);
      const ssidDisplay = net.ssid.length > 26 ? net.ssid.slice(0, 25) + "~" : net.ssid;
      write(ssidDisplay, { x: 36, y: ry + 2, size: 1, font: "font_1" });

      // Right side: "tap to connect" if selected, else saved badge / lock icon
      if (isSelected) {
        ink(80, 180, 255);
        write("connect", { x: w - 52, y: ry + 2, size: 1, font: "font_1" });
      } else if (isSaved) {
        ink(60, 160, 60);
        write("saved", { x: w - 40, y: ry + 2, size: 1, font: "font_1" });
      } else if (net.encrypted) {
        ink(FG_MUTED, FG_MUTED, FG_MUTED);
        write("*", { x: w - 20, y: ry + 2, size: 1, font: "font_1" });
      }

      globalThis.__wifiMergedList.push({ type: "scan", idx: i, ssid: net.ssid, encrypted: net.encrypted });
    }

    // Saved/preset networks section (not in scan)
    if (offlineSaved.length > 0 && row < maxRows) {
      const sepY = listY + row * rowH;
      ink(FG_MUTED, FG_MUTED, FG_MUTED);
      write("-- saved (not in range) --", { x: 20, y: sepY + 2, size: 1, font: "font_1" });
      globalThis.__wifiMergedList.push({ type: "separator" });
      row++;

      for (let i = 0; i < offlineSaved.length && row < maxRows; i++, row++) {
        const cred = offlineSaved[i];
        const ry = listY + row * rowH;
        const isPreset = cred.ssid === AC_SSID;

        const isSelSaved = row === wifiSelectedIdx;
        if (isSelSaved) {
          ink(dark ? 35 : 225, dark ? 40 : 230, dark ? 55 : 240);
          box(10, ry, w - 20, rowH, true);
        }

        // No signal bars — show dim placeholder
        for (let b = 0; b < 4; b++) {
          ink(dark ? 30 : 230, dark ? 30 : 230, dark ? 35 : 235);
          box(16 + b * 4, ry + 10 - (b + 1) * 2, 3, (b + 1) * 2, true);
        }

        // SSID in dim color
        ink(dark ? 120 : 160, dark ? 120 : 160, dark ? 130 : 170);
        write(cred.ssid, { x: 36, y: ry + 2, size: 1, font: "font_1" });

        // Right side: "connect" if selected, else label
        if (isSelSaved) {
          ink(80, 180, 255);
          write("connect", { x: w - 52, y: ry + 2, size: 1, font: "font_1" });
        } else if (isPreset) {
          ink(80, 120, 160);
          write("preset", { x: w - 44, y: ry + 2, size: 1, font: "font_1" });
        } else {
          ink(60, 140, 60);
          write("saved", { x: w - 40, y: ry + 2, size: 1, font: "font_1" });
        }

        globalThis.__wifiMergedList.push({ type: "saved", ssid: cred.ssid, pass: cred.pass });
      }
    }

    // Connected IP + latest AC message at bottom
    if (wifi.connected && wifi.ip) {
      ink(80, 200, 80);
      const sshLabel = system.sshStarted ? "  ssh root@" + wifi.ip : "";
      write("connected: " + wifi.ip + sshLabel, { x: 20, y: h - 26, size: 1, font: "font_1" });
      if (acMsg) {
        ink(FG_DIM, FG_DIM, FG_DIM);
        const preview = (acMsg.from + ": " + acMsg.text).slice(0, 52);
        write(preview, { x: 20, y: h - 16, size: 1, font: "font_1" });
      } else if (system.ws?.connecting) {
        ink(FG_MUTED, FG_MUTED, FG_MUTED);
        write("connecting to chat...", { x: 20, y: h - 16, size: 1, font: "font_1" });
      } else if (system.ws?.connected) {
        ink(FG_MUTED, FG_MUTED, FG_MUTED);
        write("loading chat...", { x: 20, y: h - 16, size: 1, font: "font_1" });
      }
    }

    // Instructions
    ink(FG_MUTED, FG_MUTED, FG_MUTED);
    write("Esc: close", { x: w - 60, y: h - 26, size: 1, font: "font_1" });

    // Rescan timer — only when not connected (avoid disrupting active connection)
    if (frame % 600 === 0 && activeScreen === "wifi" && !wifi.connected) {
      wifi.scan();
    }
  }

  // Chat preview removed from bottom — shown only in status bar at top


  // Fade trails
  Object.keys(trail).forEach((key) => {
    if (!sounds[key]) {
      trail[key].brightness *= 0.92;
      if (trail[key].brightness < 0.02) delete trail[key];
    }
  });

  // 🧚 Fairy co-presence — send cursor + paint received fireflies
  if (system.udp?.connected) {
    maybeSendUdpMidiHeartbeat(system);
    // Send current cursor/touch position as fairy point (~60Hz, throttled by UDP thread)
    if (hoverX >= 0 && hoverY >= 0) {
      system.udp.sendFairy(hoverX / w, hoverY / h);
    }
    // Paint received fairies as glowing dots
    const fairies = system.udp.fairies;
    if (fairies && fairies.length > 0) {
      for (let i = 0; i < fairies.length; i++) {
        const fx = Math.round(fairies[i].x * w);
        const fy = Math.round(fairies[i].y * h);
        // Outer glow
        ink(255, 200, 80, 40);
        box(fx - 3, fy - 3, 7, 7, true);
        // Inner bright dot
        ink(255, 240, 120, 180);
        box(fx - 1, fy - 1, 3, 3, true);
        // Center pixel
        ink(255, 255, 200);
        box(fx, fy, 1, 1, true);
      }
    }
  }

  // HDMI output: blend active note colors → solid fill on secondary display
  if (system.hasHdmi) {
    const activeKeys = Object.keys(sounds);
    if (activeKeys.length > 0) {
      let tr = 0, tg = 0, tb = 0;
      for (const k of activeKeys) {
        const s = sounds[k];
        if (s && s.note) {
          const c = noteColor(s.note);
          tr += c[0]; tg += c[1]; tb += c[2];
        }
      }
      const n = activeKeys.length;
      system.hdmi(Math.round(tr / n), Math.round(tg / n), Math.round(tb / n));
    } else {
      system.hdmi(0, 0, 0);
    }
  }

  // Help panel overlay (Meta/Win key) — drawn last so it sits on top of everything
  if (helpPanel) {
    const dark = isDark();
    const padX = 8, padY = 8;
    const lineH = 11;
    const shortcuts = [
      ["a–l, ; '",      "play notes (with sharps)"],
      ["1–9 / ↑↓",      "octave"],
      ["pgup / pgdn",   "drum kit L / R octave"],
      ["space",          "kick drum"],
      ["tab",            "cycle wave type"],
      ["shift",          "quick mode"],
      ["Fn+F1",         "deck play/pause"],
      ["Fn+F2",         "deck next track"],
      ["Fn+F3",         "deck prev track"],
      ["Fn+F4",         "deck usb rescan"],
      ["[ / `",         "deck speed − / reset"],
      [", tap tempo",   "sync metronome to deck"],
      ["drag deck",     "scratch the platter"],
      ["F9",             "metronome"],
      ["F10 (📞)",       "clear hold"],
      ["F11 (📞)",       "engage / flourish hold"],
      ["F12 (★)",        "recital mode (hide UI)"],
      ["meta (⊞)",       "toggle this help"],
      ["esc esc esc",    "exit to prompt"],
      ["- / =",          "metronome BPM"],
      ["home (sample)",  "record global sample"],
      ["end (sample)",   "arm per-key / per-drum rec"],
      ["\\",             "trackpad FX (X echo, Y pitch)"],
    ];
    // Compute panel size
    const titleH = 14;
    const panelW = Math.min(w - 20, 280);
    const panelH = titleH + lineH * shortcuts.length + padY * 2;
    const px = Math.floor((w - panelW) / 2);
    const py = Math.floor((h - panelH) / 2);
    // Background with shadow
    ink(0, 0, 0, 140); box(px + 3, py + 3, panelW, panelH, true);
    ink(dark ? 22 : 240, dark ? 22 : 240, dark ? 28 : 245); box(px, py, panelW, panelH, true);
    ink(dark ? 80 : 100, dark ? 80 : 100, dark ? 100 : 130); box(px, py, panelW, panelH, "outline");
    // Title
    ink(dark ? 220 : 40, dark ? 220 : 40, dark ? 230 : 60);
    write("notepat shortcuts", { x: px + padX, y: py + padY, size: 1, font: "font_1" });
    ink(dark ? 80 : 160, dark ? 80 : 160, dark ? 100 : 180);
    line(px + padX, py + padY + 11, px + panelW - padX, py + padY + 11);
    // Shortcuts list
    let lineY = py + padY + titleH;
    for (const [k, desc] of shortcuts) {
      ink(dark ? 180 : 60, dark ? 200 : 80, dark ? 220 : 100);
      write(k, { x: px + padX, y: lineY, size: 1, font: "font_1" });
      ink(dark ? 130 : 80, dark ? 130 : 80, dark ? 145 : 100);
      write(desc, { x: px + padX + 90, y: lineY, size: 1, font: "font_1" });
      lineY += lineH;
    }
    // Hint at bottom
    ink(dark ? 90 : 130, dark ? 90 : 130, dark ? 110 : 150);
    write("press meta (⊞) again to close", { x: px + padX, y: py + panelH - padY - 5, size: 1, font: "font_1" });
  }
}

function sim({ pressures, sound }) {
  // Flush any due setTimeout callbacks (polyfill for missing QuickJS timer)
  __tickPendingTimeouts();
  // Auto-stop recording at max duration
  if (recording && (Date.now() - recStartTime) / 1000 >= MAX_REC_SECS) {
    stopSampleRecording(sound, "max-duration");
  }
  // Update dark/light mode via global theme (every ~5 seconds)
  if (frame % 300 === 0) {
    const wasDark = dark;
    __theme.update();
    dark = __theme.dark;
    if (dark !== wasDark) bgTarget = dark ? [20, 20, 25] : [240, 238, 232];
  }

  // === DJ DECK MAINTENANCE ===
  // Hot-plug check every ~3 seconds, auto-advance when tracks end.
  if (frame - djLastUsbCheckFrame > 180) {
    djLastUsbCheckFrame = frame;
    const nowMounted = systemAPI?.mountMusic?.() || false;
    if (nowMounted && !djUsbConnected) {
      djUsbConnected = true; djMounted = true;
      sound?.speak?.("USB dj on");
      djScan(systemAPI, null);
      if (djFiles.length > 0) { djTrackIdx = 0; djLoadTrack(sound); }
    } else if (!nowMounted && djUsbConnected) {
      djUsbConnected = false;
      const dk = sound?.deck;
      if (dk?.decks?.[0]?.playing) dk.pause(0);
      djFiles = [];
      sound?.speak?.("USB dj off");
      djMsg("USB removed");
    }
  }
  // Auto-advance when the current track finishes
  const __dkD = sound?.deck?.decks?.[0];
  if (__dkD?.loaded && !__dkD.playing && __dkD.duration > 0 && __dkD.position >= __dkD.duration - 0.1 && !djDragging) {
    djTrackIdx++;
    djLoadTrack(sound);
  }
  // Spin the little deck indicator even when nothing else is happening
  if (__dkD?.playing && !djDragging) djAngle += (__dkD.speed || 1) * 0.05;

  // Continuously update synth volumes from analog key pressure AND from
  // the current per-side master volume so dragging the L/R master sliders
  // live-adjusts sustained notes on that side.
  for (const key of Object.keys(sounds)) {
    const entry = sounds[key];
    if (!entry?.synth) continue;
    const master = entry.gridOffset === 1 ? rightMasterVol : leftMasterVol;
    const p = pressures?.[key]; // 0.0-1.0 analog pressure, undefined if not analog
    if (p !== undefined) {
      const vol = p * 0.7 * master; // Linear: 0 pressure = silence, full press × master
      if (entry.__lastVol !== vol) {
        entry.synth.update({ volume: vol });
        entry.__lastVol = vol;
      }
      if (trail[key]) trail[key].brightness = p;
    } else {
      // Digital key — only react to master volume changes, not pressure.
      if (entry.__lastMaster !== master) {
        const baseVol = entry.baseVol ?? 0.7;
        entry.synth.update({ volume: baseVol * master });
        entry.__lastMaster = master;
      }
    }
  }
}

function leave() {
  stopSampleRecording(soundAPI, "leave");
  // Reset all FX before shutdown
  echoMix = 0;
  pitchShift = 0;
  fxMix = 1;
  trackpadFX = false;
  soundAPI?.room?.setMix?.(0);
  soundAPI?.fx?.setMix?.(1);
  stopAllSounds(soundAPI, systemAPI, 0.02);
}

export { boot, act, paint, sim, leave };
