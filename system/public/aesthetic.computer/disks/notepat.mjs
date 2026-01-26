// Notepat, 2024.6.26.23.17.58.736
// Tap the pads to play musical notes, or use the keyboard keys.

import {
  getNoteColorWithOctave,
  parseNotepatNote,
  isBlackKey,
} from "../lib/note-colors.mjs";
import { drawMiniControllerDiagram } from "../lib/gamepad-diagram.mjs";

/* 📝 Notes 
   - [] Make `slide` work with `composite`.
        (This may require some refactoring)
   - [] Add recordable samples / custom samples... per key?
     - [] Stored under handle?
   - [🟠] Somehow represent both of these in the graphic layout.
    - [x] Add z, x, a# and b below the lower octave. 
    - [x] Add ;, ', c# and d above the upper octave. 
   - [] Learn Tattooine theme.
   - [x] Shift octave on either half  up or down.
    - [x] Test it and see if it's fun.
    - [] Touch?
    - [x] Keyboard shortcuts.
  - [x] Add reverb that's only activated for the notepat sounds.
       (Implemented via sound.room API and / key toggle)
  - [] Add tappable touch bar for toggling the visualizer view. 
  - [🟠] Only show keyboard shortcuts once any keyboard key is pressed (on Android or iOS).
  *** Song ***
  - [] Song should loop visually, n times to the right as needed.
  - [] Holding space bar should always do the current / next note.
       (This shouldn't conflict with keys or mouse.)
  - [] Add a visual progress bar <------------------------------>
  - [] Make it so you can swipe through next or previous notes if one was
       skipped.
  -----------------------
  - [-] Encode a few more melodies with lyrics on the command line.
    - [] Make sure that transposition will work.
  - [-] Fix the single corner cross threshold.
  - [] Fix the keyboard + mouse key conflict that keeps sounds sticking
       in the upper octaves.
  - [] Add volume centroid control to buttons with draggable changes.
  - [] Add colon parameter ':lock' for hiding the ui to prevent accidental touches.
  - [] Add perc buttons to `beatpat` along with a QR code similar to notepat.
    - [] This could use the new button interface.
  - [] Make oscilloscope into a button for going fullscreen with wireframe keys.
    - [❓] I wonder if there's a way this can also be placed in a local secondary window
         using inter-frame communication of some kind, perhaps through the 'oscilloscope'
         piece which could pick up all local frame messages for the oscilloscope
         and be customizable in ways.
  + Later
  - [?] Dragging across the buttons in slide mode should slide from one key to another? 
  - [] Add record button and repeat button with a masking mode and potentially a stepper and "sing" syllable mode for adding words, followed by a way to store tracks.
  - [] Add scale overlay selection with visual hiking paths. (Color themes).
  - [?] Add longer / customizable slide duration.
  - [] Lay out keys better on wider vs vertical screen.
  - [] Add multiple tracks so that I can create "systems" that loop
       with different lengths.
  - [] Add holdable rhythm button with patterns that "cycle".
  - [] Add live highlights back in type mode that include note tokenization.
  - [] Pressing a new button down should automatically lift the other one.
  - [] Add a 'repeat' or 'hold' key which should be 'shift' on the keyboard
  - [] Add the tone-sliding that song has during tap mode. 
    - [] This could be microtonal, or shift / quantize to the next note.
    - [] There should be a way to save this modification or 'clear' it
         while playing so melodies can deviate somehow, then return to the same
         location.
  - [] Fix 'tap' mode keypresses... or disable them entirely?
    - [] Is it weirdly playable this way?
  - [] Also add support for microtonal elements in the notation. 
  - [] why are some wave type switches changing the frequency / octave?
  - [] how to build commas or sections in?
  - [] how can i "lightly" trigger the soft keyboard without a whole
        text input class... this would be useful for mobile game controls
        and such and may require a tap to open?
  - [] add restart key?
  - [] entering '0' should clear the params in the hud (re-enable editing)
  - [] be able to send that track / share it with others (via the first parameter) and the 'share' swipe that could pre-fill it.
  - [] use other letters on the keyboard for sfx
  - [] show octave in the text
  - [] tap the top right corner to switch modes...
  - [] add the pitch sliding in
  - [] show audio connectedness more visibly 
       like with a modal or 'press any key'
       or 'inactive' pause curtain situation
  - [] Bring accents back to notes.
  - [] make a memory mode a-la sage, starting with 1 note patterns
    - [] parameters are 'number of notes' and experiment with 'max length'
    - [] difficulty / note-length should increase automatically or it could
         just re-inforce
    - [] this would use the read-through mode, but be dynamically
         generating text with a 'teaches typing' like modality.
  - [] mobile key entry needs to work
  - [] represent number keys / octave somehow...
  - [] Make it work with the phone keyboard, which means number keys...
  - [] Add rhythm...
  - [] Add the ability to run through a series of notes / an existing song.
  - [] Show some form of display / color coded display to show octave state and also print the last note pressed. 
  - [] Show a melody so that a typing game can take place.
  - [] Abstract this so shift adds the 5.
  - [] Leave out all options from synth / make sensible defaults first.
  - [] Add 'scale' and 'rotation' to `write`.
  + Done
  - [x] Make a new "painting" to use as a visualizer.
   - [x] Draw symmetrical turtle graphics based on waveform values that
        paint continuously and produce consistent and differential form.
  - [x] Send udp messages from keys to `tv`.
  - [c] Add nice `composite` mode and rudimentary `sample` mode.
  - [x] Make a more mobile friendly layout.
    - [x] Everything needs to fit for the jelly.
    - [x] Buttons should always fit on screen no matter what.
  - [x] Add a new playback mode for melodies that include words... with long
       command line parameters.
  - [x] Add simple corner button for changing wavetype.
  - [x] Add simple corner button for changing octave.
  - [wip] Color code the notes and semitones, each with a color that has a specific and known name.
  - [c] Add an always active QR code.
  - [x] Add -3, -2, -1, +1, +2, +3 etc. relative overlays based on press and
       have it span the whole octave.
  - [x] Fix '+C' notes appearing in the key list during playback mode.
    - [x] on keyboard
    - [x] on touch / mouse 
  - [x] Fix subtle 1, 2, 3, 4, then release 1 and press 1 down and watch 4 get unticked touch bug on ios. 
    - [x] This may require fixing localhost testing first.
  - [x] Ghost trails.
  - [x] Rename to `notepat`.
  - [x] Fix slide mode.
  - [x] Fix 5 finger touch bug.
  - [x] Add basic percussion with kick, snare and hi-hat on space, shift and control.
  - [x] Add keyboard and button preview for second octave.
  - [x] Add better mixing when multiple keys are being pressed.
  - [x] When lifting, don't cancel other buttons.
  - [x] Reflow the mobile button layout so it is responsive (keep a grid) 
  - [x] slide only? sticky keys error... log all playing keys / sounds in slide mode / print them out to make sure about doubles...
  - [x] Implement the full scale with rollover.
  - [x] Experiment with a monovoice/slide/replace note mode.
  - [x] Get 'slide' working on the software buttons.
  - [x] Lift up should slide back.
  - [x] Fix ui button issue. 
  - [x] Tapping buttons on the layout should add notes to the notepad.
  - [x] Software buttons and keyboard buttons should not overlap and pushing
        keyboard buttons should activate the ui.
        - [x] Holding a ui button down should prevent that existing / exact note
            from being triggered.
        - [x] Holding a keyboard button down should prevent the ui button
            from being pressable.
  - [x] Implement one or two buttons.
  - [x] Match the tones properly 440 in other software should be 440 here and
         2092 should sound the same, etc.
  - [x] Solve the 'note' / 'tone'  syntax ordering API issue.
  - [x] Better 'red' preview highlighting.
  - [x] Entering number key more than once should not add extra numbers.
  - [x] Show accurate preview of top note.
  - [x] Add support for sharps and flats using normal keys somehow...
    - [x] And they can be encoded differently.
    - [x] How would the interpreter work for these, because it only reads per
         character...
  - [x] auto advance octaves... 
  - [x] change the bg color while any note is held.
  - [] clean up digit vs note triggering code...
  - [x] holding tab should not auto-switch, same with 0!
  - [x] press 'enter' 
  - [x] be able to turn that history into a 'track' that can be followed
    - [x] this will enable a type-to-repeat, or... 
        'space' to skip mode, where space will automatically hit
        the next note after an octave key.
  - [x] Show octaves in keys pressed.
  - [x] show a history of keys pressed
  - [x] Add visualization to the keys being pressed.
  - [x] Disable key repeat / don't retrigger sounds on repeat.
  - [x] Make it so keys can be held, and add a decay after releasing?
*/

// ⚖️ Keyboard Scales
// CvDsEFrGtAwB (First Octave)
// BwAtGrFEsDvC (Second Octave)

/* 🎶 Sequences

TODO: 💮 Daisy

🚫 🪀 Toys R Us Kid
  bababg (i don't wanna grow up)
  bb (i'm a)
  abaf# (toys r us kid)

  f# f# g# f# (there's a million)
  g# f# g# f# e (toys at toys r us)


✅ 🐦 Lullaby
  eeg eeg
  eg5c4b aa g
  def ddef
  dfbag b5c
  4cc5c 4afg
  ecf gag
  cc5c 4afg
  ecfedc

✅ 🌟 Twinkle Twinkle Little Star
  cc gg aa g
  ff ee dd c
  gg ff ee d
  gg ff ee d
  cc gg aa g
  ff ee dd c

✅ 🐭 Three Blind Mice
  eddc eddc
  gffe gffe
  g5cc4bab5c4gg
  g5cc4bab5c4gg
  edc edc

✅ Row Row Row Ur Boat
  cccde edefg
  5ccc 4ggg eee ccc gfedc

✅ 🧸 Gummy Bear
  aaa5c4aa aaa5cee
  eee dd dd dd dd
  edc4a g5c4a
  aa ee aa ee aa e
  5ee ee e dc
  5ee ee e dc
  ge dedc
  dc4a g5ca
*/

// import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";
// import { Android, iOS } from "../lib/platform.mjs";

let STARTING_OCTAVE = "4";
const wavetypes = [
  "sine", // 0
  "triangle", // 1
  "sawtooth", // 2
  "square", // 3
  "noise", // 4 - white noise filtered by pitch
  "composite", // 5
  "stample", // 6
];
let waveIndex = 0; // 0;
const STARTING_WAVE = wavetypes[waveIndex]; //"sine";
let wave = STARTING_WAVE;
// let hold = false;
let slide = false;
let quickFade = false;
let roomMode = false; // 🏠 Global reverb toggle
let glitchMode = false; // 🧩 Global glitch toggle
let octave = STARTING_OCTAVE;
let keys = "";
let tap = false;
let tapIndex = 0;
let tapped; // Store the last tracked key.
let editable = true;
const downs = {}; // Hardware keys that are currently held down.
const sounds = {};
const tonestack = {}; // Temporary tone-stack that always keeps currently held
//                       keys and pops them off the stack as keys are lifted.
//                       (These tones will not necessarily be playing.)

// 🔬 Telemetry: Expose state for stability testing
if (typeof window !== 'undefined') {
  window.__notepat_sounds = sounds;
  window.__notepat_tonestack = tonestack;
}

// 📊 Performance OSD (On-Screen Display)
let perfOSD = false; // Toggle with ` (backtick) key - starts OFF
let lowLatencyMode = false;
let audioReinitRequested = false;
let pendingAudioReinit = false;
let bootTimestamp = 0; // Set in boot() to detect pre-boot stuck notes
let storedSampleRate = null; // Store sample rate at boot for layout calculations
let miniInputsEnabled = true; // Show mini piano/qwerty when there is space
let paintPerfEnabled = false; // Console timing logs for paint
let paintPerfEvery = 60; // Log every N frames
let layoutCache = {
  key: null,
  layout: null,
  midiMetrics: null,
};
let waveformsCache = {
  source: null,
  scopeSamples: 0,
  resampled: null,
  sanitized: null,
};
let zeroWaveformsCache = {
  length: 0,
  buffer: [],
};
let padsBase = null;
let padsBaseKey = null;

// 🎨 Color cache for performance - cleared when octave changes
let colorCache = new Map();
let colorCacheOctave = null;

function getCachedColor(note, num, forceOctave = null) {
  const currentOctave = forceOctave ?? octave;
  // Clear cache if octave changed
  if (colorCacheOctave !== currentOctave) {
    colorCache.clear();
    colorCacheOctave = currentOctave;
  }
  const key = `${note}:${currentOctave}`;
  let color = colorCache.get(key);
  if (!color) {
    color = colorFromNote(note, num, currentOctave);
    colorCache.set(key, color);
  }
  return color;
}

const perfStats = {
  lastKeyTime: 0,        // When the last key was pressed
  lastSoundTime: 0,      // When the sound started playing (synth call)
  latency: 0,            // Key-to-synth latency in ms (JS-side only)
  latencyHistory: [],    // Rolling history of latencies
  avgLatency: 0,         // Average latency
  maxLatency: 0,         // Max latency seen
  minLatency: Infinity,  // Min latency seen
  soundCount: 0,         // Current number of active sounds
  tonesInStack: 0,       // Tones in tonestack
  keysLength: 0,         // Length of keys string
  frameTime: 0,          // Last frame time
  fps: 0,                // Estimated FPS
  lastFrameTimestamp: 0, // For FPS calculation
  memoryUsed: 0,         // JS heap used (if available)
  sampleRate: 0,         // Audio context sample rate (kHz)
  baseLatency: 0,        // AudioContext base latency (ms)
  outputLatency: 0,      // AudioContext output latency (ms)
};

// 🔬 Expose perfStats for testing
if (typeof window !== 'undefined') {
  window.__notepat_perfStats = perfStats;
}

//let sharps = false,
//  flats = false;
const notes = "cdefgab" + "vswrq" + "hijklmn" + "tyuop"; // hold shift on C D F G A for sharps.
const edges = "zx;']"; // below and above the octave
//                              cdefgab (next ovtave)
//                       // or alt on   D E G A B for flats
// This is a notes -> keys mapping, that uses v for c#

const TOP_BAR_BOTTOM = 21;
const TOP_BAR_PIANO_HEIGHT = 10; // Mini piano strip in top bar
const TRACK_HEIGHT = 25;
const TRACK_GAP = 6;
const MINI_KEYBOARD_HEIGHT = 16;
const MINI_KEYBOARD_SPACING = 6;
const QWERTY_MINIMAP_SPACING = 6;

const MIDI_BADGE_TEXT = "USB MIDI";
const MIDI_RATE_LABEL_TEXT = "SR";
const MIDI_BADGE_PADDING_X = 2;
const MIDI_BADGE_PADDING_RIGHT = 0;
const MIDI_BADGE_PADDING_Y = 2;
const MIDI_BADGE_MARGIN = 2;
const MIDI_RATE_LABEL_GAP = 2;

const QWERTY_MINIMAP_KEY_HEIGHT = 8;
const QWERTY_MINIMAP_KEY_SPACING = 1;

// Secondary top bar for mode toggle buttons (using MatrixChunky8 font)
const SECONDARY_BAR_TOP = TOP_BAR_BOTTOM;
const SECONDARY_BAR_HEIGHT = 12;
const SECONDARY_BAR_BOTTOM = SECONDARY_BAR_TOP + SECONDARY_BAR_HEIGHT;
const TOGGLE_BTN_PADDING_X = 2;
const TOGGLE_BTN_PADDING_Y = 2;
const TOGGLE_BTN_GAP = 2;

// Responsive label shortening for tight layouts
const LABEL_VARIANTS = {
  slide: ["slide", "sld", "s"],
  room: ["room", "rm", "r"],
  glitch: ["glitch", "glt", "g"],
  quick: ["quick", "qk", "q"],
};

// Current chosen labels (set by buildSecondaryBarLayout)
let secondaryBarLabels = {
  slide: "slide",
  room: "room", 
  glitch: "glitch",
  quick: "quick",
};

const MELODY_ALIAS_BASE_SIDE = 72;
const MELODY_ALIAS_MIN_SIDE = 56;
const MELODY_ALIAS_MARGIN = 6;

const NOTE_TO_KEYBOARD_KEY = {
  "-a": "control",
  "-a#": "z",
  "-b": "x",
  c: "c",
  "c#": "v",
  d: "d",
  "d#": "s",
  e: "e",
  f: "f",
  "f#": "w",
  g: "g",
  "g#": "r",
  a: "a",
  "a#": "q",
  b: "b",
  "+c": "h",
  "+c#": "t",
  "+d": "i",
  "+d#": "y",
  "+e": "j",
  "+f": "k",
  "+f#": "u",
  "+g": "l",
  "+g#": "o",
  "+a": "m",
  "+a#": "p",
  "+b": "n",
  "++c": ";",
  "++c#": "'",
  "++d": "]",
};

const KEYBOARD_TO_NOTE = Object.fromEntries(
  Object.entries(NOTE_TO_KEYBOARD_KEY).map(([note, key]) => [key, note]),
);

const QWERTY_LAYOUT_ROWS = [
  ["q", "w", "e", "r", "t", "y", "u", "i", "o", "p", "]"],
  ["a", "s", "d", "f", "g", "h", "j", "k", "l", ";", "'"],
  ["control", "z", "x", "c", "v", "b", "n", "m", "alt"],
  ["left", "down", "up", "right", "space"],
];

const QWERTY_MINIMAP_HEIGHT =
  QWERTY_LAYOUT_ROWS.length * QWERTY_MINIMAP_KEY_HEIGHT +
  (QWERTY_LAYOUT_ROWS.length - 1) * QWERTY_MINIMAP_KEY_SPACING;

const QWERTY_ROW_OFFSETS = [0, 0.5, 1];
const QWERTY_MAX_SPAN = Math.max(
  ...QWERTY_LAYOUT_ROWS.map((row, i) => row.length + QWERTY_ROW_OFFSETS[i]),
);
const PAN_RANGE = 0.9; // Keep a safe center mix; never hard L/R.

const MINI_PIANO_WHITE_KEYS = [
  "-A",
  "-B",
  "C",
  "D",
  "E",
  "F",
  "G",
  "A",
  "B",
  "+C",
  "+D",
  "+E",
  "+F",
  "+G",
  "+A",
  "+B",
  "++C",
  "++D",
];

const MINI_PIANO_BLACK_KEYS = [
  { note: "-A#", afterWhite: 0 },
  { note: "C#", afterWhite: 2 },
  { note: "D#", afterWhite: 3 },
  { note: "F#", afterWhite: 5 },
  { note: "G#", afterWhite: 6 },
  { note: "A#", afterWhite: 7 },
  { note: "+C#", afterWhite: 9 },
  { note: "+D#", afterWhite: 10 },
  { note: "+F#", afterWhite: 12 },
  { note: "+G#", afterWhite: 13 },
  { note: "+A#", afterWhite: 14 },
  { note: "++C#", afterWhite: 16 },
];

const MINI_PIANO_WHITE_KEY_WIDTH = 6;
const MINI_PIANO_WHITE_KEY_WIDTH_COMPACT = 4;
const MINI_PIANO_BLACK_KEY_WIDTH = 4;
const MINI_PIANO_BLACK_KEY_WIDTH_COMPACT = 3;
const MINI_PIANO_BLACK_KEY_HEIGHT = 9;
const MINI_PIANO_BLACK_KEY_HEIGHT_COMPACT = 7;

function getMiniPianoWhiteKeyWidth(isCompact) {
  return isCompact ? MINI_PIANO_WHITE_KEY_WIDTH_COMPACT : MINI_PIANO_WHITE_KEY_WIDTH;
}

function getMiniPianoBlackKeyWidth(isCompact) {
  return isCompact ? MINI_PIANO_BLACK_KEY_WIDTH_COMPACT : MINI_PIANO_BLACK_KEY_WIDTH;
}

function getMiniPianoBlackKeyHeight(isCompact) {
  return isCompact ? MINI_PIANO_BLACK_KEY_HEIGHT_COMPACT : MINI_PIANO_BLACK_KEY_HEIGHT;
}

// Get note from top bar piano click (returns note like 'c', 'c#', '+d', etc.)
function getTopBarPianoNoteAt(x, y, screen) {
  const topPianoY = 3;
  const topPianoHeight = 15;
  const topPianoStartX = 54;
  const topPianoWidth = Math.min(140, Math.floor((screen.width - topPianoStartX) * 0.5));
  const topPianoWhiteKeyWidth = Math.floor(topPianoWidth / MINI_PIANO_WHITE_KEYS.length);
  const topPianoBlackKeyWidth = Math.floor(topPianoWhiteKeyWidth * 0.6);
  const topPianoBlackKeyHeight = Math.floor(topPianoHeight * 0.55);
  
  const relX = x - topPianoStartX;
  const relY = y - topPianoY;
  
  // Check if within piano bounds
  if (relX < 0 || relY < 0 || relY > topPianoHeight) return null;
  if (relX >= MINI_PIANO_WHITE_KEYS.length * topPianoWhiteKeyWidth) return null;
  
  // Check black keys first (they're on top)
  for (const { note, afterWhite } of MINI_PIANO_BLACK_KEYS) {
    const bx = afterWhite * topPianoWhiteKeyWidth + topPianoWhiteKeyWidth - topPianoBlackKeyWidth / 2;
    if (relX >= bx && relX <= bx + topPianoBlackKeyWidth && relY <= topPianoBlackKeyHeight) {
      return note.toLowerCase();
    }
  }
  
  // Check white keys
  const index = Math.floor(relX / topPianoWhiteKeyWidth);
  if (index >= 0 && index < MINI_PIANO_WHITE_KEYS.length) {
    return MINI_PIANO_WHITE_KEYS[index].toLowerCase();
  }
  
  return null;
}

function clampPan(value) {
  return Math.max(-PAN_RANGE, Math.min(PAN_RANGE, value));
}

function getSampleRateText(sampleRate) {
  const fallbackRate =
    typeof window !== "undefined" ? window.audioContext?.sampleRate : null;
  const rate = Number.isFinite(sampleRate) ? sampleRate : fallbackRate;
  if (!Number.isFinite(rate) || rate <= 0) return null;
  if (rate >= 1000) return `${Math.round(rate / 1000)}k`;
  return `${Math.round(rate)}Hz`;
}

function getPanForQwertyKey(key) {
  if (typeof key !== "string" || key.length === 0) return 0;
  const lower = key.toLowerCase();

  for (let row = 0; row < QWERTY_LAYOUT_ROWS.length; row += 1) {
    const rowKeys = QWERTY_LAYOUT_ROWS[row];
    const col = rowKeys.indexOf(lower);
    if (col >= 0) {
      const x = col + QWERTY_ROW_OFFSETS[row];
      const normalized = QWERTY_MAX_SPAN > 1 ? x / (QWERTY_MAX_SPAN - 1) : 0.5;
      return clampPan(normalized * 2 - 1);
    }
  }

  return 0;
}

function getPanForButtonNote(note) {
  const key = noteToKeyboardKey(note);
  return key ? getPanForQwertyKey(key) : 0;
}

function noteToKeyboardKey(note) {
  if (typeof note !== "string" || note.length === 0) return null;
  const lower = note.toLowerCase();
  return NOTE_TO_KEYBOARD_KEY[lower] ?? null;
}

function keyboardKeyToNote(key) {
  if (typeof key !== "string" || key.length === 0) return null;
  const lower = key.toLowerCase();
  return KEYBOARD_TO_NOTE[lower] ?? null;
}

function formatKeyLabel(key) {
  if (!key) return "";
  switch (key) {
    case "control":
      return "CTL";
    case "space":
      return "SP";
    case "alt":
      return "ALT";
    case "left":
      return "L";
    case "right":
      return "R";
    case "up":
      return "U";
    case "down":
      return "D";
    default:
      return key.toUpperCase();
  }
}

let upperOctaveShift = 0, // Set by <(,) or >(.) keys.
  lowerOctaveShift = 0;

const attack = 0.0005; // Faster onset for responsive play.
// const attack = 0.005; // 0.025;
// const decay = 0.9999; // 0.9;
let toneVolume = 0.95; // 0.9;
// const killFade = 0.01; // TODO: Make this dynamic according to press time. 24.11.04.06.05
const fade = 0.005;
const fastFade = 0.005;
const killFade = 0.15; //0.05;

let perc; // A one frame percussion flash color.

//             |
// CVDSEFWGRAQB|QARGWFESDVC
// ^ ^ ^^ ^ ^ ^| ^ ^ ^^ ^ ^
// HTIYJKULOMPN|PMOLUKJYITH
// ^ ^ ^^ ^ ^ ^| ^ ^ ^^ ^ ^
//             |

// TODO: How an I add another octave to the layout...

// first octave
// c# v
// d# s
// f# w
// g# r
// a# q
// second octave
// c# t
// d# y
// f# u
// g# o
// a# p

const octaves = "123456789";
const accents = "#f";
const buttons = {}; // Software keys. 🎹
// TODO: Add better octave theme and sharp buttons, etc... 24.07.16.20.01

const buttonNotes = [
  "c",
  "c#",
  "d",
  "d#",
  "e",
  "f",
  "f#",
  "g",
  "g#",
  "a",
  "a#",
  "b",
  "+c",
  "+c#",
  "+d",
  "+d#",
  "+e",
  "+f",
  "+f#",
  "+g",
  "+g#",
  "+a",
  "+a#",
  "+b",
];

const buttonNoteLookup = new Set(buttonNotes);

const midiActiveNotes = new Map();

const MIDI_PITCH_BEND_RANGE = 2; // Semitones up/down for pitch wheel.
let midiPitchBendValue = 0; // Normalized -1..1 position of the wheel.
let midiConnected = false;
let lastOrphanCleanup = 0; // Timestamp of last orphan cleanup

// 🧹 Clean up sounds that are playing but have no corresponding input held
function cleanupOrphanedSounds(pens, fastKill = false) {
  const activePens = pens?.() || [];
  const anyKeyDown = Object.keys(downs).length > 0;
  const anyButtonDown = buttonNotes.some(note => buttons[note]?.down);
  const anyPenDown = activePens.length > 0;
  
  // If nothing is held but sounds exist, clean them up
  if (!anyKeyDown && !anyButtonDown && !anyPenDown) {
    const soundKeys = Object.keys(sounds);
    if (soundKeys.length > 0) {
      const now = performance.now();
      // Debounce: only cleanup if 100ms since last cleanup
      if (now - lastOrphanCleanup > 100) {
        console.log("🧹 Cleaning up", soundKeys.length, "orphaned sounds:", soundKeys.join(", "));
        soundKeys.forEach(note => {
          sounds[note]?.sound?.kill(fastKill ? 0.02 : 0.1);
          delete sounds[note];
          delete tonestack[note];
          trail[note] = 1;
        });
        lastOrphanCleanup = now;
      }
    }
  }
}

// red, orange, yellow, green, blue, purple, brown
//   C,      D,      E, F,     G,    A,      B

// pink, white, black, gray, tan
const notesToColors = {
  c: [255, 0, 0], // red
  "c#": [255, 192, 203], // pink
  d: [255, 165, 0], // orange
  "d#": [255, 255, 255], // white
  e: [255, 255, 0], // yellow
  f: [0, 128, 0], // green
  "f#": [0, 0, 0], // black
  g: [0, 0, 255], // blue
  "g#": [128, 128, 128], // gray
  a: [128, 0, 128], // purple
  "a#": [210, 180, 140], // tan
  b: [165, 42, 42], // brown
};

const noteOrder = [
  "c",
  "c#",
  "d",
  "d#",
  "e",
  "f",
  "f#",
  "g",
  "g#",
  "a",
  "a#",
  "b",
];

const notesToColorsFinal = {
  "4c": [255, 0, 0], // red
  "4c#": [255, 192, 203], // pink
  "4d": [255, 165, 0], // orange
  "4d#": [255, 255, 255], // white
  "4e": [255, 255, 0], // yellow
  "4f": [0, 128, 0], // green
  "4f#": [0, 0, 0], // black
  "4g": [0, 0, 255], // blue
  "4g#": [128, 128, 128], // gray
  "4a": [128, 0, 128], // purple
  "4a#": [210, 180, 140], // tan
  "4b": [165, 42, 42], // brown
};

function colorFromNote(note, num, octaveOverride = null) {
  const baseOctave = Number.isFinite(octaveOverride)
    ? octaveOverride
    : parseInt(octave);
  const { noteName, octave: noteOctave } = parseNotepatNote(note, baseOctave);
  return getNoteColorWithOctave(noteName, noteOctave, {
    baseOctave: 4,
    accentStep: 25,
  });
}

function brightenColor(color, amount = 40) {
  if (!Array.isArray(color) || color.length < 3) return color;
  return [
    Math.min(255, color[0] + amount),
    Math.min(255, color[1] + amount),
    Math.min(255, color[2] + amount),
  ];
}

function darkenColor(color, factor = 0.65) {
  if (!Array.isArray(color) || color.length < 3) return color;
  return [
    Math.max(0, Math.round(color[0] * factor)),
    Math.max(0, Math.round(color[1] * factor)),
    Math.max(0, Math.round(color[2] * factor)),
  ];
}

function getContrastingTextColor(color) {
  if (!Array.isArray(color) || color.length < 3) return [255, 255, 255];
  const [r, g, b] = color;
  const luminance = 0.2126 * r + 0.7152 * g + 0.0722 * b;
  return luminance > 150 ? [20, 20, 20] : [245, 245, 245];
}

const buttonOctaves = ["3", "4", "5", "6", "7", "8"]; // ❤️‍🔥 Add octaves...

const octaveTheme = [
  "black", // 0 (never available)
  "black", // 1
  "darkblue", // 2
  "red", // 3
  "blue", // 4
  "yellowgreen", // 6
  "yellow", // 5
  "green", // 7
  "purple", // 8
];

const { abs, round, floor, ceil, min, max } = Math;

let scope = 16; // Reduced for better visualizer performance
// let scopeTrim = 0;

let projector = false;
let visualizerFullscreen = false; // Toggle visualizer as full background behind buttons
let recitalMode = false; // 🎭 Recital mode - wireframe single-color minimal UI
let recitalBlinkPhase = 0; // For blinking back button in recital mode
const noteShake = {}; // Per-note shake amounts (note -> shake value)

// 🥁 Metronome state - UTC-synced like clock.mjs
let metronomeEnabled = false;
let metronomeBPM = 120; // Default 120 BPM
let metronomeLastBeatTime = 0; // Last time a beat was triggered (UTC ms)
let metronomeVisualPhase = 0; // 0-1 visual pulse phase
let metronomeBeatCount = 0; // Count beats for visual display
let metronomeClockRef = null; // Reference to clock API for UTC sync
let metronomeBallPos = 0; // 0-1 bouncing ball position (bounces between beats)

const trail = {};

// 🔬 Telemetry: Expose trail for stability testing
if (typeof window !== 'undefined') {
  window.__notepat_trail = trail;
}

let lastActiveNote;
let transposeOverlay = false;
let transposeOverlayFade = 0;
let paintTransposeOverlay = false;
let paintPictureOverlay = false;
// let paintPictureOverlay = true;

// let qrcells;

let waveBtn, octBtn;
let slideBtn, roomBtn, glitchBtn, quickBtn; // Toggle buttons for slide/room/glitch/quick modes
let metroBtn, bpmMinusBtn, bpmPlusBtn; // Metronome controls
let melodyAliasBtn;
let melodyAliasDown = false;
let melodyAliasActiveNote = null;
let melodyAliasStartedNote = false;

let rawSong = `
  C:Twin- C:-kle G:twin- G:-kle A:lit- A:-tle G:star,
  F:how F:I E:won- E:-der D:what D:you C:are.
  G:Up G:a- F:-bove F:the E:world E:so D:high, 
  G:like G:a F:dia- F:-mond E:in E:the D:sky.
  C:Twin- C:-kle G:twin- G:-kle A:lit- A:-tle G:star,
  F:how F:I E:won- E:-der D:what D:you C:are.
`;

let song,
  songNoteDown = false,
  songStops = [],
  songIndex = 0,
  songShift = 0,
  songShifting = false,
  songProgress = 0; // Track progress through current note (0 to 1)

let autopatConfig = {
  enabled: false,
  rawSong: null,
  events: null,
  bpm: 88,
  beatsPerNote: 1,
  restBeats: 0.1,
  startDelay: 0.35,
  ignoreInput: false,
  hudLabel: "autopat",
  paused: false,
  showTrack: true,
  sidePanelWidth: 0,
};

let autopatState = {
  activeNote: null,
  activeNoteEnd: 0,
  nextTime: 0,
  started: false,
  eventIndex: 0,
};

let autopatApi = null;

function rebuildAutopatSong() {
  if (!autopatConfig.enabled) return;
  if (autopatConfig.events?.length) {
    song = null;
    songStops = [];
    songIndex = 0;
    songShift = 0;
    songShifting = false;
    songProgress = 0;
    if (autopatConfig.hudLabel && autopatHud?.label) {
      autopatHud.label(autopatConfig.hudLabel);
    }
    return;
  }
  const autopatSong = autopatConfig.rawSong || rawSong;
  song = parseSong(autopatSong);
  songStops = [];
  songIndex = 0;
  songShift = 0;
  songShifting = false;
  songProgress = 0;

  if (song) {
    let x = 0;
    const glyphWidth = resolveMatrixGlyphMetrics(autopatTypeface).width;
    song.forEach((part) => {
      let word = part[1],
        space = 0;
      word.endsWith("-") ? (word = word.slice(0, -1)) : (space = glyphWidth);
      if (word.startsWith("-")) word = word.slice(1);
      songStops.push([word, x]);
      x += word.length * glyphWidth + space;
    });
  }

  if (autopatConfig.hudLabel && autopatHud?.label) {
    autopatHud.label(autopatConfig.hudLabel);
  }
}

function configureAutopat(options = {}) {
  autopatConfig = {
    ...autopatConfig,
    ...options,
    enabled: options.enabled ?? true,
  };

  autopatState = {
    activeNote: null,
    activeNoteEnd: 0,
    nextTime: 0,
    started: false,
    eventIndex: 0,
  };

  rebuildAutopatSong();
}

// Convert various note notations to notepat keyboard notation
function convertNoteToKeyboardKey(note) {
  if (!note || typeof note !== "string") return note;
  
  const normalizedNote = note.toLowerCase().trim();
  
  // Check if it's already a keyboard key - if so, convert to note name
  if (KEYBOARD_TO_NOTE[normalizedNote]) {
    return KEYBOARD_TO_NOTE[normalizedNote]; // e.g., "h" -> "+c"
  }
  
  // Already in note notation (c, c#, +c, etc.) - return as-is
  if (NOTE_TO_KEYBOARD_KEY[normalizedNote]) {
    return normalizedNote; // e.g., "c" -> "c", "+c" -> "+c"
  }
  
  // Handle alternative notations
  // Map flats to sharps (Db -> C#, Eb -> D#, etc.)
  const flatToSharp = {
    'db': 'c#',
    'eb': 'd#',
    'gb': 'f#',
    'ab': 'g#',
    'bb': 'a#',
    '+db': '+c#',
    '+eb': '+d#',
    '+gb': '+f#',
    '+ab': '+g#',
    '+bb': '+a#',
  };
  
  if (flatToSharp[normalizedNote]) {
    return flatToSharp[normalizedNote]; // e.g., "db" -> "c#"
  }
  
  // Return original if no conversion found
  return normalizedNote;
}

function parseSong(raw) {
  return raw
    .trim()
    .split(/\s+/)
    .map((noteword) => {
      const [note, word] = noteword.split(":");
      // Convert the note to keyboard notation
      const keyboardNote = convertNoteToKeyboardKey(note);
      // Store in uppercase to match button comparison logic (note.toUpperCase() === song[i][0])
      return [keyboardNote.toUpperCase(), word];
    });
}

// song = parseSong(rawSong);

let startupSfx;
let udpServer;

let stampleSampleId = null;
let stampleSampleData = null;
let stampleSampleRate = null;
let stampleNeedleProgress = 0;
let stampleNeedleNote = null;
let stampleProgressTick = 0;

let autopatHud = null;
let autopatTypeface = null;

let picture;
let matrixFont; // MatrixChunky8 font for note letters

async function boot({
  params,
  api,
  colon,
  ui,
  screen,
  fps,
  typeface,
  hud,
  net,
  store,
  painting,
  sound,
  clock,
}) {
  autopatApi = api;
  autopatHud = hud;
  autopatTypeface = typeface;

  // 🕒 Store clock reference for UTC-synced metronome
  metronomeClockRef = clock;
  if (clock?.resync) {
    clock.resync(); // Sync to UTC on boot
  }

  // Reset state on boot to avoid stuck notes after reloads
  bootTimestamp = performance.now() / 1000; // Current time in seconds
  Object.keys(sounds).forEach((note) => {
    sounds[note]?.sound?.kill?.(0.01);
    delete sounds[note];
  });
  Object.keys(tonestack).forEach((note) => delete tonestack[note]);
  Object.keys(trail).forEach((note) => delete trail[note]);
  Object.keys(downs).forEach((note) => delete downs[note]);
  anyDown = false;
  audioReinitRequested = false;

  // 🕒 Request minimal audio latency for this piece (must be set before AudioContext creation)
  if (typeof window !== "undefined") {
    window.__acLatencyHint = 0.005; // 5ms target
    window.__acSampleRate = 48000; // Lower CPU load for tighter real-time response
    window.__acSpeakerPerformanceMode = "disabled"; // Skip heavy analysis work in the worklet
  }
  lowLatencyMode = true;
  
  // Store sample rate for layout calculations
  storedSampleRate = sound?.sampleRate || (typeof window !== "undefined" ? window.audioContext?.sampleRate : null);

  if (Array.isArray(colon)) {
    if (colon.includes("perf")) {
      paintPerfEnabled = true;
    }
  }

  // Disabled: dynamic audio reinit was breaking audio - now using 48kHz globally
  pendingAudioReinit = false;

  // fps(4);
  udpServer = net.udp(); // For sending messages to `tv`.

  // Create picture buffer at quarter resolution (quarter width, quarter height)
  const pictureWidth = Math.max(1, Math.floor(screen.width / 4));
  const pictureHeight = Math.max(1, Math.floor(screen.height / 4));
  picture = painting(pictureWidth, pictureHeight, ({ wipe }) => {
    wipe("gray");
  });

  net
    .preload("startup") // TODO: Switch this default sample. 24.11.19.22.20
    .then((sfx) => (startupSfx = sfx))
    .catch((err) => console.warn(err)); // Load startup

  stampleSampleId = null;
  stampleSampleData = null;
  stampleSampleRate = null;
  stampleNeedleProgress = 0;
  stampleNeedleNote = null;

  if (store?.retrieve) {
    const storedSample =
      store["stample:sample"] ||
      (await store.retrieve("stample:sample", "local:db"));
    if (storedSample?.data?.length) {
      const storedId = storedSample.id || "stample";
      stampleSampleId = storedId;
      stampleSampleData = storedSample.data;
      stampleSampleRate = storedSample.sampleRate;
      sound?.registerSample?.(storedId, storedSample.data, storedSample.sampleRate);
    }
  }

  try {
    sound?.midi?.connect?.();
  } catch (err) {
    console.warn("🎹 Unable to request MIDI connection", err);
  }

  // Load MatrixChunky8 font for note letters
  if (api.Typeface) {
    matrixFont = new api.Typeface("MatrixChunky8");
    matrixFont.load(net.preload);
  }

  // qrcells = qr("https://prompt.ac/notepat", { errorCorrectLevel: 2 }).modules;

  if (params[0] === "piano") {
    toneVolume = params[1] || 0.5;
    hud.label("notepat"); // Clear the label.
  }

  if (params[0] === "twinkle") {
    song = parseSong(rawSong);
    hud.label("notepat"); // Strip "twinkle" from the label
  }

  // Check if any parameter contains a custom melody using rawSong syntax (note:word)
  const customMelodyParams = params.filter(param => param.includes(":"));
  if (customMelodyParams.length > 0) {
    // Join all custom melody params into a single string
    const customRawSong = customMelodyParams.join(" ");
    song = parseSong(customRawSong);
    hud.label("notepat"); // Clear the label
  }

  if (autopatConfig.enabled) {
    rebuildAutopatSong();
  }

  if (song) {
    let x = 0;
    const glyphWidth = resolveMatrixGlyphMetrics(typeface).width;
    song.forEach((part, index) => {
      let word = part[1],
        space = 0;
      word.endsWith("-") ? (word = word.slice(0, -1)) : (space = glyphWidth);
      if (word.startsWith("-")) word = word.slice(1);
      songStops.push([word, x]);
      x += word.length * glyphWidth + space;
    });
  }

  // keys = params.join(" ") || "";
  // keys = keys.replace(/\s/g, "");
  // if (keys.length > 0) {
  //   tap = true;
  //   editable = false;
  // }

  const wavetypes = [
    "square",
    "sine",
    "triangle",
    "sawtooth",
    "noise-white",
    "noise",
    "stample",
    "sample",
  ];
  const requestedWave = wavetypes.indexOf(colon[0]) > -1 ? colon[0] : wave;
  wave = requestedWave === "sample" ? "stample" : requestedWave;
  // Map 'noise' shorthand to 'noise-white' for the synth
  if (wave === "noise") wave = "noise-white";
  // slide = true; // colon[0] === "slide" || colon[1] === "slide";

  const colonTokens = Array.isArray(colon) ? colon.filter(Boolean) : [];
  const hideLabelTokens = new Set(["stample", "sample"]);
  const visibleColonTokens = colonTokens.filter(
    (token) => !hideLabelTokens.has(token),
  );
  const hasHiddenLabelToken = colonTokens.some((token) => hideLabelTokens.has(token));
  if (colonTokens.length > 0) {
    if (hasHiddenLabelToken && visibleColonTokens.length === 0) {
      hud.label("notepat");
    } else {
      const coloredSuffix = visibleColonTokens
        .map((token) => `\\gray\\:\\silver\\${token}`)
        .join(" ");
      const labelText = `\\silver\\notepat${coloredSuffix ? " " + coloredSuffix : ""}`;
      const plainText = `notepat${visibleColonTokens.length ? " " + visibleColonTokens.map((token) => `:${token}`).join(" ") : ""}`;
      hud.label(labelText, undefined, 0, plainText);
    }
  }

  buildWaveButton(api);
  buildOctButton(api);
  buildToggleButtons(api);
  buildMetronomeButtons(api);

  const newOctave =
    parseInt(colon[0]) || parseInt(colon[1]) || parseInt(colon[2]);

  if (newOctave) {
    STARTING_OCTAVE = newOctave.toString();
    octave = STARTING_OCTAVE;
  }

  setupButtons(api);
}

function sim({ sound, simCount, num, clock }) {
  const simTick = typeof simCount === "bigint" ? Number(simCount) : simCount;

  if (lowLatencyMode) {
    if (simTick % 3 === 0) sound.speaker?.poll();
  } else {
    sound.speaker?.poll();
  }

  // 🥁 UTC-synced metronome tick (like clock.mjs)
  if (metronomeEnabled && metronomeBPM > 0) {
    const clockRef = clock || metronomeClockRef;
    const syncedTime = clockRef?.time?.();
    const currentTimeMs = syncedTime ? syncedTime.getTime() : Date.now();
    
    // Calculate beat interval in milliseconds
    const msPerBeat = 60000 / metronomeBPM;
    
    // Calculate which beat we should be on based on UTC time
    // This ensures all instances sync to the same beat boundaries
    const beatNumber = Math.floor(currentTimeMs / msPerBeat);
    
    // Check if we've moved to a new beat
    if (beatNumber !== metronomeBeatCount) {
      metronomeBeatCount = beatNumber;
      metronomeLastBeatTime = currentTimeMs;
      metronomeVisualPhase = 1.0; // Flash on beat
      
      // Play metronome click sound
      // Accent on beat 1 of each measure (every 4 beats)
      const isDownbeat = (beatNumber % 4) === 0;
      const clickFreq = isDownbeat ? 1200 : 800; // Higher pitch for downbeat
      const clickVol = isDownbeat ? 0.4 : 0.25;
      
      sound.synth({
        type: "sine",
        tone: clickFreq,
        attack: 0.001,
        decay: 0.05,
        sustain: 0,
        release: 0.02,
        volume: clickVol,
        duration: 0.03,
      });
    }
    
    // Update bouncing ball position - oscillates between 0 and 1 within each beat
    // Use a smooth sine-based bounce that peaks in the middle of the beat interval
    const msPerBeatForBall = 60000 / metronomeBPM;
    const beatProgress = (currentTimeMs % msPerBeatForBall) / msPerBeatForBall;
    // Sine wave creates smooth back-and-forth motion
    metronomeBallPos = Math.sin(beatProgress * Math.PI);
    
    // Decay visual phase smoothly
    if (metronomeVisualPhase > 0) {
      metronomeVisualPhase = Math.max(0, metronomeVisualPhase - 0.08);
    }
  }

  // 🛡️ Stuck note protection: Kill notes held longer than MAX_NOTE_LIFETIME
  // Note: startedAt is in audio-time (soundTime), so we must compare against soundTime, not wall-clock
  const MAX_NOTE_LIFETIME = 30; // seconds - notes shouldn't be held this long
  const audioNow = sound.time; // Use audio time, not performance.now()
  if (audioNow !== undefined) {
    Object.keys(sounds).forEach((noteKey) => {
      const entry = sounds[noteKey];
      if (entry?.sound?.startedAt) {
        const age = audioNow - entry.sound.startedAt;
        if (age > MAX_NOTE_LIFETIME) {
          console.warn(`🛡️ Killing stuck note: ${noteKey} (held ${age.toFixed(1)}s)`);
          entry.sound?.kill?.(0.1);
          delete sounds[noteKey];
          delete tonestack[noteKey];
          if (buttons[noteKey]) buttons[noteKey].down = false;
        }
      }
    });
  }

  // 📊 Update performance stats
  if (perfOSD) {
    const perfNow = performance.now();
    if (perfStats.lastFrameTimestamp > 0) {
      perfStats.frameTime = perfNow - perfStats.lastFrameTimestamp;
      perfStats.fps = Math.round(1000 / perfStats.frameTime);
    }
    perfStats.lastFrameTimestamp = perfNow;
    perfStats.soundCount = Object.keys(sounds).filter(k => sounds[k]?.sound).length;
    perfStats.tonesInStack = Object.keys(tonestack).length;
    perfStats.keysLength = keys.length;
    
    // Get audio context info
    if (sound.sampleRate) {
      perfStats.sampleRate = sound.sampleRate / 1000; // Convert to kHz
    }
    // Get AudioContext latency info from window if available
    if (typeof window !== 'undefined' && window.audioContext) {
      perfStats.baseLatency = (window.audioContext.baseLatency || 0) * 1000;
      perfStats.outputLatency = (window.audioContext.outputLatency || 0) * 1000;
    }
    
    // Get memory usage if available
    if (typeof performance !== 'undefined' && performance.memory) {
      perfStats.memoryUsed = Math.round(performance.memory.usedJSHeapSize / 1024 / 1024);
    }
  }

  if (songShifting) {
    songShift += !songNoteDown ? 0.5 : 1;
    if (songShift >= songStops[songIndex][1]) {
      songShift = songStops[songIndex][1];
      songShifting = false;
    }
    
    // Calculate progress (0 to 1) through current note
    if (songIndex > 0) {
      const currentX = songStops[songIndex][1];
      const prevX = songStops[songIndex - 1]?.[1] || 0;
      const distance = currentX - prevX;
      const traveled = songShift - prevX;
      songProgress = distance > 0 ? Math.min(1, traveled / distance) : 0;
    } else {
      songProgress = songShift / (songStops[0]?.[1] || 1);
    }
  }

  if (autopatConfig.enabled && autopatConfig.events?.length && !autopatConfig.paused) {
    const now = typeof sound?.time === "number" ? sound.time : performance.now() / 1000;
    const beatSeconds = 60 / (autopatConfig.bpm || 88);

    if (!autopatState.started) {
      autopatState.started = true;
      autopatState.nextTime = now + (autopatConfig.startDelay || 0);
      autopatState.eventIndex = 0;
    }

    if (autopatState.activeNote && now >= autopatState.activeNoteEnd) {
      stopButtonNote(autopatState.activeNote, { force: true });
      autopatState.activeNote = null;
      autopatState.activeNoteEnd = 0;
      autopatState.nextTime = now;
    }

    if (!autopatState.activeNote && now >= autopatState.nextTime) {
      const event = autopatConfig.events[autopatState.eventIndex];
      if (!event) {
        autopatState.eventIndex = 0;
      } else {
        const beats = event.beats ?? autopatConfig.beatsPerNote ?? 1;
        const durationSeconds = beatSeconds * beats;
        const key = event.key;
        if (key) {
          const note = key.toLowerCase();
          const started = startButtonNote(note, 127, autopatApi);
          if (started) {
            autopatState.activeNote = note;
            autopatState.activeNoteEnd = now + durationSeconds;
          } else {
            autopatState.activeNote = null;
            autopatState.activeNoteEnd = now;
          }
        } else {
          autopatState.activeNote = null;
          autopatState.activeNoteEnd = now + durationSeconds;
        }
        autopatState.eventIndex =
          (autopatState.eventIndex + 1) % autopatConfig.events.length;
        if (!key) {
          autopatState.nextTime = now + durationSeconds;
        }
      }
    }
  } else if (autopatConfig.enabled && song?.length && !autopatConfig.paused) {
    const now = typeof sound?.time === "number" ? sound.time : performance.now() / 1000;
    const beatSeconds = 60 / (autopatConfig.bpm || 88);
    const noteSeconds = beatSeconds * (autopatConfig.beatsPerNote || 1);
    const restSeconds = beatSeconds * (autopatConfig.restBeats || 0);

    if (!autopatState.started) {
      autopatState.started = true;
      autopatState.nextTime = now + (autopatConfig.startDelay || 0);
    }

    if (autopatState.activeNote && now >= autopatState.activeNoteEnd) {
      stopButtonNote(autopatState.activeNote, { force: true });
      autopatState.activeNote = null;
      autopatState.activeNoteEnd = 0;
      autopatState.nextTime = now + restSeconds;
    }

    if (!autopatState.activeNote && now >= autopatState.nextTime) {
      const current = song?.[songIndex]?.[0];
      if (current) {
        const note = current.toLowerCase();
        const started = startButtonNote(note, 127, autopatApi);
        if (started) {
          autopatState.activeNote = note;
          autopatState.activeNoteEnd = now + noteSeconds;
        } else if (song?.length) {
          songIndex = (songIndex + 1) % song.length;
        }
      }
    }
  } else if (autopatConfig.enabled && autopatConfig.paused) {
    if (autopatState.activeNote) {
      stopButtonNote(autopatState.activeNote, { force: true });
      autopatState.activeNote = null;
      autopatState.activeNoteEnd = 0;
    }
  }

  Object.keys(trail).forEach((note) => {
    trail[note] -= 0.0065;
    if (trail[note] <= 0) delete trail[note];
  });

  // Decay shake effect per-note
  Object.keys(noteShake).forEach((note) => {
    noteShake[note] -= 0.15;
    if (noteShake[note] <= 0) delete noteShake[note];
  });

  const active = orderedByCount(sounds);

  if (wave === "stample" && active.length > 0) {
    stampleProgressTick = (stampleProgressTick + 1) % 12;
    if (stampleProgressTick === 0) {
      const note = active[active.length - 1];
      const entry = sounds[note];
      entry?.sound?.progress?.().then((progressData) => {
        if (progressData && typeof progressData.progress === "number") {
          stampleNeedleProgress = progressData.progress;
          stampleNeedleNote = note;
        }
      });
    }
  } else if (active.length === 0) {
    stampleNeedleProgress = 0;
    stampleNeedleNote = null;
  }

  if (active.length > 0 && transposeOverlayFade < 1) {
    // transposeOverlayFade += 0.05;
    transposeOverlayFade = 1;
    transposeOverlayFade = min(1, transposeOverlayFade);
  } else if (transposeOverlayFade > 0) {
    transposeOverlayFade -= 0.0065;
    transposeOverlayFade = max(0, transposeOverlayFade);
  }

  // Color shifting for the oscilloscope.
  const val = activeStr ? 0.075 : 0.02;
  let d1 = lastAverage,
    d2 = currentAverage;
  if (!activeStr) d1 = d2 = [0, 0, 0];
  if (
    !activeStr &&
    secondaryColor[0] < 50 &&
    secondaryColor[1] < 50 &&
    secondaryColor[2] < 50
  ) {
    lastAverage = [0, 0, 0];
    d1 = lastAverage;
  }
  secondaryColor = num.shiftRGB(secondaryColor, d1, val, "lerp");
  primaryColor = num.shiftRGB(primaryColor, d2, val, "lerp");
}

function resolveMatrixGlyphMetrics(fallbackTypeface) {
  const glyph =
    matrixFont?.glyphs?.["0"]?.resolution ??
    fallbackTypeface?.glyphs?.["0"]?.resolution;

  if (Array.isArray(glyph) && glyph.length >= 2) {
    return { width: glyph[0], height: glyph[1] };
  }

  return { width: 6, height: 8 };
}

function measureMatrixTextWidth(text, fallbackTypeface) {
  if (!text || !text.length) return 0;
  const advances = matrixFont?.data?.advances || fallbackTypeface?.data?.advances;
  const defaultAdvance = resolveMatrixGlyphMetrics(fallbackTypeface).width;
  let width = 0;
  for (let i = 0; i < text.length; i += 1) {
    const ch = text[i];
    width += advances && advances[ch] !== undefined ? advances[ch] : defaultAdvance;
  }
  return width;
}

function measureMatrixTextBoxWidth(text, api, screenWidth) {
  if (!text || !text.length || !api?.text?.box) return 0;
  const bounds = Math.max(screenWidth || 0, 200);
  const tb = api.text.box(
    text,
    { x: 0, y: 0 },
    bounds,
    1,
    false,
    "MatrixChunky8",
  );
  return tb?.box?.width || 0;
}

function getMiniPianoGeometry({ screen, layout, song, trackY, trackHeight }) {
  const isCompact = layout?.compactMode && layout?.splitLayout;
  const isHorizontalLayout = layout?.miniInputsHorizontal;
  const isVerticalLayout = layout?.miniInputsVertical;
  const whiteKeyWidth = getMiniPianoWhiteKeyWidth(isCompact);
  const whiteKeyHeight = MINI_KEYBOARD_HEIGHT;
  const blackKeyWidth = getMiniPianoBlackKeyWidth(isCompact);
  const blackKeyHeight = getMiniPianoBlackKeyHeight(isCompact);
  const pianoWidth = MINI_PIANO_WHITE_KEYS.length * whiteKeyWidth;
  const sidePanelWidth = layout?.sidePanelWidth || 0;
  const effectiveWidth = screen.width - sidePanelWidth;
  const qKeyWidth = 9;
  const qKeySpacing = QWERTY_MINIMAP_KEY_SPACING;
  const qwertyWidth =
    Math.max(...QWERTY_LAYOUT_ROWS.map((row) => row.length)) *
    (qKeyWidth + qKeySpacing);
  const qwertyHeight = QWERTY_MINIMAP_HEIGHT;

  let pianoY = SECONDARY_BAR_BOTTOM;
  let pianoStartX = 58;
  const centerX = layout?.centerX ?? (effectiveWidth - pianoWidth) / 2;
  const centerWidth = layout?.centerAreaWidth ?? pianoWidth;
  const centerRight = centerX + centerWidth;
  const clamp = (value, minValue, maxValue) =>
    Math.min(Math.max(value, minValue), maxValue);

  // Vertical/rotated layout: piano keys run vertically on the right side
  if (isVerticalLayout) {
    const gridWidth = (layout?.buttonsPerRow || 4) * (layout?.buttonWidth || 20) + (layout?.margin || 2) * 2;
    const gridLeft = layout?.margin || 2;
    // In vertical mode, the "width" of the piano becomes its height
    // whiteKeyWidth becomes the height of each key, whiteKeyHeight becomes the width
    const rotatedPianoWidth = whiteKeyHeight + 2; // Keys drawn horizontally but stacked vertically
    const rotatedPianoHeight = MINI_PIANO_WHITE_KEYS.length * whiteKeyWidth; // Full piano height when rotated
    pianoStartX = gridLeft + gridWidth + 4; // Gap from grid
    pianoY = layout?.topButtonY || SECONDARY_BAR_BOTTOM;
    
    // Check if rotated piano fits horizontally (x + width within screen)
    const pianoRight = pianoStartX + rotatedPianoWidth;
    // Check if rotated piano fits vertically (y + height within screen)
    const availableHeight = screen.height - pianoY - 4;
    
    // If piano doesn't fit, hide it
    if (pianoRight > screen.width - 2 || rotatedPianoHeight > availableHeight) {
      return {
        x: 0,
        y: 0,
        whiteKeyWidth,
        whiteKeyHeight,
        blackKeyWidth,
        blackKeyHeight,
        pianoWidth,
        hidden: true, // Flag to skip painting piano
        qwertyStartX: null,
        qwertyStartY: null,
      };
    }
    
    return {
      x: pianoStartX,
      y: pianoY,
      whiteKeyWidth,
      whiteKeyHeight,
      blackKeyWidth,
      blackKeyHeight,
      pianoWidth,
      rotated: true, // Flag to indicate 90-degree rotation
      rotatedWidth: rotatedPianoWidth,
      rotatedHeight: rotatedPianoHeight,
      qwertyStartX: null, // No qwerty in narrow mode
      qwertyStartY: null,
    };
  }

  // Horizontal layout: place piano/qwerty to the right of the grid, stacked vertically
  if (isHorizontalLayout) {
    const gridWidth = (layout?.buttonsPerRow || 4) * (layout?.buttonWidth || 20) + (layout?.margin || 2) * 2;
    const gridLeft = layout?.margin || 2;
    pianoStartX = gridLeft + gridWidth + 8; // 8px gap from grid
    pianoY = layout?.topButtonY || SECONDARY_BAR_BOTTOM;
    
    // Check if piano fits horizontally
    const pianoRight = pianoStartX + pianoWidth;
    if (pianoRight > screen.width - 2) {
      // Piano doesn't fit - hide it
      return {
        x: 0,
        y: 0,
        whiteKeyWidth,
        whiteKeyHeight,
        blackKeyWidth,
        blackKeyHeight,
        pianoWidth,
        hidden: true, // Flag to skip painting piano
        qwertyStartX: null,
        qwertyStartY: null,
      };
    }
    
    // Stack qwerty below piano
    const qwertyStartX = pianoStartX;
    const qwertyStartY = pianoY + whiteKeyHeight + QWERTY_MINIMAP_SPACING;
    
    return {
      x: pianoStartX,
      y: pianoY,
      whiteKeyWidth,
      whiteKeyHeight,
      blackKeyWidth,
      blackKeyHeight,
      pianoWidth,
      qwertyStartX,
      qwertyStartY,
    };
  }

  if (isCompact) {
    pianoY = SECONDARY_BAR_BOTTOM + 2;
    // Check if piano fits in center area - if not, skip it (return hidden flag)
    if (centerWidth < pianoWidth + 4) {
      // Piano doesn't fit - hide it but still compute QWERTY position for center
      // Center the QWERTY in the available center area
      const idealQwertyX = centerX + (centerWidth - qwertyWidth) / 2;
      const minQwertyX = centerX;
      const maxQwertyX = Math.max(minQwertyX, centerRight - qwertyWidth);
      const clampedQwertyX = clamp(idealQwertyX, minQwertyX, maxQwertyX);
      return {
        x: 0,
        y: 0,
        whiteKeyWidth,
        whiteKeyHeight,
        blackKeyWidth,
        blackKeyHeight,
        pianoWidth,
        hidden: true, // Flag to skip painting piano
        qwertyStartX: clampedQwertyX,
        qwertyStartY: pianoY,
      };
    }
    const idealX = centerX + (centerWidth - pianoWidth) / 2;
    const minX = centerX;
    const maxX = Math.max(minX, centerRight - pianoWidth);
    pianoStartX = clamp(idealX, minX, maxX);
  } else if (song) {
    const effectiveTrackY = trackY ?? SECONDARY_BAR_BOTTOM;
    pianoY = effectiveTrackY + (trackHeight || 0) + 2;
    pianoStartX = effectiveWidth - pianoWidth - 2;
  } else {
    const rightAlignedX = effectiveWidth - pianoWidth - 2;
    if (rightAlignedX > pianoStartX + 12) {
      pianoStartX = rightAlignedX;
    }
  }

  let qwertyStartX = pianoStartX;
  if (layout?.splitLayout) {
    const idealQwertyX = centerX + (centerWidth - qwertyWidth) / 2;
    const minQwertyX = centerX;
    const maxQwertyX = Math.max(minQwertyX, centerRight - qwertyWidth);
    qwertyStartX = clamp(idealQwertyX, minQwertyX, maxQwertyX);
  }
  let qwertyStartY = pianoY + whiteKeyHeight + QWERTY_MINIMAP_SPACING;
  const maxY = layout?.topButtonY ? layout.topButtonY - 2 : screen.height;
  if (qwertyStartY + qwertyHeight > maxY) {
    const gap = 4;
    const rightX = pianoStartX + pianoWidth + gap;
    const leftX = pianoStartX - gap - qwertyWidth;
    if (layout?.splitLayout) {
      const leftFits = leftX >= centerX + 2;
      const rightFits = rightX + qwertyWidth <= centerRight - 2;
      if (leftFits) {
        qwertyStartX = leftX;
        qwertyStartY = pianoY;
      } else if (rightFits) {
        qwertyStartX = rightX;
        qwertyStartY = pianoY;
      }
    } else if (leftX >= 2) {
      qwertyStartX = leftX;
      qwertyStartY = pianoY;
    } else if (rightX + qwertyWidth <= effectiveWidth - 2) {
      qwertyStartX = rightX;
      qwertyStartY = pianoY;
    }
  }

  return {
    x: pianoStartX,
    y: pianoY,
    whiteKeyWidth,
    whiteKeyHeight,
    blackKeyWidth,
    blackKeyHeight,
    pianoWidth,
    qwertyStartX,
    qwertyStartY,
  };
}

function getMiniPianoNoteAt(x, y, geometry) {
  if (!geometry || geometry.hidden) return null;
  const {
    x: startX,
    y: startY,
    whiteKeyWidth,
    whiteKeyHeight,
    blackKeyWidth,
    blackKeyHeight,
  } = geometry;
  const relX = x - startX;
  const relY = y - startY;
  if (relX < 0 || relY < 0 || relY > whiteKeyHeight) return null;

  for (const { note, afterWhite } of MINI_PIANO_BLACK_KEYS) {
    const bx = afterWhite * whiteKeyWidth + whiteKeyWidth - blackKeyWidth / 2;
    if (
      relX >= bx &&
      relX <= bx + blackKeyWidth &&
      relY <= blackKeyHeight
    ) {
      return note.toLowerCase();
    }
  }

  const index = Math.floor(relX / whiteKeyWidth);
  if (index < 0 || index >= MINI_PIANO_WHITE_KEYS.length) return null;
  return MINI_PIANO_WHITE_KEYS[index].toLowerCase();
}

function getQwertyKeyAt(x, y, pianoGeometry) {
  if (!pianoGeometry || pianoGeometry.hidden) return null;
  const qKeyWidth = 9;
  const qKeyHeight = QWERTY_MINIMAP_KEY_HEIGHT;
  const qKeySpacing = QWERTY_MINIMAP_KEY_SPACING;
  const qwertyStartX = pianoGeometry.qwertyStartX ?? pianoGeometry.x;
  const qwertyStartY =
    pianoGeometry.qwertyStartY ??
    pianoGeometry.y + pianoGeometry.whiteKeyHeight + QWERTY_MINIMAP_SPACING;

  for (let rowIndex = 0; rowIndex < QWERTY_LAYOUT_ROWS.length; rowIndex += 1) {
    const row = QWERTY_LAYOUT_ROWS[rowIndex];
    const rowOffset =
      rowIndex === 0
        ? 0
        : rowIndex === 1
        ? (qKeyWidth + qKeySpacing) / 2
        : rowIndex === 2
        ? qKeyWidth
        : Math.floor(
            (pianoGeometry.pianoWidth -
              row.length * (qKeyWidth + qKeySpacing)) /
              2,
          );
    const rowY = qwertyStartY + rowIndex * (qKeyHeight + qKeySpacing);
    const relY = y - rowY;
    if (relY < 0 || relY > qKeyHeight) continue;

    for (let keyIndex = 0; keyIndex < row.length; keyIndex += 1) {
      const keyX =
        qwertyStartX + rowOffset + keyIndex * (qKeyWidth + qKeySpacing);
      if (x >= keyX && x <= keyX + qKeyWidth) {
        return row[keyIndex];
      }
    }
  }

  return null;
}

function computeMidiBadgeMetrics(
  screen,
  glyphMetrics = resolveMatrixGlyphMetrics(),
  compactMode = false,
  rateLabel = null,
  rateText = null,
) {
  // Compact layout: "M|48k|60fps"
  const midiWidth = 1 * glyphMetrics.width; // "M" instead of "MIDI"
  const divWidth = 5; // divider + spacing
  const shortRate = rateText ? rateText.replace("Hz", "") : "";
  const rateWidth = shortRate.length * glyphMetrics.width;
  const fpsWidth = 6 * glyphMetrics.width; // "60fps", "120fps", or "--" (6 chars max)
  
  const totalTextWidth = midiWidth + divWidth + rateWidth + divWidth + fpsWidth;
  const width = totalTextWidth + MIDI_BADGE_PADDING_X + MIDI_BADGE_PADDING_RIGHT;
  const height = glyphMetrics.height + MIDI_BADGE_PADDING_Y * 2;
  
  // In compact mode, center the badge at bottom
  const x = compactMode 
    ? (screen.width - width) / 2 
    : screen.width - width - MIDI_BADGE_MARGIN;
  const y = screen.height - height - MIDI_BADGE_MARGIN;

  return { x, y, width, height };
}

function computeMidiBadgeTopMetrics(
  screen,
  glyphMetrics = resolveMatrixGlyphMetrics(),
  rateLabel = null,
  rateText = null,
) {
  const base = computeMidiBadgeMetrics(
    screen,
    glyphMetrics,
    false,
    rateLabel,
    rateText,
  );
  const x = MIDI_BADGE_MARGIN;
  const y = SECONDARY_BAR_TOP + Math.max(0, Math.floor((SECONDARY_BAR_HEIGHT - base.height) / 2));
  return { ...base, x, y };
}

function computeMelodyButtonRect(screen, midiMetrics) {
  if (!midiMetrics) return null;

  const maxWidth = midiMetrics.x - MELODY_ALIAS_MARGIN;
  if (maxWidth < MELODY_ALIAS_MIN_SIDE) return null;

  const side = max(
    MELODY_ALIAS_MIN_SIDE,
    min(MELODY_ALIAS_BASE_SIDE, maxWidth),
  );

  const height = side;
  const width = side;
  const y = midiMetrics.y - MELODY_ALIAS_MARGIN - height;
  if (y < 0) return null;

  const x = screen.width - width - MIDI_BADGE_MARGIN;

  return { x, y, width, height };
}

function getButtonLayoutMetrics(
  screen,
  {
    songMode = false,
    pictureOverlay = false,
    midiMetrics,
    rateLabel,
    rateText,
    sidePanelWidth = 0,
  } = {},
) {
  const reservedSide = sidePanelWidth || autopatConfig.sidePanelWidth || 0;
  const badgeMetrics =
    midiMetrics ??
    computeMidiBadgeMetrics(
      screen,
      resolveMatrixGlyphMetrics(),
      false,
      rateLabel,
      rateText,
    );
  const melodyButtonRect =
    songMode && !pictureOverlay
      ? computeMelodyButtonRect(screen, badgeMetrics)
      : null;

  const aliasPadding = melodyButtonRect
    ? melodyButtonRect.height + MELODY_ALIAS_MARGIN
    : 0;

  const baseBottomPadding = 2;
  const bottomPadding = pictureOverlay
    ? baseBottomPadding
    : songMode
    ? baseBottomPadding + badgeMetrics.height + MIDI_BADGE_MARGIN + aliasPadding
    : baseBottomPadding;

  const totalButtons = buttonNotes.length;
  const margin = 2;

  // Compact/DAW mode: split layout - octaves on sides, piano/qwerty in center
  // Only use split layout for landscape screens (wider than tall)
  const compactMode = screen.height < 200;
  const isLandscape = screen.width > screen.height;
  
  if (compactMode && isLandscape) {
    const usableWidth = max(0, screen.width - reservedSide);
    // Recompute badge metrics for compact mode (centered)
    const compactBadgeMetrics = computeMidiBadgeMetrics(
      screen,
      resolveMatrixGlyphMetrics(),
      true,
      rateLabel,
      rateText,
    );
    
    // Split layout: 4x3 grid per octave (12 notes each)
    // Left side: first octave (c to b), Right side: second octave (+c to +b)
    const notesPerSide = 12;
    const buttonsPerRow = 4;  // 4 notes per row on each side
    const totalRows = Math.ceil(notesPerSide / buttonsPerRow);  // 3 rows
    const hudReserved = SECONDARY_BAR_BOTTOM;
    
    // Piano dimensions (extended mini layout)
    const whiteKeyWidth = getMiniPianoWhiteKeyWidth(true);
    const pianoWidth = MINI_PIANO_WHITE_KEYS.length * whiteKeyWidth;
    const rotatedPianoWidth = MINI_KEYBOARD_HEIGHT + 4; // Width when piano is rotated
    
    // QWERTY minimap dimensions (10 keys per row roughly)
    const qKeyWidth = 9;
    const qKeySpacing = 1;
    const qwertyWidth =
      Math.max(...QWERTY_LAYOUT_ROWS.map((row) => row.length)) *
      (qKeyWidth + qKeySpacing);
    
    // Center area: use smaller of piano or rotated piano, depending on available space
    // Minimum center width is the rotated piano width (for narrow screens)
    const minCenterWidth = rotatedPianoWidth + 4;
    const idealCenterWidth = Math.max(pianoWidth, qwertyWidth) + 4;
    // Check how much space we'd have for buttons with each center width
    const spaceWithIdeal = (usableWidth - idealCenterWidth) / 2;
    const spaceWithMin = (usableWidth - minCenterWidth) / 2;
    // In split mode, if buttons would be too small even with minimum center, hide piano entirely
    // This means center width = 0, and buttons get the full width
    const pianoHidden = (spaceWithMin / buttonsPerRow < 16);
    const centerWidth = pianoHidden ? 0 : 
      (spaceWithIdeal / buttonsPerRow >= 16) ? idealCenterWidth : minCenterWidth;
    
    // Available width for buttons on each side
    const sideMargin = margin;
    const availableSideWidth = (usableWidth - centerWidth) / 2 - sideMargin * 2;
    
    // Calculate button size - allow non-square to fill space better on narrow screens
    const maxButtonWidth = floor(availableSideWidth / buttonsPerRow);
    const availableHeight = screen.height - hudReserved - bottomPadding - margin;
    const maxButtonHeight = floor(availableHeight / totalRows);
    
    // Use available width and height, but keep some proportionality
    let buttonWidth = max(12, maxButtonWidth);
    let buttonHeight = max(12, maxButtonHeight);
    // Keep aspect ratio reasonable (between 0.5 and 2.0)
    const aspect = buttonWidth / buttonHeight;
    if (aspect > 2.0) buttonWidth = floor(buttonHeight * 2.0);
    else if (aspect < 0.5) buttonHeight = floor(buttonWidth * 2.0);
    
    const topButtonY = hudReserved + margin;
    
    // Calculate actual button block width and height
    const buttonBlockWidth = buttonsPerRow * buttonWidth;
    const buttonBlockHeight = totalRows * buttonHeight;
    
    // Left octave starts at left margin
    const leftOctaveX = sideMargin;
    
    // Right octave ends at right edge, position accordingly
    const rightOctaveX = usableWidth - sideMargin - buttonBlockWidth;
    
    // Center area position - recalculate based on actual button sizes
    const centerX = leftOctaveX + buttonBlockWidth + sideMargin;
    // When piano is hidden, center area width should be minimal (just gap between button groups)
    const centerAreaWidth = pianoHidden ? 
      Math.max(0, rightOctaveX - centerX - sideMargin) : 
      max(minCenterWidth, rightOctaveX - centerX - sideMargin);
    
    // Bottom center area for unified active note display - position above MIDI badge
    const bottomCenterY = screen.height - compactBadgeMetrics.height - MIDI_BADGE_MARGIN - 14;
    
    return {
      buttonWidth,
      buttonHeight,
      topButtonY,
      totalRows,
      buttonsPerRow,
      margin,
      bottomPadding,
      hudReserved,
      trackHeight: 0,
      trackSpacing: 0,
      reservedTop: hudReserved,
      melodyButtonRect: null,
      midiBadge: compactBadgeMetrics,  // Use centered badge metrics
      compactMode: true,
      sidePanelWidth: reservedSide,
      usableWidth,
      miniInputsEnabled: miniInputsEnabled,
      // Split layout info
      splitLayout: true,
      leftOctaveX,
      rightOctaveX,
      centerX,
      centerAreaWidth,
      notesPerSide,
      bottomCenterY,
      buttonBlockHeight,
    };
  }

  const buttonsPerRow = 4;
  const totalRows = ceil(totalButtons / buttonsPerRow);

  if (pictureOverlay) {
    const buttonSize = 24;
    const buttonsAreaHeight = totalRows * buttonSize;
    const topButtonY = screen.height - bottomPadding - buttonsAreaHeight;

    return {
      buttonWidth: buttonSize,
      buttonHeight: buttonSize,
      topButtonY,
      totalRows,
      buttonsPerRow,
      margin,
      bottomPadding,
      hudReserved: 0,
      trackHeight: 0,
      trackSpacing: 0,
      melodyButtonRect: null,
      midiBadge: badgeMetrics,
      compactMode: false,
      sidePanelWidth: reservedSide,
    };
  }

  const hudReserved = SECONDARY_BAR_BOTTOM;
  const trackHeight = songMode ? TRACK_HEIGHT : 0;
  const trackSpacing = songMode ? TRACK_GAP : 0;
  const baseReservedTop = hudReserved + trackHeight + trackSpacing;
  // Always calculate space for mini inputs (piano + qwerty stacked)
  const qwertyReservedBase = QWERTY_MINIMAP_HEIGHT + QWERTY_MINIMAP_SPACING;
  const keyboardReservedBase =
    MINI_KEYBOARD_HEIGHT + MINI_KEYBOARD_SPACING + qwertyReservedBase;
  const minButtonSize = 20;
  
  const usableWidth = max(0, screen.width - reservedSide);
  
  // Check if there's horizontal space to put mini inputs to the right of the grid
  const gridWidthEstimate = buttonsPerRow * minButtonSize + margin * 2;
  const pianoWidth = MINI_PIANO_WHITE_KEYS.length * getMiniPianoWhiteKeyWidth(false);
  const horizontalSpaceForMini = usableWidth - gridWidthEstimate - pianoWidth > 10;
  
  // Check for narrow screens where we can fit a vertical/rotated piano on the right
  // Rotated piano: width = MINI_KEYBOARD_HEIGHT, height = pianoWidth (all keys stacked)
  const rotatedPianoWidth = MINI_KEYBOARD_HEIGHT + 4; // Piano keys become vertical
  const rotatedPianoHeight = pianoWidth; // Height needed to fit ALL white keys
  const availableHeightForRotated = screen.height - SECONDARY_BAR_BOTTOM - 4; // Leave some margin
  // Only show rotated piano if ALL keys fit vertically
  const narrowVerticalSpace = !horizontalSpaceForMini && 
    usableWidth - gridWidthEstimate - rotatedPianoWidth > 4 &&
    availableHeightForRotated >= rotatedPianoHeight;
  
  // Show mini inputs if there's either vertical or horizontal space
  const availableWithoutMini = screen.height - bottomPadding - baseReservedTop;
  const verticalSpaceForMini =
    availableWithoutMini - keyboardReservedBase >= totalRows * minButtonSize;
  const canFitMini = horizontalSpaceForMini || verticalSpaceForMini || narrowVerticalSpace;
  const showMiniInputs = miniInputsEnabled && canFitMini;
  // Only reserve top space if using vertical layout (not horizontal or narrow vertical)
  const reservedTop = baseReservedTop + (showMiniInputs && verticalSpaceForMini && !horizontalSpaceForMini && !narrowVerticalSpace ? keyboardReservedBase : 0);

  // Calculate maximum button dimensions to fill available space
  const widthLimit = floor((usableWidth - margin * 2) / buttonsPerRow);
  const available = max(0, screen.height - bottomPadding - reservedTop);
  const heightLimit = floor(available / totalRows);
  
  // In single-column mode, maximize pad size - allow non-square pads to fill space
  // Use minimum of width/height to keep some proportionality, but don't cap artificially
  const maxDimension = min(widthLimit, heightLimit);
  const maxButtonSize = 64; // Allow larger pads on bigger screens
  
  // Width fills horizontal space (up to max)
  let buttonWidth = min(maxButtonSize, widthLimit);
  buttonWidth = max(minButtonSize, buttonWidth);
  
  // Height fills vertical space independently (up to max, min of width for proportionality)
  let buttonHeight = min(maxButtonSize, heightLimit);
  buttonHeight = max(minButtonSize, buttonHeight);
  
  // Keep some proportionality - don't let aspect ratio get too extreme
  const aspectRatio = buttonWidth / buttonHeight;
  if (aspectRatio > 1.5) {
    buttonWidth = floor(buttonHeight * 1.5);
  } else if (aspectRatio < 0.67) {
    buttonHeight = floor(buttonWidth * 1.5);
  }

  const buttonsAreaHeight = totalRows * buttonHeight;
  let topButtonY = screen.height - bottomPadding - buttonsAreaHeight;
  topButtonY = max(topButtonY, reservedTop);

  return {
    buttonWidth,
    buttonHeight,
    topButtonY,
    totalRows,
    buttonsPerRow,
    margin,
    bottomPadding,
    hudReserved,
    trackHeight,
    trackSpacing,
    reservedTop,
    melodyButtonRect,
    midiBadge: badgeMetrics,
    compactMode: false,
    sidePanelWidth: reservedSide,
    usableWidth,
    miniInputsEnabled: showMiniInputs,
    miniInputsHorizontal: horizontalSpaceForMini && showMiniInputs,
    miniInputsVertical: narrowVerticalSpace && showMiniInputs && !horizontalSpaceForMini,
  };
}

function getCachedLayout(
  screen,
  {
    songMode,
    pictureOverlay,
    rateLabel,
    rateText,
    sidePanelWidth,
  },
) {
  const key = [
    screen.width,
    screen.height,
    songMode ? 1 : 0,
    pictureOverlay ? 1 : 0,
    rateLabel || "",
    rateText || "",
    sidePanelWidth || 0,
    miniInputsEnabled ? 1 : 0,
  ].join("|");

  if (layoutCache.key === key && layoutCache.layout && layoutCache.midiMetrics) {
    return layoutCache;
  }

  const midiMetrics = computeMidiBadgeMetrics(
    screen,
    resolveMatrixGlyphMetrics(),
    false,
    rateLabel,
    rateText,
  );
  const layout = getButtonLayoutMetrics(screen, {
    songMode,
    pictureOverlay,
    midiMetrics,
    rateLabel,
    rateText,
    sidePanelWidth,
  });

  layoutCache = {
    key,
    layout,
    midiMetrics,
  };

  return layoutCache;
}

function buildPadsBase({ api, screen, layout, matrixGlyphMetrics, num }) {
  if (!api?.painting) return null;

  const gridTop = Math.round(layout.topButtonY);
  const gridHeight = Math.round(layout.buttonHeight * layout.totalRows);

  // Create bitmap only for the button grid area - boxes + grid only, no text
  const painting = api.painting(screen.width, gridHeight, (p) => {
    // Start fully transparent - main bg shows through
    p.wipe(0, 0, 0, 0);

    const gridAlpha = 40;
    const topEdge = 0;
    const bottomEdge = gridHeight;

    // Grid lines
    if (!layout.splitLayout) {
      const gridWidth = layout.buttonsPerRow * layout.buttonWidth;
      const availableWidth = layout.usableWidth ?? screen.width;
      // Left-align grid when mini inputs are to the right, otherwise center
      const baseX = layout.miniInputsHorizontal
        ? layout.margin
        : Math.ceil(
            layout.margin +
              Math.max(0, Math.floor((availableWidth - layout.margin * 2 - gridWidth) / 2)),
          );
      const leftEdge = baseX;
      const rightEdge = Math.round(baseX + gridWidth);

      for (let col = 0; col <= layout.buttonsPerRow; col += 1) {
        const x = Math.round(baseX + col * layout.buttonWidth);
        p.ink(255, 255, 255, gridAlpha).line(x, topEdge, x, bottomEdge);
      }

      for (let row = 0; row <= layout.totalRows; row += 1) {
        const y = Math.round(row * layout.buttonHeight);
        p.ink(255, 255, 255, gridAlpha).line(leftEdge, y, rightEdge, y);
      }
    }

    // Colored pad boxes (no text - text drawn live)
    buttonNotes.forEach((note) => {
      const btn = buttons[note];
      if (!btn) return;

      const outOctave =
        parseInt(octave) +
        (note.startsWith("+") ? upperOctaveShift : lowerOctaveShift);
      const baseColor = colorFromNote(note, num, outOctave);
      const isSemitone = note.includes("#");
      // Semitones get darker for clear visual distinction from white notes
      const tinted = isSemitone ? darkenColor(baseColor, 0.45) : baseColor;

      // Offset box Y by gridTop since painting starts at gridTop
      const relBox = { ...btn.box, y: btn.box.y - gridTop };
      p.ink(tinted, 196).box(relBox);
      p.ink(90, 110, 120).box(relBox, "outline");
    });
  });

  return { painting, gridTop };
}

function paint({
  wipe,
  ink,
  write,
  screen,
  box,
  line,
  plot,
  sound,
  typeface,
  help,
  num,
  layer,
  paste,
  page,
  api,
  paintCount,
  zoom,
  blur,
  scroll
}) {
  const paintStart = performance.now();
  
  // Track FPS - optimized to reduce calculations
  const lastTimestamp = perfStats.lastFrameTimestamp;
  if (lastTimestamp > 0) {
    const delta = paintStart - lastTimestamp;
    if (delta > 0) {
      perfStats.frameTime = delta;
      const instantFps = 1000 / delta;
      const frameCount = perfStats.fpsFrameCount || 0;
      // Quick ramp up for first 10 frames, then smooth
      const smoothing = frameCount < 10 ? 0.5 : 0.8;
      perfStats.fps = perfStats.fps === 0 ? instantFps : perfStats.fps * smoothing + instantFps * (1 - smoothing);
      perfStats.fpsFrameCount = frameCount + 1;
    }
  }
  perfStats.lastFrameTimestamp = paintStart;
  
  const active = orderedByCount(sounds);
  const scopeSamples = Math.max(1, Math.floor(scope || 1));
  const rawWaveformsLeft = sound.speaker?.waveforms?.left;
  const waveformsAvailable =
    rawWaveformsLeft &&
    typeof rawWaveformsLeft.length === "number" &&
    rawWaveformsLeft.length > 0;
  const amplitudeRaw = sound.speaker?.amplitudes?.left;
  const amplitude =
    typeof amplitudeRaw === "number" && Number.isFinite(amplitudeRaw)
      ? amplitudeRaw
      : 0;
  const needsWaveforms =
    visualizerFullscreen || projector || paintPictureOverlay || active.length > 0 || amplitude > 0.0005;
  let sanitizedWaveforms = [];
  if (waveformsAvailable && needsWaveforms) {
    if (
      waveformsCache.source !== rawWaveformsLeft ||
      waveformsCache.scopeSamples !== scopeSamples
    ) {
      const resampled = help.resampleArray(rawWaveformsLeft, scopeSamples);
      sanitizedWaveforms = resampled.map((value) =>
        typeof value === "number" && Number.isFinite(value) ? value : 0,
      );
      waveformsCache = {
        source: rawWaveformsLeft,
        scopeSamples,
        resampled,
        sanitized: sanitizedWaveforms,
      };
    } else {
      sanitizedWaveforms = waveformsCache.sanitized || [];
    }
  }
  if (zeroWaveformsCache.length !== scopeSamples) {
    zeroWaveformsCache = {
      length: scopeSamples,
      buffer: new Array(scopeSamples).fill(0),
    };
  }
  const waveformsForBars =
    sanitizedWaveforms.length > 0
      ? sanitizedWaveforms
      : zeroWaveformsCache.buffer;
  const waveformsForOverlay = sanitizedWaveforms.length > 0 ? sanitizedWaveforms : [];
  const audioReady =
    waveformsAvailable &&
    typeof amplitudeRaw === "number" &&
    Number.isFinite(amplitudeRaw);
  const matrixGlyphMetrics = resolveMatrixGlyphMetrics(typeface);
  let bg;
  const latestNote = active.length > 0 ? active[active.length - 1] : null;
  const activeBg = latestNote
    ? darkenColor(colorFromNote(latestNote, num), tap ? 0.55 : 0.45)
    : null;

  if (!tap) {
    bg = activeBg ?? [50, 50, 255];

    if (perc && !activeBg) {
      bg = perc;
      perc = null;
    }
  } else {
    bg = activeBg ?? [0, 0, 180];
  }

  if (paintPictureOverlay) {
    wipe(0);

    if (active.length === 0) {
      page(picture);
      wipe(0, 0);
      page(screen);
    }

    pictureLines(
      {
        page,
        ink,
        wipe,
        screen,
        box,
        blur,
        line,
        plot,
        num,
        paintCount,
        sound,
        scroll,
      },
      {
        amplitude,
        waveforms: waveformsForOverlay,
        audioReady,
      },
    );

    page(picture);
    page(screen);

    paste(picture, 0, 0, { width: screen.width, height: screen.height });

    return;
  }
  
  // 🎭 Recital mode color scheme - dynamically set by active note
  // Get the first active note to determine the wireframe color
  const activeNoteKeys = Object.keys(sounds);
  let recitalColor = [0, 255, 220]; // Default cyan wireframe color
  let recitalDim = [0, 120, 100]; // Dimmer version for inactive
  
  if (recitalMode && activeNoteKeys.length > 0) {
    // Use the most recent/first active note's color
    const activeNote = activeNoteKeys[0];
    const parsed = parseNotepatNote(activeNote);
    if (parsed) {
      const noteColor = getNoteColorWithOctave(parsed.baseNote, parsed.octave);
      // Brighten the color for wireframe visibility
      recitalColor = [
        Math.min(255, noteColor[0] + 80),
        Math.min(255, noteColor[1] + 80),
        Math.min(255, noteColor[2] + 80),
      ];
      // Create dim version
      recitalDim = [
        Math.floor(recitalColor[0] * 0.5),
        Math.floor(recitalColor[1] * 0.5),
        Math.floor(recitalColor[2] * 0.5),
      ];
    }
  }
  
  // 🎨 Fullscreen visualizer mode - draw bars as background behind everything
  if (visualizerFullscreen && !recitalMode) {
    wipe(0); // Black background first
    sound.paint.bars(
      api,
      amplitude,
      waveformsForBars,
      0,
      0,
      screen.width,
      screen.height,
      [255, 0, 0, 255],
      { primaryColor, secondaryColor },
    );
  } else if (recitalMode) {
    wipe(0); // Pure black background for recital mode
    // Draw fullscreen visualizer behind wireframe UI
    sound.paint.bars(
      api,
      amplitude,
      waveformsForBars,
      0,
      0,
      screen.width,
      screen.height,
      [255, 0, 0, 255],
      { primaryColor: recitalColor, secondaryColor: recitalDim },
    );
    // Update blink phase for back button
    recitalBlinkPhase = (recitalBlinkPhase + 0.05) % (Math.PI * 2);
  } else {
    wipe(bg);
  }

  // 🎹 Draw mini piano strip in top bar (not in recital mode or fullscreen modes)
  // Store piano end position for visualizer to use
  let topBarPianoEndX = 54; // Default if piano not shown
  if (!recitalMode && !visualizerFullscreen && !paintPictureOverlay && !projector) {
    const topPianoY = 3;
    const topPianoHeight = 15;
    const topPianoStartX = 54;
    const topPianoWidth = Math.min(140, Math.floor((screen.width - topPianoStartX) * 0.5));
    const topPianoWhiteKeyWidth = Math.floor(topPianoWidth / MINI_PIANO_WHITE_KEYS.length);
    const topPianoBlackKeyWidth = Math.floor(topPianoWhiteKeyWidth * 0.6);
    const topPianoBlackKeyHeight = Math.floor(topPianoHeight * 0.55);
    const topPianoStripHeight = 2;
    topBarPianoEndX = topPianoStartX + MINI_PIANO_WHITE_KEYS.length * topPianoWhiteKeyWidth;
    
    // Pre-calculate common values
    const whiteKeyBottom = topPianoY + topPianoHeight - topPianoStripHeight;
    const blackKeyBottom = topPianoY + topPianoBlackKeyHeight - topPianoStripHeight;
    const whiteKeyW = topPianoWhiteKeyWidth - 1;
    
    // Draw white keys - use for loop instead of forEach for performance
    const whiteKeyCount = MINI_PIANO_WHITE_KEYS.length;
    for (let i = 0; i < whiteKeyCount; i++) {
      const note = MINI_PIANO_WHITE_KEYS[i];
      const x = topPianoStartX + i * topPianoWhiteKeyWidth;
      const noteKey = note.toLowerCase();
      const isActivePlaying = sounds[noteKey] !== undefined;
      
      // Use cached color lookup
      const baseFill = isActivePlaying ? [215, 225, 230] : [195, 205, 210];
      ink(baseFill[0], baseFill[1], baseFill[2]).box(x, topPianoY, whiteKeyW, topPianoHeight);
      
      const stripBase = getCachedColor(noteKey, num);
      if (isActivePlaying) {
        ink(Math.min(255, stripBase[0] + 40), Math.min(255, stripBase[1] + 40), Math.min(255, stripBase[2] + 40)).box(x, whiteKeyBottom, whiteKeyW, topPianoStripHeight);
      } else {
        ink(stripBase[0], stripBase[1], stripBase[2]).box(x, whiteKeyBottom, whiteKeyW, topPianoStripHeight);
      }
      
      ink(90, 110, 120, 80).box(x, topPianoY, whiteKeyW, topPianoHeight, "outline");
    }
    
    // Draw black keys - use for loop
    const blackKeyCount = MINI_PIANO_BLACK_KEYS.length;
    for (let i = 0; i < blackKeyCount; i++) {
      const { note, afterWhite } = MINI_PIANO_BLACK_KEYS[i];
      const x = topPianoStartX + afterWhite * topPianoWhiteKeyWidth + topPianoWhiteKeyWidth - topPianoBlackKeyWidth / 2;
      const noteKey = note.toLowerCase();
      const isActivePlaying = sounds[noteKey] !== undefined;
      
      const baseFill = isActivePlaying ? [28, 32, 36] : [0, 0, 0];
      ink(baseFill[0], baseFill[1], baseFill[2]).box(x, topPianoY, topPianoBlackKeyWidth, topPianoBlackKeyHeight);
      
      const stripBase = getCachedColor(noteKey, num);
      if (isActivePlaying) {
        ink(Math.min(255, stripBase[0] + 40), Math.min(255, stripBase[1] + 40), Math.min(255, stripBase[2] + 40)).box(x, blackKeyBottom, topPianoBlackKeyWidth, topPianoStripHeight);
      } else {
        ink(stripBase[0], stripBase[1], stripBase[2]).box(x, blackKeyBottom, topPianoBlackKeyWidth, topPianoStripHeight);
      }
    }
  }

  const sampleRateText = getSampleRateText(sound?.sampleRate);
  const sampleRateLabel = sampleRateText ? MIDI_RATE_LABEL_TEXT : null;
  const showTrack = Boolean(song) && autopatConfig.showTrack !== false;

  // 🎭 Recital mode: Draw wireframe UI on top of visualizer
  if (recitalMode && !paintPictureOverlay && !projector) {
    // Draw wireframe top bar (outline only, transparent inside)
    ink(...recitalColor, 80).box(0, 0, screen.width, TOP_BAR_BOTTOM, "outline");
    // Horizontal line under top bar
    ink(...recitalColor, 60).line(0, TOP_BAR_BOTTOM - 1, screen.width, TOP_BAR_BOTTOM - 1);
    
    // Draw wireframe secondary bar
    ink(...recitalColor, 60).box(0, SECONDARY_BAR_TOP, screen.width, SECONDARY_BAR_HEIGHT, "outline");
    
    // Wireframe toggle buttons in recital mode
    slideBtn?.paint((btn) => {
      const active = slide;
      ink(...(active ? recitalColor : recitalDim), active ? 180 : 80).box(btn.box, "outline");
      ink(...(active ? recitalColor : recitalDim), active ? 200 : 100).write(secondaryBarLabels.slide, { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });
    
    roomBtn?.paint((btn) => {
      const active = roomMode;
      ink(...(active ? recitalColor : recitalDim), active ? 180 : 80).box(btn.box, "outline");
      ink(...(active ? recitalColor : recitalDim), active ? 200 : 100).write(secondaryBarLabels.room, { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });

    glitchBtn?.paint((btn) => {
      const active = glitchMode;
      ink(...(active ? recitalColor : recitalDim), active ? 180 : 80).box(btn.box, "outline");
      ink(...(active ? recitalColor : recitalDim), active ? 200 : 100).write(secondaryBarLabels.glitch, { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });
    
    quickBtn?.paint((btn) => {
      const active = quickFade;
      ink(...(active ? recitalColor : recitalDim), active ? 180 : 80).box(btn.box, "outline");
      ink(...(active ? recitalColor : recitalDim), active ? 200 : 100).write(secondaryBarLabels.quick, { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });
    
    // 🥁 Wireframe metronome controls in recital mode
    bpmMinusBtn?.paint((btn) => {
      ink(...recitalDim, 80).box(btn.box, "outline");
      ink(...recitalDim, 100).write("-", { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });
    
    // BPM display IS the toggle button now
    metroBtn?.paint((btn) => {
      const bpmText = metronomeBPM.toString().padStart(3, " ");
      const active = metronomeEnabled;
      // Flash on beat when enabled
      const flashAlpha = active && metronomeVisualPhase > 0.5 ? 255 : (active ? 180 : 80);
      ink(...(active ? recitalColor : recitalDim), flashAlpha).box(btn.box, "outline");
      ink(...(active ? recitalColor : recitalDim), active ? 200 : 100).write(bpmText, { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });
    
    bpmPlusBtn?.paint((btn) => {
      ink(...recitalDim, 80).box(btn.box, "outline");
      ink(...recitalDim, 100).write("+", { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });
    
    // 🔙 Blinking "BACK" text - drawn LAST so it's on top of everything
    const blinkAlpha = Math.floor(180 + Math.sin(recitalBlinkPhase * 3) * 75);
    ink(...recitalColor, blinkAlpha).write("< BACK", { x: 6, y: 6 }, undefined, undefined, false, "MatrixChunky8");
  }

  // 🎛️ Draw secondary top bar with toggle buttons (normal mode)
  if (!paintPictureOverlay && !projector && !visualizerFullscreen && !recitalMode) {
    // Draw segmented secondary bar backgrounds with distinct colors
    // Calculate segment boundaries
    const midiBadgeEnd = MIDI_BADGE_MARGIN + (computeMidiBadgeTopMetrics(screen, matrixGlyphMetrics, sampleRateLabel, sampleRateText)?.width || 60) + 2;
    const metroStart = bpmMinusBtn?.box?.x ?? midiBadgeEnd + 40;
    const toggleStart = slideBtn?.box?.x ?? screen.width - 80;
    
    // Segment 1: MIDI badge area (dark blue tint)
    ink(15, 20, 35, 180).box(0, SECONDARY_BAR_TOP, midiBadgeEnd, SECONDARY_BAR_HEIGHT);
    
    // Segment 2: Content area - stream & notes (dark neutral)
    ink(20, 20, 25, 160).box(midiBadgeEnd, SECONDARY_BAR_TOP, metroStart - midiBadgeEnd, SECONDARY_BAR_HEIGHT);
    
    // Segment 3: Metronome controls (dark warm/red tint)
    ink(30, 18, 18, 180).box(metroStart, SECONDARY_BAR_TOP, toggleStart - metroStart, SECONDARY_BAR_HEIGHT);
    
    // Segment 4: Toggle buttons (dark purple tint)
    ink(25, 18, 30, 180).box(toggleStart, SECONDARY_BAR_TOP, screen.width - toggleStart, SECONDARY_BAR_HEIGHT);
    
    // Paint slide button
    slideBtn?.paint((btn) => {
      const base = [0, 220, 140];
      let bgR, bgG, bgB, bgA, olR, olG, olB, olA;
      if (slide) {
        bgR = base[0]; bgG = base[1]; bgB = base[2]; bgA = 230;
        olR = 0; olG = 255; olB = 170; olA = 255;
      } else {
        bgR = Math.round(base[0] * 0.3); bgG = Math.round(base[1] * 0.3); bgB = Math.round(base[2] * 0.3); bgA = 170;
        olR = 70; olG = 90; olB = 80; olA = 180;
      }
      const textColor = slide ? "white" : [180, 190, 200];
      ink(bgR, bgG, bgB, bgA).box(btn.box);
      ink(olR, olG, olB, olA).box(btn.box, "outline");
      if (btn.over && !btn.down) {
        ink(255, 255, 255, 24).box(btn.box);
      }
      ink(textColor).write(secondaryBarLabels.slide, { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });
    
    // Paint room button
    roomBtn?.paint((btn) => {
      const base = [150, 110, 255];
      let bgR, bgG, bgB, bgA, olR, olG, olB, olA;
      if (roomMode) {
        bgR = base[0]; bgG = base[1]; bgB = base[2]; bgA = 230;
        olR = 190; olG = 160; olB = 255; olA = 255;
      } else {
        bgR = Math.round(base[0] * 0.3); bgG = Math.round(base[1] * 0.3); bgB = Math.round(base[2] * 0.3); bgA = 170;
        olR = 90; olG = 80; olB = 120; olA = 180;
      }
      const textColor = roomMode ? "white" : [180, 190, 200];
      ink(bgR, bgG, bgB, bgA).box(btn.box);
      ink(olR, olG, olB, olA).box(btn.box, "outline");
      if (btn.over && !btn.down) {
        ink(255, 255, 255, 24).box(btn.box);
      }
      ink(textColor).write(secondaryBarLabels.room, { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });

    // Paint glitch button
    glitchBtn?.paint((btn) => {
      const base = [255, 80, 160];
      let bgR, bgG, bgB, bgA, olR, olG, olB, olA;
      if (glitchMode) {
        bgR = base[0]; bgG = base[1]; bgB = base[2]; bgA = 230;
        olR = 255; olG = 140; olB = 210; olA = 255;
      } else {
        bgR = Math.round(base[0] * 0.3); bgG = Math.round(base[1] * 0.3); bgB = Math.round(base[2] * 0.3); bgA = 170;
        olR = 110; olG = 70; olB = 100; olA = 180;
      }
      const textColor = glitchMode ? "white" : [180, 190, 200];
      ink(bgR, bgG, bgB, bgA).box(btn.box);
      ink(olR, olG, olB, olA).box(btn.box, "outline");
      if (btn.over && !btn.down) {
        ink(255, 255, 255, 24).box(btn.box);
      }
      ink(textColor).write(secondaryBarLabels.glitch, { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });
    
    // Paint quick button
    quickBtn?.paint((btn) => {
      const base = [255, 170, 0];
      let bgR, bgG, bgB, bgA, olR, olG, olB, olA;
      if (quickFade) {
        bgR = base[0]; bgG = base[1]; bgB = base[2]; bgA = 230;
        olR = 255; olG = 200; olB = 80; olA = 255;
      } else {
        bgR = Math.round(base[0] * 0.3); bgG = Math.round(base[1] * 0.3); bgB = Math.round(base[2] * 0.3); bgA = 170;
        olR = 120; olG = 100; olB = 60; olA = 180;
      }
      const textColor = quickFade ? "white" : [180, 190, 200];
      ink(bgR, bgG, bgB, bgA).box(btn.box);
      ink(olR, olG, olB, olA).box(btn.box, "outline");
      if (btn.over && !btn.down) {
        ink(255, 255, 255, 24).box(btn.box);
      }
      ink(textColor).write(secondaryBarLabels.quick, { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });

    // 🥁 Paint metronome controls: [-] [BPM] [+] [metro]
    const metroBase = [255, 100, 100]; // Red-ish for metronome
    
    // Paint BPM minus button (RED color coded)
    bpmMinusBtn?.paint((btn) => {
      let bgR, bgG, bgB, bgA, olR, olG, olB, olA;
      if (btn.down) {
        bgR = 220; bgG = 60; bgB = 60; bgA = 230;
        olR = 255; olG = 100; olB = 100; olA = 255;
      } else {
        bgR = 140; bgG = 40; bgB = 40; bgA = 200;
        olR = 180; olG = 60; olB = 60; olA = 200;
      }
      const textColor = btn.down ? "white" : [255, 120, 120];
      ink(bgR, bgG, bgB, bgA).box(btn.box);
      ink(olR, olG, olB, olA).box(btn.box, "outline");
      if (btn.over && !btn.down) ink(255, 255, 255, 24).box(btn.box);
      ink(textColor).write("-", { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });
    
    // BPM display IS the toggle button now - clicking it toggles metronome on/off (BLACK/WHITE themed)
    metroBtn?.paint((btn) => {
      const bpmText = metronomeBPM.toString().padStart(3, " ");
      // Flash background on beat when metronome is enabled
      const flashAlpha = metronomeEnabled ? Math.floor(40 + metronomeVisualPhase * 100) : 0;
      // Black/white theme for BPM value
      let bgR, bgG, bgB, bgA, olR, olG, olB, olA;
      if (metronomeEnabled) {
        bgR = 40; bgG = 40; bgB = 40; bgA = 230;
        olR = 255; olG = 255; olB = 255; olA = 255;
      } else {
        bgR = 20; bgG = 20; bgB = 20; bgA = 200;
        olR = 80; olG = 80; olB = 80; olA = 180;
      }
      ink(bgR, bgG, bgB, bgA).box(btn.box);
      if (metronomeEnabled && metronomeVisualPhase > 0) {
        ink(255, 255, 255, flashAlpha).box(btn.box);
      }
      ink(olR, olG, olB, olA).box(btn.box, "outline");
      if (btn.over && !btn.down) ink(255, 255, 255, 24).box(btn.box);
      // Flash the button on beat
      if (metronomeEnabled && metronomeVisualPhase > 0.5) {
        ink(255, 255, 255, Math.floor(metronomeVisualPhase * 60)).box(btn.box);
      }
      ink(metronomeEnabled ? "white" : [160, 160, 160]).write(bpmText, { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });
    
    // Paint BPM plus button (GREEN color coded)
    bpmPlusBtn?.paint((btn) => {
      let bgR, bgG, bgB, bgA, olR, olG, olB, olA;
      if (btn.down) {
        bgR = 60; bgG = 200; bgB = 80; bgA = 230;
        olR = 100; olG = 255; olB = 120; olA = 255;
      } else {
        bgR = 40; bgG = 120; bgB = 50; bgA = 200;
        olR = 60; olG = 160; olB = 70; olA = 200;
      }
      const textColor = btn.down ? "white" : [120, 255, 140];
      ink(bgR, bgG, bgB, bgA).box(btn.box);
      ink(olR, olG, olB, olA).box(btn.box, "outline");
      if (btn.over && !btn.down) ink(255, 255, 255, 24).box(btn.box);
      ink(textColor).write("+", { x: btn.box.x + TOGGLE_BTN_PADDING_X, y: btn.box.y + TOGGLE_BTN_PADDING_Y }, undefined, undefined, false, "MatrixChunky8");
    });

    // 🏀 Draw bouncing ball animation when metronome is running
    if (metronomeEnabled && bpmPlusBtn?.box && slideBtn?.box) {
      const ballTrackStartX = bpmPlusBtn.box.x + bpmPlusBtn.box.w + 4;
      const ballTrackEndX = slideBtn.box.x - 4;
      const ballTrackWidth = ballTrackEndX - ballTrackStartX;
      
      if (ballTrackWidth > 16) {
        const ballY = SECONDARY_BAR_TOP + Math.floor(SECONDARY_BAR_HEIGHT / 2);
        const ballRadius = 3;
        // Ball bounces from left to right based on metronomeBallPos (0-1)
        const ballX = ballTrackStartX + metronomeBallPos * ballTrackWidth;
        
        // Draw faint track line
        ink(80, 80, 80, 100).line(ballTrackStartX, ballY, ballTrackEndX, ballY);
        
        // Draw the bouncing ball with glow effect
        const flashIntensity = metronomeVisualPhase;
        const ballFloorX = Math.floor(ballX);
        let ballR, ballG, ballB, ballA, glowR, glowG, glowB, glowA;
        if (flashIntensity > 0.5) {
          ballR = 255; ballG = 255; ballB = 255; ballA = 255;
          glowR = 255; glowG = 255; glowB = 255; glowA = Math.floor(80 + flashIntensity * 100);
        } else {
          ballR = 200; ballG = 200; ballB = 255; ballA = 220;
          glowR = 150; glowG = 150; glowB = 255; glowA = 60;
        }
        
        // Glow
        ink(glowR, glowG, glowB, glowA).box(ballFloorX - ballRadius - 1, ballY - ballRadius - 1, ballRadius * 2 + 3, ballRadius * 2 + 3);
        // Ball
        ink(ballR, ballG, ballB, ballA).box(ballFloorX - ballRadius, ballY - ballRadius, ballRadius * 2 + 1, ballRadius * 2 + 1);
      }
    }

    // 🔌🎹 Draw USB/MIDI/sample-rate badge on the left side of the mini bar
    const topMidiMetrics = computeMidiBadgeTopMetrics(
      screen,
      matrixGlyphMetrics,
      sampleRateLabel,
      sampleRateText,
    );
    // Pass metronome button start as maxX to prevent FPS from overlapping
    // Ensure maxX is reasonable (at least 80px from left) to avoid cutting off FPS
    const bpmBtnX = bpmMinusBtn?.box?.x;
    const midiMaxX = (bpmBtnX && bpmBtnX > 80) ? bpmBtnX - 4 : screen.width;
    drawMidiBadge(
      topMidiMetrics,
      midiConnected,
      sampleRateLabel,
      sampleRateText,
      midiMaxX,
    );

    const streamLeft = topMidiMetrics.x + topMidiMetrics.width + 3;
    // Stream should stop before metronome buttons (bpmMinusBtn), not the toggle buttons
    const streamRight = bpmMinusBtn?.box?.x ? bpmMinusBtn.box.x - 2 : (slideBtn?.box?.x ? slideBtn.box.x - 4 : screen.width - 4);
    const streamMaxWidth = streamRight - streamLeft;
    // Use up to 40% of available space for stream, rest for active notes list
    const streamWidth = Math.max(0, Math.min(Math.floor(streamMaxWidth * 0.4), streamMaxWidth));
    const streamHeight = 6;
    if (streamWidth > 6) {
      const streamY =
        SECONDARY_BAR_TOP +
        Math.floor((SECONDARY_BAR_HEIGHT - streamHeight) / 2);
      drawSampleStream(
        {
          x: streamLeft,
          y: streamY,
          width: streamWidth,
          height: streamHeight,
        },
        waveformsForBars,
        paintCount,
      );
    }

    // Linear active note list (colored) between stream and metronome buttons
    const listStartX = streamLeft + (streamWidth > 0 ? streamWidth + 4 : 0);
    // List should also stop before metronome buttons
    const listRight = bpmMinusBtn?.box?.x ? bpmMinusBtn.box.x - 2 : (slideBtn?.box?.x ? slideBtn.box.x - 4 : screen.width - 4);
    const listHeight = 8;
    const listY =
      SECONDARY_BAR_TOP +
      Math.floor((SECONDARY_BAR_HEIGHT - listHeight) / 2);
    const activeNotes = orderedByCount(sounds);
    const activeNotesLen = activeNotes.length;
    if (activeNotesLen > 0 && listRight - listStartX > 8) {
      let x = listStartX;
      const mgWidth = matrixGlyphMetrics.width;
      for (let ani = 0; ani < activeNotesLen; ani++) {
        const note = activeNotes[ani];
        if (!note) continue;
        const keyLabel = noteToKeyboardKey(note) || note;
        const label = formatKeyLabel(keyLabel);
        const labelWidth = label.length * mgWidth;
        const boxW = Math.max(6, labelWidth + 4);
        if (x + boxW > listRight) break;

        const baseColor = getCachedColor(note, num);
        const textColor = getContrastingTextColor(baseColor);
        const outline = darkenColor(baseColor, 0.7);
        const shadow = darkenColor(baseColor, 0.85);
        const labelX = x + Math.max(1, Math.floor((boxW - labelWidth) / 2));
        const labelY = listY + 0;

        ink(baseColor[0], baseColor[1], baseColor[2], 200).box(x, listY, boxW, listHeight);
        ink(outline[0], outline[1], outline[2], 200).box(x, listY, boxW, listHeight, "outline");
        ink(shadow[0], shadow[1], shadow[2], 220).write(
          label,
          { x: labelX + 1, y: labelY + 1 },
          undefined,
          undefined,
          false,
          "MatrixChunky8",
        );
        ink(textColor[0], textColor[1], textColor[2]).write(
          label,
          { x: labelX, y: labelY },
          undefined,
          undefined,
          false,
          "MatrixChunky8",
        );

        x += boxW + 2;
      }
    }
  }

  // wipe(!projector ? bg : 64);

  // QR Code
  // if (qrcells) {
  //   const ox = screen.width - qrcells.length - 34;
  //   const oy = 0;
  //   const scale = 1;
  //   for (let y = 0; y < qrcells.length; y += 1) {
  //     for (let x = 0; x < qrcells.length; x += 1) {
  //       const black = qrcells[y][x];
  //       ink(!black).box(ox + x * scale, oy + y * scale, scale);
  //     }
  //   }
  // }

  // Song

  // TODO: Precompute the full song length with x stops next to indices.

  const trackHeight = showTrack ? TRACK_HEIGHT : 0;
  const trackY = showTrack ? SECONDARY_BAR_BOTTOM : null;

  if (showTrack) {
    const glyphWidth = matrixGlyphMetrics.width;
    const startX = 6 - songShift;
    let i = 0;

    ink(20, 30, 50).box(0, trackY, screen.width, trackHeight);

    while (i < song.length) {
      let word = songStops[i][0];
      let x = startX + songStops[i][1];

      if (x > screen.width) break;

      const current = i === songIndex;

      // Word color (lyrics) - match the cyan palette from the board
      if (current) {
        const wordColor = songNoteDown ? [180, 200, 210] : "white"; // Soften while holding
        ink(wordColor).write(word, x, trackY + 3);
      } else {
        ink(120, 140, 150).write(word, x, trackY + 3); // Lighter gray-blue
      }

      // Note color (musical note) - match the cyan boxes
      if (current) {
        const noteColor = songNoteDown ? [0, 140, 160] : [0, 180, 200]; // Keep visible while acknowledging hold
        ink(noteColor).write(song[i][0], x, trackY + 13);
      } else {
        ink(80, 100, 120).write(song[i][0], x, trackY + 13); // Darker gray-blue
      }

      i += 1;
    }
  }
  
  // Get layout info early for positioning piano/qwerty in compact mode
  const cachedLayout = getCachedLayout(screen, {
    songMode: Boolean(showTrack),
    pictureOverlay: paintPictureOverlay,
    rateText: sampleRateText,
    rateLabel: sampleRateLabel,
    sidePanelWidth: autopatConfig.sidePanelWidth,
  });
  const layout = cachedLayout.layout;
  const usePadsBase = !song && !projector && !paintPictureOverlay && !recitalMode;
  const padsBaseCacheKey = [
    cachedLayout.key,
    octave,
    upperOctaveShift,
    lowerOctaveShift,
    recitalMode ? 1 : 0,
  ].join("|");
  
  if (layout.miniInputsEnabled) {
    const pianoGeometry = getMiniPianoGeometry({
      screen,
      layout,
      song: showTrack ? song : null,
      trackY,
      trackHeight,
    });
    let pianoY = pianoGeometry.y;
    let pianoStartX = pianoGeometry.x;

    // Skip main piano drawing - now using top bar piano instead
    // Keep pianoGeometry calculations for QWERTY positioning
    if (false && !pianoGeometry.hidden) { // Piano disabled - use top bar piano
    const whiteKeyWidth = getMiniPianoWhiteKeyWidth(layout.compactMode);
    const whiteKeyHeight = MINI_KEYBOARD_HEIGHT;
    const blackKeyWidth = getMiniPianoBlackKeyWidth(layout.compactMode);
    const blackKeyHeight = getMiniPianoBlackKeyHeight(layout.compactMode);

    const whiteKeys = MINI_PIANO_WHITE_KEYS;
    const blackKeys = MINI_PIANO_BLACK_KEYS;
    const totalWhiteKeys = whiteKeys.length;
    const pianoWidth = totalWhiteKeys * whiteKeyWidth;

    // � Rotated vertical piano mode (for narrow screens)
    if (pianoGeometry.rotated && !recitalMode) {
      // In rotated mode: keys run vertically, white keys are stacked from top to bottom
      // Each key is drawn as a horizontal bar
      const keyHeight = whiteKeyWidth; // Key "width" becomes vertical height
      const keyWidth = whiteKeyHeight; // Key "height" becomes horizontal width
      const blackKeyH = blackKeyWidth; // Black key vertical size
      const blackKeyW = blackKeyHeight; // Black key horizontal size
      
      // Draw white keys (stacked vertically, lowest note at bottom)
      const wkLen = whiteKeys.length;
      for (let index = 0; index < wkLen; index++) {
        const note = whiteKeys[index];
        // Reverse index so low notes are at bottom
        const reversedIndex = totalWhiteKeys - 1 - index;
        const y = pianoY + reversedIndex * keyHeight;
        const x = pianoStartX;
        const noteKey = note.toLowerCase();
        const isActivePlaying = sounds[noteKey] !== undefined;
        
        if (isActivePlaying) {
          ink(215, 225, 230).box(x, y, keyWidth, keyHeight - 1);
        } else {
          ink(195, 205, 210).box(x, y, keyWidth, keyHeight - 1);
        }
        
        // Color strip on right edge
        const stripBase = getCachedColor(noteKey, num);
        const stripColor = isActivePlaying ? brightenColor(stripBase, 40) : stripBase;
        const stripWidth = 3;
        ink(stripColor[0], stripColor[1], stripColor[2]).box(x + keyWidth - stripWidth, y, stripWidth, keyHeight - 1);
        ink(90, 110, 120).box(x, y, keyWidth, keyHeight - 1, "outline");
      }
      
      // Draw black keys (overlaid, positioned between white keys)
      const bkLen = blackKeys.length;
      for (let bi = 0; bi < bkLen; bi++) {
        const { note, afterWhite } = blackKeys[bi];
        // afterWhite indicates which white key this black key is after
        // In vertical mode, we position it between the reversed indices
        const reversedAfterWhite = totalWhiteKeys - 1 - afterWhite;
        const y = pianoY + reversedAfterWhite * keyHeight + keyHeight - blackKeyH / 2;
        const x = pianoStartX;
        const noteKey = note.toLowerCase();
        const isActivePlaying = sounds[noteKey] !== undefined;
        
        // Black keys are pure black now
        if (isActivePlaying) {
          ink(28, 32, 36).box(x, y, blackKeyW, blackKeyH);
        } else {
          ink(0, 0, 0).box(x, y, blackKeyW, blackKeyH);
        }
        
        // White text label for black key
        if (blackKeyH >= 6) {
          const label = noteKey.replace("#", "").toUpperCase();
          ink(255, 255, 255, isActivePlaying ? 255 : 180).write(
            label,
            { x: x + 1, y: y + 1 },
            undefined,
            undefined,
            false,
            "MatrixChunky8",
          );
        }
      }
    } else if (recitalMode) {
      // 🎭 Recital mode: Wireframe piano
      const rR = recitalColor[0], rG = recitalColor[1], rB = recitalColor[2];
      // Draw piano outline
      ink(rR, rG, rB, 80).box(pianoStartX, pianoY, pianoWidth, whiteKeyHeight, "outline");
      // Draw vertical separators between white keys
      const rwkLen = whiteKeys.length;
      for (let index = 0; index < rwkLen; index++) {
        const note = whiteKeys[index];
        const x = pianoStartX + index * whiteKeyWidth;
        const noteKey = note.toLowerCase();
        const isActivePlaying = sounds[noteKey] !== undefined;
        if (isActivePlaying) {
          ink(rR, rG, rB, 200).box(x, pianoY, whiteKeyWidth - 1, whiteKeyHeight);
        }
        ink(rR, rG, rB, 50).line(x, pianoY, x, pianoY + whiteKeyHeight);
      }
      // Draw black key outlines
      const rbkLen = blackKeys.length;
      for (let bi = 0; bi < rbkLen; bi++) {
        const { note, afterWhite } = blackKeys[bi];
        const x = pianoStartX + afterWhite * whiteKeyWidth + whiteKeyWidth - blackKeyWidth / 2;
        const noteKey = note.toLowerCase();
        const isActivePlaying = sounds[noteKey] !== undefined;
        if (isActivePlaying) {
          ink(rR, rG, rB, 200).box(x, pianoY, blackKeyWidth, blackKeyHeight);
        } else {
          ink(rR, rG, rB, 100).box(x, pianoY, blackKeyWidth, blackKeyHeight, "outline");
        }
      }
    } else if (!pianoGeometry.rotated) {
    // Normal mode: Draw white keys
    const normalStripHeight = layout.compactMode ? 2 : 3;
    const songCurrentNoteForPiano = song?.[songIndex]?.[0];
    const nwkLen = whiteKeys.length;
    for (let index = 0; index < nwkLen; index++) {
      const note = whiteKeys[index];
      const x = pianoStartX + index * whiteKeyWidth;
      const noteKey = note.toLowerCase();
      const isCurrentNote = song && note === songCurrentNoteForPiano;
      const isActivePlaying = sounds[noteKey] !== undefined;
      const isCurrentAndPlaying = isCurrentNote && isActivePlaying;
      const isCurrentAwaiting = isCurrentNote && !isActivePlaying;

      // Inline fill colors to avoid array allocation and spread
      let bfR, bfG, bfB;
      if (isCurrentAndPlaying) { bfR = 230; bfG = 240; bfB = 245; }
      else if (isActivePlaying) { bfR = 215; bfG = 225; bfB = 230; }
      else { bfR = 195; bfG = 205; bfB = 210; }
      ink(bfR, bfG, bfB).box(x, pianoY, whiteKeyWidth - 1, whiteKeyHeight);

      const stripBase = getCachedColor(noteKey, num);
      const stripColor = isCurrentAndPlaying
        ? brightenColor(stripBase, 70)
        : isCurrentAwaiting
        ? darkenColor(stripBase, 0.6)
        : isActivePlaying
        ? brightenColor(stripBase, 40)
        : stripBase;
      ink(stripColor[0], stripColor[1], stripColor[2]).box(
        x,
        pianoY + whiteKeyHeight - normalStripHeight,
        whiteKeyWidth - 1,
        normalStripHeight,
      );
      ink(90, 110, 120).box(x, pianoY, whiteKeyWidth - 1, whiteKeyHeight, "outline"); // Border
    }

    // Draw black keys on top (normal mode)
    const nbkLen = blackKeys.length;
    for (let bi = 0; bi < nbkLen; bi++) {
      const { note, afterWhite } = blackKeys[bi];
      const x = pianoStartX + afterWhite * whiteKeyWidth + whiteKeyWidth - blackKeyWidth / 2;
      const noteKey = note.toLowerCase();
      const isCurrentNote = song && note === songCurrentNoteForPiano;
      const isActivePlaying = sounds[noteKey] !== undefined;
      const isCurrentAndPlaying = isCurrentNote && isActivePlaying;
      const isCurrentAwaiting = isCurrentNote && !isActivePlaying;

      // Inline fill colors to avoid array allocation and spread
      let bbfR, bbfG, bbfB;
      if (isCurrentAndPlaying) { bbfR = 35; bbfG = 40; bbfB = 45; }
      else if (isActivePlaying) { bbfR = 28; bbfG = 32; bbfB = 36; }
      else { bbfR = 14; bbfG = 18; bbfB = 20; }
      ink(bbfR, bbfG, bbfB).box(x, pianoY, blackKeyWidth, blackKeyHeight);

      const stripBase = getCachedColor(noteKey, num);
      const stripColor = isCurrentAndPlaying
        ? brightenColor(stripBase, 70)
        : isCurrentAwaiting
        ? darkenColor(stripBase, 0.6)
        : isActivePlaying
        ? brightenColor(stripBase, 40)
        : stripBase;
      ink(stripColor[0], stripColor[1], stripColor[2]).box(
        x,
        pianoY + blackKeyHeight - normalStripHeight,
        blackKeyWidth,
        normalStripHeight,
      );
    }
    } // End of normal mode piano/keys block

    // Show QWERTY minimap in compact mode (always) or in song mode (skip in rotated mode - no space)
    if ((layout.compactMode || song) && !pianoGeometry.rotated) {
      const currentKeyLetter = song ? noteToKeyboardKey(song?.[songIndex]?.[0]) : null;
      const nextKeyLetter = song ? noteToKeyboardKey(song?.[songIndex + 1]?.[0]) : null;
      const activeKeyLetters = new Set(
        Object.keys(sounds)
          .map((activeNote) => noteToKeyboardKey(activeNote))
          .filter(Boolean),
      );
      const isPercKeyActive = (key) => {
        if (!key) return false;
        if (miniMapActiveKey === key) return true;
        if (key === "space") return Boolean(percDowns.space);
        if (key === "alt") return Boolean(percDowns.alt);
        if (key === "left") return Boolean(percDowns.left);
        if (key === "right") return Boolean(percDowns.right);
        if (key === "up") return Boolean(percDowns.up);
        if (key === "down") return Boolean(percDowns.down);
        return false;
      };

      const qKeyWidth = 9;
      const qKeyHeight = QWERTY_MINIMAP_KEY_HEIGHT;
      const qKeySpacing = QWERTY_MINIMAP_KEY_SPACING;
      const qwertyStartX = pianoGeometry.qwertyStartX ?? pianoStartX;
      const qwertyStartY =
        pianoGeometry.qwertyStartY ??
        pianoY + whiteKeyHeight + QWERTY_MINIMAP_SPACING;

      // 🎭 Recital mode: Wireframe QWERTY
      if (recitalMode) {
        const recitalR = recitalColor[0], recitalG = recitalColor[1], recitalB = recitalColor[2];
        const qGlyphWidth = matrixGlyphMetrics.width;
        const qGlyphHeight = matrixGlyphMetrics.height;
        const qRowCount = QWERTY_LAYOUT_ROWS.length;
        
        for (let rowIndex = 0; rowIndex < qRowCount; rowIndex++) {
          const row = QWERTY_LAYOUT_ROWS[rowIndex];
          const rowLen = row.length;
          const rowOffset =
            rowIndex === 0
              ? 0
              : rowIndex === 1
              ? (qKeyWidth + qKeySpacing) / 2
              : rowIndex === 2
              ? qKeyWidth
              : Math.floor((pianoWidth - (rowLen * (qKeyWidth + qKeySpacing))) / 2);
          const y = qwertyStartY + rowIndex * (qKeyHeight + qKeySpacing);

          for (let keyIndex = 0; keyIndex < rowLen; keyIndex++) {
            const keyLetter = row[keyIndex];
            const x = qwertyStartX + rowOffset + keyIndex * (qKeyWidth + qKeySpacing);

            const isActiveKey = activeKeyLetters.has(keyLetter) || isPercKeyActive(keyLetter);

            // Wireframe keys - brighter when active
            if (isActiveKey) {
              ink(recitalR, recitalG, recitalB, 200).box(x, y, qKeyWidth, qKeyHeight);
            } else {
              ink(recitalR, recitalG, recitalB, 50).box(x, y, qKeyWidth, qKeyHeight, "outline");
            }

            const label = formatKeyLabel(keyLetter);
            const labelWidth = label.length * qGlyphWidth;
            const labelX = x + (qKeyWidth - labelWidth) / 2;
            const labelY = y + (qKeyHeight - qGlyphHeight) / 2;

            if (labelWidth <= qKeyWidth) {
              ink(recitalR, recitalG, recitalB, isActiveKey ? 255 : 80).write(
                label,
                { x: labelX, y: labelY },
                undefined,
                undefined,
                false,
                "MatrixChunky8",
              );
            }
          }
        }
      } else {
      // Normal mode QWERTY - styled like a real computer keyboard (gray tones)
      // Pre-create special keys Set for O(1) lookup
      const specialKeysSet = new Set(["space", "alt", "left", "right", "up", "down"]);
      const nGlyphWidth = matrixGlyphMetrics.width;
      const nGlyphHeight = matrixGlyphMetrics.height;
      const nRowCount = QWERTY_LAYOUT_ROWS.length;
      
      for (let rowIndex = 0; rowIndex < nRowCount; rowIndex++) {
        const row = QWERTY_LAYOUT_ROWS[rowIndex];
        const rowLen = row.length;
        const rowOffset =
          rowIndex === 0
            ? 0
            : rowIndex === 1
            ? (qKeyWidth + qKeySpacing) / 2
            : rowIndex === 2
            ? qKeyWidth
            : Math.floor((pianoWidth - (rowLen * (qKeyWidth + qKeySpacing))) / 2);
        const y = qwertyStartY + rowIndex * (qKeyHeight + qKeySpacing);

        for (let keyIndex = 0; keyIndex < rowLen; keyIndex++) {
          const keyLetter = row[keyIndex];
          const x = qwertyStartX + rowOffset + keyIndex * (qKeyWidth + qKeySpacing);

          const mappedNote = keyboardKeyToNote(keyLetter);
          const isMapped = Boolean(mappedNote);
          const isBlackKeyMapped = isMapped && isBlackKey(mappedNote);
          const isCurrentKey = keyLetter === currentKeyLetter;
          const isActiveKey = activeKeyLetters.has(keyLetter) || isPercKeyActive(keyLetter);
          const isCurrentAndActive = isCurrentKey && isActiveKey;
          const isNextKey = keyLetter === nextKeyLetter;
          const isSpecialKey = specialKeysSet.has(keyLetter);

          // Computer keyboard style: gray keys, pure black for black note keys
          // Use direct ink calls instead of spread operator
          let fr, fg, fb, fa;
          if (isCurrentAndActive) {
            if (isBlackKeyMapped) { fr = 28; fg = 32; fb = 36; fa = 255; }
            else { fr = 180; fg = 200; fb = 180; fa = 230; }
          } else if (isActiveKey) {
            if (isBlackKeyMapped) { fr = 28; fg = 32; fb = 36; fa = 255; }
            else { fr = 200; fg = 200; fb = 180; fa = 220; }
          } else if (isBlackKeyMapped) {
            fr = 0; fg = 0; fb = 0; fa = 255;
          } else if (isCurrentKey) {
            fr = 140; fg = 160; fb = 160; fa = 200;
          } else if (isNextKey) {
            fr = 120; fg = 140; fb = 150; fa = 180;
          } else if (isSpecialKey) {
            fr = 55; fg = 60; fb = 65; fa = 200;
          } else {
            fr = 70; fg = 75; fb = 80; fa = 200;
          }

          ink(fr, fg, fb, fa).box(x, y, qKeyWidth, qKeyHeight);
          ink(45, 50, 55, 200).box(x, y, qKeyWidth, qKeyHeight, "outline");

          const label = formatKeyLabel(keyLetter);
          const labelWidth = label.length * nGlyphWidth;
          const labelX = x + (qKeyWidth - labelWidth) / 2;
          const labelY = y + (qKeyHeight - nGlyphHeight) / 2;

          // Keyboard-style text colors - tinted by mapped note color
          let tr, tg, tb, ta;
          if (isBlackKeyMapped) {
            // White text on black keys (like the pads)
            tr = 255; tg = 255; tb = 255; ta = isActiveKey ? 255 : 180;
          } else if (isCurrentAndActive || isActiveKey) {
            tr = 30; tg = 30; tb = 30; ta = 255;
          } else if (isMapped) {
            // Tint text with the mapped note's color (brightened for visibility)
            const noteColor = getCachedColor(mappedNote, num);
            tr = Math.min(255, noteColor[0] + 100);
            tg = Math.min(255, noteColor[1] + 100);
            tb = Math.min(255, noteColor[2] + 100);
            ta = 230;
          } else {
            tr = 140; tg = 145; tb = 150; ta = 200;
          }

          // Apply shake offset when playing - only for the specific note being pressed
          const keyNoteShake = mappedNote ? (noteShake[mappedNote] || noteShake[`+${mappedNote}`] || 0) : 0;
          const shakeX = keyNoteShake > 0 ? Math.floor((Math.random() - 0.5) * keyNoteShake * 2) : 0;
          const shakeY = keyNoteShake > 0 ? Math.floor((Math.random() - 0.5) * keyNoteShake * 2) : 0;

          if (labelWidth <= qKeyWidth) {
            ink(tr, tg, tb, ta).write(
              label,
              { x: labelX + shakeX, y: labelY + shakeY },
              undefined,
              undefined,
              false,
              "MatrixChunky8",
            );
          }
        }
      }
      } // End of normal mode QWERTY

      const gamepad = Object.values(connectedGamepads).find(
        (gp) => gp && (gp.id || gp.pressedButtons.length > 0 || Object.keys(gp.axes).length > 0),
      );
      if (gamepad) {
        const gamepadWidth = 48;
        const gamepadHeight = 24;
        const gamepadX =
          qwertyStartX + Math.floor((pianoWidth - gamepadWidth) / 2);
        const gamepadY = qwertyStartY + QWERTY_MINIMAP_HEIGHT + 2;
        const maxY = layout?.topButtonY ? layout.topButtonY - 2 : screen.height;
        if (gamepadY + gamepadHeight <= maxY) {
          drawMiniControllerDiagram(ink, gamepad, gamepadX, gamepadY);
        }
      }
    }
    } // End of !pianoGeometry.hidden check
  }

  // layout already computed above for piano/qwerty positioning
  const midiBadgeMetrics = layout.midiBadge ?? cachedLayout.midiMetrics;
  const melodyButtonRect = layout.melodyButtonRect;

  if (melodyAliasBtn && melodyButtonRect && melodyAliasBtn.box) {
    melodyAliasBtn.box.x = melodyButtonRect.x;
    melodyAliasBtn.box.y = melodyButtonRect.y;
    melodyAliasBtn.box.w = melodyButtonRect.width;
    melodyAliasBtn.box.h = melodyButtonRect.height;
  }

  function drawSampleStream(
    rect,
    waveforms,
    tick,
    {
      color = [80, 200, 255, 220],
      idleColor = [120, 120, 120, 120],
    } = {},
  ) {
    if (!rect || rect.width <= 0 || rect.height <= 0) return;

    const { x, y, width, height } = rect;
    const half = Math.max(1, Math.floor(height / 2));
    const mid = y + half;
    const hasWaveforms = Array.isArray(waveforms) && waveforms.length > 0;

    const idleR = idleColor[0], idleG = idleColor[1], idleB = idleColor[2], idleA = idleColor[3] ?? 255;
    if (!hasWaveforms) {
      for (let i = 0; i < width; i += 2) {
        ink(idleR, idleG, idleB, idleA).box(x + i, mid, 1, 1);
      }
      return;
    }

    const colR = color[0], colG = color[1], colB = color[2], colA = color[3] ?? 255;
    const step = waveforms.length / width;
    const offset = waveforms.length > 0 ? (tick || 0) % waveforms.length : 0;
    const wfLen = waveforms.length;
    const yMax = y + height - 1;
    for (let i = 0; i < width; i += 1) {
      const idx = Math.floor(i * step + offset) % wfLen;
      const sample = waveforms[idx] || 0;
      const clamped = Math.max(-1, Math.min(1, sample));
      const py = Math.max(y, Math.min(yMax, Math.round(mid - clamped * half)));
      ink(colR, colG, colB, colA).box(x + i, py, 1, 1);
    }
  }

  function drawStampleWaveform(
    rect,
    data,
    progress,
    needleColor,
    {
      lineColor = [180, 200, 220, 220],
      background = [0, 0, 0, 120],
    } = {},
  ) {
    if (!rect || rect.width <= 2 || rect.height <= 2 || !data?.length) return;

    const { x, y, width, height } = rect;
    const mid = y + Math.floor(height / 2);
    const bgR = background[0], bgG = background[1], bgB = background[2], bgA = background[3] ?? 255;
    ink(bgR, bgG, bgB, bgA).box(x, y, width, height);

    const lcR = lineColor[0], lcG = lineColor[1], lcB = lineColor[2], lcA = lineColor[3] ?? 255;
    const step = data.length / width;
    const halfHeight = height * 0.45;
    for (let i = 0; i < width; i += 1) {
      const idx = Math.floor(i * step);
      const sample = data[idx] || 0;
      const clamped = Math.max(-1, Math.min(1, sample));
      const py = Math.round(mid - clamped * halfHeight);
      ink(lcR, lcG, lcB, lcA).box(x + i, py, 1, 1);
    }

    if (typeof progress === "number" && progress >= 0) {
      const needleX = Math.round(x + progress * width);
      if (needleX >= x && needleX <= x + width) {
        const ncR = Array.isArray(needleColor) ? needleColor[0] : 255;
        const ncG = Array.isArray(needleColor) ? needleColor[1] : 255;
        const ncB = Array.isArray(needleColor) ? needleColor[2] : 255;
        ink(ncR, ncG, ncB).box(needleX, y, 1, height);
      }
    }
  }

  function drawMidiBadge(
    metrics,
    connected,
    rateLabel,
    rateText,
    maxX = Infinity,
    {
      connectedText = [255, 165, 0],
      disconnectedText = [140, 140, 140, 200],
      rateTextColor = [80, 200, 255, 220],
      dividerColor = [80, 80, 80],
      fpsColor = [120, 255, 120],
    } = {},
  ) {
    if (!metrics) return;

    const { x, y } = metrics;
    const textColor = connected ? connectedText : disconnectedText;
    const tcR = textColor[0], tcG = textColor[1], tcB = textColor[2], tcA = textColor[3] ?? 255;
    const divR = dividerColor[0], divG = dividerColor[1], divB = dividerColor[2];
    const rtR = rateTextColor[0], rtG = rateTextColor[1], rtB = rateTextColor[2], rtA = rateTextColor[3] ?? 255;
    const gw = matrixGlyphMetrics.width;
    const gh = matrixGlyphMetrics.height;
    const baseX = x + MIDI_BADGE_PADDING_X;
    const baseY = y + MIDI_BADGE_PADDING_Y;

    let cursorX = baseX;

    // Draw "M" (shortened from MIDI)
    const midiText = "M";
    if (cursorX + midiText.length * gw > maxX) return;
    ink(tcR, tcG, tcB, tcA).write(
      midiText,
      { x: cursorX, y: baseY },
      undefined,
      undefined,
      false,
      "MatrixChunky8",
    );
    cursorX += midiText.length * gw;

    // Divider
    cursorX += 2;
    if (cursorX > maxX) return;
    ink(divR, divG, divB).line(cursorX, y + 2, cursorX, y + gh + 1);
    cursorX += 3;

    // Draw sample rate (e.g., "48k")
    if (rateText) {
      const shortRate = rateText.replace("Hz", ""); // "48kHz" -> "48k"
      if (cursorX + shortRate.length * gw > maxX) return;
      ink(rtR, rtG, rtB, rtA).write(
        shortRate,
        { x: cursorX, y: baseY },
        undefined,
        undefined,
        false,
        "MatrixChunky8",
      );
      cursorX += shortRate.length * gw;
    }

    // Divider
    cursorX += 2;
    if (cursorX > maxX) return;
    ink(divR, divG, divB).line(cursorX, y + 2, cursorX, y + gh + 1);
    cursorX += 3;

    // Draw FPS with suffix (skip if it would overlap metronome)
    const fpsVal = Math.round(perfStats.fps);
    const fpsText = fpsVal > 0 ? `${fpsVal}fps` : "--";
    if (cursorX + fpsText.length * gw > maxX) return;
    let fpsR, fpsG, fpsB;
    if (fpsVal < 30) { fpsR = 255; fpsG = 80; fpsB = 80; }
    else if (fpsVal < 50) { fpsR = 255; fpsG = 200; fpsB = 80; }
    else { fpsR = fpsColor[0]; fpsG = fpsColor[1]; fpsB = fpsColor[2]; }
    ink(fpsR, fpsG, fpsB).write(
      fpsText,
      { x: cursorX, y: baseY },
      undefined,
      undefined,
      false,
      "MatrixChunky8",
    );
  }


  if (projector) {
    const sy = 33;
    const sh = screen.height - sy;

    // console.log(sound.speaker.amplitudes, sound.speaker.waveforms);
    sound.paint.bars(
      api,
      amplitude,
      waveformsForBars,
      0,
      sy,
      screen.width, // width
      sh, // height
      [255, 0, 0, 255],
      { noamp: true, primaryColor, secondaryColor },
    );
    //ink("yellow").write(scope, 6, sy + sh + 5);
    ink("yellow").write(scope, 50 + 4, 6);
    // ink("pink").write(scopeTrim, 6 + 18, sy + sh + 5);
    const audioBadgeWidth = 80;
    const audioBadgeHeight = 12;
    const audioBadgeY = sy + 4;
    let audioBadgeX;

    if (!audioReady) {
      // "AUDIO ENGINE OFF" using actual MatrixChunky8 glyph metrics - shorten for narrow screens
      const fullText = "AUDIO ENGINE OFF";
      const shortText = "ENGINE OFF";
      const tinyText = "OFF";
      const fullWidth = measureMatrixTextBoxWidth(fullText, api, screen.width) || measureMatrixTextWidth(fullText, typeface);
      const shortWidth = measureMatrixTextBoxWidth(shortText, api, screen.width) || measureMatrixTextWidth(shortText, typeface);
      const audioPadding = 4;
      // Choose text based on available width
      let audioText = fullText;
      let audioTextWidth = fullWidth;
      if (fullWidth + audioPadding * 2 > screen.width - 20) {
        audioText = shortText;
        audioTextWidth = shortWidth;
      }
      if (shortWidth + audioPadding * 2 > screen.width - 20) {
        audioText = tinyText;
        audioTextWidth = measureMatrixTextBoxWidth(tinyText, api, screen.width) || measureMatrixTextWidth(tinyText, typeface);
      }
      const totalBadgeWidth = audioTextWidth + audioPadding * 2;
      audioBadgeX = Math.floor((screen.width - totalBadgeWidth) / 2); // Center horizontally
      ink(180, 0, 0, 240).box(audioBadgeX, audioBadgeY, totalBadgeWidth, audioBadgeHeight);
      ink(255, 255, 0).write(audioText, { x: audioBadgeX + audioPadding, y: audioBadgeY + 3 }, undefined, undefined, false, "MatrixChunky8");
    }

    // MIDI badge now renders in the top mini bar (left)
  } else if (!paintPictureOverlay) {
    const sy = 3;
    const sh = 15; // screen.height - sy;

    // Visualizer starts after the top bar piano (or at 54 if piano not shown)
    const vizStartX = topBarPianoEndX;
    const availableWidth = waveBtn.box.x - vizStartX;

    sound.paint.bars(
      api,
      amplitude,
      waveformsForBars,
      vizStartX, //0,
      sy, //sy,
      availableWidth,
      //screen.width, // width
      sh, // height
      [255, 0, 0, 255],
      { primaryColor, secondaryColor },
    );

    if (wave === "stample" && stampleSampleData?.length) {
      const needleNote = stampleNeedleNote || active?.[active.length - 1];
      const needleColor = needleNote ? colorFromNote(needleNote, num) : [255, 255, 255];
      const stripRight = waveBtn?.box?.x ? waveBtn.box.x - 6 : screen.width - 6;
      const stripWidth = Math.min(72, Math.max(24, stripRight - 56));
      const stripLeft = Math.max(56, stripRight - stripWidth);
      drawStampleWaveform(
        {
          x: stripLeft,
          y: sy + 2,
          width: stripRight - stripLeft,
          height: 10,
        },
        stampleSampleData,
        stampleNeedleProgress,
        needleColor,
      );
    }

    // Active notes now render in the secondary mini bar.

    // ink("yellow").write(scope, 56 + 120 + 2, sy + 3);
    // ink("pink").write(scopeTrim, 6 + 18, sy + sh + 3);
    // ink("cyan").write(sound.sampleRate, 6 + 18 + 20, sy + sh + 3);

    const rightEdge = waveBtn?.box?.x ?? screen.width - 8;
    const audioBadgeWidth = 66;
    const audioBadgeHeight = max(9, sh - 4);
    const audioBadgeY = sy + 2;
    let audioBadgeX;

    if (!audioReady) {
      // "AUDIO ENGINE OFF" using actual MatrixChunky8 glyph metrics - shorten for narrow screens
      const fullText = "AUDIO ENGINE OFF";
      const shortText = "ENGINE OFF";
      const tinyText = "OFF";
      const fullWidth = measureMatrixTextBoxWidth(fullText, api, screen.width) || measureMatrixTextWidth(fullText, typeface);
      const shortWidth = measureMatrixTextBoxWidth(shortText, api, screen.width) || measureMatrixTextWidth(shortText, typeface);
      const audioPadding = 4;
      // Choose text based on available width before waveBtn
      const availableForAudio = rightEdge - topBarPianoEndX - 4;
      let audioText = fullText;
      let audioTextWidth = fullWidth;
      if (fullWidth + audioPadding * 2 > availableForAudio) {
        audioText = shortText;
        audioTextWidth = shortWidth;
      }
      if (shortWidth + audioPadding * 2 > availableForAudio) {
        audioText = tinyText;
        audioTextWidth = measureMatrixTextBoxWidth(tinyText, api, screen.width) || measureMatrixTextWidth(tinyText, typeface);
      }
      const totalBadgeWidth = audioTextWidth + audioPadding * 2;
      audioBadgeX = rightEdge - totalBadgeWidth - 4; // Position on right side before waveBtn
      ink(180, 0, 0, 240).box(audioBadgeX, audioBadgeY, totalBadgeWidth, max(9, audioBadgeHeight));
      ink(255, 255, 0).write(audioText, { x: audioBadgeX + audioPadding, y: audioBadgeY + 2 }, undefined, undefined, false, "MatrixChunky8");
    }

    if (song && melodyAliasBtn && melodyAliasBtn.box && melodyButtonRect) {
      const baseGlyphWidth = matrixGlyphMetrics.width;
      const baseGlyphHeight = matrixGlyphMetrics.height;

      melodyAliasBtn.paint((btn) => {
        const rect = btn.box;
        const isActive = melodyAliasDown || songNoteDown;
        const backgroundColor = isActive ? [0, 120, 140, 220] : [0, 0, 0, 150];
        const borderColor = isActive ? [0, 255, 234, 240] : [0, 200, 220, 180];

        ink(...backgroundColor).box(rect.x, rect.y, rect.w, rect.h);
        ink(...borderColor).box(rect.x, rect.y, rect.w, rect.h, "outline");
        if (btn.over && !btn.down) {
          ink(255, 255, 255, 24).box(rect.x, rect.y, rect.w, rect.h);
        }

        // Note value intentionally omitted per design request

        const sanitizeLyric = (value) =>
          typeof value === "string" ? value.replace(/-/g, "") : "";

        const prevLyric = sanitizeLyric(song?.[songIndex - 1]?.[1]);
        const currentLyric = sanitizeLyric(song?.[songIndex]?.[1]);
        const nextLyric = sanitizeLyric(song?.[songIndex + 1]?.[1]);

        const glyphWidth = matrixGlyphMetrics.width;
        const glyphHeight = matrixGlyphMetrics.height;
        const centerY = rect.y + rect.h / 2;

        if (currentLyric) {
          const lyricWidth = currentLyric.length * glyphWidth;
          const lyricX = rect.x + (rect.w - lyricWidth) / 2;
          const lyricY = centerY - glyphHeight / 2;
          ink("white").write(
            currentLyric,
            { x: lyricX, y: lyricY },
            undefined,
            undefined,
            false,
            "MatrixChunky8",
          );

          if (prevLyric) {
            const prevWidth = prevLyric.length * glyphWidth;
            const prevX = lyricX - prevWidth - 4;
            if (prevX > rect.x) {
              ink(160, 160, 160, 210).write(
                prevLyric,
                { x: prevX, y: lyricY },
                undefined,
                undefined,
                false,
                "MatrixChunky8",
              );
            }
          }

          if (nextLyric) {
            const nextWidth = nextLyric.length * glyphWidth;
            const nextX = lyricX + lyricWidth + 4;
            if (nextX + nextWidth < rect.x + rect.w) {
              ink(160, 160, 160, 210).write(
                nextLyric,
                { x: nextX, y: lyricY },
                undefined,
                undefined,
                false,
                "MatrixChunky8",
              );
            }
          }
        }

        const hint = "tap or hold";
        const hintWidth = hint.length * glyphWidth;
        const hintX = rect.x + (rect.w - hintWidth) / 2;
        const hintY = rect.y + rect.h - glyphHeight - 4;
        ink(200, 200, 200, 200).write(
          hint,
          { x: hintX, y: hintY },
          undefined,
          undefined,
          false,
          "MatrixChunky8",
        );
      });
    }

    // MIDI badge now renders in the top mini bar (left)
  }

  if (paintPerfEnabled && paintTick % paintPerfEvery === 0) {
    const elapsed = performance.now() - paintStart;
    console.log("🎨 notepat paint", {
      ms: Number(elapsed.toFixed(2)),
      frame: paintTick,
      miniInputsEnabled: layout.miniInputsEnabled,
      activeNotes: active.length,
      songMode: Boolean(song),
      compact: layout?.compactMode,
    });
  }

  updateTheme({ num });

  if (tap) {
    ink("yellow");
    write("tap", { right: 6, top: 6 });
  } else if (!paintPictureOverlay) {
    waveBtn?.paint((btn) => {
      ink(btn.down ? [40, 40, 100] : "darkblue").box(
        btn.box.x,
        btn.box.y + 3,
        btn.box.w,
        btn.box.h - 3,
      );
      if (btn.over && !btn.down) {
        ink(255, 255, 255, 24).box(
          btn.box.x,
          btn.box.y + 3,
          btn.box.w,
          btn.box.h - 3,
        );
      }
      ink("orange").line(
        btn.box.x + btn.box.w,
        btn.box.y + 3,
        btn.box.x + btn.box.w,
        btn.box.y + btn.box.h - 1,
      );
      ink(btn.down ? "yellow" : "orange");
      // Use shortened wave name and smaller font on narrow screens
      const displayWave = btn.displayWave || wave;
      if (btn.isNarrow) {
        ink(btn.down ? "yellow" : "orange").write(
          displayWave,
          { x: btn.box.x + 3, y: btn.box.y + 5 },
          undefined, undefined, false, "MatrixChunky8"
        );
      } else {
        write(wave, { right: 27, top: 6 });
      }
    });

    octBtn?.paint((btn) => {
      if (btn.down) {
        ink(40, 40, 100);
      } else {
        ink(octaveTheme[octave], 196);
      }
      box(btn.box.x, btn.box.y + 3, btn.box.w - 4, btn.box.h - 3);
      if (btn.over && !btn.down) {
        ink(255, 255, 255, 24).box(
          btn.box.x,
          btn.box.y + 3,
          btn.box.w - 4,
          btn.box.h - 3,
        );
      }
      ink(btn.down ? "yellow" : "pink");
      // Use smaller font on narrow screens
      if (btn.isNarrow) {
        ink(btn.down ? "yellow" : "pink").write(
          octave,
          { x: btn.box.x + 3, y: btn.box.y + 5 },
          undefined, undefined, false, "MatrixChunky8"
        );
      } else {
        write(octave, { right: 8, top: 6 });
      }
    });
  }

  if (tap) {
    // Only render a visible window of keys around the current tapIndex
    // to prevent performance degradation with long key sequences
    const glyphWidth = 6; // Approximate character width
    const maxVisibleChars = Math.ceil(screen.width / glyphWidth) + 10;
    const halfVisible = Math.floor(maxVisibleChars / 2);
    const startIdx = Math.max(0, tapIndex - halfVisible);
    const endIdx = Math.min(keys.length, tapIndex + halfVisible);
    const visibleKeys = keys.slice(startIdx, endIdx);
    const offsetFromStart = tapIndex - startIdx;
    ink("gray").write(visibleKeys, screen.width / 2 - offsetFromStart * glyphWidth, screen.height / 2);
    
    let nextToken = keys[tapIndex];
    let tempTapIndex = tapIndex;
    if (octaves.includes(nextToken)) {
      tempTapIndex += 1;
      nextToken += keys[tempTapIndex];
    }
    if (accents.includes(keys[tempTapIndex + 1])) {
      nextToken += keys[tempTapIndex + 1];
    }
    ink("red").write(nextToken, screen.width / 2, screen.height / 2);
  }

  if (tap && !paintPictureOverlay)
    ink("orange").line(screen.width / 2, 0, screen.width / 2, screen.height);

  if (!paintPictureOverlay) {
    if (tap) {
      ink("white");
      active.forEach((sound, index) => {
        write(sound, screen.width / 2, screen.height / 2 + 12 + 12 * index);
      });
      if (tapped) {
        ink("lime");
        write(tapped, screen.width / 2, screen.height / 2);
      }
    }
  }

  if (!tap /*&& !projector*/ && !paintPictureOverlay) {
    // 🎵 Draw connecting line FIRST (behind everything)
    if (song && songIndex < song.length - 1) {
      const currentNoteKey = song[songIndex][0];
      const nextNoteKey = song[songIndex + 1][0];
      
      if (currentNoteKey && nextNoteKey) {
        // Find the button for current and next notes
        const currentButton = buttons[currentNoteKey.toLowerCase()] || buttons["+" + currentNoteKey.toLowerCase()];
        const nextButton = buttons[nextNoteKey.toLowerCase()] || buttons["+" + nextNoteKey.toLowerCase()];
        
        if (currentButton && nextButton) {
          // Get center points of both buttons
          const x1 = currentButton.box.x + currentButton.box.w / 2;
          const y1 = currentButton.box.y + currentButton.box.h / 2;
          const x2 = nextButton.box.x + nextButton.box.w / 2;
          const y2 = nextButton.box.y + nextButton.box.h / 2;
          
          // Draw brighter line with fade based on progress
          const lineOpacity = Math.floor(songProgress * 255);
          if (lineOpacity > 0) {
            ink(0, 255, 255, lineOpacity).line(x1, y1, x2, y2);
          }
        }
      }
    }
    
    const activeNote = buttonNotes.indexOf(active[0]);

    if (activeNote >= 0 && activeNote !== lastActiveNote) {
      lastActiveNote = activeNote;
      transposeOverlayFade = 0;
    }

    // 🎵 Draw grid outlines for melody notes - cache melody notes set
    if (song) {
      // Build melody notes set once per song change (could be cached at song load time)
      const melodyNotes = new Set();
      const songLen = song.length;
      for (let i = 0; i < songLen; i++) {
        melodyNotes.add(song[i][0].toLowerCase());
      }
      
      // Draw outline for each melody note button - use for loop
      const btnNotesLen = buttonNotes.length;
      for (let i = 0; i < btnNotesLen; i++) {
        const note = buttonNotes[i];
        const btn = buttons[note];
        if (btn && melodyNotes.has(note.toUpperCase())) {
          ink(0, 255, 255, 64).box(btn.box, "outline");
        }
      }
    }

    if (usePadsBase) {
      if (padsBaseKey !== padsBaseCacheKey || !padsBase) {
        padsBase = buildPadsBase({ api, screen, layout, matrixGlyphMetrics, num });
        padsBaseKey = padsBaseCacheKey;
      }
      if (padsBase?.painting) {
        paste(padsBase.painting, 0, padsBase.gridTop, { width: screen.width });
      }
    }

    // Paint all the keyboard buttons - use for loop for performance
    const songCurrentNote = song?.[songIndex]?.[0];
    const btnNotesLength = buttonNotes.length;
    const parsedOctave = parseInt(octave);

    for (let index = 0; index < btnNotesLength; index++) {
      const note = buttonNotes[index];
      const btn = buttons[note];
      if (!btn) continue;

      btn.paint((btn) => {
          let color;
          let isBlocked = false;

          // In song mode, check if this note is blocked (not the current note)
          if (song && note.toUpperCase() !== songCurrentNote) {
            isBlocked = true;
            color = "black"; // Blocked notes are black
          } else {
            const outOctave = parsedOctave + (note.startsWith("+") ? upperOctaveShift : lowerOctaveShift);
            const baseColor = getCachedColor(note, num, outOctave);
            const isSemitone = note.includes("#");
            // Semitones get darker and shifted toward black for clear visual distinction
            const tinted = isSemitone ? darkenColor(baseColor, 0.45) : baseColor;
            // When held: extreme dayglow neon flash effect - cycle through RGB
            if ((!slide && btn.down) || (btn.down && slide)) {
              const flashPhase = (paintCount * 0.15) % 3;
              const flashR = flashPhase < 1 ? 255 : (flashPhase < 2 ? 80 : 120);
              const flashG = flashPhase < 1 ? 80 : (flashPhase < 2 ? 255 : 80);
              const flashB = flashPhase < 1 ? 120 : (flashPhase < 2 ? 80 : 255);
              // Blend base color with flash color for neon effect
              color = [
                Math.min(255, Math.floor(tinted[0] * 0.4 + flashR * 0.6)),
                Math.min(255, Math.floor(tinted[1] * 0.4 + flashG * 0.6)),
                Math.min(255, Math.floor(tinted[2] * 0.4 + flashB * 0.6)),
              ];
            } else {
              color = tinted;
            }
          }

          const shouldRedrawFull = !usePadsBase || btn.down || isBlocked || recitalMode;

          // 🎭 Recital mode: Wireframe pads
          if (recitalMode) {
            const isActivePlaying = sounds[note] !== undefined || btn.down;
            if (isActivePlaying) {
              ink(recitalColor[0], recitalColor[1], recitalColor[2], 200).box(btn.box);
            }
            ink(recitalColor[0], recitalColor[1], recitalColor[2], isActivePlaying ? 180 : 50).box(btn.box, "outline");
            
            // Ghost trails in recital mode
            const trailVal = trail[note];
            if (trailVal > 0) {
              ink(recitalColor[0], recitalColor[1], recitalColor[2], max(1, trailVal * 150)).box(
                btn.box.x + btn.box.w / 2,
                btn.box.y + btn.box.h / 2,
                trailVal * btn.box.w,
                trailVal * btn.box.h,
                "center",
              );
            }
            
            // Note label in recital mode
            const meta = btn.meta || {};
            // Natural notes uppercase, black keys (sharps/flats) lowercase
            const noteLabelText = meta.noteLabelText || (isBlackKey(note) ? note.toLowerCase() : note.toUpperCase());
            const noteFont = meta.noteFont || "MatrixChunky8";
            const glyphWidth = meta.noteGlyphWidth || matrixGlyphMetrics.width;
            const glyphHeight = meta.noteGlyphHeight || matrixGlyphMetrics.height;
            const labelWidth = noteLabelText.length * glyphWidth;
            const labelX = btn.box.x + 2;
            const labelY = btn.box.y + 1;
            
            ink(...recitalColor, isActivePlaying ? 255 : 100).write(
              noteLabelText,
              { x: labelX, y: labelY },
              undefined,
              undefined,
              false,
              noteFont,
            );
            return; // Skip normal rendering in recital mode
          }

          // Normal mode rendering
          if (shouldRedrawFull) {
            if (!projector) {
              ink(color, isBlocked ? 128 : 196).box(btn.box); // Blocked notes are darker
            } else {
              ink(color, 48).box(btn.box); // One solid colored box per note.
              // ink("white", 32).box(btn.box, "inline"); // One solid colored box per note.
            }
          }
          if (btn.over && !btn.down && !isBlocked) {
            ink(255, 255, 255, 24).box(btn.box);
            ink(255, 255, 255, 48).box(btn.box, "outline");
          }
          // const accent = colorFromNote(note, num);
          // ink(accent).box(btn.box.x + btn.box.w - 8, btn.box.y + 4, 4);
          // ink("black", 64).box(
          //   btn.box.x + btn.box.w - 8,
          //   btn.box.y + 4,
          //   4,
          //   "outline",
          // );

          // Ghost trails 👻
          if (trail[note] > 0) {
            ink("maroon", max(1, trail[note] * 180)).box(
              btn.box.x + btn.box.w / 2,
              btn.box.y + btn.box.h / 2,
              trail[note] * btn.box.w,
              trail[note] * btn.box.h,
              "center",
            );
          }

          // 🎵 Note label
          const meta = btn.meta || {};
          const noteFont = meta.noteFont || (btn.box.w >= 22 ? "unifont" : "MatrixChunky8");
          const glyphWidth = meta.noteGlyphWidth || (noteFont === "unifont" ? 8 : matrixGlyphMetrics.width);
          const glyphHeight = meta.noteGlyphHeight || (noteFont === "unifont" ? 16 : matrixGlyphMetrics.height);
          const noteName = note.replace(/^[+-]+/, "").toLowerCase();
          const forceWhiteText = ["d", "e", "f"].includes(noteName[0]);
          const isSemitoneKey = note.includes("#");
          // Black keys get slightly grayer text for subtler look
          const baseTextColor = isSemitoneKey ? [140, 140, 140] : (forceWhiteText ? [255, 255, 255] : getContrastingTextColor(color));

          let noteLabelText = null;
          let noteLabelBounds = null;

          if (song) {
            const isCurrentNote = note.toUpperCase() === song?.[songIndex][0];
            const isNextNote = note.toUpperCase() === song?.[songIndex + 1]?.[0];
            if (isCurrentNote || isNextNote) {
              // Natural notes uppercase, black keys (sharps/flats) lowercase
              noteLabelText = meta.noteLabelText || (isBlackKey(note) ? note.toLowerCase() : note.toUpperCase());
              noteLabelBounds = meta.noteLabelBoundsCentered || {
                x: btn.box.x + btn.box.w / 2 - (noteLabelText.length * glyphWidth) / 2,
                y: btn.box.y + btn.box.h / 2 - glyphHeight / 2,
                w: noteLabelText.length * glyphWidth,
                h: glyphHeight,
              };
            }
          } else {
            // Natural notes uppercase, black keys (sharps/flats) lowercase
            noteLabelText = meta.noteLabelText || (isBlackKey(note) ? note.toLowerCase() : note.toUpperCase());
            noteLabelBounds = meta.noteLabelBoundsDefault || {
              x: btn.box.x + 2,
              y: btn.box.y + 1,
              w: noteLabelText.length * glyphWidth,
              h: glyphHeight,
            };
          }
          const keyLabel = meta.keyLabel || null;
          const keyFont = meta.keyFont || (btn.box.w >= 22 ? "unifont" : "MatrixChunky8");
          const keyGlyphWidth = meta.keyGlyphWidth || (keyFont === "unifont" ? 8 : matrixGlyphMetrics.width);
          const keyGlyphHeight = meta.keyGlyphHeight || (keyFont === "unifont" ? 16 : matrixGlyphMetrics.height);
          let keyBounds = null;

          if (keyLabel) {
            if (song) {
              keyBounds = meta.keyBoundsCentered;
              if (meta.keyOverlapsCentered) noteLabelText = null;
            } else {
              keyBounds = meta.keyBoundsDefault;
              if (meta.keyOverlapsDefault) noteLabelText = null;
            }
          }

          // Always draw labels (padsBase cache only has boxes, not text)
          // Apply shake offset when playing - only for the specific note being pressed
          const padNoteShake = noteShake[note] || 0;
          const padShakeX = padNoteShake > 0 ? Math.floor((Math.random() - 0.5) * padNoteShake * 2) : 0;
          const padShakeY = padNoteShake > 0 ? Math.floor((Math.random() - 0.5) * padNoteShake * 2) : 0;
          
          if (noteLabelText && noteLabelBounds) {
            ink(0, 0, 0, 160).write(
              noteLabelText,
              { x: noteLabelBounds.x + 1 + padShakeX, y: noteLabelBounds.y + 1 + padShakeY },
              undefined,
              undefined,
              false,
              noteFont,
            );
            ink(...baseTextColor).write(
              noteLabelText,
              { x: noteLabelBounds.x + padShakeX, y: noteLabelBounds.y + padShakeY },
              undefined,
              undefined,
              false,
              noteFont,
            );
          }

          // � Repeat boxes for current note
          if (song && note.toUpperCase() === song?.[songIndex][0]) {
            let repeatCount = 0;
            let currentNote = song[songIndex][0];
            // Count consecutive repeats of the same note
            for (let i = songIndex; i < song.length; i++) {
              if (song[i][0] === currentNote) {
                repeatCount++;
              } else {
                break;
              }
            }
            // Show solid boxes - one for each tap, stacked inside
            for (let i = 0; i < repeatCount; i++) {
              const inset = 6 + i * 3; // Start at 6px margin, each box gets progressively smaller
              ink(0, 100, 120, 220).box( // Darker cyan for better contrast with white text
                btn.box.x + inset,
                btn.box.y + inset,
                btn.box.w - inset * 2,
                btn.box.h - inset * 2
              );
            }
          }
          // Fade in box for next note
          else if (song && note.toUpperCase() === song?.[songIndex + 1]?.[0]) {
            // Only show 1 box for the immediate next tap (not all repeats)
            const inset = 6;
            const fadeOpacity = Math.floor(songProgress * 100); // Reduced from 200 to 100 for more fade
            if (fadeOpacity > 0) {
              ink(0, 100, 120, fadeOpacity).box( // Darker cyan for better contrast
                btn.box.x + inset,
                btn.box.y + inset,
                btn.box.w - inset * 2,
                btn.box.h - inset * 2
              );
            }
          }

          // 🧮 Transpose label
          if (paintTransposeOverlay && lastActiveNote >= 0) {
            let dist = index - lastActiveNote;
            if (dist !== 0) {
              // dist = dist > 0 ? "+" + dist : dist.toString();
              ink(dist > 0 ? "lime" : "red", transposeOverlayFade * 255).write(
                abs(dist),
                // btn.box.x +
                //   btn.box.w / 2 -
                //   ((dist.length + 1) * glyphWidth) / 2,
                btn.box.x + 2 + note.length * glyphWidth,
                btn.box.y + 1,
                //btn.box.y + btn.box.h / 2 - glyphHeight / 2,
              );
            }
          }

          // Paint keyboard shortcuts (always - cache only has boxes)
          if (keyLabel && keyBounds) {
            ink(0, 0, 0, 120).write(
              keyLabel,
              { x: keyBounds.x + 1, y: keyBounds.y + 1 },
              undefined,
              undefined,
              false,
              keyFont,
            );
            ink("white", 180).write(
              keyLabel,
              { x: keyBounds.x, y: keyBounds.y },
              undefined,
              undefined,
              false,
              keyFont,
            );
          }
        });
    }

    if (!projector && !usePadsBase) {
      const gridAlpha = 40;
      const topEdge = round(layout.topButtonY);
      const bottomEdge = round(layout.topButtonY + layout.buttonHeight * layout.totalRows);

      if (!layout.splitLayout) {
        const gridWidth = layout.buttonsPerRow * layout.buttonWidth;
        const availableWidth = layout.usableWidth ?? screen.width;
        // Left-align grid when mini inputs are to the right, otherwise center
        const baseX = layout.miniInputsHorizontal
          ? layout.margin
          : ceil(
              layout.margin +
                Math.max(0, floor((availableWidth - layout.margin * 2 - gridWidth) / 2)),
            );
        const leftEdge = baseX;
        const rightEdge = round(baseX + gridWidth);

        for (let col = 0; col <= layout.buttonsPerRow; col += 1) {
          const x = round(baseX + col * layout.buttonWidth);
          ink(255, 255, 255, gridAlpha).line(x, topEdge, x, bottomEdge);
        }

        for (let row = 0; row <= layout.totalRows; row += 1) {
          const y = round(layout.topButtonY + row * layout.buttonHeight);
          ink(255, 255, 255, gridAlpha).line(leftEdge, y, rightEdge, y);
        }
      }
    }
  }
  
  // 📊 Performance OSD (On-Screen Display) - toggle with ` key
  // Uses MatrixChunky8 font and positions intelligently to avoid pads
  if (perfOSD && !paintPictureOverlay && !projector) {
    const font = "MatrixChunky8";
    const glyphW = 6;
    const glyphH = 8;
    const lineHeight = glyphH + 2;
    const osdLines = 9;
    const osdWidth = 24 * glyphW + 8; // ~152px
    const osdHeight = osdLines * lineHeight + 4;
    
    // Position OSD above the button pads, on the right side
    // Use layout metrics to find a safe spot
    const padTop = layout?.topButtonY || (screen.height - 120);
    const osdX = screen.width - osdWidth - 4;
    const osdY = Math.max(SECONDARY_BAR_BOTTOM + 2, padTop - osdHeight - 4);
    
    // Semi-transparent background
    ink(0, 0, 0, 210).box(osdX - 2, osdY - 2, osdWidth, osdHeight);
    
    // Border
    ink(80, 80, 80).box(osdX - 2, osdY - 2, osdWidth, osdHeight, "outline");
    
    let row = 0;
    const writeRow = (color, text) => {
      ink(color).write(text, { x: osdX, y: osdY + row * lineHeight }, undefined, undefined, false, font);
      row++;
    };
    
    // Title
    writeRow("orange", "PERF OSD [`]");
    
    // Audio sample rate
    const srStr = perfStats.sampleRate > 0 ? `${perfStats.sampleRate.toFixed(1)}kHz` : "--";
    writeRow("cyan", `RATE: ${srStr}`);
    
    // Audio context latencies (hardware)
    const baseLatStr = perfStats.baseLatency > 0 ? `${perfStats.baseLatency.toFixed(1)}` : "--";
    const outLatStr = perfStats.outputLatency > 0 ? `${perfStats.outputLatency.toFixed(1)}` : "--";
    writeRow([180, 180, 180], `HW LAT: ${baseLatStr}/${outLatStr}ms`);
    
    // JS-side latency (key press to synth call)
    const latStr = perfStats.latency > 0 ? `${perfStats.latency.toFixed(1)}ms` : "--";
    const avgStr = perfStats.avgLatency > 0 ? `${perfStats.avgLatency.toFixed(1)}ms` : "--";
    writeRow("lime", `JS LAT: ${latStr} AVG:${avgStr}`);
    
    // Min/Max latency
    const minStr = perfStats.minLatency < Infinity ? `${perfStats.minLatency.toFixed(1)}` : "--";
    const maxStr = perfStats.maxLatency > 0 ? `${perfStats.maxLatency.toFixed(1)}` : "--";
    writeRow([120, 120, 120], `MIN/MAX: ${minStr}/${maxStr}ms`);
    
    // Sound counts
    writeRow("yellow", `SND:${perfStats.soundCount} STK:${perfStats.tonesInStack} KEY:${perfStats.keysLength}`);
    
    // FPS and memory
    const fpsColor = perfStats.fps >= 55 ? "lime" : perfStats.fps >= 30 ? "yellow" : "red";
    const memStr = perfStats.memoryUsed > 0 ? ` MEM:${perfStats.memoryUsed}MB` : "";
    writeRow(fpsColor, `FPS:${perfStats.fps}${memStr}`);
    
    // MIDI status
    writeRow(midiConnected ? "lime" : [100, 100, 100], midiConnected ? "MIDI: CONNECTED" : "MIDI: --");
    
    // Estimated total latency
    const totalEst = perfStats.avgLatency + perfStats.baseLatency + perfStats.outputLatency;
    const totalStr = totalEst > 0 ? `~${totalEst.toFixed(1)}ms` : "--";
    writeRow("white", `TOTAL EST: ${totalStr}`);
  }
}

let anyDown = true;

const percDowns = {};
const connectedGamepads = {};
let miniMapActiveNote = null;
let miniMapActiveKey = null;
let topBarPianoActiveNote = null; // Track active note from top bar piano
let soundContext = null;

function setSoundContext(ctx) {
  soundContext = ctx;
}

function makeNoteSound(tone, velocity = 127, pan = 0) {
  const synth = soundContext?.synth;
  const play = soundContext?.play;
  const freq = soundContext?.freq;
  const num = soundContext?.num;

  if (!synth || !play || !freq) return null;

  // 📊 Track sound creation time for latency measurement
  perfStats.lastSoundTime = performance.now();
  if (perfStats.lastKeyTime > 0) {
    const latency = perfStats.lastSoundTime - perfStats.lastKeyTime;
    perfStats.latency = latency;
    perfStats.latencyHistory.push(latency);
    if (perfStats.latencyHistory.length > 30) perfStats.latencyHistory.shift();
    perfStats.avgLatency = perfStats.latencyHistory.reduce((a, b) => a + b, 0) / perfStats.latencyHistory.length;
    perfStats.maxLatency = Math.max(perfStats.maxLatency, latency);
    perfStats.minLatency = Math.min(perfStats.minLatency, latency);
  }
  
  const velocityRatioRaw = velocity === undefined ? 1 : velocity / 127;
  const velocityRatio = num?.clamp
    ? num.clamp(velocityRatioRaw, 0, 1)
    : Math.max(0, Math.min(1, velocityRatioRaw));
  const minVelocityVolume = 0.05; // Keep a subtle floor so very light taps still play.
  const volumeScale = minVelocityVolume + (1 - minVelocityVolume) * velocityRatio;

  if (wave === "stample" || wave === "sample") {
    const sampleId = stampleSampleId || startupSfx;
    return play(sampleId, {
      volume: volumeScale,
      pitch: freq(tone),
      pan,
      loop: true,
    });
  } else if (wave === "composite") {
    let toneA, toneB, toneC, toneD, toneE;
    const baseFreq = freq(tone);

    toneA = synth({
      type: "sine",
      attack: 0.0025,
      decay: 0.9,
      tone: baseFreq,
      duration: "🔁",
      volume: toneVolume * volumeScale,
      pan,
    });

    toneB = synth({
      type: "sine",
      attack: 0.0025,
      tone: baseFreq + 9 + num.randIntRange(-1, 1),
      duration: "🔁",
      volume: (toneVolume / 3) * volumeScale,
      pan,
    });

    toneC = synth({
      type: "sawtooth",
      attack,
      decay: 0.9,
      tone: baseFreq + num.randIntRange(-6, 6),
      duration: 0.15 + num.rand() * 0.05,
      volume: (toneVolume / 48) * volumeScale,
      pan,
    });

    toneD = synth({
      type: "triangle",
      attack: 0.999,
      tone: baseFreq + 8 + num.randIntRange(-5, 5),
      duration: "🔁",
      volume: (toneVolume / 32) * volumeScale,
      pan,
    });

    toneE = synth({
      type: "square",
      attack: 0.05,
      tone: baseFreq + num.randIntRange(-10, 10),
      duration: "🔁",
      volume: (toneVolume / 64) * volumeScale,
      pan,
    });

    return {
      startedAt: toneA?.startedAt || toneB.startedAt,
      kill: (fade) => {
        toneA?.kill(fade);
        toneB?.kill(fade);
        toneC?.kill(fade * 2); // TODO: Does not kill 1 shot sounds. 24.11.19.20.56
        toneD?.kill(fade * 1.4);
        toneE?.kill(fade / 2);
      },
    };
  } else {
    // Map 'noise' shorthand to 'noise-white' for the synth
    const synthType = wave === "noise" ? "noise-white" : wave;
    return synth({
      type: synthType,
      attack: quickFade ? 0.0015 : attack,
      tone,
      duration: "🔁",
      volume: toneVolume * volumeScale,
      pan,
    });
  }
}

function computePitchBendRatio() {
  const semitoneOffset = midiPitchBendValue * MIDI_PITCH_BEND_RANGE;
  return 2 ** (semitoneOffset / 12);
}

function applyPitchBendToNotes(noteKeys, { immediate = false } = {}) {
  const freq = soundContext?.freq;
  if (!freq) return;

  const ratio = computePitchBendRatio();
  const targets = Array.isArray(noteKeys) && noteKeys.length > 0
    ? noteKeys
    : Object.keys(sounds);

  targets.forEach((noteKey) => {
    const soundEntry = sounds[noteKey];
    const toneInfo = tonestack[noteKey];
    if (!soundEntry?.sound?.update || !toneInfo?.tone) return;

    const baseFrequency = freq(toneInfo.tone);
    if (typeof baseFrequency !== "number" || Number.isNaN(baseFrequency)) return;

    // For sample-based waves, use sampleSpeed instead of tone frequency
    if (wave === "stample" || wave === "sample") {
      // Calculate the target speed based on the base speed and pitch bend ratio
      // The base speed is calculated from the original pitch / 440 (A4)
      const baseSpeed = baseFrequency / 440;
      const targetSpeed = baseSpeed * ratio;
      const payload = { sampleSpeed: targetSpeed };
      try {
        soundEntry.sound.update(payload);
      } catch (err) {
        console.warn("🎛️ Sample pitch bend update failed", { noteKey, err });
      }
    } else {
      const bentFrequency = baseFrequency * ratio;
      const payload = immediate ? { tone: bentFrequency } : { tone: bentFrequency, duration: 0.05 };
      try {
        soundEntry.sound.update(payload);
      } catch (err) {
        console.warn("🎛️ Pitch bend update failed", { noteKey, err });
      }
    }
  });
}

function startButtonNote(note, velocity = 127, apiRef = null) {
  anyDown = true;
  perfStats.lastKeyTime = performance.now();
  noteShake[note] = 3; // Trigger per-note typography shake

  if (song && note.toUpperCase() !== song?.[songIndex][0]) {
    synth({
      type: "noise-white",
      tone: 1000,
      duration: 0.05,
      volume: 0.3,
      attack: 0,
    });
    return false;
  }

  if (downs[note]) return false;

  let noteUpper = note.toUpperCase();
  keys += noteUpper;
  const active = orderedByCount(sounds);

  let tempOctave = octave;
  if (note[0] === "+") {
    noteUpper = noteUpper.replace("+", "");
    tempOctave = parseInt(octave) + 1 + upperOctaveShift;
  } else {
    tempOctave = parseInt(octave) + lowerOctaveShift;
  }

  const tone = `${tempOctave}${noteUpper}`;

  if (slide && active.length > 0) {
    // For sample-based waves, use sampleSpeed; for synths, use tone frequency
    const freq = soundContext?.freq;
    if (wave === "stample" || wave === "sample") {
      // Calculate sample speed from the target pitch
      const targetFreq = freq ? freq(tone) : 440;
      const targetSpeed = targetFreq / 440; // Speed ratio relative to A4
      sounds[active[0]]?.sound?.update({ sampleSpeed: targetSpeed });
    } else {
      sounds[active[0]]?.sound?.update({ tone, duration: 0.1 });
    }
    tonestack[note] = {
      count: Object.keys(tonestack).length,
      tone,
    };
    sounds[note] = sounds[active[0]];
    if (sounds[note]) sounds[note].note = note;
    delete sounds[active[0]];
    applyPitchBendToNotes([note], { immediate: true });
  } else {
    tonestack[note] = {
      count: Object.keys(tonestack).length,
      tone,
    };

    const pan = getPanForButtonNote(note);
    sounds[note] = {
      note,
      count: active.length + 1,
      sound: makeNoteSound(tone, velocity, pan),
    };

    applyPitchBendToNotes([note], { immediate: true });

    if (note.toUpperCase() === song?.[songIndex][0]) {
      songNoteDown = true;
    }

    delete trail[note];

    if (apiRef) {
      pictureAdd(apiRef, tone);
    }
    udpServer?.send("tv", { note });
  }

  return true;
}

function stopButtonNote(note, { force = false } = {}) {
  if (downs[note]) return false;

  const orderedTones = orderedByCount(tonestack);

  if (slide && orderedTones.length > 1 && sounds[note]) {
    const previousKey = orderedTones[orderedTones.length - 2];
    const previousTone = tonestack[previousKey]?.tone;
    
    // For sample-based waves, use sampleSpeed; for synths, use tone frequency
    const freq = soundContext?.freq;
    if (wave === "stample" || wave === "sample") {
      const targetFreq = freq ? freq(previousTone) : 440;
      const targetSpeed = targetFreq / 440;
      sounds[note]?.sound?.update({ sampleSpeed: targetSpeed });
    } else {
      sounds[note]?.sound?.update({
        tone: previousTone,
        duration: 0.1,
      });
    }
    sounds[previousKey] = sounds[note];
    if (sounds[previousKey]) sounds[previousKey].note = previousKey;
    applyPitchBendToNotes([previousKey], { immediate: true });
  } else {
    sounds[note]?.sound.kill(force ? fastFade : quickFade ? fastFade : killFade);
  }

  trail[note] = 1;

  if (note.toUpperCase() === song?.[songIndex][0]) {
    songIndex = (songIndex + 1) % song.length;
    songNoteDown = false;
    songShifting = true;
  }

  delete tonestack[note];
  delete sounds[note];
  // Note: trail[note] is intentionally NOT deleted here so the trail animation can play
  return true;
}

function triggerPercKey(key, velocity = 1, isDown = true) {
  if (key === "space" && isDown && !tap) {
    perc = pc;
    percDowns.space = true;
    makeSnare(velocity);
    return true;
  }
  if (key === "space" && !isDown) {
    delete percDowns.space;
    return true;
  }
  if (key === "alt" && isDown) {
    perc = pc;
    percDowns.alt = true;
    makeCrash(velocity);
    return true;
  }
  if (key === "alt" && !isDown) {
    delete percDowns.alt;
    return true;
  }
  if (key === "left") {
    if (isDown && !percDowns.left) {
      perc = "brown";
      percDowns.left = true;
      makeTomLow(velocity);
    } else if (!isDown) {
      delete percDowns.left;
    }
    return true;
  }
  if (key === "down") {
    if (isDown && !percDowns.down) {
      perc = "pink";
      percDowns.down = true;
      makeKick(velocity);
    } else if (!isDown) {
      delete percDowns.down;
    }
    return true;
  }
  if (key === "right") {
    if (isDown && !percDowns.right) {
      perc = "orange";
      percDowns.right = true;
      makeTomHigh(velocity);
    } else if (!isDown) {
      delete percDowns.right;
    }
    return true;
  }
  if (key === "up") {
    if (isDown && !percDowns.up) {
      perc = "cyan";
      percDowns.up = true;
      makeHihat(velocity);
    } else if (!isDown) {
      delete percDowns.up;
    }
    return true;
  }
  return false;
}

function act({
  event: e,
  sound: { synth, speaker, play, freq, midi: midiUtil, room, glitch },
  num,
  pens,
  hud,
  screen,
  painting,
  api,
}) {
  setSoundContext({ synth, play, freq, num });
  if (pendingAudioReinit && !audioReinitRequested && api?.send) {
    if (e.is("touch") || e.is("keyboard:down")) {
      api.send({
        type: "audio:reinit",
        content: {
          latencyHint: 0.005,
          sampleRate: 48000,
          speakerPerformanceMode: "disabled",
        },
      });
      audioReinitRequested = true;
      pendingAudioReinit = false;
      // Return early - don't process this gesture as a note, let the reinit complete first
      return;
    }
  }
  if (e.is("reframed")) {
    setupButtons(api);
    buildWaveButton(api);
    buildOctButton(api);
    buildToggleButtons(api);
    buildMetronomeButtons(api);
    // Resize picture to quarter resolution (half width, half height)
    const resizedPictureWidth = Math.max(1, Math.floor(screen.width / 2));
    const resizedPictureHeight = Math.max(1, Math.floor(screen.height / 2));
    picture = painting(resizedPictureWidth, resizedPictureHeight, ({ wipe }) => {
      wipe("gray");
    });
  }

  if (
    autopatConfig.enabled &&
    autopatConfig.ignoreInput &&
    (e.is("touch") ||
      e.is("lift") ||
      e.is("keyboard:down") ||
      e.is("keyboard:up") ||
      e.is("gamepad"))
  ) {
    return;
  }

  if (e.is("gamepad")) {
    const gpIndex = e.gamepad ?? 0;
    if (!connectedGamepads[gpIndex]) {
      connectedGamepads[gpIndex] = {
        id: e.gamepadId || null,
        pressedButtons: [],
        axes: {},
        lastEvent: null,
      };
    }

    const gp = connectedGamepads[gpIndex];

    if (e.gamepadId && !gp.id) {
      gp.id = e.gamepadId;
    }

    gp.lastEvent = e.name;

    if (e.button !== undefined) {
      const buttonIndex = e.button;
      if (e.action === "push") {
        if (!gp.pressedButtons.includes(buttonIndex)) {
          gp.pressedButtons.push(buttonIndex);
        }
      } else if (e.action === "release") {
        gp.pressedButtons = gp.pressedButtons.filter((b) => b !== buttonIndex);
      }
    }

    if (e.axis !== undefined) {
      const axisIndex = e.axis;
      const value = e.value;
      if (Math.abs(value) > 0.1) {
        gp.axes[axisIndex] = value.toFixed(2);
      } else {
        delete gp.axes[axisIndex];
      }
    }
  }

  // 🎭 In recital mode, tap on top bar (BACK button) to exit recital mode
  if (recitalMode && e.is("touch") && e.y < TOP_BAR_BOTTOM) {
    recitalMode = false;
    return;
  }

  // � Handle top bar piano touches (before recital mode toggle check)
  if (e.is("touch") && e.y < TOP_BAR_BOTTOM && !projector && !paintPictureOverlay && !recitalMode) {
    const topBarPianoNote = getTopBarPianoNoteAt(e.x, e.y, screen);
    if (topBarPianoNote) {
      // Stop any previous top bar piano note
      if (topBarPianoActiveNote && topBarPianoActiveNote !== topBarPianoNote) {
        stopButtonNote(topBarPianoActiveNote, { force: true });
      }
      // Start the new note if different
      if (topBarPianoActiveNote !== topBarPianoNote) {
        startButtonNote(topBarPianoNote, 127, api);
      }
      topBarPianoActiveNote = topBarPianoNote;
      return; // Don't process further (don't toggle recital mode)
    }
  }

  // 🎹 Handle lift for top bar piano
  if (e.is("lift") && topBarPianoActiveNote) {
    stopButtonNote(topBarPianoActiveNote, { force: true });
    topBarPianoActiveNote = null;
  }

  // 🎭 Tap on top bar to toggle recital mode (minimal wireframe UI)
  // Only in visualizer area (between piano end and waveBtn), not on piano keys
  if (e.is("touch") && e.y < TOP_BAR_BOTTOM && !projector && !paintPictureOverlay && !recitalMode) {
    // Check that tap is in the visualizer area (after piano, before waveBtn)
    const topPianoWidth = Math.min(140, Math.floor((screen.width - 54) * 0.5));
    const topPianoEndX = 54 + topPianoWidth;
    const vizLeft = topPianoEndX; // Start after piano
    const vizRight = waveBtn?.box?.x || screen.width;
    if (e.x >= vizLeft && e.x <= vizRight) {
      recitalMode = true;
      recitalBlinkPhase = 0;
    }
  }

  if ((e.is("touch") || e.is("lift")) && !paintPictureOverlay && !projector) {
    const sampleRateText = getSampleRateText(speaker?.sampleRate);
    const sampleRateLabel = sampleRateText ? MIDI_RATE_LABEL_TEXT : null;
    const showTrack = Boolean(song) && autopatConfig.showTrack !== false;
    const cachedLayout = getCachedLayout(screen, {
      songMode: Boolean(showTrack),
      pictureOverlay: paintPictureOverlay,
      rateText: sampleRateText,
      rateLabel: sampleRateLabel,
      sidePanelWidth: autopatConfig.sidePanelWidth,
    });
    const layout = cachedLayout.layout;

    // Skip mini input interactions in recital mode (wireframe-only rendering)
    if (layout.miniInputsEnabled && !recitalMode) {

      const trackHeight = showTrack ? TRACK_HEIGHT : 0;
      const trackY = showTrack ? SECONDARY_BAR_BOTTOM : null;
      const pianoGeometry = getMiniPianoGeometry({
        screen,
        layout,
        song: showTrack ? song : null,
        trackY,
        trackHeight,
      });

      if (e.is("touch")) {
        const velocity = e.velocity ?? 1;
        const pianoNote = getMiniPianoNoteAt(e.x, e.y, pianoGeometry);
        if (pianoNote) {
          if (miniMapActiveNote && miniMapActiveNote !== pianoNote) {
            stopButtonNote(miniMapActiveNote, { force: true });
          }
          if (miniMapActiveKey) {
            triggerPercKey(miniMapActiveKey, 1, false);
            miniMapActiveKey = null;
          }
          if (miniMapActiveNote !== pianoNote) {
            startButtonNote(pianoNote, 127, api);
          }
          miniMapActiveNote = pianoNote;
          return;
        }

        const key = getQwertyKeyAt(e.x, e.y, pianoGeometry);
        if (key) {
          const mappedNote = keyboardKeyToNote(key);
          miniMapActiveKey = key;
          if (mappedNote) {
            if (miniMapActiveNote && miniMapActiveNote !== mappedNote) {
              stopButtonNote(miniMapActiveNote, { force: true });
            }
            if (miniMapActiveKey && miniMapActiveKey !== key) {
              triggerPercKey(miniMapActiveKey, 1, false);
            }
            if (miniMapActiveNote !== mappedNote) {
              startButtonNote(mappedNote, 127, api);
            }
            miniMapActiveNote = mappedNote;
          } else {
            if (miniMapActiveNote) {
              stopButtonNote(miniMapActiveNote, { force: true });
              miniMapActiveNote = null;
            }
            if (miniMapActiveKey && miniMapActiveKey !== key) {
              triggerPercKey(miniMapActiveKey, 1, false);
            }
            triggerPercKey(key, velocity, true);
          }
          return;
        }
      }

      if (e.is("lift")) {
        if (miniMapActiveNote) {
          stopButtonNote(miniMapActiveNote, { force: true });
        }
        if (miniMapActiveKey) {
          triggerPercKey(miniMapActiveKey, 1, false);
        }
        miniMapActiveNote = null;
        miniMapActiveKey = null;
      }
    }
  }

  if (e.is("keyboard:down:.") && !e.repeat) {
    upperOctaveShift += 1;
  }

  if (e.is("keyboard:down:,") && !e.repeat) {
    upperOctaveShift -= 1;
  }

  // 🏠 Room mode toggle (/ key) - global reverb
  if (e.is("keyboard:down:/") && !e.repeat) {
    roomMode = !roomMode;
    room.toggle();
  }

  // 🧩 Glitch mode toggle (backspace key) - global raw synth effect
  if (e.is("keyboard:down:backspace") && !e.repeat) {
    glitchMode = !glitchMode;
    glitch?.toggle?.();
    cleanupOrphanedSounds(pens, true);
  }

  if (
    e.is("keyboard:down") &&
    !e.repeat &&
    e.key === "Shift" &&
    e.code === "ShiftLeft"
  ) {
    quickFade = !quickFade;
  }

  if (
    e.is("keyboard:down") &&
    !e.repeat &&
    e.key === "Shift" &&
    e.code === "ShiftRight"
  ) {
    // console.log("Code:", e.code);
    slide = !slide;

    if (slide && Object.keys(tonestack).length > 1) {
      const orderedTones = orderedByCount(tonestack);
      orderedTones.forEach((tone, index) => {
        if (index > 0) {
          sounds[tone]?.sound.kill(quickFade ? fastFade : fade); // Kill a sound if it exists.
          trail[tone] = 1;
          delete tonestack[tone]; // Remove this key from the notestack.
          delete sounds[tone];
          if (buttons[tone]) buttons[tone].down = false;
        }
      });
    }
    // Clean up any orphaned sounds when toggling slide
    cleanupOrphanedSounds(pens, true);
  }

  //  if (
  //    e.is("keyboard:down:control") ||
  //    (e.is("keyboard:down:capslock") && !e.repeat)
  //  ) {
  //    lowerOctaveShift += 1;
  //  }

  if (e.is("keyboard:down:tab") && !e.repeat) {
    api.beep();
    waveIndex = (waveIndex + 1) % wavetypes.length;
    wave = wavetypes[waveIndex];
    buildWaveButton(api);
  }

  // if (e.is("keyboard:down:shift") && !e.repeat) {
  //   lowerOctaveShift -= 1;
  // }

  if (e.is("keyboard:down:-")) paintTransposeOverlay = !paintTransposeOverlay;
  if (e.is("keyboard:down:=")) {
    paintPictureOverlay = !paintPictureOverlay;
    hud.label(paintPictureOverlay ? undefined : "notepat");
    setupButtons(api);
  }
  if (e.is("keyboard:down:\\")) projector = !projector;

  // � Performance OSD toggle (backtick key)
  if (e.is("keyboard:down:`") && !e.repeat) {
    perfOSD = !perfOSD;
  }

  // �🚨 PANIC BUTTON: Press Escape to stop all stuck sounds
  if (e.is("keyboard:down:escape")) {
    console.log("🚨 PANIC: Force stopping all sounds!");
    Object.keys(sounds).forEach(note => {
      console.log("🔇 Force killing stuck sound:", note);
      sounds[note]?.sound?.kill(0.01); // Very fast kill
    });
    // Clear all state
    Object.keys(sounds).forEach(note => delete sounds[note]);
    Object.keys(tonestack).forEach(note => delete tonestack[note]);
    Object.keys(trail).forEach(note => delete trail[note]);
    console.log("🧹 All audio state cleared");
  }

  // if (e.is("keyboard:down:arrowleft")) {
  // scopeTrim -= 1;
  // if (scopeTrim < 0) scopeTrim = 0;
  // }

  // if (e.is("keyboard:down:arrowright")) {
  // scopeTrim += 1;
  // }

  /*
  if (e.is("keyboard:down:arrowdown")) {
    scope -= 1;
    if (scope < 0) scope = 0;
  }

  if (e.is("keyboard:down:arrowup")) {
    scope += 1;
    if (scope > tones.waveforms.left.length) {
      scope = tones.waveforms.left.length;
    }
  }
  */

  //if (tap) {
  // if (e.is("keyboard:down:shift") && !e.repeat) hold = true;
  // if (e.is("keyboard:up:shift")) hold = false;
  //} else {
  // if (e.is("keyboard:down:shift") && !e.repeat) sharps = true;
  // if (e.is("keyboard:up:shift")) sharps = false;
  // if (e.is("keyboard:down:alt") && !e.repeat) flats = true;
  // if (e.is("keyboard:up:alt")) flats = false;
  //}

  // 🥁 Percussion Sound Design
  // Each drum uses layered synthesis for a more musical sound
  // Velocity (0-127) scales volume for expressive playing
  const pc = "maroon";
  
  // Kick drum - 808-style with BIG sustained BOOOOOOM
  const makeKick = (velocity = 127) => {
    const vel = velocity / 127; // Normalize to 0-1
    
    // === LAYER 1: THE BIG BOOM - long sustained sub ===
    // This is the BOOOOOooooom that sustains and rumbles
    synth({
      type: "sine",
      tone: 38, // Very deep sub
      duration: 0.6, // LONG tail for that 808 sustain
      attack: 0.001,
      decay: 0.5, // Slower decay = longer boom
      volume: 1.0 * vel,
    });
    
    // === LAYER 2: Sub harmonic for warmth ===
    synth({
      type: "sine",
      tone: 55, // Octave-ish above for body
      duration: 0.5,
      attack: 0.001,
      decay: 0.55,
      volume: 0.7 * vel,
    });
    
    // === LAYER 3: Punch body (the THUMP) ===
    synth({
      type: "sine", 
      tone: 90,
      duration: 0.07,
      attack: 0,
      decay: 0.85,
      volume: 0.8 * vel,
    });
    
    // === LAYER 4: Click transient (the SNAP) ===
    synth({
      type: "square",
      tone: 1200,
      duration: 0.008,
      attack: 0,
      decay: 0.99,
      volume: 0.5 * vel,
    });
    
    // === LAYER 5: Noise burst for attack presence ===
    synth({
      type: "noise-white",
      tone: 800,
      duration: 0.012,
      attack: 0,
      decay: 0.95,
      volume: 0.4 * vel,
    });
    
    // === LAYER 6: Mid knock (beater sound) ===
    synth({
      type: "triangle",
      tone: 150,
      duration: 0.04,
      attack: 0,
      decay: 0.9,
      volume: 0.5 * vel,
    });
    
    // === LAYER 7: Extra low end sustain ===
    // Second sub layer that decays even slower
    synth({
      type: "sine",
      tone: 42,
      duration: 0.8, // Even longer!
      attack: 0.01,
      decay: 0.4, // Very slow decay
      volume: 0.6 * vel,
    });
  };
  
  // Snare - layered noise at different pitches for character
  const makeSnare = (velocity = 127) => {
    const vel = velocity / 127;
    // High "snap" - bright filtered noise
    synth({
      type: "noise-white",
      tone: 5000,
      duration: 0.1,
      attack: 0,
      decay: 0.85,
      volume: 0.5 * vel,
    });
    // Mid "body" - pitched noise around snare resonance
    synth({
      type: "noise-white",
      tone: 250, // Pitched noise for snare body character
      duration: 0.15,
      attack: 0.001,
      decay: 0.88,
      volume: 0.45 * vel,
    });
    // Fundamental tone - the "drum" part
    synth({
      type: "triangle",
      tone: 180,
      duration: 0.1,
      attack: 0,
      decay: 0.9,
      volume: 0.35 * vel,
    });
  };
  
  // Hi-hat - layered high-pitched noise for shimmer
  const makeHihat = (velocity = 127) => {
    const vel = velocity / 127;
    // Main hat - very high pitched noise
    synth({
      type: "noise-white",
      tone: 10000,
      duration: 0.05,
      attack: 0,
      decay: 0.8,
      volume: 0.3 * vel,
    });
    // Shimmer layer - slightly lower for body
    synth({
      type: "noise-white",
      tone: 7000,
      duration: 0.04,
      attack: 0,
      decay: 0.75,
      volume: 0.2 * vel,
    });
  };
  
  // Open hi-hat / ride - longer decay with bell character
  const makeRide = (velocity = 127) => {
    const vel = velocity / 127;
    // Main body - mid-high pitched noise
    synth({
      type: "noise-white",
      tone: 6000,
      duration: 0.3,
      attack: 0.005,
      decay: 0.65,
      volume: 0.25 * vel,
    });
    // Shimmer - higher pitched noise
    synth({
      type: "noise-white",
      tone: 9000,
      duration: 0.2,
      attack: 0.01,
      decay: 0.6,
      volume: 0.2 * vel,
    });
    // Bell tone - adds pitch character
    synth({
      type: "sine",
      tone: 800,
      duration: 0.18,
      attack: 0.005,
      decay: 0.75,
      volume: 0.12 * vel,
    });
  };
  
  // Crash cymbal - wide frequency spread
  const makeCrash = (velocity = 127) => {
    const vel = velocity / 127;
    // Low body
    synth({
      type: "noise-white",
      tone: 3000,
      duration: 0.5,
      attack: 0.001,
      decay: 0.55,
      volume: 0.35 * vel,
    });
    // Mid shimmer
    synth({
      type: "noise-white",
      tone: 6000,
      duration: 0.45,
      attack: 0.001,
      decay: 0.5,
      volume: 0.4 * vel,
    });
    // High sparkle
    synth({
      type: "noise-white",
      tone: 10000,
      duration: 0.35,
      attack: 0,
      decay: 0.45,
      volume: 0.3 * vel,
    });
  };
  
  // Low tom
  const makeTomLow = (velocity = 127) => {
    const vel = velocity / 127;
    synth({
      type: "sine",
      tone: 100,
      duration: 0.2,
      attack: 0.001,
      decay: 0.9,
      volume: 0.7 * vel,
    });
    synth({
      type: "triangle",
      tone: 180,
      duration: 0.1,
      attack: 0,
      decay: 0.85,
      volume: 0.3 * vel,
    });
  };
  
  // High tom  
  const makeTomHigh = (velocity = 127) => {
    const vel = velocity / 127;
    synth({
      type: "sine",
      tone: 160,
      duration: 0.15,
      attack: 0.001,
      decay: 0.88,
      volume: 0.6 * vel,
    });
    synth({
      type: "triangle",
      tone: 280,
      duration: 0.08,
      attack: 0,
      decay: 0.82,
      volume: 0.25 * vel,
    });
  };
  
  // Legacy makePerc for backwards compatibility (clicky sounds)
  const makePerc = (hz) => {
    synth({
      type: "triangle",
      tone: hz / 2,
      duration: 0.01,
      attack: 0,
      volume: 0.5,
    });
    synth({ type: "sawtooth", tone: hz, duration: 0.0025, volume: 1 });
    synth({
      type: "square",
      tone: hz / 4,
      duration: 0.005,
      volume: 0.95,
      decay: 0.999,
    });
  };

  const lowerBaseOctave = () => parseInt(octave) + lowerOctaveShift;
  const upperBaseOctave = () => parseInt(octave) + 1 + upperOctaveShift;

  const flatToSharp = {
    DB: "C#",
    EB: "D#",
    GB: "F#",
    AB: "G#",
    BB: "A#",
  };

  const MIDI_NOTE_ON = 0x90;
  const MIDI_NOTE_OFF = 0x80;
  const MIDI_PITCH_BEND = 0xe0;

  const midiNoteToButton = (noteNumber) => {
      if (!midiUtil?.note || typeof noteNumber !== "number") return null;

      const raw = midiUtil.note(noteNumber);
      if (!raw) return null;

      const normalized = raw.toUpperCase();
      const match = normalized.match(/^([A-G])([#B]?)(-?\d+)$/);
      if (!match) return null;

      let [, letter, accidental, octaveStr] = match;

      if (accidental === "B") {
        const enharmonic = flatToSharp[`${letter}${accidental}`];
        if (!enharmonic) return null;
        letter = enharmonic[0];
        accidental = enharmonic[1] || "";
      }

      const noteName = (letter + accidental).toLowerCase();
      const noteOctave = parseInt(octaveStr, 10);

      if (Number.isNaN(noteOctave)) return null;

      const lowerOct = lowerBaseOctave();
      const upperOct = upperBaseOctave();

      if (noteOctave === lowerOct && buttonNoteLookup.has(noteName)) {
        return noteName;
      }
      if (noteOctave === upperOct) {
        const upperNote = `+${noteName}`;
        if (buttonNoteLookup.has(upperNote)) {
          return upperNote;
        }
      }

      if (noteOctave < lowerOct) {
        const offset = (lowerOct - noteOctave) % 2;
        const baseNote = offset === 0 ? noteName : `+${noteName}`;
        if (buttonNoteLookup.has(baseNote)) {
          return baseNote;
        }
      }

      if (noteOctave > upperOct) {
        const offset = (noteOctave - upperOct) % 2;
        const baseNote = offset === 0 ? `+${noteName}` : noteName;
        if (buttonNoteLookup.has(baseNote)) {
          return baseNote;
        }
      }

      if (buttonNoteLookup.has(noteName)) {
        console.warn("🎹 MIDI note folded to base octave:", {
          noteNumber,
          raw,
          mappedTo: noteName,
        });
        return noteName;
      }

      const sharpCandidate = `+${noteName}`;
      if (buttonNoteLookup.has(sharpCandidate)) {
        console.warn("🎹 MIDI note folded to upper octave:", {
          noteNumber,
          raw,
          mappedTo: sharpCandidate,
        });
        return sharpCandidate;
      }

      console.warn("🎹 MIDI note unmapped:", { noteNumber, raw, lowerOct, upperOct });
      return null;
    };

  const startMidiButtonNote = (buttonNote, velocity = 127) => {
    if (!buttonNote) return false;

    if (song && buttonNote.toUpperCase() !== song?.[songIndex][0]) {
      synth({
        type: "noise-white",
        tone: 1000,
        duration: 0.05,
        volume: 0.3,
        attack: 0,
      });
      return false;
    }

    anyDown = true;
    noteShake[buttonNote] = 3; // Trigger per-note typography shake

    let noteName = buttonNote;
    let targetOctave = lowerBaseOctave();
    if (buttonNote.startsWith("+")) {
      noteName = buttonNote.slice(1);
      targetOctave = upperBaseOctave();
    }

    const noteUpper = noteName.toUpperCase();
    const tone = `${targetOctave}${noteUpper}`;
    const active = orderedByCount(sounds);

    if (slide && active.length > 0) {
      sounds[active[0]]?.sound?.update({ tone, duration: 0.1 });
      tonestack[buttonNote] = {
        count: Object.keys(tonestack).length,
        tone,
      };
      sounds[buttonNote] = sounds[active[0]];
      if (sounds[buttonNote]) sounds[buttonNote].note = buttonNote;
      delete sounds[active[0]];
      applyPitchBendToNotes([buttonNote], { immediate: true });
    } else {
      tonestack[buttonNote] = {
        count: Object.keys(tonestack).length,
        tone,
      };

      const pan = getPanForButtonNote(buttonNote);
      let soundHandle = makeNoteSound(tone, velocity, pan);

      if (!soundHandle || typeof soundHandle.kill !== "function") {
        const velocityRatio = velocity === undefined ? 1 : velocity / 127;
        const clampedRatio = num?.clamp
          ? num.clamp(velocityRatio, 0, 1)
          : Math.max(0, Math.min(1, velocityRatio));
        const minVelocityVolume = 0.05;
        const fallbackVolume =
          toneVolume * (minVelocityVolume + (1 - minVelocityVolume) * clampedRatio);
        soundHandle = synth({
          type: "sine",
          tone: freq(tone),
          attack: quickFade ? 0.0015 : attack,
          decay: 0.9,
          duration: 0.4,
          volume: fallbackVolume,
          pan,
        });
      }

      sounds[buttonNote] = {
        note: buttonNote,
        count: active.length + 1,
        sound: soundHandle,
      };

      applyPitchBendToNotes([buttonNote], { immediate: true });

      if (buttonNote.toUpperCase() === song?.[songIndex][0]) {
        songNoteDown = true;
      }

      delete trail[buttonNote];

      pictureAdd(api, tone);
      udpServer?.send("tv", { note: buttonNote });
    }

    if (buttons[buttonNote]) {
      buttons[buttonNote].down = true;
      buttons[buttonNote].over = true;
    }

    return true;
  };

  const stopMidiButtonNote = (buttonNote) => {
    if (!buttonNote) return;

    const orderedTones = orderedByCount(tonestack);

    if (slide && orderedTones.length > 1 && sounds[buttonNote]) {
      const previousKey = orderedTones[orderedTones.length - 2];
      const previousTone = tonestack[previousKey]?.tone;
      if (previousTone) {
        sounds[buttonNote]?.sound?.update({ tone: previousTone, duration: 0.1 });
        sounds[previousKey] = sounds[buttonNote];
        if (sounds[previousKey]) sounds[previousKey].note = previousKey;
        applyPitchBendToNotes([previousKey], { immediate: true });
      }
    } else if (sounds[buttonNote]?.sound) {
      const soundEntry = sounds[buttonNote];
      const lifespan = soundEntry.sound?.startedAt
        ? performance.now() / 1000 - soundEntry.sound.startedAt
        : 0.1;
      const fade = max(0.075, min(lifespan, 0.15));
      soundEntry.sound.kill(quickFade ? fastFade : fade);
    }

    if (buttonNote.toUpperCase() === song?.[songIndex][0]) {
      songIndex = (songIndex + 1) % song.length;
      songNoteDown = false;
      songShifting = true;
    }

    delete tonestack[buttonNote];
    delete sounds[buttonNote];
    trail[buttonNote] = 1;

    if (buttons[buttonNote]) {
      buttons[buttonNote].down = false;
      buttons[buttonNote].over = false;
    }

    if (melodyAliasActiveNote === buttonNote) {
      melodyAliasDown = false;
      melodyAliasActiveNote = null;
      melodyAliasStartedNote = false;
    }
  };

  if (e.is("midi:keyboard")) {
    midiConnected = true;

    const status = e.data?.[0] ?? 0;
    const noteNumber = e.data?.[1];
    const velocity = e.data?.[2] ?? 0;
    const command = status & 0xf0;

    if (command === MIDI_PITCH_BEND) {
      const lsb = e.data?.[1] ?? 0;
      const msb = e.data?.[2] ?? 0;
      const rawValue = (msb << 7) | lsb; // 0 - 16383
      const normalized = (rawValue - 8192) / 8192;
      const clamped = num?.clamp
        ? num.clamp(normalized, -1, 1)
        : Math.max(-1, Math.min(1, normalized));

      if (clamped !== midiPitchBendValue) {
        midiPitchBendValue = clamped;
        applyPitchBendToNotes(undefined, { immediate: true });
        console.log("🎛️ MIDI pitch bend", {
          rawValue,
          normalized,
          clamped,
          ratio: computePitchBendRatio(),
        });
      }

      return;
    }

    if (typeof noteNumber === "number") {
      if (command === MIDI_NOTE_ON && velocity > 0) {
        // 📊 Track MIDI key press time for latency measurement
        perfStats.lastKeyTime = performance.now();
        
        const buttonNote = midiNoteToButton(noteNumber);
        if (buttonNote && startMidiButtonNote(buttonNote, velocity)) {
          console.log("🎹 MIDI note on", { noteNumber, velocity, buttonNote });
          midiActiveNotes.set(noteNumber, buttonNote);
        }
      } else if (command === MIDI_NOTE_OFF || (command === MIDI_NOTE_ON && velocity === 0)) {
        const mappedNote = midiActiveNotes.get(noteNumber) ?? midiNoteToButton(noteNumber);
        if (mappedNote) {
          console.log("🎹 MIDI note off", { noteNumber, buttonNote: mappedNote });
          stopMidiButtonNote(mappedNote);
        }
        midiActiveNotes.delete(noteNumber);
      }
    }

    return;
  }

  if (!tap) {
    if (e.is("keyboard:down:space") && !e.repeat) {
      perc = pc; //"cyan";
      percDowns.space = true;
      makeSnare(e.velocity);
    }
  }

  if (e.is("keyboard:down:alt") && !e.repeat && e.code === "AltLeft") {
    perc = pc; //"cyan";
    percDowns.alt = true;
    makeCrash(e.velocity);
  }

  if (e.is("keyboard:down:alt") && !e.repeat && e.code === "AltRight") {
    perc = pc; //"cyan";
    percDowns.alt = true;
    makeRide(e.velocity);
  }

  if (e.is("keyboard:down:arrowleft") && !e.repeat && !percDowns.left) {
    perc = "brown";
    percDowns.left = true;
    makeTomLow(e.velocity);
  }

  if (e.is("keyboard:down:arrowdown") && !e.repeat && !percDowns.down) {
    perc = "pink";
    percDowns.down = true;
    makeKick(e.velocity);
  }

  if (e.is("keyboard:down:arrowright") && !e.repeat && !percDowns.right) {
    perc = "orange";
    percDowns.right = true;
    makeTomHigh(e.velocity);
  }

  if (e.is("keyboard:down:arrowup") && !e.repeat && !percDowns.up) {
    percDowns.up = true;
    perc = "cyan";
    makeHihat(e.velocity);
  }
  if (e.is("keyboard:up:arrowleft")) {
    delete percDowns.left;
  }

  if (e.is("keyboard:up:arrowdown")) {
    delete percDowns.down;
  }

  if (e.is("keyboard:up:arrowright")) {
    delete percDowns.right;
  }

  if (e.is("keyboard:up:arrowup")) {
    delete percDowns.up;
  }

  if (e.is("keyboard:up:space") && !tap) {
    delete percDowns.space;
  }

  if (e.is("keyboard:up:alt")) {
    delete percDowns.alt;
  }

  if (!tap) {
    const activePens = pens?.();
    if (activePens?.length > 0) {
      anyDown = true;
    }

    if (e.is("lift") && activePens?.length <= 1) {
      anyDown = false;
      // Check for orphaned sounds when all touches are lifted
      cleanupOrphanedSounds(pens);
    }

    octBtn?.act(e, {
      down: () => api.beep(400),
      push: (btn) => {
        api.beep();
        waveIndex = (waveIndex + 1) % wavetypes.length;
        const octNum = parseInt(octave);
        octave = max(1, (octNum + 1) % 10).toString();
        buildOctButton(api);
      },
    });

    waveBtn?.act(e, {
      down: () => api.beep(400),
      push: (btn) => {
        api.beep();
        waveIndex = (waveIndex + 1) % wavetypes.length;
        wave = wavetypes[waveIndex];
        buildWaveButton(api);
      },
    });

    // 🎛️ Toggle button interactions
    slideBtn?.act(e, {
      push: () => {
        api.beep();
        slide = !slide;
        // Kill extra tones when enabling slide mode (keep only most recent)
        if (slide && Object.keys(tonestack).length > 1) {
          const orderedTones = orderedByCount(tonestack);
          orderedTones.forEach((tone, index) => {
            if (index > 0) {
              sounds[tone]?.sound.kill(quickFade ? fastFade : fade);
              trail[tone] = 1;
              delete tonestack[tone];
              delete sounds[tone];
              if (buttons[tone]) buttons[tone].down = false;
            }
          });
        }
        // Also clean up orphaned sounds when toggling slide
        cleanupOrphanedSounds(pens, true);
      },
    });

    roomBtn?.act(e, {
      push: () => {
        api.beep();
        roomMode = !roomMode;
        room.toggle();
      },
    });

    glitchBtn?.act(e, {
      push: () => {
        api.beep();
        glitchMode = !glitchMode;
        glitch?.toggle?.();
        // Clean up any stuck sounds when toggling glitch
        cleanupOrphanedSounds(pens, true);
      },
    });

    quickBtn?.act(e, {
      push: () => {
        api.beep();
        quickFade = !quickFade;
      },
    });

    // 🥁 Metronome button interactions
    bpmMinusBtn?.act(e, {
      push: () => {
        api.beep();
        // Decrease BPM by 5 (min 20)
        metronomeBPM = Math.max(20, metronomeBPM - 5);
        metronomeBeatCount = 0; // Reset beat sync
      },
    });

    bpmPlusBtn?.act(e, {
      push: () => {
        api.beep();
        // Increase BPM by 5 (max 300)
        metronomeBPM = Math.min(300, metronomeBPM + 5);
        metronomeBeatCount = 0; // Reset beat sync
      },
    });

    metroBtn?.act(e, {
      push: () => {
        api.beep();
        metronomeEnabled = !metronomeEnabled;
        if (metronomeEnabled) {
          // Reset beat count to resync on enable
          metronomeBeatCount = 0;
          metronomeVisualPhase = 0;
        }
      },
    });

    const activateMelodyAlias = () => {
      if (!song || paintPictureOverlay || projector) return false;
      const currentNote = song?.[songIndex]?.[0];
      if (!currentNote) return false;

      const targetButtonNote = currentNote.toLowerCase();

      if (melodyAliasDown && melodyAliasActiveNote === targetButtonNote) {
        return false;
      }

      if (sounds[targetButtonNote]) {
        melodyAliasDown = true;
        melodyAliasActiveNote = targetButtonNote;
        melodyAliasStartedNote = false;
        return true;
      }

      const started = startMidiButtonNote(targetButtonNote, 127);
      if (started) {
        melodyAliasDown = true;
        melodyAliasActiveNote = targetButtonNote;
        melodyAliasStartedNote = true;
        return true;
      }

      return false;
    };

    const releaseMelodyAlias = () => {
      if (!melodyAliasDown) return;
      if (melodyAliasStartedNote && melodyAliasActiveNote) {
        stopMidiButtonNote(melodyAliasActiveNote);
      }
      melodyAliasDown = false;
      melodyAliasActiveNote = null;
      melodyAliasStartedNote = false;
    };

    if (melodyAliasBtn && song && !paintPictureOverlay && !projector) {
      melodyAliasBtn.act(
        e,
        {
        down: () => {
          activateMelodyAlias();
        },
        push: () => {
          releaseMelodyAlias();
        },
        up: () => {
          releaseMelodyAlias();
        },
        cancel: () => {
          releaseMelodyAlias();
        },
        out: () => {
          releaseMelodyAlias();
        },
        },
        pens?.(),
      );
    }

    buttonNotes.forEach((note) => {
      if (buttons[note]) {
        buttons[note].act(
          e,
          {
            down: (btn) => {
              return startButtonNote(note, 127, api);
            },
            over: (btn) => {
              if (btn.up && anyDown) {
                btn.up = false;
                startButtonNote(note, 127, api);
                
                // In song mode, if we drag into the correct note, mark it as pressed
                if (song && note.toUpperCase() === song?.[songIndex][0]) {
                  songNoteDown = true;
                }
              }
            },
            // TODO: The order of over and out will be important...
            out: (btn) => {
              // Only stop the sound when dragging off, but don't affect button down state
              sounds[note]?.sound?.kill(killFade);
              delete tonestack[note]; // Remove from notestack  
              delete sounds[note]; // Remove sound reference
              delete trail[note]; // Clean up trail
              
              // If this was the song note, reset the flag so it doesn't advance on wrong note
              if (song && note.toUpperCase() === song?.[songIndex][0] && songNoteDown) {
                songNoteDown = false;
              }
            },
            cancel: (btn) => {
              // Force cleanup when button gets stuck - stop the sound immediately
              sounds[note]?.sound?.kill(fastFade); // Use fast fade for force cleanup
              delete tonestack[note]; // Remove from notestack
              delete sounds[note]; // Remove sound reference
              delete trail[note]; // Clean up trail
            },
            up: (btn) => {
              // ❤️‍🔥 TODO: How to manage the sliding here?
              if (downs[note]) return false;

              const orderedTones = orderedByCount(tonestack);

              if (slide && orderedTones.length > 1 && sounds[note]) {
                sounds[note]?.sound?.update({
                  tone: tonestack[orderedTones[orderedTones.length - 2]].tone,
                  duration: 0.1,
                });
                const previousKey = orderedTones[orderedTones.length - 2];
                sounds[previousKey] = sounds[note];
                if (sounds[previousKey]) sounds[previousKey].note = previousKey;
                applyPitchBendToNotes([previousKey], { immediate: true });
              } else {
                sounds[note]?.sound.kill(quickFade ? fastFade : killFade);
              }

              // console.log("🪱 Trail:", note);

              trail[note] = 1;

              if (note.toUpperCase() === song?.[songIndex][0]) {
                songIndex = (songIndex + 1) % song.length;
                songNoteDown = false;
                songShifting = true;
              }

              delete tonestack[note]; // Remove this key from the notestack.
              delete sounds[note];
              //} else {
                stopButtonNote(note);
              // sounds[orderedTones[orderedTones.length - 2]] = sounds[key];
              //}
            },
          },
          pens?.(),
        );
      }
    });
  }

  if (tap) {
    if ((e.is("touch") || e.is("keyboard:down:space")) && !e.repeat) {
      let reset = false;
      let tappedOctave;
      let tempTapIndex = tapIndex;

      if (!keys || keys.length === 0) {
        reset = true;
      } else {
        if (octaves.indexOf(keys[tempTapIndex]) > -1) {
          tappedOctave = keys[tempTapIndex];
          tempTapIndex += 1;
        }

        tapped = keys[tempTapIndex];
        if (!tapped) reset = true;
      }

      if (!reset) {
        if (accents.indexOf(keys[tempTapIndex + 1]) > -1) {
          tapped = keys[tempTapIndex] + keys[tempTapIndex + 1];
        }

        const tone = tapped;
        if (tappedOctave) tapped = tappedOctave + tapped;

        const pan = getPanForButtonNote(tone.toLowerCase());
        sounds[tapped] = makeNoteSound(octave + tone, e.velocity ?? 127, pan); // Use velocity from event
      } else {
        resetModeState();
      }
    }

    // TODO: This needs to work for multi-touch.

    if (e.is("keyboard:up:space") || e.is("lift")) {
      // Push the `tapIndex` forward according to octave, then for note, then
      // for the accent.
      if (octaves.indexOf(keys[tapIndex]) > -1) tapIndex += 1;
      tapIndex += 1;
      if (accents.indexOf(keys[tapIndex]) > -1) tapIndex += 1;

      if (tapIndex >= keys.length) {
        octave = STARTING_OCTAVE;
        tapIndex = 0;
      }

      sounds[tapped]?.kill(quickFade ? fastFade : killFade);
      delete sounds[tapped];
      tapped = undefined;
    }
  }

  // Individual Keyboard Notes
  [...(octaves + notes + edges).split(""), "control"].forEach((key) => {
    if (e.is(`keyboard:down:${key}`) && !e.repeat && !downs[key]) {
      // 📊 Track key press time for latency measurement
      perfStats.lastKeyTime = performance.now();
      
      downs[key] = true;

      if (!tap) {
        if (octaves.includes(key) && octaves.includes(keys.slice(-1)))
          keys = keys.slice(0, -1);
        editable = true;
      } else if (keys[tapIndex] === key) {
        tapped = key;
      }

      if (octaves.includes(key)) {
        // 🎹 Keyboard -> 🎼 Octave recognition.
        octave = parseInt(key);
        sounds[key] = "held";
        keys += key.toUpperCase();
      } else {
        // 🎹 Keyboard -> 🎵 Note recognition.
        let note = key.toUpperCase();

        // console.log("🫐 Extended Note ?:", note);

        if ("ZX".includes(note) || note === "CONTROL") {
          switch (note) {
            case "CONTROL":
              note = "-A";
              break;
            case "Z":
              note = "-A#";
              break;
            case "X":
              note = "-B";
              break;
          }
        }

        // console.log(note);

        if (";']".includes(note)) {
          switch (note) {
            case ";":
              note = "++C";
              break;
            case "'":
              note = "++C#";
              break;
            case "]":
              note = "++D";
              break;
          }
        }

        if ("VSWRQ".includes(note)) {
          switch (note) {
            case "V":
              note = "C#";
              break;
            case "S":
              note = "D#";
              break;
            case "W":
              note = "F#";
              break;
            case "R":
              note = "G#";
              break;
            case "Q":
              note = "A#";
              break;
          }
        }

        let activeOctave = parseInt(octave); //parseInt(octave);
        let buttonNote;

        if (("HIJKLMN" + "TYUOP").includes(note)) {
          switch (note) {
            case "H":
              note = "C";
              break;
            case "I":
              note = "D";
              break;
            case "J":
              note = "E";
              break;
            case "K":
              note = "F";
              break;
            case "L":
              note = "G";
              break;
            case "M":
              note = "A";
              break;
            case "N":
              note = "B";
              break;
            // Semitones
            case "T":
              note = "C#";
              break;
            case "Y":
              note = "D#";
              break;
            case "U":
              note = "F#";
              break;
            case "O":
              note = "G#";
              break;
            case "P":
              note = "A#";
              break;
          }
          activeOctave += 1;
          activeOctave += upperOctaveShift;
          buttonNote = "+" + note.toLowerCase();
        } else {
          activeOctave += lowerOctaveShift;
          buttonNote = note.toLowerCase();
        }

        // Adjust active octave based on extension note
        // if needed.

        if (note.startsWith("-")) {
          note = note.replace("-", "");
          activeOctave -= 1;
        } else if (note.startsWith("++")) {
          note = note.replace("++", "");
          activeOctave += 2;
          activeOctave += upperOctaveShift;
        }

        // if (activeOctave !== parseInt(octave)) {
        // keys += activeOctave + note;
        // } else {
        keys += activeOctave + note;
        // }

        // const buttonNote =
        //  (activeOctave === parseInt(octave) ? "" : "+") + note.toLowerCase();

        // 🎵 Block wrong keyboard keys in song mode (melody learning mode)
        if (song && buttonNote.toUpperCase() !== song?.[songIndex][0]) {
          // Play white noise bump for wrong key press
          synth({
            type: "noise-white",
            tone: 1000,
            duration: 0.05,
            volume: 0.3,
            attack: 0,
          });
          return; // Block this wrong key in melody mode
        }

        if (buttons[buttonNote]) buttons[buttonNote].down = true;

        const active = orderedByCount(sounds);

        const tone = `${activeOctave}${note}`;

        if (slide && active.length > 0) {
          // TODO: Fix slide here... 24.08.16.06.18
          //console.log("Fix slide...", active[0], sounds[active[0]]?.sound, tone, key);

          sounds[active[0]]?.sound?.update({ tone, duration: 0.1 });

          tonestack[buttonNote] = {
            count: Object.keys(tonestack).length,
            tone,
          };
          sounds[buttonNote] = sounds[active[0]]; // Switch the note label.
          if (sounds[buttonNote]) sounds[buttonNote].note = buttonNote;
          delete sounds[active[0]]; // Swap the sound reference.
          applyPitchBendToNotes([buttonNote], { immediate: true });
        } else {
          tonestack[buttonNote] = {
            count: Object.keys(tonestack).length,
            tone,
          };
          const pan = getPanForQwertyKey(key);
          sounds[buttonNote] = {
            note: buttonNote,
            count: active.length + 1,
            sound: makeNoteSound(tone, e.velocity ?? 127, pan), // Use velocity from event
          };

          applyPitchBendToNotes([buttonNote], { immediate: true });

          if (buttonNote.toUpperCase() === song?.[songIndex][0]) {
            songNoteDown = true;
          }

          delete trail[buttonNote];

          pictureAdd(api, tone);
          udpServer?.send("tv", { note: buttonNote }); // Send udp message for note.
        }
      }
    }

    // Just for the notes, not the octaves...
    if (e.is(`keyboard:up:${key}`) /*&& notes.indexOf(key) > -1*/) {
      console.log("🎹 keyboard:up event for key:", key, "downs[key]:", downs[key]);
      if (tap && tapped === key) {
        tapIndex = (tapIndex + 1) % keys.length;
        tapped = undefined;
      }

      if (downs[key]) {
        delete downs[key];

        // let buttonNote = key;
        function noteFromKey(key) {
          let buttonNote = key;

          if ("vswrq".includes(key)) {
            switch (key) {
              case "v":
                buttonNote = "c#";
                break;
              case "s":
                buttonNote = "d#";
                break;
              case "w":
                buttonNote = "f#";
                break;
              case "r":
                buttonNote = "g#";
                break;
              case "q":
                buttonNote = "a#";
                break;
            }
          }
          // TODO: This matching code should be combined across event handlers. 24.08.15.06.40
          let activeOctave = octave;

          if (("hijklmn" + "tyuop").includes(buttonNote)) {
            switch (buttonNote) {
              case "h":
                buttonNote = "c";
                break;
              case "i":
                buttonNote = "d";
                break;
              case "j":
                buttonNote = "e";
                break;
              case "k":
                buttonNote = "f";
                break;
              case "l":
                buttonNote = "g";
                break;
              case "m":
                buttonNote = "a";
                break;
              case "n":
                buttonNote = "b";
                break;
              // Semitones
              case "t":
                buttonNote = "c#";
                break;
              case "y":
                buttonNote = "d#";
                break;
              case "u":
                buttonNote = "f#";
                break;
              case "o":
                buttonNote = "g#";
                break;
              case "p":
                buttonNote = "a#";
                break;
            }
            activeOctave = parseInt(octave) + 1;
          }

          if (activeOctave !== octave) buttonNote = "+" + buttonNote;

          if ("zx".includes(buttonNote) || buttonNote === "control") {
            switch (buttonNote) {
              case "x":
                buttonNote = "-b";
                break;
              case "z":
                buttonNote = "-a#";
                break;
              case "control":
                buttonNote = "-a";
                break;
            }
          }

          if (";']".includes(buttonNote)) {
            switch (buttonNote) {
              case ";":
                buttonNote = "++c";
                break;
              case "'":
                buttonNote = "++c#";
                break;
              case "]":
                buttonNote = "++d";
                break;
            }
          }

          return buttonNote;
        }

        const buttonNote = noteFromKey(key);

        const orderedTones = orderedByCount(tonestack);

        // console.log(
        //   "Ordered Tones:",
        //   orderedTones,
        //   "Sounds:",
        //   sounds,
        //   "Key:",
        //   key,
        // );

        // In slide mode, the sound reference may have been moved to another key
        // Only process if this note still has an active sound reference
        const hasSound = sounds[buttonNote] !== undefined;

        if (slide && orderedTones.length > 1 && hasSound) {
          sounds[buttonNote]?.sound?.update({
            tone: tonestack[orderedTones[orderedTones.length - 2]].tone,
            duration: 0.1,
          });

          // console.log(
          //   "New sound key is:",
          //   tonestack[orderedTones[orderedTones.length - 2]],
          //   noteFromKey(orderedTones[orderedTones.length - 2]),
          //   orderedTones,
          //   sounds
          // );

          const previousKeyRaw = orderedTones[orderedTones.length - 2];
          const n = noteFromKey(previousKeyRaw);
          sounds[n] = sounds[buttonNote];
          if (sounds[n]) sounds[n].note = n;
          applyPitchBendToNotes([previousKeyRaw], { immediate: true });
          // console.log("Replaced:", buttonNote, "with:", n);

          // delete sounds[buttonNote];
        } else if (hasSound) {
          // Only kill the sound if we still have a reference to it
          // console.log("Killing sound:", buttonNote);

          if (sounds[buttonNote]?.sound) {
            const fade = max(
              0.075, //0.175,
              min(
                (performance.now() / 1000 - sounds[buttonNote].sound.startedAt),
                0.15, //0.45,
              ),
            );
            // console.log("🦋 Fade length:", fade);
            // killFade
            sounds[buttonNote]?.sound.kill(quickFade ? fastFade : fade); // Kill a sound if it exists.
          }
          delete sounds[buttonNote];
        }
        // If hasSound is false in slide mode, the sound was already moved
        // to another key, so we don't need to kill anything

        if (buttonNote.toUpperCase() === song?.[songIndex][0]) {
          songIndex = (songIndex + 1) % song.length;
          songNoteDown = false;
          songShifting = true;
        }

        // Always clean up tonestack and trail
        delete tonestack[buttonNote]; // Remove this key from the notestack.
        trail[buttonNote] = 1;
        if (buttons[buttonNote]) buttons[buttonNote].down = false;
      }
    }
  });
  
  // After processing all key up events, check for orphaned sounds
  if (Object.keys(downs).length === 0) {
    cleanupOrphanedSounds(pens);
  }
}

// 📚 Library

function pictureAdd({ page, screen, wipe, write, line, ink, num }, note) {
  if (!picture?.pixels) return;
  note = note.toLowerCase();

  const letter = note.slice(1);
  // console.log(note);

  page(picture);
  // wipe("black");
  ink("yellow");
  // write(note, { center: "xy"}); // TOOD: Write's random values should be confined to the
  //       page and not the screen.
  // write("POW", { center: "xy" }); // TOOD: Write's random values should be confined to the

  const notesToCols = {
    c: [255, 0, 0], // Red
    "c#": [128, 128, 128], // Gray
    d: [0, 255, 0], // Green
    "d#": [0, 0, 0], // Black
    e: [0, 0, 255], // Blue
    f: [0, 255, 255], // Cyan
    "f#": [128, 0, 128], // Purple
    g: [255, 0, 255], // Magenta
    "g#": [255, 165, 0], // Orange
    a: [255, 255, 0], // Yellow
    "a#": [139, 69, 19], // Brown
    b: [255, 255, 255], // White
  };

  // write(note, num.randInt(128), num.randInt(128));

  // TODO: Rather than draw a whole box from 0, 0, to 128, 128, i wanna
  //       draw horizontal scanlines in aloop and randomly skip some.
  const color = notesToCols[letter];
  if (!color) {
    page(screen);
    return;
  }

  ink(...color, 32).box(0, 0, picture.width, picture.height);

  // for (let y = 0; y < 128; y += 1) {
  //   // Adjust step size (e.g., 2) for scanline spacing
  //   if (Math.random() > 0.3) {
  //     // 30% chance to skip a line
  //     ink(...notesToCols[letter], 32).box(0, y, 128, 1); // Draw a single horizontal line
  //   }
  // }

  // ink(undefined);
  // line();
  page(screen);
}

function pictureLines(
  { page, ink, wipe, screen, blur, box, line, plot, num, paintCount, sound, scroll },
  { amplitude, waveforms, audioReady },
) {
  if (!picture?.pixels) return;
  page(picture);

  // Blur instead of clearing for trail effect
  blur(0.5);

  // Scroll horizontally to the right
  scroll(1, 0);

  const safeAmplitude =
    typeof amplitude === "number" && Number.isFinite(amplitude) ? amplitude : 0;

  let waveformArray = [];
  if (Array.isArray(waveforms)) {
    waveformArray = waveforms.slice();
  } else if (waveforms && typeof waveforms.length === "number") {
    waveformArray = Array.from(waveforms);
  }

  if (waveformArray.length > 0) {
    waveformArray = waveformArray.map((value) =>
      typeof value === "number" && Number.isFinite(value) ? value : 0,
    );
  }

  const hasWaveformData = audioReady && waveformArray.length > 0;

  // Get the active notes from the sounds object
  const activeNotes = Object.keys(sounds).filter(
    (key) => sounds[key]?.sound && key,
  );

  if (activeNotes.length > 0) {
    // Get color from the first active note
    const firstNote = activeNotes[0];
    if (firstNote) {
      const color = colorFromNote(firstNote, num);

      // === WAVEFORM LINE (BACKGROUND) ===
      // Draw waveform line first in the background
      if (hasWaveformData && waveformArray.length > 16) {
        const step = picture.width / waveformArray.length;
        const centerY = picture.height / 2;

        for (let i = 1; i < waveformArray.length; i++) {
          const x1 = (i - 1) * step;
          const y1 = centerY + waveformArray[i - 1] * picture.height * 0.4;
          const x2 = i * step;
          const y2 = centerY + waveformArray[i] * picture.height * 0.4;

          // Draw waveform line with slight transparency
          ink(...color, 120).line(x1, y1, x2, y2);
        }
      }

      // === WAVEFORM BARS ===
      // Draw vertical bars based on waveform amplitude
      if (hasWaveformData && waveformArray.length > 16) {
        const barWidth = picture.width / waveformArray.length;
        const centerY = picture.height / 2;

        for (let i = 0; i < waveformArray.length; i++) {
          const x = i * barWidth;
          const barHeight = Math.abs(waveformArray[i]) * picture.height * 0.4;
          const y = centerY - barHeight / 2;

          // Draw vertical bar filling the full width
          ink(...color, 180).box(x, y, Math.ceil(barWidth), barHeight);
        }
      }

      // === HORIZONTAL STRIPES FOR EACH NOTE ===
      // Draw colored stripes across the screen for each active note
      const stripeHeight = picture.height / activeNotes.length;
      activeNotes.forEach((stripeNote, idx) => {
        if (!stripeNote) return;
        const stripeColor = colorFromNote(stripeNote, num);
        const stripeY = idx * stripeHeight;

        // Oscillate stripe position based on amplitude
        const oscillation = Math.sin(paintCount * 0.1 + idx) * safeAmplitude * 20;

        // Draw thin horizontal stripes with gaps, oscillating vertically
        for (let x = 0; x < picture.width; x += 8) {
          ink(...stripeColor, 60).box(x, stripeY + oscillation, 4, stripeHeight);
        }
      });
    }

    // === ACTIVE NOTES DISPLAY ===
    // Display each active note as a colored orb/circle
    const orbSize = Math.min(picture.width, picture.height) * 0.3; // Widened from 0.25
    const spacing = picture.width / (activeNotes.length + 1);

    activeNotes.forEach((note, index) => {
      if (!note) return;

      const color = colorFromNote(note, num);
      const x = spacing * (index + 1);
      const y = picture.height * 0.5; // Centered vertically
      const pulseSize = orbSize * (0.8 + safeAmplitude * 0.4); // Pulse with audio

      // Draw concentric circles for orb effect (wider glow)
      ink(...color, 100).box(x - pulseSize / 2, y - pulseSize / 2, pulseSize, pulseSize);
      ink(...color, 200).box(
        x - (pulseSize * 0.6) / 2,
        y - (pulseSize * 0.6) / 2,
        pulseSize * 0.6,
        pulseSize * 0.6,
      );
      ink(...color, 255).box(
        x - (pulseSize * 0.3) / 2,
        y - (pulseSize * 0.3) / 2,
        pulseSize * 0.3,
        pulseSize * 0.3,
      );

      // Convert note name to keyboard key for display
      // Handle both lower octave (c, d, e...) and upper octave (+c, +d, +e...)
  let displayKey = "";

      const mappedKey = noteToKeyboardKey(note);
      displayKey = mappedKey || note.toLowerCase();

      // Draw shadow (1px offset down and right)
      ink(0, 0, 0).write(displayKey, { center: "xy", x: x + 1, y: y });

      // Flicker between white and yellow
      const flickerColor = num.rand() > 0.5 ? [255, 255, 255] : [255, 255, 0];
      ink(...flickerColor).write(displayKey, { center: "xy", x, y: y - 1 });
    });

    // Apply blur for dreamy effect
    // blur(1.5);
  } else {
    // Clear to transparent when no notes
    wipe(0, 0);
  }

  page(screen);
}

function orderedByCount(obj) {
  return Object.keys(obj)
    .filter((key) => obj[key]?.hasOwnProperty("count"))
    .sort((a, b) => obj[a].count - obj[b].count);
}

function resetModeState() {
  tapped = undefined;
  tapIndex = 0;
  octave = STARTING_OCTAVE;
  // hold = sharps = flats = false;
}

// Initialize and/or lay out the UI buttons on the bottom of the display.
function setupButtons({ ui, screen, geo }) {
  const sampleRateText = getSampleRateText(undefined);
  const sampleRateLabel = sampleRateText ? MIDI_RATE_LABEL_TEXT : null;
  const cachedLayout = getCachedLayout(screen, {
    songMode: Boolean(song) && autopatConfig.showTrack !== false,
    pictureOverlay: paintPictureOverlay,
    rateLabel: sampleRateLabel,
    rateText: sampleRateText,
    sidePanelWidth: autopatConfig.sidePanelWidth,
  });
  const layout = cachedLayout.layout;
  const glyphMetrics = resolveMatrixGlyphMetrics();
  const rectsOverlap = (a, b) =>
    a && b &&
    a.x < b.x + b.w &&
    a.x + a.w > b.x &&
    a.y < b.y + b.h &&
    a.y + a.h > b.y;
  padsBaseKey = null;
  padsBase = null;
  const {
    buttonWidth,
    buttonHeight,
    topButtonY,
    buttonsPerRow,
    totalRows,
    margin,
    melodyButtonRect,
    splitLayout,
    leftOctaveX,
    rightOctaveX,
    notesPerSide,
    usableWidth,
    miniInputsHorizontal,
  } = layout;

  buttonNotes.forEach((label, i) => {
    let x, y;
    
    if (splitLayout) {
      // Split layout: first 12 notes on left, second 12 on right
      const isSecondOctave = i >= notesPerSide;
      const localIndex = isSecondOctave ? i - notesPerSide : i;
      const row = floor(localIndex / buttonsPerRow);
      const col = localIndex % buttonsPerRow;
      
      y = topButtonY + row * buttonHeight;
      if (isSecondOctave) {
        x = rightOctaveX + col * buttonWidth;
      } else {
        x = leftOctaveX + col * buttonWidth;
      }
    } else {
      // Normal grid layout
      const row = floor(i / buttonsPerRow);
      const col = i % buttonsPerRow;
      y = topButtonY + row * buttonHeight;
      const gridWidth = buttonsPerRow * buttonWidth;
      const availableWidth = usableWidth ?? screen.width;
      // Left-align grid when mini inputs are to the right, otherwise center
      const baseX = miniInputsHorizontal 
        ? margin
        : ceil(margin + Math.max(0, floor((availableWidth - margin * 2 - gridWidth) / 2)));
      x = baseX + col * buttonWidth;
    }
    
    const geometry = [x, y, buttonWidth, buttonHeight];
    if (!buttons[label]) {
      buttons[label] = new ui.Button(...geometry);
      buttons[label].id = `note-${label}`;  // Add identifier for debugging
    } else {
      buttons[label].box = new geo.Box(...geometry);
    }

    const noteLabelText = label.toUpperCase();
    const noteFont = buttonWidth >= 22 ? "unifont" : "MatrixChunky8";
    const noteGlyphWidth = noteFont === "unifont" ? 8 : glyphMetrics.width;
    const noteGlyphHeight = noteFont === "unifont" ? 16 : glyphMetrics.height;
    const noteLabelWidth = noteLabelText.length * noteGlyphWidth;
    const noteLabelBoundsDefault = {
      x: x + 2,
      y: y + 1,
      w: noteLabelWidth,
      h: noteGlyphHeight,
    };
    const noteLabelBoundsCentered = {
      x: x + buttonWidth / 2 - noteLabelWidth / 2,
      y: y + buttonHeight / 2 - noteGlyphHeight / 2,
      w: noteLabelWidth,
      h: noteGlyphHeight,
    };

    const keyLabelCandidate = noteToKeyboardKey(label) || label.toLowerCase();
    const keyLabelMatchesNote =
      keyLabelCandidate &&
      keyLabelCandidate.toUpperCase() === noteLabelText.toUpperCase();
    // Skip apostrophe key - it's too wide and cuts into narrow pads
    const keyLabel = keyLabelMatchesNote || keyLabelCandidate === "'" ? null : keyLabelCandidate;
    const keyFont = buttonWidth >= 22 ? "unifont" : "MatrixChunky8";
    const keyGlyphWidth = keyFont === "unifont" ? 8 : glyphMetrics.width;
    const keyGlyphHeight = keyFont === "unifont" ? 16 : glyphMetrics.height;

    let keyLabelWidth = 0;
    let keyBoundsDefault = null;
    let keyBoundsCentered = null;
    let keyOverlapsDefault = false;
    let keyOverlapsCentered = false;

    // Skip key label when both note and key are single letters on small buttons (homogenize)
    const skipKeyForHomogenization = 
      keyLabel && 
      noteLabelText.length === 1 && 
      keyLabel.length === 1 && 
      buttonWidth < 30;

    if (keyLabel && !skipKeyForHomogenization) {
      keyLabelWidth = keyLabel.length * keyGlyphWidth;
      const keyBottom = {
        x: x + buttonWidth - keyLabelWidth - 2,
        y: y + buttonHeight - keyGlyphHeight - 1,
        w: keyLabelWidth,
        h: keyGlyphHeight,
      };
      const keyTop = {
        x: x + buttonWidth - keyLabelWidth - 2,
        y: y + 1,
        w: keyLabelWidth,
        h: keyGlyphHeight,
      };

      keyBoundsDefault = rectsOverlap(keyBottom, noteLabelBoundsDefault) ? keyTop : keyBottom;
      keyOverlapsDefault = rectsOverlap(keyBoundsDefault, noteLabelBoundsDefault);

      keyBoundsCentered = rectsOverlap(keyBottom, noteLabelBoundsCentered) ? keyTop : keyBottom;
      keyOverlapsCentered = rectsOverlap(keyBoundsCentered, noteLabelBoundsCentered);
    }

    buttons[label].meta = {
      noteLabelText,
      noteFont,
      noteGlyphWidth,
      noteGlyphHeight,
      noteLabelBoundsDefault,
      noteLabelBoundsCentered,
      keyLabel: skipKeyForHomogenization ? null : keyLabel,
      keyFont,
      keyGlyphWidth,
      keyGlyphHeight,
      keyLabelWidth,
      keyBoundsDefault,
      keyBoundsCentered,
      keyOverlapsDefault,
      keyOverlapsCentered,
    };
  });

  if (song && !paintPictureOverlay && melodyButtonRect) {
    if (!melodyAliasBtn) {
      melodyAliasBtn = new ui.Button(
        melodyButtonRect.x,
        melodyButtonRect.y,
        melodyButtonRect.width,
        melodyButtonRect.height,
      );
      melodyAliasBtn.id = "melody-alias-button";
    } else {
      melodyAliasBtn.box = new geo.Box(
        melodyButtonRect.x,
        melodyButtonRect.y,
        melodyButtonRect.width,
        melodyButtonRect.height,
      );
    }
  } else {
    melodyAliasBtn = undefined;
  }

  if (!melodyAliasBtn) {
    melodyAliasDown = false;
    melodyAliasActiveNote = null;
    melodyAliasStartedNote = false;
  }
}

function buildWaveButton({ screen, ui, typeface }) {
  const isNarrow = screen.width < 200;
  const useSmallFont = isNarrow;
  const glyphWidth = useSmallFont 
    ? (matrixFont?.glyphs?.["0"]?.resolution?.[0] ?? 6)
    : (typeface?.glyphs?.["0"]?.resolution?.[0] ?? matrixFont?.glyphs?.["0"]?.resolution?.[0] ?? 6);
  
  // Shorten wave names for narrow screens
  const shortWaveNames = {
    sine: "sin",
    triangle: "tri",
    sawtooth: "saw",
    square: "sqr",
    noise: "noi",
    composite: "cmp",
    stample: "stp",
  };
  const displayWave = isNarrow ? (shortWaveNames[wave] || wave.slice(0, 3)) : wave;
  const waveWidth = displayWave.length * glyphWidth;
  const margin = isNarrow ? 2 : 4;
  waveBtn = new ui.Button(
    screen.width - waveWidth - 26 - margin * 2,
    0,
    waveWidth + margin * 2 + 5,
    10 + margin * 2 - 1 + 2,
  );
  waveBtn.id = "wave-button";
  waveBtn.isNarrow = isNarrow;
  waveBtn.displayWave = displayWave;
}

function buildOctButton({ screen, ui, typeface }) {
  const isNarrow = screen.width < 200;
  const useSmallFont = isNarrow;
  const glyphWidth = useSmallFont
    ? (matrixFont?.glyphs?.["0"]?.resolution?.[0] ?? 6)
    : (typeface?.glyphs?.["0"]?.resolution?.[0] ?? matrixFont?.glyphs?.["0"]?.resolution?.[0] ?? 6);
  const octWidth = octave.length * glyphWidth;
  const margin = isNarrow ? 2 : 4;
  octBtn = new ui.Button(
    screen.width - octWidth - 6 - margin * 2,
    0,
    octWidth + margin * 2 + 7,
    10 + margin * 2 - 1 + 2,
  );
  octBtn.id = "oct-button";
  octBtn.isNarrow = isNarrow;
}

// Build metronome controls and toggle buttons with responsive layout
// Calculates available space and shortens labels as needed to prevent overlap
function buildMetronomeButtons({ screen, ui, typeface, text }) {
  const btnHeight = SECONDARY_BAR_HEIGHT - 2;
  const btnY = SECONDARY_BAR_TOP + 1;
  const glyphWidth = typeface?.glyphs?.["0"]?.resolution?.[0] ?? matrixFont?.glyphs?.["0"]?.resolution?.[0] ?? 6;
  const glyphMetrics = resolveMatrixGlyphMetrics();
  
  // Calculate actual MIDI badge width (same logic as computeMidiBadgeTopMetrics)
  // Use stored sample rate or fallback to window.audioContext
  const sampleRate = storedSampleRate || (typeof window !== "undefined" ? window.audioContext?.sampleRate : null);
  const sampleRateText = getSampleRateText(sampleRate);
  const midiWidth = 1 * glyphMetrics.width; // "M" instead of "MIDI"
  const divWidth = 5; // divider (2px) + spacing (3px)
  const shortRate = sampleRateText ? sampleRateText.replace("Hz", "") : "";
  const rateWidth = shortRate.length * glyphMetrics.width;
  const fpsWidth = 6 * glyphMetrics.width; // FPS is 6 chars like "120fps"
  const totalMidiTextWidth = midiWidth + divWidth + rateWidth + divWidth + fpsWidth;
  const midiBadgeWidth = totalMidiTextWidth + MIDI_BADGE_PADDING_X + MIDI_BADGE_PADDING_RIGHT;
  
  // Left reserved = MIDI badge margin + badge width + tiny gap
  const leftReserved = MIDI_BADGE_MARGIN + midiBadgeWidth + 1;
  const rightMargin = 4;
  
  // Fixed elements: [-] [BPM] [+] - BPM is now 3 chars with minimal padding
  const minusBtnWidth = glyphWidth + TOGGLE_BTN_PADDING_X * 2;
  const plusBtnWidth = glyphWidth + TOGGLE_BTN_PADDING_X * 2;
  const bpmTextWidth = 3 * glyphWidth + TOGGLE_BTN_PADDING_X; // Reduced padding - only left side
  
  // Available width for metronome + toggle buttons
  const availableWidth = screen.width - leftReserved - rightMargin;
  
  // Calculate width needed for each label variant level
  const calcLabelWidth = (label) => {
    return (measureMatrixTextBoxWidth(label, { text }, screen.width) || (label.length * glyphWidth)) + TOGGLE_BTN_PADDING_X * 2;
  };
  
  // Fixed metronome elements width: [-] [BPM] [+] (BPM is the toggle button now)
  const metroFixedWidth = minusBtnWidth + 1 + bpmTextWidth + 1 + plusBtnWidth + TOGGLE_BTN_GAP;
  
  // Try different label lengths until everything fits
  let variantLevel = 0;
  let totalNeeded = Infinity;
  
  while (variantLevel < 3 && totalNeeded > availableWidth) {
    // Get labels at this variant level (no metro label needed - BPM display is the toggle)
    const slideLabel = LABEL_VARIANTS.slide[Math.min(variantLevel, LABEL_VARIANTS.slide.length - 1)];
    const roomLabel = LABEL_VARIANTS.room[Math.min(variantLevel, LABEL_VARIANTS.room.length - 1)];
    const glitchLabel = LABEL_VARIANTS.glitch[Math.min(variantLevel, LABEL_VARIANTS.glitch.length - 1)];
    const quickLabel = LABEL_VARIANTS.quick[Math.min(variantLevel, LABEL_VARIANTS.quick.length - 1)];
    
    const toggleWidths = calcLabelWidth(slideLabel) + calcLabelWidth(roomLabel) + 
                        calcLabelWidth(glitchLabel) + calcLabelWidth(quickLabel) + (3 * TOGGLE_BTN_GAP);
    
    totalNeeded = metroFixedWidth + toggleWidths;
    variantLevel++;
  }
  
  // Use the labels that fit (or shortest if nothing fits)
  const finalLevel = Math.min(variantLevel - 1, 2);
  secondaryBarLabels = {
    slide: LABEL_VARIANTS.slide[Math.min(finalLevel, LABEL_VARIANTS.slide.length - 1)],
    room: LABEL_VARIANTS.room[Math.min(finalLevel, LABEL_VARIANTS.room.length - 1)],
    glitch: LABEL_VARIANTS.glitch[Math.min(finalLevel, LABEL_VARIANTS.glitch.length - 1)],
    quick: LABEL_VARIANTS.quick[Math.min(finalLevel, LABEL_VARIANTS.quick.length - 1)],
  };
  
  // Calculate toggle button widths with chosen labels
  const toggleBtns = [
    { key: "slide", width: calcLabelWidth(secondaryBarLabels.slide) },
    { key: "room", width: calcLabelWidth(secondaryBarLabels.room) },
    { key: "glitch", width: calcLabelWidth(secondaryBarLabels.glitch) },
    { key: "quick", width: calcLabelWidth(secondaryBarLabels.quick) },
  ];
  const toggleTotalWidth = toggleBtns.reduce((sum, b) => sum + b.width, 0) + (toggleBtns.length - 1) * TOGGLE_BTN_GAP;
  
  // Position toggle buttons from right side
  let toggleX = screen.width - rightMargin - toggleTotalWidth;
  toggleBtns.forEach(({ key, width }) => {
    const btn = new ui.Button(toggleX, btnY, width, btnHeight);
    btn.id = `${key}-toggle`;
    if (key === "slide") slideBtn = btn;
    else if (key === "room") roomBtn = btn;
    else if (key === "glitch") glitchBtn = btn;
    else if (key === "quick") quickBtn = btn;
    toggleX += width + TOGGLE_BTN_GAP;
  });
  
  // Position metronome controls from left, after MIDI badge
  let btnX = leftReserved;
  
  // Build minus button
  bpmMinusBtn = new ui.Button(btnX, btnY, minusBtnWidth, btnHeight);
  bpmMinusBtn.id = "bpm-minus";
  btnX += minusBtnWidth + 1;
  
  // BPM display area
  const bpmDisplayX = btnX;
  btnX += bpmTextWidth + 1;
  
  // Build plus button
  bpmPlusBtn = new ui.Button(btnX, btnY, plusBtnWidth, btnHeight);
  bpmPlusBtn.id = "bpm-plus";
  
  // The BPM display itself is the metronome toggle button (no separate metro button)
  metroBtn = new ui.Button(bpmDisplayX, btnY, bpmTextWidth, btnHeight);
  metroBtn.id = "metro-toggle";
}

// Build toggle buttons - now handled by buildMetronomeButtons for unified layout
function buildToggleButtons({ screen, ui, typeface, text }) {
  // Toggle buttons are now built in buildMetronomeButtons for responsive layout
  // This function is kept for backwards compatibility but does nothing
}

let primaryColor = [0, 0, 0];
let currentAverage = [0, 0, 0];

let secondaryColor = [0, 0, 0];
let lastAverage = [0, 0, 0];
let lastActive = null;
let activeStr;

function updateTheme({ num }) {
  let colors = ["red"];
  const active = orderedByCount(sounds);
  activeStr = active.join("");

  if (activeStr.length > 0 && activeStr !== lastActive) {
    lastActive = activeStr;
    colors = active.map((note) => colorFromNote(note, num));
    const average = averageRGB(colors);
    lastAverage = currentAverage;
    currentAverage = average;
  }
  // lerp the primary color to the current average.
}

// Average an array of [[r, g, b], [r, g, b]] values.
function averageRGB(colors) {
  return colors
    .reduce(
      (acc, color) => {
        return [acc[0] + color[0], acc[1] + color[1], acc[2] + color[2]];
      },
      [0, 0, 0],
    )
    .map((sum) => round(sum / colors.length));
}

export { boot, paint, sim, act, configureAutopat };
