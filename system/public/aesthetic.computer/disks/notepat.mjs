// Notepat, 2024.6.26.23.17.58.736
// Tap the pads to play musical notes, or use the keyboard keys.

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
  - [🍉] Add reverb that's only activated for the notepat sounds.
       (Sound tagging?)
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
  "composite", // 4
  "sample", // 5
];
let waveIndex = 0; // 0;
const STARTING_WAVE = wavetypes[waveIndex]; //"sine";
let wave = STARTING_WAVE;
// let hold = false;
let slide = false;
let quickFade = false;
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
//let sharps = false,
//  flats = false;
const notes = "cdefgab" + "vswrq" + "hijklmn" + "tyuop"; // hold shift on C D F G A for sharps.
const edges = "zx;']"; // below and above the octave
//                              cdefgab (next ovtave)
//                       // or alt on   D E G A B for flats
// This is a notes -> keys mapping, that uses v for c#

const TOP_BAR_BOTTOM = 21;
const TRACK_HEIGHT = 25;
const TRACK_GAP = 6;
const MINI_KEYBOARD_HEIGHT = 16;
const MINI_KEYBOARD_SPACING = 6;
const QWERTY_MINIMAP_HEIGHT = 28;
const QWERTY_MINIMAP_SPACING = 6;

const MIDI_BADGE_TEXT = "USB MIDI";
const MIDI_BADGE_PADDING_X = 4;
const MIDI_BADGE_PADDING_Y = 2;
const MIDI_BADGE_MARGIN = 6;

const MELODY_ALIAS_BASE_SIDE = 72;
const MELODY_ALIAS_MIN_SIDE = 56;
const MELODY_ALIAS_MARGIN = 6;

const NOTE_TO_KEYBOARD_KEY = {
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
};

const KEYBOARD_TO_NOTE = Object.fromEntries(
  Object.entries(NOTE_TO_KEYBOARD_KEY).map(([note, key]) => [key, note]),
);

const QWERTY_LAYOUT_ROWS = [
  ["q", "w", "e", "r", "t", "y", "u", "i", "o", "p"],
  ["a", "s", "d", "f", "g", "h", "j", "k", "l"],
  ["z", "x", "c", "v", "b", "n", "m"],
];

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

let upperOctaveShift = 0, // Set by <(,) or >(.) keys.
  lowerOctaveShift = 0;

const attack = 0.001; // Faster onset for responsive play.
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

function colorFromNote(note, num) {
  let oct = parseInt(octave);
  if (note.startsWith("+")) {
    note = note.slice(1);
    oct += 1;
  }
  // const noteIndex = noteOrder.indexOf(note);
  // console.log("Note Index:", noteIndex, "Octave:", oct);
  // const hue = (noteIndex / noteOrder.length * 360);
  // const satOctMod = (oct - 4) * 10;
  // const saturation = 80 + satOctMod;
  // const lightOctMod = (oct - 4) * 15;
  // const lightness = 50 + lightOctMod;
  // const color = num.hslToRgb(hue, saturation, lightness);
  let color = notesToColorsFinal?.[oct + note]?.slice(); // || "yellow";

  if (!color) {
    color = notesToColorsFinal[4 + note]?.slice() || [0, 255, 0];
    const octMod = oct - 4;
    color[0] = num.clamp(color[0] + 65 * octMod, 0, 255);
    color[1] = num.clamp(color[1] + 65 * octMod, 0, 255);
    color[2] = num.clamp(color[2] + 65 * octMod, 0, 255);
  }

  return color; //[0, 0, 0];
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

let scope = 32; // Reduced from 64 for better visualizer performance
// let scopeTrim = 0;

let projector = false;

const trail = {};

let lastActiveNote;
let transposeOverlay = false;
let transposeOverlayFade = 0;
let paintTransposeOverlay = false;
let paintPictureOverlay = false;
// let paintPictureOverlay = true;

// let qrcells;

let waveBtn, octBtn;
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

let picture;
let matrixFont; // MatrixChunky8 font for note letters

function boot({
  params,
  api,
  colon,
  ui,
  screen,
  fps,
  typeface,
  hud,
  net,
  painting,
  sound,
}) {
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

  if (song) {
    let x = 0;
    const glyphWidth = typeface.glyphs["0"].resolution[0];
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

  const wavetypes = ["square", "sine", "triangle", "sawtooth", "noise-white"];
  wave = wavetypes.indexOf(colon[0]) > -1 ? colon[0] : wave;
  // slide = true; // colon[0] === "slide" || colon[1] === "slide";

  buildWaveButton(api);
  buildOctButton(api);

  const newOctave =
    parseInt(colon[0]) || parseInt(colon[1]) || parseInt(colon[2]);

  if (newOctave) {
    STARTING_OCTAVE = newOctave.toString();
    octave = STARTING_OCTAVE;
  }

  setupButtons(api);
}

function sim({ sound, simCount, num }) {
  sound.speaker?.poll();

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

  Object.keys(trail).forEach((note) => {
    trail[note] -= 0.0065;
    if (trail[note] <= 0) delete trail[note];
  });

  const active = orderedByCount(sounds);

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

function computeMidiBadgeMetrics(screen, glyphMetrics = resolveMatrixGlyphMetrics()) {
  const textWidth = MIDI_BADGE_TEXT.length * glyphMetrics.width;
  const width = textWidth + MIDI_BADGE_PADDING_X * 2;
  const height = glyphMetrics.height + MIDI_BADGE_PADDING_Y * 2;
  const x = screen.width - width - MIDI_BADGE_MARGIN;
  const y = screen.height - height - MIDI_BADGE_MARGIN;

  return { x, y, width, height };
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
  { songMode = false, pictureOverlay = false, midiMetrics } = {},
) {
  const badgeMetrics = midiMetrics ?? computeMidiBadgeMetrics(screen);
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

  const buttonsPerRow = 4;
  const totalButtons = buttonNotes.length;
  const totalRows = ceil(totalButtons / buttonsPerRow);
  const margin = 2;

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
    };
  }

  const hudReserved = TOP_BAR_BOTTOM;
  const trackHeight = songMode ? TRACK_HEIGHT : 0;
  const trackSpacing = songMode ? TRACK_GAP : 0;
  const qwertyReserved = songMode
    ? QWERTY_MINIMAP_HEIGHT + QWERTY_MINIMAP_SPACING
    : 0;
  const keyboardReserved =
    MINI_KEYBOARD_HEIGHT + MINI_KEYBOARD_SPACING + qwertyReserved;
  const reservedTop = hudReserved + trackHeight + trackSpacing + keyboardReserved;

  const widthLimit = ceil((screen.width - margin * 2) / buttonsPerRow);
  const minButtonSize = 20;
  const maxButtonSize = 48;

  let buttonWidth = min(maxButtonSize, widthLimit);
  buttonWidth = max(minButtonSize, buttonWidth);
  buttonWidth = min(buttonWidth, widthLimit);
  let buttonHeight = buttonWidth;

  const available = max(0, screen.height - bottomPadding - reservedTop);

  if (available < totalRows * buttonHeight) {
    const maxPerRow = max(minButtonSize, floor(available / totalRows));
    buttonHeight = maxPerRow;
    buttonWidth = min(buttonWidth, maxPerRow, widthLimit);
  } else {
    buttonWidth = min(buttonWidth, widthLimit);
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
  };
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
  const resampledWaveforms = waveformsAvailable
    ? help.resampleArray(rawWaveformsLeft, scopeSamples)
    : [];
  const sanitizedWaveforms =
    resampledWaveforms.length > 0
      ? resampledWaveforms.map((value) =>
          typeof value === "number" && Number.isFinite(value) ? value : 0,
        )
      : [];
  const waveformsForBars =
    sanitizedWaveforms.length > 0
      ? sanitizedWaveforms
      : new Array(scopeSamples).fill(0);
  const waveformsForOverlay =
    sanitizedWaveforms.length > 0 ? sanitizedWaveforms : [];
  const audioReady =
    waveformsAvailable &&
    typeof amplitudeRaw === "number" &&
    Number.isFinite(amplitudeRaw);
  const matrixGlyphMetrics = resolveMatrixGlyphMetrics(typeface);
  let bg;

  if (!tap) {
    bg = active.length > 0 ? [50, 50, 255] : "blue";

    if (perc) {
      bg = perc;
      perc = null;
    }
  } else {
    bg = active.length > 0 ? [0, 0, 180] : "darkblue";
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
  wipe(bg);

  if (slide) {
    ink(undefined).write("slide", { right: 4, top: 24 });
  }

  if (quickFade) {
    ink(undefined).write("quick", { left: 6, top: 24 });
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

  const trackHeight = song ? TRACK_HEIGHT : 0;
  const trackY = song ? TOP_BAR_BOTTOM : null;

  if (song) {
    const glyphWidth = typeface.glyphs["0"].resolution[0];
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
  
  // Draw tiny piano layout (always visible, not just in song mode)
  // Position it dynamically based on available space
  // Position piano below track if in song mode, otherwise higher and left
  let pianoY, pianoStartX;
  if (song) {
    const effectiveTrackY = trackY ?? TOP_BAR_BOTTOM;
    pianoY = effectiveTrackY + trackHeight + 2; // Just below the track
  } else {
    pianoY = TOP_BAR_BOTTOM; // Below HUD label when no track
  }
  
  const whiteKeyWidth = 7;
  const whiteKeyHeight = MINI_KEYBOARD_HEIGHT;
  const blackKeyWidth = 5;
  const blackKeyHeight = 10;
  
  // Two octaves: 14 white keys total
  const totalWhiteKeys = 14;
  const pianoWidth = totalWhiteKeys * whiteKeyWidth;
  
  if (song) {
    pianoStartX = screen.width - pianoWidth - 2; // Align with right edge (2px margin)
  } else {
    pianoStartX = 58; // Align with visualizer start when no track
  }
  
  // Two octaves: notes in order
  const whiteKeys = ['C', 'D', 'E', 'F', 'G', 'A', 'B', '+C', '+D', '+E', '+F', '+G', '+A', '+B'];
  const blackKeys = [
    {note: 'C#', afterWhite: 0},
    {note: 'D#', afterWhite: 1},
    {note: 'F#', afterWhite: 3},
    {note: 'G#', afterWhite: 4},
    {note: 'A#', afterWhite: 5},
    {note: '+C#', afterWhite: 7},
    {note: '+D#', afterWhite: 8},
    {note: '+F#', afterWhite: 10},
    {note: '+G#', afterWhite: 11},
    {note: '+A#', afterWhite: 12}
  ];
  
  // Draw white keys
  whiteKeys.forEach((note, index) => {
    const x = pianoStartX + index * whiteKeyWidth;
    const noteKey = note.toLowerCase();
    const isCurrentNote = song && note === song?.[songIndex][0];
    const isActivePlaying = sounds[noteKey] !== undefined;
    const isCurrentAndPlaying = isCurrentNote && isActivePlaying;
    const isCurrentAwaiting = isCurrentNote && !isActivePlaying;

    if (isCurrentAndPlaying) {
      ink(0, 255, 234).box(x, pianoY, whiteKeyWidth - 1, whiteKeyHeight); // Bright cyan for current note sounding
    } else if (isCurrentAwaiting) {
      ink(0, 120, 140).box(x, pianoY, whiteKeyWidth - 1, whiteKeyHeight); // Muted teal for expected note
    } else if (isActivePlaying) {
      ink(255, 255, 0).box(x, pianoY, whiteKeyWidth - 1, whiteKeyHeight); // Yellow for other active notes
    } else {
      ink(200, 200, 200).box(x, pianoY, whiteKeyWidth - 1, whiteKeyHeight); // Default white
    }
    ink(100, 100, 100).box(x, pianoY, whiteKeyWidth - 1, whiteKeyHeight, "outline"); // Border
  });
  
  // Draw black keys on top
  blackKeys.forEach(({note, afterWhite}) => {
    const x = pianoStartX + afterWhite * whiteKeyWidth + whiteKeyWidth - blackKeyWidth / 2;
    const noteKey = note.toLowerCase();
    const isCurrentNote = song && note === song?.[songIndex][0];
    const isActivePlaying = sounds[noteKey] !== undefined;
    const isCurrentAndPlaying = isCurrentNote && isActivePlaying;
    const isCurrentAwaiting = isCurrentNote && !isActivePlaying;

    if (isCurrentAndPlaying) {
      ink(0, 220, 210).box(x, pianoY, blackKeyWidth, blackKeyHeight); // Bright teal for current sounding
    } else if (isCurrentAwaiting) {
      ink(0, 100, 110).box(x, pianoY, blackKeyWidth, blackKeyHeight); // Muted teal for expected
    } else if (isActivePlaying) {
      ink(200, 200, 0).box(x, pianoY, blackKeyWidth, blackKeyHeight); // Dark yellow for other actives
    } else {
      ink(40, 40, 40).box(x, pianoY, blackKeyWidth, blackKeyHeight); // Default black
    }
  });

  if (song) {
    const currentKeyLetter = noteToKeyboardKey(song?.[songIndex]?.[0]);
    const nextKeyLetter = noteToKeyboardKey(song?.[songIndex + 1]?.[0]);
    const activeKeyLetters = new Set(
      Object.keys(sounds)
        .map((activeNote) => noteToKeyboardKey(activeNote))
        .filter(Boolean),
    );

    const qKeyWidth = 9;
    const qKeyHeight = 8;
    const qKeySpacing = 1;
    const qwertyStartX = pianoStartX;
    const qwertyStartY = pianoY + whiteKeyHeight + QWERTY_MINIMAP_SPACING;

    QWERTY_LAYOUT_ROWS.forEach((row, rowIndex) => {
      const rowOffset =
        rowIndex === 0
          ? 0
          : rowIndex === 1
          ? (qKeyWidth + qKeySpacing) / 2
          : qKeyWidth;
      const y = qwertyStartY + rowIndex * (qKeyHeight + qKeySpacing);

      row.forEach((keyLetter, keyIndex) => {
        const x =
          qwertyStartX +
          rowOffset +
          keyIndex * (qKeyWidth + qKeySpacing);

        const mappedNote = keyboardKeyToNote(keyLetter);
        const isMapped = Boolean(mappedNote);
        const isCurrentKey = keyLetter === currentKeyLetter;
        const isActiveKey = activeKeyLetters.has(keyLetter);
        const isCurrentAndActive = isCurrentKey && isActiveKey;
        const isNextKey = keyLetter === nextKeyLetter;

        let fillColor;
        if (isCurrentAndActive) {
          fillColor = [0, 255, 234, 220];
        } else if (isCurrentKey) {
          fillColor = [0, 120, 140, 200];
        } else if (isActiveKey) {
          fillColor = [255, 255, 0, 210];
        } else if (isNextKey) {
          fillColor = [0, 200, 220, 120];
        } else if (isMapped) {
          fillColor = [40, 40, 40, 180];
        } else {
          fillColor = [20, 20, 20, 140];
        }

        ink(...fillColor).box(x, y, qKeyWidth, qKeyHeight);
        ink(100, 100, 100, 220).box(x, y, qKeyWidth, qKeyHeight, "outline");

        const label = keyLetter.toUpperCase();
        const glyphWidth = matrixGlyphMetrics.width;
        const glyphHeight = matrixGlyphMetrics.height;
        const labelWidth = label.length * glyphWidth;
        const labelX = x + (qKeyWidth - labelWidth) / 2;
        const labelY = y + (qKeyHeight - glyphHeight) / 2;

        let textColor;
        if (isCurrentAndActive || isActiveKey) {
          textColor = [0, 0, 0, 240];
        } else if (isMapped) {
          textColor = [230, 230, 230, 230];
        } else {
          textColor = [150, 150, 150, 200];
        }

        ink(...textColor).write(
          label,
          { x: labelX, y: labelY },
          undefined,
          undefined,
          false,
          "MatrixChunky8",
        );

      });
    });
  }

  const initialBadgeMetrics = computeMidiBadgeMetrics(screen, matrixGlyphMetrics);
  const layout = getButtonLayoutMetrics(screen, {
    songMode: Boolean(song),
    pictureOverlay: paintPictureOverlay,
    midiMetrics: initialBadgeMetrics,
  });

  const midiBadgeMetrics = layout.midiBadge ?? initialBadgeMetrics;
  const melodyButtonRect = layout.melodyButtonRect;

  if (melodyAliasBtn && melodyButtonRect && melodyAliasBtn.box) {
    melodyAliasBtn.box.x = melodyButtonRect.x;
    melodyAliasBtn.box.y = melodyButtonRect.y;
    melodyAliasBtn.box.w = melodyButtonRect.width;
    melodyAliasBtn.box.h = melodyButtonRect.height;
  }

  const drawMidiBadge = (
    metrics,
    connected,
    {
      connectedBackground = [0, 0, 0, 160],
      disconnectedBackground = [0, 0, 0, 70],
      connectedText = [255, 165, 0],
      disconnectedText = [140, 140, 140, 200],
    } = {},
  ) => {
    if (!metrics) return;

    const { x, y, width, height } = metrics;
    const bgColor = connected ? connectedBackground : disconnectedBackground;
    const textColor = connected ? connectedText : disconnectedText;

    ink(...bgColor).box(x, y, width, height);

    ink(...textColor).write(
      MIDI_BADGE_TEXT,
      { x: x + MIDI_BADGE_PADDING_X, y: y + MIDI_BADGE_PADDING_Y },
      undefined,
      undefined,
      false,
      "MatrixChunky8",
    );
  };

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
      audioBadgeX = screen.width - audioBadgeWidth - 10;
      ink(180, 0, 0, 240).box(audioBadgeX, audioBadgeY, audioBadgeWidth, audioBadgeHeight);
      ink(255, 255, 0).write("AUDIO OFF", audioBadgeX + 8, audioBadgeY + 4);
    }

    drawMidiBadge(midiBadgeMetrics, midiConnected, {
      connectedBackground: [0, 0, 0, 180],
      disconnectedBackground: [0, 0, 0, 90],
    });
  } else if (!paintPictureOverlay) {
    const sy = 3;
    const sh = 15; // screen.height - sy;

    // const right = (54 + 120);
    // const leftOfWav = wavBtn.box.x
    const availableWidth = waveBtn.box.x - 54;

    sound.paint.bars(
      api,
      amplitude,
      waveformsForBars,
      54, //0,
      sy, //sy,
      availableWidth,
      //screen.width, // width
      sh, // height
      [255, 0, 0, 255],
      { primaryColor, secondaryColor },
    );

    // Draw active notes to the right of the mini piano (not in song mode)
    if (!song) {
      const activeNotes = orderedByCount(sounds);
      if (activeNotes.length > 0) {
        // Position to the right of the piano (piano is 14 white keys * 7px = 98px wide)
        const pianoWidth = 98;
        const pianoStartX = 58;
        const notesStartX = pianoStartX + pianoWidth + 4; // 4px gap after piano
        const pianoY = 21;
        
        // Draw a subtle dark background for the active notes area
        const notesWidth = activeNotes.length * 14 + 8;
        ink(0, 0, 0, 128).box(notesStartX, pianoY, notesWidth, 16);
        
        // Draw each active note
        activeNotes.forEach((note, index) => {
          const noteX = notesStartX + 4 + index * 14;
          ink(255, 255, 0).write(note.toUpperCase(), noteX, pianoY + 4);
        });
      }
    }

    // ink("yellow").write(scope, 56 + 120 + 2, sy + 3);
    // ink("pink").write(scopeTrim, 6 + 18, sy + sh + 3);
    // ink("cyan").write(sound.sampleRate, 6 + 18 + 20, sy + sh + 3);

    const rightEdge = waveBtn?.box?.x ?? screen.width - 8;
    const audioBadgeWidth = 66;
    const audioBadgeHeight = max(9, sh - 4);
    const audioBadgeY = sy + 2;
    let audioBadgeX;

    if (!audioReady) {
      audioBadgeX = min(
        max(54, rightEdge - audioBadgeWidth - 4),
        screen.width - audioBadgeWidth - 8,
      );
      ink(180, 0, 0, 240).box(audioBadgeX, audioBadgeY, audioBadgeWidth, audioBadgeHeight);
      ink(255, 255, 0).write("AUDIO OFF", audioBadgeX + 8, audioBadgeY + 3);
    }

    if (song && melodyAliasBtn && melodyAliasBtn.box && melodyButtonRect) {
      const baseGlyphWidth = typeface.glyphs["0"].resolution[0];
      const baseGlyphHeight = typeface.glyphs["0"].resolution[1];

      melodyAliasBtn.paint((btn) => {
        const rect = btn.box;
        const isActive = melodyAliasDown || songNoteDown;
        const backgroundColor = isActive ? [0, 120, 140, 220] : [0, 0, 0, 150];
        const borderColor = isActive ? [0, 255, 234, 240] : [0, 200, 220, 180];

        ink(...backgroundColor).box(rect.x, rect.y, rect.w, rect.h);
        ink(...borderColor).box(rect.x, rect.y, rect.w, rect.h, "outline");

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

    drawMidiBadge(midiBadgeMetrics, midiConnected);
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
      // ink("white", 64).box(btn.box);
      ink("orange").line(
        btn.box.x + btn.box.w,
        btn.box.y + 3,
        btn.box.x + btn.box.w,
        btn.box.y + btn.box.h - 1,
      );
      ink(btn.down ? "yellow" : "orange");
      write(wave, { right: 27, top: 6 });
    });

    octBtn?.paint((btn) => {
      if (btn.down) {
        ink(40, 40, 100);
      } else {
        ink(octaveTheme[octave], 196);
      }
      box(btn.box.x, btn.box.y + 3, btn.box.w - 4, btn.box.h - 3);
      // ink("white", 64).box(btn.box);
      ink(btn.down ? "yellow" : "pink");
      write(octave, { right: 8, top: 6 });
    });
  }

  if (tap) {
    ink("gray").write(keys, screen.width / 2 - tapIndex * 6, screen.height / 2);
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
    if (!tap && !song) {
      // Write all the active keys.
      ink("lime");
      write(
        active.map((key) => sounds[key]?.note.toUpperCase()).join(" "),
        6,
        20,
      );
    } else if (tap) {
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

    // 🎵 Draw grid outlines for melody notes
    if (song) {
      // Collect all unique notes used in the melody
      const melodyNotes = new Set();
      song.forEach(([note]) => melodyNotes.add(note.toLowerCase()));
      
      // Draw outline for each melody note button
      buttonNotes.forEach((note) => {
        if (buttons[note] && melodyNotes.has(note.toUpperCase())) {
          const btn = buttons[note];
          ink("cyan", 64).box(btn.box, "outline"); // Subtle cyan outline
        }
      });
    }

    // Paint all the keyboard buttons.
    buttonNotes.forEach((note, index) => {
      if (buttons[note]) {
        buttons[note].paint((btn) => {
          let color;
          let isBlocked = false;

          // In song mode, check if this note is blocked (not the current note)
          if (song && note.toUpperCase() !== song?.[songIndex][0]) {
            isBlocked = true;
            color = "black"; // Blocked notes are black
          } else if ((!slide && btn.down) || (btn.down && slide)) {
            // If this button is pressed down.
            color = "maroon";
          } else {
            const outOctave =
              parseInt(octave) +
              (note.startsWith("+") ? upperOctaveShift : lowerOctaveShift);
            // console.log("Output octave:", outOctave);
            color = note.indexOf("#") === -1 ? octaveTheme[outOctave] : "gray";
          }

          // Remove red highlighting - we just use the boxes now
          // (keeping this section empty for clarity)

          if (!projector) {
            ink(color, isBlocked ? 128 : 196).box(btn.box); // Blocked notes are darker
          } else {
            ink(color, 48).box(btn.box); // One solid colored box per note.
            // ink("white", 32).box(btn.box, "inline"); // One solid colored box per note.
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
            ink("maroon", max(1, trail[note] * 96)).box(
              btn.box.x + btn.box.w / 2,
              btn.box.y + btn.box.h / 2,
              trail[note] * btn.box.w,
              "center",
            );
          }

          // 🎵 Note label
          const glyphWidth = typeface.glyphs["0"].resolution[0];
          const glyphHeight = typeface.glyphs["0"].resolution[1];
          
          if (song) {
            // In song mode, only show note labels for current/next notes, centered
            const isCurrentNote = note.toUpperCase() === song?.[songIndex][0];
            const isNextNote = note.toUpperCase() === song?.[songIndex + 1]?.[0];
            
            if (isCurrentNote || isNextNote) {
              const noteLabel = note.toUpperCase();
              const labelWidth = noteLabel.length * glyphWidth;
              const centerX = btn.box.x + btn.box.w / 2 - labelWidth / 2;
              const centerY = btn.box.y + btn.box.h / 2 - glyphHeight / 2;
              
              // Use darker color that's visible on yellow
              ink(0, 0, 50).write(noteLabel, centerX, centerY);
            }
            // Don't show labels for blocked notes
          } else {
            // Normal mode - show label in top-left corner
            ink("white").write(note.toUpperCase(), btn.box.x + 2, btn.box.y + 1);
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

          // 🎵 Paint note label OVER boxes (in song mode only)
          if (song) {
            const isCurrentNote = note.toUpperCase() === song?.[songIndex][0];
            const isNextNote = note.toUpperCase() === song?.[songIndex + 1]?.[0];
            
            if (isCurrentNote) {
              // Show the note letter in top-left using MatrixChunky8
              const noteLetter = note.toUpperCase();
              ink("white").write(noteLetter, { x: btn.box.x + 1, y: btn.box.y }, undefined, undefined, false, "MatrixChunky8");
              
              // Show the word/syllable to sing for current note (centered)
              let label = song[songIndex][1];
              // Remove hyphens for display
              label = label.replace(/-/g, '');
              const labelWidth = label.length * glyphWidth;
              const centerX = btn.box.x + btn.box.w / 2 - labelWidth / 2;
              const centerY = btn.box.y + btn.box.h / 2 - glyphHeight / 2;
              
              // Use white for labels
              ink("white").write(label, centerX, centerY);
            } else if (isNextNote) {
              // Use white with fade based on progress
              const fadeOpacity = Math.floor(songProgress * 255);
              if (fadeOpacity > 0) {
                // Show the note letter in top-left using MatrixChunky8
                const noteLetter = note.toUpperCase();
                ink("white", fadeOpacity).write(noteLetter, { x: btn.box.x + 1, y: btn.box.y }, undefined, undefined, false, "MatrixChunky8");
                
                // Show the word/syllable to sing for next note (centered)
                let label = song[songIndex + 1][1];
                // Remove hyphens for display
                label = label.replace(/-/g, '');
                const labelWidth = label.length * glyphWidth;
                const centerX = btn.box.x + btn.box.w / 2 - labelWidth / 2;
                const centerY = btn.box.y + btn.box.h / 2 - glyphHeight / 2;
                
                ink("white", fadeOpacity).write(label, centerX, centerY);
              }
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

          // Paint keyboard shortcuts (if they differ from the note)
          // Hide in song mode
          if (!song) {
            const keyLabel = noteToKeyboardKey(note);
            if (keyLabel && keyLabel !== note.toLowerCase()) {
              ink("white", 96).write(
                keyLabel,
                btn.box.x + 2,
                btn.box.y + 10,
              );
            }
          }
        });
      }
    });

    if (!projector) {
      const leftEdge = ceil(layout.margin);
      const rightEdge = round(leftEdge + layout.buttonWidth * layout.buttonsPerRow);
      const topEdge = round(layout.topButtonY);
      const bottomEdge = round(layout.topButtonY + layout.buttonHeight * layout.totalRows);
      const gridAlpha = 40;

      for (let col = 0; col <= layout.buttonsPerRow; col += 1) {
        const x = round(layout.margin + col * layout.buttonWidth);
        ink(255, 255, 255, gridAlpha).line(x, topEdge, x, bottomEdge);
      }

      for (let row = 0; row <= layout.totalRows; row += 1) {
        const y = round(layout.topButtonY + row * layout.buttonHeight);
        ink(255, 255, 255, gridAlpha).line(leftEdge, y, rightEdge, y);
      }
    }
  }
}

let anyDown = true;

const percDowns = {};

function act({
  event: e,
  sound: { synth, speaker, play, freq, midi: midiUtil },
  num,
  pens,
  hud,
  screen,
  painting,
  api,
}) {
  if (e.is("reframed")) {
    setupButtons(api);
    buildWaveButton(api);
    buildOctButton(api);
    // Resize picture to quarter resolution (half width, half height)
    const resizedPictureWidth = Math.max(1, Math.floor(screen.width / 2));
    const resizedPictureHeight = Math.max(1, Math.floor(screen.height / 2));
    picture = painting(resizedPictureWidth, resizedPictureHeight, ({ wipe }) => {
      wipe("gray");
    });
  }

  if (e.is("keyboard:down:.") && !e.repeat) {
    upperOctaveShift += 1;
  }

  if (e.is("keyboard:down:,") && !e.repeat) {
    upperOctaveShift -= 1;
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
        console.log(tone, index);
      });
    }
  }

  //  if (
  //    e.is("keyboard:down:control") ||
  //    (e.is("keyboard:down:capslock") && !e.repeat)
  //  ) {
  //    lowerOctaveShift += 1;
  //  }

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

  // 🚨 PANIC BUTTON: Press Escape to stop all stuck sounds
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

  // TODO: This (makePerc) can be factored out of the event loop. 24.11.12.02.10
  const pc = "maroon";
  const lowvol = 0.95;
  const hivol = 1;
  const makePerc = (hz) => {
    synth({
      type: "triangle",
      tone: hz / 2,
      duration: 0.01,
      attack: 0,
      volume: hivol / 2,
    });

    synth({ type: "sawtooth", tone: hz, duration: 0.0025, volume: hivol });

    synth({
      type: "square",
      tone: hz / 4,
      duration: 0.005,
      volume: lowvol,
      decay: 0.999,
    });
  };

  function makeNoteSound(tone, velocity = 127) {
    const velocityRatioRaw = velocity === undefined ? 1 : velocity / 127;
    const velocityRatio = num?.clamp
      ? num.clamp(velocityRatioRaw, 0, 1)
      : Math.max(0, Math.min(1, velocityRatioRaw));
    const minVelocityVolume = 0.05; // Keep a subtle floor so very light taps still play.
    const volumeScale = minVelocityVolume + (1 - minVelocityVolume) * velocityRatio;

    if (wave === "sample") {
      // synth({
      //   type: "sine",
      //   attack,
      //   tone: freq(tone),
      //   duration: 0.5,
      //   volume: toneVolume / 2, // / 32,
      // });

      return play(startupSfx, {
        volume: volumeScale,
        pitch: freq(tone),
        // pan: -0.5 + num.randIntRange(0, 100) / 100,
      });

      // return synth({
      //   type: wave,
      //   attack,
      //   // decay,
      //   tone,
      //   duration: "🔁",
      //   volume: toneVolume,
      // });
    } else if (wave === "composite") {
      // console.log("🐦 Composite tone:", tone);

      let toneA, toneB, toneC, toneD, toneE;
      const baseFreq = freq(tone);

      toneA = synth({
        type: "sine",
        // attack: 0.5,//attack * 8,
        attack: 0.0025,
        decay: 0.9,
        tone: baseFreq,
        // tone: baseFreq + 280 + num.randIntRange(-10, 20),
        // duration: 0.18,
        duration: "🔁",
        volume: toneVolume * volumeScale,
      });

      // TODO: Can't update straight after triggering.
      // setTimeout(() => {
      //   toneA.update({ tone: baseFreq, duration: 0.02 });
      // }, 10);

      toneB = synth({
        type: "sine",
        // attack: attack * 8,
        attack: 0.0025,
        // decay,
        tone: baseFreq + 9 + num.randIntRange(-1, 1), //+ 8, //num.randIntRange(-5, 5),
        duration: "🔁",
        volume: (toneVolume / 3) * volumeScale, // / 16,
      });

      toneC = synth({
        type: "sawtooth",
        attack,
        decay: 0.9,
        tone: baseFreq + num.randIntRange(-6, 6),
        duration: 0.15 + num.rand() * 0.05,
        volume: (toneVolume / 48) * volumeScale, // / 32,
      });

      // TODO: One-shot sounds and samples need to be 'killable'.

      toneD = synth({
        type: "triangle",
        attack: 0.999, //attack * 8,
        // decay,
        tone: baseFreq + 8 + num.randIntRange(-5, 5),
        duration: "🔁",
        volume: (toneVolume / 32) * volumeScale,
      });

      toneE = synth({
        type: "square",
        attack: 0.05, //attack * 8,
        // decay,
        tone: baseFreq + num.randIntRange(-10, 10),
        duration: "🔁",
        volume: (toneVolume / 64) * volumeScale,
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
      return synth({
        type: wave,
        attack: quickFade ? 0.0015 : attack,
        // decay,
        tone,
        duration: "🔁",
        volume: toneVolume * volumeScale,
      });
    }
  }

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

  const computePitchBendRatio = () => {
    const semitoneOffset = midiPitchBendValue * MIDI_PITCH_BEND_RANGE;
    return 2 ** (semitoneOffset / 12);
  };

  const applyPitchBendToNotes = (noteKeys, { immediate = false } = {}) => {
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

      const bentFrequency = baseFrequency * ratio;
      const payload = immediate ? { tone: bentFrequency } : { tone: bentFrequency, duration: 0.05 };
      try {
        soundEntry.sound.update(payload);
      } catch (err) {
        console.warn("🎛️ Pitch bend update failed", { noteKey, err });
      }
    });
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

      let soundHandle = makeNoteSound(tone, velocity);

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
      makePerc(2000);
    }
  }

  if (e.is("keyboard:down:alt") && !e.repeat && e.code === "AltLeft") {
    perc = pc; //"cyan";
    makePerc(3000);
  }

  if (e.is("keyboard:down:alt") && !e.repeat && e.code === "AltRight") {
    perc = pc; //"cyan";
    makePerc(4000);
  }

  if (e.is("keyboard:down:arrowleft") && !e.repeat && !percDowns.left) {
    perc = "brown";
    percDowns.left = true;
    makePerc(5000);
  }

  if (e.is("keyboard:down:arrowdown") && !e.repeat && !percDowns.down) {
    perc = "pink";
    percDowns.down = true;
    makePerc(6000);
  }

  if (e.is("keyboard:down:arrowright") && !e.repeat && !percDowns.right) {
    perc = "orange";
    percDowns.right = true;
    makePerc(7000);
  }

  if (e.is("keyboard:down:arrowup") && !e.repeat && !percDowns.up) {
    percDowns.up = true;
    perc = "cyan";
    makePerc(8000);
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

  if (!tap) {
    const activePens = pens?.();
    if (activePens?.length > 0) {
      anyDown = true;
    }

    if (e.is("lift") && activePens?.length <= 1) anyDown = false;

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
              anyDown = true;
              // In song mode, block all notes except the current one
              if (song && note.toUpperCase() !== song?.[songIndex][0]) {
                // Play white noise bump for wrong note
                synth({
                  type: "noise-white",
                  tone: 1000,
                  duration: 0.05,
                  volume: 0.3,
                  attack: 0,
                });
                return false; // Block the interaction
              }

              if (downs[note]) return false; // Cancel the down if the key is held.

              let noteUpper = note.toUpperCase();
              // console.log("Note upper:", noteUpper);
              keys += noteUpper;
              const active = orderedByCount(sounds);

              let tempOctave = octave;

              if (note[0] === "+") {
                noteUpper = noteUpper.replace("+", "");
                tempOctave = parseInt(octave) + 1;
                tempOctave += upperOctaveShift;
              } else {
                tempOctave = parseInt(octave) + lowerOctaveShift;
              }

              const tone = `${tempOctave}${noteUpper}`;

              // console.log("🔴 Chosen tone:", tone, noteUpper);

              if (slide && active.length > 0) {
                sounds[active[0]]?.sound?.update({ tone, duration: 0.1 });
                // 🟠 TODO: Instead of just duration here also be able to add
                //          a swing / easing function so it's not necessarily
                //          linear? 24.07.28.22.19
                tonestack[note] = {
                  count: Object.keys(tonestack).length,
                  tone,
                };
                sounds[note] = sounds[active[0]]; // Switch the note label.
                if (sounds[note]) sounds[note].note = note;
                delete sounds[active[0]]; // Swap the sound reference.
                applyPitchBendToNotes([note], { immediate: true });
              } else {
                tonestack[note] = {
                  count: Object.keys(tonestack).length,
                  tone,
                };

                sounds[note] = {
                  note,
                  count: active.length + 1,
                  sound: makeNoteSound(tone, 127),
                };

                applyPitchBendToNotes([note], { immediate: true });

                if (note.toUpperCase() === song?.[songIndex][0]) {
                  songNoteDown = true;
                }

                delete trail[note];

                pictureAdd(api, tone);
                udpServer?.send("tv", { note }); // Send udp message for note.
              }
            },
            over: (btn) => {
              if (btn.up && anyDown) {
                btn.up = false;
                btn.actions.down(btn);
                
                // In song mode, if we drag into the correct note, mark it as pressed
                if (song && note.toUpperCase() === song?.[songIndex][0]) {
                  songNoteDown = true;
                }
              }
            },
            // TODO: The order of over and out will be important...
            out: (btn) => {
              // Only stop the sound when dragging off, but don't affect button down state
              console.log("🔇 Note OUT - stopping sound:", note);
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
              console.log("🔇 Force stopping stuck sound:", note);
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
              // console.log(note, sounds);
              // sounds[key]?.sound?.update({
              //  tone: tonestack[orderedTones[orderedTones.length - 2]].tone,
              // });
              // sounds[orderedTones[orderedTones.length - 2]] = sounds[key];
              //}
            },
          },
          pens?.(),
        );
      }
    });
  }

  // if (editable && e.is("keyboard:down:tab") && !e.repeat) {
  //   tap = !tap;
  //   resetModeState();
  // }

  if (e.is("keyboard:down:tab")) {
    waveBtn.actions.push?.();
  }

  // Clear Track
  if (editable && e.is("keyboard:down:0") && !e.repeat) {
    keys = "";
    tap = false;
    editable = false;
    resetModeState();
  }

  if (tap) {
    if ((e.is("keyboard:down:space") || e.is("touch")) && !sounds[tapped]) {
      let reset = false;
      //if (!hold) {
      // TODO: ❤️‍🔥 Fix this.
      // }

      let tempTapIndex = tapIndex,
        tappedOctave;

      if (octaves.indexOf(keys[tapIndex]) > -1) {
        octave = keys[tapIndex];
        tappedOctave = octave;
        tempTapIndex += 1;
      }

      tapped = keys[tempTapIndex];

      if (accents.indexOf(keys[tempTapIndex + 1]) > -1) {
        tapped = keys[tempTapIndex] + keys[tempTapIndex + 1];
      }

      const tone = tapped;
      if (tappedOctave) tapped = tappedOctave + tapped;
  if (!reset) sounds[tapped] = makeNoteSound(octave + tone, 127); // synth({
      // type: wave,
      // tone: octave + tone,
      // attack,
      // decay,
      // count: orderedByCount(sounds).length,
      // duration: "🔁",
      // volume: toneVolume,
      // });
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
      // console.log("Buttons:", buttons);
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
          // console.log("Pressed:", buttonNote);
          sounds[buttonNote] = {
            note: buttonNote,
            count: active.length + 1,
            sound: makeNoteSound(tone, 127), // synth({
            // type: wave,
            // attack,
            // decay,
            // tone,
            // duration: "🔁",
            // volume: toneVolume,
            // }),
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

        // console.log("Released:", buttonNote);

        const orderedTones = orderedByCount(tonestack);

        // console.log(
        //   "Ordered Tones:",
        //   orderedTones,
        //   "Sounds:",
        //   sounds,
        //   "Key:",
        //   key,
        // );

        if (slide && orderedTones.length > 1 && sounds[buttonNote]) {
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
        } else {
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
        }

        if (buttonNote.toUpperCase() === song?.[songIndex][0]) {
          songIndex = (songIndex + 1) % song.length;
          songNoteDown = false;
          songShifting = true;
        }

        delete tonestack[buttonNote]; // Remove this key from the notestack.
        delete sounds[buttonNote];
        trail[buttonNote] = 1;
        if (buttons[buttonNote]) buttons[buttonNote].down = false;
      }
    }
  });
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
  ink(...notesToCols[letter], 32).box(0, 0, picture.width, picture.height);

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
  const midiMetrics = computeMidiBadgeMetrics(screen);
  const layout = getButtonLayoutMetrics(screen, {
    songMode: Boolean(song),
    pictureOverlay: paintPictureOverlay,
    midiMetrics,
  });
  const {
    buttonWidth,
    buttonHeight,
    topButtonY,
    buttonsPerRow,
    totalRows,
    margin,
    melodyButtonRect,
  } = layout;

  buttonNotes.forEach((label, i) => {
    const row = floor(i / buttonsPerRow);
    const col = i % buttonsPerRow;
    const y = topButtonY + row * buttonHeight;
    const x = ceil(margin + col * buttonWidth);
    const geometry = [x, y, buttonWidth, buttonHeight];
    if (!buttons[label]) {
      buttons[label] = new ui.Button(...geometry);
      buttons[label].id = `note-${label}`;  // Add identifier for debugging
    } else {
      buttons[label].box = new geo.Box(...geometry);
    }
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
  const glyphWidth = typeface.glyphs["0"].resolution[0];
  const waveWidth = wave.length * glyphWidth;
  const margin = 4;
  waveBtn = new ui.Button(
    screen.width - waveWidth - 26 - margin * 2,
    0,
    waveWidth + margin * 2 + 5,
    10 + margin * 2 - 1 + 2,
  );
  waveBtn.id = "wave-button";  // Add identifier for debugging
}

function buildOctButton({ screen, ui, typeface }) {
  const glyphWidth = typeface.glyphs["0"].resolution[0];
  const octWidth = octave.length * glyphWidth;
  const margin = 4;
  octBtn = new ui.Button(
    // screen.width - octWidth - 6, 6 + 12, octWidth, 10
    screen.width - octWidth - 6 - margin * 2,
    0,
    octWidth + margin * 2 + 7,
    10 + margin * 2 - 1 + 2,
  );
  octBtn.id = "oct-button";  // Add identifier for debugging
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
