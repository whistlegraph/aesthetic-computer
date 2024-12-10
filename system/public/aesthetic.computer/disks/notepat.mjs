// Notepat, 2024.6.26.23.17.58.736
// Tap the pads to play musical notes, or use the keyboard keys.

/* 📝 Notes 
   - [🟢] Make a new "painting" to use as a visualizer.
    - [] Draw symmetrical turtle graphics based on waveform values that
         paint continuously and produce consistent and differential form.

   - [🔵] Send udp messages from keys to `tv`.
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
let waveIndex = 4; // 0;
const STARTING_WAVE = wavetypes[waveIndex]; //"sine";
let wave = STARTING_WAVE;
// let hold = false;
let slide = false;
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

let upperOctaveShift = 0, // Set by <(,) or >(.) keys.
  lowerOctaveShift = 0;

const attack = 0.005; // 0.025;
// const attack = 0.005; // 0.025;
// const decay = 0.9999; // 0.9;
let toneVolume = 0.95; // 0.9;
// const killFade = 0.01; // TODO: Make this dynamic according to press time. 24.11.04.06.05
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
  "orange", // 4
  "yellowgreen", // 6
  "yellow", // 5
  "green", // 7
  "purple", // 8
];

const { abs, round, floor, ceil, min, max } = Math;

let scope = 12;
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
  songShifting = false;

function parseSong(raw) {
  return raw
    .trim()
    .split(/\s+/)
    .map((noteword) => noteword.split(":"));
}

// song = parseSong(rawSong);

let oscilloscopeBottom = 48;
let startupSfx;
let udpServer;

let picture;

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
}) {
  // fps(4);
  udpServer = net.udp(); // For sending messages to `tv`.

  picture = painting(128, 128, ({ wipe }) => {
    wipe("gray");
  });

  net
    .preload("startup") // TODO: Switch this default sample. 24.11.19.22.20
    .then((sfx) => (startupSfx = sfx))
    .catch((err) => console.warn(err)); // Load startup

  // qrcells = qr("https://prompt.ac/notepat", { errorCorrectLevel: 2 }).modules;

  if (params[0] === "piano") {
    toneVolume = params[1] || 0.5;
    hud.label("notepat"); // Clear the label.
  }

  if (params[0] === "twinkle") song = parseSong(rawSong);

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
  slide = colon[0] === "slide" || colon[1] === "slide";

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

function paint({
  wipe,
  ink,
  write,
  screen,
  box,
  sound,
  typeface,
  num,
  layer,
  paste,
  api,
}) {
  const active = orderedByCount(sounds);

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

  wipe(bg);
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

  if (song) {
    const glyphWidth = typeface.glyphs["0"].resolution[0];
    const startX = 6 - songShift;
    let i = 0;

    // TODO: How can I scroll the entire song to the left by adjusting the
    //       x start position

    // ink("cyan", 64).line(0, 21, screen.width, 21);
    ink(140, 120).box(0, 22, screen.width, 25);
    // ink("cyan", 64).line(0, 47, screen.width, 47);

    while (i < song.length) {
      let word = songStops[i][0];
      let x = startX + songStops[i][1];

      if (x > screen.width) break;

      const current = i === songIndex;

      ink(current ? (songNoteDown ? "red" : "yellow") : "gray").write(
        word,
        x,
        25,
      );

      ink(current ? (songNoteDown ? "red" : "lime") : "darkblue").write(
        song[i][0],
        x,
        25 + 10,
      );

      i += 1;
    }
  }

  if (projector) {
    const sy = 33;
    const sh = screen.height - sy;

    paintSound(
      api,
      sound.speaker.amplitudes.left,
      resampleArray(sound.speaker.waveforms.left, scope),
      0,
      sy,
      screen.width, // width
      sh, // height
      [255, 0, 0, 255],
      { noamp: true },
    );
    //ink("yellow").write(scope, 6, sy + sh + 5);
    ink("yellow").write(scope, 50 + 4, 6);
    // ink("pink").write(scopeTrim, 6 + 18, sy + sh + 5);
  } else {
    const sy = 3;
    const sh = 15; // screen.height - sy;

    // const right = (54 + 120);
    // const leftOfWav = wavBtn.box.x
    const availableWidth = waveBtn.box.x - 54;

    paintSound(
      api,
      sound.speaker.amplitudes.left,
      resampleArray(sound.speaker.waveforms.left, scope),
      54, //0,
      sy, //sy,
      availableWidth,
      //screen.width, // width
      sh, // height
      [255, 0, 0, 255],
    );

    // ink("yellow").write(scope, 56 + 120 + 2, sy + 3);
    // ink("pink").write(scopeTrim, 6 + 18, sy + sh + 3);
    // ink("cyan").write(sound.sampleRate, 6 + 18 + 20, sy + sh + 3);
  }

  if (tap) {
    ink("yellow");
    write("tap", { right: 6, top: 6 });
  } else {
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

  if (tap)
    ink("orange").line(screen.width / 2, 0, screen.width / 2, screen.height);

  if (!tap) {
    // Write all the active keys.
    ink("lime");
    write(
      active.map((key) => sounds[key]?.note.toUpperCase()).join(" "),
      6,
      20,
    );
  } else {
    ink("white");
    active.forEach((sound, index) => {
      write(sound, screen.width / 2, screen.height / 2 + 12 + 12 * index);
    });
    if (tapped) {
      ink("lime");
      write(tapped, screen.width / 2, screen.height / 2);
    }
  }

  if (!tap /*&& !projector*/) {
    const activeNote = buttonNotes.indexOf(active[0]);

    if (activeNote >= 0 && activeNote !== lastActiveNote) {
      lastActiveNote = activeNote;
      transposeOverlayFade = 0;
    }

    // Paint all the keyboard buttons.
    buttonNotes.forEach((note, index) => {
      if (buttons[note]) {
        buttons[note].paint((btn) => {
          let color;

          if ((!slide && btn.down) || (btn.down && slide)) {
            // If this button is pressed down.
            color = "maroon";
          } else {
            const outOctave =
              parseInt(octave) +
              (note.startsWith("+") ? upperOctaveShift : lowerOctaveShift);
            // console.log("Output octave:", outOctave);
            color = note.indexOf("#") === -1 ? octaveTheme[outOctave] : "gray";
          }

          if (note.toUpperCase() === song?.[songIndex][0]) {
            layer(1).ink("red").box(btn.box, "inline").layer(0);
            if (!btn.down) color = "red";
          }

          if (!projector) {
            ink(color, 196).box(btn.box); // One solid colored box per note.
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
          ink("white").write(note.toUpperCase(), btn.box.x + 2, btn.box.y + 1);
          const glyphWidth = typeface.glyphs["0"].resolution[0];
          const glyphHeight = typeface.glyphs["0"].resolution[1];

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
          let keyLabel;
          switch (note) {
            case "c#":
              keyLabel = "v";
              break;
            case "d#":
              keyLabel = "s";
              break;
            case "f#":
              keyLabel = "w";
              break;
            case "g#":
              keyLabel = "r";
              break;
            case "a#":
              keyLabel = "q";
              break;
            // Second octave.
            case "+c":
              keyLabel = "h";
              break;
            case "+d":
              keyLabel = "i";
              break;
            case "+e":
              keyLabel = "j";
              break;
            case "+f":
              keyLabel = "k";
              break;
            case "+g":
              keyLabel = "l";
              break;
            case "+a":
              keyLabel = "m";
              break;
            case "+b":
              keyLabel = "n";
              break;
            case "+c#":
              keyLabel = "t";
              break;
            case "+d#":
              keyLabel = "y";
              break;
            case "+f#":
              keyLabel = "u";
              break;
            case "+g#":
              keyLabel = "o";
              break;
            case "+a#":
              keyLabel = "p";
              break;
          }
          if (keyLabel)
            ink("white", 96).write(
              keyLabel,
              btn.box.x + 2, // + note.length * glyphWidth,
              btn.box.y + 10,
            );
        });
      }
    });
  }

  if (paintPictureOverlay) {
    paste(
      picture,
      screen.width / 2 - picture.width / 2,
      screen.height / 3 - picture.height / 2,
    ); // 🖼️ Picture
  }
}

let anyDown = true;

function act({
  event: e,
  sound: { synth, speaker, play, freq },
  num,
  pens,
  api,
}) {
  if (e.is("reframed")) {
    setupButtons(api);
    buildWaveButton(api);
    buildOctButton(api);
  }

  if (e.is("keyboard:down:.") && !e.repeat) {
    upperOctaveShift += 1;
  }

  if (e.is("keyboard:down:,") && !e.repeat) {
    upperOctaveShift -= 1;
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
    setupButtons(api);
  }
  if (e.is("keyboard:down:\\")) projector = !projector;

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
    if (scope > speaker.waveforms.left.length) {
      scope = speaker.waveforms.left.length;
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

  function makeNoteSound(tone) {
    if (wave === "sample") {
      // synth({
      //   type: "sine",
      //   attack,
      //   tone: freq(tone),
      //   duration: 0.5,
      //   volume: toneVolume / 2, // / 32,
      // });

      return play(startupSfx, {
        volume: 1,
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
        volume: toneVolume,
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
        volume: toneVolume / 3, // / 16,
      });

      toneC = synth({
        type: "sawtooth",
        attack,
        decay: 0.9,
        tone: baseFreq + num.randIntRange(-6, 6),
        duration: 0.15 + num.rand() * 0.05,
        volume: toneVolume / 48, // / 32,
      });

      // TODO: One-shot sounds and samples need to be 'killable'.

      toneD = synth({
        type: "triangle",
        attack: 0.999, //attack * 8,
        // decay,
        tone: baseFreq + 8 + num.randIntRange(-5, 5),
        duration: "🔁",
        volume: toneVolume / 32,
      });

      toneE = synth({
        type: "square",
        attack: 0.05, //attack * 8,
        // decay,
        tone: baseFreq + num.randIntRange(-10, 10),
        duration: "🔁",
        volume: toneVolume / 64,
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
        attack,
        // decay,
        tone,
        duration: "🔁",
        volume: toneVolume,
      });
    }
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

  if (e.is("keyboard:down:arrowleft") && !e.repeat) {
    perc = "brown";
    makePerc(5000);
  }

  if (e.is("keyboard:down:arrowdown") && !e.repeat) {
    perc = "pink";
    makePerc(6000);
  }

  if (e.is("keyboard:down:arrowright") && !e.repeat) {
    perc = "orange";
    makePerc(7000);
  }

  if (e.is("keyboard:down:arrowup") && !e.repeat) {
    perc = "cyan";
    makePerc(8000);
  }

  if (!tap) {
    if (e.is("lift") && pens().length <= 1) anyDown = false;

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

    buttonNotes.forEach((note) => {
      if (buttons[note]) {
        buttons[note].act(
          e,
          {
            down: (btn) => {
              if (downs[note]) return false; // Cancel the down if the key is held.
              anyDown = true;

              let noteUpper = note.toUpperCase();
              console.log("Note upper:", noteUpper);
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

              console.log("🔴 Chosen tone:", tone, noteUpper);

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
                delete sounds[active[0]]; // Swap the sound reference.
              } else {
                tonestack[note] = {
                  count: Object.keys(tonestack).length,
                  tone,
                };

                sounds[note] = {
                  note,
                  count: active.length + 1,
                  sound: makeNoteSound(tone),
                };

                if (note.toUpperCase() === song?.[songIndex][0]) {
                  songNoteDown = true;
                }

                delete trail[note];

                pictureAdd(api, note);
                udpServer?.send("tv", { note }); // Send udp message for note.
              }
            },
            over: (btn) => {
              if (btn.up && anyDown) {
                btn.up = false;
                btn.actions.down(btn);
              }
            },
            // TODO: The order of over and out will be important...
            out: (btn) => {
              btn.down = false;
              btn.actions.up(btn);
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
                sounds[orderedTones[orderedTones.length - 2]] = sounds[note];
              } else {
                sounds[note]?.sound.kill(killFade);
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
      if (!reset) sounds[tapped] = makeNoteSound(octave + tone); // synth({
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

      sounds[tapped]?.kill(killFade);
      delete sounds[tapped];
      tapped = undefined;
    }
  }

  // Individual Keyboard Notes
  [...(octaves + notes + edges).split(""), "control"].forEach((key) => {
    if (e.is(`keyboard:down:${key}`) && !e.repeat && !buttons[key]?.down) {
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

        // if (sharps && "CDFGA".includes(note)) {
        //   note += "#";
        // } else if (flats && "DEGAB".includes(note)) {
        //   note += "f";
        // }

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

        if (buttons[buttonNote]) buttons[buttonNote].down = true;

        const active = orderedByCount(sounds);

        const tone = `${activeOctave}${note}`;

        if (slide && active.length > 0) {
          // TODO: Fix slide here... 24.08.16.06.18
          //console.log("Fix slide...", active[0], sounds[active[0]]?.sound, tone, key);

          sounds[active[0]]?.sound?.update({ tone, duration: 0.1 });

          tonestack[key] = {
            count: Object.keys(tonestack).length,
            tone,
          };
          sounds[buttonNote] = sounds[active[0]]; // Switch the note label.
          delete sounds[active[0]]; // Swap the sound reference.
        } else {
          tonestack[buttonNote] = {
            count: Object.keys(tonestack).length,
            tone,
          };
          // console.log("Pressed:", buttonNote);
          sounds[buttonNote] = {
            note: buttonNote,
            count: active.length + 1,
            sound: makeNoteSound(tone), // synth({
            // type: wave,
            // attack,
            // decay,
            // tone,
            // duration: "🔁",
            // volume: toneVolume,
            // }),
          };

          if (buttonNote.toUpperCase() === song?.[songIndex][0]) {
            songNoteDown = true;
          }

          delete trail[buttonNote];

          pictureAdd(api, note);
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

        // console.log("Released:", buttonNote);

        const orderedTones = orderedByCount(tonestack);
        if (slide && orderedTones.length > 1 && sounds[key]) {
          sounds[buttonNote]?.sound?.update({
            tone: tonestack[orderedTones[orderedTones.length - 2]].tone,
            duration: 0.1,
          });
          sounds[orderedTones[orderedTones.length - 2]] = sounds[buttonNote];
        } else {
          // console.log("Killing sound:", buttonNote);

          if (sounds[buttonNote].sound) {
            const fade = max(
              0.175,
              min(
                (performance.now() - sounds[buttonNote].sound.startedAt) / 1000,
                0.45,
              ),
            );
            // console.log("🦋 Fade length:", fade);
            // killFade
            sounds[buttonNote]?.sound.kill(fade); // Kill a sound if it exists.
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
  page(picture);
  // wipe("black");
  ink("yellow");
  // write(note, { center: "xy"}); // TOOD: Write's random values should be confined to the
               //       page and not the screen.
  // write("POW", { center: "xy" }); // TOOD: Write's random values should be confined to the
  write(note, num.randInt(128), num.randInt(128));
  ink(undefined);
  line();
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
  const margin = 2;
  const ymargin = 2;
  const buttonsPerRow = 4;
  const totalButtons = buttonNotes.length;
  const totalRows = ceil(totalButtons / buttonsPerRow);

  let buttonWidth = min(48, ceil((screen.width - margin * 2) / 4));

  if (paintPictureOverlay) {
    buttonWidth = 24;
  }

  let buttonHeight = buttonWidth;

  if (totalRows * buttonHeight > screen.height - oscilloscopeBottom) {
    buttonWidth = buttonHeight = ceil(
      (screen.height - oscilloscopeBottom) / totalRows,
    );
  }

  buttonNotes.forEach((label, i) => {
    const row = floor(i / buttonsPerRow);
    const col = i % buttonsPerRow;
    const y = screen.height - ymargin - (totalRows - row) * buttonHeight;
    const x = ceil(margin + col * buttonWidth);
    const geometry = [x, y, buttonWidth, buttonHeight];
    if (!buttons[label]) {
      buttons[label] = new ui.Button(...geometry);
    } else {
      buttons[label].box = new geo.Box(...geometry);
    }
  });
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
}

let primaryColor = [0, 0, 0];
let currentAverage = [0, 0, 0];

let secondaryColor = [0, 0, 0];
let lastAverage = [0, 0, 0];
let lastActive = null;
let activeStr;

function paintSound(
  { ink, box, screen, num },
  amplitude,
  waveform,
  x,
  y,
  width,
  height,
  color,
  options = { noamp: false },
) {
  const yMid = round(y + (height - 2) / 2),
    yMax = round((height - 2) / 2);
  let lw = options.noamp ? 0 : 4; // levelWidth;
  const xStep = (width - lw) / waveform.length;

  // Vertical bounds.
  ink("yellow")
    .line(x + lw, y, x + width - 1, y)
    .line(x + lw, y + height, x + width - 1, y + height);

  // Level meter.
  if (!options.noamp) {
    ink("black").box(x, y, lw, height + 1);
    ink("green").box(x, y + height, lw, -amplitude * height);
  }

  // Filled waveform
  const waves = waveform.map((v, i) => {
    if (v < -1) v = -1;
    if (v > 1) v = 1;
    return [x + lw + i * xStep, yMid + v * yMax];
  });

  ink(secondaryColor || "black").box(x + lw, y + 1, width - lw, height - 1);

  const active = orderedByCount(sounds);
  activeStr = active.join("");

  let colors = ["red"];

  if (activeStr.length > 0 && activeStr !== lastActive) {
    lastActive = activeStr;
    colors = active.map((note) => colorFromNote(note, num));
    const average = averageRGB(colors);
    lastAverage = currentAverage;
    currentAverage = average;
  }

  // lerp the primary color to the current average.

  ink(primaryColor);

  let remainder = 0;
  let totalWidthCovered = 0;

  waves.forEach((point, index) => {
    let bx = x + lw + totalWidthCovered;
    if (bx >= x + width) return;
    // Compute the pixel-aligned width for the current bar.
    let barWidth = Math.floor(xStep + remainder);
    remainder = (xStep + remainder) % 1; // Collect the fractional remainder.
    // Ensure we don't exceed the full width for the last bar.
    if (index === waves.length - 1 || bx + barWidth >= x + width)
      barWidth = x + width - bx;
    box(bx, y + point[1] + 1 - y, barWidth, y + (height - 1) - point[1]);
    totalWidthCovered += barWidth;
  });

  // Waveform
  // ink("lime", 255).poly(
  //   waveform.map((v, i) => [x + lw + i * xStep, yMid + v * yMax]),
  // );

  // TODO: Fill a point above this line and below.
  // ink("blue").flood(x + 7, y + 1);
  // ink("teal").flood(x + 7, y + height - 2);

  // const my = screen.height - mic.amplitude * screen.height;
  // ink("yellow", 128).line(0, my, screen.width, my); // Horiz. line for amplitude.
}

// Resize an array by taking samples at equal intervals, with no interpolation.
function resampleArray(inputArray, newLength) {
  const inputLength = inputArray.length;
  const outputArray = [];
  for (let i = 0; i < newLength; i++) {
    const index = floor((i / newLength) * inputLength);
    outputArray.push(inputArray[index]);
  }
  return outputArray;
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
