// Notepad, 2024.6.26.23.17.58.736
// Touch pads that play musical notes, or use the keyboard keys.

// ⚖️ Scales
// CvDsEFrGtAwB
// BwAtGrFEsDvC

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

/* 📝 Notes 
  - [-] Fix slide mode.
  - [] Add perc buttons to ui interface.
  - [-] Add scale selection with visual hiking paths. (Color themes).
    - [] Ghost trails.
    - [] Scale selections.
  - [-] Fix '+C' notes appearing in the key list during playback mode.
    - [🟠] on keyboard
    - [] on touch / mouse 
  - [-] Fix subtle 1, 2, 3, 4, then release 1 and press 1 down and watch 4 get unticked touch bug on ios. 
    - [x] This may require fixing localhost testing first.
  - [] Lay out keys better on wider vs vertical screen.
  - [?] Dragging across the buttons in slide mode should slide from one key to another? 
  - [] Add octave touch buttons.
  - [] Compare my sine waves to a sine wave generator.
  - [] Add multiple tracks so that I can create "systems" that loop
       with different lengths.
  - [] Add holdable rhythm button with patterns that "cycle".
  - [] Make sure you can whack multiple keys / alternate keys in tap mode,
       and make sure every key and mouse button does the same thing now...
  + Later
  // TODO: Rethink how to do a simpler button API... perhaps with "register"
  //       and an act function?
  - [?] Add longer / customizable slide duration.
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

let STARTING_OCTAVE = "4";
const STARTING_WAVE = "sine"; //"sine";
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
//                              cdefgab (next ovtave)
//                       // or alt on   D E G A B for flats
// This is a notes -> keys mapping, that uses v for c#

const attack = 0.025;

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

const buttonOctaves = ["3", "4", "5", "6", "7", "8"]; // ❤️‍🔥 Add octaves...

const octaveTheme = [
  "black",
  "black",
  "grey",
  "red",
  "orange", // [255, 128],
  "yellowgreen",
  "green",
  "purple",
];

const { floor, ceil, min } = Math;

let scope = 64;
let scopeTrim = 0;

let projector = false;

function boot({ params, api, colon, ui, screen, fps }) {
  // fps(12);
  keys = params.join(" ") || "";
  keys = keys.replace(/\s/g, "");
  if (keys.length > 0) {
    tap = true;
    editable = false;
  }

  const wavetypes = ["square", "sine", "triangle", "sawtooth", "noise-white"];

  wave = wavetypes.indexOf(colon[0]) > -1 ? colon[0] : wave;
  slide = colon[0] === "slide" || colon[1] === "slide";

  const newOctave =
    parseInt(colon[0]) || parseInt(colon[1]) || parseInt(colon[2]);

  if (newOctave) {
    STARTING_OCTAVE = newOctave.toString();
    octave = STARTING_OCTAVE;
  }

  setupButtons(api);
}

// TODO: Get sim working again. 24.08.10.21.25
// function sim({ sound }) {
// }

function paint({ wipe, ink, write, screen, sound, api }) {
  sound.speaker?.poll();

  const active = orderedByCount(sounds);

  let bg;

  if (!tap) {
    bg = active.length > 0 ? [50, 50, 255] : "blue";
  } else {
    bg = active.length > 0 ? [0, 0, 180] : "darkblue";
  }

  wipe(bg);

  // TODO: Should this be a built-in function on sound?

  if (projector) {
    const sy = 33;
    const sh = screen.height - sy - 20;

    paintSound(
      api,
      sound.speaker.amplitudes.left,
      sound.speaker.waveforms.left.slice(scopeTrim, scope),
      0,
      sy,
      screen.width, // width
      sh, // height
      [255, 0, 0, 255],
      { noamp: true },
    );
    ink("yellow").write(scope, 6, sy + sh + 5);
    ink("pink").write(scopeTrim, 6 + 18, sy + sh + 5);
  } else {
    const sy = 32;
    const sh = 40; // screen.height - sy;

    paintSound(
      api,
      sound.speaker.amplitudes.left,
      sound.speaker.waveforms.left.slice(scopeTrim, scope),
      0,
      sy,
      screen.width, // width
      sh, // height
      [255, 0, 0, 255],
    );

    ink("yellow").write(scope, 6, sy + sh + 3);
    ink("pink").write(scopeTrim, 6 + 18, sy + sh + 3);
  }

  if (tap) {
    ink("yellow");
    write("tap", { right: 6, top: 6 });
  } else {
    ink("orange");
    write("type", { right: 6, top: 6 });
  }

  if (!tap) {
    ink("cyan");

    /*
    // Write all keys...
    write(keys, 6, 20 + 12, {
      bounds: screen.width - 12,
      wordWrap: false,
    });

    // Highlight all playing keys...
    ink("yellow");

    const notes = active.map((key) => sounds[key]?.note);
    // TODO: Tokenize the keys and replace any instances of what got pressed,
    //       including octave keys and accents.
    write(
      keys.replace(new RegExp(`[^${notes.join("")}]`, "g"), " "),
      6,
      20 + 12,
      { bounds: screen.width - 12, wordWrap: false },
    );
    */
  } else {
    ink("gray");
    write(keys, screen.width / 2 - tapIndex * 6, screen.height / 2);

    ink("red");
    //console.log("🔴 in red:", keys[tapIndex], tapped);
    // Highlight next char by looking ahead...

    let nextToken = keys[tapIndex];
    let tempTapIndex = tapIndex;
    if (octaves.includes(nextToken)) {
      tempTapIndex += 1;
      nextToken += keys[tempTapIndex];
    }

    if (accents.includes(keys[tempTapIndex + 1])) {
      nextToken += keys[tempTapIndex + 1];
    }

    write(nextToken, screen.width / 2, screen.height / 2);
  }

  if (tap)
    ink("orange").line(screen.width / 2, 0, screen.width / 2, screen.height);

  if (!tap) {
    ink("lime");
    write(active.map((key) => sounds[key]?.note).join(" "), 6, 20);
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

  if (!tap && !projector) {
    // Write current octave.
    ink("white").write(octave, screen.width - 12, 18);
    // Buttons
    buttonNotes.forEach((note) => {
      if (buttons[note]) {
        buttons[note].paint((btn) => {
          // if (note === "+c") {
          // console.log("Active:", active[active.length - 1]?.toLowerCase());
          // }

          let color;

          if (
            (!slide && btn.down) ||
            (btn.down &&
              slide &&
              active[active.length - 1]?.toLowerCase() === note)
          ) {
            color = "maroon";
          } else {
            color = note.indexOf("#") === -1 ? octaveTheme[octave] : "gray";
          }

          ink(color, 192)
            .box(btn.box)
            .ink("white")
            .write(note.toUpperCase(), btn.box.x, btn.box.y);

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
          if (keyLabel) ink("white").write(keyLabel, btn.box.x, btn.box.y + 10);
        });
      }
    });
  }
}

let anyDown = true;

function act({ event: e, sound: { synth, speaker }, pens, api }) {
  if (e.is("reframed")) setupButtons(api);

  if (e.is("keyboard:down:arrowleft")) {
    scopeTrim -= 1;
    if (scopeTrim < 0) scopeTrim = 0;
  }

  if (e.is("keyboard:down:arrowright")) {
    scopeTrim += 1;
  }

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

  if (tap) {
    // if (e.is("keyboard:down:shift") && !e.repeat) hold = true;
    // if (e.is("keyboard:up:shift")) hold = false;
  } else {
    // if (e.is("keyboard:down:shift") && !e.repeat) sharps = true;
    // if (e.is("keyboard:up:shift")) sharps = false;
    // if (e.is("keyboard:down:alt") && !e.repeat) flats = true;
    // if (e.is("keyboard:up:alt")) flats = false;
  }

  if (!tap) {
    if (e.is("keyboard:down:space")) {
      synth({
        type: "square",
        tone: 80,
        attack: 0.001,
        duration: 0.15,
      });

      synth({
        type: "square",
        tone: 100,
        attack: 0.001,
        duration: 0.15,
      });

      synth({
        type: "triangle",
        tone: 150,
        attack: 0.001,
        duration: 0.1,
      });

      synth({
        type: "sine",
        tone: 300,
        attack: 0.001,
        duration: 0.5,
      });
    }
    if (e.is("keyboard:down:control") || e.is("keyboard:down:capslock")) {
      synth({
        type: "noise-white",
        duration: 0.1,
      });
    }
    if (e.is("keyboard:down:shift")) {
      synth({
        type: "square",
        tone: 300,
        duration: 0.04,
        decay: 0.98,
      });
      synth({
        type: "triangle",
        tone: 350,
        duration: 0.14,
        decay: 0.98,
      });
      synth({
        type: "sine",
        tone: 300,
        duration: 0.14,
        decay: 0.98,
      });
    }
  }

  if (!tap) {
    if (e.is("lift") && pens().length <= 1) anyDown = false;

    buttonNotes.forEach((note) => {
      if (buttons[note]) {
        buttons[note].act(
          e,
          {
            down: (btn) => {
              if (downs[note]) return false; // Cancel the down if the key is held.
              anyDown = true;
              let noteUpper = note.toUpperCase();
              keys += noteUpper;
              const active = orderedByCount(sounds);

              let tempOctave = octave;

              if (note[0] === "+") {
                noteUpper = noteUpper.replace("+", "");
                tempOctave = parseInt(octave) + 1;
              }

              const tone = `${tempOctave}${noteUpper}`;

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
                  note: noteUpper,
                  count: active.length + 1,
                  sound: synth({
                    type: wave,
                    attack,
                    tone,
                    duration: "🔁",
                  }),
                };
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
                sounds[note]?.sound.kill(0.25);
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

  if (editable && e.is("keyboard:down:tab") && !e.repeat) {
    tap = !tap;
    resetModeState();
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
      if (!reset)
        sounds[tapped] = synth({
          type: wave,
          tone: octave + tone,
          attack,
          // count: orderedByCount(sounds).length,
          duration: "🔁",
        });
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

      sounds[tapped]?.kill(0.25);
      delete sounds[tapped];
      tapped = undefined;
    }
  }

  // Individual Keyboard Notes
  (octaves + notes).split("").forEach((key) => {
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

        // if (sharps && "CDFGA".includes(note)) {
        //   note += "#";
        // } else if (flats && "DEGAB".includes(note)) {
        //   note += "f";
        // }

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

        let activeOctave = octave;

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
          activeOctave = parseInt(octave) + 1;
        }

        if (activeOctave !== octave) {
          keys += activeOctave + note;
        } else {
          keys += note;
        }

        const buttonNote =
          (activeOctave === octave ? "" : "+") + note.toLowerCase();
        if (buttons[buttonNote]) buttons[buttonNote].down = true;

        const active = orderedByCount(sounds);
        const tone = `${activeOctave}${note}`;

        if (slide && active.length > 0) {
          // TODO: Fix slide here... 24.08.16.06.18
          console.log("Fix slide...");
          sounds[active[0]]?.sound?.update({ tone, duration: 0.1 });
          tonestack[key] = {
            count: Object.keys(tonestack).length,
            tone,
          };
          sounds[key] = sounds[active[0]]; // Switch the note label.
          delete sounds[active[0]]; // Swap the sound reference.
        } else {
          tonestack[key] = {
            count: Object.keys(tonestack).length,
            tone,
          };
          sounds[key] = {
            note,
            count: active.length + 1,
            sound: synth({
              type: wave,
              attack,
              tone,
              duration: "🔁",
            }),
          };
        }
      }
    }

    // Just for the notes, not the octaves...
    if (e.is(`keyboard:up:${key}`) /*&& notes.indexOf(key) > -1*/) {
      if (tap && tapped === key) {
        tapIndex = (tapIndex + 1) % keys.length;
        tapped = undefined;
      }
      const orderedTones = orderedByCount(tonestack);
      if (slide && orderedTones.length > 1 && sounds[key]) {
        sounds[key]?.sound?.update({
          tone: tonestack[orderedTones[orderedTones.length - 2]].tone,
          duration: 0.1,
        });
        sounds[orderedTones[orderedTones.length - 2]] = sounds[key];
      } else {
        sounds[key]?.sound?.kill?.(0.25); // Kill a sound if it exists.
      }

      delete tonestack[key]; // Remove this key from the notestack.
      delete sounds[key];

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
        if (buttons[buttonNote]) buttons[buttonNote].down = false;
      }
    }
  });
}

// 📚 Library

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
  const margin = 6;
  const buttonsPerRow = 4;
  const totalButtons = buttonNotes.length;
  const totalRows = ceil(totalButtons / buttonsPerRow);

  let buttonWidth = min(48, ceil((screen.width - margin * 2) / 4));
  let buttonHeight = buttonWidth;

  const oscilloscopeBottom = 96;

  if (totalRows * buttonHeight > screen.height - oscilloscopeBottom) {
    buttonWidth = buttonHeight = ceil(
      (screen.height - oscilloscopeBottom) / totalRows,
    );
  }

  buttonNotes.forEach((label, i) => {
    const row = floor(i / buttonsPerRow);
    const col = i % buttonsPerRow;
    const y = screen.height - margin - (totalRows - row) * buttonHeight;
    const x = ceil(margin + col * buttonWidth);
    const geometry = [x, y, buttonWidth, buttonHeight];
    if (!buttons[label]) {
      buttons[label] = new ui.Button(...geometry);
    } else {
      buttons[label].box = new geo.Box(...geometry);
    }
  });
}

function paintSound(
  { ink, screen },
  amplitude,
  waveform,
  x,
  y,
  width,
  height,
  color,
  options = { noamp: false },
) {
  const xStep = width / (waveform.length - 1); // + 2;
  const yMid = Math.ceil(y + height / 2) + 1,
    yMax = Math.ceil(height / 2);

  let lw = options.noamp ? 0 : 4; // levelWidth;

  // Vertical bounds.
  ink("darkblue").box(x, y, width, height);
  ink("yellow")
    .line(x + lw, y, x + width, y)
    .line(x + lw, y + height, x + width, y + height);

  // Level meter.
  if (!options.noamp) {
    ink("black").box(x, y, lw, height);
    ink("green").box(x, y + height, lw, -amplitude * height);
  }

  // Filled waveform
  // TODO: This could be drawn faster...
  waveform
    .map((v, i) => [x + lw + i * xStep, yMid + v * yMax])
    .forEach((point) => {
      ink("blue").box(point[0], y + 1, xStep, point[1] - y);
      ink("red").box(
        point[0],
        y + 1 + point[1] - y,
        xStep,
        y + height - point[1] - 1,
      );
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