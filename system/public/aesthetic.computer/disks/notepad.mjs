// Notepad, 2024.6.26.23.17.58.736
// Touch pads that play musical notes, or use the keyboard keys.

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
    - [🧡] Get 'slide' working on the software so dragging between
         butons enables the sliding and so does pressing.
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
  - [x] Reflow the mobile button layout so it is responsive (keep a grid) 
  - [x] slide only? sticky keys error... log all playing keys / sounds in slide mode / print them out to make sure about doubles...
  - [x] Implement the full scale with rollover.
  - [x] Experiment with a monovoice/slide/replace note mode.
    + Done
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

const STARTING_OCTAVE = "4";
const STARTING_WAVE = "sine"; //"sine";
let wave = STARTING_WAVE;
let hold = false;
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
let sharps = false,
  flats = false;
const notes = "abcdefg"; // hold shift on C D F G A for sharps.
//                       // or alt on   D E G A B for flats
const octaves = "123456789";
const accents = "#f";
const buttons = {}; // Software keys. 🎹
// TODO: Add better octave theme and sharp buttons, etc... 24.07.16.20.01
const buttonNotes = ["c", "d", "e", "f", "g", "a", "b"];
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

const { floor, ceil } = Math;

function boot({ params, api, colon, ui, screen }) {
  keys = params.join(" ") || "";
  keys = keys.replace(/\s/g, "");
  if (keys.length > 0) {
    tap = true;
    editable = false;
  }

  const wavetypes = ["square", "sine", "triangle", "sawtooth", "noise-white"];

  wave = wavetypes.indexOf(colon[0]) > -1 ? colon[0] : wave;
  slide = colon[0] === "slide" || colon[1] === "slide";
  setupButtons(api);
}

function paint({ wipe, ink, write, screen }) {
  const active = orderedByCount(sounds);

  let bg;

  if (!tap) {
    bg = active.length > 0 ? [50, 50, 255] : "blue";
  } else {
    bg = active.length > 0 ? [0, 0, 180] : "darkblue";
  }

  wipe(bg);

  if (tap) {
    ink("yellow");
    write("tap", { right: 6, top: 6 });
  } else {
    ink("orange");
    write("type", { right: 6, top: 6 });
  }

  if (!tap) {
    ink("cyan");

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

  if (!tap) {
    // Write current octave.
    ink("white").write(octave, screen.width - 12, 18);

    // Buttons
    buttonNotes.forEach((note) => {
      if (buttons[note]) {
        buttons[note].paint((btn) => {
          let color;
          if (
            (!slide && btn.down) ||
            (btn.down &&
              slide &&
              active[active.length - 1]?.toLowerCase() === note)
          ) {
            color = "maroon";
          } else {
            color = octaveTheme[octave];
          }

          ink(color)
            .box(btn.box)
            .ink("white")
            .write(note.toUpperCase(), btn.box.x, btn.box.y);
        });
      }
    });
  }
}

let anyDown = true;

function act({ event: e, sound: { synth }, pens, api }) {
  if (e.is("reframed")) setupButtons(api);

  if (tap) {
    if (e.is("keyboard:down:shift") && !e.repeat) hold = true;
    if (e.is("keyboard:up:shift")) hold = false;
  } else {
    if (e.is("keyboard:down:shift") && !e.repeat) sharps = true;
    if (e.is("keyboard:up:shift")) sharps = false;
    if (e.is("keyboard:down:alt") && !e.repeat) flats = true;
    if (e.is("keyboard:up:alt")) flats = false;
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
              const noteUpper = note.toUpperCase();
              keys += noteUpper;
              const active = orderedByCount(sounds);

              //if (slide && active.length > 0) {
                // adjust the last sound in the stack
                // sounds[key]?.sound?.update({
                //  tone: tonestack[orderedTones[orderedTones.length - 2]].tone,
                // });
                // sounds[orderedTones[orderedTones.length - 2]] = sounds[key];
              //} else {
                sounds[note] = {
                  note: noteUpper,
                  count: active.length + 1,
                  sound: synth({
                    type: wave,
                    tone: `${octave}${noteUpper}`,
                    duration: "🔁",
                  }),
                };
              //}
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

              //if (!slide) {
              sounds[note]?.sound.kill(0.25);
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
        keys += key.toUpperCase();
        editable = true;
      } else if (keys[tapIndex] === key) {
        tapped = key;
      }

      if (octaves.includes(key)) {
        octave = parseInt(key);
        sounds[key] = "held";
      } else {
        let note = key.toUpperCase();

        if (sharps && "CDFGA".includes(note)) {
          note += "#";
          keys += "#";
        } else if (flats && "DEGAB".includes(note)) {
          note += "f";
          keys += "f";
        }

        if (buttons[key]) buttons[key].down = true;

        const active = orderedByCount(sounds);

        if (slide && active.length > 0) {
          // TODO: Extend the delay for sliding...
          const tone = `${octave}${note}`;
          sounds[active[0]]?.sound?.update({ tone });
          tonestack[key] = {
            count: Object.keys(tonestack).length,
            tone,
          };
          sounds[key] = sounds[active[0]]; // Switch the note label.
          delete sounds[active[0]]; // Swap the sound reference.
        } else {
          const tone = `${octave}${note}`;
          tonestack[key] = {
            count: Object.keys(tonestack).length,
            tone,
          };
          sounds[key] = {
            note,
            count: active.length + 1,
            sound: synth({
              type: wave,
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
        });
        sounds[orderedTones[orderedTones.length - 2]] = sounds[key];
      } else {
        sounds[key]?.sound?.kill?.(0.25); // Kill a sound if it exists.
      }

      delete tonestack[key]; // Remove this key from the notestack.
      delete sounds[key];

      if (downs[key]) {
        delete downs[key];
        if (buttons[key]) buttons[key].down = false;
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
  hold = sharps = flats = false;
}

// Initialize and/or lay out the UI buttons on the bottom of the display.
function setupButtons({ ui, screen, geo }) {
  const margin = 6;
  const buttonWidth = (screen.width - margin * 2) / 4;
  const buttonHeight = buttonWidth;
  const buttonsPerRow = 4;
  const totalButtons = buttonNotes.length;
  const totalRows = Math.ceil(totalButtons / buttonsPerRow);
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