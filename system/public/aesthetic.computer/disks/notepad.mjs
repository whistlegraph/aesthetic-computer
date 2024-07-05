// Notepad, 2024.6.26.23.17.58.736
// Touch pads that play musical notes, or use the keyboard keys.

/* 🎶 Sequences
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

🚫 🧸 Gummy Bear
  aaa5c4aa aaa5cee
  eee dd dd dd dd
  edca g5c4a
  aa ee aa ee aa e
  5ee ee e dc
  5ee ee e dc
  ge ded4c
  dca gca
  bbb d bb
  bbb d f#
*/

/* 📝 Notes 
  - [⭐] Merge this piece with `song`.
  - [] Add support for sharps and flats using normal keys somehow...
    - [] And they can be encoded differently.
    - [] Also add support for microtonal elements. 
    - [] How would the interpreter work for these, because it only reads per
         character...
  - [🤩] add a 'repeat' or 'hold' key which should be 'shift' on the keyboard
  - [] why are some wave type switches changing the frequency / octave?
  - [] how to build commas or sections in?
  - [🟢] how can i "lightly" trigger the soft keyboard without a whole
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

let octave = STARTING_OCTAVE;
let keys = "";

let tap = false;
let tapIndex = 0;
let tapped; // Store the last tracked key.
let editable = true;

const sounds = {};

function boot({ params, colon }) {
  // console.log("Params", params); // TODO: params are blank in dev at times...
  keys = params.join(" ") || "";
  // Dumb way to replace white-space:
  console.log("Keys!", keys, params);
  keys = keys.replace(/\s/g, "");
  if (keys.length > 0) {
    tap = true;
    editable = false;
  }
  wave = colon[0] || wave;
}

function paint({ wipe, ink, write, screen }) {
  const active = Object.keys(sounds);
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
    write(keys, 6, 20 + 12, { bounds: screen.width - 12, wordWrap: false });
    ink("yellow");
    write(
      keys.replace(new RegExp(`[^${active.join("")}]`, "g"), " "),
      6,
      20 + 12,
      { bounds: screen.width - 12, wordWrap: false },
    );
  } else {
    ink("gray");
    write(keys, screen.width / 2 - tapIndex * 6, screen.height / 2);
    ink("red");
    write(keys[tapIndex], screen.width / 2, screen.height / 2);
  }

  if (!tap) {
    ink("lime");
    active.forEach((sound, index) => {
      write(sound, 6 + index * 6, 20);
    });
  } else {
    ink("white");
    active.forEach((sound, index) => {
      write(sound, screen.width / 2, screen.height / 2 + 12 + 12 * index);
    });
    ink("lime");
    if (tapped === keys[tapIndex]) {
      write(tapped, screen.width / 2, screen.height / 2);
    }
  }
}

function act({ event: e, sound: { synth } }) {
  const notes = "abcdefg";
  const octaves = "123456789";

  if (e.is("keyboard:down:shift") && !e.repeat) hold = true;
  if (e.is("keyboard:up:shift")) hold = false;

  if (editable && e.is("keyboard:down:tab") && !e.repeat) {
    tap = !tap;
    tapped = undefined;
    tapIndex = 0;
    octave = STARTING_OCTAVE;
  }

  // Clear Track
  if (editable && e.is("keyboard:down:0") && !e.repeat) {
    keys = "";
    tap = false;
    tapped = undefined;
    tapIndex = 0;
    octave = STARTING_OCTAVE;
  }

  if (tap) {
    if ((e.is("keyboard:down:space") || e.is("touch")) && !sounds[tapped]) {
      tapped = keys[tapIndex];
      let reset = false;
      //if (!hold) {
      // TODO: ❤️‍🔥 Fix this.
        if (octaves.indexOf(tapped) > -1) {
          octave = tapped;
          tapIndex += 1;
          if (tapIndex >= keys.length) {
            tapIndex = 0;
            reset = true;
            octave = STARTING_OCTAVE;
          }
          tapped = keys[tapIndex];
       // }
      }

      if (!reset)
        sounds[tapped] = synth({
          type: wave,
          tone: `${tapped}${octave}`,
          duration: "🔁",
        });
    }

    if (e.is("keyboard:up:space") || e.is("lift")) {
      tapIndex += 1;
      if (tapIndex >= keys.length) {
        octave = STARTING_OCTAVE;
        tapIndex = 0;
      }
      sounds[tapped]?.kill(0.25);
      delete sounds[tapped];
      tapped = undefined;
    }
  }

  (octaves + notes).split("").forEach((key) => {
    if (e.is(`keyboard:down:${key}`) && !e.repeat) {
      if (!tap) {
        keys += key;
      } else if (keys[tapIndex] === key) {
        tapped = key;
      }

      if (octaves.indexOf(key) > -1) {
        octave = parseInt(key);
        sounds[key] = "held";
      } else {
        sounds[key] = synth({
          type: wave,
          tone: `${key}${octave}`,
          duration: "🔁",
        });
      }
    }

    if (e.is(`keyboard:up:${key}`)) {
      if (tap && tapped === key) {
        tapIndex = (tapIndex + 1) % keys.length;
        tapped = undefined;
      }
      sounds[key]?.kill?.(0.25); // Kill a sound if it exists.
      delete sounds[key];
    }
  });
}
