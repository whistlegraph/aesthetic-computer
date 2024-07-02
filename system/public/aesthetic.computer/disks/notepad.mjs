// Notepad, 2024.6.26.23.17.58.736
// Touch pads that play musical notes, or use the keyboard keys.

/* 📝 Notes 
  TODO
  - [🔶] show a history of keys pressed... (including octave)
  - [] be able to turn that history into a 'track' that can be followed
  - [] be able to send that track / share it with others (via the first parameter) and the 'share' swipe that could pre-fill it.
  - [] use other letters on the keyboard for sfx
  - [] show octave in the text
  - [] show audio connectedness more visibly 
       like with a modal or 'press any key'
       or 'inactive' pause curtain situation
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
  - [] Add visual buttons.
  + Done
  - [x] Add visualization to the keys being pressed.
  - [x] Disable key repeat / don't retrigger sounds on repeat.
  - [x] Make it so keys can be held, and add a decay after releasing?
*/

let octave = 4;
let keys = "";
const sounds = {};

function paint({ wipe, ink, write, text: { reverse }, screen }) {
  wipe("blue");
  const active = Object.keys(sounds);
  active.forEach((sound, index) => {
    ink("yellow");
    write(sound, 6, 20); // TODO: Add 'scale' option and also add rotation.
  });

  ink("teal");
  write(keys, 6, 20 + 12, { bounds: screen.width - 12, wordWrap: false });
  ink("yellow");
  write(
    keys.replace(new RegExp(`[^${active.join("")}]`, "g"), " "),
    6,
    20 + 12,
    { bounds: screen.width - 12, wordWrap: false },
  );
}

function act({ event: e, sound: { synth } }) {
  // ⌨️ Keyboad Shortcuts
  const volume = 1,
    decay = 0.9,
    duration = Infinity, //0.25,
    attack = 0.01;

  "123456789".split("").forEach((digit) => {
    if (e.is(`keyboard:down:${digit}`)) octave = parseInt(digit);
  });

  const shift = e.shift ? 1 : e.alt ? -1 : 0;
  const accent = e.ctrl ? "#" : ""; /*e.;*/

  "abcdefg".split("").forEach((key) => {
    if (e.is(`keyboard:down:${key}`) && !sounds[key]) {
      const tone = key + accent + (octave + shift);
      keys += key;
      sounds[key] = synth({
        type: "square",
        tone,
        duration,
        attack,
        decay,
        volume, // TODO: <- Rename to 'level'.
      });
      console.log(sounds[key]);
    }

    if (e.is(`keyboard:up:${key}`)) {
      sounds[key]?.kill(0.25);
      delete sounds[key];
    }
  });
}
