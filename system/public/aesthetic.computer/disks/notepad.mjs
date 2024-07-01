// Notepad, 2024.6.26.23.17.58.736
// Touch pads that play musical notes, or use the keyboard keys.

/* 📝 Notes 
  TODO
  - [🟠] Add visualization to the keys being pressed.
  - [] Add the ability to run through a series of notes / an existing song.
  - [] Show some form of display / color coded display to show octave state and also print the last note pressed. 
  - [] Show a melody so that a typing game can take place.
  - [] Abstract this so shift adds the 5.
  - [] Leave out all options from synth / make sensible defaults first.
  - [] Add visual buttons.
  + Done
  - [x] Disable key repeat / don't retrigger sounds on repeat.
  - [x] Make it so keys can be held, and add a decay after releasing?
*/

function paint({ wipe }) {
  wipe("blue");
}

let octave = 4;
const sounds = {};

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
