// Notepad, 2024.6.26.23.17.58.736
// Touch pads that play musical notes, or use the keyboard keys.

/* 📝 Notes 
  TODO
    + Done
  - [] holding tab should not auto-switch, same with 0!
  - [🟡] clean up digit vs note triggering code...
  - [] entering '0' should clear the params in the hud (re-enable editing)
  - [] be able to send that track / share it with others (via the first parameter) and the 'share' swipe that could pre-fill it.
  - [] use other letters on the keyboard for sfx
  - [] show octave in the text
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

let octave = 4;
let keys = "";

let track = false;
let trackIndex = 0;
let tracked; // Store the last tracked key.
let editable = true;

const sounds = {};

function boot({ params }) {
  // console.log("Params", params); // TODO: Why is params empty in dev?
  keys = params[0] || "";
  if (keys.length > 0) {
    track = true;
    editable = false;
  }
}

function paint({ wipe, ink, write, screen }) {
  wipe(!track ? "blue" : "darkblue");

  if (track) {
    ink("yellow");
    write("track", { right: 6, top: 6 });
  }

  const active = Object.keys(sounds);

  if (!track) {
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
    write(keys, screen.width / 2 - trackIndex * 6, screen.height / 2);
    ink("red");
    write(keys[trackIndex], screen.width / 2, screen.height / 2);
  }

  if (!track) {
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
    if (tracked === keys[trackIndex]) {
      write(tracked, screen.width / 2, screen.height / 2);
    }
  }
}

function act({ event: e, sound: { synth } }) {
  // ⌨️ Keyboard Shortcuts
  if (editable && e.is("keyboard:down:tab")) track = !track;
  if (editable && e.is("keyboard:down:0")) {
    keys = "";
    track = false;
    tracked = undefined;
    trackIndex = 0;
    octave = 4;
  }

  if (track) {
    if ((e.is("keyboard:down:space") || e.is("touch")) && !sounds[tracked]) {
      tracked = keys[trackIndex];
      sounds[tracked] = synth({
        type: "square",
        tone: `${tracked}${octave}`,
        duration: Infinity,
        attack: 0.01,
        decay: 0.9,
        volume: 1, // TODO: <- Rename to 'level'.
      });
    }
    if (e.is("keyboard:up:space") || e.is("lift")) {
      trackIndex = (trackIndex + 1) % keys.length;
      sounds[tracked]?.kill(0.25);
      delete sounds[tracked];
      tracked = undefined;
    }
  }

  "123456789".split("").forEach((key) => {
    if (e.is(`keyboard:down:${key}`) && !sounds[key]) {
      if (!track) {
        keys += key;
      } else if (keys[trackIndex] === key) {
        tracked = key;
        //if (tracked !== key) {
        // TODO: Flash red and make a dud sound or something?
        //  return;
        // }
      }
      // console.log("🐾 Tracked:", tracked);

      octave = parseInt(key);
      sounds[key] = "held";
    }

    if (e.is(`keyboard:up:${key}`)) {
      if (track && tracked === key) {
        trackIndex = (trackIndex + 1) % keys.length;
        tracked = undefined;
      }
      delete sounds[key];
    }
  });

  // Deprecated: Shift won't show up / work in mobile.
  // const shift = e.shift ? 1 : e.alt ? -1 : 0;
  // const accent = e.ctrl ? "#" : ""; /*e.;*/

  "abcdefg".split("").forEach((key) => {
    if (e.is(`keyboard:down:${key}`) && !sounds[key]) {
      if (!track) {
        keys += key;
      } else if (keys[trackIndex] === key) {
        tracked = key;
        //if (tracked !== key) {
        // TODO: Flash red and make a dud sound or something?
        //  return;
        //}
      }
      // console.log("🐾 Tracked:", tracked);

      sounds[key] = synth({
        // type: "square",
        tone: `${key}${octave}`,
        duration: Infinity, // TODO: Why is the default duration so weird?
        // attack: 0.01,
        // decay: 0.9,
        // volume: 1, // TODO: <- Rename to 'level'.
      });
    }

    if (e.is(`keyboard:up:${key}`)) {
      if (track && tracked === key) {
        trackIndex = (trackIndex + 1) % keys.length;
        tracked = undefined;
      }
      sounds[key]?.kill(0.25);
      delete sounds[key];
    }
  });
}
