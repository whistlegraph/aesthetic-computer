// Notepad, 2024.6.26.23.17.58.736
// Touch pads that play musical notes, or use the keyboard keys.

function paint({ wipe }) {
  wipe("blue");
}

let octave = "4"; // event.alt ? "6" : event.shift ? "5" : "";

function act({ event, sound: { synth } }) {
  // ⌨️ Keyboad Shortcuts

  // TODO: - [] Abstract this so shift adds the 5.
  //       - [] Leave out all options from synth / make sensible defaults first.
  //       - [] Add visual buttons.
  //       - [] Disable key repeat.

  const volume = 1;
  const decay = 0.9;
  const duration = 0.25;
  const attack = 0.01;

  if (event.is("keyboard:down:3")) octave = "3";
  if (event.is("keyboard:down:4")) octave = "4";
  if (event.is("keyboard:down:5")) octave = "5";
  if (event.is("keyboard:down:6")) octave = "6";

  if (event.is("keyboard:down:a")) {
    synth({ tone: "a" + octave, duration, attack, decay, volume });
  }

  if (event.is("keyboard:down:b")) {
    synth({ tone: "b" + octave, duration, attack, decay, volume });
  }

  if (event.is("keyboard:down:c")) {
    synth({ tone: "c" + octave, duration, attack, decay, volume });
  }

  if (event.is("keyboard:down:d")) {
    synth({ tone: "d" + octave, duration, attack, decay, volume });
  }

  if (event.is("keyboard:down:e")) {
    synth({ tone: "e" + octave, duration, attack, decay, volume });
  }

  if (event.is("keyboard:down:f")) {
    synth({ tone: "f" + octave, duration, attack, decay, volume });
  }

  if (event.is("keyboard:down:g")) {
    synth({ tone: "g" + octave, duration, attack, decay, volume });
  }
}
