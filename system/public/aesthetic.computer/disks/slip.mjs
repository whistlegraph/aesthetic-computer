// Slip, 2024.8.20.18.40.34.708
// A single voice instrument.

/* 📝 Notes 
  - [] add on-screen support for switching wave types?
  - [] Show a good minimap of current note etc.
  - [] Add pitch draggability across two octaves with proper notching.
  - [] Add a visualizer graph.
  - [] Add a two column / comparison mode?
  + Done
  - [x] Get the basic tone interaction right.
  - [x] Make volume go down when further from the center.
  - [x] Create the tone while tapping.
*/

const notes = ["", "+"].flatMap((octave) =>
  ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"].map(
    (note) => octave + note,
  ),
);

const { abs, min, floor, pow } = Math;
const top = 20;
const margin = 14;
const stretch = notes.length;
let octave = "4";
let voice,
  wave = "sine",
  currentNote,
  currentVolume = 0,
  height,
  section;

function boot({ colon }) {
  const wavetypes = ["square", "sine", "triangle", "sawtooth", "noise-white"];
  wave = wavetypes.indexOf(colon[0]) > -1 ? colon[0] : wave;
  const newOctave = parseInt(colon[0]) || parseInt(colon[1]);
  if (newOctave) octave = newOctave.toString();
}

function paint({ wipe, ink, line, screen, pen, num, help: { choose } }) {
  wipe("gray");
  height = screen.height - top - 8;
  section = Math.round(height / stretch);
  const hs = Math.round(section / 2);

  notes.forEach((note, i) => {
    const y = floor(top + i * section + hs);
    const sub = pen ? min(255, floor(pow(abs(pen.y - y), 1.2))) : 0;

    const caps = note.toUpperCase();
    const x = 6; //screen.width / 2 - (caps.length * 6) / 2;

    if (note !== currentNote) {
      if (!note.endsWith("#")) {
        ink(200, 180, 100, 100).box(0, y - hs + 2, screen.width, section - 2);
      }

      if (pen) {
        const cutoff = 140;
        if (sub < cutoff) {
          ink("white", 255 - sub).write(caps, x, y - 5);
        } else {
          ink("pink", 255 - cutoff).write(caps, x, y - 5);
        }
      } else {
        ink("white").write(caps, x, y - 5);
      }

      const sub2 = pen ? min(255, floor(pow(abs(pen.y - y), 1.4))) : 0;
      if (pen && sub2 < 255) ink("yellow", 255 - sub2).write(caps, x, y - 5);
      // if (voice && sub2 < 255) ink("yellow", 255 - sub2).write(caps, 6, y - 5);
      // if (voice) ink("white").write(currentNote, 6, 18);
    } else {
      ink(200, 200, 100).box(0, y - hs + 2, screen.width, section - 2);

      ink("lime", 128).write(
        caps,
        x + choose(-1, 0, 1),
        y - 5 + choose(-1, 0, 1),
      );
      ink("red", 128).write(
        caps,
        x + choose(-1, 0, 1),
        y - 5 + choose(-1, 0, 1),
      );
      ink("white").write(caps, x + choose(-1, 0, 1), y - 5 + choose(-1, 0, 1));
    }

    ink("lime", 64).line(0, y - hs + 1, screen.width, y - hs + 1);
    if (i !== 0) ink("red", 64).line(0, y - hs, screen.width, y - hs);

    if (notes.length - 1 === i) {
      ink("red", 64).line(0, y + hs - 1, screen.width, y + hs - 1);
    }
  });

  ink("cyan", 64 + currentVolume * 128).line(
    screen.width / 2,
    0,
    screen.width / 2,
    screen.height,
  );

  if (pen)
    ink(voice ? "magenta" : "yellow", 64 + currentVolume * 128).line(
      12 + margin,
      pen.y,
      screen.width - margin,
      pen.y,
    );
}

function act({ event: e, sound, screen }) {
  // Respond to user input here.
  if (e.is("touch:1")) {
    let tone = mapNote(e);
    currentNote = tone;
    if (tone.startsWith("+")) {
      tone = tone.replace("+", parseInt(octave) + 1);
    } else {
      tone = octave + tone;
    }

    // TODO: Figure out volume based on x.
    // TODO: Should the layout be in a central column?

    const hw = screen.width / 2;
    const volume = (currentVolume = pow(1 - abs(hw - e.x) / hw, 0.8) || 0);

    voice = sound.synth({
      type: wave,
      tone,
      volume,
      attack: 0.005,
      duration: "🔁",
    });
  }

  if (e.is("draw:1")) {
    if (voice) {
      let tone = mapNote(e);
      currentNote = tone;
      if (tone.startsWith("+")) {
        tone = tone.replace("+", parseInt(octave) + 1);
      } else {
        tone = octave + tone;
      }
      const hw = screen.width / 2;
      const volume = (currentVolume = pow(1 - abs(hw - e.x) / hw, 0.8) || 0);
      voice.update({ tone, volume, duration: 0.05 });
    }
  }

  if (e.is("lift:1")) {
    voice?.kill(0.35);
    currentNote = null;
    voice = null;
  }
}

// 📚 Library

function mapNote(e) {
  const hs = Math.round(section / 2);
  const firstY = floor(top + hs); // Check if below the first,
  if (e.y < firstY) return notes[0];
  const lastY = floor(top + (notes.length - 1) * section + hs); // Or past last
  if (e.y > lastY) return notes[notes.length - 1];
  // Or inside...
  for (let i = 0; i < notes.length; i += 1) {
    const y = floor(top + i * section + hs);
    if (abs(e.y - y) < hs) return notes[i];
  }
  return "6A"; // Unfound tone.
}

// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// function beat() {
//   // Runs once per metronomic BPM.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

// ⚠️ Also available: `brush` and `filter`.
