// Slip, 2024.8.20.18.40.34.708
// A single voice instrument.

/* 📝 Notes 
  - [] Get the basic tone interaction right.
  - [] Make volume go down when further from the center.
  - [] Show a good minimap of current note etc.
  - [] Add pitch draggability across two octaves with proper notching.
  - [] Add a visualizer graph.
  - [] Add a two column / comparison mode?
  - [] add on-screen support for switching wave types?
  + Done
  - [x] Create the tone while tapping.
*/

const notes = ["", "+"].flatMap((octave) =>
  ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"].map(
    (note) => octave + note,
  ),
);

const { abs, min, floor, pow } = Math;
const top = 24;
const margin = 14;
const stretch = notes.length;
let octave = "4";
let voice, currentNote, height, section;

function boot({ colon }) {
  octave = colon[0] || "4";
}

function paint({ wipe, ink, line, screen, pen, num, help: { choose } }) {
  wipe(voice ? 150 : "gray");
  height = screen.height - top - 8;
  section = height / stretch;

  notes.forEach((note, i) => {
    const y = floor(top + i * section + section / 2);
    const sub = pen ? min(255, floor(pow(abs(pen.y - y), 1.2))) : 0;

    const caps = note.toUpperCase();

    if (note !== currentNote) {
      const cutoff = 160;
      if (sub < cutoff) {
        ink("white", 255 - sub).write(caps, 6, y - 5);
      } else {
        ink("pink", 255 - cutoff).write(caps, 6, y - 5);
      }

      const sub2 = pen ? min(255, floor(pow(abs(pen.y - y), 1.4))) : 0;
      if (sub2 < 255) ink("yellow", 255 - sub2).write(caps, 6, y - 5);
    } else {
      ink("lime", 128).write(
        caps,
        6 + choose(-1, 0, 1),
        y - 5 + choose(-1, 0, 1),
      );
      ink("red", 128).write(
        caps,
        6 + choose(-1, 0, 1),
        y - 5 + choose(-1, 0, 1),
      );
      ink("white").write(caps, 6 + choose(-1, 0, 1), y - 5 + choose(-1, 0, 1));
    }

    // ink("white", 128).line(12 + margin, y, screen.width - 12, y);

    ink("lime", 64).line(
      0,
      y - section / 2 + 1,
      screen.width - 12,
      y - section / 2 + 1,
    );
    ink("red", 64).line(0, y - section / 2, screen.width - 12, y - section / 2);
  });

  if (pen) ink("white").line(12 + margin, pen.y, screen.width - 12, pen.y);
}

function act({ event: e, sound }) {
  // Respond to user input here.
  if (e.is("touch:1")) {
    let tone = mapNote(e);
    currentNote = tone;

    if (tone.startsWith("+")) {
      tone = tone.replace("+", parseInt(octave) + 1);
    } else {
      tone = octave + tone;
    }

    voice = sound.synth({ type: "sine", tone, attack: 0.005, duration: "🔁" });
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
      voice.update({ tone, duration: 0.1 });
    }
  }

  if (e.is("lift:1")) {
    voice?.kill();
    currentNote = null;
    voice = null;
  }
}

// 📚 Library

function mapNote(e) {
  const hs = section / 2;
  const firstY = floor(top + hs); // Check if below the first,
  if (e.y < firstY) return notes[0];
  const lastY = floor(top + (notes.length - 1) * section + hs); // Or past last
  if (e.y > lastY) return notes[notes.length - 1];
  // Or inside...
  for (let i = 0; i < notes.length; i += 1) {
    const y = floor(top + i * section + hs);
    if (abs(e.y - y) < hs) return notes[i];
  }
  return "A3"; // Unfound tone.
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
