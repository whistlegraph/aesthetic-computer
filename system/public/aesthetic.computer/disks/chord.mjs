// Chord, 2024.1.14.15.11.17.218 🎼
// Play a musical chord.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🟠] Parse octave after the note and make sure
         `#` sharps work function in urls using an `s`
         replacement.
         Also allow `f` and `b` and `s` synonyms.
  - [x] Add parameters for chord generation.
  - [x] Draggable user interaction
       for the chord. 
#endregion */

const chord = ["G6", "B6", "D6"]; // Musical information (strings).
const graph = { frame: null, chords: {} }; // On-screen geometry (via `layout`).
const pluck = []; // Input "plucking" gesture.

// 🥾 Boot
function boot({ params, screen, net }) {
  if (params.length > 0) {
    chord.length = 0; // Populate chord via params.
    params.forEach((note) => chord.push(note));
    // Re-write the url replacing all "#" with "s".
    // net.rewrite("chord~", +params.join("~").replaceAll("#", "s"));
  }
  layout({ screen });
}

// 🎨 Paint
function paint({ wipe, ink, text }) {
  wipe("gray");
  ink(255, 0, 0, 32).box(graph.frame);
  chord.forEach((note) => {
    const string = graph.chords[note];
    if (!string) return;
    ink("lime").line(string);
    ink(string.lit ? "lime" : "yellow").write(note, {
      x: string.x0 - text.box(note).box.width / 4 + 1,
      y: string.y0 + graph.frame.h + 3,
    });
  });
  if (pluck.length > 1) ink("maroon", 128).line(pluck[0], pluck[1]);
}

// 🎪 Act
function act({
  event: e,
  sound: { synth, freq },
  screen,
  num: { intersects },
}) {
  // Play a chord with a single touch.
  if (e.is("touch")) {
    chord.forEach((note) => {
      synth({ type: "sine", tone: freq(note), volume: 1.0, duration: 0.1 });
    });
    pluck.length = 0;
    pluck.push({ x: e.x, y: e.y });
  }

  if (e.is("draw")) {
    if (pluck.length > 1) pluck.length = 1;
    pluck.push({ x: e.x, y: e.y });

    // Check if any line in graph.chords intersects with the pluck line
    if (pluck.length > 1) {
      const pl = {
        x0: pluck[pluck.length - 2].x,
        y0: pluck[pluck.length - 2].y,
        x1: pluck[pluck.length - 1].x,
        y1: pluck[pluck.length - 1].y,
      };

      for (const note in graph.chords) {
        const s = graph.chords[note];
        if (intersects(pl, s)) {
          s.sound = synth({
            type: "sine",
            tone: freq(note),
            volume: 1.0,
            duration: 0.15,
          });
          s.lit = true;
          pluck.length = 0;
          pluck.push({ x: e.x, y: e.y });
        }
      }
    }
  }

  if (e.is("lift")) pluck.length = 0;
  if (e.is("reframed")) layout({ screen }); // Redistribute strings on resize.
}

// 🧮 Sim
function sim({ sound: { time } }) {
  for (const note in graph.chords) {
    const s = graph.chords[note];
    if (s.sound) {
      if (s.sound.progress(time) === 1) {
        s.lit = false;
      }
    }
  }
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Chord",
    desc: "Play a musical chord.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, act, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)

// Visually layout the strings and store the geometry for `paint` and `act`.
function layout({ screen }) {
  const m = 32, // Margins
    m2 = m * 2;
  const frame = { x: m, y: m, w: screen.width - m2, h: screen.height - m2 };
  const stripe = frame.w / chord.length; // Center strings within the bounds.
  const width = stripe * (chord.length - 1);
  const startX = frame.x + (frame.w - width) / 2; // Offset all stripes.

  chord.forEach((note, index) => {
    const x = startX + stripe * index,
      y = frame.y;
    graph.chords[note] = { x0: x, y0: y, x1: x, y1: frame.y + frame.h };
  });

  graph.frame = frame;
}
