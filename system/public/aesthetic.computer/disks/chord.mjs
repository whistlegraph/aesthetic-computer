// Chord, 2024.1.14.15.11.17.218 🎼
// Play a musical chord.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🟠] Draggable user interaction
       for the chord. 
  - [x] Add parameters for chord generation.
#endregion */

const chord = ["6G", "6B", "6D"];

// 🥾 Boot
function boot({ wipe, params }) {
  // Try to populate the chord with the params.
  if (params.length > 0) {
    chord.length = 0;
    params.forEach((note) => chord.push(note));
  }
}

// 🎨 Paint
function paint({ wipe, ink, line, screen }) {
  wipe("gray");
  // TODO: Set up bounds.
  const bounds = {
    x: 16,
    y: 32,
    width: screen.width - 32,
    height: screen.height - 64,
  };
  ink(255, 0, 0, 64).box(16, 32, screen.width - 32, screen.height - 64);
  ink("lime");
  chord.forEach((note, index) => {
    const y = ((screen.height - 32) / chord.length) * index + 32;
    line(16, y, screen.width - 16, y);
    ink("yellow").write(note, { x: 16, y: y + 3 });
  });
}

// 🎪 Act
function act({ event: e, sound }) {
  // TODO: Play a chord.
  if (e.is("touch")) {
    chord.forEach((note) => {
      sound.synth({
        type: "sine",
        tone: sound.freq(note),
        volume: 1.0,
        duration: 0.1,
      });
    });
  }
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

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

export { boot, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
