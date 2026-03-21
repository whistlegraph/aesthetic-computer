// Colplay, 2023.9.10.20.13.47.216
// A song as program.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Give the whole composition stretchy delta.
#endregion */

let rects = [];
let started = true;// false;
let count = 0n;
let phase = 1;

// 🥾 Boot
function boot({ wipe, ink, line }) {
  // Runs once at the start.
  wipe(0);
}

// 🎨 Paint
function paint({
  wipe,
  ink,
  sound: { synth },
  num: { randInt: r, randIntRange: rr, randIntArr: rra, rgbToHsl },
  help: { choose },
  screen,
  paintCount,
}) {
  // Executes every display frame.
  wipe(0);
  if (!started) return;

  const end = 40n;

  if (count > 0n && count < 5n) phase = 1;
  if (count > 10n && count < 20n) phase = 2;
  if (count > 20n && count < 30n) phase = 3;
  if (count > 30n && count < end) phase = 4;
  if (count > end) count = 0n; // Loop the piece.
  count += 1n;


  let doorway = 950;
  if (phase === 1) doorway = 900;
  if (phase === 2) doorway = 100;
  if (phase === 3) doorway = 900;
  if (phase === 4) doorway = 100;

  let size = rr(4, 128);

  if (phase === 4) size = rr(20, 40);
  if (phase === 3) size = 48;

  if (r(1000) > doorway) {
    let color = rra(255, 3);
    if (phase === 1) color = choose([255, 0, 0], [0, 0, 255], [0, 255, 0]);
    if (phase === 2) color = [255, 0, 0];
    if (phase === 3) color = choose([255, 255, 255], [0, 255, 255]);
    if (phase === 4) color = [r(128), r(128), r(128)];

    const rect = {
      x: r(screen.width),
      y: r(screen.height),
      width: size,
      height: size,
      color,
      sustain: size,
    };

    rect.sustainCount = rect.sustain;
    rects.push(rect);

    // TODO: How to I get the HSL value here out of
    // rect.color if it's an array of [R, G, B] from 0-255?
    const hsl = rgbToHsl(...rect.color);
    console.log(paintCount, hsl);

    synth({
      type: "sine",
      tone: phase * 100 + hsl[0],
      attack: 0.001,
      decay: 0.96,
      volume: 0.5 + (hsl[2] / 100) / 2,
      pan: 1 - (rect.x / screen.width) * 2,
      duration: (rect.sustain / 60) * 0.8,
    });
  }

  rects.forEach((rect) => {
    rect.sustainCount -= 1;
    if (rect.sustainCount > 0) {
      const p = rect.sustainCount / rect.sustain;
      ink(...rect.color, p * 255).box(
        rect.x,
        rect.y,
        rect.width * p,
        rect.height * p,
        "*center",
      );
    } else {
      ink(255, 128).box(rect.x, rect.y, 1);
    }
  });

  // rects = rects.filter((rect) => rect.sustainCount > 0); // Cut dead rects.
}

// 🎪 Act
function act({ event }) {
  // if (event.is("touch")) started = true;
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
    title: "Colplay",
    desc: "A song as program.",
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

export { act, boot, paint, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
