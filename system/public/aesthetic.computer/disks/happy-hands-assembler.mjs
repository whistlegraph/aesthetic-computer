// Happy Hands Assembler, 23.04.24.15.02
// Let's make a some happy hands!

/* #region 🤝 Read Me 
#endregion */

import { radians } from "../lib/num.mjs";

const boxSize = 5;
const boxType = "fill*center";

// Crawl a point {x, y} dist amount in a direction, returning the new position.
function crawl(p, dist, dir) {
  dir = radians(dir);
  return { x: p.x + dist * Math.cos(dir), y: p.y + dist * Math.sin(dir) };
}

// Generate points for a digit given an orientation (deg).
function digit(from, segCount, deg = 0) {
  deg -= 90; //set the orientation of 0 to up
  const segs = [];
  const gap = 11;
  for (let s = 0; s < segCount; s += 1) {
    if (s > 0) {
      deg += 0;
      segs.push(crawl(segs[s - 1], gap, deg));
    } else {
      segs.push(crawl(from, gap, deg));
    }
  }
  return segs;
}

//hand structure
const w = [
  { x: 0, y: 0, z: 0 },
  { x: -10 / 4, y: -40, z: 0 },
  { x: -10 / 4 + 14, y: -40, z: 0 },
  { x: -10 / 4 + 27, y: -40, z: 0 },
  { x: -10 / 4 + 38, y: -35, z: 0 },
];

const hand = {
  w,
  t: digit(w[0], 4, -30),
  i: digit(w[1], 3, -8),
  m: digit(w[2], 3, 0),
  o: digit(w[3], 3, 7),
  p: digit(w[4], 3, 20),
};

const handPalette = {
  w: "#FFFFFFFF",
  t: [0, 170, 200], //teal
  i: [75, 0, 130], //indigo
  m: "magenta",
  o: "orange",
  p: "pink", //pink
};

const nudge = 10;
const hnudge = nudge / 4;

const hands = 1024; // How many happy hands exist in total?
const key = "happy-hand-assembler:hand"; // Keep track of current hand index.

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ wipe, params, screen, store }) {
  let h = parseInt(params[0]);

  if (isNaN(h)) {
    const stored = await store.retrieve(key, "local:db");
    h = stored;
  }

  if (h === null || isNaN(h) || h === undefined || h < 0 || h > hands - 1) {
    console.warn("👎 Hand Not Found:", h);
    wipe(100, 0, 0)
      .ink(120, 0, 0)
      .line(0, 0, screen.width, screen.height)
      .line(0, screen.height, screen.width, 0);
  } else {
    // 🤚 We have a hand!
    wipe(0, 64, 0)
      .ink(0, 255, 0, 128)
      .write(h, { x: 4, y: screen.height - 13 });
    store[key] = h;
    store.persist(key, "local:db");
  }
}
// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, box, line, pan, unpan, screen }) {
  wipe(0); // draw bg
  pan(screen.width / 2 - 20, screen.height / 2 + 40); // shift view

  // 🅱️ Hand Lines
  // ...

  // 🅰️ Hand Points
  ink(handPalette.w); // wrist
  for (let coord of hand.w) box(coord.x, coord.y, boxSize, boxType);
  ink(handPalette.t); //thumb
  for (let coord of hand.t) box(coord.x, coord.y, boxSize, boxType);
  ink(handPalette.i); // index
  for (let coord of hand.i) box(coord.x, coord.y, boxSize, boxType);
  ink(handPalette.m); // middle
  for (let coord of hand.m) box(coord.x, coord.y, boxSize, boxType);
  ink(handPalette.o); // ring (o)
  for (let coord of hand.o) box(coord.x, coord.y, boxSize, boxType);
  ink(handPalette.p); // pinky
  for (let coord of hand.p) box(coord.x, coord.y, boxSize, boxType);
  unpan();
}

export { boot, paint };

// 📚 Library (Useful functions used throughout the piece)
// ...

/*
// ✒ Act (Runs once per user interaction)
function act({ event }) {
  // Respond to user input here.
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  // Crunch numbers outside of rendering here.
}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // Make sound here.
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave($api) {
  // Pass data to the next piece here.
}
*/
