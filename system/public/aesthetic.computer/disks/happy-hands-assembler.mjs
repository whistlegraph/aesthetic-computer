// Happy Hands Assembler, 23.04.24.15.02
// Let's make a some happy hands!

/* #region 🤝 Read Me 
#endregion */

import { radians } from "../lib/num.mjs";

const hands = 1024; // How many happy hands exist in total?
const key = "happy-hand-assembler:hand"; // Keep track of current hand index.
const origin = { x: 0, y: 0, z: 0 }; //wrist
const handPalette = {
  w: "#FFFFFFFF",
  t: [0, 170, 200], //teal
  i: [75, 0, 130], //indigo
  m: "magenta",
  o: "orange",
  p: "pink", //pink
};

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
function paint({ wipe, ink, box, line, pan, unpan, screen, pen, paintCount }) {
  const osc = Math.sin(paintCount*0.1);
  // Build hand geometry
  const w = [
    origin,
    crawl(origin, 40 + 2*osc, 10),
    crawl(origin, 45+ -2*osc, 25),
    crawl(origin, 50+ 2*osc, 40),
    crawl(origin, 55+ -2*osc, 55),
  ];

  const hand = {
    w,
    t: digit(w[0], 4, -30, -10*osc),
    i: digit(w[1], 3, -8, -10*osc),
    m: digit(w[2], 3, 0, -10*osc),
    o: digit(w[3], 3, 7, -10*osc),
    p: digit(w[4], 3, 20, -10*osc),
  };

  // Render
  wipe(0); // draw bg

  const o = { x: -24 + 2*osc, y: 16 + 2*osc };
  pen
    ? pan(pen.x + o.x, pen.y + o.y)
    : pan(screen.width / 2 + o.x, screen.height / 2 + o.y);

  // 🅱️ Hand Lines
  ink(handPalette.t).line(hand.w[0].x, hand.w[0].y, hand.t[0].x, hand.t[0].y);
  ink(handPalette.t).line(hand.t[0].x, hand.t[0].y, hand.t[1].x, hand.t[1].y);
  ink(handPalette.t).line(hand.t[1].x, hand.t[1].y, hand.t[2].x, hand.t[2].y);
  ink(handPalette.t).line(hand.t[2].x, hand.t[2].y, hand.t[3].x, hand.t[3].y);


  ink(handPalette.w).line(hand.w[0].x, hand.w[0].y, w[1].x, w[1].y);
  ink(handPalette.i).line(w[1].x, w[1].y, hand.i[0].x, hand.i[0].y);
  ink(handPalette.i).line(hand.i[0].x, hand.i[0].y, hand.i[1].x, hand.i[1].y);
  ink(handPalette.i).line(hand.i[1].x, hand.i[1].y, hand.i[2].x, hand.i[2].y);

  ink(handPalette.w).line(w[2].x, w[2].y, w[1].x, w[1].y);
  ink(handPalette.m).line(w[2].x, w[2].y, hand.m[0].x, hand.m[0].y);
  ink(handPalette.m).line(hand.m[0].x, hand.m[0].y, hand.m[1].x, hand.m[1].y);
  ink(handPalette.m).line(hand.m[1].x, hand.m[1].y, hand.m[2].x, hand.m[2].y);


  ink(handPalette.w).line(w[3].x, w[3].y, w[2].x, w[2].y);
  ink(handPalette.o).line(w[3].x, w[3].y, hand.o[0].x, hand.o[0].y);
  ink(handPalette.o).line(hand.o[0].x, hand.o[0].y, hand.o[1].x, hand.o[1].y);
  ink(handPalette.o).line(hand.o[1].x, hand.o[1].y, hand.o[2].x, hand.o[2].y);

  ink(handPalette.w).line(w[4].x, w[4].y, w[3].x, w[3].y);
  ink(handPalette.w).line(w[4].x, w[4].y, w[0].x, w[0].y);

  ink(handPalette.p).line(w[4].x, w[4].y, hand.p[0].x, hand.p[0].y);
  ink(handPalette.p).line(hand.p[0].x, hand.p[0].y, hand.p[1].x, hand.p[1].y);
  ink(handPalette.p).line(hand.p[1].x, hand.p[1].y, hand.p[2].x, hand.p[2].y);




  // 🅰️ Hand Points
  const boxSize = 5;
  const boxType = "fill*center";
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

// Crawl a point {x, y} dist amount in a direction, returning the new position.
function crawl(p, dist, dir = 0) {
  dir = radians(dir - 90);
  return { x: p.x + dist * Math.cos(dir), y: p.y + dist * Math.sin(dir) };
}

// Generate points for a digit given an orientation (deg).
function digit(from, segCount, deg = 0, curve = 0) {
  const segs = [];
  let gap = 18;
  for (let s = 0; s < segCount; s += 1) {
    if (s > 0) {
      deg += curve;
      gap *= 0.89;
      segs.push(crawl(segs[s - 1], gap, deg));
    } else {
      segs.push(crawl(from, gap, deg));
    }
  }
  return segs;
}

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
