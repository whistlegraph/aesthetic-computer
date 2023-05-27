// Happy Hands Assembler, 23.04.24.15.02
// Let's make a some happy hands!
// Coded by Jeffrey Alan Scudder & Tina Tarighian

/* #region 🏁 todo
  - [] We need a goal.
  - [] Fix video orientation issues.
  + Done
  - [x] Clean up Hands API
    - [x] Add buffered video object / layer back with proper cropping
         and downsizing.
    - [x] Add "pause" feature to tracking.
    - [x] Add init wrapper to hand-track without video frames.
  - [x] Jeffrey tries to speed it all up.
    - [x] Implement both new and old version.
    - [x] Add dynamic switch via `useLegacyHands`.
  - [x] Get code running in worker.
  - [x] Jeffrey sets up deinitialization of mediapipe.
  - [x] Jeffrey fix improper layering of `box` and `poly`.
        (Wasn't a bug; ordering of drawing interleaves boxes & lines, so
        using layers makes sense)
  - [x] Move the corner label numbering the current hand from `boot` to `paint`.
#endregion */

/* #region 🤝 Read Me 
#endregion */

import { radians } from "../lib/num.mjs";

let h; // Current working hand.
const hands = 1024; // How many happy hands exist in total?
const key = "happy-hand-assembler:hand"; // Keep track of current hand index.
const origin = { x: 0, y: 0, z: 0 }; // Wrist
const handPalette = {
  w: "#FFFFFFFF", // Wrist, white
  t: [0, 170, 200], // Thumb, teal
  i: [75, 0, 130], // Index, Indigo
  m: "magenta", // Middle finger, magenta
  o: "orange", // Ring, orange
  p: "pink", // Pinky, pink
};

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ wipe, params, screen, store }) {
  h = parseInt(params[0]);
  if (isNaN(h)) {
    // Try to receive last edited hand.
    const stored = await store.retrieve(key, "local:db");
    h = stored;
  }

  if (h === null || isNaN(h) || h < 0 || h > hands - 1) {
    console.warn("👎 `Happy Hand` Not Found:", h);
    wipe(100, 0, 0)
      .ink(120, 0, 0)
      .line(0, 0, screen.width, screen.height)
      .line(0, screen.height, screen.width, 0); // Draw a big red X.
  } else {
    // 🤚 We have a hand!
    wipe(0, 64, 0)
      .ink(0, 255, 0, 128)
      .write(h, { x: 4, y: screen.height - 13 }); // Drawing number of hand.
    store[key] = h; // Store and persist the current hand across page refreshes.
    store.persist(key, "local:db");
  }
}

let vid;

// 🎨 Paint (Executes every display frame)
function paint({
  hand: { mediapipe },
  layer,
  wipe,
  ink,
  box,
  pan,
  unpan,
  screen: { created, resized, width, height },
  pen,
  paste,
  paintCount,
  video,
}) {
  // Start video feed once for webcam hand-tracking on mobile and desktop.
  // (And recalibrate if resized.)
  if (created || resized) {
    vid = video(created ? "camera" : "camera:update", {
      hidden: false, // Toggle to stop pulling frames.
      hands: true,
      facing: "user" || "environment",
      width,
      height,
    });
  }

  const frame = vid();
  frame ? paste(frame) : wipe(0, 64, 0);
  ink(0, 255, 0, 128).write(h, { x: 4, y: height - 13 }); // Print hand index.

  const boxSize = 5;
  const boxType = "fill*center";

  if (mediapipe?.screen.length > 0) {
    const scaled = mediapipe.screen.map((coord) => [
      coord.x * width,
      coord.y * height,
    ]);

    // A. Draw lines
    ink(handPalette.w).poly([
      scaled[0],
      scaled[5],
      scaled[9],
      scaled[13],
      scaled[17],
      scaled[0],
    ]);

    ink(handPalette.t).poly([
      scaled[0],
      scaled[1],
      scaled[2],
      scaled[3],
      scaled[4],
    ]);

    ink(handPalette.i).poly([scaled[5], scaled[6], scaled[7], scaled[8]]);
    ink(handPalette.m).poly([scaled[9], scaled[10], scaled[11], scaled[12]]);
    ink(handPalette.o).poly([scaled[13], scaled[14], scaled[15], scaled[16]]);
    ink(handPalette.p).poly([scaled[17], scaled[18], scaled[19], scaled[20]]);

    // B. Loop over the scaled points and draw the boxes.
    scaled.forEach((coord, index) => {
      if (index >= 18) {
        ink(handPalette.p); // Pinky
      } else if (index > 13 && index < 17) {
        ink(handPalette.o);
      } else if (index > 9 && index < 13) {
        ink(handPalette.m);
      } else if (index > 5 && index < 9) {
        ink(handPalette.i);
      } else if (index > 0 && index < 5) {
        ink(handPalette.t);
      } else {
        if (mediapipe.hand === "left") ink(200, 200, 255);
        if (mediapipe.hand === "right") ink(200, 255, 200);
      }
      box(coord[0], coord[1], boxSize, boxType);
    });
  } else {
    const osc = Math.sin(paintCount * 0.1); // Oscillate a value based on frame.
    // Build base wrist geometry.
    const w = [
      origin,
      crawl(origin, 40 + 2 * osc, 10),
      crawl(origin, 45 + -2 * osc, 25),
      crawl(origin, 50 + 2 * osc, 40),
      crawl(origin, 55 + -2 * osc, 55),
    ];

    // Build hand geometry with fingers.
    const hand = {
      w,
      t: digit(w[0], 4, -30, -10 * osc),
      i: digit(w[1], 3, -8, -10 * osc),
      m: digit(w[2], 3, 0, -10 * osc),
      o: digit(w[3], 3, 7, -10 * osc),
      p: digit(w[4], 3, 20, -10 * osc),
    };

    const o = { x: -24 + 2 * osc, y: 16 + 2 * osc }; // Offsets and oscilates the entire hand
    pen
      ? pan(pen.x + o.x, pen.y + o.y)
      : pan(width / 2 + o.x, height / 2 + o.y);

    // 🅰️ Hand Lines & Points
    // Draw each component (lines and boxes) of wrist, followed by each of digit.
    ["w", "t", "i", "m", "o", "p"].forEach((char, i) => {
      layer(0); // Lines always under boxes.
      if (char === "w") {
        ink(handPalette.w).poly([...w, w[0]]); // Closed polygon for wrist.
      } else {
        ink(handPalette[char]).poly([w[i - 1], ...hand[char]]);
      }
      layer(1); // Always draw the boxes on top.
      ink(handPalette[char]);
      for (let coord of hand[char]) box(coord.x, coord.y, boxSize, boxType);
    });

    unpan(); // Reset the translation.
  }
}

// Tab title and meta description of this piece.
function meta() {
  return {
    title: "Happy Hands Assembler",
    desc: "Get ready for some happy hands!",
  };
}

export { boot, paint, meta };

// 📚 Library (Useful functions used throughout the piece)

// Crawl a point {x, y} dist amount in a direction, returning the new position.
function crawl(p, dist, dir = 0) {
  dir = radians(dir - 90); // Set 0 degrees to up, convert to radians.
  return { x: p.x + dist * Math.cos(dir), y: p.y + dist * Math.sin(dir) };
}

// Generate points for a digit given an orientation (deg).
// from: { x, y }, segCount: n, deg, curve
function digit(from, segCount, deg = 0, curve = 0) {
  const segs = [];
  let gap = 18;
  for (let s = 0; s < segCount; s += 1) {
    if (s === 0) {
      segs.push(crawl(from, gap, deg));
    } else {
      deg += curve; // Curve a bit on each seg.
      gap *= 0.89; // Decrease gap as well.
      segs.push(crawl(segs[s - 1], gap, deg)); // Crawl from previous seg.
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
