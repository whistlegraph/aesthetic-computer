// 🧋 Bubble 2022.02.04.17.46 [Sage: @mxsage + Jeffrey]
// Create a visual bubble when tapped or clicked. Bubbles float up off the
// screen and the pitch or size of the bubble is related to where you tap.

// TODO
// 1. Get amplitude out of sound thread.
// 2. Refactor bubble graphics so that more than one can exist.
// 3. What parameters should be interactively controllable over time?

let bub, progress;
let originalRadius = 50;
let radius = originalRadius;

let bubbleRadii = [];

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ resize, screen, cursor, glaze, density }) {
  // cursor("none");
  // TODO: Runs only once!
  // resize(50, 20);

  originalRadius = screen.width / 3;
  radius = originalRadius;

  glaze({ on: true, type: "prompt" });
}

const bubbles = [];

// 💗 Beat (Runs once per bpm)
function beat({ sound: { bpm, bubble, time } }) {
  bpm(2000);
  bubbles.forEach((options) => bubble(options));
  bubbles.length = 0;
}

// 🧮 Sim(ulate)
function sim({ sound: { time } }) {
  //progress = squareDemo?.progress(time);
  //console.log("Square progress:", progress);
}

// 🎨 Paint (Executes every display frame)
function paint({ ink, plot, wipe, screen }) {
  const x = (progress || 0) * screen.width;
  wipe(128);

  ink(0, 0, 255).circle(screen.width / 2, screen.height / 2, radius);

  ink(255).circle(
    screen.width / 2 - radius / 3,
    screen.height / 2 - radius / 3,
    radius / 4
  );
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, num, screen }) {
  if (e.is("touch")) {
    const maxDimension = Math.max(screen.width / 2, screen.height / 2);
    let soundRadius = num.dist(e.x, e.y, screen.width / 2, screen.height / 2);
    soundRadius /= maxDimension;

    radius = soundRadius * maxDimension;
    bubbles.push({
      radius: soundRadius * 20 + 0.01,
      rise: Math.pow(Math.random(), 2) * 3,
      volume: 0.5,
      pan: 0,
    });
  }

  if (e.is("lift")) {
    radius = originalRadius;
  }

  //if ()
  // console.log(event);
}

// 📚 Library (Useful classes & functions used throughout the piece)
// ...

export { boot, sim, paint, act, beat };
