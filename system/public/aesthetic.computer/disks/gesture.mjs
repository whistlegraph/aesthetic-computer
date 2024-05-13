// ✏️ Gesture, 2024.5.13.20.19.33.448
// Track and play back a gesture.

/* #region 🏁 TODO 
  - [] Touch the screen and draw a visual line. 
  - [] Lift, and see the line playback in realtime to
       show the gesture.
  - [] Repeat to clear and watch a new gesture.
#endregion */

function boot() {
  // Runs once at the start.
}

let gesture = []; // Current gesture.
let ticks = 0; // How many ticks passed between points.

const gestures = []; // All stored gestures.

let playing = false;
let currentGesture = 0;
let progress = 0;

function paint({ wipe, ink, line }) {
  // Runs every display frame.
  wipe("pink");

  if (playing) {
    progress += 1;
    if (progress > gesture.length) progress = 0;
  } else {
    ticks += 1;
  }

  // Render existing gestures.
  gestures.forEach((g, index) => {
     ink().poly(g);
  });

  ink(playing ? "blue" : "red").poly(
    playing ? gesture.slice(0, progress) : gesture,
  );
}

function act({ event: e }) {
  if (e.is("touch")) {
    playing = false;
    ticks = 0;
    gesture = [];
    gesture.push({ x: e.x, y: e.y, delay: ticks });
  }

  if (e.is("draw")) {
    gesture.push({ x: e.x, y: e.y, delay: ticks });
    ticks = 0;
  }

  if (e.is("lift")) {
    gestures.push(gesture);
    playing = true;
  }
}

export { boot, paint, act };
