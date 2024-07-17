// ucla-5, 24.07.16.18.50
// Worms and clocks.

/* 📝 Notes 
  - Today we will be designing a worm-like form, in addition to a clock. 
    - Exercises -
    2. [x] Leading and following! 🟢🟥
      - [x] Circle <- leader
      - [x] Square <- follower
    3. [🍊] Expanding worm. 🪱
    + Done
    1. [x️] Let's remember to finish... `source ucla-4-box`.
    ---
    4. [] Keeping time and clock-making. 
    5. - [] Using LLMs to learn about the JavaScript `Date` Object. 
    6. - [] Writing the time.
    7. - [] Progress bars and using `num.map`.
    8. - [] Learning `lineAngle(x1, y1, dist, degrees)`
*/

// State
let cx = 90;
let cy = 32;
let bx = 64;
let by = 64;

// Process
function paint({ wipe, ink, box, circle, pen }) {
  // Physics

  if (pen) {
    cx = pen.x;
    cy = pen.y;
  }

  // bx needs to converge on cx
  // is bx to the right or left?
  // (do we have to add or subtract?)

  if (by > cy) {
    by -= 1;
  } else if (by !== cy) {
    by += 1;
  }

  if (bx > cx) {
    bx -= 1;
  } else if (bx !== cx) {
    bx += 1;
  }

  // Rendering
  wipe("orange");

  // Leader
  ink("green");
  circle(cx, cy, 8, "fill");

  // Follower
  ink("red");
  box(bx, by, 8);
}

// 📚 Library

// function boot() {
// // Runs once at the start.
// }

// function act({ event: e }) {
// // Respond to user events.
// }

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
