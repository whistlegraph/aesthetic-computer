// ucla-5, 24.07.16.18.50
// Worms and clocks.

/* 📝 Notes 
  - Today we will be designing a worm-like form, in addition to a clock. 
    - Exercises -
    2. [-] Leading and following! 🟢🟥
      - [] Circle <- leader
        - position (x, y)
      - [] Square <- follower
        - position (x, y)
    3. [] Expanding worm. 🪱
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
function paint({ wipe, ink, box, circle, stamp, pen }) {

  if (pen) {
    cx = pen.x;
    cy = pen.y;
  }

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

  // 1. make directory
  // 2. make 'index.html' which is the homepage
  // 3. choose web host
  // 4. register domain name
  // 5. upload entire directory to web host
  // 6. connect webhost to domain name
  // 7 'static site generators'

  wipe("orange");

  stamp("@jeffrey/2024.2.13.15.31.05.635", 8, 8);
  stamp("@jeffrey/2024.2.13.15.31.05.635", 8, 80);

  ink("green");
  circle(cx, cy, 8, "fill");

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
