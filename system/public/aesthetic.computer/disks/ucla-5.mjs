// ucla-5, 24.07.16.18.50
// Worms and clocks.

/* 📝 Notes 
  - Today we will be designing a worm-like form, in addition to a clock. 
    - Exercises -
    3. [🍊] Expanding worm. 🪱
      - [💙] Dynamically structure the worm so it can have "N" tail length.
        - [] Make an 'addTailPiece' function. 
        - [] Use `addTailPiece` to set initial size.
        - [] Up arrow key to add a segment. (Down to remove)
        - [] What about "removeTailPiece" ?
      - [] Lerp the head for "wandering mode".
      + Done
      - [x] Change the following behavior to use `lerp`.
      - [x] Add another 'node' or follower that follows the follower.
      - [x] And another...
    + Done
    1. [x️] Let's remember to finish... `source ucla-4-box`.
    2. [x] Leading and following! 🟢🟥
      - [x] Circle <- leader
      - [x] Square <- follower
    ---
    4. [] Keeping time and clock-making. 
    5. - [] Using LLMs to learn about the JavaScript `Date` Object. 
    6. - [] Writing the time.
    7. - [] Progress bars and using `num.map`.
    8. - [] Learning `lineAngle(x1, y1, dist, degrees)`
*/

// We need to make an Array of Objects....
const worm = [
  { x: 0, y: 0, color: "red" }, //    [0] HEAD
  { x: 0, y: 0, color: "blue" }, //   [1] TAIL A
  { x: 0, y: 0, color: "green" }, //  [2] TAIL B
  { x: 0, y: 0, color: "black" }, //  [3] TAIL C
  { x: 0, y: 0, color: "brown" }, //  [4] TAIL D
  { x: 0, y: 0, color: "purple" }, // [5] TAIL E
];

let head = worm[0];
let tailInitialized = false;

// Process
function paint({ wipe, ink, box, circle, pen, screen, num, line }) {
  head.x = pen ? pen.x : screen.width / 2;
  head.y = pen ? pen.y : screen.height / 2;

  // Setting the initial positions.
  if (tailInitialized === false) {
    worm.forEach((tailPiece, index) => {
      if (index === 0) return; // Skip the head at slot 0.
      tailPiece.x = head.x;
      tailPiece.y = head.y;
    });
    tailInitialized = true;
  }

  // Lerp all positions...
  worm.forEach((tailPiece, index) => {
    if (index === 0) return; // Skip the head at slot 0.
    tailPiece.x = num.lerp(tailPiece.x, worm[index - 1].x, 0.1);
    tailPiece.y = num.lerp(tailPiece.y, worm[index - 1].y, 0.1);
  });

  wipe("orange");
  const size = 16;

  // Drawing the worm.
  worm.forEach((tailPiece, index) => {
    if (index === 0) return; // Skip the head at slot 0.
    ink(tailPiece.color, 0.5).circle(tailPiece.x, tailPiece.y, size, "fill");
    const last = worm[index - 1];
    ink("yellow").line(tailPiece.x, tailPiece.y, last.x, last.y);
  });

  ink("green", 0.5).circle(head.x, head.y, size, "fill"); // Head
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
