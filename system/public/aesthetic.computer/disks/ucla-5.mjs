// ucla-5, 24.07.16.18.50
// Worms and clocks.

/* 📝 Notes 
  - Today we will be designing a worm-like form, in addition to a clock. 
    - Exercises -
    3. [🍊] Expanding worm. 🪱
      + Done
      - [x] Dynamic color.
        - [x] "Fade out as we get to the end".
      - [x] Dynamic size.
      - [x] Dynamically structure the worm so it can have "N" tail length.
      - [x] Lerp the head for "wandering mode".
      - [x] Make an 'addTailPiece' function. 
      - [x] Use `addTailPiece` to set initial size.
      - [x] Up arrow key to add a segment. (Down to remove)
      - [x] What about "removeTailPiece" ?
      - [x] Change the following behavior to use `lerp`.
      - [x] Add another 'node' or follower that follows the follower.
      - [x] And another...
    + Done
    1. [x️] Let's remember to finish... `source ucla-4-box`.
    2. [x] Leading and following! 🟢🟥
      - [x] Circle <- leader
      - [x] Square <- follower
    ---
    -> For ucla-6
    4. [] Keeping time and clock-making. 
    5. - [] Using LLMs to learn about the JavaScript `Date` Object. 
    6. - [] Writing the time.
    7. - [] Progress bars and using `num.map`.
    8. - [] Learning `lineAngle(x1, y1, dist, degrees)`
*/

// We need to make an Array of Objects....
const worm = [];

// This is a function definition or "blue print" with code.
function addTailPiece(pieceColor) {
  // Pre-fill x and y with the current last tail piece.
  // [*, *, *, *] Length is (4)
  //  0  1  2  3 = length-1
  const last = worm[worm.length - 1] || { x: 0, y: 0 };
  let size = Math.max(16 - worm.length, 3);
  // Is worm.length odd or even?
  // if (worm.length % 2 === 0) {
  //   size = 12;
  // }
  const piece = { x: last.x, y: last.y, color: pieceColor, size };
  worm.push(piece);
}

function removeTailPiece() {
  if (worm.length > 1) worm.pop(); // Removes the last item and changes worm.
}

const colors = ["yellow", "red", "blue", "red", "blue", "red", "blue", "white"];
colors.forEach(addTailPiece);

let head = worm[0];
let headGoal;

let wanderingMode = true;

function boot({ wipe, screen }) {
  wipe("gray");
  head.x = screen.width / 2;
  head.y = screen.height / 2;
  headGoal = { x: head.x, y: head.y };

  worm.forEach((tailPiece, index) => {
    if (index === 0) return; // Skip the head at slot 0.
    tailPiece.x = head.x;
    tailPiece.y = head.y;
  });
}

// Process
function paint({ wipe, ink, box, circle, pen, screen, num, line }) {

  if (wanderingMode && Math.random() > 0.97) {
    headGoal.x = num.map(Math.random(), 0, 1, 0, screen.width);
    headGoal.y = num.map(Math.random(), 0, 1, 0, screen.height);
  }

  head.x = num.lerp(head.x, headGoal.x, 0.1);
  head.y = num.lerp(head.y, headGoal.y, 0.1);

  worm.forEach((tailPiece, index) => {
    if (index === 0) return; // Skip the head at slot 0.
    tailPiece.x = num.lerp(tailPiece.x, worm[index - 1].x, 0.1);
    tailPiece.y = num.lerp(tailPiece.y, worm[index - 1].y, 0.1);
  });

  wipe("gray");
  // Drawing the worm.
  worm.forEach((tailPiece, index) => {
    if (index === 0) return; // Skip the head at slot 0.

    // Map from worm index to 0->1.
    const transparency = num.map(index, 0, worm.length, 1, 0);
    ink(tailPiece.color, transparency).circle(tailPiece.x, tailPiece.y, tailPiece.size, "fill");

    const last = worm[index - 1];
    ink(tailPiece.color).line(tailPiece.x, tailPiece.y, last.x, last.y);
  });

  ink(head.color, 0.5).circle(head.x, head.y, head.size, "fill"); // Head
}

function act({ event: e, help }) {
  if (e.is("keyboard:down:arrowup")) {
    addTailPiece(help.choose("red", "yellow", "blue"));
  }
  if (e.is("keyboard:down:arrowdown")) {
    removeTailPiece();
  }

  if (e.is("touch")) {
    headGoal.x = e.x;
    headGoal.y = e.y;
  }
}

// 📚 Library

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
