// ucla-6, 24.07.23.07.33
// Clocks, component design and relative coordinate systems.

/* 📝 Notes 
  - Today we will be designing clockfaces and working with time. 
    1. [x] 🐢 Introduction to turtle graphics and relative coordinate systems.
    2.   - [x] Making lines and basic operation.
    3.   - [x] Making a circle.
    4.   - [x] Patterns.
    4. [] Keeping time and clock-making. 
    5. - [] Using LLMs to learn about the JavaScript `Date` Object. 
    6. - [] Writing the time.
    7. - [] Progress bars and review using `num.map`.
    8. - [] Learning `lineAngle(x1, y1, dist, degrees)`
    9. - [] Using parameters in Aesthetic Computer
*/

// Adjust the frames per second of the paint loop with 'fps'.

function boot({ wipe, fps }) {
  wipe("gray");
  // fps(6); // How many times does paint run per second?
}

let baseAngle = 0;
let secondaryAngle = 0;

function paint({ wipe, ink, box, goto, crawl, left, right, up, down, face, screen, pen }) {
  wipe("gray"); // Clear the screen entirely!

  goto(pen?.x || screen.width / 2, pen?.y || screen.height / 2); // Teleport 🐢

  down(); // start drawing

  baseAngle += 1; // increase base angle by 1
  const angle = face(baseAngle);

  ink("white").write(angle, 6, 20);

  const segColors = [ "red", "orange", "yellow", "green", "blue", "indigo", "violet", "black"];
  let position; // Will store the last position after each crawl in the loop.
  let handLength = 16;
  const segments = 8;
  for (let segment = 0; segment < segments; segment += 1) {
    ink(segColors[segment]); // Pick out the color based on our segment index.
    position = crawl(handLength / segments);
  }

  ink("yellow").write("x: " + position.x, 6, 32);
  ink("yellow").write("y: " + position.y, 6, 42);
  // ink("white", 127).box(position.x, position.y, 32, 32, "center");
  ink("lime")

  secondaryAngle -= 3;
  face(secondaryAngle);
  crawl(32 / 2);
}

// 📚 Library

// function boot() {
// Runs once at the start.
// }

// function act({ event: e }) {
//  // Respond to user input here.
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
