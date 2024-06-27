// ucla-1, 2024.6.27.19.14.56.196
// Basic graphics.
// Enter `source ucla-1` to open this file.

/* 📝 Notes 
  - Today we will be painting basic graphics on 🟪 Aesthetic Computer
    in order to get a feel for 📜 JavaScript programming, using...
      🟪️ `paint` `wipe`, `ink`, `line`, `screen`, `resolution` and color specification
      📜 `function`, `console`, `let`, `const`, `if`, `for`, and basic math
    - Exercises -
    - 1. Lines and Lets -
    - 2. Stripes and For Loops -
    - 3. Making Boxes -
*/

function paint({ wipe, point, ink, resolution, screen, write }) {
  resolution(3); // size or width and height
  // wipe(127, 80); // Paint all pixels on screen a given color.
  //   ^Brightness 0->255
  //        ^Alpha 0->255
  // wipe(255, 0, 0); // R, G, B (Red)
  // wipe(0, 255, 0); // R, G, B (Green)
  //wipe(0, 0, 255, 40); // R, G, B, Alpha (Blue)
  wipe("purple"); // Or use any CSS Color Name.
  // ink(255); // White
  ink(255, 255, 0); // Yellow
  point(0, 0); // x, y
  point(2, 0);
  ink("red");
  point(0, 2);
  ink("red", 128);
  point(1, 2);
  ink("lime");
  point(2, 2);
  // write(screen.width, {x: screen.width - 30, y: 160}); // text, {x, y}
  // console.log(screen.width);
}

export const nohud = true;

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