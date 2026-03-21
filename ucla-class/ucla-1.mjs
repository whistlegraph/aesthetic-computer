// ucla-1, 2024.6.27.19.14.56.196
// Basic graphics.
// Enter `source ucla-1` to open this file.

/* 📝 Notes 

  - Opening the JavaScript Console -
  VS Code: Open Command Bar: Ctrl (Command) + Shift + P
           Help -> Toggle Developer Tools
           ⚠️ Then find the 'Console' tab.

  - Today we will be painting basic graphics on 🟪 Aesthetic Computer
    in order to get a feel for 📜 JavaScript programming, using...
      🟪️ `paint` `wipe`, `ink`, `line`, `screen`, `resolution` and color specification
      📜 `function`, `console`, `let`, `const`, `if`, `for`, and basic math
    - Exercises -
    - 1. ✅ Lines and Lets -
    - 2. ✅ Stripes and For Loops -
    - 3. ✅ Making Boxes -
*/

function paint({ wipe, point, line, ink, resolution, screen, write, pen, box }) {
  wipe("black");

  ink("teal");
  box(0, 0, 100, 100);

  // Paint a box...
  // x, y
  // width, height
  function myBox(x, y, width, height, boxColor) {
    for (let startX = x; startX <= x + width; startX = startX + 1) {
      ink(boxColor);
      line(startX, y, startX, y + height);
    }
  }

  myBox(30, pen.y, 10, 10, "blue"); // ^ Calls the code from above.
  myBox(0, 100, 20, 20, "red");
  myBox(pen.x, pen.y, 8, 8, "pink");

  // {
  //   let x = 32;
  //   let y = 64;
  //   let width = 40;
  //   let height = 40;
  //   let boxColor = [255, 0, 0, 128]; // "Array"

  //   for (let startX = x; startX <= x + width; startX = startX + 1) {
  //     ink(boxColor);
  //     line(startX, y, startX, y + height);
  //   }

  //   // Plot four points in our box.
  //   ink("white");
  //   point(x, y); // Top left.
  //   point(x + width, y); // Top right
  //   point(x, y + height); // Bottom left
  //   point(x + width, y + height); // Bottom left
  // }

  // `for` "Loop" - JavaScript 'for' loop.
  // determines how many times the code in { ... } will run
  // for (let start = 0; start < screen.width; start = start + 1) {
  //   if (start % 2 === 1) {
  //     // start is odd
  //     ink("red");
  //     line(start, 0, start, screen.height);
  //   }
  // }

  // for (let start = 0; start < screen.height; start = start + 1) {
  //   if (start % 2 === 1) {
  //     // start is odd
  //     ink("blue");
  //     line(0, start, screen.width, start);
  //   }
  // }
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
