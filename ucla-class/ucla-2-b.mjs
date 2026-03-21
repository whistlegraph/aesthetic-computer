// ucla-2, 2024.7.02.03.11.28.762
// Interactive graphics.


/* 📝 Notes 
  - Today we will be interacting with graphics and sound using the mouse
    and keyboard.
      🟪️ `box` `write`, `act`, `pen`, `synth`
      📜 `if`, `else`, `const`, booleans and logical operators
    - Exercises -
    - 1. Making Boxes (review) 🟧 
      - [🟢] Make a new box from outlined lines.
    - 2. Conditionals, and Coordinates and clicks. 🚥 
      - [] Collision detection. 🚗
        - [] Single axis,
        - [] Boxes
    - 3. Buttons that... 
      - 4. [] Blink, 🚨
      - 5. [] Write, ✍️
      - 6. [] and Sing! 🎵

    ⌨️ Useful source code shortcuts.
    🍎 + Shift + P -> Format Document
    🍎 + / -> Toggle Comments
*/

let x = 40; // top left corner
let y = 82;
let width = 60; // size of box
let height = 80;

// TOP, LEFT, RIGHT, BOTTOM
let top = y;
let left = x;
let bottom = y + height;
let right = x + width;

let inX = false;
let inY = false;

function paint({ wipe, ink, pen, point, line, flood, screen, write }) {
  wipe("black");

  if (pen.x < left) {
    ink("lime", 0.2);
    inX = false;
  } else if (pen.x < right) {
    ink("lime", 0.8);
    inX = true; // 😃 WE ARE INSIDE THE X RANGE
  } else {
    inX = false;
    ink("red");
  }

  line(left, 0, left, screen.height);
  line(right, 0, right, screen.height);

  if (pen.y < top) {
    ink("orange", 0.4);
    inY = false;
  } else if (pen.y < bottom) {
    ink("orange", 0.8);
    inY = true; // 😃 INSIDE THE Y RANGE
  } else {
    inY = false;
    ink("red");
  }
  line(0, top, screen.width, top);
  line(0, bottom, screen.width, bottom);

  write(inX, 6, 20);
  write(inY, 6, 30);

  ink("yellow"); // Four corners...
  point(left, top); // top (y) left (x)
  ink("red");
  point(right, top); // top right
  ink("cyan");
  point(left, bottom); // bottom left
  ink("white");
  point(right, bottom); // bottom right

  ink("yellow", 0.25); // transparent yellow
  line(left, top, right, top); // top line
  line(left, top, left, bottom); // left line
  line(left, bottom, right, bottom); // bottom line
  line(right, top, right, bottom); // right line

  // inX and inY must BOTH BE TRUE
  // if inX AND inY is true... then,
  if (inX && inY) {
    ink(floodColor, 0.4); // 0->1 transparency (alpha)
    flood(left + 1, top + 1); // x, y (Paint Bucket)
  }
}

let floodColor = "pink";

function act({ event: e, sound }) {
 // Respond to user input here.

 if (e.is("touch")) {
  if (inX && inY) {
    floodColor = "red";
    sound.synth({ tone: 'c', duration: 1, type: "square" });
    sound.synth({ tone: 'e', duration: 1, type: "square" });
    sound.synth({ tone: 'g', duration: 1, type: "square" });
  }
 }

 if (e.is("lift")) {
  sound.synth({ tone: 6000 });
  floodColor = "pink";
 }

}

// 📚 Library

// function boot() {
// Runs once at the start.
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
