// ucla-4, 24.07.11.19.08
// Intermediate graphics and modal logic.

/* 📝 Notes 
  - Today we will be exploring interactive graphics through making
    a worm, among other things. 🪱 
      🟪️ `box`, `shape`, `synth`.
      📜 Features of: `array`, `object`, and `function`.
    - Exercises -
    1. [🟢] 🥁 Drum sound! (`synth` and `act` review) 
    2. [🟠] Rectangle painting and the backbuffer. (graphics review)
    3. [] What is a mode and how to design with modes...
    2. [] Leading and following! 🟢🟥
    4. [] Expanding worm. 🪱
*/

let stamp;

function boot({ painting, screen }) {
  stamp = painting(screen.width, screen.height, ({ wipe, ink, line, noise16 }) => {
    wipe("yellow", 100);
    ink("red");
    line(0, 0, screen.width, screen.height);
  });
}

let needsStamp = false;

function paint({ wipe, ink, line, write, box, screen, pen, painting, paste, page }) {
  wipe("gray");
  paste(stamp);

  const x = pen?.x || screen.width / 2;
  const y = pen?.y || screen.height / 2;
  const width = x - startingCornerX;
  const height = y - startingCornerY;

  ink("yellow").write("sx:" + startingCornerX, 6, 20);
  ink("cyan").write("x:" + x, 6, 30);
  ink("blue").write("width:" + width, 6, 40);

  if (pen?.drawing || needsStamp) {
    ink("red").box(startingCornerX, startingCornerY, width, height, "outline"); // x, y, size, style
    // ink("blue", 64).box(startingCornerX, startingCornerY, width, height, "fill"); // x, y, size, style
  }

  if (needsStamp) { // Draw to the "stamp" buffer.
    needsStamp = false;
    page(stamp);
    ink("blue").box(startingCornerX, startingCornerY, width, height, "fill"); // x, y, size, style
    page(screen);
  }
}

let startingCornerX;
let startingCornerY;

function act({ event: e, sound, screen }) {
  if (e.is("touch")) {
    startingCornerX = e.x;
    startingCornerY = e.y;
  }
  if (e.is("lift")) {
    needsStamp = true;
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
