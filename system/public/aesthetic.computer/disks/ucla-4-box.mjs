// ucla-4-box, 24.07.11.19.08
// Intermediate graphics and modal logic.
// (A rectangle painting program.)

/* 📝 Notes 
  - Today we will be exploring interactive graphics through making
    a worm, among other things. 🪱 
      🟪️ `box`, `shape`, `synth`.
      📜 Features of: `array`, `object`, and `function`.
    - Exercises -
    1. [x] Rectangle painting and the backbuffer. (graphics review)
    2. [🟠] Modes and colors. 
      - [x] Added 'pan'.
      - [] Outline mode switch.
        - [x] Flag for outline mode.
        - [x] Make a keyboard shotcut. 'act'
        - [x] Make an overlay.
        - [x] Paint outlines if outline mode :).
      - [🌈] Color array cycle switch.
        - [x] Create a palette of possible colors. (Array)
        - [x] Create "chosenColor" state to hold the option.
        - [x] Make a label that paints 'chosenColor'
        - [x] Make sure we paint and preview in the 'chosenColor'
        - [x] Add a keyboard <- -> control to cycle.
    3. [x] Downloading the 'canvas'.
      - [x] Keyboard shortcut 'd' key will trigger 'download'.
      - [x] Learning `download(filename, data, modifiers)`.
*/

let canvas;
let outline = false;

let palette = ["red", 'yellow', "blue", "black"]; // Array of Strings
//                0         1        2       3
// Arrays are ORDERED       ^ 'index'
let chosenColor = 1; // not the color itself, but a reference to the index

function boot({ painting, screen }) {
  // "Instantiation of State"
  canvas = painting(
    screen.width / 2,
    screen.height / 2,
    ({ wipe, ink, line, noise16 }) => {
      wipe(); // Random starting color.
    },
  );
}

let needsCanvas = false;
let panX = 48;
let panY = 48;
let lastPenX, lastPenY;

function paint({
  wipe,
  ink,
  line,
  write,
  box,
  screen,
  pen,
  painting,
  paste,
  page,
}) {
  wipe("gray");
  paste(canvas, panX, panY); // painting, x, y

  if (pen) {
    lastPenX = pen.x;
    lastPenY = pen.y;
  }

  const x = lastPenX || screen.width / 2;
  const y = lastPenY || screen.height / 2;
  const width = x - startX;
  const height = y - startY;

  ink("yellow").write("sx:" + startX, 6, 20);
  ink("cyan").write("x:" + x, 6, 30);
  ink("blue").write("width:" + width, 6, 40);

  if (pen?.drawing || needsCanvas) {
    ink(palette[chosenColor]).box(startX, startY, width, height, "outline"); // x, y, size, style
    // ink("blue", 64).box(startingCornerX, startingCornerY, width, height, "fill"); // x, y, size, style
  }

  if (needsCanvas) {
    // Draw to the "canvas" buffer.
    needsCanvas = false;
    page(canvas);

    let style = "fill"; // Default.
    if (outline) style = "outline"; // Adjusting from the default.
    ink(palette[chosenColor]).box(startX - panX, startY - panY, width, height, style);
    page(screen);
  }

  if (panning) {
    ink("white").write("PAN", 6, screen.height - 12);
  }

  if (outline) {
    ink("yellow").write("OUTLINE", 6, screen.height - 32);
  }

  ink(palette[chosenColor]).write(palette[chosenColor], 6, 52);
}

let startX;
let startY;

let panning = false; // 🚦 Flag

function act({ event: e, sound, screen, download }) {
  // Storage
  if (e.is("keyboard:down:d")) {
      // - [] Learning `download(filename, data, modifiers)`.
      //                                   ^canvas
      //                         ^"rectangles.png"
      download("rectangles.png", canvas, { scale: 1 });
  }

  // Palette Switching
  if (e.is("keyboard:down:arrowright")) {
    chosenColor += 1;
    if (chosenColor === palette.length) {
      chosenColor = 0;
    }
    console.log("chosenColor:", chosenColor, "palette.length", palette.length);
    // Same as... chosenColor = chosenColor + 1
  }

  if (e.is("keyboard:down:arrowleft")) {
    chosenColor -= 1; // Move to the left....
    if (chosenColor === -1) {
      chosenColor = palette.length - 1;
    }
  }

  // Outline
  if (e.is("keyboard:down:o")) {
    outline = !outline; // Set outline to its opposite.
    // Same as...
    // if (outline === false) {
    //   outline = true;
    // } else {
    //   outline = false;
    // }
  }

  // if (e.is("keyboard:up:o")) {
  // outline = false;
  // }

  // Panning
  if (e.is("keyboard:down:shift")) {
    panning = true;
  }

  if (e.is("keyboard:up:shift")) {
    panning = false;
  }

  if (panning && e.is("move")) {
    panX = panX + e.delta.x;
    panY = panY + e.delta.y;
  }

  // Drawing
  if (e.is("touch")) {
    startX = e.x;
    startY = e.y;
  }

  if (e.is("lift")) {
    needsCanvas = true;
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
