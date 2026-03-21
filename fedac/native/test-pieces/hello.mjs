// hello.mjs — Simple AC test piece for the native runner
// Draws colored boxes, lines, circles, and text to verify the graphics stack.

let frame = 0;
let boxX = 100, boxY = 100;

function boot({ screen, wipe }) {
  wipe(30, 30, 60);
  console.log("hello.mjs booted on " + screen.width + "x" + screen.height);
}

function paint({ wipe, ink, box, line, circle, write, screen }) {
  // Dark blue background
  wipe(20, 20, 50);

  // Red filled box
  ink(255, 50, 50).box(boxX, boxY, 120, 80);

  // Green outline box
  ink(50, 255, 50).box(boxX + 150, boxY, 120, 80, "outline");

  // Yellow line
  ink(255, 255, 0);
  line(0, 0, screen.width, screen.height);

  // Cyan filled circle
  ink(0, 255, 255);
  circle(screen.width / 2, screen.height / 2, 60, true);

  // White circle outline
  ink(255, 255, 255);
  circle(screen.width / 2, screen.height / 2, 80, false);

  // Text
  ink(255, 255, 255);
  write("aesthetic computer", { x: 20, y: 20, size: 3 });
  write("native runtime", { x: 20, y: 50, size: 2 });

  // Animated pixel wave
  ink(255, 150, 0);
  for (let i = 0; i < screen.width; i += 2) {
    let y = screen.height - 40 + Math.sin((i + frame) * 0.05) * 20;
    box(i, Math.floor(y), 2, 2);
  }

  frame++;
}

function act({ event: e }) {
  if (e.is("keyboard:down:arrowright")) boxX += 10;
  if (e.is("keyboard:down:arrowleft")) boxX -= 10;
  if (e.is("keyboard:down:arrowup")) boxY -= 10;
  if (e.is("keyboard:down:arrowdown")) boxY += 10;

  if (e.is("touch")) {
    boxX = e.x - 60;
    boxY = e.y - 40;
  }
}

function sim() {
  // Could add physics/game logic here
}

// Export lifecycle — in native runner, these become globals
globalThis.boot = boot;
globalThis.paint = paint;
globalThis.act = act;
globalThis.sim = sim;
