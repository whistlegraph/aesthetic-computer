// $NAME, $TIMESTAMP
// $THIS_IS_A_TEMPLATE_FOR_MAKING_NEW_PIECES

/* 📝 Engineering Notes
  The `paint` function runs every animation frame.
  `screen.pixels` is a Uint8ClampedArray with direct access.
  `screen.width` and `screen.height` is also available for aspect ratio / limits.
  Special note: `Use screen.pixels / direct pixel array access for any automated drawing.`
*/

function paint({ api, wipe, ink, line, screen, box, circle, pen, write, paste, kidlisp }) {
  wipe("navy"); // Clear the background.
  
  // ✨ Test the new tri function!
  kidlisp(0, 0, screen.width, screen.height, `
    ; Test the new triangle function
    (ink "red")
    (tri 50 50 100 50 75 100)
    
    ; Test triangle outline  
    (ink "lime")
    (tri 150 50 200 50 175 100 "outline")
    
    ; Test play button triangle (similar to the one mentioned)
    (ink "yellow")
    (tri 100 150 120 160 100 170)
  `);
  
  // Regular JavaScript paste still works with quoted URLs
  paste("https://assets.aesthetic.computer/wipppps/cow.png", 0, 0, 0.05);
  
  ink("lime"); // Paint some decoration
  line(0, 0, screen.width, screen.height);
  line(screen.width, 0, 0, screen.height);

  ink("white");
  write("� Testing tri function in KidLisp!", 5, screen.height - 20);

  if (pen) {
    ink("yellow").circle(pen.x, pen.y, 8);
    ink("white").write("Cursor!", { x: pen.x, y: pen.y + 15, center: "x" });
  }
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
//   // Runs once per system metronome (BPM) tick.
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
