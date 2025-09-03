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
  
  // ✨ NEW: Test unquoted URL support in KidLisp paste function!
  // kidlisp(0, 0, screen.width, screen.height, `
  //   ; Now you can use unquoted URLs in KidLisp paste commands!
  //   (paste https://assets.aesthetic.computer/wipppps/cow.png 10 10 0.3)
  //   (paste https://assets.aesthetic.computer/wipppps/cow.png (/ width 2) 50 0.5)
  //   
  //   ; Also works with stamp (centered pasting)
  //   (stamp https://assets.aesthetic.computer/wipppps/cow.png (/ width 2) (- height 50))
  //   
  //   ; Quoted URLs still work too
  //   (paste "https://assets.aesthetic.computer/wipppps/cow.png" 150 10 0.2)
  // `);

  // kidlisp(96, 24, 64, 64, 'fade:red-rainbow');
  
  // Regular JavaScript paste still works with quoted URLs
  paste("https://assets.aesthetic.computer/wipppps/cow.png", 0, 0, 0.05);
  
  ink("lime"); // Paint some decoration
  line(0, 0, screen.width, screen.height);
  line(screen.width, 0, 0, screen.height);

  ink("white");
  write("🐄 Unquoted URL support in KidLisp!", 5, screen.height - 20);

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
