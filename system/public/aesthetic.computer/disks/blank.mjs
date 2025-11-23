// $NAME, $TIMESTAMP
// $THIS_IS_A_TEMPLATE_FOR_MAKING_NEW_PIECES

/* 📝 Engineering Notes
  The `paint` function runs every animation frame.
  `screen.pixels` is a Uint8ClampedArray with direct access.
  `screen.width` and `screen.height` is also available for aspect ratio / limits.
  Special note: `Use screen.pixels / direct pixel array access for any automated drawing.`
*/

function boot({ api }) {
  // Enable WebGPU rendering and disable CPU renderer
  api.webgpu.enabled = true;
}

function paint({ wipe, ink, line, screen }) {
  // Clear with dark blue/purple background (fully opaque)
  wipe(32, 32, 64, 255);
  
  // Draw a yellow diagonal line
  ink(255, 255, 0, 255);
  line(screen.width, 0, 0, screen.height);
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
