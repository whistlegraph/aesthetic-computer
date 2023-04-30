// Zoom, 23.04.29.22.51
// Bounces in and rotates the current painting.
// 🖼️ Used for testing and implementing bitmap zoom.

// 🎨 Paint (Executes every display frame)
function paint({ wipe, paste, screen, system, paintCount }) {
  const osc = Math.sin(paintCount / 20);
  const scale = 1.25 + osc; // Bounce in and out
  const angle = (paintCount / 2); // Slowly rotate
  const x = screen.width / 2 - (system.painting.width * scale) / 2; // Center
  const y = screen.height / 2 - (system.painting.height * scale) / 2;
  wipe(0, 0, 255).paste(system.painting, x, y, { scale, angle });
}

export { paint };

// 📚 Library (Useful functions used throughout the piece)
// ...
