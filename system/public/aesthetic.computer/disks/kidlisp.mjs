// KidLisp, 2025.11.20
// Default KidLisp piece that shows a checkerboard pattern when no code is provided

export function boot({ wipe, dark }) {
  if (dark) {
    wipe(32, 32, 32); // Dark gray for dark mode
  } else {
    wipe(240, 240, 240); // Light gray for light mode
  }
}

export function paint({ wipe, ink, box, screen, dark, paintCount }) {
  // Draw a subtle checkerboard pattern as the default visual
  if (dark) {
    wipe(32, 32, 32); // Dark gray background
    ink(40, 40, 40); // Slightly lighter dark gray squares
  } else {
    wipe(240, 240, 240); // Light gray background
    ink(220, 220, 220); // Very subtle gray squares
  }
  
  const size = 32; // Larger squares
  const cols = Math.ceil(screen.width / size) + 2; // Extra columns for scrolling
  const rows = Math.ceil(screen.height / size) + 2; // Extra rows for scrolling
  
  // Scroll animation
  const speed = 0.25;
  const offsetX = (paintCount * speed) % (size * 2);
  const offsetY = (paintCount * speed) % (size * 2);
  
  for (let y = -1; y < rows; y++) {
    for (let x = -1; x < cols; x++) {
      if ((x + y) % 2 === 0) {
        box(x * size + offsetX - size, y * size + offsetY - size, size, size);
      }
    }
  }
}

export function act({ event: e, needsPaint }) {
  // Repaint when dark mode changes
  if (e.is("dark-mode") || e.is("light-mode")) {
    needsPaint();
  }
}
