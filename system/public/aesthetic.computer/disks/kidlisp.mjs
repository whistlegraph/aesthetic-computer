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
  // Add extra buffer on all sides to ensure full coverage during scroll
  const cols = Math.ceil(screen.width / size) + 3;
  const rows = Math.ceil(screen.height / size) + 3;
  
  // Scroll animation - use modulo to keep offset within bounds
  const speed = 0.25;
  const offsetX = (paintCount * speed) % (size * 2);
  const offsetY = (paintCount * speed) % (size * 2);
  
  // Start further off-screen to ensure no gaps at edges
  const startX = -size * 2;
  const startY = -size * 2;
  
  for (let y = 0; y < rows; y++) {
    for (let x = 0; x < cols; x++) {
      if ((x + y) % 2 === 0) {
        box(startX + x * size + offsetX, startY + y * size + offsetY, size, size);
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
