// Stamp, 24.02.13.15.23
// A basic stamp brush that imports a user's painting.

// 🖌️ Brush
function brush({ clear, pen }) {
  clear().stamp("@jeffrey/2024.2.13.15.31.05.635", pen.x, pen.y);
}

export { brush };