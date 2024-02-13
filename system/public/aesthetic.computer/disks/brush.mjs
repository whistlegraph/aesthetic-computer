// Brush, 23.09.29.01.48
// The most basic brush.

// 🖌️ Brush
function brush({ pen, ink }) {
  ink().circle(pen.x, pen.y, 16, true);
}

export { brush };