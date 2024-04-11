// Fill, 2024.4.11.07.08.03.042
// Fill pixels in with a particular color.

let color;

// 🥤️ Filter
function filter({ params, num, pen, flood }) {
  color ||= num.parseColor(params);
  flood(pen.x, pen.y, color);
}

export { filter };