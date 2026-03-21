// rect, 26.02.13.XX.XX
// This piece is an alias / shortcut for "box".
// rect is now deprecated in favor of box.

// 🥾 Boot (Runs once before first paint and sim)
export function boot({ colon, params, alias }) {
  // Just pass all parameters to `box`.
  alias(`box`, colon, params);
}
