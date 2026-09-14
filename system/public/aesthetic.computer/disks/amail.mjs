// amail — alias for `mail`, the post of aesthetic.computer.
// The piece wore this name for a day (2026-09-13); the path keeps working,
// mirroring how `laer-klokken` aliases to `laklok`.

// 🥾 Boot (Runs once before first paint and sim)
export function boot({ colon, params, alias }) {
  // Pass all parameters through to `mail`.
  alias(`mail`, colon, params);
}
