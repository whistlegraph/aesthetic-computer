// mail — alias for `amail`, the post of aesthetic.computer.
// The piece now lives in `amail.mjs` so it has a proper noun of its own; this
// keeps the original /mail path working, mirroring how `laer-klokken`
// aliases to `laklok`.

// 🥾 Boot (Runs once before first paint and sim)
export function boot({ colon, params, alias }) {
  // Pass all parameters through to `amail`.
  alias(`amail`, colon, params);
}
