// laer-klokken — alias/shortcut for `laklok` (learn the clock).
// The piece now lives in `laklok.mjs` (backed by laklok.com); this keeps the
// original /laer-klokken path working, mirroring how `ff` aliases to
// `freaky-flowers`.

// 🥾 Boot (Runs once before first paint and sim)
export function boot({ colon, params, alias }) {
  // Pass all parameters through to `laklok`.
  alias(`laklok`, colon, params);
}
