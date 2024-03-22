// ff, 22.11.23.16.58 
// This piece is a meta-router / shortcut for "freaky-flowers".
// It's mostly an experiment in abstraction.

// 🥾 Boot (Runs once before first paint and sim)
export function boot({ colon, params, alias }) {
  // Just pass all parameters to `freaky-flowers`.
  alias(`freaky-flowers`, colon, params);
}

export { meta } from "../disks/freaky-flowers.mjs";