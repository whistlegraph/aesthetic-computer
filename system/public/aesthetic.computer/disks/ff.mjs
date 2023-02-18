// ff, 22.11.23.16.58 
// This piece is a meta-router / shortcut for "freaky-flowers".
// It's mostly an experiment in abstraction.

// 🥾 Boot (Runs once before first paint and sim)
export function boot({ params, jump, store }) {
  // Just pass all parameters to `freaky-flowers`.
  // store["ff"] = true; // But remember where we came from!
  jump(`freaky-flowers` + params.map((p) => `~` + p).join(""), true, false);
}

export { meta } from "../disks/freaky-flowers.mjs";