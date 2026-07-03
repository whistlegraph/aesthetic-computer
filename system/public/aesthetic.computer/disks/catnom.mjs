// Catnom, 2026.06.09
// The Categories edition of nom (shared lib/nom.mjs) — same game, but every
// board is one category from the classic parlor game, AC-flavored: slang,
// code words, aesthetic computer pieces, vibes, keys, internet words. Each
// munch speaks the word's meaning. Reuses lib/nom.mjs wholesale and just
// forces category mode at boot. Paints on disk's hd() native-resolution
// Canvas2D layer (the "hd" param), matching engnom.

import { boot as nomBoot, sim, paint, act, makeMeta } from "../lib/nom.mjs";

function boot(api) {
  nomBoot({ ...api, params: ["cat", "hd"] });
}

// Fixed identity (computed from params, not engine state) so the title is right
// even though meta() runs before boot().
function meta() {
  return makeMeta(["cat"]);
}

export { boot, sim, paint, act, meta };
