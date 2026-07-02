// Engnom, 2026.06.07
// The English/word edition of nom (shared lib/nom.mjs) — same game, word categories instead of
// numbers (animals, colors, rhymes, …). Reuses lib/nom.mjs wholesale and just
// forces word mode at boot. First test bed for the "hd" param: paint runs on
// disk's hd() native-resolution Canvas2D layer (worker OffscreenCanvas).

import { boot as nomBoot, sim, paint, act, makeMeta } from "../lib/nom.mjs";

function boot(api) {
  nomBoot({ ...api, params: ["words", "hd"] });
}

// Fixed identity (computed from params, not engine state) so the title is right
// even though meta() runs before boot().
function meta() {
  return makeMeta(["words"]);
}

export { boot, sim, paint, act, meta };
