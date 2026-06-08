// Engnom, 2026.06.07
// The English/word edition of nom (shared lib/nom.mjs) — same game, word categories instead of
// numbers (animals, colors, rhymes, …). Reuses lib/nom.mjs wholesale and just
// forces word mode at boot.

import { boot as numnomBoot, sim, paint, act, meta } from "../lib/nom.mjs";

function boot(api) {
  numnomBoot({ ...api, params: ["words"] });
}

export { boot, sim, paint, act, meta };
