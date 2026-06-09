// Rusnom, 2026.06.09
// The Russian word edition of nom (shared lib/nom.mjs) — еда, животные, цвета,
// фрукты, природа. Reuses lib/nom.mjs and forces Russian word mode at boot.
// Each munch speaks the English translation aloud, so it doubles as a vocab
// drill. Cyrillic renders via MatrixChunky8 (full Russian alphabet).

import { boot as nomBoot, sim, paint, act, makeMeta } from "../lib/nom.mjs";

function boot(api) {
  nomBoot({ ...api, params: ["russian"] });
}

// Fixed Russian identity (computed from params, not engine state) so the title
// is right even though meta() runs before boot().
function meta() {
  return makeMeta(["russian"]);
}

export { boot, sim, paint, act, meta };
