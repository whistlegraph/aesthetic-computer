// Mexinom, 2026.06.07
// The Spanish (Mexican-culture-flavored) word edition of nom (shared lib/nom.mjs) — comida,
// animales, colores, frutas, fiesta. Reuses lib/nom.mjs and forces Spanish
// word mode at boot.

import { boot as nomBoot, sim, paint, act, makeMeta } from "../lib/nom.mjs";

function boot(api) {
  nomBoot({ ...api, params: ["spanish"] });
}

// Fixed Spanish identity (computed from params, not engine state) so the title
// is right even though meta() runs before boot().
function meta() {
  return makeMeta(["spanish"]);
}

export { boot, sim, paint, act, meta };
