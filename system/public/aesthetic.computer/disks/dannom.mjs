// Dannom, 2026.06.08
// The Danish word edition of nom (shared lib/nom.mjs) — mad, dyr, farver, frugt,
// hygge. Reuses lib/nom.mjs and forces Danish word mode at boot. Each munch
// speaks the English translation aloud, so it doubles as a vocab drill.

import { boot as nomBoot, sim, paint, act, meta } from "../lib/nom.mjs";

function boot(api) {
  nomBoot({ ...api, params: ["danish"] });
}

export { boot, sim, paint, act, meta };
