// Mexinom, 2026.06.07
// The Spanish (Mexican-culture-flavored) word edition of nom (shared lib/nom.mjs) — comida,
// animales, colores, frutas, fiesta. Reuses lib/nom.mjs and forces Spanish
// word mode at boot.

import { boot as mathnomBoot, sim, paint, act, meta } from "../lib/nom.mjs";

function boot(api) {
  mathnomBoot({ ...api, params: ["spanish"] });
}

export { boot, sim, paint, act, meta };
