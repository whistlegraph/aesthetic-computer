// numbnom, 2026.06.08
// Number edition of the "nom" muncher game — eat grid squares that match the
// math rule (odds, evens, primes, multiples, factors) on a beat, dodge troggles.
// Shares the engine in ../lib/nom.mjs with engnom (words) and mexinom (español).

import { boot as nomBoot, sim, paint, act, makeMeta } from "../lib/nom.mjs";

function boot(api) {
  // Pass colon params through (numbnom:words / numbnom:spanish still work);
  // default to numbers so navigating here always resets to number mode.
  const params = api.params?.length ? api.params : ["numbers"];
  nomBoot({ ...api, params });
}

// Fixed identity (computed from params, not engine state) so the title is right
// even though meta() runs before boot().
function meta() {
  return makeMeta(["numbers"]);
}

export { boot, sim, paint, act, meta };
