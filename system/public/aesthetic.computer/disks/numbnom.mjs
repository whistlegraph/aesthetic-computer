// numbnom, 2026.06.08
// Number edition of the "nom" muncher game — eat grid squares that match the
// math rule (odds, evens, primes, multiples, factors) on a beat, dodge troggles.
// Shares the engine in ../lib/nom.mjs with engnom (words) and mexinom (español).

import { boot as nomBoot, sim, paint, act, meta } from "../lib/nom.mjs";

function boot(api) {
  // Pass colon params through (numbnom:words / numbnom:spanish still work);
  // default to numbers so navigating here always resets to number mode.
  nomBoot({ ...api, params: api.params?.length ? api.params : ["numbers"] });
}

export { boot, sim, paint, act, meta };
