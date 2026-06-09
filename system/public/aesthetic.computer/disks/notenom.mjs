// notenom, 2026.06.08
// Music edition of the "nom" muncher game — eat the grid squares whose note
// matches the rule (C MAJOR, A MINOR, SHARPS, chords, HIGH / LOW) on the beat,
// while dodging troggles. Each munch voices the note; the rule's scale plays on
// board start. Shares the engine in ../lib/nom.mjs with numbnom (numbers),
// engnom (words), mexinom (español) and dannom (dansk) — this just forces note
// mode (BPM 92, per-cell note voicing) at boot.

import { boot as nomBoot, sim, paint, act, makeMeta } from "../lib/nom.mjs";

function boot(api) {
  nomBoot({ ...api, params: ["notes"] });
}

// Fixed note identity (computed from params, not engine state) so the title is
// right even though meta() runs before boot().
function meta() {
  return makeMeta(["notes"]);
}

export { boot, sim, paint, act, meta };
