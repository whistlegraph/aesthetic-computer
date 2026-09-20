// 🐾 Organic 2026.09.19
// Procedural animal-like voices for the speaker worklet. One registry, one
// constructor shape `(params, id)`, so a new generator is a single file in
// `sound/organic/` and one line here. Shared parts live in `organic/parts.mjs`,
// the lifecycle every voice inherits in `organic/voice.mjs`.
import Growl from "./organic/growl.mjs";
import Breath from "./organic/breath.mjs";
import Howl from "./organic/howl.mjs";
import Chirp from "./organic/chirp.mjs";

export const ORGANICS = { growl: Growl, breath: Breath, howl: Howl, chirp: Chirp };

export function createOrganic({ kind, id, params = {} }) {
  const Kind = ORGANICS[kind];
  if (!Kind) {
    console.warn("🐾 Unknown organic:", kind);
    return null;
  }
  return new Kind(params, id);
}
