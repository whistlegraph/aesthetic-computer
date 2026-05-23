// trancepenta, 2026.05.23
// pop/dance/ released single — see pop/RELEASES.md.
// Thin wrapper around lib/pop.mjs; see disks/pop/README.md for the recipe.

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL = "/aesthetic.computer/disks/pop/trancepenta.json";
let manifest = null;

async function boot($) {
  if (!manifest) {
    manifest = await fetch(MANIFEST_URL).then((r) => r.json());
  }
  return pop.boot($, manifest);
}

function paint($) { return pop.paint($); }
function sim($) { return pop.sim($); }
function act($) { return pop.act($); }
function leave($) { return pop.leave($); }
function meta() {
  return manifest
    ? pop.meta(manifest)
    : { title: "trancepenta — Aesthetic Dot Computer", desc: "5/4 chill-trance." };
}

export { boot, paint, sim, act, leave, meta };
