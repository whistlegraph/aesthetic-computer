// amaythingra, 2026.06.04
// pop/big-pictures/ released single — see pop/RELEASES.md.
// Thin wrapper around lib/pop.mjs (mirror of marimbaba.mjs).

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL = "/aesthetic.computer/disks/pop/amaythingra.json";
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
    : { title: "amaythingra — Aesthetic Dot Computer", desc: "amazing grace, deep-house." };
}

export { boot, paint, sim, act, leave, meta };
