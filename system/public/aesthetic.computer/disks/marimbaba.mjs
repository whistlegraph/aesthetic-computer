// marimbaba, 2026.05.23
// pop/marimba/ released single — see pop/RELEASES.md.
// Thin wrapper around lib/pop.mjs (mirror of how laer-klokken wraps chat).
// Add a new track piece by dropping a `disks/pop/<slug>.json` manifest +
// a 12-line wrapper like this one.

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL = "/aesthetic.computer/disks/pop/marimbaba.json";
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
    : { title: "marimbaba — Aesthetic Dot Computer", desc: "marimba lullaby." };
}

export { boot, paint, sim, act, leave, meta };
