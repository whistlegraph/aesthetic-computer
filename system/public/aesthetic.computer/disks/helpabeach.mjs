// helpabeach, 2026.05.23
// pop/chillwave/ released single — see pop/RELEASES.md.
// Thin wrapper around lib/pop.mjs; see disks/pop/README.md for the recipe.

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL = "/aesthetic.computer/disks/pop/helpabeach.json";
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
function receive($) { return pop.receive($); } // stream:* events from bios
function meta() {
  return manifest
    ? pop.meta(manifest)
    : { title: "helpabeach — Aesthetic Dot Computer", desc: "chillwave instrumental." };
}

export { boot, paint, sim, act, receive, leave, meta };
