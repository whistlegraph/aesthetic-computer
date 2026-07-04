// fluttabap360, 2026.07.03
// pop/marimba/ released single — see pop/RELEASES.md.
// Thin wrapper around lib/pop.mjs (mirror of how laer-klokken wraps chat).
// Add a new track piece by dropping a `disks/pop/<slug>.json` manifest +
// a 12-line wrapper like this one.

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL = "/aesthetic.computer/disks/pop/fluttabap360.json";
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
    : { title: "fluttabap360 — Aesthetic Dot Computer", desc: "the fat six-minute butterfly-park banger." };
}

export { boot, paint, sim, act, receive, leave, meta };
