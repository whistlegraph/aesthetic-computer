// hellsine, 2026.05.27
// pop/hellsine/ released single — see pop/RELEASES.md.
// Thin wrapper around lib/pop.mjs (mirror of marimbaba.mjs).

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL = "/aesthetic.computer/disks/pop/hellsine.json";
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
    : { title: "hellsine — Aesthetic Dot Computer", desc: "felt-puppet hellfire waltz." };
}

export { boot, paint, sim, act, receive, leave, meta };
