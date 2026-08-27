// femrag-plusplus, 2026.08.27
// pop/maytrax/ released single — see pop/RELEASES.md.
// Thin wrapper around lib/pop.mjs (mirror of hellsine.mjs).
// Also reachable as `femrag++`, which is how the record is spelled.

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL = "/aesthetic.computer/disks/pop/femrag-plusplus.json";
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
    : { title: "Femrag++ — Aesthetic Dot Computer", desc: "bell rag torn open by drum and bass." };
}

export { boot, paint, sim, act, receive, leave, meta };
