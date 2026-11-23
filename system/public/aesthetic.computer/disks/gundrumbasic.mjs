// gundrumbasic, 2026.11.23
// pop/gundrum/ released single — see pop/RELEASES.md.
// Thin wrapper around lib/pop.mjs; see disks/pop/README.md for the recipe.

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL = "/aesthetic.computer/disks/pop/gundrumbasic.json";
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
    : { title: "Gundrumbasic — Aesthetic Dot Computer", desc: "a drum-and-bass fugue where every drum is a gun." };
}

export { boot, paint, sim, act, receive, leave, meta };
