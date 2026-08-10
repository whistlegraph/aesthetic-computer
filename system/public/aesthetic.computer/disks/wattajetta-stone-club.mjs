// Wattajetta Stone Club, 26.08.09.17.03
// Water-engine stone club single by Aesthetic Dot Computer.

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL =
  "/aesthetic.computer/disks/pop/wattajetta-stone-club.json";
let manifest = null;

async function boot($) {
  if (!manifest) {
    manifest = await fetch(MANIFEST_URL).then((response) => response.json());
  }
  return pop.boot($, manifest);
}

function paint($) { return pop.paint($); }
function sim($) { return pop.sim($); }
function act($) { return pop.act($); }
function receive($) { return pop.receive($); }
function leave($) { return pop.leave($); }
function meta() {
  return manifest
    ? pop.meta(manifest)
    : {
        title: "Wattajetta Stone Club — Aesthetic Dot Computer",
        desc: "Water-engine stone club single.",
      };
}

export { boot, paint, sim, act, receive, leave, meta };
