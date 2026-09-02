// wannadash, 26.09.01.23.10
// Whistlegraph Dot Org — dot dot dot, dash dash dash, the three of us.

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL = "/aesthetic.computer/disks/pop/wannadash.json";
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
        title: "wannadash — Whistlegraph Dot Org",
        desc: "dot dot dot, dash dash dash, the three of us.",
      };
}

export { boot, paint, sim, act, receive, leave, meta };
