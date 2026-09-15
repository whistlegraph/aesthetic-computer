// amazing-grace, 26.09.15.16.40
// Aesthetic Dot Computer — verse 1 of the hymn, sung by jeffrey-pvc over the hymn's own cells.

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL = "/aesthetic.computer/disks/pop/amazing-grace.json";
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
        title: "amazing grace — Aesthetic Dot Computer",
        desc: "verse 1 of the hymn, sung by jeffrey-pvc over the hymn's own cells.",
      };
}

export { boot, paint, sim, act, receive, leave, meta };
