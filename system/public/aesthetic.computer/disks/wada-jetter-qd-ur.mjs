// wada-jetter-qd-ur, 26.11.25.16.59
// Play wada jetter QD UR by Aesthetic Dot Computer.

import * as pop from "../lib/pop.mjs";

const MANIFEST_URL = "/aesthetic.computer/disks/pop/wada-jetter-qd-ur.json";
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
    : { title: "wada jetter QD UR — Aesthetic Dot Computer", desc: "Music by Jeffrey Scudder." };
}

export { boot, paint, sim, act, receive, leave, meta };
