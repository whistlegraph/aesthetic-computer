#!/usr/bin/env node
// iris-lane-watch.mjs — mirror the Iris lane board onto a watching Mac's MacPal.
//
// The mission daemon on the controller pushes the structured lane card
// (~/.local/share/desktop-badge/mission.json, with a `lanes` array) to the
// lane machines. A watcher (a laptop the operator sits at) can't be reached by
// them, so it pulls instead: read the first host that answers with a lane
// card and write it here verbatim, stamped with where it came from. MacPal
// draws the same board everywhere and a footer row says how fresh it is.
//
//   iris-lane-watch.mjs panda chicken            one pass
//   iris-lane-watch.mjs --every 10 panda chicken  loop (the LaunchAgent form)
//
// Hosts are ssh aliases, controller first. When none answers, the last board
// stays up marked `watch.ok: false`, so the badge shows the link is down
// instead of quietly freezing; MacPal drops it once updatedAt is 24h old.

import { execFile } from "node:child_process";
import { readFileSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

const OUT = join(homedir(), ".local/share/desktop-badge/mission.json");
const CARD = "~/.local/share/desktop-badge/mission.json";

const args = process.argv.slice(2);
let every = 0;
const hosts = [];
for (let i = 0; i < args.length; i++) {
  if (args[i] === "--every") every = Number(args[++i]) || 10;
  else hosts.push(args[i]);
}
if (!hosts.length) hosts.push("panda", "chicken");

function read(host) {
  return new Promise((resolve) => {
    execFile("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=6", host, `cat ${CARD}`],
      { timeout: 20000, maxBuffer: 4 << 20 }, (err, stdout) => {
        if (err) return resolve(null);
        try {
          const card = JSON.parse(stdout);
          resolve(Array.isArray(card.lanes) && card.lanes.length ? card : null);
        } catch { resolve(null); }
      });
  });
}

function save(doc) {
  const tmp = OUT + ".tmp";
  writeFileSync(tmp, JSON.stringify(doc, null, 2));
  renameSync(tmp, OUT);
}

async function pass() {
  const cards = await Promise.all(hosts.map(read));
  const i = cards.findIndex(Boolean);
  const at = new Date().toISOString();
  if (i >= 0) {
    save({ ...cards[i], role: "watcher", watch: { from: hosts[i], ok: true, at } });
    return;
  }
  // Nobody answered: keep the last board, flagged, rather than blanking it.
  try {
    const last = JSON.parse(readFileSync(OUT, "utf8"));
    if (Array.isArray(last.lanes)) {
      save({ ...last, watch: { from: last.watch?.from || hosts[0], ok: false, at } });
    }
  } catch {}
}

if (!every) await pass();
else for (;;) {
  try { await pass(); } catch (e) { console.error(e.message); }
  await new Promise((r) => setTimeout(r, every * 1000));
}
