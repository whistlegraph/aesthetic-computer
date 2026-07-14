// loop.mjs — the whole game, once.
//
//   generate → gate → enqueue.   Then a human says cancel or ok, and that
//   verdict is what the NEXT generate reads. That's the entire machine.
//
// Run it on a cron (hourly) and the queue fills while you sleep; open `cancelok`
// and spend a minute emptying it. The loop's only ambition is to never waste
// one of those seconds on something that doesn't run.
//
//   node cancelok/loop.mjs             # one turn
//   node cancelok/loop.mjs --tries 3   # keep generating until one passes

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, rmSync } from "node:fs";
import { resolve } from "node:path";
import { REPO, QUEUE } from "./taste.mjs";
import { gate } from "./gate.mjs";
import { ship } from "./ship.mjs";

const DISKS = resolve(REPO, "system/public/aesthetic.computer/disks");
const TRIES = parseInt(process.argv[process.argv.indexOf("--tries") + 1] || "1", 10) || 1;
const SHIP = !process.argv.includes("--no-ship");

// The colony guards on "is the tree dirty," but that rule would mute this loop
// forever — @jeffrey always has something open. The honest guard is narrower:
// ship.mjs only ever stages the new pad and bags.json, so the only way it can
// step on live work is if HE is mid-edit in bags.json. Everything else in the
// tree is none of our business, and `pull --rebase --autostash` keeps it safe.
function clean() {
  const status = execFileSync("git", ["status", "--porcelain", "--", "system/public/aesthetic.computer/bags.json"], {
    cwd: REPO,
    encoding: "utf8",
  }).trim();
  return status === "";
}

// serve-local is what the gate measures against, so the loop owns its lifetime —
// a cron can't assume a terminal left one running.
function serving() {
  try {
    execFileSync("curl", ["-sf", "-o", "/dev/null", "http://localhost:8899/"], { timeout: 4000 });
    return true;
  } catch {
    return false;
  }
}

const queue = () => (existsSync(QUEUE) ? JSON.parse(readFileSync(QUEUE, "utf8")) : { items: [] });

function enqueue(item) {
  const q = queue();
  q.items = q.items.filter((i) => i.code !== item.code).concat(item);
  writeFileSync(QUEUE, JSON.stringify(q, null, 2) + "\n");
}

if (!serving()) {
  console.error("❌ nothing on :8899 — the gate has nothing to measure.");
  console.error("   start it: node marketing/av-reels/bin/serve-local.mjs");
  process.exit(1);
}

for (let turn = 1; turn <= TRIES; turn++) {
  console.log(`\n— turn ${turn}/${TRIES} —`);

  let made;
  try {
    const out = execFileSync("node", [resolve(REPO, "cancelok/generate.mjs")], {
      encoding: "utf8",
      cwd: REPO,
      stdio: ["inherit", "pipe", "inherit"],
      maxBuffer: 1024 * 1024 * 8,
    });
    made = JSON.parse(out.trim().split("\n").pop());
  } catch (e) {
    console.error("❌ generation failed this turn — moving on.");
    continue;
  }

  const r = gate(made.code);
  if (!r.pass) {
    // A pad that doesn't run never reaches a person. We delete it rather than
    // queue it: the human tier is for taste, and this was never a taste
    // question. The failure is still worth saying out loud — it's the only
    // way the perf contract gets tightened.
    rmSync(resolve(DISKS, `${made.code}.mjs`), { force: true });
    console.log(`❌ ${made.code} rejected by the gate: ${r.why}`);
    continue;
  }

  enqueue({ ...made, fps: r.fps, born: new Date().toISOString() });
  console.log(`✅ ${made.code} passed the gate (${r.fps}fps)`);

  if (SHIP) {
    if (!clean()) {
      console.log("✋ bags.json is mid-edit — the queen is curating. Left it queued locally.");
      process.exit(0);
    }
    ship(made.code, made.trait);
  } else {
    console.log("   (--no-ship) queued locally. open: cancelok");
  }
  process.exit(0);
}

console.log("\nno candidate survived the gate this run.");
process.exit(1);
