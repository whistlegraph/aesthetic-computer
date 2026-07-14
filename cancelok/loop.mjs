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

// Breed or invent. Two thirds of the time we breed from something that already
// earned attention; one third we throw the history away and make something with
// no parents at all.
//
// That third is not a hedge, it's the anti-collapse valve. A loop that only ever
// breeds from winners walks straight to a local maximum and then spends the rest
// of its life congratulating itself there — and it can never notice, because every
// generation still scores well against the last one. The only cure is to keep
// spending real attention on ideas the history says nothing about.
const EVOLVE_ODDS = 0.66;

for (let turn = 1; turn <= TRIES; turn++) {
  console.log(`\n— turn ${turn}/${TRIES} —`);

  const breeding = !process.argv.includes("--fresh") && Math.random() < EVOLVE_ODDS;
  const tool = breeding ? "cancelok/evolve.mjs" : "cancelok/generate.mjs";
  // The non-breeding turn is BLIND — it isn't told what anyone liked. A "fresh"
  // generation that still reads the pheromone isn't fresh at all: it just invents
  // another pendulum, politely. That was the bug that let five of them through.
  const argv = breeding ? [] : ["--blind"];

  let made;
  try {
    const out = execFileSync("node", [resolve(REPO, tool), ...argv], {
      encoding: "utf8",
      cwd: REPO,
      stdio: ["inherit", "pipe", "inherit"],
      maxBuffer: 1024 * 1024 * 8,
    });
    made = JSON.parse(out.trim().split("\n").pop());
  } catch (e) {
    // Exit 2 = nothing kept yet (a cold start). Exit 3 = every line is saturated
    // and breeding would only deepen the rut. Both mean the same thing: stop
    // asking the history what to do, and go make something it can't predict.
    if (breeding && (e.status === 2 || e.status === 3)) {
      console.log(
        e.status === 3
          ? "   the gene pool has collapsed — going blind."
          : "   nothing kept yet — inventing from scratch instead.",
      );
      try {
        const out = execFileSync("node", [resolve(REPO, "cancelok/generate.mjs"), "--blind"], {
          encoding: "utf8",
          cwd: REPO,
          stdio: ["inherit", "pipe", "inherit"],
          maxBuffer: 1024 * 1024 * 8,
        });
        made = JSON.parse(out.trim().split("\n").pop());
      } catch {
        console.error("❌ generation failed this turn — moving on.");
        continue;
      }
    } else {
      console.error("❌ generation failed this turn — moving on.");
      continue;
    }
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
