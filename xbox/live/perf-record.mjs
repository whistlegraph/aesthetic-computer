// perf-record.mjs — take one oskiewar performance reading and append it to this
// machine's ledger.
//
//   node xbox/live/perf-record.mjs            # record a reading
//   node xbox/live/perf-record.mjs --quick    # cpu only, skip determinism
//   node xbox/live/perf-record.mjs --show     # print the ledger's recent history
//
// The point is drift. A single number says nothing — the sim costing 21 µs a
// tick only means something next to the 9 µs it cost at 2fcb1b6f4, which is how
// we learned it had more than doubled, and which three commits bought it.
// So every reading is stamped with the commit and the machine and kept, and the
// tool compares each one against the run before it on the same machine.
//
// It shells out to the probes in `tmp/oskiewar-world/`, which are UNTRACKED —
// they live in one person's working directory and have to be copied to any
// machine that measures. That is a real trap: a fresh clone can run this tool
// and find nothing to run. It says so plainly rather than failing obscurely.
//
// The cross-process hash matters as much as the timing. Two machines running
// the same commit must produce the same hash — that bit-exactness is what the
// authoritative-tick netcode plan rests on, so a change there is a louder
// finding than any slowdown.

import { execFileSync } from "node:child_process";
import { appendFileSync, existsSync, mkdirSync, readFileSync } from "node:fs";
import { homedir, hostname } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const PROBES = join(REPO, "tmp", "oskiewar-world");
const LEDGER_DIR = join(homedir(), ".ac-oskiewar-perf");
const HOST = hostname().replace(/\.local$/, "");
const LEDGER = join(LEDGER_DIR, `${HOST}.jsonl`);

const flags = new Set(process.argv.slice(2));

function history() {
  if (!existsSync(LEDGER)) return [];
  return readFileSync(LEDGER, "utf8").split("\n").filter(Boolean)
    .map((line) => { try { return JSON.parse(line); } catch { return null; } })
    .filter(Boolean);
}

if (flags.has("--show")) {
  const rows = history();
  if (!rows.length) { console.log(`no readings yet in ${LEDGER}`); process.exit(0); }
  console.log(`${rows.length} reading(s) · ${LEDGER}\n`);
  for (const row of rows.slice(-12)) {
    console.log(`${row.at}  ${String(row.usPerTick).padStart(7)} µs/tick  ` +
      `${String(row.realtimeMatchesPerCore ?? "—").padStart(4)} worlds  ` +
      `${row.commit}  ${row.hash ?? "—"}`);
  }
  process.exit(0);
}

if (!existsSync(join(PROBES, "probe.mjs"))) {
  console.error(`No probes at ${PROBES}.\n` +
    "They are untracked, so they do not arrive with a clone. Copy them from a\n" +
    "machine that has them:\n" +
    `  rsync -az tmp/oskiewar-world/ ${HOST}:<repo>/tmp/oskiewar-world/`);
  process.exit(2);
}

const git = (...args) => {
  try {
    return execFileSync("git", args, { cwd: REPO, encoding: "utf8" }).trim();
  } catch { return "unknown"; }
};

// The probes print a human preamble before their JSON, so take the last
// balanced object rather than assuming the whole stream parses.
function lastJson(text) {
  const start = text.lastIndexOf("\n{");
  const candidate = start >= 0 ? text.slice(start + 1) : text;
  try { return JSON.parse(candidate); } catch { return null; }
}

function probe(name, args = []) {
  const out = execFileSync(process.execPath, [join(PROBES, name), ...args],
    { cwd: REPO, encoding: "utf8", maxBuffer: 32 * 1024 * 1024 });
  return { text: out, json: lastJson(out) };
}

const METRIC = "bare-sim";   // bumped if what we time ever changes again

const reading = {
  at: new Date().toISOString(),
  metric: METRIC,
  host: HOST,
  node: process.version,
  commit: git("rev-parse", "--short", "HEAD"),
  branch: git("rev-parse", "--abbrev-ref", "HEAD"),
  dirty: git("status", "--porcelain").length > 0,
};

// Time the simulation directly rather than reading probe.mjs's number, because
// that number is not the simulation. Its timed pass snapshots the world, walks
// it into a canonical form and SHA-256s it on every single tick — around 30 µs
// against a sim that costs 21, so roughly two thirds of what it reports is the
// instrument watching itself. Worse, all that per-tick allocation drives GC,
// which is where the 20-50% run-to-run spread came from; timing the bare loop
// lands inside 3% and only then is a regression visible at all.
const TICK_US = 16667;
const TICKS = 1800;
const REPEATS = 3;

const { createHeadless, inputScript } = await import(
  new URL("file://" + join(PROBES, "harness.mjs")).href);

function timeBareSim() {
  const script = inputScript(20260807, TICKS);   // (seed, ticks) — in that order
  const host = createHeadless({ withPaint: false });
  host.fight.enterGame();
  host.fight.startFight();
  const started = process.hrtime.bigint();
  for (let tick = 0; tick < script.length; tick++) {
    host.setPads(script[tick]);
    host.tick(TICK_US);
  }
  return Number(process.hrtime.bigint() - started) / 1000 / TICKS;
}

// V8 needs to see the sim hot before its numbers mean anything: timed cold, the
// first pass runs ~35% slow and drags the spread out past the signal. Throw the
// warmups away rather than averaging them in.
timeBareSim();
timeBareSim();
// The minimum is the least-disturbed run. Background load can only ever make a
// sample slower, never faster, so min is the honest estimator here.
const samples = Array.from({ length: REPEATS }, timeBareSim);
const usPerTick = +Math.min(...samples).toFixed(2);
Object.assign(reading, {
  usPerTick,
  spread: +(Math.max(...samples) - Math.min(...samples)).toFixed(2),
  ticksPerSecondPerCore: Math.round(1e6 / usPerTick),
  realtimeMatchesPerCore: Math.round(1e6 / usPerTick / 60),
});

// The hash is the correctness signal and costs a full probe run, so it is worth
// it unless someone asked for speed.
if (!flags.has("--quick")) {
  const cpu = probe("probe.mjs");
  reading.hash = cpu.json?.crossProcessHash ?? null;
  reading.harnessUsPerTick = cpu.json?.performance?.usPerTick ?? null;
}

if (!flags.has("--quick")) {
  try {
    const determinism = probe("nondeterminism.mjs");
    reading.deterministic = /identical/.test(determinism.text);
  } catch {
    reading.deterministic = null;   // a crashed probe is unknown, not a pass
  }
}

mkdirSync(LEDGER_DIR, { recursive: true });
appendFileSync(LEDGER, JSON.stringify(reading) + "\n");

const previous = history().slice(0, -1)
  .filter((row) => row.host === HOST && row.metric === METRIC).at(-1);
console.log(`${reading.host} · ${reading.commit}${reading.dirty ? "+dirty" : ""} ` +
  `· node ${reading.node}`);
console.log(`  ${reading.usPerTick} µs/tick (±${reading.spread}) · ` +
  `${reading.realtimeMatchesPerCore} realtime worlds per core` +
  (reading.hash ? ` · hash ${reading.hash}` : ""));
if (reading.harnessUsPerTick) {
  console.log(`  (probe.mjs reports ${reading.harnessUsPerTick} µs — that figure` +
    " includes its own per-tick hashing, not just the sim)");
}
if (reading.deterministic === false) console.log("  ⚠️  determinism probe DIVERGED");

if (previous) {
  const drift = (reading.usPerTick - previous.usPerTick) / previous.usPerTick * 100;
  const sign = drift >= 0 ? "+" : "";
  console.log(`  vs ${previous.commit} (${previous.at.slice(0, 16)}): ` +
    `${sign}${drift.toFixed(1)}% cpu`);
  if (previous.hash && reading.hash && previous.hash !== reading.hash) {
    console.log(`  ⚠️  cross-process hash CHANGED ${previous.hash} -> ${reading.hash}`);
    console.log("     Same commit should mean the same hash. If the commit moved," +
      " this is expected; if it did not, the sim stopped being deterministic.");
  }
  // Ten percent is wider than run-to-run noise on a quiet machine and narrower
  // than anything a person would notice, which makes it a useful place to look.
  if (drift > 10) console.log("  ⚠️  slower by more than 10% since the last reading");
}
console.log(`  recorded to ${LEDGER}`);
