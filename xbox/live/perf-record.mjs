// perf-record.mjs — take one oskiewar performance reading and append it to this
// machine's ledger.
//
//   node xbox/live/perf-record.mjs            # record a reading
//   node xbox/live/perf-record.mjs --quick    # cpu only, skip determinism
//   node xbox/live/perf-record.mjs --show     # print the ledger's recent history
//
// The point is drift. A single number says nothing — 52.97 µs per tick is only
// alarming next to the 27.66 µs the architecture was written against, which is
// how we learned the sim had roughly doubled in cost without anyone noticing.
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

const reading = {
  at: new Date().toISOString(),
  host: HOST,
  node: process.version,
  commit: git("rev-parse", "--short", "HEAD"),
  branch: git("rev-parse", "--abbrev-ref", "HEAD"),
  dirty: git("status", "--porcelain").length > 0,
};

const cpu = probe("probe.mjs");
if (!cpu.json?.performance) {
  console.error("probe.mjs produced no performance block:\n" + cpu.text.slice(-800));
  process.exit(1);
}
Object.assign(reading, {
  usPerTick: cpu.json.performance.usPerTick,
  ticksPerSecondPerCore: cpu.json.performance.ticksPerSecondPerCore,
  realtimeMatchesPerCore: cpu.json.performance.realtimeMatchesPerCore,
  resimulatePerTickUs: cpu.json.performance.resimulatePerTickUs,
  hash: cpu.json.crossProcessHash ?? null,
});

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

const previous = history().slice(0, -1).filter((row) => row.host === HOST).at(-1);
console.log(`${reading.host} · ${reading.commit}${reading.dirty ? "+dirty" : ""} ` +
  `· node ${reading.node}`);
console.log(`  ${reading.usPerTick} µs/tick · ` +
  `${reading.realtimeMatchesPerCore} realtime worlds per core · hash ${reading.hash}`);
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
