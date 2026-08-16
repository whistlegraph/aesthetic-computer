#!/usr/bin/env node
// sing6.mjs — the extra sung material v6 needs, on top of bin/sing.mjs.
//
// bin/sing.mjs is v5's bank and is left exactly as it was, so v5 still
// renders byte-identically. This file only ADDS, and it adds two things,
// one for each of @jeffrey's vocal notes:
//
//   "so daaaaash daaaash gets lower in octaves"
//     → the descent takes. v5's dash ladder ran out below B3 for the two
//       upper performers, so the record could not actually sink. Camille
//       gets F#3 and D3, Alex gets D3, and Jeffrey already had the floor.
//       Real lower takes of the real voices — nothing is pitch-shifted.
//
//   "can we like quantize it more like 'bad' autotune it / eiffel 65 style"
//     → the `at-` bank. Every hook/chorus lead line and the two upper
//       dashes, rendered a second time through sing.py's new
//       `--autotune hard` mode: f0 snapped to B natural minor, held flat
//       inside 125 ms (1/16 at 120 BPM) cells, zero retune time, formants
//       untouched. render6.mjs plays the `at-` copy wherever it wants the
//       robot and the plain copy wherever it wants the human, which is how
//       the effect stays an effect.
//
// Renders are cached by an args hash in sung/.manifest6.json, so re-runs
// are free and this never disturbs sing.mjs's own manifest.
//
//   node pop/cult/bin/sing6.mjs           # → pop/cult/sung/*.wav
//   node pop/cult/bin/sing6.mjs --force

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");
const SAMPLES = resolve(LANE, "samples");
const SUNG = resolve(LANE, "sung");
const PY = resolve(REPO, "pop/.venv/bin/python");
const SCRIPT = resolve(HERE, "sing.py");
const FORCE = process.argv.includes("--force");

mkdirSync(SUNG, { recursive: true });
if (!existsSync(PY)) {
  console.error(`✗ no python at ${PY} — see bin/sing.mjs for the venv recipe`);
  process.exit(1);
}

const BANK = [];
const sing = (name, src, notes, opts = {}) => BANK.push({ name, src, notes, opts });

// ── (2a) THE DESCENT — real takes below B3 for Camille and Alex ───────
// The octave ladder render6 walks down is fs4 → d4 → b3 → fs3 → d3. v5's
// bank stopped at B3 for these two, which is why v5 could not descend past
// one step. Floors drop with the target: harvest will not find 147 Hz with
// its floor at 110.
sing("dash-camille-fs3-hold", "dash-camille", "F#3:1.50", { floor: 85 });
sing("dash-camille-d3-hold", "dash-camille", "D3:1.50", { floor: 70 });
sing("dash-alex-d3-hold", "dash-alex", "D3:1.50", { floor: 68 });
// …and the four-second versions, for act X where the hold IS the section.
sing("dashlong-camille-fs3", "dash-camille", "F#3:4.00", { floor: 85, vib: 20 });
sing("dashlong-alex-d3", "dash-alex", "D3:4.00", { floor: 68, vib: 18 });

// ── (3) THE `at-` BANK — hard autotune ───────────────────────────────
// drive is the one knob worth per-take attention: it scales how far the
// take's own (high-passed) pitch wander is exaggerated before the snap, so
// it decides how OFTEN the staircase steps and how far it leaps. Measured
// on run-real-fast: 2.6 steps between neighbours, 3.6 occasionally leaps a
// third, 4.6 starts losing the first syllable. Held vowels want less than
// short words because a long note gives the wander more room anyway.
const AT = (name, src, notes, opts = {}) =>
  sing(`at-${name}`, src, notes, {
    ...opts,
    autotune: "hard",
    atDrive: opts.atDrive ?? 3.2,
    atGrid: opts.atGrid ?? 125,
    // the four Saitou expression effects are what hard autotune is the
    // absence of: no vibrato, no overshoot. The mode ignores them, but
    // saying so here keeps the intent legible in the manifest.
    vib: 0, over: 0,
  });

// -- the hook's lead words -----------------------------------------------
// The short words get a FINER grid (1/32) and less drive: a 0.20 s
// syllable only spans one and a half 1/16 cells, so at 1/16 a single cell
// decides the whole word and the median can land it a fifth off. At 1/32
// each syllable gets three steps of its own.
AT("iwanna-a-sung", "i-wanna-a", "D4:0.20,E4:0.30", { atDrive: 2.4, atGrid: 62.5, atRange: 2.0 });
AT("iwanna-b-sung", "i-wanna-b", "B3:0.20,C#4:0.30", { atDrive: 2.4, atGrid: 62.5, atRange: 2.0 });
AT("iwanna-c-sung", "iwanna-c", "B3:0.22,C#4:0.28", { atDrive: 2.4, atGrid: 62.5, atRange: 2.0 });
AT("runrealfast-long-hi", "run-real-fast", "G4:0.38,F#4:0.88,D4:1.05", { atDrive: 3.2 });
AT("runrealfast-long-lo", "run-real-fast", "G3:0.38,F#3:0.88,D3:1.05", { floor: 60, atDrive: 3.0 });
AT("runrealfast-fast-hi", "run-real-fast-b", "G4:0.20,F#4:0.20,D4:0.60", { atDrive: 2.6, atGrid: 62.5, atRange: 2.0 });
AT("runrealfast-fast-lo", "run-real-fast-b", "G3:0.20,F#3:0.20,D3:0.60", { floor: 60, atDrive: 2.6, atGrid: 62.5, atRange: 2.0 });

// -- the chorus's lead words ---------------------------------------------
AT("iwannahide-hi", "iwanna-hide", "D4:0.24,E4:0.36,F#4:0.40", { atDrive: 3.4 });
AT("iwannahide-lo", "iwanna-hide", "D3:0.24,E3:0.36,F#3:0.40", { floor: 60, atDrive: 3.2 });
// "a — waaaaay" is 1.60 s on one note: the longest hold in the record and
// therefore the clearest place to hear the steps land in time.
AT("away-hi", "away", "G4:0.28,A4:1.60", { atDrive: 2.8 });
AT("away-lo", "away", "G3:0.28,A3:1.60", { floor: 60, atDrive: 2.6 });

// -- the two upper dashes, every rung of the descent ---------------------
// Jeffrey stays OUT of the autotune bank on purpose. He is the floor of the
// unison and the sub-octave under it; stepping the bottom voice makes the
// low end warble, and leaving one real human under the robot is what keeps
// the stack from reading as a synth.
const RUNGS = [["fs4", "F#4", 0], ["d4", "D4", 0], ["b3", "B3", 0],
  ["fs3", "F#3", 1], ["d3", "D3", 2]];
for (const [tag, note, low] of RUNGS)
  for (const [who, floor] of [["camille", [110, 85, 70][low]], ["alex", [90, 78, 68][low]]])
    AT(`dash-${who}-${tag}-hold`, `dash-${who}`, `${note}:1.50`, { floor, atDrive: 2.6, atRange: 4.0 });

// ── run ────────────────────────────────────────────────────────────────
const manifestPath = resolve(SUNG, ".manifest6.json");
const manifest = existsSync(manifestPath) && !FORCE
  ? JSON.parse(readFileSync(manifestPath, "utf8")) : {};

console.log(`→ v6 sung bank · ${BANK.length} renders · WORLD (pyworld) via bin/sing.py`);
let made = 0, cached = 0, failed = 0;
const report = {};

for (const { name, src, notes, opts } of BANK) {
  const inWav = resolve(SAMPLES, `${src}.wav`);
  if (!existsSync(inWav)) { console.warn(`  ! missing ${inWav}`); failed++; continue; }
  const outWav = resolve(SUNG, `${name}.wav`);

  const args = [
    "-W", "ignore", SCRIPT, inWav, outWav, "--notes", notes,
    "--f0-floor", String(opts.floor ?? 90),
    "--vibrato-cents", String(opts.vib ?? 32),
    "--overshoot-cents", String(opts.over ?? 42),
    "--formant-db", String(opts.formant ?? 3.2),
  ];
  if (opts.autotune === "hard")
    args.push("--autotune", "hard", "--autotune-scale", "b-minor",
      "--autotune-grid-ms", String(opts.atGrid ?? 125),
      "--autotune-drive", String(opts.atDrive ?? 3.2),
      "--autotune-range", String(opts.atRange ?? 3.0));
  args.push("--verify");

  const key = createHash("sha256")
    .update(args.slice(2).join(" ") + readFileSync(inWav).length)
    .digest("hex").slice(0, 16);
  if (!FORCE && manifest[name]?.key === key && existsSync(outWav)) {
    cached++; report[name] = manifest[name];
    continue;
  }
  try {
    const out = execFileSync(PY, args, { encoding: "utf8" }).trim();
    console.log(`  ${name.padEnd(28)} ${out.replace(/^\s*sing · /, "")}`);
    report[name] = { key, src, notes, autotune: opts.autotune ?? "off", line: out.trim() };
    made++;
  } catch (e) {
    console.warn(`  ✗ ${name}: ${String(e.stderr || e.message).trim().split("\n").pop()}`);
    failed++;
  }
}

writeFileSync(manifestPath, JSON.stringify(report, null, 2));
console.log(`✓ v6 bank → ${SUNG}  (${made} rendered, ${cached} cached${failed ? `, ${failed} failed` : ""})`);
