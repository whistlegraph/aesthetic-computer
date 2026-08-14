#!/usr/bin/env node
// sing.mjs — render the sung bank for the cult remix v2.
//
// @jeffrey's headline note on v1 was "we should be extending their words —
// use our whole speech-to-singing pipeline". This is that pipeline: every
// entry below is a spoken/chanted word off the whistlegraph (or an
// ElevenLabs jeffrey-pvc line) run through bin/sing.py, which is the
// Saitou recipe on the WORLD vocoder — the phonemes get lengthened to fit
// the note, the f0 contour is REPLACED by the score's (not shifted), and a
// modest singer's formant is added. A 0.33 s spoken "dash" comes back as a
// 1.5 s sung F#4 that holds.
//
// Renders are cached by a hash of their arguments, so re-runs are free.
//
//   node pop/cult/bin/sing.mjs          # → pop/cult/sung/*.wav
//   node pop/cult/bin/sing.mjs --force  # ignore the cache

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
  console.error(`✗ no python at ${PY}\n` +
    `  create it:  python3 -m venv pop/.venv && pop/.venv/bin/pip install ` +
    `numpy soundfile pyworld librosa "setuptools<81"`);
  process.exit(1);
}

// ── the score, in B minor ─────────────────────────────────────────────
// Timings are in seconds at 120 BPM (beat = 0.5 s, bar = 2 s), which is
// the tempo the source chant was already sitting at.
//
// THE HOOK — four bars, so every word gets room to hold:
//
//   0.00  dash ────────── F#4, 1.50 s   (a 0.33 s chant hit, sung out)
//   1.50  i wanna         D4 → E4
//   2.00  dash ────────── D4,  1.50 s
//   3.50  i wanna         B3 → C#4
//   4.00  run it fast     G4 → F#4 → D4 (D4 held 1.20 s)
//   6.00  dot             B3, 0.22 s
//   6.50  dot             F#3, 0.22 s
//   7.00  ─ rest ─                       (the chill needs the hole)
//
// The dashes are long and the dots are short, which is morse doing the
// phrasing for us. The SOS figure below takes that literally.
const BANK = [];
const sing = (name, src, notes, opts = {}) =>
  BANK.push({ name, src, notes, opts });

// -- the hook's held dashes, one render per performer so the three of
//    them can be stacked into a chord on the same syllable ------------
for (const [who, floor] of [["camille", 110], ["alex", 90], ["jeffrey", 65]]) {
  sing(`dash-${who}-fs4-hold`, `dash-${who}`, "F#4:1.50", { floor });
  sing(`dash-${who}-d4-hold`, `dash-${who}`, "D4:1.50", { floor });
}
// -- the hook's ElevenLabs words, now sung -------------------------------
sing("iwanna-a-sung", "i-wanna-a", "D4:0.20,E4:0.30");
sing("iwanna-b-sung", "i-wanna-b", "B3:0.20,C#4:0.30");
sing("runitfast-sung", "run-it-fast", "G4:0.40,F#4:0.40,D4:1.20");
// -- the hook's dots: short by design, so barely stretched --------------
sing("dot-b3", "dot-camille", "B3:0.22", { vib: 0, over: 20 });
sing("dot-fs3", "dot-alex", "F#3:0.22", { vib: 0, over: 20, floor: 90 });
sing("dot-d4", "dot-camille", "D4:0.22", { vib: 0, over: 20 });
sing("dot-a3", "dot-alex", "A3:0.22", { vib: 0, over: 20, floor: 90 });

// -- THE SOS FIGURE ------------------------------------------------------
// The source's own caption is "dot dot dot dash dash dash dot dot dot
// (jk)" — SOS in morse. Four bars: three shorts, three longs, three
// shorts. It reads as a riff, not a gimmick, because the long/short
// contrast is exactly what a chill techno lead wants anyway.
sing("sos-dash-d4", "dash-camille", "D4:1.20", { floor: 110 });
sing("sos-dash-fs4", "dash-camille", "F#4:1.20", { floor: 110 });
sing("sos-dash-e4", "dash-camille", "E4:1.20", { floor: 110 });

// -- THE CHOIR ----------------------------------------------------------
// One word — the sung "cult", B3 in the source — held four seconds at
// every chord tone the progression pool visits. Stacked in the renderer
// with small offsets, one take becomes a section's worth of pad.
const CHOIR = ["B2", "D3", "F#3", "G3", "A3", "B3", "C#4", "D4", "E4", "F#4", "G4"];
for (const n of CHOIR)
  sing(`cult-${n.replace("#", "s").toLowerCase()}`, "cult", `${n}:4.00`,
    { floor: n.endsWith("2") ? 55 : n.endsWith("3") ? 75 : 100, vib: 26 });

// -- "the three of us are in a" as an actual melody ---------------------
// Rising in the hollow, answered descending later. Six syllables mapped
// to six notes, the last one held.
sing("threeofus-rise", "three-of-us", "B3:0.50,C#4:0.50,D4:0.50,D4:0.50,E4:0.50,F#4:1.50");
sing("threeofus-fall", "three-of-us", "F#4:0.50,E4:0.50,D4:0.50,D4:0.50,C#4:0.50,B3:1.50");

// -- a sung bass voice: Jeffrey's B2 dash, held --------------------------
for (const n of ["B2", "G2", "A2", "E2"])
  sing(`bassdash-${n.toLowerCase()}`, "dash-jeffrey", `${n}:2.00`,
    { floor: 55, vib: 12, over: 20, formant: 0 });

// ══ v3 ═════════════════════════════════════════════════════════════════
// @jeffrey dictated a real four-line chorus and asked for it up front:
//
//   run real fast
//   i wanna hide away
//   i wanna dash
//   dot dot dash
//
// Over Bm · D · G · Em, two bars a chord, eight bars total:
//
//   0.00  run real fast      G4 → F#4 → D4 held 1.20      (Bm)
//   2.00  i wanna hide       D4 → E4 → F#4                 (D)
//   3.00  a - waaaay         G4 → A4 held 1.60             (D)
//   4.00  i wanna            B3 → C#4                      (G)
//   4.50  dash ───────────── D4 held 1.50                  (G)
//   6.00  dot · dot          B3 · G3                       (Em)
//   7.00  dash ───────────── B3 held 1.50 (into the turn)  (Em)
//
// Two more asks land here: octave mixing (#6) and a double-time
// "run real fast" that lets the lyric perform itself. So every lead line
// is rendered TWICE, an octave apart, and "run real fast" also comes back
// at exactly half its length.

// -- line 1 ---------------------------------------------------------------
sing("runrealfast-hi", "run-real-fast", "G4:0.40,F#4:0.40,D4:1.20");
// v4: @jeffrey — "the 'real' in run real should be longer". The hold moves
// off "fast" and onto "real": run (short) · reeeeeaaaal (2.0 s) · fast (short).
// Same 2.80 s slot as the -hi take plus a beat, so it still lands on the grid.
sing("runrealfast-long-hi", "run-real-fast", "G4:0.38,F#4:0.88,D4:1.05");
sing("runrealfast-long-lo", "run-real-fast", "G3:0.38,F#3:0.88,D3:1.05", { floor: 60 });
sing("runrealfast-lo", "run-real-fast", "G3:0.40,F#3:0.40,D3:1.20", { floor: 60 });
// the lyric performing itself — exactly half the duration
sing("runrealfast-fast-hi", "run-real-fast-b", "G4:0.20,F#4:0.20,D4:0.60");
sing("runrealfast-fast-lo", "run-real-fast-b", "G3:0.20,F#3:0.20,D3:0.60", { floor: 60 });

// -- line 2: the new words, and the hold is the point ---------------------
sing("iwannahide-hi", "iwanna-hide", "D4:0.24,E4:0.36,F#4:0.40");
sing("iwannahide-lo", "iwanna-hide", "D3:0.24,E3:0.36,F#3:0.40", { floor: 60 });
sing("away-hi", "away", "G4:0.28,A4:1.60");                 // "a — waaaaay"
sing("away-lo", "away", "G3:0.28,A3:1.60", { floor: 60 });
sing("hideaway-hi", "hide-away", "F#4:0.40,G4:0.30,A4:1.30");  // change-up form

// -- line 3 / line 4 fragments -------------------------------------------
sing("iwanna-c-sung", "iwanna-c", "B3:0.22,C#4:0.28");
sing("dotdotdash-hi", "dotdotdash", "B3:0.28,B3:0.28,E4:1.44");
sing("dotdotdash-lo", "dotdotdash", "B2:0.28,B2:0.28,E3:1.44", { floor: 55 });

// -- the held dashes, per performer, in two octaves -----------------------
// The unison is real people. Camille and Alex hold the upper octave, and
// Jeffrey — whose chant hit was already B2 — holds the one below it, so
// the octave mix is three humans rather than a pitch-shifted copy.
const HI = ["B3", "D4", "E4", "F#4", "G4"];
const LO = ["B2", "D3", "E3", "F#3", "G3"];
for (const n of HI)
  for (const [who, floor] of [["camille", 110], ["alex", 90]])
    sing(`dash-${who}-${n.replace("#", "s").toLowerCase()}-hold`, `dash-${who}`,
      `${n}:1.50`, { floor });
for (const n of LO)
  sing(`dash-jeffrey-${n.replace("#", "s").toLowerCase()}-hold`, "dash-jeffrey",
    `${n}:1.50`, { floor: 55 });
// v5: the two holds acts II and VII need that the earlier octave tables
// happened not to cover — Alex down at F#3 (his own chant register, for the
// "one voice at a time" introduction) and Jeffrey up at B3 (so the chorus's
// closing "dot dot dash" can stack all three on B3).
sing("dash-alex-fs3-hold", "dash-alex", "F#3:1.50", { floor: 90 });
sing("dash-jeffrey-b3-hold", "dash-jeffrey", "B3:1.50", { floor: 55 });
// four-second versions for the dash-only break, where the hold IS the section
for (const n of ["B3", "D4", "F#4", "G4", "E4"])
  sing(`dashlong-camille-${n.replace("#", "s").toLowerCase()}`, "dash-camille", `${n}:4.00`, { floor: 110, vib: 26 });
for (const n of ["B2", "D3", "F#3", "G3", "E3"])
  sing(`dashlong-jeffrey-${n.replace("#", "s").toLowerCase()}`, "dash-jeffrey", `${n}:4.00`, { floor: 55, vib: 20 });

// -- the dots, per performer, three octaves, for the dot-only section -----
// Short by design: barely stretched, no vibrato, small overshoot. These
// are percussion that happens to be made of people.
for (const [tag, src, floor, pitches] of [
  ["c", "dot-camille", 110, ["B3", "C#4", "D4", "E4", "F#4", "G4", "A4"]],
  ["a", "dot-alex", 90, ["F#3", "G3", "A3", "B3", "D4", "E4"]],
  ["j", "dot-jeffrey", 55, ["B2", "D3", "E3", "F#3", "G3", "A3"]],
])
  for (const n of pitches)
    sing(`dot-${tag}-${n.replace("#", "s").toLowerCase()}`, src, `${n}:0.20`,
      { vib: 0, over: 20, floor });

// ── run ────────────────────────────────────────────────────────────────
const manifestPath = resolve(SUNG, ".manifest.json");
const manifest = existsSync(manifestPath) && !FORCE
  ? JSON.parse(readFileSync(manifestPath, "utf8")) : {};

console.log(`→ speech-to-singing · ${BANK.length} renders · WORLD (pyworld) via bin/sing.py`);
let made = 0, cached = 0;
const report = {};

for (const { name, src, notes, opts } of BANK) {
  const inWav = resolve(SAMPLES, `${src}.wav`);
  if (!existsSync(inWav)) { console.warn(`  ! missing ${inWav} — run bin/slice.mjs`); continue; }
  const outWav = resolve(SUNG, `${name}.wav`);

  const args = [
    "-W", "ignore", SCRIPT, inWav, outWav, "--notes", notes,
    "--f0-floor", String(opts.floor ?? 90),
    "--vibrato-cents", String(opts.vib ?? 32),
    "--overshoot-cents", String(opts.over ?? 42),
    "--formant-db", String(opts.formant ?? 3.2),
    "--verify",
  ];
  const key = createHash("sha256")
    .update(args.slice(2).join(" ") + readFileSync(inWav).length)
    .digest("hex").slice(0, 16);

  if (!FORCE && manifest[name]?.key === key && existsSync(outWav)) {
    cached++; report[name] = manifest[name];
    continue;
  }
  const out = execFileSync(PY, args, { encoding: "utf8" }).trim();
  console.log(`  ${name.padEnd(24)} ${out.replace(/^\s*sing · /, "")}`);
  report[name] = { key, src, notes, line: out.trim() };
  made++;
}

writeFileSync(manifestPath, JSON.stringify(report, null, 2));
console.log(`✓ sung bank → ${SUNG}  (${made} rendered, ${cached} cached)`);
