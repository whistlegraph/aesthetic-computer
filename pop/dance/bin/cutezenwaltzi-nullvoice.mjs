#!/usr/bin/env node
// cutezenwaltzi-nullvoice.mjs — carve the cutezenwaltzi melody out of
// cancelled noise (the nullabye technique, pop/nullabye/c/nullnoise.c).
//
// Reads the trance.mjs struct.json (per-note events), re-voices the lead
// as a breathy Q-55 noise-whistle an octave up + soft Q-70 sparkles on
// the bells, and bakes a nullnoise score. Render the stem with:
//
//   node pop/dance/bin/cutezenwaltzi-nullvoice.mjs
//   node pop/nullabye/c/run-c.mjs pop/dance/out/cutezenwaltzi-nullvoice.score.txt \
//     --out pop/dance/out/cutezenwaltzi-nullvoice.mp3 --master lullaby
//
// Lanes are monophonic — each event's cut = the next event's t0, and
// events must be time-sorted or the mono-cut silences the earlier note.
import { readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
const structPath =
  process.argv[2] || resolve(OUT, "cutezenwaltzi-bed.assets", "struct.json");
const struct = JSON.parse(readFileSync(structPath, "utf8"));

const midiHz = (m) => 440 * Math.pow(2, (m - 69) / 12);

// One band per voice; events sorted, mono-cut chained.
function bakeBand(events, { octave = 12, peakDb, q, atk, rel, minGap = 0.02 }) {
  const sorted = [...events].sort((a, b) => a.t - b.t);
  const lines = [];
  for (let i = 0; i < sorted.length; i++) {
    const e = sorted[i];
    const next = sorted[i + 1];
    if (next && next.t - e.t < minGap) continue; // drop grace-note pileups
    const freq = midiHz(e.midi + octave);
    const dur = Math.max(e.dur ?? 0.3, 0.12);
    const hold = Math.max(dur - atk - rel, 0.04);
    const cut = next ? next.t : -1;
    lines.push(
      `${e.t} ${freq.toFixed(2)} 0 ${peakDb} ${q} ${atk} ${hold.toFixed(3)} ${rel} ${cut}`,
    );
  }
  return lines;
}

// The whistler — the lead melody an octave up, breathy and soft.
const whistle = bakeBand(struct.events.lead ?? [], {
  octave: 12,
  peakDb: 26,
  q: 55,
  atk: 0.06,
  rel: 0.4,
});

// Sparkles — the bells echoed two octaves up, tighter Q, quieter.
const sparkle = bakeBand(struct.events.bells ?? [], {
  octave: 24,
  peakDb: 19,
  q: 70,
  atk: 0.015,
  rel: 0.22,
});

const L = [
  "sr 48000",
  `dur ${struct.totalSec}`,
  "detune 1.0012 0.9988",
  "seed 48879 12648430",
  "normpeak 0.88",
  "fadein 0.004",
  "fadeout 2.5",
];
if (whistle.length) L.push(`band ${whistle.length}`, ...whistle);
if (sparkle.length) L.push(`band ${sparkle.length}`, ...sparkle);

const scorePath = resolve(OUT, "cutezenwaltzi-nullvoice.score.txt");
writeFileSync(scorePath, L.join("\n") + "\n");
console.log(
  `✓ baked whistle ${whistle.length} + sparkle ${sparkle.length} events · ${struct.totalSec.toFixed(1)}s → ${scorePath}`,
);
