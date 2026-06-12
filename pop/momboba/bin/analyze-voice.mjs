#!/usr/bin/env node
// analyze-voice.mjs — pitch-analyze jeffrey's wave-wizard vowel takes so the
// renderer can use them as a sampled harmony voice.
//
// Reads:  wave-wizard/samples/momabobasheep/takes/*.wav  (48 kHz mono PCM)
// Writes: pop/momboba/voice/manifest.json
//         [{ file, vowel, midi, hz, clarity, durSec }]
//
// Pitch via pop/lib/analysis.mjs pitchTrack (normalized autocorrelation):
// the take's pitch is the CLARITY-WEIGHTED MEDIAN of its voiced frames —
// robust against the onset scoop and the breathy tail.
//
// Run:  node pop/momboba/bin/analyze-voice.mjs

import { writeFileSync, readdirSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { pitchTrack } from "../../lib/analysis.mjs";
import { readWavMono } from "../../lib/wav.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const TAKES = resolve(REPO, "wave-wizard/samples/momabobasheep/takes");
const OUT_DIR = resolve(HERE, "../voice");
mkdirSync(OUT_DIR, { recursive: true });

const VOWELS = ["oo", "oh", "ah", "eh", "mm"]; // spec.json take order

if (!existsSync(TAKES)) {
  console.error(`✗ no takes yet — record first: wave-wizard samples/momabobasheep/spec.json`);
  process.exit(1);
}
const files = readdirSync(TAKES).filter((f) => f.endsWith(".wav")).sort();
if (!files.length) { console.error(`✗ ${TAKES} is empty`); process.exit(1); }

const rows = [];
for (const f of files) {
  const path = resolve(TAKES, f);
  const { samples, sampleRate } = readWavMono(path);
  const frames = pitchTrack(samples, { sampleRate, fmin: 60, fmax: 500 });
  const voiced = frames.filter((r) => r.hz != null && r.clarity > 0.6);
  if (voiced.length < 5) { console.warn(`  ⚠ ${f}: too little voiced material, skipping`); continue; }
  // clarity-weighted median of midi values
  const sorted = voiced.map((r) => ({ m: r.midi, w: r.clarity })).sort((a, b) => a.m - b.m);
  const totW = sorted.reduce((s, r) => s + r.w, 0);
  let acc = 0, midi = sorted[0].m;
  for (const r of sorted) { acc += r.w; if (acc >= totW / 2) { midi = r.m; break; } }
  const idx = files.indexOf(f);
  rows.push({
    file: path.replace(REPO + "/", ""),
    vowel: VOWELS[idx] ?? `take${idx}`,
    midi: Math.round(midi * 100) / 100,
    hz: Math.round(440 * Math.pow(2, (midi - 69) / 12) * 10) / 10,
    clarity: Math.round((voiced.reduce((s, r) => s + r.clarity, 0) / voiced.length) * 100) / 100,
    durSec: Math.round((samples.length / sampleRate) * 100) / 100,
  });
  console.log(`  ${VOWELS[idx] ?? f} · ${rows.at(-1).hz} Hz (midi ${rows.at(-1).midi}) · clarity ${rows.at(-1).clarity} · ${rows.at(-1).durSec}s`);
}

const out = resolve(OUT_DIR, "manifest.json");
writeFileSync(out, JSON.stringify(rows, null, 2) + "\n");
console.log(`✓ ${rows.length} takes → ${out.replace(REPO + "/", "")}`);
