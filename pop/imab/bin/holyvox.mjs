#!/usr/bin/env node
// holyvox.mjs — imab, angelic: the sacred phrase stretched to HALF-TIME
// over a 124 kick, note-locked to jeffrey's written melody (C G C C C ·
// B A G F · E E D C C C — pitch class his, octave from the performance,
// note changes anchored on vowel onsets), floating on a pure-sine choir
// bed with a church halo. Every render is VERIFIED by notecheck.py.
//
//   node pop/imab/bin/holyvox.mjs [--take <id>]
//   → out/imab-holyvox.wav + out/imab-vox-demo8.mp3 (+ notecheck table)

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out");
const DL = `${REPO}/toolchain/whistlegraph/downloads`;
const WORK = `${process.env.HOME}/.cache/ac/imab`;
mkdirSync(WORK, { recursive: true });
const argv = process.argv.slice(2);
const flag = (n, d) => { const i = argv.indexOf(`--${n}`); return i >= 0 && argv[i + 1] ? argv[i + 1] : d; };
const TAKE = flag("take", "7311159624588070175");
const PY = `${REPO}/pop/.venv/bin/python`;
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"], ...opts });
const SR = 48_000, BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;
const CYCLE = 16 * BAR;                       // half-time vocal needs room

// ── the sacred phrase, stretched to half-time (his beat = 2 floor beats)
const SEP = `${WORK}/sep/htdemucs/whistlegraph-${TAKE}/vocals.wav`;
if (!existsSync(SEP)) {
  console.log("→ demucs (slow)");
  sh("demucs", ["-n", "htdemucs", "--two-stems=vocals", "-o", `${WORK}/sep`, `${DL}/whistlegraph-${TAKE}.wav`]);
}
// boundaries: energy-valley refined (boundfix.py), REQUIRED
const BOUNDS = JSON.parse(readFileSync(`${WORK}/bounds-${TAKE}.json`, "utf8")).words;
const doc = JSON.parse(readFileSync(`${WORK}/stem-${TAKE}.syllnote.json`, "utf8"));
const NSYL = { butterfly: 3, flapping: 2, costume: 2 };
// the melody is fully @jeffrey's — absolute pitches, no detection involved
// notepat truth (@jeffrey): c g c c c · c h c c c · g f e d e e d c c c
const GT = ["C4", "G4", "C4,C4,C4", "C4,C5", "C4", "C4", "C4",
            "G4", "F4", "E4,D4", "E4", "E4", "D4", "C4", "C4", "C4"];
// musical minimum lengths, in beats (the held high-G "a" was too short)
const MINB = { 1: 1.0, 4: 0.5, 5: 0.5, 6: 0.75 };
const NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const toMidi = (n) => { const m = n.match(/^([A-G]#?)(-?\d)$/); return (Number(m[2]) + 1) * 12 + NAMES.indexOf(m[1]); };
const nname = (m) => NAMES[((m % 12) + 12) % 12] + (Math.floor(m / 12) - 1);

const iois = BOUNDS.slice(1).map((w, i) => (w.fromMs - BOUNDS[i].fromMs) / 1000).filter((d) => d > 0.08).sort((x, y) => x - y);
const ioi = iois[Math.floor(iois.length / 2)];
const ratio = (2 * BEAT) / ioi;               // his beat becomes two floor beats
const t0 = BOUNDS[0].fromMs / 1000;
const t1 = BOUNDS[BOUNDS.length - 1].toMs / 1000 + 0.25;
console.log(`take ${TAKE}: half-time base stretch ${ratio.toFixed(3)}`);

// ── variable time map: per-word stretch, held notes get their beats ───
const trim = `${WORK}/holy-trim.wav`, str = `${WORK}/holy-str.wav`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-ss", t0.toFixed(3), "-t", (t1 - t0).toFixed(3),
  "-i", SEP, "-ac", "1", "-ar", String(SR), "-af", "highpass=f=80", trim]);
let outCursor = 0;
const mapLines = [], wordsOut = [];
for (let i = 0; i < BOUNDS.length; i++) {
  const w = BOUNDS[i];
  const inStart = w.fromMs / 1000 - t0;
  const srcDur = (w.toMs - w.fromMs) / 1000;
  const outDur = Math.max(srcDur * ratio, (MINB[i] ?? 0.4) * BEAT);
  mapLines.push(`${Math.round(inStart * SR)} ${Math.round(outCursor * SR)}`);
  wordsOut.push({ i, text: w.text, outStart: outCursor, outDur });
  outCursor += outDur;
}
mapLines.push(`${Math.round((t1 - t0) * SR)} ${Math.round(outCursor * SR)}`);
writeFileSync(`${WORK}/holy-map.txt`, mapLines.join("\n") + "\n");
sh("rubberband", ["-M", `${WORK}/holy-map.txt`, "-F", "-c", "6", trim, str]);

// ── targets: absolute melody on the mapped timeline; note changes land
// on the REAL vowel onsets inside each word (the PING flare starts where
// the sung vowel actually starts, not at an even split)
const noteNames = [], noteStarts = [], targets = [];
const sylByIdx = {};
{
  let ti2 = 0;
  for (const w of doc.words)
    if (ti2 < TMPL.length && fuzzy(TMPL[ti2], norm(w.text))) sylByIdx[ti2++] = w;
}
for (const wo of wordsOut) {
  const notes = GT[wo.i].split(",");
  const b = BOUNDS[wo.i];
  const srcDur = (b.toMs - b.fromMs) / 1000;
  const toOut = (srcSec) => wo.outStart + Math.max(0, Math.min(1, (srcSec - b.fromMs / 1000) / srcDur)) * wo.outDur;
  const nucs = (sylByIdx[wo.i]?.nuclei ?? [])
    .filter((n) => n.startSec >= b.fromMs / 1000 - 0.05 && n.startSec <= b.toMs / 1000)
    .sort((x, y) => x.startSec - y.startSec);
  let ats;
  if (wo.i === 3)                      // flap-PING: the flare owns the word —
    ats = [wo.outStart + 0.03,         // "flap" is one floor beat, C5 after
           wo.outStart + Math.min(BEAT, wo.outDur * 0.3)];
  else if (notes.length === 1) ats = [wo.outStart + 0.03];
  else if (nucs.length >= notes.length)
    ats = nucs.slice(0, notes.length).map((n) => toOut(n.startSec));
  else if (nucs.length === notes.length - 1)
    ats = [wo.outStart + 0.03, ...nucs.map((n) => toOut(n.startSec))];
  else ats = notes.map((_, k) => wo.outStart + (k * wo.outDur) / notes.length + (k === 0 ? 0.03 : 0));
  for (let k = 0; k < notes.length; k++) {
    const midi = toMidi(notes[k]);
    const end = k + 1 < notes.length ? ats[k + 1] : wo.outStart + wo.outDur;
    noteNames.push(nname(midi));
    noteStarts.push(ats[k].toFixed(3));
    targets.push({ label: wo.text + (notes.length > 1 ? `·${k + 1}` : ""), t: +ats[k].toFixed(3),
                   dur: +Math.max(0.1, end - ats[k]).toFixed(3), note: nname(midi) });
  }
}
console.log(`→ WORLD snap: ${noteNames.join(" ")}`);
const tuned = `${OUT}/imab-holyvox.wav`;
const r = spawnSync(PY, [`${REPO}/pop/bin/pitchsnap_world.py`, str, tuned,
  "--notes", noteNames.join(","), "--note-starts", noteStarts.join(","),
  "--retain", "1.0", "--xfade-ms", "60", "--voicing-ramp-ms", "40",
  "--vibrato-hz", "5.0", "--vibrato-cents", "12",
], { stdio: ["ignore", "inherit", "inherit"] });
if (r.status !== 0) { console.error("✗ WORLD failed"); process.exit(1); }
writeFileSync(`${WORK}/holy-targets.json`, JSON.stringify(targets, null, 1));
console.log("→ notecheck");
sh(PY, [`${HERE}/notecheck.py`, tuned, `${WORK}/holy-targets.json`], { stdio: ["ignore", "inherit", "inherit"] });

// ── the holy bed: pure sines breathing under a soft kick ──────────────
const BARS = 64, NT = Math.ceil((BARS * BAR + 3) * SR);
const bed = new Float32Array(NT);
const CH = { C: [36, 48, 55, 64, 72], F: [41, 53, 57, 60, 69], G: [43, 55, 59, 62, 67] };
const MAP16 = ["C", "C", "C", "C", "C", "F", "F", "G", "C", "C", "C", "C", "C", "C", "C", "C"];
for (let bar = 0; bar < BARS; bar++) {
  const chord = CH[MAP16[bar % 16]];
  const a = Math.floor(bar * BAR * SR), n = Math.floor((BAR + 0.6) * SR);
  for (let vi = 0; vi < chord.length; vi++) {
    const f = 440 * 2 ** ((chord[vi] - 69) / 12);
    const g = 0.045 * (1 - vi * 0.12);
    const lfo = 0.11 + 0.02 * vi;
    for (let j = 0; j < n && a + j < NT; j++) {
      const t = j / SR;
      const env = Math.min(t / 0.4, 1) * Math.min((n / SR - t) / 0.55, 1);
      bed[a + j] += Math.sin(2 * Math.PI * f * t) * g * env * (0.8 + 0.2 * Math.sin(2 * Math.PI * lfo * (bar * BAR + t)));
    }
  }
}
// kick5, entering after half the intro cycle, gently
const kn = Math.floor(0.4 * SR), K = new Float32Array(kn);
{
  let ph = 0, acc = 0;
  const aa = 1 - Math.exp(-2 * Math.PI * 2200 / SR);
  for (let j = 0; j < kn; j++) {
    const t = j / SR;
    ph += 2 * Math.PI * (40 + 80 * Math.exp(-t / 0.034)) / SR;
    const raw = Math.tanh(2.0 * (Math.sin(ph) * Math.exp(-t / 0.17) + Math.sin(2 * ph) * Math.exp(-t / 0.05) * 0.22 + Math.sin(2 * Math.PI * 150 * t) * Math.exp(-t / 0.024) * 0.30));
    acc += aa * (raw - acc); K[j] = acc;
  }
}
const duck = new Float32Array(NT).fill(1);
for (let b = 0; b < BARS * 4; b++) {
  const at = Math.floor(b * BEAT * SR);
  for (let j = 0; j < kn && at + j < NT; j++) bed[at + j] *= 1;   // (sines ducked below)
  for (let j = 0; j < Math.floor(0.5 * SR) && at + j < NT; j++)
    duck[at + j] *= 1 - 0.35 * Math.exp(-j / (0.09 * SR));
}
for (let i = 0; i < NT; i++) bed[i] *= duck[i];
for (let b = 0; b < (BARS - 1) * 4; b++) {
  const at = Math.floor(b * BEAT * SR);
  const g = 0.62 * (b % 4 === 0 ? 1.05 : 1.0);
  for (let j = 0; j < kn && at + j < NT; j++) bed[at + j] += K[j] * g;
}

// ── halo the vocal, place at cycles 2–4, master lightly ───────────────
sh(PY, [`${REPO}/spinging/lib/vocal_bus.py`, "reverb", tuned, `${WORK}/holy-halo.wav`, "-14", "1.6"]);
const readF32 = (wav) => {
  const raw = `${WORK}/.r.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav, "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
};
const vox = readF32(`${WORK}/holy-halo.wav`);
const rms = (a) => { let s = 0, n = 0; for (let i = 0; i < a.length; i++) if (Math.abs(a[i]) > 1e-4) { s += a[i] * a[i]; n++; } return Math.sqrt(s / Math.max(1, n)); };
const vg = Math.min(8, (rms(bed) * 2.2) / Math.max(1e-9, rms(vox)));
for (let cyc = 1; cyc < 4; cyc++) {
  const off = Math.floor((cyc * CYCLE + 0.1) * SR);
  for (let j = 0; j < vox.length; j++) { const d = off + j; if (d < NT) bed[d] += vox[j] * vg; }
}
const fadeN = Math.floor(2.5 * SR);
for (let i = 0; i < fadeN; i++) bed[NT - 1 - i] *= i / fadeN;
let pk = 0; for (let i = 0; i < NT; i++) pk = Math.max(pk, Math.abs(bed[i]));
if (pk > 0.9) for (let i = 0; i < NT; i++) bed[i] *= 0.9 / pk;
const stb = new Float32Array(NT * 2);
for (let i = 0; i < NT; i++) { stb[2 * i] = bed[i]; stb[2 * i + 1] = bed[i]; }
writeFileSync(`${WORK}/.holy.f32`, Buffer.from(stb.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", `${WORK}/.holy.f32`,
  "-metadata", "title=imab-vox-demo8", "-metadata", "artist=Whistlegraph Dot Org",
  "-c:a", "libmp3lame", "-q:a", "2", `${OUT}/imab-vox-demo8.mp3`]);
console.log(`✓ ${OUT}/imab-vox-demo8.mp3 (vox gain ${vg.toFixed(2)})`);
