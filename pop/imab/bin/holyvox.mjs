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
const doc = JSON.parse(readFileSync(`${DL}/whistlegraph-${TAKE}.syllnote.json`, "utf8"));
const TMPL = "i'm a butterfly flapping for you guys just a costume i put on in my room".split(" ");
const NSYL = { butterfly: 3, flapping: 2, costume: 2 };
// GROUND TRUTH pitch classes (@jeffrey); null = snap the take's own note to C major
// "^" = take the octave ABOVE the previous target (the high G — @jeffrey)
const GT = { 0: ["C"], 1: ["G^"], 2: ["C", "C", "C"], 3: ["C", "C^"], 4: null, 5: null, 6: null,
             7: ["B"], 8: ["Av"], 9: ["Gv", "Fv"], 10: ["Ev"], 11: ["Ev"], 12: ["Dv"],
             13: ["Dv"], 14: ["Cv"], 15: ["Cv"] };
// "v" = at-or-below the previous target: the back half walks DOWN to low C
const CMAJ = [0, 2, 4, 5, 7, 9, 11];
const NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const nname = (m) => NAMES[((m % 12) + 12) % 12] + (Math.floor(m / 12) - 1);
const norm = (w) => w.toLowerCase().replace(/[^a-z']/g, "");
const fuzzy = (a, b) => a === b || (a.length > 3 && b.length > 3 && (a.startsWith(b.slice(0, 4)) || b.startsWith(a.slice(0, 4))));
const seq = [];
let ti = 0;
for (const w of doc.words) {
  if (ti < TMPL.length && fuzzy(TMPL[ti], norm(w.text))) { seq.push({ ti, w }); ti++; }
}
const onsets = seq.map((s) => s.w.nuclei[0]?.startSec ?? s.w.fromMs / 1000);
const iois = onsets.slice(1).map((t, i) => t - onsets[i]).filter((d) => d > 0.08).sort((a, b) => a - b);
const ioi = iois[Math.floor(iois.length / 2)];
const ratio = (2 * BEAT) / ioi;               // his beat becomes two floor beats
const t0 = Math.max(0, seq[0].w.fromMs / 1000 - 0.15);
const t1 = seq[seq.length - 1].w.toMs / 1000 + 0.8;
console.log(`take ${TAKE}: ${ti}/16 · half-time stretch ${ratio.toFixed(3)} (phrase ${((t1 - t0) * ratio).toFixed(1)}s)`);
const trim = `${WORK}/holy-trim.wav`, str = `${WORK}/holy-str.wav`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-ss", t0.toFixed(3), "-t", (t1 - t0).toFixed(3),
  "-i", SEP, "-ac", "1", "-ar", String(SR), "-af", "highpass=f=80", trim]);
sh("rubberband", ["-t", ratio.toFixed(4), "-F", "-c", "6", trim, str]);

// ── note targets: his classes, performance octaves, vowel-onset anchors
const noteNames = [], noteStarts = [], targets = [];
for (const { ti: tidx, w } of seq) {
  const need = NSYL[TMPL[tidx]] ?? 1;
  let ns = [...w.nuclei].sort((a, b) => a.startSec - b.startSec);
  if (ns.length > need)
    ns = ns.sort((a, b) => b.rms * b.durSec - a.rms * a.durSec).slice(0, need).sort((a, b) => a.startSec - b.startSec);
  for (let k = 0; k < Math.min(need, ns.length); k++) {
    const det = ns[k].midi;
    const gtRaw = GT[tidx] ? GT[tidx][Math.min(k, GT[tidx].length - 1)] : null;
    const up = gtRaw?.endsWith("^");
    const dn = gtRaw?.endsWith("v");
    const cls = gtRaw ? NAMES.indexOf(gtRaw.replace(/[\^v]/, ""))
      : CMAJ.reduce((best, c) => {
          const d = Math.min(...[-12, 0, 12].map((o) => Math.abs(det % 12 - c + o)));
          const bd = Math.min(...[-12, 0, 12].map((o) => Math.abs(det % 12 - best + o)));
          return d < bd ? c : best;
        }, 0);
    let midi = Math.round(det / 12) * 12 + cls;   // nearest octave of the class
    for (const cand of [midi - 12, midi, midi + 12])
      if (Math.abs(cand - det) < Math.abs(midi - det)) midi = cand;
    const prevT = targets.length ? NAMES.indexOf(targets[targets.length - 1].note.replace(/-?\d+$/, "")) +
      12 * (parseInt(targets[targets.length - 1].note.match(/-?\d+$/)[0]) + 1) : null;
    if (up && prevT !== null) while (midi <= prevT) midi += 12;
    if (dn && prevT !== null) while (midi > prevT) midi -= 12;
    const at = (ns[k].startSec - t0) * ratio;
    noteNames.push(nname(midi));
    noteStarts.push(at.toFixed(3));
    targets.push({ label: TMPL[tidx] + (need > 1 ? `·${k + 1}` : ""), t: +at.toFixed(3),
                   dur: +(ns[k].durSec * ratio).toFixed(3), note: nname(midi) });
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
for (let b = 8 * 4; b < BARS * 4; b++) {
  const at = Math.floor(b * BEAT * SR);
  for (let j = 0; j < kn && at + j < NT; j++) bed[at + j] *= 1;   // (sines ducked below)
  for (let j = 0; j < Math.floor(0.5 * SR) && at + j < NT; j++)
    duck[at + j] *= 1 - 0.35 * Math.exp(-j / (0.09 * SR));
}
for (let i = 0; i < NT; i++) bed[i] *= duck[i];
for (let b = 8 * 4; b < (BARS - 1) * 4; b++) {
  const at = Math.floor(b * BEAT * SR);
  const g = 0.5 * (b % 4 === 0 ? 1.05 : 1.0);
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
