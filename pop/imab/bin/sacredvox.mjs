#!/usr/bin/env node
// sacredvox.mjs — the loner law applied to imab: the vocal IS the
// original recording. The chosen take's voice is demucs-separated,
// kept as ONE continuous phrase — real timing, real pitch, no WORLD,
// no slicing, no snapping — tempo-fit to 124 with a single
// formant-preserving stretch, and laid over the bed TRANSPOSED TO HIS
// KEY (C: he sings C G C C C — "i'm a BUT-ter-fly").
//
//   node pop/imab/bin/sacredvox.mjs [--take <id>]
//   → out/imab-sacredvox.wav + out/imab-vox-demo6.mp3
//   (bed: out/imab-accomp-124-x3.wav — gen-accomp.mjs --transpose 3)

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
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"], ...opts });
const SR = 48_000, BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;

const WAV = `${DL}/whistlegraph-${TAKE}.wav`;
const SYLJ = `${DL}/whistlegraph-${TAKE}.syllnote.json`;
const doc = JSON.parse(readFileSync(SYLJ, "utf8"));

// ── separate the voice (cached) ───────────────────────────────────────
const SEP = `${WORK}/sep/htdemucs/whistlegraph-${TAKE}/vocals.wav`;
if (!existsSync(SEP)) {
  console.log("→ demucs (slow)");
  const r = sh("demucs", ["-n", "htdemucs", "--two-stems=vocals", "-o", `${WORK}/sep`, WAV]);
  if (r.status !== 0 || !existsSync(SEP)) { console.error("✗ demucs failed"); process.exit(1); }
}

// ── the sung phrase window + tempo, from whisper timing ───────────────
const TMPL = "i'm a butterfly flapping for you guys just a costume i put on in my room".split(" ");
const norm = (w) => w.toLowerCase().replace(/[^a-z']/g, "");
const fuzzy = (a, b) => a === b || (a.length > 3 && b.length > 3 && (a.startsWith(b.slice(0, 4)) || b.startsWith(a.slice(0, 4))));
const seq = [];
let ti = 0;
for (const w of doc.words) {
  if (ti < TMPL.length && fuzzy(TMPL[ti], norm(w.text))) { seq.push(w); ti++; }
}
if (ti < 14) { console.error(`✗ matched ${ti}/16`); process.exit(1); }
const onsets = seq.map((w) => w.nuclei[0]?.startSec ?? w.fromMs / 1000);
const iois = onsets.slice(1).map((t, i) => t - onsets[i]).filter((d) => d > 0.08).sort((a, b) => a - b);
const ioi = iois[Math.floor(iois.length / 2)];
const ratio = BEAT / ioi;                       // one global tempo fit, nothing else
const t0 = Math.max(0, seq[0].fromMs / 1000 - 0.15);
const t1 = seq[seq.length - 1].toMs / 1000 + 0.6;
console.log(`take ${TAKE}: phrase ${t0.toFixed(2)}–${t1.toFixed(2)}s · ${(60 / ioi).toFixed(1)} BPM → ${BPM} (stretch ${ratio.toFixed(3)})`);

const trim = `${WORK}/phrase.wav`, str = `${WORK}/phrase-124.wav`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-ss", t0.toFixed(3), "-t", (t1 - t0).toFixed(3),
  "-i", SEP, "-ac", "1", "-ar", String(SR), "-af", "highpass=f=80", trim]);
sh("rubberband", ["-t", ratio.toFixed(4), "-F", "-c", "6", trim, str]);
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", str, "-c:a", "pcm_s16le", `${OUT}/imab-sacredvox.wav`]);
console.log(`✓ ${OUT}/imab-sacredvox.wav`);

// ── demo over the C-tonic bed, phrase at cycles 2–4 ───────────────────
const readF32 = (wav) => {
  const raw = `${WORK}/.r.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav, "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
};
const BED = `${OUT}/imab-accomp-124-x3.wav`;
if (!existsSync(BED)) { console.error("✗ render the bed: node pop/imab/bin/gen-accomp.mjs --transpose 3 --lead 0.18"); process.exit(1); }
const acc = readF32(BED), vox = readF32(str);
const rms = (a) => { let s = 0, n = 0; for (let i = 0; i < a.length; i++) if (Math.abs(a[i]) > 1e-4) { s += a[i] * a[i]; n++; } return Math.sqrt(s / Math.max(1, n)); };
const vg = Math.min(6, (rms(acc) * 2.0) / Math.max(1e-9, rms(vox)));
const mix = Float32Array.from(acc);
for (let cyc = 1; cyc < 4; cyc++) {
  const off = Math.floor((cyc * 8 * BAR + 0.1) * SR);
  for (let j = 0; j < vox.length; j++) { const d = off + j; if (d < mix.length) mix[d] += vox[j] * vg; }
}
let pk = 0; for (let i = 0; i < mix.length; i++) pk = Math.max(pk, Math.abs(mix[i]));
if (pk > 0.9) for (let i = 0; i < mix.length; i++) mix[i] *= 0.9 / pk;
const stb = new Float32Array(mix.length * 2);
for (let i = 0; i < mix.length; i++) { stb[2 * i] = mix[i]; stb[2 * i + 1] = mix[i]; }
writeFileSync(`${WORK}/.demo.f32`, Buffer.from(stb.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", `${WORK}/.demo.f32`,
  "-metadata", "title=imab-vox-demo6", "-metadata", "artist=Whistlegraph Dot Org",
  "-c:a", "libmp3lame", "-q:a", "2", `${OUT}/imab-vox-demo6.mp3`]);
console.log(`✓ ${OUT}/imab-vox-demo6.mp3 (vox gain ${vg.toFixed(2)})`);
