#!/usr/bin/env node
// coolvox.mjs — sacredvox + the aesthetivox sheen + the video.
//
// The confirmed base (one continuous demucs phrase, single tempo-fit
// stretch, no slicing) gets the loner-proven autotune on top:
// pop/bin/autotune.py in NOTE mode, C major, strength 0.92 with scoop/
// vibrato preservation — pitch correction rides the voiced vowels and
// leaves consonants alone, so the placement stays natural. Then the
// take's own TikTok footage is retimed by the same stretch ratio and
// cut to the demo: bed-only intro on a held frame, three sung passes.
//
//   node pop/imab/bin/coolvox.mjs [--take <id>] [--strength 0.92]
//   → out/imab-coolvox-<take>.wav, out/imab-vox-demo7-<take>.mp3,
//     out/imab-vox-demo7-<take>.mp4

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
const STRENGTH = flag("strength", "0.92");
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"], ...opts });
const SR = 48_000, BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;
const CYCLE = 8 * BAR;

const WAV = `${DL}/whistlegraph-${TAKE}.wav`;
const MP4 = `${DL}/whistlegraph-${TAKE}.mp4`;
const SYLJ = `${DL}/whistlegraph-${TAKE}.syllnote.json`;
if (!existsSync(SYLJ)) { console.error(`✗ no syllnote for ${TAKE}`); process.exit(1); }
const doc = JSON.parse(readFileSync(SYLJ, "utf8"));

// ── separate, trim, stretch (the sacred base) ─────────────────────────
const SEP = `${WORK}/sep/htdemucs/whistlegraph-${TAKE}/vocals.wav`;
if (!existsSync(SEP)) {
  console.log("→ demucs (slow)");
  const r = sh("demucs", ["-n", "htdemucs", "--two-stems=vocals", "-o", `${WORK}/sep`, WAV]);
  if (r.status !== 0 || !existsSync(SEP)) { console.error("✗ demucs failed"); process.exit(1); }
}
const TMPL = "i'm a butterfly flapping for you guys just a costume i put on in my room".split(" ");
const norm = (w) => w.toLowerCase().replace(/[^a-z']/g, "");
const fuzzy = (a, b) => a === b || (a.length > 3 && b.length > 3 && (a.startsWith(b.slice(0, 4)) || b.startsWith(a.slice(0, 4))));
const seq = [];
let ti = 0;
for (const w of doc.words) {
  if (ti < TMPL.length && fuzzy(TMPL[ti], norm(w.text))) { seq.push(w); ti++; }
}
if (ti < 13) { console.error(`✗ matched ${ti}/16 — pick another take`); process.exit(1); }
const onsets = seq.map((w) => w.nuclei[0]?.startSec ?? w.fromMs / 1000);
const iois = onsets.slice(1).map((t, i) => t - onsets[i]).filter((d) => d > 0.08).sort((a, b) => a - b);
const ioi = iois[Math.floor(iois.length / 2)];
const ratio = BEAT / ioi;
const t0 = Math.max(0, seq[0].fromMs / 1000 - 0.15);
const t1 = seq[seq.length - 1].toMs / 1000 + 0.6;
console.log(`take ${TAKE}: ${ti}/16 words · phrase ${t0.toFixed(2)}–${t1.toFixed(2)}s · stretch ${ratio.toFixed(3)}`);
const trim = `${WORK}/ph-${TAKE}.wav`, str = `${WORK}/ph124-${TAKE}.wav`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-ss", t0.toFixed(3), "-t", (t1 - t0).toFixed(3),
  "-i", SEP, "-ac", "1", "-ar", String(SR), "-af", "highpass=f=80", trim]);
sh("rubberband", ["-t", ratio.toFixed(4), "-F", "-c", "6", trim, str]);

// ── the sheen: aesthetivox autotune, C major, scoops preserved ────────
console.log(`→ autotune C major · strength ${STRENGTH}`);
const tuned = `${OUT}/imab-coolvox-${TAKE}.wav`;
const r = spawnSync(`${REPO}/pop/.venv/bin/python`, [
  `${REPO}/pop/bin/autotune.py`, str, tuned,
  "--key", "C", "--scale", "major", "--mode", "note",
  "--strength", STRENGTH, "--preserve", "0.6", "--glide-ms", "35",
], { stdio: ["ignore", "inherit", "inherit"] });
if (r.status !== 0) { console.error("✗ autotune failed"); process.exit(1); }
console.log(`✓ ${tuned}`);

// ── demo mix: bed-only cycle, then three sung passes ──────────────────
const readF32 = (wav) => {
  const raw = `${WORK}/.r.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav, "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
};
const BED = `${OUT}/imab-accomp-124-x3.wav`;
const acc = readF32(BED), vox = readF32(tuned);
const rms = (a) => { let s = 0, n = 0; for (let i = 0; i < a.length; i++) if (Math.abs(a[i]) > 1e-4) { s += a[i] * a[i]; n++; } return Math.sqrt(s / Math.max(1, n)); };
const vg = Math.min(8, (rms(acc) * 2.1) / Math.max(1e-9, rms(vox)));
const mix = Float32Array.from(acc);
for (let cyc = 1; cyc < 4; cyc++) {
  const off = Math.floor((cyc * CYCLE + 0.1) * SR);
  for (let j = 0; j < vox.length; j++) { const d = off + j; if (d < mix.length) mix[d] += vox[j] * vg; }
}
let pk = 0; for (let i = 0; i < mix.length; i++) pk = Math.max(pk, Math.abs(mix[i]));
if (pk > 0.9) for (let i = 0; i < mix.length; i++) mix[i] *= 0.9 / pk;
const stb = new Float32Array(mix.length * 2);
for (let i = 0; i < mix.length; i++) { stb[2 * i] = mix[i]; stb[2 * i + 1] = mix[i]; }
writeFileSync(`${WORK}/.demo.f32`, Buffer.from(stb.buffer));
const demoMp3 = `${OUT}/imab-vox-demo7-${TAKE}.mp3`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", `${WORK}/.demo.f32`,
  "-metadata", `title=imab-vox-demo7-${TAKE.slice(-6)}`, "-metadata", "artist=Whistlegraph Dot Org",
  "-c:a", "libmp3lame", "-q:a", "2", demoMp3]);
console.log(`✓ ${demoMp3} (vox gain ${vg.toFixed(2)})`);

// ── the mp4: held-frame intro + three retimed sung passes ─────────────
if (existsSync(MP4)) {
  const seg = `${WORK}/seg-${TAKE}.mp4`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
    "-ss", t0.toFixed(3), "-t", (t1 - t0).toFixed(3), "-i", MP4,
    "-vf", `setpts=${ratio.toFixed(4)}*PTS,scale=1080:1920:force_original_aspect_ratio=increase,crop=1080:1920,fps=30`,
    "-an", "-c:v", "libx264", "-preset", "fast", "-crf", "20", seg]);
  const segDur = (t1 - t0) * ratio;
  const intro = `${WORK}/intro-${TAKE}.mp4`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", seg,
    "-vf", `select=eq(n\\,0),tpad=stop_mode=clone:stop_duration=${CYCLE.toFixed(3)},eq=brightness=-0.15,fps=30`,
    "-t", CYCLE.toFixed(3), "-an", "-c:v", "libx264", "-preset", "fast", "-crf", "20", intro]);
  const padded = `${WORK}/pad-${TAKE}.mp4`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", seg,
    "-vf", `tpad=stop_mode=clone:stop_duration=${Math.max(0, CYCLE - segDur + 0.2).toFixed(3)},fps=30`,
    "-t", CYCLE.toFixed(3), "-an", "-c:v", "libx264", "-preset", "fast", "-crf", "20", padded]);
  const list = `${WORK}/concat-${TAKE}.txt`;
  writeFileSync(list, [intro, padded, padded, padded].map((p) => `file '${p}'`).join("\n") + "\n");
  const demoMp4 = `${OUT}/imab-vox-demo7-${TAKE}.mp4`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
    "-f", "concat", "-safe", "0", "-i", list, "-i", demoMp3,
    "-map", "0:v", "-map", "1:a", "-c:v", "copy", "-c:a", "aac", "-b:a", "192k", "-shortest", demoMp4]);
  console.log(`✓ ${demoMp4}`);
}
