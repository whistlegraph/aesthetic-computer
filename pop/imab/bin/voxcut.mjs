#!/usr/bin/env node
// voxcut.mjs — the imab hook from the ORIGINAL recordings: slice jeffrey's
// real sung words out of a TikTok take by whisper utterance timing,
// QUANTIZE them onto the 124 grid (onsets + durations snapped to 8ths,
// formant-preserving stretch), then AESTHETIVOX the pitch — WORLD
// f0-replace onto his own sung notes, semitone-quantized and shifted so
// the take's center lands on the lane tonic (A3). No TTS anywhere.
//
//   node pop/imab/bin/voxcut.mjs [--take <tiktok-id>] [--tonic 57]
//   → out/imab-realvox.wav + out/imab-vox-demo3.mp3
//
// Needs toolchain/whistlegraph downloads for the take (wav + syllnote).

import { readFileSync, writeFileSync, existsSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out");
const DL = `${REPO}/toolchain/whistlegraph/downloads`;
const argv = process.argv.slice(2);
const flag = (n, d) => { const i = argv.indexOf(`--${n}`); return i >= 0 && argv[i + 1] ? argv[i + 1] : d; };
const TAKE = flag("take", "7311159624588070175");
const TONIC = Number(flag("tonic", 57));                  // A3
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"], ...opts });
const SR = 48_000, BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;

const WAV = `${DL}/whistlegraph-${TAKE}.wav`;
const SYL = `${DL}/whistlegraph-${TAKE}.syllnote.json`;
if (!existsSync(WAV) || !existsSync(SYL)) { console.error(`✗ missing ${WAV} (run analyze-corpus)`); process.exit(1); }
const doc = JSON.parse(readFileSync(SYL, "utf8"));

// ── match the lyric template through the take's whisper words ─────────
const TMPL = "i'm a butterfly flapping for you guys just a costume i put on in my room".split(" ");
const NSYL = { butterfly: 3, flapping: 2, costume: 2 };
const norm = (w) => w.toLowerCase().replace(/[^a-z']/g, "");
const fuzzy = (a, b) => a === b || (a.length > 3 && b.length > 3 && (a.startsWith(b.slice(0, 4)) || b.startsWith(a.slice(0, 4))));
const seq = [];
let ti = 0;
for (const w of doc.words) {
  if (ti < TMPL.length && fuzzy(TMPL[ti], norm(w.text))) { seq.push({ tw: TMPL[ti], w }); ti++; }
}
if (ti < 14) { console.error(`✗ only matched ${ti}/16 template words in take ${TAKE}`); process.exit(1); }
console.log(`take ${TAKE}: matched ${ti}/16 words`);

// take register → shift so his center lands on the lane tonic
const nucs = seq.flatMap((s) => s.w.nuclei.map((n) => [n.midi + n.cents / 100, n.rms * n.durSec]));
const vals = nucs.map((x) => x[0]), wts = nucs.map((x) => x[1]);
const order = vals.map((_, i) => i).sort((a, b) => vals[a] - vals[b]);
let cum = 0; const half = wts.reduce((s, x) => s + x, 0) / 2;
let center = vals[order[0]];
for (const i of order) { cum += wts[i]; if (cum >= half) { center = vals[i]; break; } }
const SHIFT = TONIC - Math.round(center);
console.log(`take center ≈ midi ${center.toFixed(1)} → shift ${SHIFT >= 0 ? "+" : ""}${SHIFT} st`);

// ── quantize: his own rhythm, snapped to the 124 grid in 8ths ─────────
const onsets = seq.map((s) => (s.w.nuclei[0]?.startSec ?? s.w.fromMs / 1000));
const t0 = onsets[0];
const iois = onsets.slice(1).map((t, i) => t - onsets[i]).filter((d) => d > 0.08);
const ioi = iois.sort((a, b) => a - b)[Math.floor(iois.length / 2)];
console.log(`take beat ≈ ${ioi.toFixed(3)}s (${(60 / ioi).toFixed(1)} BPM) → ${BPM}`);
const snap = (x, q = 0.5) => Math.round(x / q) * q;

const NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const midiToName = (m) => NAMES[((m % 12) + 12) % 12] + (Math.floor(m / 12) - 1);
const tmp = mkdtempSync(join(tmpdir(), "voxcut-"));
const readF32 = (wav) => {
  const raw = join(tmp, "r.f32");
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav, "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
};

const G0 = 0.1;
const master = new Float32Array(Math.ceil((G0 + 24 * BEAT + 2) * SR));
const noteStarts = [], noteNames = [], table = [];
for (let i = 0; i < seq.length; i++) {
  const { tw, w } = seq[i];
  const wt0 = w.fromMs / 1000 - 0.04, wt1 = w.toMs / 1000 + 0.06;
  const srcDur = wt1 - wt0;
  const beatsOn = snap((onsets[i] - t0) / ioi, 0.5);
  const winBeats = (w.toMs - w.fromMs) / 1000 / ioi;
  const beatsDur = Math.min(3, Math.max(0.5, snap(Math.max(winBeats, 0.6), 0.5)));
  const tgtDur = beatsDur * BEAT;
  const clip = join(tmp, `c${i}.wav`), str = join(tmp, `s${i}.wav`);
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-ss", Math.max(0, wt0).toFixed(3), "-t", srcDur.toFixed(3), "-i", WAV, "-ac", "1", "-ar", String(SR), clip]);
  const ratio = Math.min(4, Math.max(0.5, tgtDur / srcDur));
  sh("rubberband", ["-t", ratio.toFixed(4), "-F", "-c", "5", clip, str]);
  const segArr = readF32(str);
  const placed = G0 + beatsOn * BEAT;
  const off = Math.floor(placed * SR);
  const n = Math.min(segArr.length, master.length - off);
  const fade = Math.min(Math.floor(0.02 * SR), n >> 2);
  for (let j = 0; j < n; j++) {
    let g = 1;
    if (j < fade) g = j / fade;
    if (j > n - fade) g = (n - j) / fade;
    master[off + j] += segArr[j] * g;
  }
  // aesthetivox targets: HIS notes, semitone-quantized, register-shifted
  const need = NSYL[tw] ?? 1;
  let ns = [...w.nuclei].sort((a, b) => a.startSec - b.startSec);
  if (ns.length > need)
    ns = ns.sort((a, b) => b.rms * b.durSec - a.rms * a.durSec).slice(0, need).sort((a, b) => a.startSec - b.startSec);
  const words = [];
  for (const nuc of ns) {
    const rel = (nuc.startSec - wt0) * ratio;
    noteStarts.push((placed + Math.max(0, rel)).toFixed(3));
    noteNames.push(midiToName(nuc.midi + SHIFT));
    words.push(midiToName(nuc.midi + SHIFT));
  }
  table.push(`${tw.padEnd(10)} beat ${String(beatsOn).padStart(4)} dur ${beatsDur}  → ${words.join(" ") || "·"}`);
}
console.log(table.join("\n"));

const dry = join(tmp, "dry.f32"), dryWav = join(tmp, "dry.wav");
writeFileSync(dry, Buffer.from(master.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", dry, dryWav]);
console.log(`→ aesthetivox: WORLD snap, ${noteNames.length} notes`);
const pit = join(tmp, "pitched.wav");
const r = spawnSync(`${REPO}/pop/.venv/bin/python`, [
  `${REPO}/pop/bin/pitchsnap_world.py`, dryWav, pit,
  "--notes", noteNames.join(","), "--note-starts", noteStarts.join(","),
  "--retain", "1.0", "--xfade-ms", "30", "--voicing-ramp-ms", "20",
  "--vibrato-hz", "5.2", "--vibrato-cents", "16",
], { stdio: ["ignore", "inherit", "inherit"] });
if (r.status !== 0) { console.error("✗ WORLD failed"); process.exit(1); }
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", pit, "-c:a", "pcm_s16le", `${OUT}/imab-realvox.wav`]);
console.log(`✓ ${OUT}/imab-realvox.wav`);

// halo + demo over the bed, hook at cycles 2–4
sh(`${REPO}/pop/.venv/bin/python`, [`${REPO}/spinging/lib/vocal_bus.py`, "reverb", pit, join(tmp, "halo.wav"), "-16", "1.1"]);
const acc = readF32(`${OUT}/imab-accomp-124.wav`);
const vox = readF32(join(tmp, "halo.wav"));
const rms = (a) => { let s = 0, n = 0; for (let i = 0; i < a.length; i++) if (Math.abs(a[i]) > 1e-4) { s += a[i] * a[i]; n++; } return Math.sqrt(s / Math.max(1, n)); };
const vg = Math.min(6, (rms(acc) * 1.9) / Math.max(1e-9, rms(vox)));
const mix = Float32Array.from(acc);
for (let cyc = 1; cyc < 4; cyc++) {
  const off = Math.floor(cyc * 8 * BAR * SR);
  for (let j = 0; j < vox.length; j++) { const d = off + j; if (d < mix.length) mix[d] += vox[j] * vg; }
}
let pk = 0; for (let i = 0; i < mix.length; i++) pk = Math.max(pk, Math.abs(mix[i]));
if (pk > 0.9) for (let i = 0; i < mix.length; i++) mix[i] *= 0.9 / pk;
const stbuf = new Float32Array(mix.length * 2);
for (let i = 0; i < mix.length; i++) { stbuf[2 * i] = mix[i]; stbuf[2 * i + 1] = mix[i]; }
writeFileSync(join(tmp, "demo.f32"), Buffer.from(stbuf.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", join(tmp, "demo.f32"),
  "-metadata", "title=imab-vox-demo3", "-metadata", "artist=Whistlegraph Dot Org",
  "-c:a", "libmp3lame", "-q:a", "2", `${OUT}/imab-vox-demo3.mp3`]);
rmSync(tmp, { recursive: true, force: true });
console.log(`✓ ${OUT}/imab-vox-demo3.mp3 (vox gain ${vg.toFixed(2)})`);
