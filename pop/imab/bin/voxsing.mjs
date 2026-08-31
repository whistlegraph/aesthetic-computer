#!/usr/bin/env node
// voxsing.mjs — the imab hook: ORIGINAL recording, FULL engine.
//
// The take's sung words (whisper utterance windows) drive the spinging
// round-6.5 line engine — guided phoneme alignment, vowel-domain
// stretching so each note is carried across its whole duration, strictly
// monotonic f0 glides between plateaus, legato bridging, self-choir —
// with targets = HIS OWN sung notes, semitone-quantized and shifted to
// the lane tonic, on the 124 grid quantized from his own pacing.
// (voxcut.mjs was the rubberband+f0-replace sketch; this replaces it.)
//
//   node pop/imab/bin/voxsing.mjs [--take <id>] [--tonic 57] [--wet -16]
//   → out/imab-realvox2.wav + out/imab-vox-demo4.mp3

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { buildLineScore, writeLineScore } from "../../../spinging/lib/notation.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out");
const WORK = resolve(OUT, "voxsing");
mkdirSync(WORK, { recursive: true });
const DL = `${REPO}/toolchain/whistlegraph/downloads`;
const VENV_PY = `${REPO}/pop/.venv/bin/python`;
const argv = process.argv.slice(2);
const flag = (n, d) => { const i = argv.indexOf(`--${n}`); return i >= 0 && argv[i + 1] ? argv[i + 1] : d; };
const TAKE = flag("take", "7311159624588070175");
const TONIC = Number(flag("tonic", 57));
const WET = Number(flag("wet", -16));
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"], ...opts });
const SR = 48_000, BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;

const WAV = `${DL}/whistlegraph-${TAKE}.wav`;
const SYLJ = `${DL}/whistlegraph-${TAKE}.syllnote.json`;
if (!existsSync(WAV) || !existsSync(SYLJ)) { console.error(`✗ missing take ${TAKE}`); process.exit(1); }
const doc = JSON.parse(readFileSync(SYLJ, "utf8"));

// ── template match through the take ───────────────────────────────────
const TMPL = "i'm a butterfly flapping for you guys just a costume i put on in my room".split(" ");
const NSYL = { butterfly: 3, flapping: 2, costume: 2 };
const norm = (w) => w.toLowerCase().replace(/[^a-z']/g, "");
const fuzzy = (a, b) => a === b || (a.length > 3 && b.length > 3 && (a.startsWith(b.slice(0, 4)) || b.startsWith(a.slice(0, 4))));
const seq = [];
let ti = 0;
for (const w of doc.words) {
  if (ti < TMPL.length && fuzzy(TMPL[ti], norm(w.text))) { seq.push({ tw: TMPL[ti], w }); ti++; }
}
if (ti < 14) { console.error(`✗ only matched ${ti}/16 words`); process.exit(1); }

// register shift → lane tonic
const nucs = seq.flatMap((s) => s.w.nuclei.map((n) => [n.midi + n.cents / 100, n.rms * n.durSec]));
const vals = nucs.map((x) => x[0]), wts = nucs.map((x) => x[1]);
const ord = vals.map((_, i) => i).sort((a, b) => vals[a] - vals[b]);
let cum = 0; const half = wts.reduce((s, x) => s + x, 0) / 2;
let center = vals[ord[0]];
for (const i of ord) { cum += wts[i]; if (cum >= half) { center = vals[i]; break; } }
const SHIFT = TONIC - Math.round(center);
console.log(`take ${TAKE}: ${ti}/16 words · center midi ${center.toFixed(1)} → shift ${SHIFT >= 0 ? "+" : ""}${SHIFT}`);

// ── grid: his pacing quantized to 8ths at 124 ─────────────────────────
const onsets = seq.map((s) => (s.w.nuclei[0]?.startSec ?? s.w.fromMs / 1000));
const t0take = onsets[0];
const iois = onsets.slice(1).map((t, i) => t - onsets[i]).filter((d) => d > 0.08).sort((a, b) => a - b);
const ioi = iois[Math.floor(iois.length / 2)];
const snapq = (x, q = 0.5) => Math.round(x / q) * q;
const G0 = 0.1;

const LINEWORDS = [];
for (let i = 0; i < seq.length; i++) {
  const { tw, w } = seq[i];
  const beatsOn = snapq((onsets[i] - t0take) / ioi, 0.5);
  const winBeats = (w.toMs - w.fromMs) / 1000 / ioi;
  const beatsDur = Math.min(3, Math.max(0.5, snapq(Math.max(winBeats, 0.6), 0.5)));
  const need = NSYL[tw] ?? 1;
  let ns = [...w.nuclei].sort((a, b) => a.startSec - b.startSec);
  if (ns.length > need)
    ns = ns.sort((a, b) => b.rms * b.durSec - a.rms * a.durSec).slice(0, need).sort((a, b) => a.startSec - b.startSec);
  const per = beatsDur / need;
  const slots = [];
  for (let k = 0; k < need; k++) {
    const midi = (ns[Math.min(k, ns.length - 1)]?.midi ?? Math.round(center)) + SHIFT;
    slots.push({ t: G0 + (beatsOn + k * per) * BEAT, dur: per * BEAT, midi });
  }
  LINEWORDS.push({ w: tw, slots, win: w });
  console.log(`  ${tw.padEnd(10)} beat ${String(beatsOn).padStart(4)} dur ${beatsDur}  → ${slots.map((s) => s.midi).join(" ")}`);
}

// ── notation + plan ───────────────────────────────────────────────────
const W48 = `${WORK}/line.wav`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", WAV, "-ac", "1", "-ar", String(SR), W48]);
const text = "I'm a butterfly, flapping for you guys, just a costume, I put on, in my room.";
const score = await buildLineScore({ text, words: LINEWORDS.map((w) => ({ w: w.w, slots: w.slots })) });
const scorePath = `${WORK}/score.json`;
writeLineScore(scorePath, score);
const phraseStartOf = new Array(LINEWORDS.length).fill(false);
for (const n of score.notes)
  if (n.syllableIndex === 0 && n.articulation === "phraseStart") phraseStartOf[n.wordIndex] = true;

const lineDurMs = doc.words[doc.words.length - 1].toMs + 300;
const planWords = [];
for (let wi = 0; wi < LINEWORDS.length; wi++) {
  const lw = LINEWORDS[wi], slots = lw.slots, win = lw.win;
  const last = slots[slots.length - 1];
  let tEnd = last.t + Math.min(last.dur, 1.8);
  const next = LINEWORDS[wi + 1];
  if (next) tEnd = Math.min(tEnd, next.slots[0].t - 0.01);
  const prevWin = wi > 0 ? LINEWORDS[wi - 1].win : null;
  const nextWin = wi + 1 < LINEWORDS.length ? LINEWORDS[wi + 1].win : null;
  let s0 = win.fromMs - 60, s1 = win.toMs + 100;
  if (prevWin) s0 = Math.max(s0, (prevWin.toMs + win.fromMs) / 2);
  if (nextWin) s1 = Math.min(s1, (win.toMs + nextWin.fromMs) / 2 + 20);
  planWords.push({ w: lw.w, wordIndex: wi,
    srcFromMs: Math.round(Math.max(0, s0)), srcToMs: Math.round(Math.min(lineDurMs, s1)),
    slots, hardEnd: +tEnd.toFixed(4), phraseStart: phraseStartOf[wi] });
}
const lineT0 = Math.max(0, LINEWORDS[0].slots[0].t - 0.35);
const lineT1 = planWords[planWords.length - 1].hardEnd + 0.4;
const sung = `${WORK}/sung.wav`;
const plan = {
  line_wav: W48, out_wav: sung, lead_wav: `${WORK}/lead.wav`,
  phoneme_sidecar: `${WORK}/phonemes.json`,
  score: scorePath, goalposts: `${REPO}/spinging/cache/goalposts.json`,
  line_t0: +lineT0.toFixed(4), line_t1: +lineT1.toFixed(4),
  harmony: 0.875, seed: 11, f0_floor: 60, f0_ceil: 400,
  octave_opt: false, choir: true, register: 0,
  tweaks: { drift_scale: 1.6, glide_scale: 1, vib_depth_scale: 1, beta_scale: 1,
            air_scale: 1, cons_stretch_scale: 1 },
  words: planWords,
};
writeFileSync(`${WORK}/plan.json`, JSON.stringify(plan, null, 1));
console.log("→ sing_line_world on the real take");
const wr = spawnSync(VENV_PY, [`${REPO}/spinging/lib/sing_line_world.py`, `${WORK}/plan.json`], { encoding: "utf8" });
if (wr.status !== 0) { console.error(wr.stderr?.slice(-1500)); process.exit(1); }

// ── halo + demo mix ───────────────────────────────────────────────────
const halo = `${WORK}/halo.wav`;
sh(VENV_PY, [`${REPO}/spinging/lib/vocal_bus.py`, "reverb", sung, halo, String(WET), "1.1"]);
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", halo, "-c:a", "pcm_s16le", `${OUT}/imab-realvox2.wav`]);
console.log(`✓ ${OUT}/imab-realvox2.wav`);
const readF32 = (wav) => {
  const raw = `${WORK}/.r.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav, "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
};
const acc = readF32(`${OUT}/imab-accomp-124.wav`), vox = readF32(halo);
const rms = (a) => { let s = 0, n = 0; for (let i = 0; i < a.length; i++) if (Math.abs(a[i]) > 1e-4) { s += a[i] * a[i]; n++; } return Math.sqrt(s / Math.max(1, n)); };
const vg = Math.min(6, (rms(acc) * 1.9) / Math.max(1e-9, rms(vox)));
const mix = Float32Array.from(acc);
for (let cyc = 1; cyc < 4; cyc++) {
  const off = Math.floor((cyc * 8 * BAR + lineT0) * SR);
  for (let j = 0; j < vox.length; j++) { const d = off + j; if (d < mix.length) mix[d] += vox[j] * vg; }
}
let pk = 0; for (let i = 0; i < mix.length; i++) pk = Math.max(pk, Math.abs(mix[i]));
if (pk > 0.9) for (let i = 0; i < mix.length; i++) mix[i] *= 0.9 / pk;
const stb = new Float32Array(mix.length * 2);
for (let i = 0; i < mix.length; i++) { stb[2 * i] = mix[i]; stb[2 * i + 1] = mix[i]; }
writeFileSync(`${WORK}/.demo.f32`, Buffer.from(stb.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", `${WORK}/.demo.f32`,
  "-metadata", "title=imab-vox-demo4", "-metadata", "artist=Whistlegraph Dot Org",
  "-c:a", "libmp3lame", "-q:a", "2", `${OUT}/imab-vox-demo4.mp3`]);
console.log(`✓ ${OUT}/imab-vox-demo4.mp3 (vox gain ${vg.toFixed(2)})`);
