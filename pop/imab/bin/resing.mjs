#!/usr/bin/env node
// resing.mjs — the imab hook through the FULL spinging engine (round 6.5):
// one continuous WORLD line — guided phoneme alignment, legato bridging,
// plosive-safe onsets, self-choir on vowels, goalpost conformance — on the
// MEASURED melody (imab.np), then a reverb halo and a demo mix over the
// accompaniment bed. This replaces the sliced sing-hook.mjs route.
//
//   node pop/imab/bin/resing.mjs [--register 0] [--wet -16]
//   → out/imab-hookvox2.wav (the sung line) + out/imab-vox-demo2.mp3

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { buildLineScore, writeLineScore } from "../../../spinging/lib/notation.mjs";
import { alignWords } from "../../../spinging/lib/align-words.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out");
const WORK = resolve(OUT, "resing");
mkdirSync(WORK, { recursive: true });
const VENV_PY = `${REPO}/pop/.venv/bin/python`;
const ENGINE = `${REPO}/spinging/lib/sing_line_world.py`;
const VOCAL_BUS = `${REPO}/spinging/lib/vocal_bus.py`;
const GOALPOSTS = `${REPO}/spinging/cache/goalposts.json`;
const argv = process.argv.slice(2);
const flag = (n, d) => { const i = argv.indexOf(`--${n}`); return i >= 0 && argv[i + 1] ? Number(argv[i + 1]) : d; };
const REGISTER = flag("register", 0);
const WET = flag("wet", -16);
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "inherit", "inherit"], ...opts });

const BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;
const T = (bar, beat) => 0.1 + ((bar - 1) * 4 + (beat - 1)) * BEAT;
// ── the measured melody, word → slots (imab.np) ───────────────────────
const A3 = 57, B3 = 59, Cs4 = 61, D4 = 62, Gs3 = 56, A4 = 69;
const LINE = {
  text: "I'm a butterfly, flapping for you guys, just a costume, I put on, in my room.",
  words: [
    { w: "i'm",       slots: [{ t: T(1, 1), dur: 0.5 * BEAT, midi: A3 }] },
    { w: "a",         slots: [{ t: T(1, 1.75), dur: 0.25 * BEAT, midi: A3 }] },
    { w: "butterfly", slots: [{ t: T(1, 2), dur: BEAT, midi: A3 },
                              { t: T(1, 3), dur: BEAT, midi: A3 },
                              { t: T(1, 4), dur: BEAT, midi: A3 }] },
    { w: "flapping",  slots: [{ t: T(2, 1.5), dur: 0.5 * BEAT, midi: A3 },
                              { t: T(2, 2.5), dur: BEAT, midi: A4 }] },
    { w: "for",       slots: [{ t: T(2, 3.5), dur: 0.5 * BEAT, midi: A3 }] },
    { w: "you",       slots: [{ t: T(2, 4), dur: 0.5 * BEAT, midi: A3 }] },
    { w: "guys",      slots: [{ t: T(2, 4.5), dur: 0.5 * BEAT, midi: A3 }] },
    { w: "just",      slots: [{ t: T(3, 2.5), dur: 0.5 * BEAT, midi: D4 }] },
    { w: "a",         slots: [{ t: T(3, 4), dur: 0.5 * BEAT, midi: D4 }] },
    { w: "costume",   slots: [{ t: T(3, 4.5), dur: 0.5 * BEAT, midi: Cs4 },
                              { t: T(4, 1), dur: 1.5 * BEAT, midi: B3 }] },
    { w: "i",         slots: [{ t: T(4, 3), dur: 0.5 * BEAT, midi: B3 }] },
    { w: "put",       slots: [{ t: T(4, 3.5), dur: 0.5 * BEAT, midi: B3 }] },
    { w: "on",        slots: [{ t: T(4, 4), dur: 0.5 * BEAT, midi: A3 }] },
    { w: "in",        slots: [{ t: T(4, 4.5), dur: 0.5 * BEAT, midi: A3 }] },
    { w: "my",        slots: [{ t: T(5, 1), dur: 0.5 * BEAT, midi: Gs3 }] },
    { w: "room",      slots: [{ t: T(5, 1.5), dur: 1.5 * BEAT, midi: A3 }] },
  ],
};

// ── 1 · TTS + words (cached) ──────────────────────────────────────────
const MP3 = `${OUT}/imab-line-vocal.mp3`;
if (!existsSync(MP3)) {
  console.log("→ spinging say");
  const r = sh("node", [`${REPO}/spinging/bin/spinging.mjs`, "say", `${LANE}/imab-hook.sing.txt`, "--out", MP3]);
  if (r.status !== 0 || !existsSync(MP3)) { console.error("✗ say failed"); process.exit(1); }
}
const W48 = `${WORK}/line.wav`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", MP3, "-ac", "1", "-ar", "48000", W48]);
const WJ = MP3.replace(/\.mp3$/, "-words.json");
if (!existsSync(WJ)) {
  console.log("→ whisper align");
  const r = sh("node", [`${REPO}/spinging/bin/spinging.mjs`, "align", MP3]);
  if (r.status !== 0 || !existsSync(WJ)) { console.error("✗ align failed"); process.exit(1); }
}
const heard = JSON.parse(readFileSync(WJ, "utf8"));
console.log(`  heard: ${heard.map((h) => h.text).join(" ")}`);
const mapWords = LINE.words.map((w) => w.w);
const windows = alignWords(mapWords, heard);
for (let i = 0; i < windows.length; i++) {          // repair empty windows
  if (windows[i] && windows[i].fromMs != null) continue;
  const prev = windows[i - 1], next = windows.slice(i + 1).find((w) => w && w.fromMs != null);
  const a = prev ? prev.toMs : 0, b = next ? next.fromMs : a + 300;
  windows[i] = { fromMs: a, toMs: Math.max(a + 120, b) };
  console.log(`  (repaired window for "${mapWords[i]}")`);
}

// ── 2 · choral notation ───────────────────────────────────────────────
const score = await buildLineScore({ text: LINE.text, words: LINE.words });
const scorePath = `${WORK}/score.json`;
writeLineScore(scorePath, score);
const phraseStartOf = new Array(LINE.words.length).fill(false);
for (const n of score.notes)
  if (n.syllableIndex === 0 && n.articulation === "phraseStart") phraseStartOf[n.wordIndex] = true;

// ── 3 · the plan ──────────────────────────────────────────────────────
const lineDur = heard[heard.length - 1].toMs / 1000 + 0.3;
const planWords = [];
for (let wi = 0; wi < LINE.words.length; wi++) {
  const word = LINE.words[wi], win = windows[wi], slots = word.slots;
  const last = slots[slots.length - 1];
  let tEnd = last.t + Math.min(last.dur, 1.8);
  const next = LINE.words[wi + 1];
  if (next) tEnd = Math.min(tEnd, next.slots[0].t - 0.01);
  const prevWin = wi > 0 ? windows[wi - 1] : null;
  const nextWin = wi + 1 < windows.length ? windows[wi + 1] : null;
  let s0 = win.fromMs - 60, s1 = win.toMs + 100;
  if (prevWin) s0 = Math.max(s0, (prevWin.toMs + win.fromMs) / 2);
  if (nextWin) s1 = Math.min(s1, (win.toMs + nextWin.fromMs) / 2 + 20);
  planWords.push({ w: word.w, wordIndex: wi,
    srcFromMs: Math.round(Math.max(0, s0)), srcToMs: Math.round(Math.min(lineDur * 1000, s1)),
    slots, hardEnd: +tEnd.toFixed(4), phraseStart: phraseStartOf[wi] });
}
const lineT0 = Math.max(0, LINE.words[0].slots[0].t - 0.35);
const lineT1 = planWords[planWords.length - 1].hardEnd + 0.4;
const sung = `${WORK}/sung.wav`;
const plan = {
  line_wav: W48, out_wav: sung, lead_wav: `${WORK}/lead.wav`,
  phoneme_sidecar: `${WORK}/phonemes.json`,
  score: scorePath, goalposts: GOALPOSTS,
  line_t0: +lineT0.toFixed(4), line_t1: +lineT1.toFixed(4),
  harmony: 0.875, seed: 7, f0_floor: 60, f0_ceil: 300,
  octave_opt: true, choir: true, register: REGISTER,
  tweaks: { drift_scale: 1.6, glide_scale: 1, vib_depth_scale: 1, beta_scale: 1,
            air_scale: 1, cons_stretch_scale: 1 },
  words: planWords,
};
const planPath = `${WORK}/plan.json`;
writeFileSync(planPath, JSON.stringify(plan, null, 1));
console.log(`→ sing_line_world (register ${REGISTER})`);
const wr = spawnSync(VENV_PY, [ENGINE, planPath], { encoding: "utf8" });
if (wr.status !== 0) { console.error(wr.stderr?.slice(-1500)); process.exit(1); }
try { console.log("  " + wr.stdout.trim().split("\n").pop().slice(0, 240)); } catch {}

// ── 4 · halo + demo mix over the bed ──────────────────────────────────
const halo = `${WORK}/sung-halo.wav`;
sh(VENV_PY, [VOCAL_BUS, "reverb", sung, halo, String(WET), "1.1"]);
const readF32 = (wav) => {
  const raw = `${WORK}/.r.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav, "-f", "f32le", "-ac", "1", "-ar", "48000", raw]);
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
};
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", halo, "-c:a", "pcm_s16le", `${OUT}/imab-hookvox2.wav`]);
console.log(`✓ ${OUT}/imab-hookvox2.wav`);

const ACC = `${OUT}/imab-accomp-124.wav`;
if (!existsSync(ACC)) { console.error("✗ run gen-accomp.mjs first"); process.exit(1); }
const acc = readF32(ACC), vox = readF32(halo);
const rms = (a) => { let s = 0, n = 0; for (let i = 0; i < a.length; i++) if (Math.abs(a[i]) > 1e-4) { s += a[i] * a[i]; n++; } return Math.sqrt(s / Math.max(1, n)); };
const vg = Math.min(6, (rms(acc) * 1.9) / Math.max(1e-9, rms(vox)));
const mix = Float32Array.from(acc);
for (let cyc = 1; cyc < 4; cyc++) {
  const off = Math.floor((cyc * 8 * BAR + lineT0) * 48000);
  for (let j = 0; j < vox.length; j++) { const d = off + j; if (d < mix.length) mix[d] += vox[j] * vg; }
}
let pk = 0; for (let i = 0; i < mix.length; i++) pk = Math.max(pk, Math.abs(mix[i]));
if (pk > 0.9) for (let i = 0; i < mix.length; i++) mix[i] *= 0.9 / pk;
const st = new Float32Array(mix.length * 2);
for (let i = 0; i < mix.length; i++) { st[2 * i] = mix[i]; st[2 * i + 1] = mix[i]; }
writeFileSync(`${WORK}/.demo.f32`, Buffer.from(st.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "f32le", "-ar", "48000", "-ac", "2", "-i", `${WORK}/.demo.f32`,
  "-metadata", "title=imab-vox-demo2", "-metadata", "artist=Whistlegraph Dot Org",
  "-c:a", "libmp3lame", "-q:a", "2", `${OUT}/imab-vox-demo2.mp3`]);
console.log(`✓ ${OUT}/imab-vox-demo2.mp3 (vox gain ${vg.toFixed(2)})`);
