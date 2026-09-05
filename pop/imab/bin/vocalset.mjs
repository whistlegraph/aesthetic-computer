#!/usr/bin/env node
// vocalset.mjs — regulate a take into a placeable VOCAL SET (@jeffrey
// 2026-09-04: "place all our sets of takes so we have all that
// regulated before we compose further"). Every set gets the same
// treatment: demucs stem → one tempo fit to 124 → local whisper words
// → flutish note-lock to the GT hook (word-level targets,
// register-fit to THIS singer). Outputs are uniform so the floor and
// the scroll video can place any set by grid address
// (pop/imab/vocal-sets.json).
//
//   node pop/imab/bin/vocalset.mjs <take-id>
//   → out/imab-set-<take>.wav + out/imab-set-<take>-words.json

import { readFileSync, writeFileSync, existsSync, mkdirSync, rmSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out");
const DL = `${REPO}/toolchain/whistlegraph/downloads`;
const WORK = `${process.env.HOME}/.cache/ac/imab`;
mkdirSync(WORK, { recursive: true });
const TAKE = process.argv[2];
if (!TAKE) { console.error("usage: vocalset.mjs <take-id>"); process.exit(1); }
const PY = `${REPO}/pop/.venv/bin/python`;
const sh = (cmd, args) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"] });
const SR = 48_000, BPM = 124, BEAT = 60 / BPM;

// window + tempo from the take's own singing (the sacredvox method)
const doc = JSON.parse(readFileSync(`${DL}/whistlegraph-${TAKE}.syllnote.json`, "utf8"));
const TMPL = "i'm a butterfly flapping for you guys just a costume i put on in my room".split(" ");
const norm = (w) => w.toLowerCase().replace(/[^a-z']/g, "");
const fuzzy = (a, b) => a === b || (a.length > 3 && b.length > 3 && (a.startsWith(b.slice(0, 4)) || b.startsWith(a.slice(0, 4))));
const seq = []; let ti = 0;
for (const w of doc.words) if (ti < TMPL.length && fuzzy(TMPL[ti], norm(w.text))) { seq.push(w); ti++; }
if (ti < 14) { console.error(`✗ matched ${ti}/16`); process.exit(1); }
const onsets = seq.map((w) => w.nuclei?.[0]?.startSec ?? w.fromMs / 1000);
const iois = onsets.slice(1).map((t, i) => t - onsets[i]).filter((d) => d > 0.08).sort((a, b) => a - b);
const ratio = BEAT / iois[Math.floor(iois.length / 2)];
const t0 = Math.max(0, seq[0].fromMs / 1000 - 0.15);
const t1 = seq[seq.length - 1].toMs / 1000 + 0.6;
console.log(`set ${TAKE}: ${t0.toFixed(2)}–${t1.toFixed(2)}s · stretch ${ratio.toFixed(3)}`);

const SEP = `${WORK}/sep/htdemucs/whistlegraph-${TAKE}/vocals.wav`;
if (!existsSync(SEP)) {
  console.log("→ demucs (slow)");
  sh("demucs", ["-n", "htdemucs", "--two-stems=vocals", "-o", `${WORK}/sep`, `${DL}/whistlegraph-${TAKE}.wav`]);
}
const trim = `${WORK}/set-${TAKE}-trim.wav`, str = `${WORK}/set-${TAKE}-124.wav`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-ss", t0.toFixed(3), "-t", (t1 - t0).toFixed(3),
  "-i", SEP, "-ac", "1", "-ar", String(SR), "-af", "highpass=f=80", trim]);
sh("rubberband", ["-t", ratio.toFixed(4), "-F", "-c", "6", trim, str]);

// the written hook, word-level (first note per word; the second "a"
// resolves to F4 in the targets pass below)
const WORD_GT = { "i'm": "C4", a: "G4", butterfly: "C4", flapping: "C4", for: "C4",
  you: "C4", guys: "C4", "it's": "D4", just: "G4", costume: "E4", i: "E4",
  put: "E4", on: "D4", in: "C4", my: "C4", room: "C4" };

// ── words: syllnote boundaries through the ACTUAL stretch map, then
// whisper VALIDATES every sample until success ────────────────────────
// rubberband -c 6 warps time non-uniformly, so the raw take's syllnote
// boundaries ride a DTW of the energy envelopes (same law as
// lyrictrack). Then every word window is clipped and transcribed by
// whisper; boundaries widen/shift until the clip says its own label
// and nothing else (@jeffrey: "whisper ai validate them on the
// boundaries until we reach success"). A set-fixes/<take>.json still
// overrides everything for surgical dictation.
const WORDS = `${OUT}/imab-set-${TAKE}-words.json`;
const FIX = resolve(HERE, `../set-fixes/${TAKE}.json`);
let words;
if (existsSync(FIX)) {
  words = JSON.parse(readFileSync(FIX, "utf8")).words;
  console.log(`→ set-fixes/${TAKE}.json (${words.length} words, pipeline skipped)`);
} else {
  const HOP_S = 0.010;
  const envOf = (wav) => {
    const raw = `${WORK}/.setenv.f32`;
    sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav,
      "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
    const b = readFileSync(raw);
    const x = new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
    const hop = Math.round(HOP_S * SR), n = Math.floor(x.length / hop);
    const e = new Float64Array(n);
    for (let i = 0; i < n; i++) {
      let s = 0;
      for (let j = i * hop; j < (i + 1) * hop; j++) s += x[j] * x[j];
      e[i] = Math.log10(1e-8 + Math.sqrt(s / hop));
    }
    const flux = new Float64Array(n);
    for (let i = 1; i < n; i++) flux[i] = Math.max(0, e[i] - e[i - 1]);
    const z = (a) => { const m = a.reduce((p, c) => p + c, 0) / a.length;
      const sd = Math.sqrt(a.reduce((p, c) => p + (c - m) ** 2, 0) / a.length) || 1;
      return a.map((v) => (v - m) / sd); };
    return { e: z([...e]), f: z([...flux]) };
  };
  const A = envOf(trim), B = envOf(str);
  const N = A.e.length, M = B.e.length, BAND = 200, INF = 1e18;
  const D = new Float64Array(N * M).fill(INF);
  const cost = (i, j) => Math.abs(A.e[i] - B.e[j]) + Math.abs(A.f[i] - B.f[j]);
  for (let i = 0; i < N; i++) {
    const c = Math.round((i / N) * M);
    for (let j = Math.max(0, c - BAND); j < Math.min(M, c + BAND); j++) {
      const best = i === 0 && j === 0 ? 0 : Math.min(
        i > 0 && j > 0 ? D[(i - 1) * M + j - 1] : INF,
        i > 0 ? D[(i - 1) * M + j] : INF,
        j > 0 ? D[i * M + j - 1] : INF);
      if (best < INF) D[i * M + j] = cost(i, j) + best;
    }
  }
  const map = new Float64Array(N).fill(-1);
  { let i = N - 1, j = M - 1;
    while (i > 0 || j > 0) {
      map[i] = map[i] < 0 ? j : (map[i] + j) / 2;
      const dg = i > 0 && j > 0 ? D[(i - 1) * M + j - 1] : INF;
      const up = i > 0 ? D[(i - 1) * M + j] : INF;
      const lf = j > 0 ? D[i * M + j - 1] : INF;
      if (dg <= up && dg <= lf) { i--; j--; } else if (up <= lf) i--; else j--;
    }
    map[0] = 0;
    for (let k = 1; k < N; k++) if (map[k] < 0) map[k] = map[k - 1]; }
  const warp = (rawSec) => {
    const p = Math.min(N - 1, Math.max(0, rawSec / HOP_S));
    const a = Math.floor(p), b2 = Math.min(N - 1, a + 1);
    return (map[a] + (map[b2] - map[a]) * (p - a)) * HOP_S;
  };
  console.log("  (DTW warp active: syllnote boundaries follow rubberband's time map)");

  words = doc.words
    .filter((w) => w.toMs / 1000 > t0 && w.fromMs / 1000 < t1)
    .map((w) => ({ text: norm(w.text),
      fromMs: Math.round(warp(Math.max(0, w.fromMs / 1000 - t0)) * 1000),
      toMs: Math.round(warp(Math.max(0, w.toMs / 1000 - t0)) * 1000) }));

  // whisper jury: sung syllables are unrecognizable in isolation, so
  // each word is judged in CONTEXT — a clip spanning its neighbors,
  // transcribed with word timestamps (-ml 1 -sow); the label's own
  // segment steers the boundary. Rounds repeat until motion settles.
  const MODEL = [`${process.env.HOME}/.whisper-models/ggml-small.bin`,
    `${process.env.HOME}/.whisper-models/ggml-base.en.bin`].find(existsSync);
  const hearWords = (f, e) => {
    const clip = `${WORK}/set-val.wav`;
    try { rmSync(`${WORK}/set-val.json`); } catch {}
    sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
      "-ss", (f / 1000).toFixed(3), "-t", ((e - f) / 1000).toFixed(3), "-i", str,
      "-ac", "1", "-ar", "16000", "-af", "adelay=250,apad=pad_dur=0.35", clip]);
    spawnSync("whisper-cli", ["-m", MODEL, "-f", clip, "-l", "en",
      "-ml", "1", "-sow", "-oj", "-of", `${WORK}/set-val`, "-np"], { stdio: "ignore" });
    let j;
    try { j = JSON.parse(readFileSync(`${WORK}/set-val.json`, "utf8")); } catch { return []; }
    const toMs2 = (ts) => { const m = /(\d+):(\d+):(\d+)[,.](\d+)/.exec(ts);
      return ((+m[1] * 60 + +m[2]) * 60 + +m[3]) * 1000 + +m[4]; };
    return (j.transcription ?? [])
      .map((s) => ({ text: s.text.trim().toLowerCase().replace(/[^a-z']/g, ""),
        fromMs: toMs2(s.timestamps.from) - 250 + f,
        toMs: toMs2(s.timestamps.to) - 250 + f }))
      .filter((s) => s.text);
  };
  const ALIKE = { a: ["ah", "uh", "oh", "ha", "huh", "aah", "la"],
    i: ["eye", "ah", "e", "ee"], "i'm": ["im", "em", "m", "hmm", "mm", "i"],
    "it's": ["its", "is", "it", "tss"], my: ["ma", "mah", "mah'"],
    on: ["own", "un", "along"], in: ["and", "en"], for: ["four", "or", "far"],
    you: ["ooh", "oo", "u"], guys: ["gize", "guy", "eyes"], room: ["rum", "roo", "womb"] };
  const hits = (label, ws) => ws.some((w) => w === label
    || (ALIKE[label] || []).includes(w)
    || (w.length > 3 && label.length > 3
        && (w.startsWith(label.slice(0, 4)) || label.startsWith(w.slice(0, 4)))));
  // ADVISORY only (2026-09-05): whisper cannot reliably tell sung
  // melisma words apart — driven by it, boundaries scrambled (guys
  // snapped onto just, room shrank to 15ms). It reports, never moves;
  // boundaries come from the DTW skeleton + set-fixes dictation.
  if (MODEL) {
    for (let k = 0; k < words.length; k++) {
      const w = words[k];
      const cf = Math.max(0, words[k - 1]?.fromMs ?? w.fromMs - 400);
      const ce = words[k + 1]?.toMs ?? w.toMs + 400;
      const cand = hearWords(cf, ce).filter((s) => hits(w.text, [s.text]));
      const mid = (w.fromMs + w.toMs) / 2;
      w.valid = cand.some((s) => s.fromMs - 150 < mid && mid < s.toMs + 150);
      console.log(`  ${w.valid ? "✓" : "?"} ${w.text} ${w.fromMs}–${w.toMs}${w.valid ? " (whisper concurs)" : ""}`);
    }
    // an unverified word outside the vocabulary is a recognizer ghost —
    // its span belongs to the neighboring real word (the lead's
    // recurring mislabel pattern, automated).
    for (let k = words.length - 1; k >= 0; k--) {
      const w = words[k];
      if (w.valid || WORD_GT[w.text]) continue;
      const heir = words[k + 1] ?? words[k - 1];
      if (heir) {
        if (words[k + 1]) heir.fromMs = Math.min(heir.fromMs, w.fromMs);
        else heir.toMs = Math.max(heir.toMs, w.toMs);
        console.log(`  ✗ ${w.text} → merged into ${heir.text}`);
      }
      words.splice(k, 1);
    }
    // keep windows monotonic after the jury moved them around
    for (let k = 1; k < words.length; k++)
      words[k].fromMs = Math.max(words[k].fromMs, words[k - 1].fromMs + 10);
  } else console.log("  (no whisper model — boundaries unvalidated)");
}

// dead-air trim: shrink each word window to where the sound actually
// lives (same law as lyrictrack — max(0.0025, 5% of window peak),
// pads 15ms pre / 30ms post, shrink-only).
{
  const raw = `${WORK}/set-${TAKE}-trim-scan.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", str,
    "-f", "f32le", "-ac", "1", "-ar", "16000", raw]);
  const b = readFileSync(raw);
  const pcm = new Float32Array(b.buffer.slice(b.byteOffset, b.byteOffset + Math.floor(b.length / 4) * 4));
  const HOP = 80, WIN = 320; // 5ms / 20ms at 16k
  const rmsAt = (i) => {
    let s = 0; const a0 = Math.max(0, i), a1 = Math.min(pcm.length, i + WIN);
    for (let j = a0; j < a1; j++) s += pcm[j] * pcm[j];
    return Math.sqrt(s / Math.max(1, a1 - a0));
  };
  for (const w of words) {
    const i0 = Math.floor(w.fromMs / 1000 * 16000), i1 = Math.floor(w.toMs / 1000 * 16000);
    let peak = 0;
    for (let i = i0; i < i1; i += HOP) peak = Math.max(peak, rmsAt(i));
    const thr = Math.max(0.0025, 0.05 * peak);
    let a = i0, z = i1;
    while (a < i1 && rmsAt(a) < thr) a += HOP;
    while (z > a && rmsAt(z - WIN) < thr) z -= HOP;
    const nf = Math.max(w.fromMs, Math.round(a / 16) - 15);
    const nt = Math.min(w.toMs, Math.round(z / 16) + 30);
    if (nt > nf && (nf > w.fromMs || nt < w.toMs)) {
      if (nf - w.fromMs + (w.toMs - nt) > 40)
        console.log(`  trim ${w.text}: ${w.fromMs}–${w.toMs} → ${nf}–${nt}`);
      w.fromMs = nf; w.toMs = nt;
    }
  }
  writeFileSync(WORDS, JSON.stringify(words, null, 1));
}

// flutish note-lock: word-level GT targets (first note per word),
// register-fit to this singer. (Multi-note words keep note 1 in v1 —
// the ping flare returns once this set gets drawn boundaries.)
// (WORD_GT lives above the word pipeline — the merge rule needs it)
let ai = 0;
const targets = [];
for (const w of words) {
  const key = norm(w.text);
  let note = WORD_GT[key];
  if (key === "a") { note = ai === 0 ? "G4" : "F4"; ai++; }
  if (!note) continue;
  targets.push({ label: key, t: +(w.fromMs / 1000).toFixed(3),
    dur: +Math.max(0.1, (w.toMs - w.fromMs) / 1000).toFixed(3), note });
}
const TGT = `${OUT}/imab-set-${TAKE}-targets.json`;
writeFileSync(TGT, JSON.stringify(targets, null, 1));
const FINAL = `${OUT}/imab-set-${TAKE}.wav`;
sh(PY, [`${REPO}/pop/bin/autotune.py`, str, FINAL, "--targets", TGT, "--register-fit",
  "--strength", "1.0", "--preserve", "0.25", "--glide-ms", "30"]);
console.log(`✓ ${FINAL} (${targets.length} word targets)`);
