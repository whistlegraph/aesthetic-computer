#!/usr/bin/env node
// verify-vocals.mjs — pre-listen vocal verification.
//
// Slices the master into ~12 s windows where the jeffrey-pvc mantra
// should be present, runs whisper-cli (whisper.cpp) on each slice,
// and prints expected vs detected lyrics with a confidence score.
//
// Catches:
//   - vocals starting at the wrong time (window 0 should detect "i hope")
//   - inaudible vocals (window has no detected words)
//   - garbled vocals (detected text barely matches expected mantra)
//   - choppy vocals (detected fragments shorter than expected per window)
//
// Run AFTER bake-c.mjs, BEFORE opening in QuickTime:
//   node pop/hellsine/c/verify-vocals.mjs

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, statSync, readFileSync } from "node:fs";
import { homedir } from "node:os";

const MASTER =
  process.argv[2] ||
  `${homedir()}/Documents/Shelf/hellsine/hellsine-c-MASTER.wav`;
const MODEL = `${homedir()}/.whisper-models/ggml-base.en.bin`;
const WORK = "/tmp/hellsine-verify";

if (!existsSync(MASTER)) {
  console.error(`[verify] master not found: ${MASTER}`);
  process.exit(1);
}
if (!existsSync(MODEL)) {
  console.error(`[verify] whisper model not found: ${MODEL}`);
  process.exit(1);
}
mkdirSync(WORK, { recursive: true });

// hellsine: BPM 182 → SPBAR ≈ 1.319 s. Statement begins at 15.82 s.
// One mantra per 8 bars (≈ 10.55 s). With JVOC_RATE = 2^(-3/12) the
// audio stretches by ~1.19× so a 3 s spoken mantra renders ~3.6 s.
// Climax cuts vocals near acStampJ - 3*SPBAR (≈ 103 s by default).
const BPM = 182;
const SPB = 60 / BPM;
const SPBAR = 4 * SPB;
const STATEMENT = 15.82;
const STEP = 4 * SPBAR;            // ≈ 5.27 s — one mantra (2 per brass loop)
const WINDOW = STEP;               // listen for the full mantra slot

const VARIANTS = ["money", "honey", "bunnies"];
// 12-word mantra (extended 2026-05-26)
const MANTRA = (v) => `i hope that we get all of the ${v} that we want`;

// Build the list of windows to test — first 6 mantras after the drop.
const windows = [];
for (let i = 0; i < 6; i++) {
  const t = STATEMENT + i * STEP;
  if (t + WINDOW > 145) break;
  windows.push({
    idx: i,
    startS: t,
    durS: WINDOW,
    variant: VARIANTS[i % 3],
    expect: MANTRA(VARIANTS[i % 3]),
  });
}

// Levenshtein for fuzzy match scoring
function lev(a, b) {
  const m = a.length, n = b.length;
  if (m === 0) return n; if (n === 0) return m;
  const dp = Array.from({ length: m + 1 }, () => new Array(n + 1).fill(0));
  for (let i = 0; i <= m; i++) dp[i][0] = i;
  for (let j = 0; j <= n; j++) dp[0][j] = j;
  for (let i = 1; i <= m; i++)
    for (let j = 1; j <= n; j++)
      dp[i][j] = Math.min(
        dp[i - 1][j] + 1,
        dp[i][j - 1] + 1,
        dp[i - 1][j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1),
      );
  return dp[m][n];
}

const norm = (s) => s.toLowerCase().replace(/[^a-z ]+/g, " ").replace(/\s+/g, " ").trim();
const wordMatchRatio = (expectRaw, gotRaw) => {
  const ex = norm(expectRaw).split(" ").filter(Boolean);
  const gw = norm(gotRaw).split(" ").filter(Boolean);
  if (ex.length === 0) return 1;
  let hit = 0;
  const used = new Array(gw.length).fill(false);
  for (const w of ex) {
    let bestI = -1, bestD = Infinity;
    for (let i = 0; i < gw.length; i++) {
      if (used[i]) continue;
      const d = lev(w, gw[i]);
      if (d < bestD) { bestD = d; bestI = i; }
    }
    if (bestI >= 0 && bestD <= Math.max(1, Math.floor(w.length * 0.4))) {
      hit++; used[bestI] = true;
    }
  }
  return hit / ex.length;
};

console.log(`\n[verify] master = ${MASTER}`);
console.log(`[verify] ${(statSync(MASTER).size / 1e6).toFixed(2)} MB`);
console.log(`[verify] testing ${windows.length} mantra windows after drop @ ${STATEMENT}s`);
console.log("─".repeat(80));

// Word-alignment check across ALL vocal layers — ElevenLabs, wizard,
// and every Apple-say choir voice that has a sidecar. Confirms they
// share the canonical 10-word mantra so the C engine's time-warp can
// snap them all to the same THEME beats in unison.
function alignmentCheck() {
  const SAMPLES = "/Users/jas/aesthetic-computer/pop/hellsine/samples";
  const CHOIR   = `${SAMPLES}/say-choir`;
  const CHOIR_VOICES = [
    "cellos", "bells", "good-news", "bad-news", "whisper", "bahh",
    "trinoids", "zarvox", "organ", "boing", "wobble", "bubbles",
  ];
  const norm = (s) => s.toLowerCase().replace(/[^a-z]/g, "");
  let layerOK = 0, layerSkip = 0, totalWords = 0, alignedWords = 0;
  for (const v of VARIANTS) {
    const refPath = `${SAMPLES}/jeffrey-vocal-${v}.words.txt`;
    if (!existsSync(refPath)) continue;
    const ref = readFileSync(refPath, "utf8").trim().split(/\n+/)
      .map((l) => l.split(/\s+/)[0]);
    console.log(`\n[align] ${v.padEnd(8)} reference=ElevenLabs (${ref.length} words)`);
    const layers = [
      ["wizard", `${SAMPLES}/jeffrey-live-archived-102516/jeffrey-live-that-${v}.words.txt`],
      ...CHOIR_VOICES.map((cv) => [cv, `${CHOIR}/${cv}-${v}.words.txt`]),
    ];
    for (const [name, p] of layers) {
      if (!existsSync(p)) { layerSkip++; continue; }
      const w = readFileSync(p, "utf8").trim().split(/\n+/)
        .map((l) => l.split(/\s+/)[0]);
      let matched = 0;
      const n = Math.max(ref.length, w.length);
      for (let i = 0; i < n; i++) {
        const a = ref[i] ?? "", b = w[i] ?? "";
        const ok = norm(a) === norm(b) || norm(a).replace(/-/g, "") === norm(b);
        if (ok) matched++;
      }
      totalWords  += n;
      alignedWords += matched;
      const tag = (matched === n && w.length === ref.length) ? "✓ UNISON" : "△ skew";
      console.log(`  ${name.padEnd(11)} ${matched}/${n}  ${tag}`);
      if (tag.startsWith("✓")) layerOK++;
    }
  }
  console.log(`\n[align] ${layerOK} layers in full unison · ${alignedWords}/${totalWords} word-positions aligned`);
}
alignmentCheck();
console.log("─".repeat(80));

const results = [];
for (const w of windows) {
  const slice = `${WORK}/win-${w.idx}.wav`;
  // 16k mono WAV — whisper.cpp wants exactly this.
  const ff = spawnSync("ffmpeg", [
    "-y", "-ss", String(w.startS), "-t", String(w.durS),
    "-i", MASTER, "-ar", "16000", "-ac", "1", "-c:a", "pcm_s16le",
    slice,
  ], { stdio: "pipe" });
  if (ff.status !== 0) {
    console.error(`[verify] ffmpeg failed @ window ${w.idx}: ${ff.stderr.toString().slice(-200)}`);
    continue;
  }
  const wh = spawnSync("whisper-cli", [
    "-m", MODEL, "-f", slice,
    "-l", "en", "-nt", "-otxt", "-of", `${WORK}/win-${w.idx}`,
    "-t", "4", "--no-prints",
  ], { stdio: "pipe" });
  if (wh.status !== 0) {
    console.error(`[verify] whisper failed @ window ${w.idx}: ${wh.stderr.toString().slice(-200)}`);
    continue;
  }
  const txtPath = `${WORK}/win-${w.idx}.txt`;
  const got = existsSync(txtPath) ? readFileSync(txtPath, "utf8").trim() : "";
  const ratio = wordMatchRatio(w.expect, got);
  const verdict = ratio >= 0.85 ? "OK     "
              : ratio >= 0.60 ? "PARTIAL"
              : ratio >= 0.30 ? "WEAK   "
              :                 "MISSING";
  results.push({ ...w, got, ratio, verdict });
  console.log(`win ${w.idx}  ${w.startS.toFixed(2).padStart(6)}s  [${w.variant.padEnd(7)}]  ${verdict}  ${(ratio * 100).toFixed(0).padStart(3)}%`);
  console.log(`         expect : ${w.expect}`);
  console.log(`         heard  : ${got || "(silence)"}`);
}

console.log("─".repeat(80));
const ok = results.filter((r) => r.ratio >= 0.85).length;
const partial = results.filter((r) => r.ratio >= 0.60 && r.ratio < 0.85).length;
const weak = results.filter((r) => r.ratio >= 0.30 && r.ratio < 0.60).length;
const missing = results.filter((r) => r.ratio < 0.30).length;
console.log(`SUMMARY  ok=${ok}  partial=${partial}  weak=${weak}  missing=${missing}  (of ${results.length})`);
const avg = results.reduce((s, r) => s + r.ratio, 0) / Math.max(1, results.length);
console.log(`AVG word-match: ${(avg * 100).toFixed(1)}%`);
process.exit(avg >= 0.60 ? 0 : 2);
