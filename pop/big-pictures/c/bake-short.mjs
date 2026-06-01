#!/usr/bin/env node
// bake-short.mjs — the SHORT cut of "amazing grace (dance)", retracked
// from scratch (@jeffrey 2026-05-30).
//
// Design (a hard reset of the old --short remap):
//   • START FROM THE ORIGINAL SAMPLE — slice all 26 hymn words straight
//     out of amazing-vocal.mp3 (the 7.8 s jeffrey-pvc take) at their
//     natural word timings from amazing-vocal-words.json. KEEP THEM
//     SHORT — natural length, no rubberband stretching.
//   • BEATMAPPED — every word onset snaps to the 120 BPM beat grid
//     (beat = 0.5 s, bar = 2.0 s).
//   • ANCHOR TO THE FIRST BEAT — the bed's kick enters at bar 3 = 6.0 s
//     (bars 0-2 are a bells-only intro). The first word lands there.
//   • WITHIN A BAR OR TWO — each of the 4 hymn lines fills exactly 2
//     bars, then the WHOLE HYMN LOOPS ~6× across the 2-min cut.
//
// Pipeline:  build engine → render 300 s bed (trim to 120) →
//            slice 26 words → render beatmapped looped vocal →
//            mix bed + vocal → master chain → -SHORT-* outputs.
//
// Usage: node pop/big-pictures/c/bake-short.mjs [outDir]

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { existsSync, statSync, mkdirSync } from "node:fs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const POP  = resolve(REPO, "pop");

const outDir = process.argv[2] || `${homedir()}/Documents/Shelf/amazing-grace-dance`;
mkdirSync(outDir, { recursive: true });

const ENGINE   = `${HERE}/amazing-dance`;
const ENGINE_C = `${HERE}/amazing-dance.c`;
const BED_WAV   = `${outDir}/.amazing-dance-SHORT-bed.wav`;
const VOX_WAV   = `${outDir}/.amazing-dance-SHORT-vox.wav`;
const PRE_WAV   = `${outDir}/.amazing-dance-SHORT-pre.wav`;
const FINAL_WAV = `${outDir}/amazing-grace-dance-c-SHORT-MASTER.wav`;
const FINAL_MP3 = `${outDir}/amazing-grace-dance-c-SHORT.mp3`;
const PRE_BRIGHT_WAV = `${outDir}/amazing-grace-dance-c-SHORT-MASTER-preBright.wav`;

const PVC_SOURCE = resolve(POP, "big-pictures/out/amazing-vocal.mp3");
const WORD_DIR   = resolve(POP, "big-pictures/out/short-words");
mkdirSync(WORD_DIR, { recursive: true });

const run = (cmd, a, label, opts = {}) => {
  console.log(`\n[bake-short] ${label}`);
  const r = spawnSync(cmd, a, { stdio: "inherit", ...opts });
  if (r.status !== 0) { console.error(`[bake-short] FAILED: ${label}`); process.exit(1); }
};

// ── 28 hymn SYLLABLES — note + natural source span ───────────────────
// Notes come from amazing.np ("New Britain", G-major pentatonic, oct 3).
// `from`/`to` (ms) are the syllable's span in amazing-vocal.mp3 — word
// boundaries from amazing-vocal-words.json, except "amazing" which is
// split into its three syllables a-/-ma-/-zing (D3→G3→B3). Each syllable
// is sliced short, WORLD-locked to its single note, then stretched onto
// a beat. Names are unique (filename keys): a_l2/i1/was1/etc disambiguate
// repeats.
const L1 = [
  { name: "a",     note: "D3", from: 0,    to: 130  },   // a-     ┐
  { name: "ma",    note: "G3", from: 130,  to: 310  },   // -ma-   ├ "amazing"
  { name: "zing",  note: "B3", from: 310,  to: 453  },   // -zing  ┘
  { name: "grace", note: "G3", from: 488,  to: 766  },
  { name: "how",   note: "B3", from: 813,  to: 952  },
  { name: "sweet", note: "A3", from: 1010, to: 1300 },
  { name: "the",   note: "G3", from: 1358, to: 1440 },
  { name: "sound", note: "B3", from: 1486, to: 1800 },
];
const L2 = [
  { name: "that",   note: "D3", from: 1846, to: 1974 },
  { name: "saved",  note: "E3", from: 2020, to: 2322 },
  { name: "a_l2",   note: "D3", from: 2357, to: 2380 },
  { name: "wretch", note: "B3", from: 2426, to: 2659 },
  { name: "like",   note: "G3", from: 2717, to: 2891 },
  { name: "me",     note: "B3", from: 2949, to: 3251 },
];
const L3 = [
  { name: "i1",     note: "D3", from: 3402, to: 3518 },
  { name: "once",   note: "G3", from: 3611, to: 3820 },
  { name: "was1",   note: "D4", from: 3855, to: 3971 },
  { name: "lost",   note: "D4", from: 4040, to: 4435 },
  { name: "but1",   note: "B3", from: 4505, to: 4656 },
  { name: "now1",   note: "D4", from: 4714, to: 4923 },
  { name: "am",     note: "A3", from: 4992, to: 5097 },
  { name: "found",  note: "G3", from: 5178, to: 5503 },
];
const L4 = [
  { name: "was2",   note: "B3", from: 5573, to: 5689 },
  { name: "blind",  note: "D4", from: 5747, to: 6177 },
  { name: "but2",   note: "D4", from: 6455, to: 6629 },
  { name: "now2",   note: "B3", from: 6676, to: 6908 },
  { name: "i2",     note: "A3", from: 6989, to: 7024 },
  { name: "see",    note: "G3", from: 7140, to: 7802 },
];
const LINES = [L1, L2, L3, L4];
const WORDS = LINES.flat();

// Beat grid (defined early — the sung-stem stretch needs it).
const BEAT = 0.5;               // 120 BPM → 0.5 s/beat, 2.0 s/bar
const T0 = 6.0;                 // first kick (bar 3)
// LYRIC PACING — onset spacing between syllables + rest between lines.
// Raise SYL_BEATS to slow the words down: 1 = a syllable every beat
// (rushed), 1.5 = every dotted beat, 2 = every half-bar. Holds scale
// with it so syllables stay legato. @jeffrey "slow the lyrics down".
const SYL_BEATS = 1.5;                 // beats between syllable onsets
const LINE_GAP_BEATS = 2;              // extra rest between lines
const HOLD_BEATS = SYL_BEATS + 0.5;    // sustain past the next onset (legato)
const HOLD_BEATS_END = SYL_BEATS + 2;  // line-ending syllables ring longer

// ── slice each word — natural length, short ──────────────────────────
// PRE-ROLL 15 ms to catch the consonant onset, +60 ms tail, floored to
// 130 ms so ultra-short words (the 35 ms "i") are still audible. Tiny
// fades kill edge clicks. Cached by source mtime.
const PRE = 0.015, TAIL = 0.060, FLOOR = 0.130;
for (const w of WORDS) {
  w.file = resolve(WORD_DIR, `${w.name}.wav`);
  const start = Math.max(0, w.from / 1000 - PRE);
  const dur = Math.max(FLOOR, (w.to - w.from) / 1000 + PRE + TAIL);
  w.dur = dur;
  if (existsSync(w.file) && statSync(PVC_SOURCE).mtimeMs <= statSync(w.file).mtimeMs) continue;
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-ss", start.toFixed(3), "-t", dur.toFixed(3), "-i", PVC_SOURCE,
                 "-af", "afade=t=in:st=0:d=0.005,afade=t=out:st=" +
                        (dur - 0.03).toFixed(3) + ":d=0.03",
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", w.file],
      `slice ${w.name} (${start.toFixed(2)}s, ${dur.toFixed(2)}s)`);
}

// ── WORLD-autotune + stretch each word → sung, tuned ─────────────────
// Mirrors the mary-lamb / amazing.np pipeline. Each word slice is pitch-
// LOCKED to its melody note(s) with the WORLD vocoder (f0 replacement,
// formants preserved → still jeffrey), THEN time-stretched onto the beat
// grid. WORLD sets the ABSOLUTE target pitch directly, so no source
// base-pitch detection is needed — and unlike `rubberband --pitch` it
// REPLACES jeffrey's spoken wobble with the steady note, so it actually
// sounds autotuned. Notes are straight from amazing.np ("New Britain",
// G-major pentatonic, octave 3 = jeffrey's baritone → small/clean
// shifts). One note PER SYLLABLE (amazing is now 3 syllables a/ma/zing).
const ENDINGS = new Set(["sound", "me", "found", "see"]);
const SUNG_DIR = resolve(POP, "big-pictures/out/short-sung");
mkdirSync(SUNG_DIR, { recursive: true });
const WORLD_PY  = resolve(POP, ".venv/bin/python");
const PITCHSNAP = resolve(POP, "bin/pitchsnap_world.py");
const USE_WORLD = existsSync(WORLD_PY) && existsSync(PITCHSNAP);
if (!USE_WORLD)
  console.warn("[bake-short] WARNING: WORLD vocoder not found — words won't be pitch-locked");

for (const w of WORDS) {
  const notes = [w.note];                  // one note per syllable
  const holdBeats = ENDINGS.has(w.name) ? HOLD_BEATS_END : HOLD_BEATS;
  const targetDur = holdBeats * BEAT;
  w.holdSec = targetDur;
  const sung = resolve(SUNG_DIR, `${w.name}_${notes.join("-")}_d${targetDur.toFixed(2)}.wav`);
  const fresh = existsSync(sung) && statSync(w.file).mtimeMs <= statSync(sung).mtimeMs;
  if (!fresh) {
    // 1) WORLD f0-replacement → lock to the melody note(s)
    let src = w.file;                      // natural slice
    if (USE_WORLD) {
      const pitched = resolve(SUNG_DIR, `.${w.name}-pitched.wav`);
      console.log(`[bake-short] WORLD ${w.name} → ${notes.join(",")}`);
      const r = spawnSync(WORLD_PY, [PITCHSNAP, w.file, pitched,
        "--notes", notes.join(","), "--xfade-ms", "30", "--voicing-ramp-ms", "20"],
        { encoding: "utf8" });
      if (r.status === 0) src = pitched;
      else console.warn(`[bake-short] WORLD failed ${w.name}: ` +
        `${(r.stderr || "").trim().split("\n").pop()}`);
    }
    // 2) time-stretch the pitch-locked word onto its grid slot
    const ratio = targetDur / w.dur;
    run("rubberband", ["--time", ratio.toFixed(4), "--formant", "--crisp", "5", src, sung],
        `stretch ${w.name} ${ratio.toFixed(2)}× → ${targetDur.toFixed(2)}s`);
  }
  w.file = sung;                           // placement now uses the autotuned stem
}

// ── beatmap + loop ───────────────────────────────────────────────────
// Beat grid: 120 BPM → beat 0.5 s, bar 2.0 s. First word on the first
// kick (6.0 s). Each line = a 2-bar block (8 beats): one word per beat,
// trailing beats rest. Lines stack 4-in-a-row = an 8-bar hymn, then the
// whole hymn loops. Loop while the hymn still starts before the break
// (kick drops at bar 48 = 96 s); the final loop trails into the break.
// One hymn = every syllable spaced SYL_BEATS apart, with LINE_GAP_BEATS
// between lines (and a trailing gap before it repeats). Compute its
// length, then loop the whole hymn with a running cursor until the break.
const HYMN_BEATS =
  LINES.reduce((a, ln) => a + ln.length * SYL_BEATS, 0) +
  LINE_GAP_BEATS * LINES.length;
const HYMN = HYMN_BEATS * BEAT;
const BREAK_AT = 96.0;

const placements = [];           // { w, at }
for (let k = 0; T0 + k * HYMN < BREAK_AT; k++) {
  let cursor = T0 + k * HYMN;    // seconds
  LINES.forEach((line) => {
    line.forEach((w) => {
      placements.push({ w, at: +cursor.toFixed(3) });
      cursor += SYL_BEATS * BEAT;
    });
    cursor += LINE_GAP_BEATS * BEAT;
  });
}
console.log(`[bake-short] ${placements.length} word placements ` +
  `(${placements.length / WORDS.length} hymn loops), ` +
  `${T0}s → ${(placements[placements.length - 1].at).toFixed(1)}s`);

// How many times each distinct word is placed → asplit fan-out count.
const useCount = {};
for (const pl of placements) useCount[pl.w.name] = (useCount[pl.w.name] || 0) + 1;

// ── render the beatmapped looped vocal stem ──────────────────────────
// One input per distinct word; asplit into its use-count; adelay each
// copy to its absolute time; amix them all; light forward bus (close,
// dry-ish room + glue compression + presence EQ).
const TOTAL = 120.0;
const voxInputs = [];
WORDS.forEach((w, i) => { w.inputIdx = i; voxInputs.push("-i", w.file); });

const splitChains = WORDS.map((w) => {
  const n = useCount[w.name] || 0;
  if (n === 0) return null;
  // label outputs s_<name>_0 ... s_<name>_(n-1)
  const outs = Array.from({ length: n }, (_, j) => `[s_${w.name}_${j}]`).join("");
  return `[${w.inputIdx}:a]asplit=${n}${outs}`;
}).filter(Boolean);

// per-word running index so each placement grabs a fresh asplit copy
const copyIdx = {};
const placeChains = placements.map((pl, k) => {
  const j = (copyIdx[pl.w.name] = (copyIdx[pl.w.name] ?? -1) + 1);
  const ms = Math.round(pl.at * 1000);
  return `[s_${pl.w.name}_${j}]adelay=${ms}|${ms},` +
         `aresample=48000,aformat=channel_layouts=stereo,` +
         `atrim=duration=${TOTAL}[p${k}]`;
});
const mixIns = placements.map((_, k) => `[p${k}]`).join("");
const voxComplex = [
  ...splitChains,
  ...placeChains,
  `${mixIns}amix=inputs=${placements.length}:duration=longest:` +
    `dropout_transition=0:normalize=0[voxraw]`,
  // FORWARD BUS — keep the words close, LOUD & intelligible. The source
  // word slices are quiet (-13..-25 dB), so we drive them hard here:
  //   • highpass 110 — clear rumble
  //   • aecho out_gain 0.85 (NOT 0.18 — that was scaling the whole
  //     vocal down ~15 dB and burying it) — light, short, close room
  //   • compressor makeup 4 — level the words + push forward
  //   • +10 dB volume — bring the quiet PVC slices up to lead level
  //   • 3.4 kHz consonant lift for diction
  `[voxraw]highpass=f=110,` +
    `aecho=0.85:0.85:55|130:0.12|0.07,` +
    `acompressor=threshold=0.05:ratio=4:attack=5:release=160:makeup=4,` +
    `equalizer=f=3400:t=q:w=1.2:g=2.5,` +
    `volume=7dB,` +
    `atrim=duration=${TOTAL}[vox]`,
].join(";");

// ── 1 · build engine if stale ────────────────────────────────────────
if (!existsSync(ENGINE) || statSync(ENGINE_C).mtimeMs > statSync(ENGINE).mtimeMs)
  run("bash", [`${HERE}/build-dance.sh`], "build C engine");

// ── 2 · render bed (engine always makes 300 s; we trim to 120) ───────
run(ENGINE, ["--out", BED_WAV], "C engine render (loukeman bed)");

// ── 3 · render the vocal stem ────────────────────────────────────────
run("ffmpeg", ["-y", ...voxInputs, "-filter_complex", voxComplex,
               "-map", "[vox]", "-ar", "48000", "-c:a", "pcm_f32le", VOX_WAV],
    `render ${placements.length} beatmapped word hits`);

// ── 4 · mix bed + vocal ──────────────────────────────────────────────
// Bed sits a touch back so the lyric leads. Master fade: silent until
// 2 s, ramp to ~0.5 over 4 s, full by 24 s. Tail fade over the last 3 s.
const BED_GAIN = 0.58, VOX_GAIN = 1.6;
const mixComplex = [
  `[0:a]atrim=duration=${TOTAL},volume=${BED_GAIN}[bed]`,
  `[1:a]atrim=duration=${TOTAL},volume=${VOX_GAIN}[vx]`,
  `[bed][vx]amix=inputs=2:duration=longest:dropout_transition=0:normalize=0,` +
    `volume=eval=frame:volume='max(0\\,min(1\\,(t-2)/4))*` +
    `(0.5+0.5*sin(min(1\\,t/24)*PI/2))',` +
    `afade=t=out:st=${(TOTAL - 3).toFixed(1)}:d=3[out]`,
].join(";");
run("ffmpeg", ["-y", "-i", BED_WAV, "-i", VOX_WAV,
               "-filter_complex", mixComplex, "-map", "[out]",
               "-ar", "48000", "-c:a", "pcm_f32le", PRE_WAV],
    "mix bed + beatmapped vocal");

// pre-bright stash for A/B
run("ffmpeg", ["-y", "-i", PRE_WAV, "-ar", "44100", "-sample_fmt", "s16",
               PRE_BRIGHT_WAV], "pre-bright stash");

// ── 5 · master chain (same pop master as the longlong bake) ──────────
const brightChain = [
  "atrim=start=2", "asetpts=PTS-STARTPTS",
  "equalizer=f=80:t=q:w=1.0:g=1.2",
  "equalizer=f=200:t=q:w=1.0:g=-1.8",
  "equalizer=f=420:t=q:w=1.4:g=-1.4",
  "equalizer=f=900:t=q:w=1.6:g=-0.6",
  "equalizer=f=2400:t=q:w=1.8:g=-1.6",
  "equalizer=f=3400:t=q:w=1.0:g=1.8",
  "equalizer=f=5500:t=q:w=1.4:g=1.2",
  "equalizer=f=7200:t=q:w=2.0:g=-1.6",
  "highshelf=f=10000:g=1.8",
  "highshelf=f=14000:g=1.4",
  "acompressor=threshold=0.18:ratio=2.4:attack=30:release=260:makeup=1.5",
  "volume=1.8dB",
].join(",");
const masterComplex =
  `[0:a]${brightChain},acrossover=split=200:order=4th[lo][hi];` +
  `[lo]pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1[lo_mono];` +
  `[hi]extrastereo=m=1.20:c=true[hi_wide];` +
  `[lo_mono][hi_wide]amix=inputs=2:weights='1 1':normalize=0,` +
  `alimiter=limit=0.91:attack=3:release=150:level=disabled[out]`;
run("ffmpeg", ["-y", "-i", PRE_WAV, "-filter_complex", masterComplex,
               "-map", "[out]", "-ar", "44100", "-sample_fmt", "s16",
               FINAL_WAV], "brightening polish + mono-bass widen");
run("ffmpeg", ["-y", "-i", FINAL_WAV, "-codec:a", "libmp3lame", "-b:a", "320k",
               FINAL_MP3], "320k mp3");

spawnSync("open", ["-a", "QuickTime Player", FINAL_MP3]);
console.log(`\n[bake-short] done`);
console.log(`             master: ${FINAL_WAV}`);
console.log(`             mp3:    ${FINAL_MP3}`);
console.log(`             A/B:    ${PRE_BRIGHT_WAV}`);
