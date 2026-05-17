#!/usr/bin/env node
// sing.mjs — produce an actually-sung jeffrey-pvc vocal stem for the
// dance lane, by mirroring the amazing-grace big-pictures pipeline:
//
//   1. say.mjs --timestamps    → vocal.mp3 + alignment.json
//   2. alignment → words.json
//   3. score-pitch.mjs (WORLD) → pitched.mp3   (f0 replaced from .np)
//
// The trance vocal sits in midi 57–69 (jeffrey baritone + upper iconic
// range) so WORLD's pitch-shift reads as singing, not artifacted speech.
//
// Outputs the sung stem at --out (default ~/Desktop/trance-sung-vox.mp3),
// suitable as input to recap/bin/trance.mjs --vocal-stem.
//
// Usage:
//   node pop/dance/bin/sing.mjs                          # default settings
//   node pop/dance/bin/sing.mjs --transpose 0 --out ~/Desktop/vox.mp3
//   node pop/dance/bin/sing.mjs --score pop/dance/trance-hook.np

import {
  writeFileSync, readFileSync, existsSync, mkdirSync, copyFileSync,
} from "node:fs";
import { resolve, dirname } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  }
}
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const HERE = dirname(fileURLToPath(import.meta.url));
const DANCE = resolve(HERE, "..");
const POP = resolve(DANCE, "..");

const SCORE_PATH = expandHome(flags.score) || `${DANCE}/trance-hook.np`;
// 64 items across 2 shuffled verses. Full alphabet A-Z, numbers 1-10,
// 15 thematic words (i, love, computer, death, driving, sex, infinite,
// silliness, talking, fighting, calling, singing, playing, writing,
// crying), plus long numbers 30456 + 8888.
// Order matches trance-hook.np strictly (word index → score position).
const LYRIC      = flags.lyric || [
  // Verse 1 — "computer" appears 3× (bumped from 1×)
  "i.","A.","love.","computer.","computer.","C.","talking.","death.",   // Am  (B → computer, then bonus computer)
  "D.","fighting.","E.","F.","driving.","calling.","G.","2.",            // F
  "H.","sex.","singing.","J.","playing.","K.","infinite.","computer.",   // C   (L → computer)
  "writing.","M.","3.","silliness.","crying.","N.","30456.","O.",        // G
  // Verse 2 — "computer" appears 3× (bumped from 1×)
  "silliness.","30456.","8888.","crying.","writing.","playing.","singing.","calling.",  // Am
  "fighting.","talking.","P.","Q.","R.","S.","T.","U.",                                // F
  "V.","computer.","X.","Y.","Z.","4.","5.","6.",                                       // C   (W → computer)
  "7.","8.","computer.","10.","i.","love.","computer.","death.",                        // G   (9 → computer)
].join(" ");
const TRANSPOSE  = Number(flags.transpose ?? 0);
const BPM        = Number(flags.bpm ?? 92); // score-stretch tempo
const MAX_STRETCH = Number(flags["max-stretch"] ?? 8.0);
const OUT_PATH   = expandHome(flags.out) || `${homedir()}/Desktop/trance-sung-vox.mp3`;
const FORCE      = !!flags.force;

const OUT_DIR    = `${DANCE}/out`;
mkdirSync(OUT_DIR, { recursive: true });

const LYRIC_TXT  = `${OUT_DIR}/trance-hook.txt`;
const VOCAL_MP3  = `${OUT_DIR}/trance-hook-vocal.mp3`;
const VOCAL_ALIGN = `${VOCAL_MP3}.alignment.json`;
const WORDS_JSON = `${OUT_DIR}/trance-hook-vocal-words.json`;
const PITCHED_MP3 = `${OUT_DIR}/trance-hook-pitched.mp3`;
const STRETCHED_MP3 = `${OUT_DIR}/trance-hook-stretched.mp3`;
const STRETCHED_RAP_MP3 = `${OUT_DIR}/trance-hook-stretched-rap.mp3`;
const PITCHED_ALIGN = `${OUT_DIR}/trance-hook-pitched-alignment.json`;
const LAYERED_MP3 = `${OUT_DIR}/trance-hook-layered.mp3`;
const TMP_DIR = `${OUT_DIR}/.tmp`;
mkdirSync(TMP_DIR, { recursive: true });

if (!existsSync(SCORE_PATH)) { console.error(`✗ score missing: ${SCORE_PATH}`); process.exit(1); }

// Write the lyric to a file (say.mjs reads from a path).
writeFileSync(LYRIC_TXT, LYRIC + "\n");

function step(name, fn) {
  const t0 = Date.now();
  process.stdout.write(`▸ ${name}\n`);
  fn();
  console.log(`  ${((Date.now() - t0) / 1000).toFixed(1)}s`);
}

function run(cmd, args, cwd = POP) {
  const r = spawnSync(cmd, args, { cwd, stdio: ["ignore", "inherit", "inherit"] });
  if (r.status !== 0) {
    console.error(`✗ ${cmd} ${args.join(" ")} failed (exit ${r.status})`);
    process.exit(1);
  }
}

console.log(`━━━ dance · sing ━━━ score: ${SCORE_PATH.replace(POP + "/", "")} · transpose ${TRANSPOSE >= 0 ? "+" : ""}${TRANSPOSE}st\n`);

// ── 1. say (jeffrey-pvc TTS with /with-timestamps alignment) ─────────
step("1 · say (TTS — stronger performance settings)", () => {
  // Settings tuned for intelligibility — higher stability locks the
  // jeffrey identity (memory: ≥0.5 to avoid drift) and the lower style
  // value gives clearer enunciation of consonants so words like
  // "computer" survive the supersaw wall in the bed. Tradeoff: a touch
  // less performative expression vs the previous style=0.6 take.
  const baseArgs = ["bin/say.mjs", LYRIC_TXT.replace(POP + "/", ""),
                    "--stability", "0.65", "--similarity", "0.92", "--style", "0.40",
                    "--out", VOCAL_MP3.replace(POP + "/", "")];
  const argsWithTs = [...baseArgs, "--timestamps"];
  if (FORCE) { baseArgs.push("--force"); argsWithTs.push("--force"); }
  const r = spawnSync("node", argsWithTs, { cwd: POP, stdio: ["ignore", "inherit", "pipe"] });
  if (r.status === 0) return;
  console.log("  ↪ falling back to plain TTS + whisper alignment");
  run("node", baseArgs);
});

// ── 2. align → words.json ────────────────────────────────────────────
// Letters/numbers content fools whisper — it transcribes "A. B. C." as
// one mega-word, which breaks per-word slicing downstream. We try
// (1) ElevenLabs timestamps, (2) whisper, (3) uniform-spacing fallback
// over ffprobe-measured duration. Whichever produces enough words to
// match the lyric wins.
const lyricWords = LYRIC.split(/\s+/)
  .map((w) => w.replace(/[^a-zA-Z0-9]/g, ""))
  .filter(Boolean);
const expectedWords = lyricWords.length;

step(`2 · align (target: ${expectedWords} words from lyric)`, () => {
  // Try 1: ElevenLabs /with-timestamps
  if (existsSync(VOCAL_ALIGN)) {
    const a = JSON.parse(readFileSync(VOCAL_ALIGN, "utf8"));
    if (Array.isArray(a.words) && a.words.length >= expectedWords * 0.7) {
      writeFileSync(WORDS_JSON, JSON.stringify(a.words, null, 0));
      console.log(`  ✓ ${a.words.length} words from ElevenLabs (exact)`);
      return;
    }
  }
  // Try 2: whisper
  const wargs = ["bin/align.mjs", VOCAL_MP3.replace(POP + "/", "")];
  if (FORCE) wargs.push("--force");
  const wr = spawnSync("node", wargs, { cwd: POP, stdio: ["ignore", "pipe", "pipe"] });
  if (wr.status === 0 && existsSync(WORDS_JSON)) {
    const ww = JSON.parse(readFileSync(WORDS_JSON, "utf8"));
    if (ww.length >= expectedWords * 0.7) {
      console.log(`  ✓ ${ww.length} words from whisper`);
      return;
    }
    console.log(`  ⚠ whisper found ${ww.length} words; need ~${expectedWords} → uniform spacing`);
  }
  // Try 3: uniform spacing across ffprobe-measured duration
  const probe = spawnSync("ffprobe", [
    "-v", "error", "-show_entries", "format=duration",
    "-of", "default=noprint_wrappers=1:nokey=1", VOCAL_MP3,
  ], { encoding: "utf8" });
  const durSec = parseFloat(probe.stdout.trim()) || 0;
  if (durSec <= 0) {
    console.error(`✗ ffprobe failed to read duration; cannot synthesize alignment`);
    process.exit(1);
  }
  const durMs = durSec * 1000;
  const span = durMs / expectedWords;
  const synth = lyricWords.map((w, i) => ({
    text: w,
    fromMs: Math.floor(i * span),
    toMs: Math.floor((i + 1) * span),
  }));
  writeFileSync(WORDS_JSON, JSON.stringify(synth, null, 0));
  console.log(`  ✓ synthesized ${synth.length} uniform-spaced word windows over ${durSec.toFixed(2)}s (${(span / 1000).toFixed(2)}s each)`);
});

// ── 3. score-pitch (WORLD f0 replacement → sung) ────────────────────
step(`3 · score-pitch (WORLD · transpose ${TRANSPOSE >= 0 ? "+" : ""}${TRANSPOSE}st · vibrato)`, () => {
  run("node", [
    "bin/score-pitch.mjs",
    "--slug", "trance-hook",
    "--section", "all",
    "--score", SCORE_PATH,
    "--vocal", VOCAL_MP3,
    "--words", WORDS_JSON,
    "--transpose", String(TRANSPOSE),
    "--vibrato-hz", "5.2",
    "--vibrato-cents", "18",
    "--out", PITCHED_MP3,
  ]);
});

// ── 4. score-stretch (rubberband per-word, formant-preserving) ───────
// This is what HOLDS the notes — each word's source duration is
// stretched to its target beats from the .np. Sustained notes (*4, *5)
// actually hold; melisma stays inside the same word window.
step(`4 · score-stretch (rubberband · ${BPM} BPM · max ${MAX_STRETCH}×)`, () => {
  run("node", [
    "bin/score-stretch.mjs",
    "--slug", "trance-hook",
    "--section", "all",
    "--score", SCORE_PATH,
    "--in", PITCHED_MP3,
    "--alignment", PITCHED_ALIGN,
    "--bpm", String(BPM),
    "--max-stretch", String(MAX_STRETCH),
    "--out", STRETCHED_MP3,
  ]);
});

// 4b: a SECOND stretch at the bed's tempo (138 BPM) so the rap layer
// is actually beat-locked — each word lands on a beat rather than
// drifting through whatever the rubberband free-time tile happens to do.
step("4b · score-stretch RAP (115 BPM — slower beat-locked rap)", () => {
  run("node", [
    "bin/score-stretch.mjs",
    "--slug", "trance-hook",
    "--section", "all",
    "--score", SCORE_PATH,
    "--in", PITCHED_MP3,
    "--alignment", PITCHED_ALIGN,
    "--bpm", "115",
    "--max-stretch", "5.0",
    "--out", STRETCHED_RAP_MP3,
  ]);
});

// ── 5. layer: many-jeffreys-racing harmony stack ─────────────────────
// Take the stretched stem and stack faster + harmony-shifted copies so
// it sounds like multiple jeffreys racing against each other inside
// the same bar. Each layer at lower gain so the main vocal stays lead.
//   main     ×1.0 time, +0 st   (gain 1.0)   the lead voice
//   fast     ×0.5 time, +0 st   (gain 0.40)  same notes, twice the speed
//   faster   ×0.33 time, +12 st (gain 0.28)  octave up, 3× speed
//   low      ×1.0 time, −7 st   (gain 0.32)  perfect fifth below
//   high     ×1.0 time, +12 st  (gain 0.30)  octave above
// All layers loop within the main stem's duration so they cascade.
step("5 · layer (many-jeffreys: pitched + raw + rap + harmonies)", () => {
  // Three source wavs:
  //   baseWav    = stretched + WORLD-pitched stem (sung)
  //   rawWav     = original TTS, no pitching, no stretch
  //   whisperWav = close-mic whisper treatment of rawWav (proximity
  //                effect + heavy compression + brightness cut + tiny
  //                room reverb → intimate ASMR-adjacent character)
  const baseWav    = `${TMP_DIR}/base.wav`;
  const rawWav     = `${TMP_DIR}/raw.wav`;
  const rapWav     = `${TMP_DIR}/rap.wav`;
  const whisperWav = `${TMP_DIR}/whisper.wav`;
  // Force 48 kHz on every decode — /api/say returns 44.1 kHz, intermediate
  // tools then process at that rate and trance.mjs resamples at decode.
  // Resampling at every hop adds artifacts; pinning 48 k throughout fixes it.
  run("ffmpeg", ["-hide_banner","-y","-loglevel","error","-i",STRETCHED_MP3,"-ar","48000","-c:a","pcm_s16le",baseWav]);
  run("ffmpeg", ["-hide_banner","-y","-loglevel","error","-i",STRETCHED_RAP_MP3,"-ar","48000","-c:a","pcm_s16le",rapWav]);
  run("ffmpeg", ["-hide_banner","-y","-loglevel","error","-i",VOCAL_MP3,"-ar","48000","-c:a","pcm_s16le",rawWav]);

  // Slice layer: take the rap layer and gate it with an 8th-note tremolo
  // so each word gets sliced into rhythmic chops. 138 BPM means 8th note
  // ≈ 0.217 s → tremolo at ~4.6 Hz. Heavy depth = full gating.
  const sliceWav = `${TMP_DIR}/slice.wav`;
  run("ffmpeg", [
    "-hide_banner","-y","-loglevel","error",
    "-i", STRETCHED_RAP_MP3,
    "-af", "tremolo=f=4.6:d=0.85,highpass=f=200,equalizer=f=1500:t=q:w=1.5:g=3",
    "-ar","48000","-c:a","pcm_s16le", sliceWav,
  ]);

  // Computer-stamp layer: extract ONLY the "computer" word windows
  // from the stretched stem, gate everything else, and auto-pan the
  // result. User wants jeffrey's "computer" littered throughout the
  // mix bouncing left-right like a stamp, NOT a master sweep on
  // every voice. Compute target word timings from the .np score.
  const scoreText = readFileSync(SCORE_PATH, "utf8");
  const scoreWords = [];
  let cumBeats = 0;
  let cur = null;
  for (const raw of scoreText.split("\n")) {
    const ln = raw.trim();
    if (!ln || ln.startsWith("#")) continue;
    if (/^[a-z]+(?: \d+)?$/i.test(ln)) continue; // section header
    for (const tok of ln.split(/\s+/)) {
      const m = tok.match(/^([A-Ga-g][#b]?-?\d):(.+?)(?:\*(\d+(?:\.\d+)?))?$/);
      if (!m) continue;
      const text = m[2];
      const beats = Number(m[3] || 1);
      if (text.startsWith("-") && cur) {
        cur.beats += beats;
      } else {
        if (cur) scoreWords.push(cur);
        cur = { text: text.replace(/-$/, "").toLowerCase(), startBeats: cumBeats, beats };
      }
      cumBeats += beats;
    }
  }
  if (cur) scoreWords.push(cur);

  // Find computer windows in seconds (stretched at BPM).
  const beatSec = 60 / BPM;
  const computerWindows = scoreWords
    .filter((w) => w.text.startsWith("com"))
    .map((w) => ({ startSec: w.startBeats * beatSec, endSec: (w.startBeats + w.beats) * beatSec }));
  console.log(`  computer stamps: ${computerWindows.length} × {${computerWindows.map((w) => w.startSec.toFixed(1) + "-" + w.endSec.toFixed(1) + "s").join(", ")}}`);

  // Build gate expression: 1.6x volume in computer windows, 0 outside.
  // Slight fade ±50ms via 'between' boundaries.
  const gateExpr = computerWindows
    .map((w) => `between(t,${w.startSec.toFixed(3)},${w.endSec.toFixed(3)})`)
    .join("+");
  const stampWav = `${TMP_DIR}/computer-stamp.wav`;
  if (gateExpr) {
    run("ffmpeg", [
      "-hide_banner","-y","-loglevel","error",
      "-i", STRETCHED_MP3,
      "-af",
        `volume=eval=frame:volume='1.6*(${gateExpr})',` +
        `pan=stereo|c0=c0|c1=c0,` +
        `apulsator=hz=0.7:width=0.95`,
      "-ar","48000","-c:a","pcm_s16le", stampWav,
    ]);
  }

  // Whisper filter chain:
  //   acompressor   — crush dynamics so soft phonemes come up
  //   highpass 120  — remove distant rumble
  //   lowpass 4500  — soften any harsh top end (whispers are dark)
  //   eq +5 @ 100   — proximity bass boost (the "close to the mic" warmth)
  //   eq -4 @ 2.5k  — pull mid harshness, leave breathy quality
  //   eq +3 @ 8k    — "air band" — keeps the breath audible
  //   aecho         — tight 30 ms close-room reflection
  //   volume +6 dB  — make the small dynamics actually present
  const whisperFilter =
    "acompressor=threshold=-30dB:ratio=6:attack=2:release=80:makeup=5:knee=4," +
    "highpass=f=120,lowpass=f=4500," +
    "equalizer=f=100:t=q:w=1.5:g=5," +
    "equalizer=f=2500:t=q:w=1.0:g=-4," +
    "equalizer=f=8000:t=q:w=1.0:g=3," +
    "aecho=0.6:0.3:30:0.18," +
    "volume=2.0";
  run("ffmpeg", [
    "-hide_banner","-y","-loglevel","error",
    "-i", rawWav, "-af", whisperFilter,
    "-ar","48000","-c:a","pcm_s16le", whisperWav,
  ]);

  // Six layers total:
  //   fast/faster — racing pitched copies
  //   low/high    — pitched harmonies
  //   raw         — UNALTERED jeffrey TTS in the mix (the user wanted this)
  //   rap         — sped-up TTS, word-per-beat delivery
  const layers = [
    { name: "fast",    time: 0.50, pitch:   0, gain: 0.22, src: baseWav    },
    { name: "faster",  time: 0.33, pitch:  12, gain: 0.16, src: baseWav    },
    { name: "low",     time: 1.00, pitch:  -7, gain: 0.20, src: baseWav    },
    { name: "high",    time: 1.00, pitch:  12, gain: 0.18, src: baseWav    },
    { name: "raw",     time: 1.00, pitch:   0, gain: 0.32, src: rawWav     },
    // Rap layer is the second score-stretch pass at 138 BPM. Already
    // tempo-locked, so time=1.0 (no rubberband retime); pitch=0 to keep
    // the WORLD-applied melody on every word.
    { name: "rap",     time: 1.00, pitch:   0, gain: 0.32, src: rapWav     },
    // Close-mic whisper: intimate, breathy. Slightly pitched down so
    // jeffrey sounds confessional rather than declamatory.
    { name: "whisper", time: 1.00, pitch:  -2, gain: 0.40, src: whisperWav },
    // Slice: 8th-note-gated chop of the rap layer — adds rhythmic
    // staccato vocal movement.
    { name: "slice",   time: 1.00, pitch:   0, gain: 0.28, src: sliceWav   },
  ];

  // Rubberband each layer → wav from its specific source.
  const layerWavs = [];
  for (const l of layers) {
    const wav = `${TMP_DIR}/${l.name}.wav`;
    const args = ["-t", String(l.time), "-p", String(l.pitch), "--formant", l.src, wav];
    run("rubberband", args);
    layerWavs.push({ ...l, wav });
  }

  // Mix base + layers + computer-stamp via ffmpeg. Loop the
  // time-stretched layers so they fill the main stem's duration.
  // All inputs upmix to stereo so the panned stamp survives amix.
  const inputs = ["-i", STRETCHED_MP3];
  for (const l of layerWavs) inputs.push("-i", l.wav);
  const hasStamp = !!gateExpr && existsSync(stampWav);
  if (hasStamp) inputs.push("-i", stampWav);
  // Get main duration via ffprobe so aloop can target it.
  const probe = spawnSync("ffprobe", [
    "-v","error","-show_entries","format=duration",
    "-of","default=noprint_wrappers=1:nokey=1", STRETCHED_MP3,
  ], { encoding: "utf8" });
  const mainSec = parseFloat(probe.stdout.trim()) || 21;
  const mainSamples = Math.floor(mainSec * 48000);

  // Per-layer build-up envelope: most layers fade in progressively
  // across the first half of the stem so the opening is lean and the
  // mix grows richer toward the middle of the track.
  // Format: {start: seconds_when_fade_begins, dur: seconds_of_fade}
  const fadeMap = {
    fast:    { start: 22, dur: 8 },
    faster:  { start: 28, dur: 8 },
    low:     { start: 30, dur: 10 },
    high:    { start: 34, dur: 10 },
    raw:     { start: 12, dur: 6 },
    rap:     { start: 24, dur: 8 },
    whisper: { start: 16, dur: 6 },
    slice:   { start: 30, dur: 10 },
  };

  const parts = [];
  // Main stem: tiny fade-in just to avoid a click; full by ~2s.
  parts.push(`[0:a]pan=stereo|c0=c0|c1=c0,volume=eval=frame:volume='min(max(t/2,0),1)'[m]`);
  layerWavs.forEach((l, i) => {
    const idx = i + 1;
    const fm = fadeMap[l.name] || { start: 0, dur: 1 };
    // Volume = layer.gain × ramp(0..1) over [fm.start, fm.start+fm.dur]
    const volExpr = `${l.gain}*min(max((t-${fm.start})/${fm.dur},0),1)`;
    if (l.time < 1.0) {
      parts.push(`[${idx}:a]pan=stereo|c0=c0|c1=c0,volume=eval=frame:volume='${volExpr}',aloop=loop=-1:size=2147483647,atrim=duration=${mainSec.toFixed(3)},asetpts=N/SR/TB[l${i}]`);
    } else {
      parts.push(`[${idx}:a]pan=stereo|c0=c0|c1=c0,volume=eval=frame:volume='${volExpr}'[l${i}]`);
    }
  });
  const labels = ["m", ...layerWavs.map((_, i) => `l${i}`)];
  let totalInputs = layers.length + 1;
  if (hasStamp) {
    // Stamp comes in around break1 — 14s fade start.
    const stampIdx = layerWavs.length + 1;
    parts.push(`[${stampIdx}:a]volume=eval=frame:volume='0.85*min(max((t-14)/6,0),1)',atrim=duration=${mainSec.toFixed(3)}[stamp]`);
    labels.push("stamp");
    totalInputs++;
  }
  const mixInputs = labels.map((n) => `[${n}]`).join("");
  parts.push(`${mixInputs}amix=inputs=${totalInputs}:duration=longest:dropout_transition=0:normalize=0,atrim=duration=${mainSec.toFixed(3)}[out]`);
  const filter = parts.join(";");
  void mainSamples;

  run("ffmpeg", [
    "-hide_banner","-y","-loglevel","error",
    ...inputs,
    "-filter_complex", filter,
    "-map", "[out]",
    "-c:a","libmp3lame","-q:a","3",
    LAYERED_MP3,
  ]);
});

// ── output ───────────────────────────────────────────────────────────
mkdirSync(dirname(OUT_PATH), { recursive: true });
copyFileSync(LAYERED_MP3, OUT_PATH);
const sz = (readFileSync(OUT_PATH).length / 1024).toFixed(0);
console.log(`\n✓ ${OUT_PATH} (${sz} KB · sung jeffrey-pvc stem)`);
