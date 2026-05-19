#!/usr/bin/env node
// sing-penta.mjs — jeffrey-pvc vocal-HARMONY COUNTERPOINT stem for the
// trancepenta dance track. A worktree-local sibling of sing.mjs: same
// proven chain (say → align → score-pitch[WORLD] → score-stretch[
// rubberband] → many-jeffreys harmony stack) but tuned for a SHORT
// wordless vocalise that sits UNDER the instrumental as a complementary
// answer-phrase, not a lead.
//
//   1. say.mjs --timestamps         → trancepenta-hum-vocal.mp3 + alignment
//   2. alignment / whisper / uniform → words.json
//   3. score-pitch.mjs (WORLD)      → pitched.mp3 (G-dorian chord tones)
//   4. score-stretch.mjs (rubberband) @ 126 BPM → held notes
//   5. layer: restrained harmony stack (root + fifth-below + octave-up
//      shimmer + one soft fast echo) — quiet TEXTURE, not a lead.
//
// Differences vs sing.mjs (all so this reads as a counterpoint bed):
//   · SCORE   = pop/dance/trancepenta-hum.np  (16 wordless words)
//   · LYRIC   = the vocalise tokens, period-terminated, .np word order
//   · TRANSPOSE default 0 (the .np already sits baritone G3-F4 = midi
//     55-65, jeffrey's warm low range — no shift needed)
//   · BPM     = 126 (trancepenta tempo) for score-stretch
//   · harmony stack is smaller + softer (no rap/slice/whisper/stamp;
//     this is an undercurrent, the bed/lead stay in front)
//   · OUT     = pop/dance/out/trancepenta-vocal.mp3 (gitignored)
//
// jeffrey-pvc settings: stability 0.62 (>=0.5 keeps jeffrey identity —
// memory: jeffrey_pvc_settings), similarity 0.9, modest style 0.30.
//
// Usage:
//   node pop/dance/bin/sing-penta.mjs
//   node pop/dance/bin/sing-penta.mjs --force --bpm 126

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

const SCORE_PATH = expandHome(flags.score) || `${DANCE}/trancepenta-hum.np`;
// 16 wordless vocalise tokens — ORDER strictly matches the .np word
// grouping in trancepenta-hum.np (continuation `-` syllables merge into
// the preceding word). hmm/mmm/ooh/doo/la/dum/hoo = toots & humms;
// wee/oo = the two bright "whistle" lifts.
const LYRIC = flags.lyric || [
  // CALL                ANSWER
  "hmm.","mmm.","ooh.","doo.",  "la.","wee.","hoo.","dum.",
  // CALL echo            ANSWER (final settle)
  "mmm.","ooh.","doo.","hmm.",  "la.","oo.","hoo.","mmm.",
].join(" ");
const TRANSPOSE  = Number(flags.transpose ?? 0);
const BPM        = Number(flags.bpm ?? 126); // trancepenta tempo
const MAX_STRETCH = Number(flags["max-stretch"] ?? 8.0);
const OUT_PATH   = expandHome(flags.out) || `${DANCE}/out/trancepenta-vocal.mp3`;
const FORCE      = !!flags.force;

const OUT_DIR    = `${DANCE}/out`;
mkdirSync(OUT_DIR, { recursive: true });

const LYRIC_TXT  = `${OUT_DIR}/trancepenta-hum.txt`;
const VOCAL_MP3  = `${OUT_DIR}/trancepenta-hum-vocal.mp3`;
const VOCAL_ALIGN = `${VOCAL_MP3}.alignment.json`;
const WORDS_JSON = `${OUT_DIR}/trancepenta-hum-vocal-words.json`;
const PITCHED_MP3 = `${OUT_DIR}/trancepenta-hum-pitched.mp3`;
const STRETCHED_MP3 = `${OUT_DIR}/trancepenta-hum-stretched.mp3`;
const PITCHED_ALIGN = `${OUT_DIR}/trancepenta-hum-pitched-alignment.json`;
const LAYERED_MP3 = `${OUT_DIR}/trancepenta-hum-layered.mp3`;
const TMP_DIR = `${OUT_DIR}/.tmp-penta`;
mkdirSync(TMP_DIR, { recursive: true });

if (!existsSync(SCORE_PATH)) { console.error(`✗ score missing: ${SCORE_PATH}`); process.exit(1); }

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

console.log(`━━━ dance · sing-penta (counterpoint) ━━━ score: ${SCORE_PATH.replace(POP + "/", "")} · transpose ${TRANSPOSE >= 0 ? "+" : ""}${TRANSPOSE}st · ${BPM} BPM\n`);

// ── 1. say (jeffrey-pvc TTS with /with-timestamps alignment) ─────────
step("1 · say (jeffrey-pvc — gentle vocalise settings)", () => {
  // stability 0.62 keeps the jeffrey clone identity (>=0.5 per memory:
  // jeffrey_pvc_settings); similarity 0.90; modest style 0.30 — for a
  // soft hummed vocalise we want warmth, not declamatory consonants.
  const baseArgs = ["bin/say.mjs", LYRIC_TXT.replace(POP + "/", ""),
                    "--stability", "0.62", "--similarity", "0.90", "--style", "0.30",
                    "--out", VOCAL_MP3.replace(POP + "/", "")];
  const argsWithTs = [...baseArgs, "--timestamps"];
  if (FORCE) { baseArgs.push("--force"); argsWithTs.push("--force"); }
  const r = spawnSync("node", argsWithTs, { cwd: POP, stdio: ["ignore", "inherit", "pipe"] });
  if (r.status === 0) return;
  console.log("  ↪ /with-timestamps failed — falling back to plain TTS + whisper align");
  run("node", baseArgs);
});

// ── 2. align → words.json ────────────────────────────────────────────
// Wordless vocalise: whisper will mangle "hmm mmm ooh"; the uniform
// fallback (even split over ffprobe duration) is actually IDEAL here
// since the syllables are evenly metered anyway.
const lyricWords = LYRIC.split(/\s+/)
  .map((w) => w.replace(/[^a-zA-Z0-9]/g, ""))
  .filter(Boolean);
const expectedWords = lyricWords.length;

step(`2 · align (target: ${expectedWords} words from lyric)`, () => {
  // Try 1: ElevenLabs /with-timestamps (exact)
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
  // Try 3: uniform spacing across ffprobe-measured duration (ideal for
  // an evenly-metered wordless vocalise).
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
  console.log(`  ✓ synthesized ${synth.length} uniform-spaced windows over ${durSec.toFixed(2)}s (${(span / 1000).toFixed(2)}s each)`);
});

// ── 3. score-pitch (WORLD f0 replacement → sung G-dorian chord tones) ─
step(`3 · score-pitch (WORLD · transpose ${TRANSPOSE >= 0 ? "+" : ""}${TRANSPOSE}st · gentle vibrato)`, () => {
  run("node", [
    "bin/score-pitch.mjs",
    "--slug", "trancepenta-hum",
    "--section", "all",
    "--score", SCORE_PATH,
    "--vocal", VOCAL_MP3,
    "--words", WORDS_JSON,
    "--transpose", String(TRANSPOSE),
    // Slower, shallower vibrato than the lead — a calm hummed bed.
    "--vibrato-hz", "4.6",
    "--vibrato-cents", "12",
    "--out", PITCHED_MP3,
  ]);
});

// ── 4. score-stretch (rubberband per-word → held counterpoint notes) ─
step(`4 · score-stretch (rubberband · ${BPM} BPM · max ${MAX_STRETCH}×)`, () => {
  run("node", [
    "bin/score-stretch.mjs",
    "--slug", "trancepenta-hum",
    "--section", "all",
    "--score", SCORE_PATH,
    "--in", PITCHED_MP3,
    "--alignment", PITCHED_ALIGN,
    "--bpm", String(BPM),
    "--max-stretch", String(MAX_STRETCH),
    // Small crossfade so adjacent hummed notes glue legato (counterpoint
    // wants smooth connective tissue, not staccato word edges).
    "--overlap-ms", "45",
    "--out", STRETCHED_MP3,
  ]);
});

// ── 5. layer: restrained many-jeffreys harmony (a TEXTURE, not a lead)─
// The lead vocal stack in sing.mjs is dense (8 layers incl. rap/slice/
// whisper/stamp). This counterpoint must sit UNDER the bed, so we keep
// only a small consonant choir:
//   main   ×1.0 +0  st  (gain 1.00)  the hummed counterpoint
//   low    ×1.0 -12 st  (gain 0.34)  octave-below — body/warmth
//   high   ×1.0 +12 st  (gain 0.22)  octave-above — a faint "whistle"
//   fifth  ×1.0 +7  st  (gain 0.20)  a soft fifth — choir thickening
//   echo   ×0.5 +0  st  (gain 0.16)  one half-time copy = call/response
// (-12/+12/+7 are all G-dorian-consonant against the tonic chord tones.)
step("5 · layer (restrained jeffrey counterpoint choir)", () => {
  const baseWav = `${TMP_DIR}/base.wav`;
  run("ffmpeg", ["-hide_banner","-y","-loglevel","error","-i",STRETCHED_MP3,"-ar","48000","-c:a","pcm_s16le",baseWav]);

  const layers = [
    { name: "low",   time: 1.00, pitch: -12, gain: 0.34, src: baseWav },
    { name: "high",  time: 1.00, pitch:  12, gain: 0.22, src: baseWav },
    { name: "fifth", time: 1.00, pitch:   7, gain: 0.20, src: baseWav },
    { name: "echo",  time: 0.50, pitch:   0, gain: 0.16, src: baseWav },
  ];

  const layerWavs = [];
  for (const l of layers) {
    const wav = `${TMP_DIR}/${l.name}.wav`;
    run("rubberband", ["-t", String(l.time), "-p", String(l.pitch), "--formant", l.src, wav]);
    layerWavs.push({ ...l, wav });
  }

  const probe = spawnSync("ffprobe", [
    "-v","error","-show_entries","format=duration",
    "-of","default=noprint_wrappers=1:nokey=1", STRETCHED_MP3,
  ], { encoding: "utf8" });
  const mainSec = parseFloat(probe.stdout.trim()) || 30;

  const inputs = ["-i", STRETCHED_MP3];
  for (const l of layerWavs) inputs.push("-i", l.wav);

  const parts = [];
  // Main: short anti-click ramp.
  parts.push(`[0:a]pan=stereo|c0=c0|c1=c0,volume=eval=frame:volume='min(max(t/0.5,0),1)'[m]`);
  layerWavs.forEach((l, i) => {
    const idx = i + 1;
    // Gentle stereo spread: octave/fifth nudged off-center so the choir
    // has width without smearing the center of the mix.
    const pan = l.name === "high" ? "c0=0.7*c0|c1=1.0*c0"
              : l.name === "fifth" ? "c0=1.0*c0|c1=0.7*c0"
              : "c0=c0|c1=c0";
    if (l.time < 1.0) {
      parts.push(`[${idx}:a]pan=stereo|${pan},volume=${l.gain},aloop=loop=-1:size=2147483647,atrim=duration=${mainSec.toFixed(3)},asetpts=N/SR/TB[l${i}]`);
    } else {
      parts.push(`[${idx}:a]pan=stereo|${pan},volume=${l.gain}[l${i}]`);
    }
  });
  const labels = ["m", ...layerWavs.map((_, i) => `l${i}`)];
  const mixInputs = labels.map((n) => `[${n}]`).join("");
  parts.push(`${mixInputs}amix=inputs=${labels.length}:duration=longest:dropout_transition=0:normalize=0,atrim=duration=${mainSec.toFixed(3)}[out]`);
  const filter = parts.join(";");

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
const probe = spawnSync("ffprobe", ["-v","error","-show_entries","format=duration","-of","default=noprint_wrappers=1:nokey=1", OUT_PATH], { encoding: "utf8" });
console.log(`\n✓ ${OUT_PATH} (${sz} KB · ${parseFloat(probe.stdout.trim()).toFixed(1)}s · jeffrey-pvc counterpoint stem)`);
