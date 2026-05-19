#!/usr/bin/env node
// sing-penta.mjs — jeffrey-pvc SINGING ALONG WITH THE LEAD for the
// trancepenta dance track. A worktree-local sibling of sing.mjs: same
// proven chain (say → align → score-pitch[WORLD] → score-stretch[
// rubberband] → soft warm harmony) but tuned to be a SUNG DOUBLE of
// the chill lead — much more STRETCHED / DRIPPIN (legato, gooey,
// long-held), replacing the old sparse wordless hum counterpoint.
//
//   1. say.mjs --timestamps         → trancepenta-vocal-raw.mp3 + align
//   2. alignment / whisper / uniform → words.json
//   3. score-pitch.mjs (WORLD)      → pitched.mp3 (G-dorian tonic-pole
//      chord tones tracking the lead's recurring contour; gentle, a
//      touch deeper vibrato so the long held notes shimmer)
//   4. score-stretch.mjs (rubberband) @ a SLOW bpm + very high
//      --max-stretch + big --overlap-ms → drawn-out gooey legato notes
//   5. layer: a SOFT warm double — one octave-below body + one quiet
//      fifth — NOT the dense many-jeffreys racing stack. A warm
//      co-lead, not a texture and not a wall.
//
// Differences vs sing.mjs (so this reads as a SUNG LEAD-DOUBLE):
//   · LYRIC   = pop/dance/trancepenta.txt (real words, AC trance idiom)
//   · SCORE   = pop/dance/trancepenta-sing.np (NOTES TRACK THE LEAD —
//     G-dorian tonic-pole arpeggio contour, the line the chill lead
//     keeps returning to through progressionAt(); singable G3-G4)
//   · TRANSPOSE default 0 (the .np already sits G3-G4 = midi 55-67,
//     jeffrey's warm baritone-tenor — no shift needed)
//   · BPM     = 100 for score-stretch (SLOWER than the 126 track tempo
//     → even longer absolute sustains = the "drippin" hold)
//   · MAX_STRETCH = 16 (vs 8) — the drips actually draw all the way out
//   · OVERLAP_MS  = 140 (vs 45) — generous legato so adjacent sung
//     notes glue gooey, no staccato word edges
//   · harmony stack = ONLY a soft octave-below + a quiet fifth (a warm
//     double), no rap/slice/whisper/stamp/racing copies
//   · OUT     = pop/dance/out/trancepenta-vocal.mp3 (gitignored)
//
// jeffrey-pvc settings: stability 0.64 (>=0.5 keeps jeffrey identity —
// memory: jeffrey_pvc_settings), similarity 0.90, style 0.25 (plain/
// tender — not declamatory), speed 0.9 (slightly slow, eased).
//
// Usage:
//   node pop/dance/bin/sing-penta.mjs
//   node pop/dance/bin/sing-penta.mjs --force --bpm 100

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

const SCORE_PATH = expandHome(flags.score) || `${DANCE}/trancepenta-sing.np`;
// The lyric is authored in pop/dance/trancepenta.txt (real words, AC
// trance idiom — a foggy-harbour / horse / digital-sea image). Read it
// and flatten to a single TTS line; the .np word ORDER matches it 1:1
// (continuation `-` syllables merge into the preceding word).
const LYRIC_SRC = expandHome(flags.lyric) || `${DANCE}/trancepenta.txt`;
function loadLyric(p) {
  if (!existsSync(p)) {
    console.error(`✗ lyric file missing: ${p}`); process.exit(1);
  }
  // Drop blank lines + comments + bare section headers; period-terminate
  // each line so /api/say phrases it as discrete sung words (matching
  // the .np grouping). Em/en dashes → commas for cleaner prosody.
  const lines = readFileSync(p, "utf8").split("\n")
    .map((l) => l.trim())
    .filter((l) => l && !l.startsWith("#") &&
      !/^(verse|hook|outro|bridge|chorus|intro)\s*\d*$/i.test(l));
  return lines
    .map((l) => l.replace(/[—–]/g, ",").replace(/\s+/g, " ").trim())
    .map((l) => (/[.!?]$/.test(l) ? l : l + "."))
    .join(" ");
}
const LYRIC = loadLyric(LYRIC_SRC);

const TRANSPOSE  = Number(flags.transpose ?? 0);
// SLOWER than the 126 track tempo on purpose — score-stretch's per-word
// target duration = beats × (60/BPM); a lower BPM ⇒ longer absolute
// sustains ⇒ the gooey "drippin" draw-out.
const BPM        = Number(flags.bpm ?? 100);
const MAX_STRETCH = Number(flags["max-stretch"] ?? 16.0);
const OVERLAP_MS  = Number(flags["overlap-ms"] ?? 140);
const VIBRATO_HZ  = String(flags["vibrato-hz"] ?? 5.0);
const VIBRATO_CENTS = String(flags["vibrato-cents"] ?? 16);
const OUT_PATH   = expandHome(flags.out) || `${DANCE}/out/trancepenta-vocal.mp3`;
const FORCE      = !!flags.force;

const OUT_DIR    = `${DANCE}/out`;
mkdirSync(OUT_DIR, { recursive: true });

const LYRIC_TXT  = `${OUT_DIR}/trancepenta-sing.txt`;
const VOCAL_MP3  = `${OUT_DIR}/trancepenta-sing-vocal.mp3`;
const VOCAL_ALIGN = `${VOCAL_MP3}.alignment.json`;
const WORDS_JSON = `${OUT_DIR}/trancepenta-sing-vocal-words.json`;
const PITCHED_MP3 = `${OUT_DIR}/trancepenta-sing-pitched.mp3`;
const STRETCHED_MP3 = `${OUT_DIR}/trancepenta-sing-stretched.mp3`;
const PITCHED_ALIGN = `${OUT_DIR}/trancepenta-sing-pitched-alignment.json`;
const LAYERED_MP3 = `${OUT_DIR}/trancepenta-sing-layered.mp3`;
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

console.log(`━━━ dance · sing-penta (SUNG LEAD-DOUBLE · drippin) ━━━`);
console.log(`  score: ${SCORE_PATH.replace(POP + "/", "")} · transpose ${TRANSPOSE >= 0 ? "+" : ""}${TRANSPOSE}st`);
console.log(`  stretch BPM ${BPM} · max ${MAX_STRETCH}× · overlap ${OVERLAP_MS}ms · vibrato ${VIBRATO_HZ}Hz/${VIBRATO_CENTS}c`);
console.log(`  lyric: "${LYRIC}"\n`);

// ── 1. say (jeffrey-pvc TTS with /with-timestamps alignment) ─────────
// stability 0.64 keeps the jeffrey clone identity (>=0.5 per memory:
// jeffrey_pvc_settings); similarity 0.90; style 0.25 (plain/tender —
// we want a soft, eased sung double, not declamatory consonants);
// speed 0.9 (slightly slow & relaxed so the words already drift).
// One automatic retry on a transient network/socket failure, then the
// graceful no-timestamps fallback (uniform spacing is fine — the .np
// drives the melody, whisper only needs rough word windows).
step("1 · say (jeffrey-pvc — plain/tender, eased)", () => {
  const baseArgs = ["bin/say.mjs", LYRIC_TXT.replace(POP + "/", ""),
                    "--stability", "0.64", "--similarity", "0.90",
                    "--style", "0.25", "--speed", "0.9",
                    "--out", VOCAL_MP3.replace(POP + "/", "")];
  const argsWithTs = [...baseArgs, "--timestamps"];
  if (FORCE) { baseArgs.push("--force"); argsWithTs.push("--force"); }
  // Try with timestamps; retry once on a transient failure.
  let r = spawnSync("node", argsWithTs, { cwd: POP, stdio: ["ignore", "inherit", "pipe"] });
  if (r.status === 0) return;
  console.log("  ↪ /with-timestamps attempt failed — retrying once …");
  r = spawnSync("node", argsWithTs, { cwd: POP, stdio: ["ignore", "inherit", "pipe"] });
  if (r.status === 0) return;
  console.log("  ↪ still failing — falling back to plain TTS + whisper align");
  run("node", baseArgs);
});

// ── 2. align → words.json ────────────────────────────────────────────
// Real words this time, so whisper CAN transcribe — but ElevenLabs
// /with-timestamps is exact & free, so prefer it. Uniform fallback is
// still acceptable: the .np note sequence (not whisper) drives pitch;
// whisper only needs rough word windows for the per-word slice.
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
  // Try 3: uniform spacing across ffprobe-measured duration.
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

// ── 3. score-pitch (WORLD f0 → sung G-dorian tonic-pole chord tones) ─
// A touch deeper + slightly faster vibrato than the old hum so the
// LONG held drippin notes shimmer instead of sitting dead-flat.
step(`3 · score-pitch (WORLD · transpose ${TRANSPOSE >= 0 ? "+" : ""}${TRANSPOSE}st · vibrato ${VIBRATO_HZ}Hz/${VIBRATO_CENTS}c)`, () => {
  run("node", [
    "bin/score-pitch.mjs",
    "--slug", "trancepenta-sing",
    "--section", "all",
    "--score", SCORE_PATH,
    "--vocal", VOCAL_MP3,
    "--words", WORDS_JSON,
    "--transpose", String(TRANSPOSE),
    "--vibrato-hz", VIBRATO_HZ,
    "--vibrato-cents", VIBRATO_CENTS,
    "--out", PITCHED_MP3,
  ]);
});

// ── 4. score-stretch (rubberband → drawn-out gooey legato) ───────────
// Slow BPM + very high max-stretch + big crossfade overlap = the
// defining "drippin" quality: each note draws all the way out and
// glues legato into the next with no staccato word edge.
step(`4 · score-stretch (rubberband · ${BPM} BPM · max ${MAX_STRETCH}× · overlap ${OVERLAP_MS}ms)`, () => {
  run("node", [
    "bin/score-stretch.mjs",
    "--slug", "trancepenta-sing",
    "--section", "all",
    "--score", SCORE_PATH,
    "--in", PITCHED_MP3,
    "--alignment", PITCHED_ALIGN,
    "--bpm", String(BPM),
    "--max-stretch", String(MAX_STRETCH),
    "--overlap-ms", String(OVERLAP_MS),
    "--out", STRETCHED_MP3,
  ]);
});

// ── 5. layer: a SOFT warm double (octave-below body + quiet fifth) ───
// NOT the dense many-jeffreys racing stack from sing.mjs. Just a warm
// co-lead: the main sung line + one octave-below for body + one quiet
// fifth for choir warmth. Both consonant against the G-dorian tonic
// pole the line tracks. Gentle stereo spread, no echo/slice/rap.
//   main   ×1.0 +0  st  (gain 1.00)  the sung lead-double
//   low    ×1.0 -12 st  (gain 0.30)  octave-below — warm body
//   fifth  ×1.0 +7  st  (gain 0.16)  a soft fifth — choir warmth
step("5 · layer (soft warm double — octave + quiet fifth)", () => {
  const baseWav = `${TMP_DIR}/base.wav`;
  run("ffmpeg", ["-hide_banner","-y","-loglevel","error","-i",STRETCHED_MP3,"-ar","48000","-c:a","pcm_s16le",baseWav]);

  const layers = [
    { name: "low",   pitch: -12, gain: 0.30, src: baseWav },
    { name: "fifth", pitch:   7, gain: 0.16, src: baseWav },
  ];

  const layerWavs = [];
  for (const l of layers) {
    const wav = `${TMP_DIR}/${l.name}.wav`;
    run("rubberband", ["-t", "1.0", "-p", String(l.pitch), "--formant", l.src, wav]);
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
  // Main: a longer anti-click ramp (so the drippin entry breathes in).
  parts.push(`[0:a]pan=stereo|c0=c0|c1=c0,volume=eval=frame:volume='min(max(t/0.8,0),1)'[m]`);
  layerWavs.forEach((l, i) => {
    const idx = i + 1;
    // Octave-below stays centred (body); fifth nudged off-centre for
    // a little width without smearing the line.
    const pan = l.name === "fifth" ? "c0=1.0*c0|c1=0.65*c0" : "c0=c0|c1=c0";
    parts.push(`[${idx}:a]pan=stereo|${pan},volume=${l.gain}[l${i}]`);
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
console.log(`\n✓ ${OUT_PATH} (${sz} KB · ${parseFloat(probe.stdout.trim()).toFixed(1)}s · jeffrey-pvc SUNG LEAD-DOUBLE · drippin)`);
