#!/usr/bin/env node
// extract-vowels.mjs — turn the Amazing Grace dance vocal into a pure
// VOWEL-EXTRACTION drone. No words, no consonants: just the open vowel
// nucleus of every sung syllable, stretched over long distances and
// stacked so the vowels are always rolling through each other in
// harmony. Less a cover of the hymn, more an extraction of its vowels.
//
// Concept (per @jeffrey, 2026-06-01):
//   • ONLY the vowels — strip the consonant onsets/codas from each take.
//   • Stretch each vowel WAY out (the "first ahh" feeling) — many seconds.
//   • Heavy overlap so multiple held vowels always sound together; the
//     G-pentatonic melody harmonizes itself, plus a continuous low drone
//     floor (root + fifth) so it's NEVER silent and always in harmony.
//
// Sustain method (robust — the old single 32-48× rubberband stretch
// disintegrated extreme holds into near-silence; the destructive
// silenceremove also ate the soft "see" take down to 22 ms → 24 s of
// silence at the end):
//   1. Grain = MIDDLE window of the RAW take (skips leading + closing
//      consonant). No silenceremove. Min-length guard widens the window
//      if a take is short, so a grain is always a real vowel.
//   2. WORLD f0-lock the grain to the target pitch (steady tone).
//   3. aloop the pitched grain to ~hold/5, then rubberband ≤~5× to the
//      exact hold. Looping-then-moderate-stretch keeps a constant level
//      and can't break up.
//   4. Long afade in/out so holds bleed together; loudnorm per voice.
//
// Reads:
//   • wave-wizard/samples/amazing-grace-dance/spec.json  (note order/midi)
//   • pop/big-pictures/voice-takes/wavewizard/<line>-note-<i>.wav
// Writes:
//   • pop/big-pictures/out/amaythingra-vowels.wav            (the drone stem)
//   • pop/big-pictures/out/amaythingra-vowels-preview.mp3    (quick listen)
//
// Usage:
//   node pop/big-pictures/bin/extract-vowels.mjs \
//        [--hold-per-beat 8] [--stride-per-beat 4] [--harmony 0,-12] \
//        [--drone-gain 0.18] [--crisp 2] [--no-cache]

import { execSync } from "node:child_process";
import { readFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const POP  = resolve(REPO, "pop");
const SPEC = resolve(REPO, "wave-wizard/samples/amazing-grace-dance/spec.json");
const TAKES_DIR = resolve(POP, "big-pictures/voice-takes/wavewizard");
const OUT  = resolve(POP, "big-pictures/out/amaythingra-vowels.wav");
const OUT_MP3 = OUT.replace(/\.wav$/, "-preview.mp3");
const TMP  = "/tmp/amaythingra-vowels";
mkdirSync(TMP, { recursive: true });
mkdirSync(dirname(OUT), { recursive: true });

// ── args ────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
function flag(name, def) {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : def;
}
const HOLD_PER_BEAT   = parseFloat(flag("hold-per-beat", "8"));   // s of sustain per durBeat
const STRIDE_PER_BEAT = parseFloat(flag("stride-per-beat", "4")); // s between onsets per durBeat
const CRISP           = flag("crisp", "2");                       // rubberband smoothness (low = smooth)
const HARMONY = flag("harmony", "0,-12").split(",").map((s) => parseInt(s, 10));
const DRONE_GAIN = parseFloat(flag("drone-gain", "0.18"));        // continuous floor level (0 = off)
const NO_CACHE = argv.includes("--no-cache");
// Tighter default core window than before: pull the grain to the pure
// vowel nucleus so consonant onsets/codas can't leak in (@jeffrey: "no
// consonants in the jeffrey takes"). The min-length guard below still
// widens it for short takes.
const CORE_START = parseFloat(flag("core-start", "0.36"));
const CORE_END   = parseFloat(flag("core-end", "0.62"));
const STRETCH_CAP = 5.0;   // max rubberband ratio (loop first to stay under)
// Vowel gate (@jeffrey: "only ashhhs and oooh"). Keep the open + rounded
// vowels — ah (the "ashh") and aw/uh (the rounded "oooh") — and drop the
// bright/closed syllables (zing=ih, grace/wretch=eh, sweet/me/see=ee,
// saved=ay) whose hard consonant attacks carry the words. Timing still
// advances across skipped syllables, so the kept vowels stay on their
// original onsets with the dropped ones simply left as breathing room.
const KEEP_VOWELS = flag("vowels", "ah,aw,uh").split(",").map((s) => s.trim());

// ── tooling ───────────────────────────────────────────────────────────
const WORLD_PY  = resolve(POP, ".venv/bin/python");
const PITCHSNAP = resolve(POP, "bin/pitchsnap_world.py");
const USE_WORLD = existsSync(WORLD_PY) && existsSync(PITCHSNAP);
function midiToName(m) {
  const NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
  return NAMES[((m % 12) + 12) % 12] + (Math.floor(m / 12) - 1);
}
function dur(path) {
  return parseFloat(execSync(
    `ffprobe -v error -show_entries format=duration -of csv=p=0 "${path}"`
  ).toString().trim());
}
function sh(cmd) { execSync(cmd, { stdio: ["ignore", "ignore", "pipe"] }); }

// ── vowel map (documentation only; audio is extracted from real takes) ──
const VOWELS = {
  "line-1": ["ah","ah","ih","eh","ah","ee","uh","ah"],     // a ma zing grace how sweet the sound
  "line-2": ["ah","ay","uh","eh","ah","ee"],                // that saved a wretch like me
  "line-3": ["ah","uh","uh","aw","uh","ah","ah","ah"],      // i once was lost but now am found
  "line-4": ["uh","ah","uh","ah","ah","ee"],                // was blind but now i see
};

if (!existsSync(SPEC)) { console.error(`✗ spec not found: ${SPEC}`); process.exit(1); }
const spec = JSON.parse(readFileSync(SPEC, "utf8"));

// ── grain: middle window of the RAW take, with a min-length guard ───────
function makeGrain(takePath, id) {
  const grain = `${TMP}/${id}-grain.wav`;
  if (!NO_CACHE && existsSync(grain)) return grain;
  const d = dur(takePath);
  let a = CORE_START, b = CORE_END;
  // Guard: never let the window collapse below 0.30 s of source.
  if (d * (b - a) < 0.30) { a = 0.20; b = 0.80; }
  const ss = (d * a).toFixed(3);
  const len = Math.max(0.30, d * (b - a)).toFixed(3);
  sh(`ffmpeg -y -loglevel error -ss ${ss} -t ${len} -i "${takePath}" ` +
     `-ar 48000 -ac 1 "${grain}"`);
  return grain;
}

// ── sustain: WORLD-lock → aloop → moderate rubberband → envelope ────────
// Returns a holdSec-long stereo drone locked to targetMidi. Cached by key.
function sustain(grainPath, targetMidi, holdSec, fadeSec, outPath) {
  if (!NO_CACHE && existsSync(outPath)) return outPath;
  const targetName = midiToName(targetMidi);
  const pitched = outPath.replace(/\.wav$/, "-p.wav");
  const looped  = outPath.replace(/\.wav$/, "-l.wav");
  const stretched = outPath.replace(/\.wav$/, "-s.wav");

  // 1) WORLD pitch lock
  let src = grainPath;
  if (USE_WORLD) {
    try {
      sh(`"${WORLD_PY}" "${PITCHSNAP}" "${grainPath}" "${pitched}" ` +
         `--notes "${targetName}" --xfade-ms 0 --voicing-ramp-ms 20`);
      src = pitched;
    } catch (e) {
      console.error(`  WORLD failed @ ${targetName}: ${(e.stderr||"").toString().split("\n")[0]}`);
    }
  }
  const grainDur = dur(src);

  // 2) loop the grain up to ~hold/CAP so the final stretch ratio is ≤ CAP
  const loopTargetDur = Math.max(grainDur, holdSec / STRETCH_CAP);
  const loops = Math.max(0, Math.ceil(loopTargetDur / grainDur) - 1);
  const samples = Math.round(grainDur * 48000);
  if (loops > 0) {
    sh(`ffmpeg -y -loglevel error -i "${src}" ` +
       `-af "aloop=loop=${loops}:size=${samples},atrim=duration=${loopTargetDur.toFixed(3)}" ` +
       `-ar 48000 -ac 1 "${looped}"`);
  } else {
    execSync(`cp "${src}" "${looped}"`);
  }
  const loopedDur = dur(looped);

  // 3) moderate rubberband stretch to the exact hold
  const ratio = (holdSec / loopedDur).toFixed(3);
  try {
    sh(`rubberband --time ${ratio} --formant --crisp ${CRISP} ` +
       `"${looped}" "${stretched}" 2>/dev/null`);
  } catch (e) {
    execSync(`cp "${looped}" "${stretched}"`);
  }

  // 4) envelope + per-voice loudnorm → stereo
  const fOut = (holdSec - fadeSec).toFixed(2);
  sh(`ffmpeg -y -loglevel error -i "${stretched}" ` +
     `-af "afade=t=in:st=0:d=${fadeSec.toFixed(2)},` +
     `afade=t=out:st=${fOut}:d=${fadeSec.toFixed(2)},` +
     `loudnorm=I=-20:TP=-2:LRA=6,aformat=channel_layouts=stereo" ` +
     `-ar 48000 -ac 2 -c:a pcm_s16le "${outPath}"`);
  return outPath;
}

// ── walk the spec → note list with stretched timing ─────────────────────
const notes = [];
let cumBeats = 0;
for (const sample of spec.samples) {
  if (!sample.score) continue;
  for (let i = 0; i < sample.score.notes.length; i++) {
    const n = sample.score.notes[i];
    const vowel = (VOWELS[sample.name] || [])[i] || "ah";
    // Advance the timeline for EVERY syllable so kept vowels keep their
    // original onsets; only push the ones that pass the vowel gate.
    const onsetSec = cumBeats * STRIDE_PER_BEAT;
    cumBeats += n.beats;
    if (!KEEP_VOWELS.includes(vowel)) continue;  // drop bright/closed vowels
    const takePath = `${TAKES_DIR}/${sample.name}-note-${i}.wav`;
    notes.push({
      idx: notes.length,
      sampleName: sample.name,
      noteIdx: i,
      onsetSec,
      holdSec: n.beats * HOLD_PER_BEAT,
      targetMidi: sample.score.rootMel + n.off,
      vowel,
      takePath: existsSync(takePath) ? takePath : null,
    });
  }
}
const TOTAL = notes.length
  ? Math.max(...notes.map((n) => n.onsetSec + n.holdSec)) + 1
  : 0;
console.log(
  `vowel extraction: ${notes.length} syllables kept [${KEEP_VOWELS.join("/")}] · ` +
  `onset span ${(cumBeats * STRIDE_PER_BEAT).toFixed(1)}s · total ${TOTAL.toFixed(1)}s\n` +
  `  hold/beat ${HOLD_PER_BEAT}s · stride/beat ${STRIDE_PER_BEAT}s · ` +
  `harmony [${HARMONY.join(", ")}] · drone ${DRONE_GAIN}\n`
);

// ── render melodic vowel voices ─────────────────────────────────────────
const stems = [];
let made = 0, skipped = 0;
for (const note of notes) {
  if (!note.takePath) { skipped++; continue; }
  const id = note.idx.toString().padStart(2, "0");
  const grain = makeGrain(note.takePath, id);
  const fade = Math.min(note.holdSec * 0.45, 4);
  for (const interval of HARMONY) {
    const hid = `${id}_${interval >= 0 ? "p" : "m"}${Math.abs(interval)}`;
    const droned = `${TMP}/${hid}-drone.wav`;
    sustain(grain, note.targetMidi + interval, note.holdSec, fade, droned);
    stems.push({ path: droned, onsetSec: note.onsetSec, gain: interval < 0 ? 0.7 : 1.0 });
  }
  made++;
  process.stdout.write(
    `  ${id} ${note.sampleName}#${note.noteIdx} ` +
    `${note.vowel.padEnd(2)} → ${midiToName(note.targetMidi).padEnd(3)} ` +
    `hold ${note.holdSec.toFixed(1)}s @ ${note.onsetSec.toFixed(1)}s\n`
  );
}

// ── continuous low harmonic drone floor (root + fifth) ──────────────────
// Built from the very first open "ah" grain, held the whole length at low
// level. Guarantees the track is never silent and always sits in harmony.
if (DRONE_GAIN > 0) {
  const floorGrain = makeGrain(notes[0].takePath, "00");
  const FLOOR = [43, 50];   // G2 root + D3 fifth (G pentatonic)
  for (const midi of FLOOR) {
    const fid = `floor-${midi}`;
    const droned = `${TMP}/${fid}.wav`;
    sustain(floorGrain, midi, TOTAL, 8, droned);
    stems.push({ path: droned, onsetSec: 0, gain: DRONE_GAIN });
    process.stdout.write(`  floor ${midiToName(midi)} hold ${TOTAL.toFixed(0)}s @ 0s (gain ${DRONE_GAIN})\n`);
  }
}
console.log(`\n  vowels made: ${made} · skipped: ${skipped} · voices: ${stems.length}\n`);
if (!stems.length) { console.error(`✗ no stems — check takes at ${TAKES_DIR}`); process.exit(1); }

// ── stitch: adelay + volume per voice → amix → loudnorm ─────────────────
const args = ["-y", "-loglevel", "error"];
for (const s of stems) args.push("-i", s.path);
const filters = [];
for (let k = 0; k < stems.length; k++) {
  const ms = Math.round(stems[k].onsetSec * 1000);
  filters.push(
    `[${k}:a]aresample=48000,aformat=channel_layouts=stereo,` +
    `volume=${stems[k].gain},adelay=${ms}|${ms}[d${k}]`
  );
}
filters.push(
  stems.map((_, k) => `[d${k}]`).join("") +
  `amix=inputs=${stems.length}:duration=longest:dropout_transition=0:normalize=0,` +
  `atrim=duration=${TOTAL.toFixed(3)}[out]`
);
args.push("-filter_complex", filters.join(";"), "-map", "[out]",
          "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", `${TMP}/mix.wav`);
console.log(`→ mixing ${stems.length} vowel voices over ${TOTAL.toFixed(1)}s...`);
execSync(`ffmpeg ${args.map((a) => `"${a}"`).join(" ")}`);

console.log(`→ loudnorm + write stem...`);
sh(`ffmpeg -y -loglevel error -i "${TMP}/mix.wav" ` +
   `-af "loudnorm=I=-14:TP=-1.5:LRA=7" ` +
   `-ar 48000 -ac 2 -c:a pcm_s16le "${OUT}"`);
sh(`ffmpeg -y -loglevel error -i "${OUT}" -b:a 256k "${OUT_MP3}"`);

console.log(`\n✓ ${OUT.replace(POP + "/", "pop/")} (${dur(OUT).toFixed(1)}s)`);
console.log(`✓ ${OUT_MP3.replace(POP + "/", "pop/")}  ← quick listen`);
