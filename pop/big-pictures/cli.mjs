#!/usr/bin/env node
// big-pictures/cli.mjs — single-command pipeline for vocal tracks.
//
// Each step is content-hash cached by its underlying script (say.mjs,
// align.mjs, score-pitch.mjs, etc.); rerunning costs $0 unless inputs
// change. The CLI just wires them together with sane defaults.
//
// Pipeline:
//   1. say.mjs           <slug>.txt              → <slug>-vocal.mp3
//   2. align.mjs         <slug>-vocal.mp3        → <slug>-vocal-words.json
//   3. score-pitch.mjs   WORLD f0 replacement    → <slug>-pitched.mp3
//                                                + <slug>-pitched-alignment.json
//   4. score-stretch.mjs per-word rubberband     → <slug>-pitched-stretched.mp3
//                        (skipped with --no-stretch)
//   5. melody-bells.mjs  pad/bell accompaniment  → <slug>-pad.mp3
//   6. waltz.mjs         sinebells harmonic bed  → <slug>-bed.mp3
//   7. ffmpeg amix       vocal-forward 3-layer   → <slug>-mix.mp3
//   8. stamp concat      ac signoff (chipmunk +
//                        4-bit crush)            → <slug>-stamped.mp3
//                        (skipped with --no-stamp)
//   9. finalize.mjs      ID3 + cover + lyrics    → <slug>-final.mp3
//  10. cp                                        → ~/Desktop/<slug>.mp3
//
// Usage:
//   node cli.mjs <slug>
//   node cli.mjs all                  build every <slug>.np in this dir
//   node cli.mjs <slug> --status      show cache state
//   node cli.mjs <slug> --bpm 80 --transpose 0 --voice pad
//   node cli.mjs <slug> --no-stretch  natural prosody, no hymn pacing
//   node cli.mjs <slug> --no-stamp    skip the ac signoff
//   node cli.mjs <slug> --force       bust caches end to end
//
// Per-song defaults live in DEFAULTS below. Anything not listed gets
// the universal fallback (bpm 80, transpose 0, pad voice, etc.).

import { spawnSync } from "node:child_process";
import {
  readFileSync, writeFileSync, copyFileSync,
  existsSync, statSync, mkdirSync, readdirSync,
} from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP = resolve(HERE, "..");
const REPO = resolve(POP, "..");
const RECAP = resolve(REPO, "recap");
const OUT = `${HERE}/out`;
mkdirSync(OUT, { recursive: true });

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[k] = next; i++; }
    else flags[k] = true;
  } else positional.push(a);
}

const SLUG = positional[0];
if (!SLUG || SLUG === "--help" || SLUG === "-h") {
  console.log(`big-pictures cli — vocal track pipeline

usage:
  node cli.mjs <slug>
  node cli.mjs all                    every <slug>.np in this dir
  node cli.mjs <slug> --status        cache state
  node cli.mjs <slug> --bpm 80 --transpose 0 --voice pad
  node cli.mjs <slug> --no-stretch    natural-paced (no hymn rubberband)
  node cli.mjs <slug> --no-stamp      skip the ac signoff
  node cli.mjs <slug> --force         bust caches end to end

flags:
  --bpm N                tempo for stretch + bells + bed
  --transpose N          semitone shift on the vocal (sign matters)
  --transpose-bed N      semitone shift on the bed (key root, e.g. 7 = G)
  --max-stretch X        rubberband ceiling (default 6.0; held notes hit this)
  --onset-shift-ms N     shift cuts earlier so vowels land on beats (default 60)
  --voice pad|bell       accompaniment timbre (default pad)
  --scale major|minor    bed scale
  --progression "0,3,0,4"  chord-degree cycle for the bed
`);
  process.exit(SLUG === "--help" || SLUG === "-h" ? 0 : 1);
}

// ── per-song defaults ────────────────────────────────────────────────
// Flags override these. Any slug not listed gets DEFAULT_FALLBACK.
const DEFAULT_FALLBACK = {
  bpm: 80, transpose: 0, maxStretch: 6.0, voice: "pad",
  scale: "major", transposeBed: 0, progression: "0,3,4,0",
  title: null,
};
const DEFAULTS = {
  amazing:  { bpm: 70, transpose: 0,  maxStretch: 8.0, voice: "pad",
              scale: "major", transposeBed: 7, progression: "0,3,0,4",
              title: "amazing grace" },
  twinkle:  { bpm: 80, transpose: 0,  maxStretch: 6.0, voice: "pad",
              scale: "major", transposeBed: 0, progression: "0,3,4,0",
              title: "twinkle twinkle little star" },
  mary:     { bpm: 100, transpose: 0,  maxStretch: 5.0, voice: "pad",
              scale: "major", transposeBed: 0, progression: "0,3,4,0",
              title: "mary had a little lamb" },
  row:      { bpm: 90,  transpose: -5, maxStretch: 6.0, voice: "pad",
              scale: "major", transposeBed: 0, progression: "0,3,4,0",
              title: "row row row your boat" },
  elephant: { bpm: 100, transpose: 0,  maxStretch: 5.0, voice: "pad",
              scale: "minor", transposeBed: 0, progression: "0,5,3,4",
              title: "elephant" },
  plork:    { bpm: 90,  transpose: 0,  maxStretch: 5.0, voice: "bell",
              scale: "minor", transposeBed: 0, progression: "0,5,3,4",
              title: "plork" },
  sentence: { bpm: 90,  transpose: 0,  maxStretch: 5.0, voice: "pad",
              scale: "minor", transposeBed: 0, progression: "0,5,3,4",
              title: "the music is real" },
  "small-world": { bpm: 110, transpose: -5, maxStretch: 5.0, voice: "pad",
              scale: "major", transposeBed: 5, progression: "0,3,4,0",
              title: "it's a small world" },
  frere:    { bpm: 100, transpose: -5, maxStretch: 5.0, voice: "pad",
              scale: "major", transposeBed: 0, progression: "0,4,0,4",
              title: "frère jacques" },
};

// ── 'all' subcommand ─────────────────────────────────────────────────
if (SLUG === "all") {
  const slugs = readdirSync(HERE)
    .filter((f) => f.endsWith(".np"))
    .map((f) => f.replace(/\.np$/, ""))
    .sort();
  console.log(`▸ building ${slugs.length} song(s): ${slugs.join(", ")}`);
  for (const s of slugs) {
    console.log(`\n━━━━━━━━━━━━━━━━━━━━ ${s} ━━━━━━━━━━━━━━━━━━━━`);
    const r = spawnSync(process.execPath, [process.argv[1], s, ...argv.slice(1).filter((a) => a !== "all")],
                        { stdio: "inherit", cwd: HERE });
    if (r.status !== 0) console.error(`✗ ${s} failed (continuing)`);
  }
  process.exit(0);
}

// ── resolve config for this slug ─────────────────────────────────────
const D = { ...DEFAULT_FALLBACK, ...(DEFAULTS[SLUG] || {}) };
const BPM           = Number(flags.bpm ?? D.bpm);
const TRANSPOSE     = Number(flags.transpose ?? D.transpose);
const MAX_STRETCH   = Number(flags["max-stretch"] ?? D.maxStretch);
const ONSET_SHIFT   = Number(flags["onset-shift-ms"] ?? 60);
const VOICE         = flags.voice || D.voice;
const SCALE         = flags.scale || D.scale;
const TRANSPOSE_BED = Number(flags["transpose-bed"] ?? D.transposeBed);
const PROGRESSION   = flags.progression || D.progression;
const STRETCH       = !flags["no-stretch"];
const STAMP         = !flags["no-stamp"];
const FORCE         = flags.force === true;
const TITLE         = flags.title || D.title || SLUG.replace(/[-_]+/g, " ");

const TXT = `${HERE}/${SLUG}.txt`;
const NP  = `${HERE}/${SLUG}.np`;
if (!existsSync(TXT)) { console.error(`✗ missing ${TXT}`); process.exit(1); }
if (!existsSync(NP))  { console.error(`✗ missing ${NP}`);  process.exit(1); }

// Output paths (all underneath OUT)
const VOCAL     = `${OUT}/${SLUG}-vocal.mp3`;
const WORDS     = `${OUT}/${SLUG}-vocal-words.json`;
const PITCHED   = `${OUT}/${SLUG}-pitched.mp3`;
const STRETCHED = `${OUT}/${SLUG}-pitched-stretched.mp3`;
const PAD       = `${OUT}/${SLUG}-pad.mp3`;
const BED       = `${OUT}/${SLUG}-bed.mp3`;
const MIX       = `${OUT}/${SLUG}-mix.mp3`;
const MIXSFX    = `${OUT}/${SLUG}-mix-sfx.mp3`;
const STAMPED   = `${OUT}/${SLUG}-stamped.mp3`;
const FINAL     = `${OUT}/${SLUG}-final.mp3`;
const STAMP_VOCAL = `${OUT}/ac-stamp-vocal.mp3`;
const STAMP_FX    = `${OUT}/ac-stamp-crunched.mp3`;
const DESK = `${homedir()}/Desktop/${SLUG}.mp3`;

// ── --status: show cache state and bail ──────────────────────────────
if (flags.status) {
  const tally = (label, p) => {
    if (existsSync(p)) {
      const s = statSync(p);
      const kb = (s.size / 1024).toFixed(0);
      const age = ((Date.now() - s.mtimeMs) / 1000 / 60).toFixed(1);
      console.log(`  ✓ ${label.padEnd(28)} ${kb.padStart(7)} KB · ${age}m old`);
    } else {
      console.log(`  · ${label.padEnd(28)}    (not built)`);
    }
  };
  console.log(`▸ cache for '${SLUG}'`);
  tally("vocal (TTS)", VOCAL);
  tally("words.json (whisper)", WORDS);
  tally("pitched (WORLD)", PITCHED);
  tally("pitched-stretched", STRETCHED);
  tally("pad/bells", PAD);
  tally("bed (waltz)", BED);
  tally("mix", MIX);
  tally("mix+sfx", MIXSFX);
  tally("stamped", STAMPED);
  tally("final", FINAL);
  tally("Desktop copy", DESK);
  process.exit(0);
}

// ── helpers ──────────────────────────────────────────────────────────
function step(name, fn) {
  const t0 = Date.now();
  process.stdout.write(`▸ ${name}\n`);
  fn();
  const ms = Date.now() - t0;
  console.log(`  ${(ms / 1000).toFixed(1)}s`);
}

function run(cmd, args, cwd = POP) {
  const r = spawnSync(cmd, args, { cwd, stdio: ["ignore", "inherit", "inherit"] });
  if (r.status !== 0) {
    console.error(`✗ ${cmd} ${args.join(" ")} failed (exit ${r.status})`);
    process.exit(1);
  }
}

// Pull every NOTE: token's midi from the .np for range diagnostics.
function scoreMidis() {
  const NOTE_BASE = { C:0,"C#":1,DB:1,D:2,"D#":3,EB:3,E:4,F:5,
                      "F#":6,GB:6,G:7,"G#":8,AB:8,A:9,"A#":10,BB:10,B:11 };
  const lines = readFileSync(NP, "utf8").split("\n");
  const out = [];
  for (const raw of lines) {
    const l = raw.trim();
    if (!l || l.startsWith("#")) continue;
    if (/^(verse|hook|bridge|chorus|outro|intro)( \d+)?$/i.test(l)) continue;
    for (const tok of l.split(/\s+/)) {
      const m = tok.match(/^([A-Ga-g][#b]?)-?(\d):/);
      if (!m) continue;
      const name = m[1].toUpperCase();
      const oct = parseInt(m[2], 10);
      out.push(12 * (oct + 1) + NOTE_BASE[name]);
    }
  }
  return out;
}

// Warn when the post-transpose target range falls below jeffrey's
// sweet spot. WORLD f0 replacement only sounds like SINGING when the
// target sits above natural speech (~midi 56-58 / 175 Hz). When the
// target median lands in his speaking baritone, listeners hear his
// prosody, not the melody. This was the mary -7 bug.
function pitchSanityCheck() {
  const midis = scoreMidis();
  if (!midis.length) return;
  const sorted = [...midis].sort((a, b) => a - b);
  const median = sorted[Math.floor(sorted.length / 2)] + TRANSPOSE;
  const climax = sorted[sorted.length - 1] + TRANSPOSE;
  const SPEECH_TOP = 58; // ~Bb3, top of jeffrey's natural baritone
  const ICONIC_LOW = 65; // E4 — below this and pitching feels subtle
  const ICONIC_TOP = 76; // E5 — above this and source struggles
  const noteName = (m) => {
    const oct = Math.floor(m / 12) - 1;
    const n = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"][m % 12];
    return `${n}${oct}`;
  };
  if (median <= SPEECH_TOP) {
    console.log(`⚠ post-transpose median ${noteName(median)} (midi ${median}) is in jeffrey's speech range`);
    console.log(`  WORLD's pitch shift will be subtle / inaudible. Try --transpose ${ICONIC_LOW - sorted[Math.floor(sorted.length/2)]} or higher.`);
  } else if (climax > ICONIC_TOP) {
    console.log(`⚠ post-transpose climax ${noteName(climax)} (midi ${climax}) above E5 — formant artifacts likely`);
    console.log(`  Consider --transpose ${ICONIC_TOP - sorted[sorted.length - 1]} or lower.`);
  }
}

// Sum beats × inter-verse gaps from the .np to predict total duration.
// Used to size the bed and trim the mix.
function computeDuration() {
  const lines = readFileSync(NP, "utf8").split("\n");
  let beats = 0;
  let sectionCount = 0;
  for (const raw of lines) {
    const l = raw.trim();
    if (!l || l.startsWith("#")) continue;
    if (/^(verse|hook|bridge|chorus|outro|intro)( \d+)?$/i.test(l)) {
      sectionCount++; continue;
    }
    for (const tok of l.split(/\s+/)) {
      const m = tok.match(/^[A-Ga-g][#b]?-?\d:[^*]+(?:\*(\d+(?:\.\d+)?))?$/);
      if (!m) continue;
      beats += Number(m[1] ?? 1);
    }
  }
  if (sectionCount > 1) beats += 2 * (sectionCount - 1); // inter-section rest
  return Math.ceil((beats * 60) / BPM);
}

// Parse a `<slug>.sfx.txt` cue sidecar. Each non-comment line:
//   at=SEC gain=G dur=S [loop] : sound description
// The head (before the colon) is whitespace-separated key=val tokens;
// everything after the colon is the prompt.
function parseSfxCues(path) {
  const cues = [];
  for (const raw of readFileSync(path, "utf8").split("\n")) {
    const line = raw.trim();
    if (!line || line.startsWith("#")) continue;
    const ci = line.indexOf(":");
    if (ci === -1) continue;
    const prompt = line.slice(ci + 1).trim();
    if (!prompt) continue;
    const cue = { at: 0, gain: 0.7, dur: null, loop: false, prompt };
    for (const tok of line.slice(0, ci).trim().split(/\s+/)) {
      if (!tok) continue;
      const [k, v] = tok.split("=");
      if (k === "at") cue.at = Number(v) || 0;
      else if (k === "gain") cue.gain = Number(v);
      else if (k === "dur") cue.dur = Math.max(0.5, Math.min(30, Number(v)));
      else if (k === "loop") cue.loop = true;
    }
    cues.push(cue);
  }
  return cues;
}

// ── pipeline ─────────────────────────────────────────────────────────
const t0 = Date.now();
console.log(`━━━ ${SLUG} ━━━ bpm ${BPM} · transpose ${TRANSPOSE >= 0 ? "+" : ""}${TRANSPOSE}st · ${VOICE} · ${STRETCH ? "stretched" : "natural"}${STAMP ? " · stamp" : ""}\n`);
pitchSanityCheck();

// ElevenLabs `/with-timestamps` returns exact per-character alignment
// for the TTS it just synthesized — no recognition step, no drift.
// Falls back to whisper if the alignment sidecar doesn't appear (e.g.
// the server endpoint isn't patched).
const VOCAL_ALIGN = `${OUT}/${SLUG}-vocal.mp3.alignment.json`;

step("1 · say (jeffrey-pvc TTS)", () => {
  // Try /with-timestamps first; if the server isn't patched (returns
  // audio/mpeg instead of JSON), fall back to plain TTS + whisper.
  const STABILITY = String(flags.stability ?? "0.6");
  const SIMILARITY = String(flags.similarity ?? "0.9");
  const baseArgs = ["bin/say.mjs", `big-pictures/${SLUG}.txt`,
                "--stability", STABILITY, "--similarity", SIMILARITY,
                "--out", `big-pictures/out/${SLUG}-vocal.mp3`];
  const argsWithTs = [...baseArgs, "--timestamps"];
  if (FORCE) { baseArgs.push("--force"); argsWithTs.push("--force"); }
  const r = spawnSync("node", argsWithTs, { cwd: POP, stdio: ["ignore", "inherit", "pipe"] });
  if (r.status === 0) return;
  // /with-timestamps unsupported — retry plain. Existing cached vocal
  // (without alignment sidecar) will be reused via content hash.
  console.log("  ↪ falling back to plain TTS + whisper alignment");
  run("node", baseArgs);
});

step("2 · align (ElevenLabs timestamps → words.json)", () => {
  if (existsSync(VOCAL_ALIGN)) {
    // Convert ElevenLabs alignment → words.json schema that score-pitch
    // and melody-bells already expect ([{text, fromMs, toMs}]).
    const a = JSON.parse(readFileSync(VOCAL_ALIGN, "utf8"));
    if (Array.isArray(a.words) && a.words.length) {
      writeFileSync(WORDS, JSON.stringify(a.words, null, 0));
      console.log(`  ✓ ${a.words.length} words from ElevenLabs (exact)`);
      return;
    }
    console.log(`  ⚠ alignment sidecar empty; falling back to whisper`);
  }
  const args = ["bin/align.mjs", `big-pictures/out/${SLUG}-vocal.mp3`];
  if (FORCE) args.push("--force");
  run("node", args);
});

// Step 2.5 — librosa onset refinement. Whisper word boundaries can
// drift 50-150ms from the actual consonant attack. librosa's
// onset_detect (with backtrack) finds energy onsets to ~30ms. We snap
// each whisper word's fromMs to the nearest onset within ±200ms,
// keeping whisper's word IDENTITY but acoustically-precise TIMING.
//
// Skipped automatically when the ElevenLabs timestamps path was used —
// those are already exact and don't need refinement.
const REFINED_WORDS = `${OUT}/${SLUG}-vocal-words-refined.json`;
step("2.5 · refine word boundaries (librosa onset detection)", () => {
  if (existsSync(VOCAL_ALIGN)) {
    // ElevenLabs timestamps are ground truth — just copy.
    copyFileSync(WORDS, REFINED_WORDS);
    console.log("  ↪ skipped (ElevenLabs timestamps already exact)");
    return;
  }
  run(`${POP}/.venv/bin/python`, [`${POP}/bin/refine_words.py`,
       `big-pictures/out/${SLUG}-vocal.mp3`,
       `big-pictures/out/${SLUG}-vocal-words.json`,
       `big-pictures/out/${SLUG}-vocal-words-refined.json`]);
});

step(`3 · score-pitch (WORLD · transpose ${TRANSPOSE >= 0 ? "+" : ""}${TRANSPOSE}st)`, () => {
  // Prefer the librosa-refined boundaries when available; the file
  // schema is identical so score-pitch reads it transparently.
  const wordsArg = existsSync(REFINED_WORDS)
    ? `big-pictures/out/${SLUG}-vocal-words-refined.json`
    : `big-pictures/out/${SLUG}-vocal-words.json`;
  run("node", ["bin/score-pitch.mjs", "--slug", SLUG, "--section", "all",
               "--transpose", String(TRANSPOSE),
               "--vocal", `big-pictures/out/${SLUG}-vocal.mp3`,
               "--words", wordsArg,
               "--out", `big-pictures/out/${SLUG}-pitched.mp3`]);
});

let VOCAL_TRACK = PITCHED;
if (STRETCH) {
  step(`4 · score-stretch (rubberband · ${BPM}bpm · max ${MAX_STRETCH}× · +${ONSET_SHIFT}ms onset)`, () => {
    run("node", ["bin/score-stretch.mjs", "--slug", SLUG, "--section", "all",
                 "--bpm", String(BPM),
                 "--max-stretch", String(MAX_STRETCH),
                 "--onset-shift-ms", String(ONSET_SHIFT),
                 "--in", `big-pictures/out/${SLUG}-pitched.mp3`,
                 "--alignment", `big-pictures/out/${SLUG}-pitched-alignment.json`,
                 "--out", `big-pictures/out/${SLUG}-pitched-stretched.mp3`]);
  });
  VOCAL_TRACK = STRETCHED;
} else {
  console.log(`▸ 4 · score-stretch (skipped — natural pace)\n`);
}

step(`5 · melody-bells (${VOICE} · ${BPM}bpm)`, () => {
  run("node", ["bin/melody-bells.mjs", "--slug", SLUG, "--section", "all",
               "--voice", VOICE, "--transpose", "0", "--gain", "0.45",
               "--bpm", String(BPM), "--inter-verse-beats", "2",
               "--out", `big-pictures/out/${SLUG}-pad.mp3`]);
});

step(`6 · waltz bed (sinebells · ${SCALE} · ${PROGRESSION})`, () => {
  const dur = STRETCH ? computeDuration() + 2 : 0;
  // For natural-paced cuts, size the bed to the WORDS sidecar duration.
  let bedDur = dur;
  if (!STRETCH && existsSync(WORDS)) {
    const w = JSON.parse(readFileSync(WORDS, "utf8"));
    if (w.length) bedDur = Math.ceil(w[w.length - 1].toMs / 1000) + 2;
  }
  run("node", ["bin/waltz.mjs", "general",
               "--voice", "sinebells",
               "--bpm", String(BPM),
               "--scale", SCALE,
               "--transpose", String(TRANSPOSE_BED),
               "--progression", PROGRESSION,
               "--density", "0.35", "--gain", "0.18",
               "--duration", String(bedDur),
               "--seed", `${SLUG}-bp`,
               "--out", BED], RECAP);
});

step("7 · amix (vocal forward · vox 2.5 · pad 0.55 · bed 0.20)", () => {
  // Trim the bed to match the vocal length so it doesn't overhang.
  let trimDur;
  if (STRETCH) trimDur = computeDuration() + 1;
  else {
    const w = JSON.parse(readFileSync(WORDS, "utf8"));
    trimDur = Math.ceil(w[w.length - 1].toMs / 1000) + 1;
  }
  const filter = `[0:a]volume=2.5[v];[1:a]volume=0.55[m];[2:a]volume=0.20,atrim=duration=${trimDur}[b];[v][m][b]amix=inputs=3:duration=longest:dropout_transition=0:weights='1 1 1':normalize=0,atrim=duration=${trimDur + 1}`;
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-i", VOCAL_TRACK, "-i", PAD, "-i", BED,
                 "-filter_complex", filter,
                 "-c:a", "libmp3lame", "-q:a", "2", MIX]);
});

// ── 7.5 · sfx overlay (optional) ─────────────────────────────────────
// If a `<slug>.sfx.txt` sidecar exists, render each cue through
// /api/sfx (ElevenLabs sound effects, content-hash cached by sfx.mjs)
// and lay it onto the mix at its placement time. Sidecar line format:
//   at=SEC gain=G dur=S [loop] : a short sound description
// `at` defaults 0, `gain` defaults 0.7, `dur` omit = auto-length.
let MIXED = MIX;
const SFX_TXT = `${HERE}/${SLUG}.sfx.txt`;
if (existsSync(SFX_TXT)) {
  step("7.5 · sfx (ElevenLabs sound effects)", () => {
    const cues = parseSfxCues(SFX_TXT);
    if (!cues.length) { console.log("  ↪ no cues in sidecar"); return; }
    const sfxDir = `${OUT}/sfx/${SLUG}`;
    mkdirSync(sfxDir, { recursive: true });
    const paths = [];
    cues.forEach((c, i) => {
      const p = `${sfxDir}/${String(i).padStart(2, "0")}.mp3`;
      const args = ["bin/sfx.mjs", "--text", c.prompt, "--out", p];
      if (c.dur != null) args.push("--duration", String(c.dur));
      if (c.loop) args.push("--loop");
      if (FORCE) args.push("--force");
      run("node", args);
      paths.push(p);
    });
    // [0]=mix, [1..n]=cues, each delayed + gained, all amixed (no normalize).
    const inputs = ["-i", MIX];
    for (const p of paths) inputs.push("-i", p);
    const filters = [];
    const labels = ["[0:a]"];
    cues.forEach((c, i) => {
      const d = Math.max(0, Math.round(c.at * 1000));
      filters.push(`[${i + 1}:a]adelay=${d}|${d},volume=${c.gain}[s${i}]`);
      labels.push(`[s${i}]`);
    });
    const amix = `${labels.join("")}amix=inputs=${cues.length + 1}:duration=first:dropout_transition=0:normalize=0`;
    run("ffmpeg", ["-y", "-loglevel", "error", ...inputs,
                   "-filter_complex", [...filters, amix].join(";"),
                   "-c:a", "libmp3lame", "-q:a", "2", MIXSFX]);
    MIXED = MIXSFX;
    console.log(`  ✓ ${cues.length} sfx cue(s) layered`);
  });
} else {
  console.log("▸ 7.5 · sfx (no <slug>.sfx.txt — skipped)\n");
}

let TO_FINAL = MIXED;
if (STAMP) {
  step("8 · stamp (ac signoff · chipmunk + 4-bit crush)", () => {
    if (!existsSync(STAMP_VOCAL)) {
      const tmpTxt = `/tmp/ac-stamp-${process.pid}.txt`;
      writeFileSync(tmpTxt, "aesthetic computer.\n");
      run("node", ["bin/say.mjs", tmpTxt,
                   "--stability", "0.7", "--similarity", "0.9", "--speed", "0.9",
                   "--out", `big-pictures/out/ac-stamp-vocal.mp3`]);
    }
    if (!existsSync(STAMP_FX) || statSync(STAMP_FX).mtimeMs < statSync(STAMP_VOCAL).mtimeMs) {
      run("ffmpeg", ["-y", "-loglevel", "error", "-i", STAMP_VOCAL,
                     "-af", "asetrate=66150,aresample=44100,acrusher=bits=4:mode=lin:aa=1,aformat=sample_rates=22050,aresample=44100,volume=1.6",
                     STAMP_FX]);
    }
    const filter = "[0:a]apad=pad_dur=0.5[song];[1:a]volume=1.0[stamp];[song][stamp]concat=n=2:v=0:a=1";
    run("ffmpeg", ["-y", "-loglevel", "error",
                   "-i", MIXED, "-i", STAMP_FX,
                   "-filter_complex", filter,
                   "-c:a", "libmp3lame", "-q:a", "2", STAMPED]);
  });
  TO_FINAL = STAMPED;
} else {
  console.log("▸ 8 · stamp (skipped)\n");
}

step("9 · finalize (ID3 + cover + lyrics)", () => {
  run("node", ["bin/finalize.mjs",
               "--in", TO_FINAL.replace(POP + "/", ""),
               "--slug", SLUG,
               "--title", TITLE,
               "--out", FINAL.replace(POP + "/", ""),
               "--force"]);
});

copyFileSync(FINAL, DESK);
const dt = ((Date.now() - t0) / 1000).toFixed(1);
const sz = (statSync(DESK).size / 1024 / 1024).toFixed(2);
console.log(`\n✓ ~/Desktop/${SLUG}.mp3   ${sz} MB · pipeline ${dt}s`);
