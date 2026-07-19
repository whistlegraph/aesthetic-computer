#!/usr/bin/env node
// spinging.mjs — one door into the vocal stack.
//
// spinging (jeffrey): "moving spoken audio — metadata'd up and generated —
// into singing, out of text." Text becomes generated spoken TTS audio,
// gets enriched with metadata (word/phoneme boundaries, f0, timing), and
// is lifted into singing.
//
// v1 subcommands are thin routers: each one execs the existing tool in
// its canonical home (see spinging/lib/paths.mjs) with your args passed
// straight through. The tools keep working from their old paths too —
// nothing broke to make this exist.
//
// Usage:
//   spinging <subcommand> [args…]        (args go to the underlying tool)
//   spinging help                         list subcommands
//   spinging doctor                       check the python env + model
//
// Subcommands:
//   say        pop/bin/say.mjs             lyric file → /api/say spoken stem (cached)
//   say-dry    pop/bin/say-dry.mjs         say + dryness validation + retry
//   tts        recap/bin/tts.mjs           recap audience narration → recap.mp3
//   align      pop/bin/align.mjs           whisper-cli word timestamps for a stem
//   refine     pop/bin/refine_words.py     snap word boundaries to librosa onsets
//   phonemes   recap/bin/phonetics.mjs     audio-side phonetic events (silence/voice/plosive)
//   floor      live/bin/floor.py           measure a speaker's f0 floor (run per source!)
//   snap       pop/bin/pitchsnap.mjs       per-word grid snap + pitch to .np score
//   worldsnap  pop/bin/pitchsnap_world.py  WORLD f0-replacement on one slice
//   sing       pop/bin/score-pitch.mjs     line-level WORLD pass — whole vocal sings the score
//   autotune   pop/bin/autotune.py         WORLD scale-snap (note or frame mode)
//   check      pop/bin/pitchcheck.mjs      audit rendered f0 vs intent (cents drift)
//   ingest     live/bin/ingest.mjs         youtube url → words.json corpus (talking heads)
//   sample     pop/bin/sample-from-youtube.mjs  youtube → onset-aligned chops + index
//   stems      pop/bin/separate-stems.mjs  demucs vocal/drums/bass/other split
//   master     marketing/podcast/bin/master.mjs  loudnorm finishing (see README for -14 LUFS)
//   pronounce  spinging/lib/pronounce.mjs  word → curated IPA + syllables (Wiktionary→espeak)
//   goalposts  spinging/lib/goalposts.py   reference sung wavs → shape percentile bands
//   singline   spinging/lib/sing_line_world.py  plan.json → sung line (the round-3 engine)
//   bus        spinging/lib/vocal_bus.py   reverb halo on the vocal bus / click scan

import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { TOOLS, VENV_PY, checkEnv } from "../lib/paths.mjs";

const ROUTES = {
  say: { path: TOOLS.say, runner: "node" },
  "say-dry": { path: TOOLS.sayDry, runner: "node" },
  tts: { path: TOOLS.recapTts, runner: "node" },
  align: { path: TOOLS.align, runner: "node" },
  refine: { path: TOOLS.refineWords, runner: "python" },
  phonemes: { path: TOOLS.phonetics, runner: "node" },
  floor: { path: TOOLS.floor, runner: "python" },
  snap: { path: TOOLS.pitchsnap, runner: "node" },
  worldsnap: { path: TOOLS.pitchsnapWorld, runner: "python" },
  sing: { path: TOOLS.scorePitch, runner: "node" },
  autotune: { path: TOOLS.autotune, runner: "python" },
  check: { path: TOOLS.pitchcheck, runner: "node" },
  ingest: { path: TOOLS.liveIngest, runner: "node" },
  sample: { path: TOOLS.sampleFromYoutube, runner: "node" },
  stems: { path: TOOLS.separateStems, runner: "node" },
  master: { path: TOOLS.master, runner: "node" },
  // round 3 — the sing core lives in spinging/lib
  pronounce: { path: TOOLS.pronounce, runner: "node" },
  goalposts: { path: TOOLS.goalposts, runner: "python" },
  singline: { path: TOOLS.singLineWorld, runner: "python" },
  bus: { path: TOOLS.vocalBus, runner: "python" },
};

const [cmd, ...rest] = process.argv.slice(2);

function help() {
  console.log("spinging — text → spoken TTS → metadata → singing\n");
  const rel = (p) => p.replace(/^.*aesthetic-computer\//, "");
  for (const [name, r] of Object.entries(ROUTES)) {
    console.log(`  ${name.padEnd(10)} → ${rel(r.path)}`);
  }
  console.log("  doctor     → check python env (pyworld) + whisper model");
  console.log("\nSing core (round 3, adopted): spinging/lib/{pronounce.mjs, notation.mjs,");
  console.log("  sing_line_world.py, vocal_shapes.py, goalposts.py, vocal_bus.py} —");
  console.log(`  first caller: ${rel(TOOLS.singJingle)}`);
}

if (!cmd || cmd === "help" || cmd === "--help" || cmd === "-h") {
  help();
  process.exit(0);
}

if (cmd === "doctor") {
  const env = checkEnv();
  console.log(`venv python (pop/.venv): ${env.venvPython ? "ok" : "MISSING"}`);
  console.log(`whisper model (recap/models): ${env.whisperModel ? "ok" : "MISSING"}`);
  const missing = Object.entries(ROUTES).filter(([, r]) => !existsSync(r.path));
  if (missing.length) {
    for (const [name, r] of missing) console.log(`route ${name}: MISSING ${r.path}`);
    process.exit(1);
  }
  console.log(`routes: all ${Object.keys(ROUTES).length} resolve`);
  process.exit(env.venvPython && env.whisperModel ? 0 : 1);
}

const route = ROUTES[cmd];
if (!route) {
  console.error(`spinging: unknown subcommand "${cmd}" — try \`spinging help\``);
  process.exit(2);
}

const exe = route.runner === "python" ? VENV_PY : process.execPath;
const r = spawnSync(exe, [route.path, ...rest], { stdio: "inherit" });
process.exit(r.status ?? 1);
