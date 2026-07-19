// paths.mjs — the spinging map. One place that knows where every vocal
// tool in the repo lives, so the CLI router and future lib modules never
// hardcode a path twice.
//
// Nothing here spawns anything; it only resolves. Python tools run under
// pop/.venv (pyworld, librosa, soundfile, resemblyzer live there).

import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { existsSync } from "node:fs";

const HERE = dirname(fileURLToPath(import.meta.url));
export const REPO = resolve(HERE, "..", "..");

export const POP = resolve(REPO, "pop");
export const RECAP = resolve(REPO, "recap");
export const LIVE = resolve(REPO, "live");
export const PODCAST = resolve(REPO, "marketing", "podcast");

// Python interpreter with the WORLD stack (pyworld/librosa/soundfile).
export const VENV_PY = resolve(POP, ".venv", "bin", "python");

// whisper.cpp model the pipelines share.
export const WHISPER_MODEL = resolve(RECAP, "models", "ggml-base.en.bin");

// ── tool registry: canonical homes of the spinging chain ────────────────
export const TOOLS = {
  // text → spoken audio (/api/say, provider "jeffrey"; voice ids live
  // server-side + in the vault — never in this repo)
  say: resolve(POP, "bin", "say.mjs"),
  sayDry: resolve(POP, "bin", "say-dry.mjs"),
  perword: resolve(POP, "bin", "perword.mjs"),
  bestOfTakes: resolve(POP, "bin", "best-of-takes.mjs"),
  recapTts: resolve(RECAP, "bin", "tts.mjs"),
  profileVocal: resolve(POP, "bin", "profile-vocal.mjs"),

  // metadata: word boundaries, onsets, phonetic events, f0
  align: resolve(POP, "bin", "align.mjs"),
  mfaAlign: resolve(POP, "bin", "mfa-align.mjs"),
  refineWords: resolve(POP, "bin", "refine_words.py"),
  detectOnsets: resolve(POP, "bin", "detect_onsets.py"),
  phonetics: resolve(RECAP, "bin", "phonetics.mjs"),
  floor: resolve(LIVE, "bin", "floor.py"),
  pitchcheck: resolve(POP, "bin", "pitchcheck.mjs"),

  // spoken → sung (WORLD)
  pitchsnap: resolve(POP, "bin", "pitchsnap.mjs"),
  pitchsnapWorld: resolve(POP, "bin", "pitchsnap_world.py"),
  scorePitch: resolve(POP, "bin", "score-pitch.mjs"),
  autotune: resolve(POP, "bin", "autotune.py"),
  timefit: resolve(POP, "bin", "timefit.mjs"),
  pitchwords: resolve(POP, "bin", "pitchwords.mjs"),
  melodyBells: resolve(POP, "bin", "melody-bells.mjs"),

  // sources (other voices in)
  liveIngest: resolve(LIVE, "bin", "ingest.mjs"),
  sampleFromYoutube: resolve(POP, "bin", "sample-from-youtube.mjs"),
  separateStems: resolve(POP, "bin", "separate-stems.mjs"),
  identifySpeaker: resolve(POP, "bin", "identify-speaker.py"),

  // finishing
  master: resolve(PODCAST, "bin", "master.mjs"),

  // pending adoption (in-flight; see README) — referenced, never imported
  singJingle: resolve(POP, "menuband", "bin", "sing-jingle.mjs"),
  singWordWorld: resolve(POP, "menuband", "bin", "sing_word_world.py"),
};

export function checkEnv() {
  return {
    venvPython: existsSync(VENV_PY),
    whisperModel: existsSync(WHISPER_MODEL),
  };
}
