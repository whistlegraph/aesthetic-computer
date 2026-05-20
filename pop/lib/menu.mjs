// menu.mjs — the pop menu. Single declarative registry of every
// instrument, fx, percussion voice, bed, vocal pipeline, scale, and form
// that lives in /pop. Adding an entry here makes it pickable from the
// `pop play` CLI and any future track scaffold.
//
// Categories:
//   instruments  — tonal voices. signature: mixEventX(ev, out, opts)
//                  where ev = { startSec, midi, durSec, gain }
//   fx           — buffer transforms. signature: applyX(buf, opts)
//                  (mutates buf in place; supports startSec/endSec opts)
//   percussion   — AC percussion kit (one-shot triggers)
//   beds         — continuous textures (still inline per-lane)
//   master       — bus/master chain stages (reverb, sidechain, softclip)
//   pitch_time   — offline pitch/time transforms (autotune, rubberband)
//   vocal        — TTS + alignment + per-word placement pipelines
//   scales       — interval sets keyed by tonic
//   forms        — arrangement templates (trance-bbd, chorale-16, …)
//
// The CLI loader only handles `instruments` + `fx` today; the rest is
// here so the menu stays the single source of truth as we wire more.

import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP = resolve(HERE, "..");

export const MENU = {
  instruments: {
    supersaw: {
      file: "dance/synths/supersaw.mjs", export: "mixEventSupersaw",
      presets: ["lead", "pad", "stab"], lane: "dance",
      params: ["voices", "detuneCents", "attack", "decay"],
      blurb: "JP-8000 detuned-saw stack (7 voices)",
    },
    sinepower: {
      file: "dance/synths/sinepower.mjs", export: "mixEventSinePower",
      presets: ["lead", "pad", "stab"], lane: "dance",
      params: ["partials", "attack", "decay"],
      blurb: "stacked harmonic sines, no aliasing",
    },
    skrill: {
      file: "dance/synths/skrill.mjs", export: "mixEventSkrill",
      presets: ["growl", "talk", "reese", "screech", "sub"], lane: "dance",
      params: ["modRatio", "fmIndex", "vowelA", "vowelB", "lfo", "q", "drive", "crush", "subGain"],
      blurb: "FM talking-bass w/ swept formants",
    },
    hoover: {
      file: "hippyhayzard/synths/hoover.mjs", export: "mixEventHoover",
      presets: ["whoop", "stab", "hazard", "pad"], lane: "hippyhayzard",
      params: ["voices", "detune", "whoop", "cutoff", "q", "drive", "subGain"],
      blurb: "Alpha-Juno 'mentasm' w/ pitch whoop",
    },
    zitar: {
      file: "hippyhayzard/synths/zitar.mjs", export: "mixEventZitar",
      presets: ["sitar", "lead", "drone", "dry"], lane: "hippyhayzard",
      params: ["stretch", "damp", "jawari", "taraf", "symGain", "chikari"],
      blurb: "Karplus-Strong sitar + jawari buzz",
    },
  },

  fx: {
    wobble: {
      file: "dance/synths/fx.mjs", export: "applyWobble",
      params: ["target", "rate", "depth", "baseCutoffHz", "waveform"],
      blurb: "LFO on amp or filter (trance/dubstep wub)",
    },
    bitcrush: {
      file: "dance/synths/fx.mjs", export: "applyBitcrush",
      params: ["bits", "downsample", "mix"],
      blurb: "bit-depth + sample-rate reduction",
    },
    flange: {
      file: "dance/synths/fx.mjs", export: "applyFlange",
      params: ["rate", "depthMs", "baseDelayMs", "feedback", "mix"],
      blurb: "LFO-swept delay line w/ feedback",
    },
  },

  // ── declared but not yet wired into the play CLI ─────────────────────

  percussion: {
    note: "AC percussion kit — `playPercussion(sound, letter, opts)`. Lives in system/public/aesthetic.computer/lib/percussion.mjs.",
    voices: ["kick", "snare", "clap", "snap", "hat-c", "hat-o", "ride", "crash", "splash", "cowbell", "block", "tambo"],
  },

  beds: {
    ocean:          { lane: "chillwave",     blurb: "pink-noise LP w/ slow 8s wave LFO",        inline: "chillwave/bin/render.mjs" },
    "noise-sweep":  { lane: "chillwave",     blurb: "very slow white-noise filter sweep",       inline: "chillwave/bin/render.mjs" },
    "sacred-drone": { lane: "big-pictures",  blurb: "3-voice human-hum drone w/ breath LFO",    inline: "big-pictures/bin/sacred-drone.mjs" },
    "cool-sine":    { lane: "big-pictures",  blurb: "sub + pad + sparkle sine layer",           inline: "big-pictures/bin/cool-sine-layer.mjs" },
    sinebells:      { lane: "shared",        blurb: "struck bell partials, T60 decay",          inline: "bin/melody-bells.mjs" },
  },

  master: {
    "stereo-reverb": { lane: "jungle", blurb: "Freeverb-lite (comb + allpass)",       inline: "jungle/bin/render.mjs" },
    sidechain:       {                  blurb: "kick-triggered duck on sub/pad",       inline: "per-lane (implicit envelope gate)" },
    softclip:        { file: "dance/synths/fx.mjs", export: "softClip", params: ["drive"], blurb: "tanh saturation glue" },
  },

  pitch_time: {
    autotune:        { file: "bin/autotune.py",        blurb: "WORLD f0 snap (note/frame)",         params: ["key", "scale", "strength", "preserve", "glide-ms"] },
    "score-pitch":   { file: "bin/score-pitch.mjs",    blurb: "MIDI-driven rubberband pitch shift" },
    "score-stretch": { file: "bin/score-stretch.mjs",  blurb: "rubberband time-fit (no pitch shift)" },
    "pitchsnap-world": { file: "bin/pitchsnap_world.py", blurb: "WORLD pitch correction variant" },
  },

  vocal: {
    "jeffrey-pvc":  { file: "bin/say.mjs",         blurb: "ElevenLabs jeffrey-pvc TTS (stability ≥ 0.5)", params: ["speed", "style", "stability", "similarity"] },
    "score-render": { file: "bin/score-render.mjs", blurb: "per-word vocal placement onto a .np score" },
    "mfa-align":    { file: "bin/mfa-align.mjs",    blurb: "Montreal Forced Aligner word/phoneme timing" },
    "sacred-hum":   { file: "big-pictures/bin/sacred-drone.mjs", blurb: "3-voice non-vocal human-hum drone" },
  },

  scales: {
    minor:        [0, 2, 3, 5, 7, 8, 10],
    major:        [0, 2, 4, 5, 7, 9, 11],
    "minor-pent": [0, 3, 5, 7, 10],
    "major-pent": [0, 2, 4, 7, 9],
    dorian:       [0, 2, 3, 5, 7, 9, 10],
    phrygian:     [0, 1, 3, 5, 7, 8, 10],
    chromatic:    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
  },

  forms: {
    "trance-bbd":   { lane: "dance",        blurb: "breakdown → build → drop (138 bpm minor)" },
    "chorale-16":   { lane: "hippyhayzard", blurb: "16-bar Bach chorale, borrowed iv + deceptive cadence" },
    "ballad-32":    { lane: "hippyhayzard", blurb: "32-bar nightcore-ballad form (~1:28)" },
    "williams-aba": { lane: "hellsine",     blurb: "ABA' heroic-leitmotif arc, climax key lift" },
    "jungle-16":    { lane: "jungle",       blurb: "16-bar chopped-break (jungletón / raggasol / rodando)" },
  },

  modulation: {
    "ad-env":         { blurb: "linear attack → exponential decay (AC sound.synth contract)" },
    "lfo":            { blurb: "sine/tri/square · free Hz or beat-synced (1/8, 1/4, 1/8t, 1/8d)" },
    "pitch-env":      { blurb: "bend-down → spring-up (the hoover 'whoop')", file: "hippyhayzard/synths/hoover.mjs" },
    "breakpoint-env": { blurb: "[{time, ...params}] linear-interp ramps; honored by fx + beds", file: "dance/synths/fx.mjs" },
    "vibrato":        { blurb: "sine pitch mod, rate + depth (cents)" },
  },

  score: {
    ".np":             { blurb: "notepat — `NOTE:syllable*beats` cells, space-separated; sections via `verse N` / `#`" },
    "midi-to-np":      { file: "bin/midi_to_np.py",      blurb: "MIDI file → .np converter" },
    "musicxml-to-np":  { file: "bin/musicxml_to_np.py",  blurb: "MusicXML → .np converter (preferred for hymn imports)" },
    "os-to-np":        { file: "bin/os_to_np.py",        blurb: "Open Score → .np converter" },
    ".txt":            { blurb: "paired lyric file, one line per .np section" },
  },
};

async function loadByEntry(entry) {
  if (!entry?.file || !entry?.export) return null;
  if (!entry.file.endsWith(".mjs")) return null;
  const mod = await import(resolve(POP, entry.file));
  return mod[entry.export] ?? null;
}

export async function loadInstrument(name) {
  const e = MENU.instruments[name];
  if (!e) throw new Error(`unknown instrument: ${name}`);
  const fn = await loadByEntry(e);
  if (!fn) throw new Error(`instrument ${name} not loadable from ${e.file}:${e.export}`);
  return fn;
}

export async function loadFx(name) {
  const e = MENU.fx[name] || MENU.master[name];
  if (!e) throw new Error(`unknown fx: ${name}`);
  const fn = await loadByEntry(e);
  if (!fn) throw new Error(`fx ${name} not loadable from ${e.file}:${e.export}`);
  // softClip has signature (buf, drive); wrap to the (buf, opts) contract.
  if (name === "softclip") return (buf, opts = {}) => fn(buf, opts.drive ?? 1.0);
  return fn;
}
