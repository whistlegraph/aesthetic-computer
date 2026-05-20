#!/usr/bin/env node
// place-penta-harmony.mjs — chordal/wandering jeffrey vocal bus with
// per-entry treatments that fake phoneme variety from the single hum
// source. Each entry: pitch-shift (semis, speed-preserving), per-entry
// fx treatment (vibrato/tremolo/bitcrush/phaser/reverse/formant), wider
// stereo placement. The result dances with the Odyssey theremin and
// sounds like distinct phoneme calls rather than the same repeated hum.

import { spawnSync } from "node:child_process";
import { resolve } from "node:path";
import { homedir } from "node:os";
import { existsSync } from "node:fs";

const flags = {};
for (let i = 2; i < process.argv.length; i += 2) {
  flags[process.argv[i].replace(/^--/, "")] = process.argv[i + 1];
}
const expandHome = (p) => p?.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p;
const PHRASE = resolve(process.cwd(), expandHome(flags.phrase));
const OUT    = resolve(process.cwd(), expandHome(flags.out));
const TOTAL  = Number(flags.dur);
const SR     = 44100;
if (!existsSync(PHRASE)) { console.error(`✗ phrase missing: ${PHRASE}`); process.exit(1); }

const probe = spawnSync("ffprobe", [
  "-v","error","-show_entries","format=duration",
  "-of","default=noprint_wrappers=1:nokey=1", PHRASE], { encoding: "utf8" });
const PH = parseFloat(probe.stdout.trim()) || 6.0;

// Per-entry treatment FX chains. Each adds character to the hum so it
// reads as a distinct phoneme call rather than the same repeated tone.
const TREATMENTS = {
  // SMOOTHED treatments — no aggressive volume gating that read as
  // "guttural repetition" per @jeffrey. Tremolo dropped to gentle
  // pulses only; bitcrush mellowed; vibrato kept since pitch wobble is
  // the smooth theremin-y feel jeffrey wants.
  // SLOWED + tremolo REMOVED entirely per @jeffrey (guttural rep was
  // the volume gating). Each treatment now: light vibrato (1.5-3 Hz)
  // + atempo 0.7-0.8 (stretches the hum slower) + the formant/character
  // FX. No volume gating anywhere — pure smooth held vowel calls.
  vibrato:      "atempo=0.75,vibrato=f=2.0:d=0.30",
  vibrato_fast: "atempo=0.80,vibrato=f=3.0:d=0.40",
  tremolo:      "atempo=0.70,vibrato=f=1.8:d=0.25",
  tremolo_slow: "atempo=0.75,vibrato=f=1.5:d=0.20",
  bitcrush:     "atempo=0.75,aresample=14000,aresample=44100,vibrato=f=2.0:d=0.20",
  bitcrush_hot: "atempo=0.75,aresample=12000,aresample=44100,vibrato=f=2.5:d=0.25",
  phaser:       "atempo=0.75,aphaser=in_gain=0.85:out_gain=0.95:delay=3:decay=0.45:speed=0.25",
  bright:       "atempo=0.75,highpass=f=350,equalizer=f=2400:t=q:w=1.2:g=4,vibrato=f=2.0:d=0.30",
  deep:         "atempo=0.70,lowpass=f=1200,equalizer=f=400:t=q:w=1.0:g=3,vibrato=f=1.5:d=0.30",
  reverse:      "atempo=0.75,areverse,vibrato=f=2.0:d=0.25",
  mumble:       "atempo=0.75,bandpass=f=750:width_type=q:width=0.6,vibrato=f=2.0:d=0.35",
  hollow:       "atempo=0.70,lowpass=f=1500,aphaser=in_gain=0.7:out_gain=1.0:delay=4:decay=0.5:speed=0.2",
};

// 9 entries spread across the song. Each gets a distinct treatment so
// the perceived phonemes vary. BATTLE bars (1:28-1:35) intentionally
// left clear — theremin carries the encounter alone.
// CHILL pass per @jeffrey — vocals were too high + loud. Pitches
// brought down (max +5 was +12, max -7 stays), volumes uniformly
// pulled ~6 dB (× 0.5) so the vocal sits well under the bed.
const entries = [
  { name: "j1-low-tremolo",  start:  19.0, semi: -7, len: 7.0,  fi: 2.0, fo: 3.0, vol: 0.22, pan: -0.65, treat: "tremolo"      },
  { name: "j2-mid-bright",   start:  36.0, semi: -2, len: 8.0,  fi: 2.5, fo: 3.5, vol: 0.26, pan:  0.55, treat: "bright"       },
  { name: "j3-mid-mumble",   start:  55.0, semi:  0, len: 7.0,  fi: 2.5, fo: 3.5, vol: 0.28, pan: -0.40, treat: "mumble"       },
  { name: "j4-low-deep",     start:  72.0, semi: -4, len: 9.0,  fi: 3.0, fo: 4.0, vol: 0.30, pan:  0.70, treat: "deep"         },
  // BATTLE (1:28-1:35) — clear, theremin solo
  { name: "j5-mid-phaser",   start: 110.0, semi: +2, len: 8.0,  fi: 3.0, fo: 4.0, vol: 0.30, pan: -0.80, treat: "phaser"       },
  { name: "j6-bitcrunch",    start: 128.0, semi: -2, len: 8.0,  fi: 3.0, fo: 4.0, vol: 0.26, pan:  0.55, treat: "bitcrush_hot" },
  { name: "j7-low-reverse",  start: 145.0, semi: -5, len: 7.5,  fi: 3.0, fo: 4.0, vol: 0.26, pan: -0.30, treat: "reverse"      },
  { name: "j8-mid-fast",     start: 156.0, semi: +5, len: 6.0,  fi: 2.5, fo: 3.0, vol: 0.20, pan:  0.75, treat: "vibrato_fast" },
  { name: "j9-hollow-tonic", start: 172.0, semi: -2, len: 12.0, fi: 4.0, fo: 7.0, vol: 0.22, pan:  0.10, treat: "hollow"       },
];

const inputs = [];
const parts = [];
let placed = 0;
for (const e of entries) {
  const ratio = Math.pow(2, e.semi / 12);
  const atempoRatio = 1 / ratio;
  const playLen = Math.min(PH, e.len);
  const fi = Math.min(e.fi, playLen / 3);
  const fo = Math.min(e.fo, playLen / 2);
  const delayMs = Math.round(e.start * 1000);
  const idx = placed;
  const fx = TREATMENTS[e.treat] || TREATMENTS.vibrato;
  inputs.push("-i", PHRASE);
  parts.push(
    `[${idx}:a]` +
    `asetrate=${Math.round(SR * ratio)},aresample=${SR},atempo=${atempoRatio.toFixed(6)},` +
    `atrim=0:${playLen.toFixed(3)},asetpts=N/SR/TB,` +
    `${fx},` +
    `pan=stereo|c0=${(1 - e.pan).toFixed(3)}*c0|c1=${(1 + e.pan).toFixed(3)}*c0,` +
    `volume=${e.vol.toFixed(3)},` +
    `afade=t=in:st=0:d=${fi.toFixed(3)},` +
    `afade=t=out:st=${(playLen - fo).toFixed(3)}:d=${fo.toFixed(3)},` +
    `adelay=${delayMs}|${delayMs}[e${idx}]`
  );
  placed++;
}
if (placed === 0) { console.error("✗ no harmony entries"); process.exit(1); }
const lbls = parts.map((_, i) => `[e${i}]`).join("");
const filter =
  parts.join(";") +
  `;${lbls}amix=inputs=${placed}:duration=longest:dropout_transition=0:normalize=0[mix];` +
  `[mix]apad=whole_dur=${TOTAL.toFixed(3)},atrim=0:${TOTAL.toFixed(3)},asetpts=N/SR/TB[out]`;

const r = spawnSync("ffmpeg", [
  "-hide_banner","-y","-loglevel","error",
  ...inputs,
  "-filter_complex", filter,
  "-map","[out]",
  "-ar","44100","-ac","2","-c:a","pcm_s16le",
  OUT,
], { stdio: ["ignore","inherit","inherit"] });
if (r.status !== 0) { console.error("✗ harmony bus render failed"); process.exit(1); }

const p2 = spawnSync("ffprobe", ["-v","error","-show_entries",
  "format=duration","-of","default=noprint_wrappers=1:nokey=1", OUT],
  { encoding: "utf8" });
console.log(`✓ ${OUT} (${parseFloat(p2.stdout.trim()).toFixed(2)}s · ${placed} differentiated harmony entries)`);
for (const e of entries) console.log(`    ${(e.start.toFixed(1)+"s").padStart(7)}  ${(e.semi>=0?"+":"")+e.semi}st  pan=${e.pan>0?"+":""}${e.pan.toFixed(2)}  ${e.treat}`);
