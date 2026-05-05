#!/usr/bin/env node
// gm-bake.mjs — One-shot bake of a FOSS SoundFont into per-instrument MP3 packs
// for the General MIDI bank. Output is structured for upload to
// assets.aesthetic.computer/gm/* (run `npm run assets:sync:up` after).
//
// Default SoundFont: GeneralUser GS v1.471 by S. Christian Collins
//   https://schristiancollins.com/generaluser.php
// Override with $GM_SF2=/path/to/file.sf2 if the upstream URL drifts.
//
// Required tools (both expected on lith; check + install hints printed below):
//   fluidsynth    brew install fluid-synth        |  apt install fluidsynth
//   ffmpeg        brew install ffmpeg              |  apt install ffmpeg
//
// Usage:
//   node lith/scripts/gm-bake.mjs                      # melodic + drums, step=3
//   node lith/scripts/gm-bake.mjs --note-step=1        # render every semitone
//   node lith/scripts/gm-bake.mjs --note-step=6        # coarse, smaller bundle
//   node lith/scripts/gm-bake.mjs --only=0,1,24        # subset of program IDs
//   node lith/scripts/gm-bake.mjs --skip-drums
//   node lith/scripts/gm-bake.mjs --out=/tmp/gm        # alt output dir
//   node lith/scripts/gm-bake.mjs --dry-run            # plan only, no render
//
// Re-runnable: skips MP3s that already exist on disk.
// Output tree (default):
//   lith/cache/                          (downloaded SF2)
//   lith/scripts/out/gm/manifest.json
//   lith/scripts/out/gm/LICENSE.txt
//   lith/scripts/out/gm/000/A4.mp3       (program 0 = Acoustic Grand, A4)
//   lith/scripts/out/gm/000/Cs4.mp3      (C#4 — sharps spelled with 's')
//   ...
//   lith/scripts/out/gm/drum-000/35.mp3  (Standard Kit, MIDI note 35)
//   ...

import { spawnSync, execFileSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  writeFileSync,
  rmSync,
  statSync,
} from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const LITH_DIR = resolve(__dirname, "..");
const CACHE_DIR = join(LITH_DIR, "cache");

// ─── ANSI ───────────────────────────────────────────────────────────────────
const C = {
  red: "\x1b[0;31m",
  green: "\x1b[0;32m",
  yellow: "\x1b[1;33m",
  dim: "\x1b[2m",
  reset: "\x1b[0m",
};
const log = (msg) => console.log(`${C.green}->${C.reset} ${msg}`);
const warn = (msg) => console.warn(`${C.yellow}!${C.reset} ${msg}`);
const die = (msg) => {
  console.error(`${C.red}x${C.reset} ${msg}`);
  process.exit(1);
};

// ─── CLI args ───────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flag = (name, fallback = undefined) => {
  for (const a of argv) {
    if (a === `--${name}`) return true;
    if (a.startsWith(`--${name}=`)) return a.slice(name.length + 3);
  }
  return fallback;
};
const NOTE_STEP = parseInt(flag("note-step", "3"), 10);
const ONLY = flag("only");
const SKIP_DRUMS = flag("skip-drums", false) === true;
const SKIP_MELODIC = flag("skip-melodic", false) === true;
const DRY_RUN = flag("dry-run", false) === true;
const OUT_DIR = resolve(flag("out", join(__dirname, "out", "gm")));

if (!Number.isInteger(NOTE_STEP) || NOTE_STEP < 1 || NOTE_STEP > 12) {
  die(`--note-step must be an integer 1..12 (got ${NOTE_STEP})`);
}

// ─── GM patch names (program 0..127) ────────────────────────────────────────
// Standard General MIDI Level 1 melodic bank.
const GM_PATCHES = [
  "Acoustic Grand Piano", "Bright Acoustic Piano", "Electric Grand Piano",
  "Honky-tonk Piano", "Electric Piano 1", "Electric Piano 2", "Harpsichord",
  "Clavi",
  "Celesta", "Glockenspiel", "Music Box", "Vibraphone", "Marimba", "Xylophone",
  "Tubular Bells", "Dulcimer",
  "Drawbar Organ", "Percussive Organ", "Rock Organ", "Church Organ",
  "Reed Organ", "Accordion", "Harmonica", "Tango Accordion",
  "Acoustic Guitar (nylon)", "Acoustic Guitar (steel)",
  "Electric Guitar (jazz)", "Electric Guitar (clean)",
  "Electric Guitar (muted)", "Overdriven Guitar", "Distortion Guitar",
  "Guitar harmonics",
  "Acoustic Bass", "Electric Bass (finger)", "Electric Bass (pick)",
  "Fretless Bass", "Slap Bass 1", "Slap Bass 2", "Synth Bass 1", "Synth Bass 2",
  "Violin", "Viola", "Cello", "Contrabass", "Tremolo Strings",
  "Pizzicato Strings", "Orchestral Harp", "Timpani",
  "String Ensemble 1", "String Ensemble 2", "SynthStrings 1", "SynthStrings 2",
  "Choir Aahs", "Voice Oohs", "Synth Voice", "Orchestra Hit",
  "Trumpet", "Trombone", "Tuba", "Muted Trumpet", "French Horn",
  "Brass Section", "SynthBrass 1", "SynthBrass 2",
  "Soprano Sax", "Alto Sax", "Tenor Sax", "Baritone Sax",
  "Oboe", "English Horn", "Bassoon", "Clarinet",
  "Piccolo", "Flute", "Recorder", "Pan Flute",
  "Blown Bottle", "Shakuhachi", "Whistle", "Ocarina",
  "Lead 1 (square)", "Lead 2 (sawtooth)", "Lead 3 (calliope)",
  "Lead 4 (chiff)", "Lead 5 (charang)", "Lead 6 (voice)",
  "Lead 7 (fifths)", "Lead 8 (bass + lead)",
  "Pad 1 (new age)", "Pad 2 (warm)", "Pad 3 (polysynth)", "Pad 4 (choir)",
  "Pad 5 (bowed)", "Pad 6 (metallic)", "Pad 7 (halo)", "Pad 8 (sweep)",
  "FX 1 (rain)", "FX 2 (soundtrack)", "FX 3 (crystal)", "FX 4 (atmosphere)",
  "FX 5 (brightness)", "FX 6 (goblins)", "FX 7 (echoes)", "FX 8 (sci-fi)",
  "Sitar", "Banjo", "Shamisen", "Koto", "Kalimba", "Bag pipe", "Fiddle",
  "Shanai",
  "Tinkle Bell", "Agogo", "Steel Drums", "Woodblock", "Taiko Drum",
  "Melodic Tom", "Synth Drum", "Reverse Cymbal",
  "Guitar Fret Noise", "Breath Noise", "Seashore", "Bird Tweet",
  "Telephone Ring", "Helicopter", "Applause", "Gunshot",
];
if (GM_PATCHES.length !== 128) {
  die(`internal error: GM_PATCHES length = ${GM_PATCHES.length}, expected 128`);
}

// GM Level 1 standard kit note names (MIDI notes 35..81).
const GM_DRUM_KIT_NOTE_NAMES = {
  35: "Acoustic Bass Drum", 36: "Bass Drum 1", 37: "Side Stick",
  38: "Acoustic Snare", 39: "Hand Clap", 40: "Electric Snare",
  41: "Low Floor Tom", 42: "Closed Hi Hat", 43: "High Floor Tom",
  44: "Pedal Hi-Hat", 45: "Low Tom", 46: "Open Hi-Hat", 47: "Low-Mid Tom",
  48: "Hi-Mid Tom", 49: "Crash Cymbal 1", 50: "High Tom", 51: "Ride Cymbal 1",
  52: "Chinese Cymbal", 53: "Ride Bell", 54: "Tambourine", 55: "Splash Cymbal",
  56: "Cowbell", 57: "Crash Cymbal 2", 58: "Vibraslap", 59: "Ride Cymbal 2",
  60: "Hi Bongo", 61: "Low Bongo", 62: "Mute Hi Conga", 63: "Open Hi Conga",
  64: "Low Conga", 65: "High Timbale", 66: "Low Timbale", 67: "High Agogo",
  68: "Low Agogo", 69: "Cabasa", 70: "Maracas", 71: "Short Whistle",
  72: "Long Whistle", 73: "Short Guiro", 74: "Long Guiro", 75: "Claves",
  76: "Hi Wood Block", 77: "Low Wood Block", 78: "Mute Cuica", 79: "Open Cuica",
  80: "Mute Triangle", 81: "Open Triangle",
};

// ─── note helpers ───────────────────────────────────────────────────────────
const NOTE_NAMES = [
  "C", "Cs", "D", "Ds", "E", "F", "Fs", "G", "Gs", "A", "As", "B",
];
function midiToName(m) {
  const pc = NOTE_NAMES[m % 12];
  const oct = Math.floor(m / 12) - 1; // MIDI 0 = C-1, 60 = C4
  return `${pc}${oct}`;
}

// ─── tool checks ────────────────────────────────────────────────────────────
function which(bin) {
  const r = spawnSync("command", ["-v", bin], { shell: true });
  return r.status === 0;
}
function requireTool(bin, hint) {
  if (!which(bin)) {
    console.error(`${C.red}x${C.reset} missing required tool: ${bin}`);
    console.error(`  install: ${hint}`);
    process.exit(1);
  }
}
if (!DRY_RUN) {
  requireTool(
    "fluidsynth",
    "macOS:  brew install fluid-synth\n           Linux:  apt install fluidsynth",
  );
  requireTool(
    "ffmpeg",
    "macOS:  brew install ffmpeg\n           Linux:  apt install ffmpeg",
  );
}

// ─── SoundFont resolution ───────────────────────────────────────────────────
// Christian Collins's official GitHub mirror — stable raw download, unlike
// the schristiancollins.com page (now an SPA whose download URLs flux).
const GENERALUSER_URL =
  "https://raw.githubusercontent.com/mrbumpy409/GeneralUser-GS/main/GeneralUser-GS.sf2";
const GENERALUSER_NAME = "GeneralUser GS v1.471";
const GENERALUSER_LICENSE = `${GENERALUSER_NAME}
by S. Christian Collins (https://schristiancollins.com/generaluser.php)

GeneralUser GS is freely usable for any purpose, commercial or otherwise,
and may be redistributed under the same conditions, with the following
notes from the author:

  "GeneralUser GS is a GM and GS compatible SoundFont bank for composing,
  playing MIDI files, and use in any sound module that supports the
  SoundFont 2.01 standard. It is free to use and freely distributable, as
  long as you do not charge specifically for it or modify the included
  documentation. You are free to use samples from this SoundFont in your
  own work, including commercial work, without any further permission."

See the upstream page for the full README and the most current license
text:
  https://schristiancollins.com/generaluser.php
`;

function resolveSoundFont() {
  const env = process.env.GM_SF2;
  if (env) {
    if (!existsSync(env)) die(`GM_SF2 set but file missing: ${env}`);
    log(`using $GM_SF2 = ${env}`);
    return { path: env, name: GENERALUSER_NAME };
  }
  mkdirSync(CACHE_DIR, { recursive: true });
  const cachedSf2 = join(CACHE_DIR, "GeneralUser_GS.sf2");
  if (existsSync(cachedSf2)) {
    log(`using cached SF2: ${cachedSf2}`);
    return { path: cachedSf2, name: GENERALUSER_NAME };
  }
  if (DRY_RUN) {
    warn(`SF2 not present; would download from ${GENERALUSER_URL}`);
    return { path: cachedSf2, name: GENERALUSER_NAME };
  }
  log(`downloading ${GENERALUSER_NAME} → ${cachedSf2}`);
  const isDirectSf2 = /\.sf2$/i.test(GENERALUSER_URL);
  const downloadPath = isDirectSf2 ? cachedSf2 : join(CACHE_DIR, "GeneralUser_GS.zip");
  const curl = spawnSync(
    "curl",
    ["-fsSL", "-o", downloadPath, GENERALUSER_URL],
    { stdio: "inherit" },
  );
  if (curl.status !== 0) {
    die(
      `download failed. Set $GM_SF2=/path/to/file.sf2 to bypass.\n` +
        `  Upstream page: https://schristiancollins.com/generaluser.php`,
    );
  }
  if (isDirectSf2) {
    log(`downloaded: ${cachedSf2}`);
    return { path: cachedSf2, name: GENERALUSER_NAME };
  }
  // Archive case: unzip and locate the .sf2 inside.
  const extractDir = join(CACHE_DIR, "extract");
  rmSync(extractDir, { recursive: true, force: true });
  mkdirSync(extractDir, { recursive: true });
  const unzip = spawnSync("unzip", ["-q", "-o", downloadPath, "-d", extractDir], {
    stdio: "inherit",
  });
  if (unzip.status !== 0) die("unzip failed; install `unzip` or extract manually");
  const found = execFileSync("find", [extractDir, "-name", "*.sf2"])
    .toString()
    .trim()
    .split("\n")
    .filter(Boolean);
  if (found.length === 0) die("no .sf2 found inside the archive");
  execFileSync("cp", [found[0], cachedSf2]);
  log(`extracted: ${cachedSf2}`);
  return { path: cachedSf2, name: GENERALUSER_NAME };
}

// ─── render core ────────────────────────────────────────────────────────────
const HOLD_SEC = 3;
const RELEASE_SEC = 1;
const TOTAL_SEC = HOLD_SEC + RELEASE_SEC;
const BITRATE = "96k";
const SAMPLE_RATE = 44100;
const VELOCITY = 100;

// Build a tiny MIDI file in memory: program change + note on + note off.
// Format 0, single track, 480 PPQ, tempo 120 → 1 quarter = 0.5s.
// Hold = HOLD_SEC. Track tail extends RELEASE_SEC for the release sample.
function buildMidi({ program, note, isDrum }) {
  const PPQ = 480;
  const TEMPO_US_PER_QN = 500000; // 120 BPM
  const secondsToTicks = (s) => Math.round((s * 1_000_000 * PPQ) / TEMPO_US_PER_QN);

  const writeVarLen = (n) => {
    const bytes = [];
    bytes.push(n & 0x7f);
    n >>= 7;
    while (n > 0) {
      bytes.unshift((n & 0x7f) | 0x80);
      n >>= 7;
    }
    return bytes;
  };

  const channel = isDrum ? 9 : 0; // GM channel 10 = index 9
  const events = [];

  // Tempo meta event @ tick 0.
  events.push(0); // delta
  events.push(0xff, 0x51, 0x03,
    (TEMPO_US_PER_QN >> 16) & 0xff,
    (TEMPO_US_PER_QN >> 8) & 0xff,
    TEMPO_US_PER_QN & 0xff);

  // Program change (skip for drum channel — kit selection is via bank,
  // but for GM standard kit on chan 10 we still emit a PC=0).
  events.push(...writeVarLen(0));
  events.push(0xc0 | channel, program & 0x7f);

  // Note on @ delta=0
  events.push(...writeVarLen(0));
  events.push(0x90 | channel, note & 0x7f, VELOCITY);

  // Note off @ delta=HOLD
  events.push(...writeVarLen(secondsToTicks(HOLD_SEC)));
  events.push(0x80 | channel, note & 0x7f, 0x40);

  // End of track @ delta=RELEASE (lets the SF release tail render fully)
  events.push(...writeVarLen(secondsToTicks(RELEASE_SEC)));
  events.push(0xff, 0x2f, 0x00);

  const trackBody = Buffer.from(events);
  const header = Buffer.alloc(14);
  header.write("MThd", 0, "ascii");
  header.writeUInt32BE(6, 4);
  header.writeUInt16BE(0, 8);  // format 0
  header.writeUInt16BE(1, 10); // 1 track
  header.writeUInt16BE(PPQ, 12);
  const trkHdr = Buffer.alloc(8);
  trkHdr.write("MTrk", 0, "ascii");
  trkHdr.writeUInt32BE(trackBody.length, 4);
  return Buffer.concat([header, trkHdr, trackBody]);
}

function renderOne({ sf2, midiFile, wavFile, mp3File }) {
  // fluidsynth headless render → wav
  const fs = spawnSync(
    "fluidsynth",
    [
      "-ni",
      "-g", "0.7",
      "-r", String(SAMPLE_RATE),
      "-F", wavFile,
      "--fast-render", wavFile,
      sf2,
      midiFile,
    ],
    { stdio: ["ignore", "ignore", "pipe"] },
  );
  // Note: some fluidsynth builds use `--fast-render=path` form — try fallback.
  if (fs.status !== 0 || !existsSync(wavFile)) {
    const fs2 = spawnSync(
      "fluidsynth",
      [
        "-ni",
        "-g", "0.7",
        "-r", String(SAMPLE_RATE),
        `--fast-render=${wavFile}`,
        sf2,
        midiFile,
      ],
      { stdio: ["ignore", "ignore", "pipe"] },
    );
    if (fs2.status !== 0 || !existsSync(wavFile)) {
      const stderr = (fs.stderr?.toString() || "") + (fs2.stderr?.toString() || "");
      throw new Error(`fluidsynth failed:\n${stderr}`);
    }
  }
  // ffmpeg → mp3 mono 96k
  const ff = spawnSync(
    "ffmpeg",
    [
      "-y", "-loglevel", "error",
      "-i", wavFile,
      "-t", String(TOTAL_SEC),
      "-codec:a", "libmp3lame",
      "-b:a", BITRATE,
      "-ac", "1",
      "-ar", String(SAMPLE_RATE),
      mp3File,
    ],
    { stdio: ["ignore", "ignore", "pipe"] },
  );
  if (ff.status !== 0 || !existsSync(mp3File)) {
    throw new Error(`ffmpeg failed:\n${ff.stderr?.toString() || ""}`);
  }
}

// ─── plan & execute ─────────────────────────────────────────────────────────
function planMelodicNotes(step) {
  const notes = [];
  for (let m = 21; m <= 108; m += step) notes.push(m);
  // Always include the boundary if step skipped it.
  if (notes[notes.length - 1] !== 108) notes.push(108);
  return notes;
}
function planDrumNotes() {
  return Object.keys(GM_DRUM_KIT_NOTE_NAMES)
    .map(Number)
    .sort((a, b) => a - b);
}

function selectedPrograms() {
  if (!ONLY) return [...Array(128).keys()];
  return ONLY.split(",")
    .map((s) => parseInt(s.trim(), 10))
    .filter((n) => Number.isInteger(n) && n >= 0 && n <= 127);
}

async function main() {
  log(`output: ${OUT_DIR}`);
  log(`note step: ${NOTE_STEP}  (range A0..C8 = 21..108)`);
  if (DRY_RUN) warn("dry-run: planning only, no audio rendered");

  const { path: sf2Path, name: sf2Name } = resolveSoundFont();
  mkdirSync(OUT_DIR, { recursive: true });

  const programs = selectedPrograms();
  const melodicNotes = planMelodicNotes(NOTE_STEP);
  const drumNotes = planDrumNotes();

  const manifest = {
    soundfont: sf2Name,
    license: "https://schristiancollins.com/generaluser.php",
    noteStep: NOTE_STEP,
    format: "mp3",
    sampleRate: SAMPLE_RATE,
    bitrate: BITRATE,
    channels: 1,
    durationSec: TOTAL_SEC,
    holdSec: HOLD_SEC,
    releaseSec: RELEASE_SEC,
    patches: [],
    drumKits: [],
  };

  const tmp = tmpdir();
  let rendered = 0;
  let skipped = 0;
  const failures = [];

  // Melodic programs.
  if (!SKIP_MELODIC) {
    for (const program of programs) {
      const dirName = String(program).padStart(3, "0");
      const progDir = join(OUT_DIR, dirName);
      mkdirSync(progDir, { recursive: true });
      manifest.patches.push({
        id: program,
        name: GM_PATCHES[program],
        notes: [...melodicNotes],
      });
      for (const note of melodicNotes) {
        const noteName = midiToName(note);
        const mp3File = join(progDir, `${noteName}.mp3`);
        if (existsSync(mp3File) && statSync(mp3File).size > 0) {
          skipped++;
          continue;
        }
        if (DRY_RUN) {
          rendered++;
          continue;
        }
        const midiFile = join(tmp, `gm-${program}-${note}.mid`);
        const wavFile = join(tmp, `gm-${program}-${note}.wav`);
        try {
          writeFileSync(midiFile, buildMidi({ program, note, isDrum: false }));
          renderOne({ sf2: sf2Path, midiFile, wavFile, mp3File });
          rendered++;
          if (rendered % 25 === 0) {
            process.stdout.write(
              `${C.dim}    rendered ${rendered} (skipped ${skipped})${C.reset}\n`,
            );
          }
        } catch (err) {
          failures.push({ program, note, error: err.message });
          warn(`program ${program} note ${note}: ${err.message.split("\n")[0]}`);
        } finally {
          rmSync(midiFile, { force: true });
          rmSync(wavFile, { force: true });
        }
      }
    }
  }

  // Drum kit (GM Standard Kit on channel 10, program 0).
  if (!SKIP_DRUMS) {
    const drumDir = join(OUT_DIR, "drum-000");
    mkdirSync(drumDir, { recursive: true });
    manifest.drumKits.push({
      id: 0,
      name: "Standard Kit",
      notes: drumNotes,
      noteNames: GM_DRUM_KIT_NOTE_NAMES,
    });
    for (const note of drumNotes) {
      const mp3File = join(drumDir, `${note}.mp3`);
      if (existsSync(mp3File) && statSync(mp3File).size > 0) {
        skipped++;
        continue;
      }
      if (DRY_RUN) {
        rendered++;
        continue;
      }
      const midiFile = join(tmp, `gm-drum-${note}.mid`);
      const wavFile = join(tmp, `gm-drum-${note}.wav`);
      try {
        writeFileSync(midiFile, buildMidi({ program: 0, note, isDrum: true }));
        renderOne({ sf2: sf2Path, midiFile, wavFile, mp3File });
        rendered++;
      } catch (err) {
        failures.push({ kit: 0, note, error: err.message });
        warn(`drum note ${note}: ${err.message.split("\n")[0]}`);
      } finally {
        rmSync(midiFile, { force: true });
        rmSync(wavFile, { force: true });
      }
    }
  }

  // Manifest + license.
  writeFileSync(
    join(OUT_DIR, "manifest.json"),
    JSON.stringify(manifest, null, 2) + "\n",
  );
  writeFileSync(join(OUT_DIR, "LICENSE.txt"), GENERALUSER_LICENSE);

  log(`done. rendered=${rendered}  skipped=${skipped}  failures=${failures.length}`);
  if (failures.length > 0) {
    warn(`${failures.length} render failure(s); see warnings above`);
  }
  console.log("");
  console.log(`  manifest: ${join(OUT_DIR, "manifest.json")}`);
  console.log(`  license:  ${join(OUT_DIR, "LICENSE.txt")}`);
  console.log("");
  console.log(`  publish:  npm run assets:sync:up  # uploads to assets.aesthetic.computer/gm/`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
