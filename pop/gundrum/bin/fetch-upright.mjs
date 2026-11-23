#!/usr/bin/env node
// fetch-upright.mjs — acoustic upright (double) bass samples into
// assets/upright/, one folder per source, named by note, layer and take.
// A fixed list (no search, no credentials), so a re-run gets the same files
// and skips the ones already there. Writes assets/upright/SOURCES.json.
//
// Three sources, all cleared for commercial music:
//   pjcohen — Freesound pack "Standup Upright Acoustic Double Bass" by
//             pjcohen, every item CC0 1.0. Chromatic plucks (long and
//             finger-stopped) plus slides, slaps and body knocks. These are
//             the public preview MP3s (lossy): the 16-bit WAVs need OAuth.
//   vsco    — VS Chamber Orchestra 2: Community Edition, Solo Contrabass
//             (Versilian Studios), CC0 1.0 on GitHub. Lossless pizz with two
//             round robins (two dynamics on the low notes) and arco sustains.
//             The readme asks, but does not require, a credit to Versilian
//             Studios / Sam Gossner and a link to the VSCO CE homepage.
//   iowa    — University of Iowa Electronic Music Studios, Musical Instrument
//             Samples, Double Bass 2012, pizz ff. The site says the
//             recordings "may be downloaded and used for any projects,
//             without restrictions". Lossless, chromatic, one take per
//             string (rr1 = E string, rr2 = A string).
// Nothing here is ripped from a record, film or game.
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, renameSync, statSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../assets/upright");
const UA = "sound-fetch/1.0";
const MAX_BYTES = 150e6;

const NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const midiOf = (note) => {
  const m = note.match(/^([A-G])([#b]?)(-?\d)$/);
  const base = { C: 0, D: 2, E: 4, F: 5, G: 7, A: 9, B: 11 }[m[1]];
  return (+m[3] + 1) * 12 + base + (m[2] === "#" ? 1 : m[2] === "b" ? -1 : 0);
};
const nameOf = (midi) => NAMES[midi % 12] + (Math.floor(midi / 12) - 1);
const range = (lo, hi) => Array.from({ length: hi - lo + 1 }, (_, i) => lo + i);
const F1 = 29, D3 = 50;

const SOUNDS = [];

// --- pjcohen (Freesound, CC0) ---------------------------------------------
// [scientific note, Freesound id] — note names on Freesound are scientific.
const PJ_UID = 394391;
const PJ_AUTHOR = "pjcohen (Freesound)";
const pj = (file, id, name, articulation, description, extra = {}) => SOUNDS.push({
  file: `pjcohen/${file}`, description, author: PJ_AUTHOR, licence: "CC0 1.0", attribution: null,
  source: `https://freesound.org/people/pjcohen/sounds/${id}/`,
  download: `https://cdn.freesound.org/previews/${Math.floor(id / 1000)}/${id}_${PJ_UID}-hq.mp3`,
  originalName: name, articulation, ...extra,
});
const PJ_NORMAL = { E1: 383566, F1: 383689, "F#1": 383685, G1: 383707, "G#1": 383710, A1: 383551,
  "A#1": 383615, B1: 383554, C2: 383569, "C#2": 383553, D2: 383575, "D#2": 383570, E2: 383682,
  F2: 383690, "F#2": 383686, G2: 383706, "G#2": 383709, A2: 383548, "A#2": 383614, B2: 383555,
  C3: 383568, "C#3": 383546, D3: 383574, "D#3": 383573, E3: 383683 };
const PJ_STOP = { E1: 383680, F1: 383557, "F#1": 383665, G1: 383544, "G#1": 383561, A1: 383592,
  "A#1": 383605, B1: 383694, C2: 383700, "C#2": 383697, D2: 383668, "D#2": 383693, E2: 383679,
  F2: 383562, "F#2": 383558, G2: 383543, "G#2": 383564, A2: 383587, "A#2": 383604, B2: 383695,
  C3: 383701, "C#3": 383698, D3: 383667, "D#3": 383670, E3: 383671 };
for (const [note, id] of Object.entries(PJ_NORMAL))
  pj(`upright-pizz-${note}-v1-rr1.mp3`, id, `Standup_Bass_Normal_${note}.wav`, "pizz",
    `jazz upright, finger pluck left to ring, ${note}`, { note, midi: midiOf(note), layer: 1, rr: 1 });
for (const [note, id] of Object.entries(PJ_STOP))
  pj(`upright-pizzstop-${note}-v1-rr1.mp3`, id, `Standup_Bass_Stop_Finger_${note}.wav`, "pizz-stopped",
    `jazz upright, finger pluck stopped short by the left hand, ${note}`, { note, midi: midiOf(note), layer: 1, rr: 1 });
const PJ_FX = {
  slide: [383607, 383606, 383613, 383612, 383611, 383610],
  slap: [383621, 383620, 383634, 383633, 383609, 383608],
  knock: [383619, 383618, 383617, 383616, 383632, 383622],
};
const FX_WHAT = { slide: "hand slide down the strings", slap: "open-hand slap on the strings (ghost thump)", knock: "knock on the wooden body" };
for (const [kind, ids] of Object.entries(PJ_FX))
  ids.forEach((id, i) => pj(`upright-${kind}-rr${i + 1}.mp3`, id,
    `Standup_Bass_Effects_${kind[0].toUpperCase() + kind.slice(1)}${i + 1}.wav`, kind, FX_WHAT[kind], { rr: i + 1 }));

// --- VSCO 2 CE Solo Contrabass (GitHub, CC0) -------------------------------
// VSCO names the bass an octave low (its lowest string, E1, is "E0"); the
// measured pitches agree, so files are renamed one octave up.
const VSCO_RAW = "https://raw.githubusercontent.com/sgossner/VSCO-2-CE/master/Strings/Solo%20Contrabass/";
const VSCO_PAGE = "https://github.com/sgossner/VSCO-2-CE";
const VSCO_CREDIT = "Contrabass samples from VS Chamber Orchestra: Community Edition by Versilian Studios (Sam Gossner), https://vis.versilstudios.net/vsco-community.html";
const VSCO_OCTAVE = 1;
const vsco = (dir, label, v, rr, articulation, description) => {
  const midi = midiOf(label) + 12 * VSCO_OCTAVE, note = nameOf(midi);
  const layer = v === 1 ? 1 : 2; // VSCO v1 = soft, v3 = loud
  const orig = `BKCtbss_${dir}_${label}_v${v}_rr${rr}.wav`;
  SOUNDS.push({
    file: `vsco/upright-${articulation}-${note}-v${layer}-rr${rr}.wav`, description: `${description}, ${v === 1 ? "soft" : "loud"}`,
    author: "Versilian Studios / Sam Gossner (VSCO 2 CE)", licence: "CC0 1.0", attribution: VSCO_CREDIT,
    source: VSCO_PAGE, download: VSCO_RAW + `${dir}/${encodeURIComponent(orig)}`,
    originalName: orig, articulation, note, midi, layer, rr,
  });
};
const PIZZ = { "E0": [1, 3], "F#0": [1, 3], "G0": [1, 3], "A#0": [1, 3], "C1": [1, 3], "D1": [1], "E1": [1, 3],
  "F#1": [1], "G#1": [1], "A1": [1], "C#2": [1], "E2": [1], "G#2": [1], "B2": [1] };
for (const [label, vs] of Object.entries(PIZZ))
  for (const v of vs) for (const rr of [1, 2]) vsco("Pizz", label, v, rr, "pizz", "solo contrabass pizzicato");
for (const label of ["E0", "F#0", "G0", "A#0", "C1", "D1", "E1", "F#1", "G#1", "A1", "C#2"])
  for (const v of [1, 3]) vsco("SusNV", label, v, 1, "arco", "solo contrabass arco sustain, no vibrato");

// --- University of Iowa MIS, Double Bass 2012, pizz ff ---------------------
const IOWA_RAW = "https://theremin.music.uiowa.edu/sound%20files/MIS%20Pitches%20-%202014/Strings/Double%20Bass/";
const IOWA_PAGE = "https://theremin.music.uiowa.edu/MIS-Pitches-2012/MISDoubleBass2012.html";
const FLAT = ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"];
const iowaName = (midi) => FLAT[midi % 12] + (Math.floor(midi / 12) - 1);
for (const [string, rr, lo] of [["sulE", 1, F1], ["sulA", 2, midiOf("A1")]])
  for (const midi of range(lo, D3)) {
    const orig = `Bass.pizz.ff.${string}.${iowaName(midi)}.stereo.aif`, note = nameOf(midi);
    SOUNDS.push({
      file: `iowa/upright-pizz-${note}-v1-rr${rr}.aif`,
      description: `double bass pizzicato ff on the ${string.slice(3)} string, ${note}`,
      author: "University of Iowa Electronic Music Studios", licence: "free for any project, without restrictions (UIowa MIS terms)",
      attribution: "Double bass samples: University of Iowa Electronic Music Studios (credit not required)",
      source: IOWA_PAGE, download: IOWA_RAW + encodeURIComponent(orig),
      originalName: orig, articulation: "pizz", dynamic: "ff", string: string.slice(3), note, midi, layer: 1, rr,
    });
  }

function probe(path) {
  try {
    const j = JSON.parse(execFileSync("ffprobe", ["-v", "error", "-show_entries",
      "format=duration:stream=sample_rate,channels,codec_name", "-of", "json", path], { encoding: "utf8" }));
    const s = j.streams[0];
    return { seconds: +(+j.format.duration).toFixed(2), sampleRate: +s.sample_rate, channels: s.channels, codec: s.codec_name };
  } catch { return null; }
}

// Measured tuning (added to SOURCES.json after a pitch pass) survives a re-run.
const TUNING_KEYS = ["measuredCents", "measuredAttackCents", "tuningNote"];
let previous = {}, tuningLine;
try {
  const old = JSON.parse(readFileSync(resolve(OUT, "SOURCES.json"), "utf8"));
  tuningLine = old.tuning;
  for (const s of old.sounds) previous[s.file] = Object.fromEntries(TUNING_KEYS.filter((k) => k in s).map((k) => [k, s[k]]));
} catch {}

let total = 0, failed = 0;
const manifest = [];
for (const s of SOUNDS) {
  const dest = resolve(OUT, s.file);
  mkdirSync(dirname(dest), { recursive: true });
  if (!existsSync(dest)) {
    try {
      let buf, err;
      for (let n = 0; n < 4 && !buf; n++) {
        try {
          const res = await fetch(s.download, { headers: { "User-Agent": UA } });
          if (!res.ok) throw new Error(`http ${res.status}`);
          buf = Buffer.from(await res.arrayBuffer());
        } catch (e) { err = e; await new Promise((r) => setTimeout(r, 1000 * (n + 1))); }
      }
      if (!buf) throw err;
      if (total + buf.length > MAX_BYTES) throw new Error("over the size budget");
      writeFileSync(dest + ".part", buf);
      renameSync(dest + ".part", dest);
    } catch (e) { failed++; console.error(`✗ ${s.file}: ${e.message}`); continue; }
  }
  const bytes = statSync(dest).size;
  const audio = probe(dest);
  if (!audio) { failed++; console.error(`✗ ${s.file}: does not decode`); continue; }
  total += bytes;
  manifest.push({ ...s, bytes, ...audio, ...previous[s.file] });
  console.log(`✓ ${s.file}  ${audio.seconds}s  ${audio.sampleRate} Hz`);
}
writeFileSync(resolve(OUT, "SOURCES.json"), JSON.stringify({
  note: "pjcohen/ and vsco/ are CC0 1.0 (commercial use, no attribution required); iowa/ is free for any project without restrictions per the UIowa MIS site. pjcohen/ files are Freesound preview MP3s (lossy). Note names are scientific pitch (E1 = 41.2 Hz).",
  credits: {
    vsco: VSCO_CREDIT + " (requested, not required)",
    iowa: "University of Iowa Electronic Music Studios, Musical Instrument Samples (optional)",
    pjcohen: "pjcohen on Freesound, pack 'Standup Upright Acoustic Double Bass' (optional)",
  },
  licenceUrls: {
    cc0: "https://creativecommons.org/publicdomain/zero/1.0/",
    vsco: "https://github.com/sgossner/VSCO-2-CE/blob/master/LICENSE",
    iowa: "https://theremin.music.uiowa.edu/MIS.html",
  },
  ...(tuningLine ? { tuning: tuningLine } : {}),
  sounds: manifest,
}, null, 2) + "\n");
console.log(`${manifest.length}/${SOUNDS.length} files, ${(total / 1e6).toFixed(1)} MB in ${OUT}`);
if (failed) process.exitCode = 1;
