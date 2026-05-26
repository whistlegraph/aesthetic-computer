#!/usr/bin/env node
// stitch.mjs — concatenate one clip per group (america + computer + dora)
// back-to-back. by default each clip is pitch-shifted to the matching note
// from melody.json (variant 0 = 5-4-1 = A4 → G4 → D4) so the three voices
// land as a tuned hook. pass --raw to skip pitch-shift.
//
// dead-simple "do these three samples actually fit together as a hook?" test.
//
// usage:
//   node bin/stitch.mjs                          # auto-pick + pitch-shift + play
//   node bin/stitch.mjs --raw                    # no pitch correction at all
//   node bin/stitch.mjs --variant 2              # use melody.json hook.variants[2]
//   node bin/stitch.mjs --gap 0.08               # 80 ms silence between (default 0)
//   node bin/stitch.mjs --pick computer=zarvox-mid  # pin a specific clip in a group
//   node bin/stitch.mjs --no-play                # write only, don't play
//   node bin/stitch.mjs --out ~/Desktop/test.mp3 # explicit out path
//   node bin/stitch.mjs --random                 # random pick per group
//   node bin/stitch.mjs --preview                # play each picked clip alone with a label (audit mode)

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readdirSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { resolve, dirname, join, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir, tmpdir } from "node:os";

import { readWavMono } from "../../lib/wav.mjs";

const SR = 48_000;
const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE);

// ── YIN-lite pitch detection (autocorrelation in plausible voice range) ─
function detectPitchHz(samples, sr) {
  const n = samples.length;
  const start = Math.floor(n * 0.2);
  const end = Math.floor(n * 0.8);
  const seg = samples.subarray(start, end);
  const minLag = Math.floor(sr / 600);
  const maxLag = Math.floor(sr / 80);
  let bestLag = 0, bestScore = -Infinity;
  for (let lag = minLag; lag <= maxLag; lag++) {
    let r = 0, e0 = 0, e1 = 0;
    for (let i = 0; i + lag < seg.length; i++) {
      r += seg[i] * seg[i + lag];
      e0 += seg[i] * seg[i];
      e1 += seg[i + lag] * seg[i + lag];
    }
    const norm = Math.sqrt(e0 * e1) || 1;
    const score = r / norm;
    if (score > bestScore) { bestScore = score; bestLag = lag; }
  }
  return bestLag === 0 ? null : sr / bestLag;
}
const hzToMidi = (hz) => 69 + 12 * Math.log2(hz / 440);

function chainAtempo(target) {
  const stages = [];
  let r = target;
  while (r > 2.0) { stages.push(2.0); r /= 2.0; }
  while (r < 0.5) { stages.push(0.5); r /= 0.5; }
  stages.push(+r.toFixed(6));
  return stages.map((s) => `atempo=${s}`).join(",");
}

function pitchShiftToWav(srcAbs, semitones, outWav) {
  if (Math.abs(semitones) < 0.01) {
    // unity — just resample to 48k mono.
    return spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-i", srcAbs, "-ac", "1", "-ar", String(SR), outWav,
    ], { stdio: ["ignore", "ignore", "inherit"] }).status === 0;
  }
  const ratio = Math.pow(2, semitones / 12);
  const filter = `asetrate=${SR}*${ratio},aresample=${SR},${chainAtempo(1 / ratio)},loudnorm=I=-14:TP=-1:LRA=11`;
  return spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", srcAbs, "-af", filter, "-ac", "1", "-ar", String(SR), outWav,
  ], { stdio: ["ignore", "ignore", "inherit"] }).status === 0;
}

const midiName = (m) => {
  const names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
  return names[m % 12] + (Math.floor(m / 12) - 1);
};

const argv = process.argv.slice(2);
const flags = {};
const pickPins = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a === "--pick") {
    const v = argv[++i];
    const [g, name] = v.split("=");
    pickPins[g] = name;
  } else if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

function pickOne(group) {
  const d = join(ROOT, "utterances", group);
  if (!existsSync(d)) return null;
  const files = readdirSync(d).filter((f) => f.endsWith(".wav"));
  if (!files.length) return null;
  if (pickPins[group]) {
    const want = pickPins[group].endsWith(".wav") ? pickPins[group] : pickPins[group] + ".wav";
    const found = files.find((f) => f === want);
    if (found) return join(d, found);
    console.warn(`  ! '${pickPins[group]}' not in utterances/${group}/, falling back to first`);
  }
  if (flags.random) {
    return join(d, files[Math.floor(Math.random() * files.length)]);
  }
  // deterministic default: prefer fred-mid for computer, first alphabetical otherwise
  const preferred = group === "computer" ? files.find((f) => f === "fred-mid.wav") : null;
  return join(d, preferred || files.sort()[0]);
}

const groups = ["america", "computer", "dora"];
const picks = {};
for (const g of groups) picks[g] = pickOne(g);

console.log("# stitch picks:");
let missing = [];
for (const g of groups) {
  if (picks[g]) console.log(`  ✓ ${g}: ${basename(picks[g])}`);
  else { console.log(`  ✗ ${g}: NO CLIPS in utterances/${g}/`); missing.push(g); }
}

if (missing.length === groups.length) {
  console.error("\nno clips at all. run bin/say-computer.mjs (computer), bin/fetch.mjs (america/dora), bin/extract.mjs.");
  process.exit(1);
}

// ── --preview: play each picked clip alone with a label, then exit ─────
if (flags.preview) {
  console.log("\n# preview — playing each clip in isolation");
  for (const g of groups) {
    if (!picks[g]) { console.log(`  (${g}: skipped, no clips)`); continue; }
    console.log(`\n  ▶ ${g.toUpperCase()}  →  ${basename(picks[g])}`);
    spawnSync("afplay", [picks[g]], { stdio: "inherit" });
  }
  console.log("\n# done — utterances confirmed?");
  process.exit(0);
}

// ── pitch-shift each clip to its melody target (unless --raw) ──────────
const raw = !!flags.raw;
const variantIdx = Number(flags.variant ?? 0);
let melody = null;
let pitched = { ...picks }; // group → final file path going into concat
const tmpFiles = [];

if (!raw) {
  const melodyPath = join(ROOT, "melody.json");
  if (!existsSync(melodyPath)) {
    console.warn("  ! melody.json not found, falling back to --raw");
  } else {
    melody = JSON.parse(readFileSync(melodyPath, "utf8"));
    const variant = melody.hook.variants[variantIdx % melody.hook.variants.length];
    console.log(`\n# autotune: variant ${variantIdx} (${variant.label}) — ${variant.notes.map(midiName).join(" → ")}`);
    for (let i = 0; i < groups.length; i++) {
      const g = groups[i];
      if (!picks[g]) continue;
      const target = variant.notes[i];
      // detect source pitch
      let srcMidi = null, srcHz = null;
      try {
        const { samples } = readWavMono(picks[g]);
        srcHz = detectPitchHz(samples, SR);
        srcMidi = srcHz ? hzToMidi(srcHz) : null;
      } catch (e) {
        console.warn(`  ! pitch detect failed for ${g}: ${e.message}`);
      }
      if (srcMidi == null) {
        console.log(`  · ${g}: pitch undetected, using raw`);
        continue;
      }
      // shift to target. fold by octaves so we never stretch more than ±6 semis.
      let semis = target - srcMidi;
      while (semis > 6) semis -= 12;
      while (semis < -6) semis += 12;
      const tmpOut = join(tmpdir(), `acd-stitch-${process.pid}-${g}.wav`);
      const ok = pitchShiftToWav(picks[g], semis, tmpOut);
      if (ok) {
        pitched[g] = tmpOut;
        tmpFiles.push(tmpOut);
        const actualMidi = srcMidi + semis;
        console.log(`  · ${g}: ${srcHz.toFixed(1)} Hz (${midiName(Math.round(srcMidi))}) → ${midiName(Math.round(actualMidi))}  shift ${semis >= 0 ? "+" : ""}${semis.toFixed(1)} semis (target ${midiName(target)})`);
      } else {
        console.warn(`  ! pitch-shift failed for ${g}, using raw`);
      }
    }
  }
}

// build a tiny concat list for ffmpeg's concat demuxer
const gap = Number(flags.gap ?? 0);
const tmpSilence = join(tmpdir(), `acd-silence-${process.pid}.wav`);
if (gap > 0) {
  spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "lavfi", "-i", `anullsrc=r=${SR}:cl=mono`,
    "-t", String(gap),
    tmpSilence,
  ], { stdio: ["ignore", "ignore", "inherit"] });
}

// concat demuxer needs a file list; entries: file '<abs>'
const listPath = join(tmpdir(), `acd-stitch-${process.pid}.txt`);
const lines = [];
for (let i = 0; i < groups.length; i++) {
  const g = groups[i];
  if (!pitched[g]) continue;
  lines.push(`file '${pitched[g].replace(/'/g, "'\\''")}'`);
  if (gap > 0 && i < groups.length - 1) lines.push(`file '${tmpSilence}'`);
}
writeFileSync(listPath, lines.join("\n") + "\n");

const wantWav = !!flags.wav;
const ext = wantWav ? "wav" : "mp3";
const outPath = expandHome(flags.out) || resolve(ROOT, `out/americomputadora-stitch.${ext}`);
mkdirSync(dirname(outPath), { recursive: true });

const ffArgs = [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "concat", "-safe", "0", "-i", listPath,
  // resample everything to 48k mono first so concat works across mixed-rate inputs
  "-ac", "1", "-ar", String(SR),
];
if (wantWav) ffArgs.push("-c:a", "pcm_s16le");
else ffArgs.push("-c:a", "libmp3lame", "-q:a", "2",
  "-metadata", "title=americomputadora (stitch)",
  "-metadata", "artist=jeffrey",
  "-metadata", "album=pixsies",
);
ffArgs.push(outPath);

const ff = spawnSync("ffmpeg", ffArgs, { stdio: "inherit" });
try { unlinkSync(listPath); } catch {}
try { if (gap > 0) unlinkSync(tmpSilence); } catch {}
for (const t of tmpFiles) { try { unlinkSync(t); } catch {} }
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }

const dur = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", outPath], { encoding: "utf8" });
console.log(`\n✓ ${outPath}  (${(+dur.stdout.trim()).toFixed(2)}s, gap=${gap}s)`);

if (!flags["no-play"]) {
  console.log("playing…");
  spawnSync("afplay", [outPath], { stdio: "inherit" });
}
