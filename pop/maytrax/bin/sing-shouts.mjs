#!/usr/bin/env node
// sing-shouts.mjs — turn the dry jeffrey-pvc shout clips (gen-shouts.mjs)
// into LONG, AUTOTUNED, HARMONIZED sustains — jeffrey's voice stretched out
// and stacked into a flute-like harmony choir, pitched to the maytrax.np
// F-minor notes.
//
// For each phrase:
//   1. WORLD pitch-lock (bin/pitchsnap_world.py) onto the target note(s)
//      from maytrax.np — full clamp + a little vibrato (emo autotune).
//   2. Time-stretch a lot longer (rubberband) so the vowels ring as held
//      notes across the drop.
//   3. HARMONIZE — sum formant-preserving pitch-shifted copies (octave-up
//      airy "flute", fifth, minor third) so each sustain blooms into a
//      harmonized flute-choir.
//
// Rewrites out/shouts/<slug>.wav in place (maytrax.mjs loads the same paths).
//
//   node pop/maytrax/bin/sing-shouts.mjs                     # hold ≈ 3 s
//   node pop/maytrax/bin/sing-shouts.mjs --hold 4 --harmonies "12:0.5,7:0.35,3:0.3"

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const POP = resolve(LANE, "..");
const PY = resolve(POP, ".venv", "bin", "python");
const PSNAP = resolve(POP, "bin", "pitchsnap_world.py");
const SR = 48000;

const argv = process.argv.slice(2);
const takeFlag = (n, d) => { const i = argv.indexOf(n); return i >= 0 && argv[i + 1] ? argv[i + 1] : d; };
const HOLD = parseFloat(takeFlag("--hold", "4.2"));
const VIB_CENTS = takeFlag("--vibrato-cents", "22");
const RETAIN = takeFlag("--retain", "1.0");
// flute-choir harmony stack: "semis:gain,…" relative to the autotuned lead
const HARMONIES = takeFlag("--harmonies", "12:0.5,7:0.35,3:0.28")
  .split(",").map((s) => s.trim()).filter(Boolean)
  .map((s) => { const [semi, g] = s.split(":"); return { semi: parseFloat(semi), gain: parseFloat(g) }; });

const NOTES = {
  wake_up:                 ["F4", "F4"],
  it_s_real:               ["G#4", "G4"],
  hold_on:                 ["F4", "F4"],
  let_go:                  ["C5", "C5"],
  follow_the_white_rabbit: ["F4", "F4", "G#4", "G4", "F4", "F4"],
  now:                     ["C5"],
};
const HOLD_FOR = { follow_the_white_rabbit: HOLD * 1.7, now: HOLD * 1.5 };

const SH_PATH = resolve(LANE, "shouts.json");
if (!existsSync(SH_PATH)) { console.error("✗ run gen-shouts.mjs first (no shouts.json)"); process.exit(1); }
const sh = JSON.parse(readFileSync(SH_PATH, "utf8"));
const run = (cmd, args) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"] });

// ── f32 wav read/write + rubberband helpers (mono) ───────────────────
function readF32(path) {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-i", path, "-f", "f32le", "-ar", String(SR), "-ac", "1", "-"], { maxBuffer: 1 << 30 });
  return new Float32Array(r.stdout.buffer, r.stdout.byteOffset, Math.floor(r.stdout.length / 4));
}
function writeF32Wav(buf, path) {
  const n = buf.length, b = Buffer.alloc(44 + n * 4);
  b.write("RIFF", 0); b.writeUInt32LE(36 + n * 4, 4); b.write("WAVE", 8);
  b.write("fmt ", 12); b.writeUInt32LE(16, 16); b.writeUInt16LE(3, 20); b.writeUInt16LE(1, 22);
  b.writeUInt32LE(SR, 24); b.writeUInt32LE(SR * 4, 28); b.writeUInt16LE(4, 32); b.writeUInt16LE(32, 34);
  b.write("data", 36); b.writeUInt32LE(n * 4, 40);
  for (let i = 0; i < n; i++) b.writeFloatLE(buf[i], 44 + i * 4);
  writeFileSync(path, b);
}
function pitchShift(buf, semis) {
  if (semis === 0) return buf.slice();
  const pin = resolve(tmpdir(), "maytrax-harm-in.wav"), pout = resolve(tmpdir(), `maytrax-harm-${semis}.wav`);
  writeF32Wav(buf, pin);
  const r = spawnSync("rubberband", ["-p", String(semis), "-F", "--pitch-hq", pin, pout], { stdio: "ignore" });
  if (r.status !== 0 || !existsSync(pout)) return buf.slice();
  return readF32(pout);
}

// transpose note names by `semis` (deeper voice = negative)
const NM2 = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const NNm = { C: 0, "C#": 1, D: 2, "D#": 3, E: 4, F: 5, "F#": 6, G: 7, "G#": 8, A: 9, "A#": 10, B: 11 };
function transpose(note, semis) {
  const m = /^([A-G]#?)(-?\d)$/.exec(note); if (!m) return note;
  const midi = NNm[m[1]] + (parseInt(m[2], 10) + 1) * 12 + semis;
  return NM2[((midi % 12) + 12) % 12] + (Math.floor(midi / 12) - 1);
}
// autotune (WORLD) → stretch (rubberband) → harmonize (flute stack) → write
function singClip(key, notes, srcWav, outWav, label, semis = 0) {
  if (!existsSync(srcWav)) return;
  if (semis) notes = notes.map((n) => transpose(n, semis));
  const dur = parseFloat(spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", srcWav]).stdout.toString().trim()) || 0.6;
  const tuned = resolve(tmpdir(), `maytrax-${key}-${label}-tuned.wav`);
  const stretched = resolve(tmpdir(), `maytrax-${key}-${label}-stretch.wav`);
  if (run(PY, [PSNAP, srcWav, tuned, "--notes", notes.join(","), "--retain", RETAIN, "--xfade-ms", "60",
    "--vibrato-hz", "5.5", "--vibrato-cents", VIB_CENTS, "--vibrato-onset-ms", "180"]).status !== 0 || !existsSync(tuned)) { console.warn(`! pitch-lock failed ${key}/${label}`); return; }
  const target = HOLD_FOR[key] ?? HOLD, ratio = Math.max(1.5, target / dur);
  if (run("rubberband", ["-t", ratio.toFixed(3), "-c", "4", tuned, stretched]).status !== 0) { console.warn(`! stretch failed ${key}/${label}`); return; }
  const lead = readF32(stretched), mix = new Float32Array(lead.length);
  for (let i = 0; i < lead.length; i++) mix[i] = lead[i];
  for (const h of HARMONIES) { const v = pitchShift(lead, h.semi); for (let i = 0; i < lead.length && i < v.length; i++) mix[i] += v[i] * h.gain; }
  const atk = Math.floor(0.04 * SR), rel = Math.floor(0.12 * SR);
  for (let i = 0; i < atk && i < mix.length; i++) mix[i] *= i / atk;
  for (let i = 0; i < rel && i < mix.length; i++) mix[mix.length - 1 - i] *= i / rel;
  let pk = 0; for (let i = 0; i < mix.length; i++) pk = Math.max(pk, Math.abs(mix[i]));
  if (pk > 0.97) { const g = 0.97 / pk; for (let i = 0; i < mix.length; i++) mix[i] *= g; }
  writeF32Wav(mix, outWav);
  console.log(`✓ ${key.padEnd(22)} ${label.padEnd(7)} ${notes.join(",")} · ${dur.toFixed(2)}s → ${(lead.length / SR).toFixed(1)}s + ${HARMONIES.length}v harmony`);
}

for (const [key, notes] of Object.entries(NOTES)) {
  const entry = sh[key]; if (!entry) { console.warn(`! missing ${key}`); continue; }
  // jeffrey-pvc: immutable source = the dry .mp3 → decode → process → .wav
  const drySrc = entry.path.replace(/\.wav$/, ".mp3");
  if (existsSync(drySrc)) {
    const dry = resolve(tmpdir(), `maytrax-${key}-dry.wav`);
    if (run("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", drySrc, "-ar", String(SR), "-ac", "1", dry]).status === 0)
      singClip(key, notes, dry, entry.path, "jeffrey", -12); // deeper voiced
  }
  // Apple say: immutable source = <slug>-say.wav → process → <slug>-say-sung.wav
  const saySrc = entry.path.replace(/\.wav$/, "-say.wav");
  singClip(key, notes, saySrc, entry.path.replace(/\.wav$/, "-say-sung.wav"), "apple");
}
console.log("✓ shouts (jeffrey + apple say) autotuned + stretched + harmonized — re-run maytrax.mjs");
