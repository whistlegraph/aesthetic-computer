#!/usr/bin/env node
// rfa.mjs — "request for audio". Walk a /pop track's vocal score and
// record @jeffrey's own voice, one note at a time, to slowly replace
// the computer/synthesized vocals. See pop/REQUEST-FOR-AUDIO.md.
//
// Per note: prints the syllable + target pitch, plays a guide
// (count-in clicks at the track BPM + a reference tone at the note's
// pitch + duration), records from the MacBook Neo mic via ffmpeg
// avfoundation, plays the take back, then keep / redo / skip.
//
// Usage:
//   node pop/bin/rfa.mjs --track marimba             # walk missing notes
//   node pop/bin/rfa.mjs --track marimba --status    # punch-list
//   node pop/bin/rfa.mjs --track marimba --only 4-0  # (re)record one note
//   node pop/bin/rfa.mjs --track marimba --device :1 # pick an input
//   node pop/bin/rfa.mjs --list-devices              # list mic inputs
//   node pop/bin/rfa.mjs --sample --track hellsine --name rattle
//                                                    # free one-shot sample
//
// Takes are written to pop/<lane>/voice-takes/<id>.wav — the renderer
// picks them up automatically on the next render. Headphones keep the
// reference tone out of the mic; it works without them too.

import { readFileSync, writeFileSync, existsSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { createInterface } from "node:readline";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP  = resolve(HERE, "..");
const SR   = 48_000;

// ── args ──────────────────────────────────────────────────────────────
const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
  else flags[a.slice(2)] = true;
}

// ── list audio inputs ─────────────────────────────────────────────────
if (flags["list-devices"]) {
  console.log("avfoundation inputs (audio devices are the ones to use):\n");
  const r = spawnSync("ffmpeg", ["-hide_banner", "-f", "avfoundation",
    "-list_devices", "true", "-i", ""], { encoding: "utf8" });
  process.stdout.write((r.stderr || "") + "\n");
  console.log("→ pass the audio index, e.g. --device :0");
  process.exit(0);
}

const TRACK = flags.track;
if (!TRACK) {
  console.error("usage: node pop/bin/rfa.mjs --track <lane> [--status] [--only <id>] [--device :N]");
  console.error("       node pop/bin/rfa.mjs --sample --track <lane> --name <id> [--dur N]");
  process.exit(1);
}
const DEVICE = flags.device || ":default";

const LANE_DIR  = resolve(POP, TRACK);
const TAKES_DIR = resolve(LANE_DIR, "voice-takes");
const MANIFEST  = resolve(TAKES_DIR, "manifest.json");
let manifest = null;
if (!flags.sample) {
  if (!existsSync(MANIFEST)) {
    console.error(`✗ no manifest at ${MANIFEST.replace(POP + "/", "pop/")}`);
    console.error(`  render the track once first — its renderer writes the manifest.`);
    process.exit(1);
  }
  manifest = JSON.parse(readFileSync(MANIFEST, "utf8"));
}
mkdirSync(TAKES_DIR, { recursive: true });

const takePath = (id) => resolve(TAKES_DIR, `${id}.wav`);
function hasTake(id) { return existsSync(takePath(id)); }

// ── status / punch-list ───────────────────────────────────────────────
if (flags.status) {
  const done = manifest.notes.filter((n) => hasTake(n.id)).length;
  console.log(`\n${manifest.track} — ${done}/${manifest.notes.length} takes recorded\n`);
  for (const n of manifest.notes) {
    const mark = hasTake(n.id) ? "✓" : "·";
    console.log(`  ${mark} ${n.id.padEnd(8)} ${n.note.padEnd(4)} "${n.vowel}"  ` +
      `bar ${n.bar} · ${n.durSec.toFixed(2)}s · @${n.startSec.toFixed(1)}s`);
  }
  console.log("");
  process.exit(0);
}

// ── wav helpers ───────────────────────────────────────────────────────
function writeWavMono(path, f32, sr) {
  const n = f32.length;
  const buf = Buffer.alloc(44 + n * 2);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + n * 2, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20);
  buf.writeUInt16LE(1, 22); buf.writeUInt32LE(sr, 24);
  buf.writeUInt32LE(sr * 2, 28); buf.writeUInt16LE(2, 32); buf.writeUInt16LE(16, 34);
  buf.write("data", 36); buf.writeUInt32LE(n * 2, 40);
  for (let i = 0; i < n; i++) {
    let s = Math.max(-1, Math.min(1, f32[i]));
    buf.writeInt16LE((s * 32767) | 0, 44 + i * 2);
  }
  writeFileSync(path, buf);
}
function midiToFreq(m) { return 440 * Math.pow(2, (m - 69) / 12); }

// Guide tone — `count` metronome clicks one beat apart, then a soft
// reference sine at the note pitch for the note's duration.
function buildGuide(note, beatSec, count) {
  const clickN = Math.floor(beatSec * count * SR);
  const toneN  = Math.floor(note.durSec * SR);
  const f32 = new Float32Array(clickN + toneN);
  // clicks
  for (let c = 0; c < count; c++) {
    const at = Math.floor(c * beatSec * SR);
    const accent = c === count - 1 ? 1.0 : 0.6;
    for (let i = 0; i < 0.04 * SR && at + i < f32.length; i++) {
      const env = Math.exp(-i / (0.012 * SR));
      f32[at + i] += Math.sin(2 * Math.PI * (c === count - 1 ? 1600 : 1100) * i / SR)
                   * env * 0.5 * accent;
    }
  }
  // reference tone — soft sine, fades in/out
  const f = midiToFreq(note.midi);
  for (let i = 0; i < toneN; i++) {
    const u = i / toneN;
    const env = 0.5 - 0.5 * Math.cos(2 * Math.PI * Math.min(u, 1 - u, 0.15) / 0.15);
    f32[clickN + i] = Math.sin(2 * Math.PI * f * i / SR) * 0.32 * env;
  }
  return f32;
}

function afplay(path) { spawnSync("afplay", [path], { stdio: "ignore" }); }

function recordMic(path, durSec) {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
    "-f", "avfoundation", "-i", DEVICE, "-t", String(durSec),
    "-ac", "1", "-ar", String(SR), path], { stdio: "inherit" });
  return r.status === 0 && existsSync(path);
}

const rl = createInterface({ input: process.stdin, output: process.stdout });
const ask = (q) => new Promise((res) => rl.question(q, (a) => res(a.trim().toLowerCase())));

// ── free-sample mode — record an arbitrary one-shot (a rattle, a clap,
// a found sound), no score + no pitch reference. Writes the take to
// pop/<track>/samples/<name>.wav, where a track's renderer can load it.
//   node pop/bin/rfa.mjs --sample --track hellsine --name rattle [--dur 4]
if (flags.sample) {
  const name = String(flags.name || "sample").replace(/[^a-z0-9_-]/gi, "");
  const durSec = Number(flags.dur || 4);
  const SAMP_DIR = resolve(LANE_DIR, "samples");
  mkdirSync(SAMP_DIR, { recursive: true });
  const dest = resolve(SAMP_DIR, `${name}.wav`);
  console.log(`\n🎙  request for audio · free sample · ${TRACK}/${name}`);
  console.log(`   device ${DEVICE} · ${durSec}s window · ctrl-c to stop\n`);
  let keep = false;
  while (!keep) {
    await ask(`   ⏎ to record "${name}" — perform right after Enter (${durSec}s)… `);
    console.log(`   ● RECORDING`);
    const ok = recordMic(dest, durSec);
    if (!ok) {
      console.error(`   ✗ recording failed — check --device (--list-devices)`);
      const a = await ask(`   [r]etry / [q]uit: `);
      if (a === "q") { rl.close(); process.exit(1); }
      continue;
    }
    console.log(`   ▸ playback…`);
    afplay(dest);
    const a = await ask(`   [k]eep / [r]edo: `);
    if (a === "k" || a === "") keep = true;
  }
  rl.close();
  console.log(`\n   ✓ kept → ${dest.replace(POP + "/", "pop/")}`);
  console.log(`   re-render the track to hear it in.\n`);
  process.exit(0);
}

// ── the record loop ───────────────────────────────────────────────────
const COUNT = 3;                             // count-in beats
const beatSec = manifest.beatSec;
const tmpGuide = resolve(TAKES_DIR, ".guide.wav");

let todo = manifest.notes;
if (flags.only) {
  todo = manifest.notes.filter((n) => n.id === String(flags.only));
  if (!todo.length) { console.error(`✗ no note with id ${flags.only}`); process.exit(1); }
} else {
  todo = manifest.notes.filter((n) => !hasTake(n.id));   // missing only
}

if (!todo.length) {
  console.log(`\n✓ ${manifest.track} — every note already has a take. ` +
    `(--only <id> to re-record one.)\n`);
  process.exit(0);
}

console.log(`\n🎙  request for audio · ${manifest.track} · ${todo.length} note(s) to record`);
console.log(`   device ${DEVICE} · headphones recommended · ctrl-c to stop\n`);

for (let idx = 0; idx < todo.length; idx++) {
  const note = todo[idx];
  console.log(`── [${idx + 1}/${todo.length}]  note ${note.id} ─────────────────────`);
  console.log(`   sing "${note.vowel}"  on  ${note.note}  ` +
    `· ${note.durSec.toFixed(2)}s · bar ${note.bar} @ ${note.startSec.toFixed(1)}s`);

  let keep = false;
  while (!keep) {
    // 1. guide — listen
    writeWavMono(tmpGuide, buildGuide(note, beatSec, COUNT), SR);
    console.log(`   ▸ listen (count-in + reference tone)…`);
    afplay(tmpGuide);
    // 2. count-in, then record
    await ask(`   ⏎ to record (${COUNT}-beat count-in then sing)… `);
    writeWavMono(tmpGuide, buildGuide({ ...note, durSec: 0.001 }, beatSec, COUNT), SR);
    afplay(tmpGuide);                          // count-in only
    console.log(`   ● RECORDING — sing now`);
    const ok = recordMic(takePath(note.id), note.durSec + 1.0);
    if (!ok) {
      console.error(`   ✗ recording failed — check --device (--list-devices)`);
      const a = await ask(`   [r]etry / [s]kip / [q]uit: `);
      if (a === "q") { rl.close(); process.exit(1); }
      if (a === "s") break;
      continue;
    }
    // 3. play back, review
    console.log(`   ▸ playback…`);
    afplay(takePath(note.id));
    const a = await ask(`   [k]eep / [r]edo / [s]kip: `);
    if (a === "s") { try { unlinkSync(takePath(note.id)); } catch {} break; }
    if (a === "k" || a === "") keep = true;
    // anything else → redo
  }
  if (keep) console.log(`   ✓ kept → voice-takes/${note.id}.wav\n`);
  else      console.log(`   · skipped\n`);
}

try { unlinkSync(tmpGuide); } catch {}
const done = manifest.notes.filter((n) => hasTake(n.id)).length;
console.log(`\ndone · ${done}/${manifest.notes.length} takes recorded`);

// ── recompile + playback — close the loop. If the lane has a renderer,
// offer to rebuild the track with the fresh takes and play it back.
const renderer = resolve(LANE_DIR, "bin", "render.mjs");
if (done > 0 && existsSync(renderer)) {
  const a = await ask(`\n   ▸ recompile ${TRACK} with your voice + play it back? [Y/n] `);
  if (a !== "n" && a !== "q") {
    console.log(`   ⚙  rendering…`);
    spawnSync("node", [renderer, "--play"], { stdio: "inherit" });
  }
} else {
  console.log(`re-render the track to hear them in.`);
}
rl.close();
