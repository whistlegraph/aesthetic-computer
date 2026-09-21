#!/usr/bin/env node
// register.mjs — measure where each member's voice actually SPEAKS, and
// write that into its profile so the composer places its lines there.
//
//   node bin/register.mjs            # all members with a voice.json
//
// For a member with remembered speech (neo → jeffrey's clone) the cached
// stems are the evidence. For a member that speaks with its own synthesizer
// (Kathy, Junior, Allison…) its autobiography's first sentences are rendered
// with `say -o` and measured. WORLD's f0 tracker gives the voiced frames;
// the profile gets `spoken_median_midi` and `register_midi` = [p10, p90] of
// the spoken pitch — the band the composer keeps the sung line inside, and
// the centre it aims the line's mean at (spinging rule: shifts stay small).

import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..", "..");
const PY = resolve(REPO, "pop/.venv/bin/python");

const MEASURE = `
import sys, json, numpy as np, pyworld, soundfile as sf
allv = []
for p in sys.argv[1:]:
    x, fs = sf.read(p)
    if x.ndim > 1: x = x.mean(axis=1)
    x = np.ascontiguousarray(x.astype(np.float64))
    f0, t = pyworld.harvest(x, fs, f0_floor=50.0, f0_ceil=600.0, frame_period=5.0)
    allv += list(f0[f0 > 0])
v = np.array(allv)
m = lambda hz: 69 + 12 * np.log2(hz / 440.0)
print(json.dumps({"median_hz": float(np.median(v)), "median_midi": float(m(np.median(v))),
  "p10_midi": float(m(np.percentile(v, 10))), "p90_midi": float(m(np.percentile(v, 90))), "frames": int(len(v))}))
`;

const NOTE = (m) => ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"][Math.round(m) % 12] + (Math.floor(Math.round(m) / 12) - 1);

for (const member of readdirSync(resolve(LANE, "members"))) {
  const vp = resolve(LANE, "members", member, "voice.json");
  if (!existsSync(vp)) continue;
  const prof = JSON.parse(readFileSync(vp, "utf8"));
  const av = prof.aesthetivox || (prof.aesthetivox = {});
  let files = [];
  let source;
  const speechDir = resolve(LANE, "members", member, "speech");
  if (av.speech?.provider && existsSync(speechDir)) {
    files = readdirSync(speechDir).map((h) => resolve(speechDir, h, "stem.wav")).filter(existsSync);
    source = `${files.length} cached stems (${av.speech.provider})`;
  }
  if (!files.length) {
    // its own synthesizer, its own words
    const bio = resolve(LANE, "members", member, "autobiography.md");
    const text = existsSync(bio)
      ? readFileSync(bio, "utf8").split("\n").filter((l) => l && !l.startsWith("#")).join(" ").split(/(?<=\.)\s/).slice(0, 4).join(" ")
      : "I am a MacBook Neo. This is the voice I was given.";
    const dir = mkdtemp();
    const aiff = resolve(dir, "bio.aiff"), wav = resolve(dir, "bio.wav");
    const voice = av.base_voice || "Fred";
    if (spawnSync("say", ["-v", voice, "-o", aiff, text]).status !== 0) { console.log(`✗ ${member}: say -v ${voice} failed`); continue; }
    spawnSync("afconvert", ["-f", "WAVE", "-d", "LEI16@22050", aiff, wav]);
    files = [wav];
    source = `say -v ${voice}, ${text.split(" ").length} words of its autobiography`;
  }
  const r = spawnSync(PY, ["-c", MEASURE, ...files], { encoding: "utf8" });
  if (r.status !== 0) { console.log(`✗ ${member}: measurement failed\n${r.stderr}`); continue; }
  const m = JSON.parse(r.stdout.trim().split("\n").pop());
  av.spoken_median_midi = +m.median_midi.toFixed(1);
  av.spoken_median_hz = +m.median_hz.toFixed(1);
  av.register_midi = [Math.round(m.p10_midi), Math.round(m.p90_midi)];
  av.register_source = source;
  av.register_note = "measured by bin/register.mjs — the composer aims each line's mean at spoken_median_midi (+1 for brightness) and keeps notes inside register_midi";
  writeFileSync(vp, JSON.stringify(prof, null, 2) + "\n");
  console.log(`♪ ${member.padEnd(10)} speaks at ${m.median_hz.toFixed(0)} Hz = ${NOTE(m.median_midi)} (midi ${m.median_midi.toFixed(1)}) · band ${NOTE(m.p10_midi)}–${NOTE(m.p90_midi)} · ${source}`);
}

function mkdtemp() { const d = resolve(tmpdir(), `register-${process.pid}-${Date.now()}`); mkdirSync(d, { recursive: true }); return d; }
