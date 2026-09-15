#!/usr/bin/env node
// amazing-score.mjs — the shipped-time score of "amazing grace" as the
// JSON pop/viz/lyric-reel.py reads (the shape cult/viz/review-score.py
// --dump writes): tokens on the jeffrey rail, an event lane for the sung
// notes, an event lane for the bed's cells, an envelope lane for the bed,
// the cell grid as bars, the sections as acts.
//
//   node pop/big-pictures/bin/amazing-score.mjs
//   → pop/big-pictures/out/amazing-grace/amazing-grace-score.json
import { readFileSync, writeFileSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");
const OUT = resolve(LANE, "out/amazing-grace");
const PY = resolve(REPO, "pop/.venv/bin/python");
const receipt = JSON.parse(readFileSync(`${OUT}/vox-receipt.json`, "utf8"));
const INTRO = 6.0;
const SPB = receipt.beatSec;
const DUR = +execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "csv=p=0", `${OUT}/amazing-grace-release.flac`], { encoding: "utf8" });
const tb = (b) => +(INTRO + b * SPB).toFixed(3);

// cells — mirror c/amazinhym.c CELLS (beat, len)
const CELLS = [[1,4],[5,4],[9,4],[13,6],[19,4],[23,4],[27,6],[33,4],[37,4],[41,4],[45,6],[51,4],[55,4],[59,5],[64,4]];
const bars = [{ bar: 0, t: tb(-7) }, { bar: 1, t: tb(-3) }];
CELLS.forEach(([b], i) => bars.push({ bar: i + 2, t: tb(b) }));
bars.push({ bar: CELLS.length + 2, t: tb(68) });

const JEFF = [236, 196, 96];
const tokens = receipt.words.map((w) => {
  const t0 = tb(w.notes[0].start / SPB), t1 = tb(w.notes[0].start / SPB + w.beats) - 0.06;
  return { word: w.text, disp: w.text, whos: ["jeffrey"], rails: { jeffrey: { t0, t1 } }, t0, t1 };
});
const vocalEvents = receipt.words.flatMap((w) => w.notes.map((n) => ({
  t0: tb(n.start / SPB), t1: +(tb(n.start / SPB + n.beats) - 0.06).toFixed(3),
  word: w.text, who: "jeffrey", midi: n.midi,
})));
const cellEvents = CELLS.map(([b, len], i) => ({
  t0: tb(b), t1: tb(b + len), word: ["G","G","G","C","G","G","D","G","G","C","C","G","D","G","C"][i], who: null, midi: null,
}));

// the reel's data buses — band-limited envelopes of the bed (and the
// sung lead as "signal"), 50 Hz, each named as lyric-reel.py expects
const envOf = (src, af) => {
  const tmp = `${OUT}/.bus-${af.replace(/[^a-z0-9]/gi, "")}.wav`;
  execFileSync("ffmpeg", ["-y", "-loglevel", "error", "-i", src, "-af", af, "-ac", "1", tmp]);
  return JSON.parse(execFileSync(PY, ["-W", "ignore", "-c", `
import json, sys, numpy as np, soundfile as sf
y, sr = sf.read(sys.argv[1], dtype="float64"); y = y.mean(axis=1) if y.ndim > 1 else y
per = sr // 50; n = len(y) // per
e = np.sqrt((y[:n*per].reshape(n, per) ** 2).mean(axis=1)); e = e / max(e.max(), 1e-9)
print(json.dumps([round(float(v), 3) for v in e]))
`, tmp], { encoding: "utf8" }));
};
const BED = `${OUT}/bed.wav`;
const VOXD = `${OUT}/.vox-delayed.wav`;
execFileSync("ffmpeg", ["-y", "-loglevel", "error", "-i", `${OUT}/vox.wav`, "-af", `adelay=${INTRO * 1000}|${INTRO * 1000},apad=whole_dur=${DUR}`, VOXD]);
const au = (name, color, env) => ({ name, kind: "au", color, clips: [[0, +DUR.toFixed(3)]], env_hz: 50, env });
const buses = [
  au("kick <150", [230, 150, 80], envOf(BED, "lowpass=f=150")),
  au("bass <250", [196, 120, 110], envOf(BED, "highpass=f=50,lowpass=f=250")),
  au("bed 250+", [120, 150, 190], envOf(BED, "highpass=f=250,lowpass=f=5000")),
  au("perc+skids", [214, 180, 120], envOf(BED, "highpass=f=5000")),
  au("signal", JEFF, envOf(VOXD, "anull")),
];

const acts = [
  { t0: 0, name: "intro", color: [214, 180, 120] },
  { t0: tb(0), name: "amazing grace", color: [236, 196, 96] },
  { t0: tb(18), name: "that saved a wretch", color: [196, 120, 110] },
  { t0: tb(32), name: "i once was lost", color: [150, 120, 190] },
  { t0: tb(50), name: "was blind", color: [230, 150, 80] },
  { t0: tb(64), name: "amen", color: [250, 230, 170] },
];
const score = {
  title: "amazing-grace", artist: "aesthetic dot computer",
  audio: `${OUT}/amazing-grace-release.flac`, dur: +DUR.toFixed(4),
  cover: resolve(LANE, "covers/amazing-grace/amazing-grace-cover.jpg"),
  who_colors: { jeffrey: JEFF },
  tokens,
  lanes: [
    { name: "vocal", kind: "ev", color: JEFF, events: vocalEvents },
    { name: "cells", kind: "ev", color: [150, 120, 190], events: cellEvents },
    ...buses,
  ],
  bars, acts, explosions: [],
};
writeFileSync(`${OUT}/amazing-grace-score.json`, JSON.stringify(score));
console.log(`✓ ${OUT}/amazing-grace-score.json · ${tokens.length} tokens · ${bars.length} bars · ${DUR.toFixed(2)}s`);
