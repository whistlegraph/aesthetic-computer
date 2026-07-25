#!/usr/bin/env node
// Convert Special Sign's C-engine score plus the accepted Jeffrey-choir
// provenance into the generic scorodeon contract. The release trim is applied
// here so time zero in the picture is byte-aligned with the locked master.

import {
  existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { decodeAudioMono } from "../../lib/preview-shared.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const LANE = resolve(HERE, "..");
const RELEASE = resolve(LANE, "release/special-sign");
const arg = (key, fallback) => {
  const i = process.argv.indexOf(key);
  return i >= 0 && process.argv[i + 1] ? resolve(process.argv[i + 1]) : fallback;
};
const audioPath = arg("--audio", resolve(RELEASE, "special-sign-MASTER.wav"));
const provenancePath = arg("--provenance", resolve(RELEASE, "special-sign.provenance.json"));
const outPath = arg("--out", resolve(RELEASE, "special-sign.scorodeon.json"));
for (const path of [audioPath, provenancePath]) {
  if (!existsSync(path)) throw new Error(`missing input: ${path}`);
}

const work = mkdtempSync(join(tmpdir(), "special-sign-score-"));
const engineBin = resolve(work, "spatial-sineabye");
const engineJson = resolve(work, "engine-score.json");
const cDir = resolve(LANE, "c");
const run = (command, args, cwd = REPO) => {
  const result = spawnSync(command, args, { cwd, stdio: "inherit" });
  if (result.status !== 0) throw new Error(`${command} failed (${result.status})`);
};
try {
  run("cc", ["-O3", "-std=c11", "-Wall", "-Wextra", "-o", engineBin,
    resolve(cDir, "spatial-sineabye.c"), "-lm"]);
  run(engineBin, ["--score-data", engineJson, "--score-only"], cDir);

  const engine = JSON.parse(readFileSync(engineJson, "utf8"));
  const provenance = JSON.parse(readFileSync(provenancePath, "utf8"));
  const start = provenance.releaseStartSeconds;
  const dur = provenance.duration;
  const clip = (event) => {
    const t0 = Math.max(start, event.t);
    const t1 = Math.min(start + dur, event.t + event.dur);
    if (t1 <= t0) return null;
    return { ...event, t: t0 - start, dur: t1 - t0 };
  };
  const midi = (hz) => hz > 0 ? 69 + 12 * Math.log2(hz / 440) : null;
  const sourceEvents = engine.sources.map((source) => {
    const raw = engine.events.filter((event) => event.source === source.index)
      .map(clip).filter(Boolean);
    const maxGain = Math.max(1e-9, ...raw.map((event) => event.g));
    return {
      name: source.name,
      color: source.color,
      events: raw.map((event) => ({
        t: +event.t.toFixed(6),
        dur: +event.dur.toFixed(6),
        pitch: event.kind === "noise" || event.f0 <= 0
          ? null : +midi(Math.sqrt(event.f0 * Math.max(event.f0, event.f1))).toFixed(3),
        g: +(0.10 + 0.44 * Math.sqrt(event.g / maxGain)).toFixed(3),
        kind: event.kind,
      })),
    };
  });
  const choirEvents = provenance.events.map((event) => clip({
    t: event.start, dur: event.dur, pitch: event.midi, g: 0.24,
    vowel: event.vowel, pan: event.pan,
  })).filter(Boolean).map((event) => ({
    t: +event.t.toFixed(6), dur: +event.dur.toFixed(6),
    pitch: event.pitch, g: event.g, vowel: event.vowel, pan: event.pan,
  }));

  const { audio, sr } = decodeAudioMono(audioPath);
  const arc = [];
  for (let second = 0; second <= Math.ceil(dur); second++) {
    const a = Math.max(0, Math.floor((second - 0.5) * sr));
    const b = Math.min(audio.length, Math.ceil((second + 0.5) * sr));
    let sum = 0;
    for (let i = a; i < b; i++) sum += audio[i] * audio[i];
    arc.push(Math.sqrt(sum / Math.max(1, b - a)));
  }
  const arcPeak = Math.max(...arc, 1e-9);
  for (let i = 0; i < arc.length; i++) arc[i] = +Math.sqrt(arc[i] / arcPeak).toFixed(4);

  const bar = 4 * 60 / provenance.bpm;
  const chordNames = ["C", "Am", "F", "G"];
  const bridgeNames = ["F", "Dm", "B♭", "C"];
  const chords = [];
  for (let sourceBar = 6; sourceBar <= 37; sourceBar++) {
    const t = (sourceBar - 6) * bar;
    const names = sourceBar >= 20 && sourceBar < 28 ? bridgeNames : chordNames;
    const name = sourceBar === 36 ? "G7" : sourceBar === 37 ? "Cmaj9" : names[sourceBar % 4];
    chords.push({ t: +t.toFixed(6), dur: +Math.min(bar, dur - t).toFixed(6), name });
  }

  const window = (t, a, b) => t < a || t > b ? 0 : Math.pow(Math.sin(Math.PI * (t - a) / (b - a)), 2);
  const spins = [
    [32.6 - start, 38.8 - start, 1],
    [43.6 - start, 49.5 - start, 0.65],
    [provenance.superSpin.start - start, provenance.superSpin.end - start, 0.92],
    [85.7 - start, 94.3 - start, 0.78],
    [dur - 6, dur, 0.55],
  ];
  const spinEnv = Array.from({ length: Math.ceil(dur) + 1 }, (_, t) =>
    +Math.min(1, spins.reduce((sum, [a, b, level]) => sum + level * window(t, a, b), 0)).toFixed(4));
  const gravityEnv = Array.from({ length: Math.ceil(dur) + 1 }, (_, t) => {
    const active = sourceEvents[1].events.some((event) => t >= event.t && t < event.t + Math.min(event.dur, 0.8));
    return active ? 1 : 0.08;
  });

  const centralSpinStart = provenance.superSpin.start - start;
  const centralSpinEnd = provenance.superSpin.end - start;
  const score = {
    title: "Special Sign",
    artist: "Aesthetic Dot Computer",
    dur,
    movements: [
      { name: "I · Assembly", sub: "bodies enter high to low", t0: 0, t1: 18.95, level: 0.38 },
      { name: "II · Signal", sub: "question and answer", t0: 18.95, t1: centralSpinStart, level: 0.58 },
      { name: "III · Super-Spin", sub: "eight listener-relative turns", t0: centralSpinStart, t1: centralSpinEnd, level: 1 },
      { name: "IV · Constellation", sub: "counterpoint and sine garden", t0: centralSpinEnd, t1: 75.4, level: 0.82 },
      { name: "V · Home Sign", sub: "G7 resolves to C major", t0: 75.4, t1: dur - 6, level: 0.62 },
      { name: "VI · Run-Down", sub: "the physical field reaches rest", t0: dur - 6, t1: dur, level: 0.28 },
    ],
    chords,
    arc,
    goldenSec: +(centralSpinStart + (centralSpinEnd - centralSpinStart) / 2).toFixed(6),
    lanes: [...sourceEvents, {
      name: "jeffrey vowels", color: "#8E5A77", events: choirEvents,
    }],
    ribbons: [
      { name: "spatial rotation", color: "#3E7C8A", env: spinEnv },
      { name: "kick gravity", color: "#B3402E", env: gravityEnv },
    ],
    release: {
      master: audioPath.replace(REPO + "/", ""),
      provenance: provenancePath.replace(REPO + "/", ""),
      sourceTrimSeconds: start,
      authoredEngineEvents: engine.events.length,
      visibleEngineEvents: sourceEvents.reduce((sum, lane) => sum + lane.events.length, 0),
      choirEvents: choirEvents.length,
    },
  };
  mkdirSync(dirname(outPath), { recursive: true });
  writeFileSync(outPath, JSON.stringify(score, null, 2) + "\n");
  console.log(`✓ ${outPath}`);
  console.log(`  ${score.release.visibleEngineEvents} engine events + ${choirEvents.length} choir events · ${score.lanes.length} lanes · ${dur}s`);
} finally {
  rmSync(work, { recursive: true, force: true });
}
