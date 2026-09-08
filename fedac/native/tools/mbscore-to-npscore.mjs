#!/usr/bin/env node
// mbscore → npscore. Menu Band scores come in two dialects: the app's flat
// auto-perform score ({ name, notes: [{midi,start,dur,velocity}] }, seconds)
// and the fleet score ({ title, bpm, voices: [{ program, velocity,
// notes: "60:0.25,r:1.5,…" }] }, durations in beats). Both normalize to:
//
//   .npscore — the AC Native performance score
//   {
//     "name": "prelude-in-c",
//     "bpm": 76,                     // informational
//     "leadSeconds": 3,              // silence before the first note
//     "tailSeconds": 0.5,            // hold after the last note-end
//     "voices": [{ "name", "program", "velocity",
//                  "notes": [{ "midi", "start", "dur", "velocity"? }] }],
//     "lights": [{ "start", "dur", "rgb": [r,g,b] }]
//   }
//
// All npscore times are seconds. When the source carries no lighting, cues
// are derived one-per-note from the notepat pitch-class palette (sharps are
// black — the black-key idiom carries to the room); the player averages
// whatever cues overlap, exactly like notepat's held-tone backdrop.
//
//   node mbscore-to-npscore.mjs <in.mbscore> [out.npscore]

import { readFileSync, writeFileSync } from "node:fs";
import { basename } from "node:path";

// notepat base palette by pitch class (c c# d … b); sharps black.
const PC_RGB = [
  [255, 50, 50], [0, 0, 0], [255, 160, 0], [0, 0, 0], [255, 230, 0],
  [50, 200, 50], [0, 0, 0], [50, 120, 255], [0, 0, 0], [130, 50, 200],
  [0, 0, 0], [180, 80, 255],
];

const [, , inPath, outPath] = process.argv;
if (!inPath) {
  console.error("usage: mbscore-to-npscore.mjs <in.mbscore> [out.npscore]");
  process.exit(1);
}

const src = JSON.parse(readFileSync(inPath, "utf8"));
const name = (src.name || src.title || basename(inPath).replace(/\.mbscore$/, ""))
  .toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");

let voices;
if (Array.isArray(src.voices)) {
  // Fleet dialect: compact "midi:beats" strings, r = rest, sequential.
  const spb = 60 / (src.bpm || 120);
  voices = src.voices.map((v, i) => {
    const notes = [];
    let cursor = 0;
    for (const tok of String(v.notes || "").split(",")) {
      const [n, beats] = tok.trim().split(":");
      const dur = (parseFloat(beats) || 0) * spb;
      if (n !== "r" && n !== "") notes.push({ midi: +n, start: +cursor.toFixed(4), dur: +dur.toFixed(4) });
      cursor += dur;
    }
    return { name: v.name || `voice-${i}`, program: v.program ?? 0,
             velocity: v.velocity ?? 100, notes };
  });
} else {
  // App dialect: one flat pre-timed voice.
  voices = [{
    name: "lead", program: src.program ?? 0, velocity: 100,
    notes: (src.notes || []).map(n => ({
      midi: n.midi, start: n.start, dur: n.dur,
      ...(n.velocity != null ? { velocity: n.velocity } : {}),
    })),
  }];
}

const lights = Array.isArray(src.lights) && src.lights.length
  ? src.lights
  : voices.flatMap(v => v.notes.map(n => ({
      start: n.start, dur: n.dur, rgb: PC_RGB[((n.midi % 12) + 12) % 12],
    }))).sort((a, b) => a.start - b.start);

const out = {
  name,
  ...(src.bpm ? { bpm: src.bpm } : {}),
  ...(src.lead ? { leadSeconds: src.lead } : {}),
  tailSeconds: src.tailSeconds ?? 0.5,
  voices,
  lights,
};

const dest = outPath || inPath.replace(/\.mbscore$/, "") + ".npscore";
writeFileSync(dest, JSON.stringify(out, null, 1) + "\n");
const noteCount = voices.reduce((s, v) => s + v.notes.length, 0);
console.log(`${dest} — ${voices.length} voice(s), ${noteCount} notes, ${out.lights.length} light cues`);
