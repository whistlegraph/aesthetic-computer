#!/usr/bin/env node
// mbscore → npscore. Menu Band scores come in two dialects: the app's flat
// auto-perform score ({ name, notes: [{midi,start,dur,velocity}] }, seconds)
// and the fleet score ({ title, bpm, voices: [{ program, velocity,
// notes: "60:0.25,r:1.5,k:1,…" }] }, durations in beats; letter tokens are
// drums — k kick, s snare, c clap, h closed hat, o open hat, t tambo,
// b ride, x crash). Both normalize to:
//
//   .npscore — the AC Native performance score
//   {
//     "name": "prelude-in-c",
//     "bpm": 76,                     // informational
//     "leadSeconds": 3,              // silence before the first note
//     "tailSeconds": 0.5,            // hold after the last note-end
//     "voices": [
//       { "name", "program", "velocity",
//         "notes": [{ "midi", "start", "dur", "velocity"? }] },
//       { "name", "kind": "percussion", "velocity",
//         "notes": [{ "drum": "kick", "start", "velocity"? }] }
//     ],
//     "lights": [
//       { "start", "dur", "rgb": [r,g,b], "gain": 0..1 },   // sustained
//       { "start", "decay": 0.15, "rgb", "gain" }           // flash (no dur)
//     ]
//   }
//
// All npscore times are seconds; velocity shapes BOTH volume and light gain.
// When the source carries no lighting, sustained cues derive one-per-note
// from the notepat pitch-class palette (sharps black — the black-key idiom
// carries to the room) and drums become white flashes; the player averages
// sustained cues and adds flashes on top, exactly like notepat's backdrop.
//
//   node mbscore-to-npscore.mjs <in.mbscore> [out.npscore]

import { readFileSync, writeFileSync } from "node:fs";
import { basename } from "node:path";

const PC_RGB = [
  [255, 50, 50], [0, 0, 0], [255, 160, 0], [0, 0, 0], [255, 230, 0],
  [50, 200, 50], [0, 0, 0], [50, 120, 255], [0, 0, 0], [130, 50, 200],
  [0, 0, 0], [180, 80, 255],
];

const DRUM_TOKEN = {
  k: "kick", s: "snare", c: "clap", h: "hat-c", o: "hat-o",
  t: "tambo", b: "ride", x: "crash",
};

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
  // Fleet dialect: compact "tok:beats" strings, r = rest, sequential.
  const spb = 60 / (src.bpm || 120);
  voices = src.voices.map((v, i) => {
    const tones = [], drums = [];
    let cursor = 0;
    for (const raw of String(v.notes || "").split(",")) {
      const [tok, beats] = raw.trim().split(":");
      const dur = (parseFloat(beats) || 0) * spb;
      if (DRUM_TOKEN[tok]) drums.push({ drum: DRUM_TOKEN[tok], start: +cursor.toFixed(4) });
      else if (tok !== "r" && tok !== "") tones.push({ midi: +tok, start: +cursor.toFixed(4), dur: +dur.toFixed(4) });
      cursor += dur;
    }
    const base = { name: v.name || `voice-${i}`, velocity: v.velocity ?? 100 };
    return drums.length
      ? { ...base, kind: "percussion", notes: drums }
      : { ...base, program: v.program ?? 0, notes: tones };
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

const gain = (n, v) => +(((n.velocity ?? v.velocity ?? 100) / 127).toFixed(3));
const lights = Array.isArray(src.lights) && src.lights.length
  ? src.lights
  : voices.flatMap(v => v.notes.map(n => v.kind === "percussion"
      ? { start: n.start, decay: n.drum === "kick" ? 0.18 : 0.1,
          rgb: [255, 255, 255], gain: gain(n, v) }
      : { start: n.start, dur: n.dur,
          rgb: PC_RGB[((n.midi % 12) + 12) % 12], gain: gain(n, v) }))
    .sort((a, b) => a.start - b.start);

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
