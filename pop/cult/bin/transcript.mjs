#!/usr/bin/env node
// transcript.mjs — a word-level transcript of the shipped record.
//
// @jeffrey: "make me a 'sample transcript' so i can just see timestamps of
// every word said in the song, so we can make specific edits". The events
// receipt already knows every sample the score placed; this maps sample
// names back to the words they say and prints them in SHIPPED time (the
// cut starts 15.95 s into the full render), one line per utterance, with
// who is speaking and which act it lands in. Non-verbal voices (kicks,
// beeps, skids, friction) are left out — this is the libretto, not the
// score.
//
//   node pop/cult/bin/transcript.mjs            # prints + writes .transcript.txt

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const OUT = resolve(dirname(fileURLToPath(import.meta.url)), "../out");
const TRIM = 15.95;
const CUTS = [[20, 58], [120, 128], [136, 144], [168, 184]];

function shipTime(fullTime) {
  let removed = 0;
  for (const [start, end] of CUTS) {
    if (fullTime < start) break;
    if (fullTime < end) return null;
    removed += end - start;
  }
  return fullTime - TRIM - removed;
}

const receipt = JSON.parse(readFileSync(join(OUT, "cult-remix-v10.events.json"), "utf8"));
const events = receipt.events ?? receipt;

// Acts in shipped bars (bar 8 of the full render is shipped 0:00).
const ACTS = [
  [8, "II THREE VOICES"], [24, "III THE MESSAGE"], [40, "IV THE SECRET"],
  [48, "V THE REPLY"], [64, "VI IT SPREADS"], [76, "VII THE WHOLE MESSAGE"],
  [96, "VIII RECOGNITION"], [104, "IX CARRIER OFF"],
];
const actAt = (bar) => { let a = ACTS[0][1]; for (const [b, n] of ACTS) if (bar >= b) a = n; return a; };

function words(name) {
  if (/^runrealfast-fast/.test(name)) return "run-real-fast (double-time)";
  if (/^runrealfast/.test(name)) return "run real fast";
  if (/^runitfast/.test(name)) return "run it fast";
  if (/^hideaway/.test(name)) return "hide away";
  if (/^iwannahide/.test(name)) return "i wanna hide";
  if (/^iwanna/.test(name)) return "i wanna";
  if (/^away/.test(name)) return "a-waaay";
  if (/^dotdotdash/.test(name)) return "dot dot dash";
  if (/^(dot|voxdot)-/.test(name)) return "dot";
  if (/^altdot-(\d+)-long$/.test(name)) return `dot (held, post ${name.split("-")[1]}…)`;
  if (/^alt-(\d+)-dot/.test(name)) return `dot (post ${name.split("-")[1]}…)`;
  if (/^alt-(\d+)-cult/.test(name)) return `cult (post ${name.split("-")[1]}…)`;
  if (/^alt-(\d+)-threeofus/.test(name)) return `the three of us (post ${name.split("-")[1]}…)`;
  if (/^(cult|cultlong)-/.test(name)) return "cult";
  if (/^(dash|dashlong|bassdash|sos-dash)/.test(name)) return "dash";
  if (/^eleven-dotdot/.test(name)) return "dot dot";
  if (/^eleven-dash/.test(name)) return "dash";
  if (name === "hook-spoken") return "[spoken hook]";
  if (name === "chant-full") return "[full chant]";
  if (name === "three-of-us") return "the three of us";
  if (name === "tagline") return "[tagline]";
  if (name === "hide-away") return "hide away";
  return null;
}

const mmss = (s) => `${Math.floor(s / 60)}:${(s % 60).toFixed(1).padStart(4, "0")}`;

const lines = [];
for (const e of events) {
  if (typeof e.sample !== "string") continue;
  const w = words(e.sample);
  if (!w) continue;
  const t = shipTime(e.t);
  if (t == null || t < -0.5) continue;
  const bar = e.t / 2;
  lines.push({
    t, text: `${mmss(Math.max(0, t))}  ${w}${e.who ? `  · ${e.who}` : ""}` +
      `  · ${e.sample}  · g${e.gain}  · ${actAt(bar)}  · bar ${bar.toFixed(1)}`,
  });
}
lines.sort((a, b) => a.t - b.t);

const text = [
  "whistlegraph cult --- remix (v10.1) — word transcript, shipped time",
  `${lines.length} utterances · two-bar opening buildup`,
  "",
  ...lines.map((l) => l.text),
].join("\n");

writeFileSync(join(OUT, "cult-remix-v10.transcript.txt"), text + "\n");
console.log(text);
console.log(`\n✓ ${join(OUT, "cult-remix-v10.transcript.txt")}`);
