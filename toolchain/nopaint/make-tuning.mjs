#!/usr/bin/env node
// make-tuning.mjs: write classic-tuning.json from the 2021 game's own weights
// (papers/arxiv-nopaint-brushbook/brushes.json), as a place to start tuning.
// It overwrites the file, so run it only to start again from 2021.
//
//   node toolchain/nopaint/make-tuning.mjs

import { readFileSync, writeFileSync } from "node:fs";

const { brushes } = JSON.parse(readFileSync(new URL("../../papers/arxiv-nopaint-brushbook/brushes.json", import.meta.url), "utf8"));
const tuning = {
  note: "The odds of each proposal in No Paint classic. split: the chance of each list. first: the list the first proposal comes from. norepeat: lists that never give the same brush twice running. An entry is a weight, or {in100: n} to ask for n in 100 of all proposals; when: \"ai\" offers it only while an AI turn can be had. Print the table: node toolchain/nopaint/classic-odds.mjs",
  // The 2021 game: Brushes two times in three, the first proposal always a brush, no filter twice running.
  split: { Brushes: 2, Filters: 1 },
  first: "Brushes",
  norepeat: ["Filters"],
  Brushes: {},
  Filters: {},
};
for (const brush of brushes) if (brush.table) tuning[brush.table][brush.name] = brush.weight;
// The AI brush is not in the 2021 game. It is its own kind, with its own list:
// its share of the split comes evenly off everything else.
tuning.split = { Brushes: 63.33, Filters: 31.67, AI: 5 };
tuning.AI = { AI: { weight: 1, when: "ai", ai: true } };
// An AI turn is Note Paint's move (/api/nopaint-inference). No Paint takes no
// input, so the game writes the note itself: one of these, at random.
tuning.ai = {
  strength: 0.5,
  // Guests get the service's one free turn per browser. A signed-in painter
  // pays Braincells, so their AI turns are off until they turn them on.
  guests: true,
  notes: [
    "add something surprising",
    "make it stranger",
    "add a small creature",
    "turn it into a landscape",
    "add weather",
    "make it glow",
    "fill the empty parts",
    "add a pattern",
    "make it look older",
    "add a face somewhere",
    "paint over it loosely",
    "add something from the sea",
  ],
};
writeFileSync(new URL("./classic-tuning.json", import.meta.url), JSON.stringify(tuning, null, 2) + "\n");
console.log("wrote classic-tuning.json");
