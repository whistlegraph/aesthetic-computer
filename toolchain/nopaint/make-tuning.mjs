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
// input, so the game writes the parameters itself, as it does for any brush.
tuning.ai = {
  // Guests get the service's one free turn per browser. A signed-in painter
  // pays Braincells, so their AI turns are off until they turn them on.
  guests: true,
  // The AI brush's parameters (AI: <strength> <model> <note>), each rolled
  // from these weights when it is not given, as a brush's "?" is.
  // strength: how much of the painting is repainted, in 100.
  strengths: { 25: 2, 50: 3, 75: 3 },
  // model: a short code for the label, and the model it names in the
  // /api/nopaint-inference catalog at 256 × 256 (the id after "ac-openrouter:").
  // Cheaper ones weigh more. Braincells a turn, 2026-11-25: hy, gpt, mini 4000;
  // flare 6000; seed, flux, krea 8000.
  models: {
    hy: { model: "tencent/hy-image-v3.5-preview", weight: 3 },
    gpt: { model: "openai/gpt-image-2", weight: 3 },
    mini: { model: "openai/gpt-image-1-mini", weight: 1 },
    flare: { model: "openai/gpt-image-2.5-flare", weight: 1 },
    seed: { model: "bytedance-seed/seedream-5-0-flash", weight: 2 },
    flux: { model: "black-forest-labs/flux.2-klein-4b", weight: 1 },
    krea: { model: "krea/krea-2-medium-turbo", weight: 1 },
  },
  // note: what to do. Mostly whole changes, a few small additions. Each is at
  // most 14 letters, so "AI: 75 flare <note>" fits the game's one line (28).
  notes: [
    "make it night",
    "change season",
    "another place",
    "go abstract",
    "another style",
    "zoom out",
    "make it a city",
    "underwater",
    "make it a map",
    "as a mosaic",
    "make it winter",
    "something else",
    "add weather",
    "add a creature",
    "add a pattern",
    "make it glow",
  ],
};
writeFileSync(new URL("./classic-tuning.json", import.meta.url), JSON.stringify(tuning, null, 2) + "\n");
console.log("wrote classic-tuning.json");
