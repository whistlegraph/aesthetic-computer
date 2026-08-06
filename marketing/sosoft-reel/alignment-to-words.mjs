#!/usr/bin/env node
import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { SCENES } from "./timing.mjs";

const ROOT = dirname(fileURLToPath(import.meta.url));
const text = readFileSync(resolve(ROOT, "narration.txt"), "utf8").trim();
const { alignment } = JSON.parse(readFileSync(resolve(ROOT, "out", "narration-alignment.json"), "utf8"));
const words = [];
for (const match of text.matchAll(/\S+/g)) {
  const from = match.index;
  const to = from + match[0].length - 1;
  words.push({
    text: match[0],
    fromMs: Math.round(alignment.character_start_times_seconds[from] * 1000),
    toMs: Math.round(alignment.character_end_times_seconds[to] * 1000),
  });
}
writeFileSync(resolve(ROOT, "out", "words.json"), JSON.stringify(words, null, 2) + "\n");
const starts = SCENES.map(([, , phrase]) => phrase ? alignment.character_start_times_seconds[text.indexOf(phrase)] : 0);
const totalDuration = alignment.character_end_times_seconds.at(-1);
writeFileSync(resolve(ROOT, "out", "narration-timeline.json"), JSON.stringify({
  source: "synthetic",
  totalDuration,
  lines: SCENES.map(([id, title], i) => ({ id, title, startSec: starts[i], endSec: starts[i + 1] ?? totalDuration })),
}, null, 2) + "\n");
writeFileSync(resolve(ROOT, "out", "narration-source.json"), JSON.stringify({
  kind: "synthetic", audio: "out/narration.mp3", generatedAt: new Date().toISOString(),
}, null, 2) + "\n");
console.log(`wrote ${words.length} exactly timed words`);
