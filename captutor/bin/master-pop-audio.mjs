#!/usr/bin/env node

// CLI for Captutor's reusable /pop spoken-word mastering lane.

import { existsSync } from "node:fs";
import { resolve } from "node:path";
import { masterPopAudio } from "../lib/pop-audio-master.mjs";

const value = (flag) => {
  const i = process.argv.indexOf(flag);
  return i === -1 ? null : process.argv[i + 1];
};
const input = resolve(value("--input") || "");
const out = resolve(value("--out") || "");
if (!input || !out || input === out || !existsSync(input)) {
  throw new Error("usage: master-pop-audio --input <video.mp4> --out <mastered.mp4>");
}
const result = await masterPopAudio({ input, out });
console.log(`${out}\n${result.receipt}\n${result.after.integratedLufs.toFixed(1)} LUFS · ${result.after.truePeakDbtp.toFixed(1)} dBTP`);
