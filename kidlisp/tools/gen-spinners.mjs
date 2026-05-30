#!/usr/bin/env node
// gen-spinners.mjs — generate a catalog of N spinner kidlisp pieces by
// crossing fade palettes with motion expressions.
//
// Usage:
//   node kidlisp/tools/gen-spinners.mjs                 → prints 256 sources, one per line
//   node kidlisp/tools/gen-spinners.mjs --out file.txt  → writes to file
//   node kidlisp/tools/gen-spinners.mjs --json          → JSON with palette+motion+source per entry

import { writeFileSync } from "node:fs";

const PALETTES = [
  // Classic rainbow tokens (AC special fade tokens)
  { name: "red-rainbow", fade: "red-rainbow" },
  { name: "orange-rainbow", fade: "orange-rainbow" },
  { name: "yellow-rainbow", fade: "yellow-rainbow" },
  { name: "green-rainbow", fade: "green-rainbow" },
  // Single-color sandwiches (ROYGBIV anchor)
  { name: "red-band", fade: "black-red-black" },
  { name: "orange-band", fade: "black-orange-black" },
  { name: "yellow-band", fade: "black-yellow-black" },
  { name: "green-band", fade: "black-green-black" },
  { name: "blue-band", fade: "black-blue-black" },
  { name: "indigo-band", fade: "black-indigo-black" },
  { name: "violet-band", fade: "black-violet-black" },
  // Themed multi-stop gradients
  { name: "sunset", fade: "black-orange-red-purple-black" },
  { name: "ocean", fade: "navy-blue-cyan-teal-navy" },
  { name: "fire", fade: "black-red-yellow-red-black" },
  { name: "ice", fade: "white-cyan-blue-indigo-black" },
  { name: "vapor", fade: "pink-cyan-purple-cyan-pink" },
  { name: "midnight", fade: "black-purple-navy-purple-black" },
  { name: "lava", fade: "red-orange-black-orange-red" },
  { name: "aurora", fade: "green-cyan-purple-cyan-green" },
  { name: "sakura", fade: "white-pink-magenta-pink-white" },
  { name: "candy", fade: "pink-magenta-yellow-cyan-white" },
  { name: "jungle", fade: "darkgreen-green-yellow-green-darkgreen" },
  { name: "autumn", fade: "brown-orange-red-orange-brown" },
  { name: "cosmos", fade: "black-purple-pink-orange-yellow" },
  // Original mint reference
  { name: "purple-red", fade: "black-purple-red-purple-black" },
  // Full ROYGBIV
  { name: "roygbiv", fade: "red-orange-yellow-green-blue-indigo-violet" },
  { name: "roygbiv-reverse", fade: "violet-indigo-blue-green-yellow-orange-red" },
];

const MOTIONS = [
  { name: "spin", expr: "frame" },
  { name: "fast-spin", expr: "(* frame 5)" },
  { name: "slow-spin", expr: "(/ frame 3)" },
  { name: "swing", expr: "(+ frame (* (sin (* frame 0.05)) 60))" },
  { name: "wobble", expr: "(+ frame (* (sin frame) 50))" },
  { name: "accelerate", expr: "(* frame frame 0.001)" },
  { name: "decelerate", expr: "(* (sqrt frame) 30)" },
  { name: "bounce", expr: "(abs (* (sin (* frame 0.03)) 360))" },
  { name: "pendulum", expr: "(* (sin (* frame 0.02)) 180)" },
  { name: "pulse", expr: "(+ frame (* (sin (* frame 0.1)) 30))" },
  { name: "jitter", expr: "(+ frame (* (sin (* frame 2)) 20))" },
  { name: "reverse", expr: "(- 0 frame)" },
  { name: "chaos", expr: "(+ frame (* (sin (* frame 0.07)) (cos (* frame 0.13)) 200))" },
  { name: "meditative", expr: "(* (sin (* frame 0.01)) 90)" },
  { name: "exp-sin", expr: "(* frame (sin (* frame 0.05)))" },
  { name: "double", expr: "(+ frame (* (sin (* frame 0.1)) (sin (* frame 0.03)) 100))" },
];

const args = process.argv.slice(2);
const opts = {};
for (let i = 0; i < args.length; i++) {
  if (args[i] === "--out") opts.out = args[++i];
  else if (args[i] === "--json") opts.json = true;
  else if (args[i] === "--catalog") opts.catalog = true;
}

const entries = [];
let n = 0;
for (const p of PALETTES) {
  for (const m of MOTIONS) {
    entries.push({
      index: n++,
      palette: p.name,
      motion: m.name,
      source: `(wipe fade:${p.fade}:${m.expr})`,
    });
    if (n >= 256) break;
  }
  if (n >= 256) break;
}

let output;
if (opts.json) {
  output = JSON.stringify(entries, null, 2);
} else if (opts.catalog) {
  output = entries
    .map((e) => `${String(e.index).padStart(3, "0")}  ${e.palette.padEnd(20)} ${e.motion.padEnd(14)} ${e.source}`)
    .join("\n");
} else {
  output = entries.map((e) => e.source).join("\n");
}

if (opts.out) {
  writeFileSync(opts.out, output);
  console.error(`wrote ${entries.length} entries to ${opts.out}`);
} else {
  console.log(output);
}
