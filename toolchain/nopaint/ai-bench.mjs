#!/usr/bin/env node
// ai-bench.mjs, 2026.11.25
// The visual bench (toolchain/visual-bench/bench.mjs) for No Paint classic's
// AI brush: the models in classic-tuning.json's ai.models, at 256 × 256, through
// /api/nopaint-inference, with a few of its notes. Same options as the bench.
//
//   node toolchain/nopaint/ai-bench.mjs                          the plan and its cost
//   AC_BEARER=<token> node toolchain/nopaint/ai-bench.mjs --yes   run it
//   ... --all                                                    every model in the catalog, not only the tuning's

import { readFileSync } from "node:fs";
import { join } from "node:path";
import { bench } from "../visual-bench/bench.mjs";

const ai = JSON.parse(readFileSync(new URL("./classic-tuning.json", import.meta.url), "utf8")).ai;
const args = process.argv.slice(2), value = (name, fallback) => { const at = args.indexOf(`--${name}`); return at >= 0 ? args[at + 1] : fallback; };
// A tuning model id is the catalog id after "ac-openrouter:"; the 256 one has no "@size".
const only = args.includes("--all") ? [] : Object.values(ai.models).map((entry) => `ac-openrouter:${entry.model}`);
await bench({
  yes: args.includes("--yes"), bearer: value("bearer", process.env.AC_BEARER || null), base: value("base", "https://aesthetic.computer"),
  paths: ["nopaint"], only, exact: only.length > 0, runs: Number(value("runs", 1)), max: Number(value("max", 25000)), size: 256,
  notes: ["make it night", "go abstract", "add a creature"].filter((note) => ai.notes.includes(note)).slice(0, Number(value("notes", 2))),
  canvases: ["landscape", "shapes", "strokes"].slice(0, Number(value("canvases", 2))),
  out: value("out", join("tmp", "nopaint-ai-bench", new Date().toISOString().replace(/[:.]/g, "-"))),
}).catch((error) => { console.error(error.message); process.exit(1); });
