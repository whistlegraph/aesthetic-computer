#!/usr/bin/env node
// separate-stems.mjs — split a sample's source into Demucs stems with a
// progress heartbeat the Slab menubar can pick up.
//
// Reads pop/samples/<slug>/source.{wav,m4a}, runs Demucs (htdemucs by
// default) on the pop/.venv, and writes drums/bass/vocals/other into
// pop/samples/<slug>/stems/.
//
// Usage:
//   node pop/bin/separate-stems.mjs --slug oskie-bounce
//   node pop/bin/separate-stems.mjs --slug oskie-bounce --model htdemucs_ft

import { existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn } from "node:child_process";
import * as progress from "../lib/render-progress.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP = resolve(HERE, "..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
  else flags[a.slice(2)] = true;
}

if (!flags.slug) {
  console.error("usage: separate-stems.mjs --slug <sample-slug> [--model htdemucs]");
  process.exit(1);
}

const slug = flags.slug;
const model = flags.model || "htdemucs";
const sampleDir = resolve(POP, "samples", slug);
const source = ["source.wav", "source.m4a"]
  .map((n) => resolve(sampleDir, n))
  .find((p) => existsSync(p));

if (!source) {
  console.error(`no source.wav / source.m4a in ${sampleDir}`);
  process.exit(1);
}

const outDir = resolve(sampleDir, "stems");
const py = resolve(POP, ".venv/bin/python3");

console.log(`▶ demucs ${model} → ${source}`);
progress.begin({ type: "audio", label: `stems: ${slug}` });
progress.update(null);  // indeterminate until demucs reports a %

const child = spawn(py, [
  "-m", "demucs",
  "-n", model,
  "-d", "cpu",
  "-o", outDir,
  "--filename", "{stem}.{ext}",
  source,
], { cwd: sampleDir });

// Demucs prints tqdm bars to stderr like "  47%|████      | 5.3/11.3 ..."
// Strip ANSI and pick the last %-token on each chunk.
const PCT = /(\d{1,3})%/g;
function relay(buf, label) {
  const s = buf.toString();
  process[label].write(s);
  let last = null, m;
  while ((m = PCT.exec(s)) !== null) last = +m[1];
  if (last != null) progress.update(Math.min(99, last));
}
child.stdout.on("data", (b) => relay(b, "stdout"));
child.stderr.on("data", (b) => relay(b, "stderr"));

child.on("close", (code) => {
  if (code === 0) {
    progress.update(100);
    progress.end();
    console.log(`✓ stems → ${outDir}/${model}/`);
  } else {
    progress.end();
    console.error(`demucs exited ${code}`);
    process.exit(code ?? 1);
  }
});
