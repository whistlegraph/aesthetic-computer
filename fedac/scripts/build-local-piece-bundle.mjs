#!/usr/bin/env node
// build-local-piece-bundle.mjs
//
// Build a local HTML piece bundle using the in-repo oven bundler.
// This avoids dependency on remote oven endpoints when infrastructure is down.

import path from "path";
import fs from "fs/promises";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const repoRoot = path.resolve(__dirname, "..", "..");

function usage(exitCode = 1) {
  console.log("Usage: node fedac/scripts/build-local-piece-bundle.mjs <piece-or-$code> [options]");
  console.log("");
  console.log("Options:");
  console.log("  --out <path>      Output HTML path (default: fedac/out/<piece>-local-piece.html)");
  console.log("  --density <n>     Bundle density hint (default: 8)");
  console.log("  --nocompress      Disable self-extracting compression wrapper");
  console.log("  --noboxart        Disable generated box art image");
  process.exit(exitCode);
}

const args = process.argv.slice(2);
if (args.length === 0) usage(1);
if (args.includes("--help") || args.includes("-h")) usage(0);

let target = "";
let outPath = "";
let density = 8;
let nocompress = false;
let noboxart = false;

for (let i = 0; i < args.length; i += 1) {
  const arg = args[i];
  if (arg === "--out") {
    outPath = args[++i] || "";
  } else if (arg === "--density") {
    density = Number.parseInt(args[++i] || "", 10);
  } else if (arg === "--nocompress") {
    nocompress = true;
  } else if (arg === "--noboxart") {
    noboxart = true;
  } else if (!target) {
    target = arg;
  } else {
    console.error(`Unknown argument: ${arg}`);
    usage();
  }
}

if (!target) usage();
if (!Number.isFinite(density) || density <= 0) {
  console.error("--density must be a positive integer");
  process.exit(1);
}

const pieceSlug = target.replace(/^\$/, "");
if (!outPath) {
  outPath = path.join(repoRoot, "fedac", "out", `${pieceSlug}-local-piece.html`);
}
outPath = path.resolve(outPath);

if (!process.env.AC_SOURCE_DIR) {
  process.env.AC_SOURCE_DIR = path.join(repoRoot, "system", "public", "aesthetic.computer");
}

const { createBundle, createJSPieceBundle } = await import(path.join(repoRoot, "oven", "bundler.mjs"));
const isKidLisp = target.startsWith("$");

const onProgress = (progress) => {
  if (!progress?.message) return;
  const stage = progress.stage ? `[${progress.stage}]` : "[bundle]";
  console.log(`${stage} ${progress.message}`);
};

const result = isKidLisp
  ? await createBundle(target, onProgress, nocompress, density, false, noboxart)
  : await createJSPieceBundle(pieceSlug, onProgress, nocompress, density, false, noboxart);

await fs.mkdir(path.dirname(outPath), { recursive: true });
await fs.writeFile(outPath, result.html, "utf8");

const sizeBytes = Buffer.byteLength(result.html, "utf8");
const sizeMiB = (sizeBytes / (1024 * 1024)).toFixed(2);
console.log(`Wrote ${outPath} (${sizeMiB} MiB)`);
