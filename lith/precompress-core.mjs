#!/usr/bin/env node
// Max-level Brotli sidecars for AC's critical-path modules, so lith's
// `file_server { precompressed br }` answers Cloudflare (which asks the origin
// for `br, gzip`) with brotli -q 11 instead of Caddy's on-the-fly gzip, which
// Cloudflare passes straight through to browsers. Measured 2026-11-25:
// bios.mjs 250 KB gz → 192 KB br, lib/disk.mjs 182 KB → 145 KB.
//
// Runs on lith after every checkout (deploys, rollbacks, the webhook) through
// xbox/tools/precompress-live.sh. Caddy serves a sidecar that exists without
// asking whether it matches its source, so freshness is kept here:
//   - a sidecar carries its source's exact mtime; any other mtime is stale;
//   - stale and orphaned sidecars are deleted BEFORE anything is compressed,
//     so a run that dies half way leaves missing sidecars (Caddy falls back
//     to `encode`), never wrong ones;
//   - each sidecar is written to a temp name and renamed into place.
// Only git-tracked files are covered: private products installed into the
// tree are left to `encode`.
//
// Usage: node lith/precompress-core.mjs [--dry]

import { execFileSync } from "node:child_process";
import { brotliCompressSync, constants } from "node:zlib";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const REPO = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const PUBLIC = path.join(REPO, "system/public");
const AC = path.join(PUBLIC, "aesthetic.computer");
const DRY = process.argv.includes("--dry");

// Directories whose sidecars this script owns (non-recursive except lib/).
const OWNED = [PUBLIC, AC, path.join(AC, "lib")];
const WORKER = /^disk\.worker\.[a-f0-9]+\.mjs$/;

function managedSources() {
  const tracked = execFileSync(
    "git",
    ["-C", REPO, "ls-files", "-z", "--",
      "system/public/sw.js",
      "system/public/aesthetic.computer/*.mjs",
      "system/public/aesthetic.computer/*.css",
      "system/public/aesthetic.computer/lib/"],
    { encoding: "utf8", maxBuffer: 64 * 1024 * 1024 },
  ).split("\0").filter(Boolean);

  // Of the ~150 committed worker bundles only the current one is fetched.
  let currentWorker = null;
  try {
    const manifest = JSON.parse(fs.readFileSync(path.join(AC, "lib/disk-worker-manifest.json"), "utf8"));
    currentWorker = manifest.filename;
  } catch {}

  const out = new Set();
  for (const rel of tracked) {
    if (!/\.(mjs|js|css)$/.test(rel)) continue;
    const base = path.basename(rel);
    if (WORKER.test(base) && base !== currentWorker) continue;
    const abs = path.join(REPO, rel);
    // Top-level ls-files globs match nested paths too; keep only owned dirs.
    if (!OWNED.includes(path.dirname(abs)) && !abs.startsWith(path.join(AC, "lib") + path.sep)) continue;
    if (fs.existsSync(abs)) out.add(abs);
  }
  return out;
}

function sidecarsIn(dir, recursive) {
  const found = [];
  let entries = [];
  try { entries = fs.readdirSync(dir, { withFileTypes: true }); } catch { return found; }
  for (const e of entries) {
    const p = path.join(dir, e.name);
    if (e.isDirectory() && recursive) found.push(...sidecarsIn(p, true));
    else if (e.isFile() && (e.name.endsWith(".br") || e.name.includes(".br.tmp-"))) found.push(p);
  }
  return found;
}

const sources = managedSources();
// Whole milliseconds: utimes does not round-trip sub-millisecond precision.
const sameTime = (a, b) => Math.trunc(a.mtimeMs) === Math.trunc(b.mtimeMs);
const isFresh = (src, side) => {
  try {
    return sameTime(fs.statSync(side), fs.statSync(src));
  } catch {
    return false;
  }
};

// 1. Remove every sidecar that is orphaned, unmanaged, stale or half-written.
let removed = 0;
const existing = [
  ...sidecarsIn(PUBLIC, false),
  ...sidecarsIn(AC, false),
  ...sidecarsIn(path.join(AC, "lib"), true),
];
for (const side of existing) {
  const src = side.endsWith(".br") ? side.slice(0, -3) : null;
  if (src && sources.has(src) && isFresh(src, side)) continue;
  if (!DRY) fs.rmSync(side, { force: true });
  removed++;
}

// 2. Write the missing ones.
let written = 0;
let inBytes = 0;
let outBytes = 0;
for (const src of sources) {
  const side = src + ".br";
  if (fs.existsSync(side)) continue;
  const stat = fs.statSync(src);
  const body = fs.readFileSync(src);
  const br = brotliCompressSync(body, {
    params: {
      [constants.BROTLI_PARAM_QUALITY]: 11,
      [constants.BROTLI_PARAM_MODE]: constants.BROTLI_MODE_TEXT,
      [constants.BROTLI_PARAM_SIZE_HINT]: body.length,
    },
  });
  inBytes += body.length;
  outBytes += br.length;
  written++;
  if (DRY) continue;
  // The source may have moved while we compressed; then leave no sidecar.
  if (!sameTime(fs.statSync(src), stat)) continue;
  const tmp = `${side}.tmp-${process.pid}`;
  fs.writeFileSync(tmp, br);
  fs.utimesSync(tmp, stat.atimeMs / 1000, stat.mtimeMs / 1000);
  fs.renameSync(tmp, side);
}

console.log(
  `precompress-core: ${sources.size} sources, ${written} written` +
    (written ? ` (${(inBytes / 1024).toFixed(0)} KB → ${(outBytes / 1024).toFixed(0)} KB br)` : "") +
    `, ${removed} removed${DRY ? " (dry run)" : ""}`,
);
