// Boot preloads — what the shell tells the browser to fetch before boot.mjs
// has even run.
//
// The modulepreload list used to be typed by hand and had drifted: it named
// lib/disk.mjs (169 KB gzipped, only ever imported dynamically by the
// no-worker fallback) and missed the ~30 modules bios imports two to five
// levels down, so a cold boot paid four sequential round trips for them.
// This walks the static import graph from the entry modules instead, once
// per process (a deploy restarts lith), so the list is exactly the graph.
//
// It also names the current worker bundle so the shell can start that
// download at parse time rather than after bios has evaluated and fetched
// the manifest itself.

import { promises as fs } from "fs";
import path from "path";
import { fileURLToPath } from "url";

const ROOT = fileURLToPath(new URL("../public/aesthetic.computer/", import.meta.url));
const ENTRIES = ["boot.mjs", "bios.mjs", "lib/parse.mjs"];
// Dynamically imported by boot.mjs for the dev WebSocket loader only.
const SKIP = new Set(["module-loader.mjs"]);
const IMPORT_RE = /^\s*import\s+(?:[^;'"]|\n)*?from\s*['"]([^'"]+)['"]/gm;
const SIDE_EFFECT_IMPORT_RE = /^\s*import\s*['"]([^'"]+)['"]/gm;

let preloadPromise = null;

async function walk() {
  const seen = new Set();
  const order = [];
  const queue = [...ENTRIES];
  while (queue.length) {
    const rel = queue.shift();
    if (seen.has(rel) || SKIP.has(rel)) continue;
    seen.add(rel);
    let text;
    try {
      text = await fs.readFile(path.join(ROOT, rel), "utf8");
    } catch {
      continue; // named but absent: not preloadable, not fatal
    }
    order.push(rel);
    const specs = [];
    for (const re of [IMPORT_RE, SIDE_EFFECT_IMPORT_RE]) {
      re.lastIndex = 0;
      let m;
      while ((m = re.exec(text))) specs.push(m[1]);
    }
    for (const spec of specs) {
      if (!spec.startsWith(".")) continue; // bare or absolute: not ours
      const next = path.posix.normalize(path.posix.join(path.posix.dirname(rel), spec));
      if (next.startsWith("..")) continue;
      if (!seen.has(next)) queue.push(next);
    }
  }
  // The entries themselves are script tags or imported by name already;
  // preloading them is harmless but bios first keeps the biggest file at
  // the head of the queue.
  return order;
}

export function bootPreloads() {
  if (!preloadPromise) preloadPromise = walk().catch(() => []);
  return preloadPromise;
}

let manifestCache = { at: 0, filename: null };
export async function workerBundleFilename() {
  const now = Date.now();
  if (now - manifestCache.at < 10_000) return manifestCache.filename;
  let filename = null;
  try {
    const raw = await fs.readFile(path.join(ROOT, "lib/disk-worker-manifest.json"), "utf8");
    const name = JSON.parse(raw)?.filename;
    if (/^disk\.worker\.[a-f0-9]{12}\.mjs$/.test(name || "")) filename = name;
  } catch {}
  manifestCache = { at: now, filename };
  return filename;
}
