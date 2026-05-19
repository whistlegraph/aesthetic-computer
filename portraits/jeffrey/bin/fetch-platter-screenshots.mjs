#!/usr/bin/env node
// fetch-platter-screenshots.mjs — pull the 90 jeffrey-platter "screenshots"
// bucket entries (first-person POV captures of jeffrey's working environment)
// from the assets CDN into a local cache so image-gen pipelines can pass them
// via --refs.
//
// Source manifest:  papers/jeffrey-platter/manifest.json (buckets.screenshots)
// Source CDN:       https://assets.aesthetic.computer/screenshots/images/{name}
// Cache dest:       portraits/jeffrey/corpus/screenshots/
//
// Usage:
//   node portraits/jeffrey/bin/fetch-platter-screenshots.mjs           # missing only
//   node portraits/jeffrey/bin/fetch-platter-screenshots.mjs --force   # re-download all

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { dirname, resolve, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..");
const MANIFEST = join(REPO, "papers", "jeffrey-platter", "manifest.json");
const CACHE = join(REPO, "portraits", "jeffrey", "corpus", "screenshots");

const force = process.argv.includes("--force");

const m = JSON.parse(readFileSync(MANIFEST, "utf8"));
const bucket = m.buckets.screenshots;
const names = Object.keys(bucket.items);

mkdirSync(CACHE, { recursive: true });

const CONCURRENCY = 4; // 8GB RAM machine, gentle on CDN
let ok = 0;
let skip = 0;
let fail = 0;

const queue = [...names];

async function fetchOne(name) {
  const dest = join(CACHE, name);
  if (!force && existsSync(dest)) {
    skip++;
    return;
  }
  const url = bucket.url_pattern.replace("{name}", name);
  try {
    const res = await fetch(url);
    if (!res.ok) {
      console.error(`  FAIL ${name}: HTTP ${res.status}`);
      fail++;
      return;
    }
    const buf = Buffer.from(await res.arrayBuffer());
    writeFileSync(dest, buf);
    ok++;
    process.stderr.write(".");
  } catch (e) {
    console.error(`\n  FAIL ${name}: ${e.message}`);
    fail++;
  }
}

async function worker() {
  while (queue.length) {
    const name = queue.shift();
    await fetchOne(name);
  }
}

console.error(`fetching ${names.length} screenshots → ${CACHE}`);
console.error(`(force=${force}, concurrency=${CONCURRENCY})`);

await Promise.all(Array.from({ length: CONCURRENCY }, worker));

console.error(`\ndone: ${ok} downloaded, ${skip} cached, ${fail} failed`);
