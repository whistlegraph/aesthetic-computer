#!/usr/bin/env node

import { readFileSync, mkdirSync, existsSync, statSync, writeFileSync } from "fs";
import { join } from "path";

const HERE = new URL(".", import.meta.url).pathname;
const ROOT = join(HERE, "..");
const REPO_ROOT = join(ROOT, "..", "..");
const MANIFEST = join(REPO_ROOT, "papers/jeffrey-platter/manifest.json");
const CORPUS = join(ROOT, "corpus");

const args = new Set(process.argv.slice(2));
const flags = ["--shoot", "--masters", "--candids"];
const selected = flags.filter((f) => args.has(f)).map((f) => f.slice(2));
const buckets = selected.length > 0 ? selected : ["shoot", "masters", "candids"];
const force = args.has("--force");

const manifest = JSON.parse(readFileSync(MANIFEST, "utf8"));

let fetched = 0,
  skipped = 0,
  failed = 0;

for (const bucketName of buckets) {
  const bucket = manifest.buckets[bucketName];
  if (!bucket) {
    console.error(`unknown bucket: ${bucketName}`);
    continue;
  }
  const dest = join(CORPUS, bucketName);
  mkdirSync(dest, { recursive: true });

  for (const key of Object.keys(bucket.items)) {
    const filename = bucket.key_includes_extension ? key : `${key}.jpg`;
    const url = bucket.url_pattern.replace("{name}", key);
    const localPath = join(dest, filename);

    if (!force && existsSync(localPath) && statSync(localPath).size > 0) {
      skipped++;
      continue;
    }

    process.stdout.write(`fetching ${bucketName}/${filename} ... `);
    try {
      const res = await fetch(url);
      if (!res.ok) {
        console.log(`HTTP ${res.status}`);
        failed++;
        continue;
      }
      const buf = Buffer.from(await res.arrayBuffer());
      writeFileSync(localPath, buf);
      console.log(`${(buf.length / 1024).toFixed(0)} KB`);
      fetched++;
    } catch (err) {
      console.log(`error: ${err.message}`);
      failed++;
    }
  }
}

const indexPath = join(CORPUS, "index.json");
const index = {
  generated: new Date().toISOString(),
  manifest_version: manifest.version,
  buckets: Object.fromEntries(
    buckets.map((b) => [
      b,
      {
        count: Object.keys(manifest.buckets[b].items).length,
        url_pattern: manifest.buckets[b].url_pattern,
      },
    ]),
  ),
};
writeFileSync(indexPath, JSON.stringify(index, null, 2));

console.log(`\nfetched: ${fetched}, skipped: ${skipped}, failed: ${failed}`);
console.log(`corpus: ${CORPUS.replace(REPO_ROOT + "/", "")}`);
