#!/usr/bin/env node
// publish.mjs — stage the public feed and (optionally) sync it to the CDN.
//
// Copies only the public artifacts — episode mp3s, per-episode covers, the
// series cover, feed.xml, index.json — into publish/, leaving the say cache
// (out/cache) and intermediate build files behind. Then syncs publish/ to
// s3://assets-aesthetic-computer/podcast (served at
// https://assets.aesthetic.computer/podcast).
//
// DRY BY DEFAULT: prints the sync command. Pass --push to actually upload
// (publishing makes the readings public — a deliberate, per-run choice).
//
// Usage:
//   node bin/feed.mjs && node bin/publish.mjs           # stage + show command
//   node bin/publish.mjs --push                          # actually upload

import { readFileSync, writeFileSync, mkdirSync, rmSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const OUT = resolve(ROOT, "out");
const PUB = resolve(ROOT, "publish");
const PUSH = process.argv.includes("--push");

const BUCKET = "s3://assets-aesthetic-computer/podcast";
const ENDPOINT = "https://sfo3.digitaloceanspaces.com";

if (!existsSync(resolve(OUT, "feed.xml")) || !existsSync(resolve(OUT, "index.json"))) {
  console.error("✗ out/feed.xml or out/index.json missing — run `node bin/feed.mjs` first.");
  process.exit(1);
}

const index = JSON.parse(readFileSync(resolve(OUT, "index.json"), "utf8"));

// Assemble the publish set.
rmSync(PUB, { recursive: true, force: true });
mkdirSync(PUB, { recursive: true });

const want = new Set(["feed.xml", "index.json", "cover.png"]);
for (const ep of index.episodes) {
  want.add(`${ep.slug}.mp3`);
  want.add(`${ep.slug}-cover.png`);
}
let staged = 0, missing = [];
for (const name of want) {
  const src = resolve(OUT, name);
  if (existsSync(src)) { writeFileSync(resolve(PUB, name), readFileSync(src)); staged++; }
  else missing.push(name);
}

console.log(`✓ staged ${staged} file${staged === 1 ? "" : "s"} → publish/`);
for (const ep of index.episodes) console.log(`   · ${ep.slug}.mp3 + cover`);
if (missing.length) console.log(`  ⚠ missing (skipped): ${missing.join(", ")}`);

const syncArgs = [
  "s3", "sync", PUB, BUCKET,
  "--endpoint-url", ENDPOINT,
  "--exclude", "*.DS_Store",
  "--acl", "public-read",
  "--content-type", "", // let aws guess per-extension; placeholder removed below
];

// aws guesses content-types by extension; drop the placeholder pair.
syncArgs.splice(syncArgs.indexOf("--content-type"), 2);

if (!PUSH) {
  console.log(`\nDRY RUN — nothing uploaded. To publish (makes the readings public):`);
  console.log(`  aws ${syncArgs.join(" ")}`);
  console.log(`\nAfter upload the feed is subscribable at:`);
  console.log(`  ${index.feed}`);
  process.exit(0);
}

console.log(`\n→ uploading publish/ → ${BUCKET} …`);
const r = spawnSync("aws", syncArgs, { stdio: "inherit" });
if (r.status !== 0) { console.error(`✗ aws s3 sync failed (${r.status})`); process.exit(1); }
console.log(`\n✓ published. Subscribe at: ${index.feed}`);
