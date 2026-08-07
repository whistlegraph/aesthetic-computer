#!/usr/bin/env node

import { existsSync, mkdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const OUT = resolve(ROOT, "out");
const PUB = resolve(ROOT, "publish");
const push = process.argv.includes("--push");

for (const required of ["index.json", "feed.xml", "cover-1400.png"]) {
  if (!existsSync(resolve(OUT, required))) throw new Error(`missing out/${required}; run bin/feed.mjs first`);
}

const index = JSON.parse(readFileSync(resolve(OUT, "index.json"), "utf8"));
rmSync(PUB, { recursive: true, force: true });
mkdirSync(PUB, { recursive: true });
const files = new Set(["index.json", "feed.xml", "cover-1400.png"]);
for (const item of index.episodes) {
  files.add(`${item.slug}.mp3`);
  files.add(`${item.slug}-cover-1400.png`);
}
for (const file of files) {
  const source = resolve(OUT, file);
  if (!existsSync(source)) throw new Error(`public catalog references missing ${source}`);
  writeFileSync(resolve(PUB, file), readFileSync(source));
}

const args = [
  "s3", "sync", PUB, "s3://assets-aesthetic-computer/klokkentales",
  "--endpoint-url", "https://sfo3.digitaloceanspaces.com",
  "--acl", "public-read", "--exclude", "*.DS_Store",
];
console.log(`staged ${files.size} files in ${PUB}`);
if (!push) {
  console.log(`dry run; publish with:\naws ${args.join(" ")}`);
  process.exit(0);
}
const result = spawnSync("aws", args, { stdio: "inherit" });
if (result.status !== 0) process.exit(result.status || 1);
console.log("published https://assets.aesthetic.computer/klokkentales/index.json");

