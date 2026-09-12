#!/usr/bin/env node
// pack — build the tarball easel.sh downloads.
//
// What travels: the source, the bin, the shell integration, the context bundle,
// the licence, the README, and package.json. What does not: tests, the git
// directory, and anything generated. An install is a thing you run, not a
// checkout you develop in — and every file here is a file someone has to be
// allowed to have, so the list is explicit rather than an exclusion glob that
// quietly grows.
//
//   node easel/bin/pack.mjs    → system/public/easel.tar.gz
import { execFileSync } from "node:child_process";
import { mkdirSync, readFileSync, statSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const EASEL = join(HERE, "..");
const REPO = join(EASEL, "..");
const OUT = join(REPO, "system", "public", "easel.tar.gz");

const INCLUDE = ["bin", "src", "shell", "context", "package.json", "README.md", "LICENSE"];

const version = JSON.parse(readFileSync(join(EASEL, "package.json"), "utf8")).version;

for (const entry of INCLUDE) {
  try {
    statSync(join(EASEL, entry));
  } catch {
    console.error(`missing: ${entry}`);
    process.exit(1);
  }
}

mkdirSync(dirname(OUT), { recursive: true });
// --no-mac-metadata and a fixed mtime keep the tarball byte-stable across
// machines, so a rebuild that changed nothing does not look like a new release.
execFileSync("tar", [
  "--no-mac-metadata",
  "--no-xattrs",
  "-czf", OUT,
  "-C", EASEL,
  ...INCLUDE,
], { stdio: "inherit" });

const size = statSync(OUT).size;
console.log(`easel.tar.gz — v${version}, ${(size / 1024).toFixed(0)} KB`);
console.log(`  ${OUT}`);
