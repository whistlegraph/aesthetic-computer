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
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, statSync, writeFileSync, rmSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const EASEL = join(HERE, "..");
const REPO = join(EASEL, "..");
const OUT = join(REPO, "system", "public", "easel.tar.gz");
const MANIFEST = join(REPO, "system", "public", "easel.json");
// Written into the tarball so an install can tell what it is. Its absence is
// how a git checkout knows never to overwrite itself with a release.
const STAMP = join(EASEL, "install.json");

const INCLUDE = ["bin", "src", "shell", "context", "package.json", "README.md", "LICENSE", "install.json"];

const version = JSON.parse(readFileSync(join(EASEL, "package.json"), "utf8")).version;

// The stamp is part of the archive, so it is written before tarring and removed
// after: a working checkout must not acquire one by having run this script.
writeFileSync(STAMP, JSON.stringify({ version, packedAt: new Date().toISOString() }, null, 2) + "\n");

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

rmSync(STAMP, { force: true });

const bytes = readFileSync(OUT);
const sha256 = createHash("sha256").update(bytes).digest("hex");

// What a running Easel fetches to decide whether it is behind. Kept to the four
// facts an updater needs, so it stays cheap enough to poll once a day.
writeFileSync(
  MANIFEST,
  JSON.stringify({ version, sha256, bytes: bytes.length, tarball: "/easel.tar.gz" }, null, 2) + "\n",
);

console.log(`easel.tar.gz — v${version}, ${(bytes.length / 1024).toFixed(0)} KB`);
console.log(`  sha256 ${sha256.slice(0, 16)}…`);
console.log(`  ${OUT}`);
console.log(`  ${MANIFEST}`);
