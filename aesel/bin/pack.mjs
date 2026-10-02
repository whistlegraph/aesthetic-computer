#!/usr/bin/env node
// pack — build the tarball aesel.sh (and easel.sh) downloads.
//
// What travels: the source, the bin, the shell integration, the context bundle,
// the licence, the README, and package.json. What does not: tests, the git
// directory, and anything generated. An install is a thing you run, not a
// checkout you develop in — and every file here is a file someone has to be
// allowed to have, so the list is explicit rather than an exclusion glob that
// quietly grows.
//
//   node aesel/bin/pack.mjs    → system/public/aesel.tar.gz (+ easel.tar.gz)
//
// Every copy up to 0.8.3 polls easel.json and fetches easel.tar.gz, so the
// release goes out under both names, byte for byte, and bin/easel stays in it:
// that is the file an older updater checks for before it will install.
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { copyFileSync, mkdirSync, readFileSync, statSync, writeFileSync, rmSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import {buildTui} from './build-tui.mjs';

const HERE = dirname(fileURLToPath(import.meta.url));
const aesel = join(HERE, "..");
const REPO = join(aesel, "..");
const PUBLIC = join(REPO, "system", "public");
const OUT = join(PUBLIC, "aesel.tar.gz");
const NAMES = ["aesel", "easel"];
// Written into the tarball so an install can tell what it is. Its absence is
// how a git checkout knows never to overwrite itself with a release.
const STAMP = join(aesel, "install.json");

const INCLUDE = ["bin", "src", "shell", "context", "media", "layouts", "package.json", "README.md", "LICENSE", "install.json"];

const version = JSON.parse(readFileSync(join(aesel, "package.json"), "utf8")).version;
await buildTui();

// The stamp is part of the archive, so it is written before tarring and removed
// after: a working checkout must not acquire one by having run this script.
writeFileSync(STAMP, JSON.stringify({ version, packedAt: new Date().toISOString() }, null, 2) + "\n");

for (const entry of INCLUDE) {
  try {
    statSync(join(aesel, entry));
  } catch {
    console.error(`missing: ${entry}`);
    process.exit(1);
  }
}

mkdirSync(dirname(OUT), { recursive: true });
// BSD tar (macOS) understands --no-mac-metadata; GNU tar (the box this actually
// packs on) does not and refuses the whole command. Probe once rather than
// branching on platform, since the tar in PATH is the thing that matters and it
// is not always the one the platform implies.
let extraFlags = [];
try {
  execFileSync("tar", ["--no-mac-metadata", "--version"], { stdio: "ignore" });
  extraFlags = ["--no-mac-metadata", "--no-xattrs"];
} catch {}

execFileSync("tar", [
  ...extraFlags,
  "-czf", OUT,
  "-C", aesel,
  ...INCLUDE,
], { stdio: "inherit" });

rmSync(STAMP, { force: true });

const bytes = readFileSync(OUT);
const sha256 = createHash("sha256").update(bytes).digest("hex");

// What a running aesel fetches to decide whether it is behind. Kept to the four
// facts an updater needs, so it stays cheap enough to poll once a day.
for (const name of NAMES) {
  if (name !== "aesel") copyFileSync(OUT, join(PUBLIC, `${name}.tar.gz`));
  writeFileSync(
    join(PUBLIC, `${name}.json`),
    JSON.stringify({ version, sha256, bytes: bytes.length, tarball: `/${name}.tar.gz` }, null, 2) + "\n",
  );
}

console.log(`aesel.tar.gz — v${version}, ${(bytes.length / 1024).toFixed(0)} KB`);
console.log(`  sha256 ${sha256.slice(0, 16)}…`);
for (const name of NAMES) console.log(`  ${join(PUBLIC, name)}.{tar.gz,json}`);
