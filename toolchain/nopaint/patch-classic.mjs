#!/usr/bin/env node
// patch-classic.mjs, 2026.11.18
// The served No Paint classic build is the 2021 Construct export plus the
// patches listed here, and nothing else. This script proves that and keeps it
// true.
//
//   node toolchain/nopaint/patch-classic.mjs            apply every patch, update the manifest
//   node toolchain/nopaint/patch-classic.mjs --check    change nothing; exit 1 if the build has drifted
//   node toolchain/nopaint/patch-classic.mjs --revert   put the pristine export back
//
// Each patch names its file and can be applied and taken away. Taking every
// patch away must give the bytes whose SHA-256 the manifest
// (classic-export.json) records for the 2021 export; that is the check. A
// patched file also gets a "served" hash in the manifest, which
// lith/nopaint-routing.test.mjs compares with what Caddy serves.
//
// To add a patch: add an entry to PATCHES, run this script, run the brush
// test (tests/browser/brushbook.e2e.mjs), and make the same change in
// the Construct source (nopaint/construct/) so a later editor export keeps it.
// index.html is kept by hand and is not part of the manifest.

import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

const BUILD = new URL("../../system/public/nopaint.art/", import.meta.url);
const MANIFEST = new URL("./classic-export.json", import.meta.url);
const sha = (text) => createHash("sha256").update(text).digest("hex");
const count = (text, part) => text.split(part).length - 1;

// Swap one literal for another. The old text must occur exactly once in the
// pristine file and the new text not at all, so the swap can be undone.
function replace(file, name, from, to) {
  return {
    file, name,
    apply(text) {
      if (count(text, from) !== 1 || count(text, to) !== 0) throw new Error(`${name}: expected one ${JSON.stringify(from)} and no ${JSON.stringify(to)}`);
      return text.replace(from, () => to);
    },
    revert(text) {
      if (count(text, to) === 0 && count(text, from) === 1) return text; // not applied
      if (count(text, to) !== 1 || count(text, from) !== 0) throw new Error(`${name}: cannot tell whether it is applied`);
      return text.replace(to, () => from);
    },
  };
}

// Add a source file to the end, between two marker lines.
function append(file, name, source) {
  const begin = `\n/* nopaint-patch:${name}:begin */\n`, end = `\n/* nopaint-patch:${name}:end */\n`;
  return {
    file, name,
    apply(text) {
      if (text.includes(begin)) throw new Error(`${name}: already applied`);
      return text + begin + readFileSync(source, "utf8").trimEnd() + end;
    },
    revert(text) {
      const at = text.indexOf(begin);
      if (at === -1) return text;
      const stop = text.indexOf(end, at);
      if (stop === -1 || stop + end.length !== text.length) throw new Error(`${name}: its block is not the last thing in the file`);
      return text.slice(0, at);
    },
  };
}

// Ordered. Appends go after replacements on the same file.
export const PATCHES = [
  // 2026-11-04 (762637e3e1): the splash link follows the studio's move.
  replace("scripts/c3runtime.js", "whistlegraph-org", '"https://whistlegraph.com"', '"https://whistlegraph.org"'),
  // 2026-11-18: page ↔ game messages for the brush test and step storage.
  append("scripts/c3runtime.js", "bridge", new URL("./classic-bridge.js", import.meta.url)),
];

export function build({ write = false, revert = false } = {}) {
  const manifest = JSON.parse(readFileSync(MANIFEST, "utf8"));
  const report = [];
  for (const file of [...new Set(PATCHES.map((patch) => patch.file))]) {
    const entry = manifest.files.find((item) => item.path === file);
    if (!entry) throw new Error(`${file} is not in the manifest`);
    const path = new URL(file.split("/").map(encodeURIComponent).join("/"), BUILD);
    const current = readFileSync(path, "utf8");
    const patches = PATCHES.filter((patch) => patch.file === file);
    const pristine = patches.toReversed().reduce((text, patch) => patch.revert(text), current);
    if (sha(pristine) !== entry.sha256) throw new Error(`${file}: with every patch taken away it is not the 2021 export. Something outside PATCHES changed it.`);
    const served = revert ? pristine : patches.reduce((text, patch) => patch.apply(text), pristine);
    const row = { file, patches: revert ? [] : patches.map((patch) => patch.name), sha256: sha(served),
      current: served === current, manifest: (entry.served || entry.sha256) === sha(served) };
    report.push(row);
    if (!write) continue;
    if (!row.current) writeFileSync(path, served);
    if (revert) delete entry.served; else entry.served = row.sha256;
  }
  if (write) {
    for (const entry of manifest.files) if (entry.served && !PATCHES.some((patch) => patch.file === entry.path)) delete entry.served;
    const next = JSON.stringify(manifest, null, 2) + "\n";
    if (next !== readFileSync(MANIFEST, "utf8")) writeFileSync(MANIFEST, next);
  }
  return report;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const check = process.argv.includes("--check"), revert = process.argv.includes("--revert");
  const report = build({ write: !check, revert });
  for (const row of report) {
    const drift = check && !(row.current && row.manifest);
    console.log(`${drift ? "DRIFT" : "ok   "} ${row.file}  ${row.sha256.slice(0, 12)}  ${row.patches.join(", ") || "pristine"}` +
      (check && !row.current ? "  (file differs from pristine + patches)" : "") + (check && !row.manifest ? "  (manifest hash is stale)" : ""));
  }
  if (check && report.some((row) => !(row.current && row.manifest))) process.exit(1);
}
