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
// Three kinds of patch: replace (one literal in a text file), append (a source
// file at the end of a text file) and json (operations on data.json by name,
// SID or UID; see `at` below).
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
      // The new text may hold the old one (a suffix added), so count the new first.
      const applied = count(text, to);
      if (applied === 0 && count(text, from) === 1) return text; // not applied
      if (applied !== 1 || (!to.includes(from) && count(text, from) !== 0)) throw new Error(`${name}: cannot tell whether it is applied`);
      return text.replace(to, () => from);
    },
  };
}

// Add a source file to the end, between two marker lines. `source` is a file,
// or a function that returns the text.
function append(file, name, source) {
  const begin = `\n/* nopaint-patch:${name}:begin */\n`, end = `\n/* nopaint-patch:${name}:end */\n`;
  return {
    file, name,
    apply(text) {
      if (text.includes(begin)) throw new Error(`${name}: already applied`);
      return text + begin + (typeof source === "function" ? source() : readFileSync(source, "utf8")).trimEnd() + end;
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

// ---- data.json ---------------------------------------------------------------
// data.json is {"project": [...]}: nested arrays, no keys. Array offsets move
// when the project is exported again; names, SIDs and UIDs do not. So a JSON
// patch never names an offset from the top. It finds a row with one of the
// locators in `at`, then steps to a position inside that row, which the
// export's row shape fixes (docs/nopaint-construct-sourcemap.md, section 2).
//
//   json("data.json", "name", [
//     { set: at.instance("Splash", 398), path: [5, 0], from: "38 Brushes & ? Stamps", to: "39 Brushes & ? Stamps" },
//     { set: at.variable("Core", 635115374915280), path: [3], from: 0.2, to: 0.3 },
//     { insert: at.sheet("Core"), path: [1], row: [...], index: 0 },   // index is optional: the end
//     { remove: at.sheet("Core"), path: [1], row: [...] },             // the row as it is now
//   ])
//
// Every operation states what is there before it runs and fails if it is not,
// so each one can be taken away: `set` puts `from` back, `insert` takes its
// row out, `remove` puts its row back.
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
const one = (list, what) => {
  if (list.length !== 1) throw new Error(`${what}: expected one, found ${list.length}`);
  return list[0];
};
// Every node of an event sheet, at any depth: blocks, groups and function
// blocks keep their children in [8].
function* nodes(list) {
  for (const node of list) {
    yield node;
    if (Array.isArray(node) && [0, 3, 4].includes(node[0]) && Array.isArray(node[8])) yield* nodes(node[8]);
  }
}
const sheetNodes = (project, sheet) => one(project[6].filter((row) => row[0] === sheet), `event sheet ${sheet}`)[1];
const layoutRow = (project, layout) => one(project[5].filter((row) => row[0] === layout), `layout ${layout}`);
export const at = {
  // The whole project array, for the few top-level slots (name, version).
  project: () => (project) => project,
  objectType: (name) => (project) => one(project[3].filter((row) => row[0] === name), `object type ${name}`),
  layout: (name) => (project) => layoutRow(project, name),
  layer: (layout, name) => (project) => one(layoutRow(project, layout)[6].filter((row) => row[0] === name), `layer ${name} of ${layout}`),
  // A placed instance, by the UID the Construct source also carries.
  instance: (layout, uid) => (project) => one(layoutRow(project, layout)[6].flatMap((layer) => layer[14].filter((row) => row[2] === uid)), `instance ${uid} on ${layout}`),
  // An event sheet row: [name, nodes].
  sheet: (name) => (project) => one(project[6].filter((row) => row[0] === name), `event sheet ${name}`),
  // A variable (global or local), by SID.
  variable: (sheet, sid) => (project) => one([...nodes(sheetNodes(project, sheet))].filter((node) => node[0] === 1 && node[6] === sid), `variable ${sid} in ${sheet}`),
  // A block, group or function block, by SID.
  block: (sheet, sid) => (project) => one([...nodes(sheetNodes(project, sheet))].filter((node) => [0, 3, 4].includes(node[0]) && node[4] === sid), `block ${sid} in ${sheet}`),
  // A function block, by name.
  function: (sheet, name) => (project) => one([...nodes(sheetNodes(project, sheet))].filter((node) => node[0] === 4 && node[1]?.[0] === name), `function ${name} in ${sheet}`),
  // One action of any block, by the action's SID.
  action: (sheet, sid) => (project) => one([...nodes(sheetNodes(project, sheet))].flatMap((node) => [0, 3, 4].includes(node[0]) ? (node[7] || []).filter((row) => row[3] === sid) : []), `action ${sid} in ${sheet}`),
  // One condition of any block, by the condition's SID.
  condition: (sheet, sid) => (project) => one([...nodes(sheetNodes(project, sheet))].flatMap((node) => [0, 3, 4].includes(node[0]) ? (node[6] || []).filter((row) => row[7] === sid) : []), `condition ${sid} in ${sheet}`),
};

// Walk `path` from a located row. Returns the container and the last key.
function reach(project, locate, path = []) {
  let target = locate(project);
  for (const key of path.slice(0, -1)) {
    if (!Array.isArray(target) || !(key in target)) throw new Error(`no position ${JSON.stringify(path)} in the row`);
    target = target[key];
  }
  return [target, path.at(-1)];
}
const listAt = (project, locate, path = []) => {
  const list = path.reduce((target, key) => target?.[key], locate(project));
  if (!Array.isArray(list)) throw new Error(`position ${JSON.stringify(path)} is not a list`);
  return list;
};

function runJson(project, ops, name, forward) {
  for (const [n, op] of (forward ? ops : ops.toReversed()).entries()) {
    const label = `${name}, operation ${forward ? n + 1 : ops.length - n}`;
    try {
      if (op.set) {
        if (!op.path?.length) throw new Error("set needs a path inside the row");
        const [target, key] = reach(project, op.set, op.path);
        const [before, after] = forward ? [op.from, op.to] : [op.to, op.from];
        if (same(target[key], after) && !same(before, after)) { if (forward) throw new Error("already applied"); continue; }
        if (!same(target[key], before)) throw new Error(`expected ${JSON.stringify(before)}, found ${JSON.stringify(target[key])?.slice(0, 80)}`);
        target[key] = structuredClone(after);
      } else if (op.insert || op.remove) {
        const list = listAt(project, op.insert || op.remove, op.path);
        const found = list.flatMap((row, index) => same(row, op.row) ? [index] : []);
        // Putting a row in: forward for insert, backward for remove.
        if (Boolean(op.insert) === forward) {
          if (found.length) { if (forward) throw new Error("the row is already there"); continue; }
          list.splice(op.index ?? list.length, 0, structuredClone(op.row));
        } else {
          if (!found.length && !forward) continue; // not applied
          list.splice(one(found, "the row"), 1);
        }
      } else throw new Error("an operation is set, insert or remove");
    } catch (error) { throw new Error(`${label}: ${error.message}`); }
  }
}

// A patch of ordered operations on a JSON file shaped like data.json.
export function json(file, name, ops) {
  const run = (text, forward) => {
    const data = JSON.parse(text);
    // The file must survive a load and dump unchanged, or a patch would
    // rewrite bytes it never meant to touch.
    if (JSON.stringify(data) !== text) throw new Error(`${name}: ${file} does not reproduce byte for byte through JSON`);
    runJson(data.project, ops, name, forward);
    return JSON.stringify(data);
  };
  return { file, name, apply: (text) => run(text, true), revert: (text) => run(text, false) };
}

// Ordered. Appends go after replacements on the same file.
export const PATCHES = [
  // 2026-11-04 (762637e3e1): the splash link follows the studio's move.
  replace("scripts/c3runtime.js", "whistlegraph-org", '"https://whistlegraph.com"', '"https://whistlegraph.org"'),
  // 2026-11-18: the odds of each proposal, as parameters. The picker is the
  // rule; the tuning is the numbers (classic-tuning.json; print them with
  // classic-odds.mjs). To tune: edit the JSON, run this script, test, deploy.
  append("scripts/c3runtime.js", "picker", new URL("./classic-picker.js", import.meta.url)),
  append("scripts/c3runtime.js", "tuning", () => `self.nopaintTuning = ${JSON.stringify(JSON.parse(readFileSync(new URL("./classic-tuning.json", import.meta.url), "utf8")))};`),
  // 2026-11-18: page ↔ game messages for the brush test and step storage,
  // and the hook that makes the game pick by the tuning.
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
