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
// A row may also be a function of the project, (project) => row, for a row
// made from others (the AI brush below is made from Load's).
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
        const row = typeof op.row === "function" ? op.row(project) : op.row;
        const found = list.flatMap((item, index) => same(item, row) ? [index] : []);
        // Putting a row in: forward for insert, backward for remove.
        if (Boolean(op.insert) === forward) {
          if (found.length) { if (forward) throw new Error("the row is already there"); continue; }
          list.splice(op.index ?? list.length, 0, structuredClone(row));
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

// ---- the AI brush -----------------------------------------------------------
// An AI turn is a brush of its own: layout "AI" with event sheet "AI", as Wafer
// or Mirror is a layout with its sheet. It is made from Load's machinery (Load
// fetches a picture by a code and offers it), so both rows are built here from
// Load's, found by name and SID. Its picture is the page's: the bridge
// (classic-bridge.js) names a turn when the layout starts, and answers the
// fetch for it.
//
//   Params         the turn's code ("ai3"); the picture is painting/<code>.png
//   parameterHint  " " + the note ("add weather"): the label reads "AI: add weather"
//
// The sheet differs from Load's in this: it always fetches (no code, no
// file picker), it prints the note rather than the code, its picture holds
// at full strength (Load's flickers between full and half, and Paint keeps
// whichever it is at), a failed fetch leaves the error to the bridge ("AI
// Failed", not "Load Failed"), its function is AIInitialize, and every SID is
// its own.
// The group has no "is group active" condition: the export spells that
// condition with an expression of the group's name, and the 2021 export has no
// expression for "AI". Nothing turns the group off, so it changes nothing.
// The same brush is in the Construct source: nopaint/construct/layouts/Brushes/AI.json
// and eventSheets/Brushes/AI.json, with the SIDs and UIDs made here.

// A new SID, from a key: an old SID of Load's, or a name.
export const fresh = (key) => 1e14 + (createHash("sha256").update(`nopaint-ai-brush:${key}`).digest().readUIntBE(0, 6) % 9e14);
const LOAD_SHEET = {
  start: 688133908767763, // On start of layout: interpret the code, fetch, print
  emptyAudio: 380243743784753, endOfLayout: 157318655377097, // blocks with no live actions
  flickerOn: 775239693119383, flickerOff: 205668413617385, // the flicker's opacity, full and half
  successLog: 464973692496639, failureLog: 138728831301839,
  loadFailed: 820911502599547, // CurrentError = "Load Failed"; the bridge says "AI Failed" instead
  keep: [538563104555363, 619254325671763, 142012069639449, 728811519425736, 941126452946853, 987190512269628, 421299895042979],
  //     Loaded false, Initialized false, StartSpinner, WiFi shown, WiFi animates, LoadSprite.LoadURL, PrintParameterHint
};
const SIZE_PARAMS = 347358172583688; // Core, ProcessPromptText: ParamsArray.SetSize(tokencount(Params, " "), 1, 1)
// Every SID a sheet's rows define: variables, blocks, conditions, actions.
function defined(list) {
  const sids = new Set();
  for (const node of nodes(list)) {
    if (node[0] === 1) sids.add(node[6]);
    if (![0, 3, 4].includes(node[0])) continue;
    sids.add(node[4]);
    for (const condition of node[6] || []) if (condition[7]) sids.add(condition[7]);
    for (const action of node[7] || []) if (action[3]) sids.add(action[3]);
  }
  return sids;
}
// Every number in the project that could be a SID, other than the AI brush's own.
function allSids(project) {
  const found = new Set();
  JSON.stringify(project.map((slot, i) => (i === 5 || i === 6) ? slot.filter((row) => row[0] !== "AI") : slot),
    (key, value) => { if (Number.isInteger(value) && value >= 1e13) found.add(value); return value; });
  return found;
}
export function aiSheet(project) {
  const load = at.sheet("Load")(project)[1], own = defined(load), taken = allSids(project);
  const remap = (row) => JSON.parse(JSON.stringify(row), (key, value) => {
    if (typeof value !== "number" || !own.has(value)) return value;
    const sid = fresh(value);
    if (taken.has(sid)) throw new Error(`the AI brush's SID for ${value} is taken`);
    return sid;
  });
  const group = remap(one(load.filter((node) => node[0] === 3 && node[1][1] === "Load"), "the Load group"));
  group[1] = [true, "AI"];
  group[6] = [];
  const all = () => [...nodes(group[8])];
  const block = (sid) => one(all().filter((node) => [0, 3, 4].includes(node[0]) && node[4] === fresh(sid)), `Load block ${sid}`);
  const actions = () => all().flatMap((node) => [0, 3, 4].includes(node[0]) ? node[7] || [] : []);
  const action = (sid) => one(actions().filter((row) => row[3] === fresh(sid)), `Load action ${sid}`);
  const drop = (...sids) => { for (const node of all()) if ([0, 3, 4].includes(node[0]) && node[7]) node[7] = node[7].filter((row) => !sids.map(fresh).includes(row[3])); };
  // Start: fetch the turn's picture and print its note.
  const start = block(LOAD_SHEET.start), sizeParams = structuredClone(at.action("Core", SIZE_PARAMS)(project));
  sizeParams[3] = fresh(`core:${SIZE_PARAMS}`);
  const keep = LOAD_SHEET.keep.map(action);
  start[7] = [...keep.slice(0, 2), sizeParams, ...keep.slice(2)];
  start.length = 8; // no sub-events
  // Blocks with nothing left to do, the flicker's opacity, the console lines.
  group[8] = group[8].filter((node) => ![LOAD_SHEET.emptyAudio, LOAD_SHEET.endOfLayout].map(fresh).includes(node[4]));
  drop(LOAD_SHEET.flickerOn, LOAD_SHEET.flickerOff, LOAD_SHEET.successLog, LOAD_SHEET.failureLog, LOAD_SHEET.loadFailed);
  // Its own function name: function names are the project's, not the sheet's.
  const initialize = one(all().filter((node) => node[0] === 4 && node[1][0] === "LoadInitialize"), "LoadInitialize");
  initialize[1] = ["AIInitialize", ...initialize[1].slice(1)];
  for (const row of actions()) if (row[0] === -2 && row[1] === "LoadInitialize") row[1] = "AIInitialize";
  // Number the events as the export does: the group 1, then each in order.
  let n = 0;
  group[5] = ++n;
  for (const node of all()) if ([0, 3, 4].includes(node[0])) node[5] = ++n;
  return ["AI", [structuredClone(one(load.filter((node) => node[0] === 2), "Load's include")), group]];
}
export function aiLayout(project) {
  const row = structuredClone(at.layout("Load")(project));
  const taken = allSids(project);
  // UIDs after the highest in the project; the AI brush's own come first in its layer order.
  let uid = Math.max(...project[5].filter((layout) => layout[0] !== "AI").flatMap((layout) => layout[6].flatMap((layer) => layer[14].map((instance) => instance[2]))));
  row[0] = "AI"; row[4] = "AI"; row[5] = fresh(row[5]);
  for (const layer of row[6]) {
    layer[2] = fresh(layer[2]);
    // The UI, Keyboard and Emoji layers are the project's global layers: the same UIDs on every brush.
    if (layer[0] === "Layer 0") for (const instance of layer[14]) instance[2] = ++uid;
  }
  for (const sid of [row[5], ...row[6].map((layer) => layer[2])]) if (taken.has(sid)) throw new Error(`the AI layout's SID ${sid} is taken`);
  return row;
}

// Ordered. Appends go after replacements on the same file.
export const PATCHES = [
  // 2026-11-04 (762637e3e1): the splash link follows the studio's move.
  replace("scripts/c3runtime.js", "whistlegraph-org", '"https://whistlegraph.com"', '"https://whistlegraph.org"'),
  // 2026-11-18: the title screen's Community button goes to laklok.com, where
  // the conversation is now, and no longer to the 2021 Discord.
  replace("scripts/c3runtime.js", "community-laklok", '"https://discord.gg/4BdxKC5gd9"', '"https://laklok.com"'),
  // 2026-11-18: the odds of each proposal, as parameters. The picker is the
  // rule; the tuning is the numbers (classic-tuning.json; print them with
  // classic-odds.mjs). To tune: edit the JSON, run this script, test, deploy.
  append("scripts/c3runtime.js", "picker", new URL("./classic-picker.js", import.meta.url)),
  append("scripts/c3runtime.js", "tuning", () => `self.nopaintTuning = ${JSON.stringify(JSON.parse(readFileSync(new URL("./classic-tuning.json", import.meta.url), "utf8")))};`),
  // 2026-11-18: page ↔ game messages for the brush test and step storage,
  // and the hook that makes the game pick by the tuning.
  append("scripts/c3runtime.js", "bridge", new URL("./classic-bridge.js", import.meta.url)),
  // 2026-11-25: the AI brush, a layout and an event sheet of its own (above).
  // An AI turn no longer borrows Load, and its label reads "AI: <note>".
  json("data.json", "ai-brush", [
    { insert: at.project(), path: [5], row: aiLayout },
    { insert: at.project(), path: [6], row: aiSheet },
  ]),
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
