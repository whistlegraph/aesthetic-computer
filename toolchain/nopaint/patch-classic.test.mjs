// The data.json patch type, run against the real build in memory. Nothing is
// written: each case applies operations to the served data.json's text and
// takes them away again.
//
//   node --test toolchain/nopaint/patch-classic.test.mjs

import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { json, at, PATCHES, build } from "./patch-classic.mjs";

const raw = readFileSync(new URL("../../system/public/nopaint.art/data.json", import.meta.url), "utf8");
const project = () => JSON.parse(raw).project;

test("the served build is the 2021 export plus the patch list", () => {
  for (const row of build()) assert.ok(row.current && row.manifest, `${row.file} has drifted`);
  assert.ok(PATCHES.length >= 2);
});

test("set finds a text by layout and UID and a variable by SID, and both come back", () => {
  const patch = json("data.json", "worked-example", [
    { set: at.instance("Splash", 398), path: [5, 0], from: "38 Brushes & ? Stamps", to: "39 Brushes & ? Stamps" },
    { set: at.variable("Core", 635115374915280), path: [3], from: 0.2, to: 0.3 },
  ]);
  const patched = patch.apply(raw);
  assert.notEqual(patched, raw);
  assert.equal(patched.split("39 Brushes & ? Stamps").length - 1, 1);
  assert.equal(at.variable("Core", 635115374915280)(JSON.parse(patched).project)[1], "IndexHangTime");
  assert.equal(at.variable("Core", 635115374915280)(JSON.parse(patched).project)[3], 0.3);
  assert.equal(patch.revert(patched), raw);
  assert.equal(patch.revert(raw), raw, "taking away a patch that is not applied changes nothing");
  assert.throws(() => patch.apply(patched), /already applied/);
});

test("set refuses a value that is not what the patch says is there", () => {
  const patch = json("data.json", "wrong", [{ set: at.variable("Core", 635115374915280), path: [3], from: 0.25, to: 0.3 }]);
  assert.throws(() => patch.apply(raw), /wrong, operation 1: expected 0.25, found 0.2/);
});

test("insert and remove a row, by position in a located list, and both come back", () => {
  const core = at.sheet("Core")(project())[1];
  const first = core[0];
  const added = [1, "BrushTestProbe", 0, 7, false, false, 111122223333444, false];
  const patch = json("data.json", "rows", [
    { insert: at.sheet("Core"), path: [1], row: added, index: 0 },
    { remove: at.sheet("Core"), path: [1], row: first },
  ]);
  const patched = patch.apply(raw);
  const after = at.sheet("Core")(JSON.parse(patched).project)[1];
  assert.deepEqual(after[0], added);
  assert.equal(after.length, core.length);
  assert.ok(!after.some((row) => JSON.stringify(row) === JSON.stringify(first)));
  // Taken away, the removed row goes to the end, not where it was: say so.
  const back = at.sheet("Core")(JSON.parse(patch.revert(patched)).project)[1];
  assert.deepEqual(back.at(-1), first);
  const exact = json("data.json", "rows-exact", [
    { insert: at.sheet("Core"), path: [1], row: added, index: 0 },
    { remove: at.sheet("Core"), path: [1], row: first, index: 1 },
  ]);
  assert.equal(exact.revert(exact.apply(raw)), raw, "with the index given, remove puts the row back in place");
  assert.throws(() => exact.apply(exact.apply(raw)), /already there/);
});

test("locators find what the source map says, and refuse what is not there or not single", () => {
  const p = project();
  assert.equal(at.objectType("Painting")(p)[11], 853010152845463);
  assert.equal(at.layout("Flip")(p)[4], "Flip");
  assert.equal(at.layer("Flip", "UI")(p)[14].length, 45);
  assert.equal(at.function("Core", "UploadPainting")(p)[4], 784578080760308);
  assert.equal(at.block("Core", 784578080760308)(p)[1][0], "UploadPainting");
  assert.equal(at.action("Core", 653194526514032)(p)[3], 653194526514032);
  assert.equal(at.project()(p)[0], "No Paint");
  assert.throws(() => at.layout("Nowhere")(p), /layout Nowhere: expected one, found 0/);
  assert.throws(() => at.variable("Core", 1)(p), /expected one, found 0/);
});

test("the AI brush is Load's layout and sheet under its own name and SIDs, and comes away cleanly", () => {
  const p = project(), patch = PATCHES.find((item) => item.name === "ai-brush");
  const layout = at.layout("AI")(p), sheet = at.sheet("AI")(p);
  assert.equal(layout[4], "AI");
  assert.deepEqual(layout[6].map((layer) => layer[0]), at.layout("Load")(p)[6].map((layer) => layer[0]));
  // Every SID the AI sheet defines is its own.
  const sids = (list, out = []) => { for (const node of list) {
    if (node[0] === 1) out.push(node[6]);
    if ([0, 3, 4].includes(node[0])) { out.push(node[4], ...(node[6] || []).map((c) => c[7]).filter(Boolean), ...(node[7] || []).map((a) => a[3]).filter(Boolean)); if (node[8]) sids(node[8], out); }
  } return out; };
  const ours = sids(sheet[1]), others = new Set(p[6].filter((row) => row[0] !== "AI").flatMap((row) => sids(row[1])));
  assert.equal(new Set(ours).size, ours.length);
  assert.ok(ours.every((sid) => !others.has(sid)));
  const text = JSON.stringify(sheet);
  assert.ok(!text.includes("LoadInitialize") && text.includes('"AIInitialize"') && text.includes('"PrintParameterHint"'));
  assert.ok(!text.includes('"InitParameters"'), "the AI brush keeps its hint: it never resets the parameters");
  // Taken away, the export is back; put back, the served file is back.
  const pristine = patch.revert(raw);
  assert.ok(!JSON.parse(pristine).project[5].some((row) => row[0] === "AI"));
  assert.equal(patch.apply(pristine), raw);
});

test("the AI brush reads AI: <strength> <model> <note>, any part missing or ?, and generates the rest from the tuning", () => {
  const scope = {};
  new Function("self", readFileSync(new URL("./classic-picker.js", import.meta.url), "utf8"))(scope);
  const tuning = JSON.parse(readFileSync(new URL("./classic-tuning.json", import.meta.url), "utf8")), ai = tuning.ai;
  const read = (line, random) => scope.nopaintPicker.aiParams(line.split(" ").filter(Boolean), ai, random)
    .map((part) => (part.given ? "" : "?") + part.value).join(" | ");
  const first = () => 0; // every roll takes the first entry
  assert.equal(read("75 seed make it night", first), "75 | seed | make it night");
  assert.equal(read("make it night", first), "?25 | ?hy | make it night");
  assert.equal(read("75 make it night", first), "75 | ?hy | make it night");
  assert.equal(read("SEED night", first), "?25 | seed | night");
  assert.equal(read("? ? night", first), "?25 | ?hy | night");
  assert.equal(read("", first), `?25 | ?hy | ?${ai.notes[0]}`);
  assert.equal(read("? ? ?", first), `?25 | ?hy | ?${ai.notes[0]}`);
  // Rolls follow the weights and stay in the tables.
  const counts = {};
  for (let i = 0; i < 4000; i++) for (const part of scope.nopaintPicker.aiParams([], ai)) counts[part.value] = (counts[part.value] || 0) + 1;
  for (const value of Object.keys(counts)) assert.ok(value in ai.strengths || value in ai.models || ai.notes.includes(value), value);
  assert.ok(counts.hy > counts.krea * 1.8, `hy ${counts.hy}, krea ${counts.krea}`);
  // Every label the game can draw fits its one line of 28 letters.
  const longest = (list) => Math.max(...list.map((item) => String(item).length));
  assert.ok(`AI: ${"x".repeat(longest(Object.keys(ai.strengths)))} ${"x".repeat(longest(Object.keys(ai.models)))} ${"x".repeat(longest(ai.notes))}`.length <= 28);
  for (const entry of Object.values(ai.models)) assert.match(entry.model, /^[\w.-]+\/[\w.-]+$/);
});

test("a generated model is rolled only among the codes the free day covers, weights kept; a typed one stays", () => {
  const scope = {};
  new Function("self", readFileSync(new URL("./classic-picker.js", import.meta.url), "utf8"))(scope);
  const ai = JSON.parse(readFileSync(new URL("./classic-tuning.json", import.meta.url), "utf8")).ai, picker = scope.nopaintPicker;
  const counts = {};
  for (let i = 0; i < 6000; i++) { const code = picker.aiParams([], ai, Math.random, ["hy", "seed", "krea"])[1].value; counts[code] = (counts[code] || 0) + 1; }
  assert.deepEqual(Object.keys(counts).sort(), ["hy", "krea", "seed"]);
  // hy 3 : seed 2 : krea 1
  assert.ok(Math.abs(counts.hy / 6000 - 0.5) < 0.04 && Math.abs(counts.seed / 6000 - 1 / 3) < 0.04, JSON.stringify(counts));
  assert.deepEqual(picker.aiParams(["75", "gpt", "night"], ai, Math.random, ["hy"])[1], { value: "gpt", given: true });
  // Nothing covered (or not known yet): the whole table, and the page falls back.
  assert.ok(picker.aiParams([], ai, () => 0.999, [])[1].value in ai.models);
  assert.equal(picker.aiParams([], ai, () => 0, null)[1].value, "hy");
});
