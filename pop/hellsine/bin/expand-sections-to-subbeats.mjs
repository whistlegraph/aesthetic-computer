#!/usr/bin/env node
// pop/hellsine/bin/expand-sections-to-subbeats.mjs — expand the 6
// engine-level sections (overture/statement/bridge/develop/climax/coda)
// into the 18 sub-beat sections (overture-a/b/c, …, coda-a/b) that the
// 18 panel illustrations + preview-score.mjs + the AC piece expect.
//
// Each engine section gets evenly divided into N sub-beats per the
// hellsine storyline (gen-sections.mjs): overture=3, statement=3,
// bridge=4, develop=3, climax=3, coda=2 → 18 total. Each sub-beat
// inherits its parent's [startSec, endSec] range as { startSec, endSec }
// + name "<parent>-<a|b|c|d>" + letter `code` a..r in order.
//
// Run AFTER hellsine.mjs --strategy ultimate + warp-struct-to-master.mjs
// to keep events + 18-beat structure aligned with the master audio.
//
// Usage:
//   node pop/hellsine/bin/expand-sections-to-subbeats.mjs

import { readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const STRUCT_PATH = resolve(HERE, "../hellsine.struct.json");

const SUB_BEATS = {
  overture: ["a", "b", "c"],
  statement: ["a", "b", "c"],
  bridge: ["a", "b", "c", "d"],
  develop: ["a", "b", "c"],
  climax: ["a", "b", "c"],
  coda: ["a", "b"],
};
const CODE_ORDER = "abcdefghijklmnopqrstuvwxyz";

const s = JSON.parse(readFileSync(STRUCT_PATH, "utf8"));
if (s.subBeatsExpanded) {
  console.log("✓ struct already expanded to 18 sub-beats — no-op");
  process.exit(0);
}

const expanded = [];
let codeIdx = 0;
for (const sec of s.sections) {
  const suffixes = SUB_BEATS[sec.name];
  if (!suffixes) {
    console.warn(`  ⚠ no sub-beat layout for "${sec.name}" — leaving as-is`);
    expanded.push({ ...sec, code: CODE_ORDER[codeIdx++] });
    continue;
  }
  const span = sec.endSec - sec.startSec;
  const step = span / suffixes.length;
  for (let i = 0; i < suffixes.length; i++) {
    const sStart = sec.startSec + i * step;
    const sEnd   = i === suffixes.length - 1 ? sec.endSec : sStart + step;
    expanded.push({
      name: `${sec.name}-${suffixes[i]}`,
      t: +sStart.toFixed(4),
      startSec: +sStart.toFixed(4),
      endSec: +sEnd.toFixed(4),
      code: CODE_ORDER[codeIdx++],
    });
  }
}

if (expanded.length !== 18) {
  console.warn(`  ⚠ expected 18 sub-beats, got ${expanded.length} — verify SUB_BEATS map`);
}

s.sections = expanded;
s.subBeatsExpanded = true;
writeFileSync(STRUCT_PATH, JSON.stringify(s, null, 2) + "\n");
console.log(`✓ expanded ${expanded.length} sub-beats:`);
for (const e of expanded) console.log(`  ${e.code} · ${e.name.padEnd(12)} · ${e.t.toFixed(2)}–${e.endSec.toFixed(2)}s`);
console.log(`  ${STRUCT_PATH.replace(process.env.HOME, "~")}`);
