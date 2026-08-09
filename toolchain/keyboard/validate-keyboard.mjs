#!/usr/bin/env node
// Confirm the canonical MacBook Neo layout is physically well-formed.
//
//   node toolchain/keyboard/validate-keyboard.mjs
//
// Exits non-zero on any failure so it can gate a build. The checks are
// aimed at the two failure modes that are invisible in a render: a row
// that comes up short (fusing its last two caps) and a legend that has
// gone missing or doubled.

import {
  ROW_UNITS, rows, expectedLegends, doubledLegends, layout,
} from "./macbook-neo-layout.mjs";

const failures = [];
const check = (ok, message) => { if (!ok) failures.push(message); };

// 1. Every row spans the full deck. A short row is what fuses `=` into
//    delete or drops a bracket off the end of the upper row.
for (const row of rows) {
  const sum = row.keys.reduce((total, [, units]) => total + units, 0);
  check(
    Math.abs(sum - ROW_UNITS) < 1e-9,
    `row "${row.name}" spans ${sum}u, expected ${ROW_UNITS}u ` +
    `(${sum < ROW_UNITS ? "short — caps will fuse" : "long — caps will overflow the deck"})`,
  );
}

// 2. Exact legend inventory. Counted independently of the widths above,
//    because a row can sum correctly and still be missing a key.
const counts = new Map();
for (const row of rows) {
  for (const [label] of row.keys) counts.set(label, (counts.get(label) ?? 0) + 1);
}
for (const legend of expectedLegends) {
  check(counts.get(legend) === 1,
    `legend ${JSON.stringify(legend)} appears ${counts.get(legend) ?? 0}×, expected exactly 1`);
}
for (const legend of doubledLegends) {
  check(counts.get(legend) === 2,
    `legend ${JSON.stringify(legend)} appears ${counts.get(legend) ?? 0}×, expected exactly 2 (one per hand)`);
}
const known = new Set([...expectedLegends, ...doubledLegends]);
for (const legend of counts.keys()) {
  check(known.has(legend), `unexpected legend ${JSON.stringify(legend)} on the deck`);
}

// 3. The specific adjacencies that were reported broken. Named explicitly
//    so a regression reads as itself rather than as an arithmetic error.
const numberRow = rows.find((row) => row.name === "number").keys.map(([label]) => label);
check(numberRow.includes("=") && numberRow.includes("delete"),
  "the number row must carry `=` and `delete` as separate caps, never one fused key");
check(numberRow.indexOf("=") === numberRow.indexOf("delete") - 1,
  "`=` must sit immediately left of `delete`");
const upperRow = rows.find((row) => row.name === "upper").keys.map(([label]) => label);
check(upperRow.filter((l) => l === "[").length === 1 && upperRow.filter((l) => l === "]").length === 1,
  "the upper row must carry both `[` and `]`");
check(upperRow.indexOf("[") === upperRow.indexOf("p") + 1,
  "`[` must sit immediately right of `p`");

// 4. Geometry actually lands inside the deck once laid out in pixels.
const { caps, width, pad } = layout();
for (const cap of caps) {
  check(cap.x >= pad - 1 && cap.x + cap.w <= width - pad + 1,
    `cap ${JSON.stringify(cap.label)} in row "${cap.row}" falls outside the deck`);
}

const capCount = rows.reduce((n, row) => n + row.keys.length, 0);
if (failures.length) {
  console.error(`✗ MacBook Neo layout invalid — ${failures.length} problem(s):\n`);
  for (const failure of failures) console.error(`  • ${failure}`);
  process.exit(1);
}
console.log(`✓ MacBook Neo layout valid — ${rows.length} rows, ${capCount} caps, every row ${ROW_UNITS}u`);
