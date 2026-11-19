#!/usr/bin/env node
// classic-odds.mjs, 2026.11.18
// Print how often each brush is proposed in No Paint classic: the 2021 game's
// odds beside the tuned ones (classic-tuning.json), with the change.
//
//   node toolchain/nopaint/classic-odds.mjs                  the table
//   node toolchain/nopaint/classic-odds.mjs --no-ai          as it is when no AI turn can be had
//   node toolchain/nopaint/classic-odds.mjs --sample 100000  also draw that many proposals and count them
//   node toolchain/nopaint/classic-odds.mjs --json           the tuned odds as JSON
//
// The numbers come from classic-picker.js, the same file the game runs.

import { readFileSync } from "node:fs";

const read = (path) => readFileSync(new URL(path, import.meta.url), "utf8");
const self = {};
new Function("self", read("./classic-picker.js"))(self);
const picker = self.nopaintPicker;
const tuning = JSON.parse(read("./classic-tuning.json"));
const { brushes, totals } = JSON.parse(read("../../papers/arxiv-nopaint-brushbook/brushes.json"));
const args = process.argv.slice(2);
const flags = { ai: !args.includes("--no-ai") };
const samples = args.includes("--sample") ? Number(args[args.indexOf("--sample") + 1]) : 0;

// The 2021 game in the long run: Brushes two times in three, then by weight.
const SPLIT_2021 = { Brushes: 2 / 3, Filters: 1 / 3 };
const game = Object.fromEntries(brushes.filter((b) => b.table).map((b) => [b.name, SPLIT_2021[b.table] * b.weight / totals[b.table]]));
const resolved = picker.resolve(tuning, flags);
const rows = picker.table(resolved);
if (args.includes("--json")) {
  console.log(JSON.stringify(Object.fromEntries(rows.map((r) => [r.name, +(r.chance * 100).toFixed(3)])), null, 1));
  process.exit(0);
}

const counts = {};
// Drawn as the game draws: one long run, with the first-proposal and no-repeat rules.
const run = {};
for (let i = 0; i < samples; i++) { const name = picker.next(run, tuning, resolved); counts[name] = (counts[name] || 0) + 1; }
const pct = (p) => p === undefined ? "    -" : (p * 100).toFixed(2).padStart(5);
console.log(`${"brush".padEnd(12)} ${"list".padEnd(8)} ${"2021".padStart(5)} ${"tuned".padStart(5)} ${"change".padStart(6)}${samples ? ` ${"drawn".padStart(5)}` : ""}   in 100 proposals${flags.ai ? "" : ", no AI turn to be had"}`);
for (const row of rows) {
  const was = game[row.name];
  const change = was === undefined ? "   new" : `${row.chance - was >= 0 ? "+" : "-"}${(Math.abs(row.chance - was) * 100).toFixed(2)}`.padStart(6);
  console.log(`${row.name.padEnd(12)} ${row.list.padEnd(8)} ${pct(was)} ${pct(row.chance)} ${change}${samples ? ` ${pct((counts[row.name] || 0) / samples)}` : ""}   ${"#".repeat(Math.round(row.chance * 100 * 4))}`);
}
console.log(`${"total".padEnd(21)} ${pct(Object.values(game).reduce((a, b) => a + b, 0))} ${pct(rows.reduce((a, r) => a + r.chance, 0))}`);
for (const [list, { share, entries }] of Object.entries(resolved)) console.log(`${list}: ${(share * 100).toFixed(0)} in 100, ${Object.keys(entries).length} brushes`);
