#!/usr/bin/env node
// tally-demos.mjs — what the bot fights actually contain.
//
// Reads saved demos — the farm's JSONL days, or any demo.json the factory
// left in a queue — and turns their signal streams into the questions worth
// asking: who wins and by what, how long rounds really run, which events
// dominate, and where the stalemates are (ties, timeouts, rounds that drag
// past double the median). This is the "is the game boring?" instrument: run
// it before tuning bots, run it after, diff the tables.
//
//   node xbox/live/marketing/tally-demos.mjs oskiewar-demos/          # the farm
//   node xbox/live/marketing/tally-demos.mjs tmp/oskiewar-reels/queue # factory
//   node xbox/live/marketing/tally-demos.mjs some/day.jsonl work/demo.json

import { readFileSync, readdirSync, statSync } from "node:fs";
import { join } from "node:path";

const inputs = process.argv.slice(2);
if (!inputs.length) {
  console.error("usage: tally-demos.mjs <dir|file.jsonl|demo.json> ...");
  process.exit(1);
}

function* demoFiles(path) {
  const info = statSync(path);
  if (info.isFile()) { yield path; return; }
  for (const name of readdirSync(path)) {
    const child = join(path, name);
    if (statSync(child).isDirectory()) yield* demoFiles(child);
    else if (name.endsWith(".jsonl") || name === "demo.json") yield child;
  }
}

function* demosIn(file) {
  const text = readFileSync(file, "utf8");
  if (file.endsWith(".jsonl")) {
    for (const line of text.split("\n")) {
      if (!line.trim()) continue;
      try { yield JSON.parse(line); } catch {}
    }
  } else {
    try { yield JSON.parse(text); } catch {}
  }
}

const events = new Map();          // event name → count
const winners = new Map();         // winner label → rounds won
const causes = new Map();          // ko/balled/tie/timeout → count
const durations = [];              // ticks per round
let rounds = 0, signalsTotal = 0;

for (const input of inputs) {
  for (const file of demoFiles(input)) {
    for (const demo of demosIn(file)) {
      // The stream has worn two names across demo versions.
      const signals = demo.signals || demo.events || [];
      if (!signals.length && !demo.durationTicks) continue;
      rounds++;
      signalsTotal += signals.length;
      const last = signals.at(-1)?.[0] ?? 0;
      durations.push(demo.durationTicks || last);
      let cause = "unresolved";
      for (const [, event] of signals) {
        events.set(event, (events.get(event) || 0) + 1);
        if (event === "ko" || event === "balled" || event === "tie")
          cause = event;
      }
      causes.set(cause, (causes.get(cause) || 0) + 1);
      const winner = demo.winner ?? null;
      if (winner !== null)
        winners.set(String(winner), (winners.get(String(winner)) || 0) + 1);
    }
  }
}

if (!rounds) { console.log("no demos found"); process.exit(0); }

durations.sort((a, b) => a - b);
const median = durations[Math.floor(durations.length / 2)];
const seconds = (ticks) => (ticks / 60).toFixed(1) + "s";
// A stalemate is a round that dragged: double the median or beyond. With few
// rounds the median is noisy — the count still points at the right replays.
const dragged = durations.filter((ticks) => ticks > median * 2).length;

console.log(`${rounds} rounds · ${signalsTotal} signals`);
console.log(`duration median ${seconds(median)} · ` +
  `p90 ${seconds(durations[Math.floor(durations.length * .9)])} · ` +
  `max ${seconds(durations.at(-1))} · dragged>2×median ${dragged}`);
console.log("\nround endings:");
for (const [cause, count] of [...causes].sort((a, b) => b[1] - a[1]))
  console.log(`  ${cause.padEnd(11)} ${count}`);
if (winners.size) {
  console.log("\nround wins by pad:");
  for (const [pad, count] of [...winners].sort())
    console.log(`  pad ${pad.padEnd(7)} ${count}`);
}
console.log("\nevents:");
const width = Math.max(...[...events.keys()].map((name) => name.length));
for (const [event, count] of [...events].sort((a, b) => b[1] - a[1]))
  console.log(`  ${event.padEnd(width)} ${String(count).padStart(6)}` +
    `  ${(count / rounds).toFixed(1)}/round`);
