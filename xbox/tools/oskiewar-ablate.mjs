#!/usr/bin/env node
// oskiewar remote ablation, 26.08.18
// Walk the render experiment flags on a LIVE session and price each layer in
// the only currency that matters: the fps that machine actually reports.
//
//   node xbox/tools/oskiewar-ablate.mjs <session-name> [--settle 3] [--sample 10]
//
// For each experiment the tool flips one layer off through the relay, waits
// for the change to settle, reads the fps out of the live telemetry frames,
// and restores the layer before moving on. The finale turns everything off at
// once, and the table that comes back is the machine's own testimony about
// where its frame budget goes. Restores the full picture on exit, Ctrl-C
// included.

const RELAY = process.env.OSKIEWAR_RELAY ||
  "wss://session-server.aesthetic.computer/oskiewar-live";
const NAME = /^(?:ow-)?([a-z]{4,7}[0-9]{1,3})$/;

const args = process.argv.slice(2);
const target = args.find((value) => !value.startsWith("--")) || "";
const option = (name, fallback) => {
  const at = args.indexOf(`--${name}`);
  return at >= 0 ? Number(args[at + 1]) || fallback : fallback;
};
const settleMs = option("settle", 3) * 1000;
const sampleMs = option("sample", 10) * 1000;
const match = target.toLowerCase().match(NAME);
if (!match) {
  console.error("usage: oskiewar-ablate.mjs <session-name> [--settle 3] [--sample 10]");
  process.exit(1);
}
const room = "ow-" + match[1];

const FULL = { sky: true, grass: true, shadows: true, dust: true, keys: true,
  bands: 6 };
const EXPERIMENTS = [
  ["baseline", {}],
  ["sky off", { sky: false }],
  ["grass off", { grass: false }],
  ["shadows off", { shadows: false }],
  ["dust off", { dust: false }],
  ["keys off", { keys: false }],
  ["bands 1", { bands: 1 }],
  ["everything off", { sky: false, grass: false, shadows: false, dust: false,
    keys: false, bands: 1 }],
];

const socket = new WebSocket(`${RELAY}?match=${encodeURIComponent(room)}&role=agent`);
let fps = [];
let collecting = false;
socket.addEventListener("message", (event) => {
  let message;
  try { message = JSON.parse(event.data); } catch { return; }
  if (message.type === "oskiewar:state" && collecting &&
      Number.isFinite(message.content?.perf?.fps))
    fps.push(message.content.perf.fps);
});
socket.addEventListener("close", () => {
  console.error("relay closed — the session may have reloaded; rerun with its new name");
  process.exit(1);
});

const wait = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const send = (flags) => socket.send(JSON.stringify(
  { type: "oskiewar:flags", content: flags }));
const restore = () => { try { send(FULL); } catch {} };
process.on("SIGINT", () => { restore(); process.exit(1); });

await new Promise((resolve, reject) => {
  socket.addEventListener("open", resolve);
  socket.addEventListener("error", reject);
});
console.error(`attached to ${room} — ${EXPERIMENTS.length} experiments, ` +
  `${settleMs / 1000}s settle + ${sampleMs / 1000}s sample each`);

const results = [];
for (const [label, flags] of EXPERIMENTS) {
  send({ ...FULL, ...flags });
  await wait(settleMs);
  fps = [];
  collecting = true;
  await wait(sampleMs);
  collecting = false;
  const sorted = fps.slice().sort((a, b) => a - b);
  const median = sorted.length ? sorted[Math.floor(sorted.length / 2)] : NaN;
  results.push({ label, median, samples: sorted.length,
    min: sorted[0] ?? NaN, max: sorted.at(-1) ?? NaN });
  console.error(`  ${label.padEnd(16)} median ${median} fps ` +
    `(${sorted.length} samples, ${sorted[0]}–${sorted.at(-1)})`);
}
restore();

const baseline = results[0].median;
console.log(`\n| experiment | median fps | Δ vs baseline | samples |`);
console.log(`|---|---|---|---|`);
for (const { label, median, samples } of results)
  console.log(`| ${label} | ${median} | ` +
    `${label === "baseline" ? "—" : (median >= baseline ? "+" : "") +
      (median - baseline)} | ${samples} |`);
setTimeout(() => process.exit(0), 500);
