#!/usr/bin/env node
// stress.mjs — walk cancelok hundreds of times and watch what leaks.
//
// The bug that started this: sound died after many turns, because each room's
// sustained voices piled up until the synth's pool was full. A stress test is the
// only honest way to know it's fixed — and to catch the next thing that grows
// without bound (JS heap, frozen frames, cached modules).
//
//   node cancelok/stress.mjs                 # 200 walks on production
//   node cancelok/stress.mjs --base http://localhost:8899 --walks 120
//   node cancelok/stress.mjs --wander        # random 4-way instead of a straight line
//
// Reads the piece's own __cancelok counters (voices, rooms, frozen, mods) plus the
// JS heap, and fails loudly if any of them climb without settling.

import { existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..");
const argv = process.argv.slice(2);
const flag = (n, d) => {
  const i = argv.indexOf("--" + n);
  return i >= 0 ? argv[i + 1] : d;
};
const BASE = flag("base", "https://aesthetic.computer");
const WALKS = parseInt(flag("walks", "200"), 10);
const WANDER = argv.includes("--wander");
const SETTLE = parseInt(flag("settle", "250"), 10); // ms in each room

const PUP = [`${REPO}/node_modules/puppeteer`, `${REPO}/oven/node_modules/puppeteer`].find((p) => existsSync(p));
if (!PUP) throw new Error("puppeteer not found");
const puppeteer = (await import(`${PUP}/lib/esm/puppeteer/puppeteer.js`)).default;
const CHROME = [process.env.PUPPETEER_EXECUTABLE_PATH, "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"].find((p) => p && existsSync(p));

const b = await puppeteer.launch({
  headless: "new",
  ...(CHROME ? { executablePath: CHROME } : {}),
  args: ["--no-sandbox", "--autoplay-policy=no-user-gesture-required", "--use-gl=angle", "--use-angle=metal", "--enable-gpu", "--js-flags=--expose-gc", "--window-size=800,600"],
});
const p = await b.newPage();
await p.setViewport({ width: 800, height: 600 });

const errors = [];
let stat = null; // latest { voices, rooms, frozen, mods } from the piece heartbeat
// Offline serve-local is noisy in ways that mean nothing here: no backend, no
// fonts, no session socket, and AC's own boot/status logs (which contain the word
// "audio"). Only real thrown errors and genuine synth/voice complaints count.
const NOISE = /google-analytics|favicon|net::ERR_|store-kidlisp|api\/cancelok|the server responded|version info|glyph|Session connection|audio-context-state|starting audio|BOOT|📼|📦|🚀|🎵|Cannot call receive/i;
const REAL = /uncaught|cannot read|is not a function|undefined is not|TypeError|ReferenceError|exception|voice pool|out of voices|max.*voices/i;
const CE = /\[ce\] v(\d+) r(\d+) f(\d+) m(\d+) t(\d+)/;
const onLine = (type, t) => {
  const m = CE.exec(t);
  if (m) {
    stat = { voices: +m[1], rooms: +m[2], frozen: +m[3], mods: +m[4], trail: +m[5] };
    return;
  }
  if (NOISE.test(t)) return;
  if (type === "error" || REAL.test(t)) errors.push(t.slice(0, 140));
};
p.on("console", (m) => onLine(m.type(), m.text()));
p.on("workercreated", (wk) => wk.on("console", (m) => onLine(m.type(), m.text()))); // pieces run in a worker
p.on("pageerror", (e) => errors.push("PAGEERR: " + String(e.message).slice(0, 160)));

const wait = (ms) => new Promise((r) => setTimeout(r, ms));
const heap = () => p.evaluate(() => (performance.memory ? performance.memory.usedJSHeapSize : 0));

console.log(`🏃 stress: ${WALKS} walks on ${BASE}${WANDER ? " (wander)" : " (line)"}\n`);
await p.goto(`${BASE}/cancelok?nogap`, { waitUntil: "networkidle2", timeout: 60000 });
await wait(6000);

if (!stat) {
  console.error("❌ no [ce] heartbeat seen — is this build deployed, and is it painting?");
  await b.close();
  process.exit(1);
}

const KEYS = ["ArrowRight", "ArrowLeft", "ArrowUp", "ArrowDown"];
const samples = [];
let peakVoices = 0;

for (let i = 1; i <= WALKS; i++) {
  // line = pace between two rooms (tests the swap path); wander = trend southeast
  // into ALWAYS-NEW rooms (tests the frozen-frame + module caches growing).
  const explore = ["ArrowRight", "ArrowDown", "ArrowRight", "ArrowDown", "ArrowRight"];
  const dir = WANDER ? explore[i % explore.length] : i % 2 ? "ArrowRight" : "ArrowLeft";
  await p.keyboard.press(dir);
  await wait(SETTLE);
  const c = stat; // latest heartbeat
  if (c) peakVoices = Math.max(peakVoices, c.voices);
  if (i % Math.max(1, Math.floor(WALKS / 10)) === 0 || i === WALKS) {
    await p.evaluate(() => window.gc && window.gc()).catch(() => {});
    const mb = (await heap()) / 1048576;
    samples.push({ walk: i, mb, ...c });
    console.log(
      `  walk ${String(i).padStart(4)}  heap ${mb.toFixed(1)}MB  ` +
        `voices ${c.voices}  rooms ${c.rooms}  frozen ${c.frozen}  mods ${c.mods}`,
    );
  }
}

const first = samples[0];
const last = samples[samples.length - 1];
const growthMB = last.mb - first.mb;
const perWalk = (growthMB * 1024) / WALKS; // KB per walk

console.log("\n— verdict —");
console.log(`  heap: ${first.mb.toFixed(1)} → ${last.mb.toFixed(1)} MB  (${growthMB >= 0 ? "+" : ""}${growthMB.toFixed(1)}MB over ${WALKS} walks, ${perWalk.toFixed(1)}KB/walk)`);
console.log(`  voices: peak ${peakVoices} (must stay small — this is the sound-death signal)`);
console.log(`  JS errors: ${errors.length}`);
errors.slice(0, 6).forEach((e) => console.log(`    · ${e}`));

// Fail conditions. A voice log that climbs past a handful means the leak is back.
// Heap growth over ~0.5MB/walk means something is accumulating per room.
const fails = [];
if (peakVoices > 24) fails.push(`voice log peaked at ${peakVoices} — sustained voices are leaking again`);
if (perWalk > 512) fails.push(`heap grew ${perWalk.toFixed(0)}KB/walk — something accumulates per room`);
// The frozen-frame cache holds a ~300KB painting per room. It must plateau, not
// climb 1:1 with rooms visited, or a long wander eats memory.
if (last.frozen > 40) fails.push(`frozen frames reached ${last.frozen} — the photo cache isn't bounded`);
if (errors.length > 0) fails.push(`${errors.length} JS error(s)`);

await b.close();
if (fails.length) {
  console.log("\n❌ FAIL");
  fails.forEach((f) => console.log("   " + f));
  process.exit(1);
}
console.log("\n✅ PASS — voices bounded, heap stable, no errors.");
