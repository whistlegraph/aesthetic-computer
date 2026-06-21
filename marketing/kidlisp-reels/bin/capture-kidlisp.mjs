#!/usr/bin/env node
// capture-kidlisp.mjs — record a live KidLisp piece (e.g. $tezz) to a strip of
// PNG frames, headless, with NO source-code HUD. The reel's side-stamps carry
// the $code to type in, so we capture the piece clean.
//
// Clean-capture query params (see system/public/aesthetic.computer/boot.mjs):
//   nolabel    — hide the top-left source/HUD label (the $code)
//   nogap      — piece fills the frame (no border gap)
//   tv         — disable touch/keyboard input (steady display mode)
//   density=N  — chunky pixels (canvas buffer scale; 2–4 reads well)
//   spoofaudio — synthetic audio amplitude so audio-reactive pieces still move
//                headlessly (we render silent; audio is added at post time)
//
// Output: <out>/frames/frame-%05d.png + <out>/capture.json (meta for render-reel).
//
// Usage:
//   node marketing/kidlisp-reels/bin/capture-kidlisp.mjs '$tezz'
//   node .../capture-kidlisp.mjs '$tezz' --duration 12 --fps 30 --density 3 --size 1080

import { existsSync, mkdirSync, rmSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
    else flags[a.slice(2)] = true;
  } else positional.push(a);
}

const CODE = positional[0];
if (!CODE) {
  console.error("usage: capture-kidlisp.mjs '$code' [--duration 12] [--fps 30] [--density 3] [--size 1080]");
  process.exit(1);
}
// slug for filenames: $tezz → tezz
const SLUG = CODE.replace(/^\$/, "").replace(/[^a-z0-9_-]/gi, "-").toLowerCase() || "piece";

const DURATION = parseFloat(flags.duration || 12);
const FPS = parseInt(flags.fps || 30, 10);     // target playback fps (resampled in render)
const DENSITY = String(flags.density || 6);    // higher = chunkier pixels (buffer = viewport/density)
// Portrait by default: the piece fills the whole 9:16 reel frame (full-bleed).
// --square (or --width/--height) captures a 1080×1080 piece for the center-band look.
const CAP_W = parseInt(flags.width || (flags.square ? 1080 : 1080), 10);
const CAP_H = parseInt(flags.height || (flags.square ? 1080 : 1920), 10);
const SETTLE = parseInt(flags.settle || 2500, 10);  // ms for first paint + asset load

const OUT = resolve((flags.out || `${LANE}/out/${SLUG}`).replace(/^~/, process.env.HOME));
const FRAMES_DIR = `${OUT}/frames`;
rmSync(FRAMES_DIR, { recursive: true, force: true });
mkdirSync(FRAMES_DIR, { recursive: true });

const PUPPETEER_DIR = [
  `${REPO}/oven/node_modules/puppeteer`,
  "/opt/oven/node_modules/puppeteer",
  `${REPO}/node_modules/puppeteer`,
].find((p) => existsSync(p));
if (!PUPPETEER_DIR) throw new Error("puppeteer not found (looked in oven/node_modules, node_modules)");
const puppeteer = (await import(`${PUPPETEER_DIR}/lib/esm/puppeteer/puppeteer.js`)).default;

const CHROME = [
  process.env.PUPPETEER_EXECUTABLE_PATH,
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary",
].find((p) => p && existsSync(p));

const BASE = flags.base || "https://aesthetic.computer";
// Build the clean-capture URL. $ stays literal in the path (AC routes $code).
const url = `${BASE}/${CODE}?nolabel&nogap&tv&density=${DENSITY}&spoofaudio`;

console.log(`▸ capture-kidlisp ${CODE} → ${FRAMES_DIR}`);
console.log(`  ${url}`);
console.log(`  ${CAP_W}×${CAP_H} · realtime capture for ${DURATION}s → resample to ${FPS}fps${CHROME ? `  [${CHROME.split("/").pop()}]` : ""}`);

const browser = await puppeteer.launch({
  headless: "new",
  ...(CHROME ? { executablePath: CHROME } : {}),
  args: ["--no-sandbox", "--use-gl=swiftshader", "--autoplay-policy=no-user-gesture-required",
         `--window-size=${CAP_W},${CAP_H}`],
});

const page = await browser.newPage();
await page.setViewport({ width: CAP_W, height: CAP_H, deviceScaleFactor: 1 });
page.on("pageerror", (e) => console.log(`  [pageerror] ${e.message.slice(0, 120)}`));

try {
  await page.goto(url, { waitUntil: "networkidle2", timeout: 45000 });
} catch (e) {
  console.log(`  ⚠ goto: ${e.message.slice(0, 80)} — capturing anyway`);
}
// Focus + nudge audio/runtime awake (tv mode ignores input, but harmless).
await page.evaluate(() => {
  const c = document.querySelector("canvas") || document.getElementById("screen");
  if (c) { c.focus(); c.click(); }
}).catch(() => {});
await page.keyboard.press("Space").catch(() => {});

await new Promise((r) => setTimeout(r, SETTLE));

// REALTIME capture. The piece animates on wall-clock time in the browser, and
// each screenshot costs ~30–80ms (more for heavy pieces). So we DON'T try to
// hit a fixed fps — we grab frames as fast as we can for DURATION real seconds
// and record each frame's true elapsed time. render-reel resamples these to the
// playback fps by timestamp, so the animation plays at its TRUE speed (heavy
// pieces just yield fewer real frames → duped on resample, never sped up).
const t0 = Date.now();
const stamps = [];
let i = 0;
while (true) {
  const t = (Date.now() - t0) / 1000;
  if (t >= DURATION) break;
  const file = `frame-${String(i).padStart(5, "0")}.png`;
  await page.screenshot({ path: `${FRAMES_DIR}/${file}`, omitBackground: false });
  stamps.push({ file, t });
  i++;
  if (i % 60 === 0) console.log(`  ${i} frames · ${t.toFixed(1)}/${DURATION}s real (${(i / Math.max(t, 0.001)).toFixed(1)} fps)`);
}
const realDuration = (Date.now() - t0) / 1000;

await browser.close();

writeFileSync(`${OUT}/capture.json`, JSON.stringify({
  code: CODE, slug: SLUG, captured: stamps.length, targetFps: FPS,
  width: CAP_W, height: CAP_H, duration: DURATION, realDuration,
  density: DENSITY, url, frames: stamps,
}, null, 2));

console.log(`✓ captured ${stamps.length} real frames over ${realDuration.toFixed(1)}s (${(stamps.length / realDuration).toFixed(1)} fps avg) → ${FRAMES_DIR}`);
console.log(`  next: node marketing/kidlisp-reels/bin/render-reel.mjs ${CODE}`);
