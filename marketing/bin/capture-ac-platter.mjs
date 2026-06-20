#!/usr/bin/env node
// capture-ac-platter.mjs — capture a platter of REAL aesthetic.computer /
// notepat screens (live production), at laptop aspect, for use as laptop-screen
// imagery and as identity refs for gpt-image-2 regens (e.g. the Restless Egg
// felt video). No generation — these are actual frames of the running product.
//
// Writes one PNG per target to marketing/captures/platter/<name>.png, then run
// build-ac-platter.py to assemble the labeled contact sheet.
//
// Usage:
//   node marketing/bin/capture-ac-platter.mjs                # all targets
//   node marketing/bin/capture-ac-platter.mjs --only notepat # one target
//   node marketing/bin/capture-ac-platter.mjs --out ~/Desktop/foo
//   node marketing/bin/capture-ac-platter.mjs --width 1280 --height 800 --scale 2

import { existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..");

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
    else flags[a.slice(2)] = true;
  }
}

const OUT = resolve((flags.out || `${REPO}/marketing/captures/platter`).replace(/^~/, process.env.HOME));
mkdirSync(OUT, { recursive: true });

const PUPPETEER_DIR = [
  `${REPO}/oven/node_modules/puppeteer`,
  "/opt/oven/node_modules/puppeteer",
].find((p) => existsSync(p));
if (!PUPPETEER_DIR) throw new Error("puppeteer not found in oven/node_modules");
const puppeteer = (await import(`${PUPPETEER_DIR}/lib/esm/puppeteer/puppeteer.js`)).default;

// Curated platter of live AC screens. boot=true → click + Space to wake audio/
// piece; settle is per-target ms to let the piece paint.
const BASE = "https://aesthetic.computer";
// Every note key across the whole notepat board (two octaves of tiles).
const SWEEP = ["KeyC","KeyV","KeyD","KeyS","KeyE","KeyF","KeyW","KeyG","KeyR","KeyA","KeyQ","KeyB",
               "KeyH","KeyT","KeyI","KeyY","KeyJ","KeyK","KeyU","KeyL","KeyO","KeyM","KeyP","KeyN"];
const TARGETS = [
  // notepat: boot, Tab to switch from gm → sine, then sweep every key across the
  // whole board (×3) so the center timeline fills, ending on a held chord.
  { name: "notepat",   url: `${BASE}/notepat`,  label: "notepat — the instrument",        settle: 4000, boot: true,
    setup: ["Tab"], play: [...SWEEP, ...SWEEP, ...SWEEP], playDelay: 70,
    hold: ["KeyC","KeyE","KeyG","KeyH"] },
  { name: "prompt",    url: `${BASE}/prompt`,   label: "aesthetic.computer — the prompt",  settle: 6000, boot: false },
  { name: "kidlisp-roz",  url: `${BASE}/$roz`,  label: "$roz — a KidLisp piece",           settle: 16000, boot: true },
  { name: "kidlisp-ger",  url: `${BASE}/$ger`,  label: "$ger — a KidLisp piece",           settle: 16000, boot: true },
  { name: "laklok",       url: `${BASE}/laklok`, label: "laklok",                          settle: 24000, boot: true },
];

// Chunkier pixels read better as laptop-screen imagery / felt-screen refs.
// AC's ?density=N scales the canvas buffer — higher = bigger pixels (device
// mode uses 8). Tune per capture with --density.
const DENSITY = flags.density ? String(flags.density) : "4";
const withDensity = (url) => `${url}${url.includes("?") ? "&" : "?"}density=${DENSITY}`;

const only = typeof flags.only === "string" ? flags.only.split(",").map((s) => s.trim()) : null;
const targets = only ? TARGETS.filter((t) => only.includes(t.name)) : TARGETS;

// Puppeteer's bundled Chrome may be absent; fall back to the system Chrome.
const CHROME = [
  process.env.PUPPETEER_EXECUTABLE_PATH,
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary",
].find((p) => p && existsSync(p));

console.log(`▸ capture-ac-platter → ${OUT}  (${targets.length} targets)${CHROME ? `  [${CHROME.split("/").pop()}]` : ""}`);
const browser = await puppeteer.launch({
  headless: "new",
  ...(CHROME ? { executablePath: CHROME } : {}),
  args: ["--no-sandbox", "--use-gl=swiftshader", "--autoplay-policy=no-user-gesture-required"],
});

for (const t of targets) {
  const page = await browser.newPage();
  await page.setViewport({
    width: parseInt(flags.width || 1280, 10),
    height: parseInt(flags.height || 800, 10),
    deviceScaleFactor: parseFloat(flags.scale || 2),
  });
  page.on("pageerror", (e) => console.log(`  [${t.name}/pageerror] ${e.message.slice(0, 120)}`));
  try {
    await page.goto(withDensity(t.url), { waitUntil: "networkidle2", timeout: 45000 });
  } catch (e) {
    console.log(`  ⚠ ${t.name}: goto ${e.message.slice(0, 80)} — capturing anyway`);
  }
  if (t.boot) {
    await page.evaluate(() => {
      const c = document.querySelector("canvas") || document.getElementById("screen");
      if (c) { c.focus(); c.click(); }
    }).catch(() => {});
    await page.keyboard.press("Space").catch(() => {});
  }
  await new Promise((r) => setTimeout(r, t.settle));
  // optional: type a command (e.g. to reveal the prompt discovery UI, no Enter)
  if (typeof t.type === "string") {
    await page.keyboard.type(t.type, { delay: 110 }).catch(() => {});
    await new Promise((r) => setTimeout(r, 2500));
  }
  // optional: one-shot setup keys (e.g. Tab to switch notepat's synth → sine)
  if (Array.isArray(t.setup)) {
    for (const k of t.setup) {
      await page.keyboard.press(k).catch(() => {});
      await new Promise((r) => setTimeout(r, 500));
    }
  }
  // optional: play a run of note keys to populate the center (notepat roll)
  if (Array.isArray(t.play)) {
    for (const k of t.play) {
      await page.keyboard.press(k).catch(() => {});
      await new Promise((r) => setTimeout(r, t.playDelay || 150));
    }
  }
  // optional: hold a chord down through the screenshot, then release
  const held = [];
  if (Array.isArray(t.hold)) {
    for (const k of t.hold) { await page.keyboard.down(k).catch(() => {}); held.push(k); }
    await new Promise((r) => setTimeout(r, 600));
  }
  const out = `${OUT}/${t.name}.png`;
  await page.screenshot({ path: out, omitBackground: false });
  for (const k of held) await page.keyboard.up(k).catch(() => {});
  console.log(`✓ ${t.name} → ${out}`);
  await page.close();
}

await browser.close();
console.log("done — assemble with: python3 marketing/bin/build-ac-platter.py");
