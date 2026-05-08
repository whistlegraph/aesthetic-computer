#!/usr/bin/env node
// capture-ac-native.mjs — capture real ac-native UI frames from the
// WASM offline pages.
//
// Loads `system/public/ac-native-wasm/{index,notepat}.html` in headless
// Chrome via puppeteer, focuses the canvas so the WASM main loop wakes
// up, types `notepat` at the prompt for the first frame, lets the
// notepat instrument settle for the second, and writes both PNGs to
// `<out-dir>/ac-native-{prompt,notepat}.png`.
//
// Use the captures as identity refs in `marketing/bin/gen-promo.mjs`
// so model renders of the laptop screen match the real ac-native UI.
//
// Usage:
//   node marketing/bin/capture-ac-native.mjs                      # writes to marketing/captures/
//   node marketing/bin/capture-ac-native.mjs --out <dir>          # custom out dir
//   node marketing/bin/capture-ac-native.mjs --out ~/Desktop/foo/refs

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

const OUT = resolve((flags.out || `${REPO}/marketing/captures`).replace(/^~/, process.env.HOME));
mkdirSync(OUT, { recursive: true });

const PUPPETEER_DIR = [
  `${REPO}/oven/node_modules/puppeteer`,
  "/opt/oven/node_modules/puppeteer",
].find((p) => existsSync(p));
if (!PUPPETEER_DIR) throw new Error("puppeteer not found in oven/node_modules");
const puppeteer = (await import(`${PUPPETEER_DIR}/lib/esm/puppeteer/puppeteer.js`)).default;

const targets = [
  { name: "ac-native-prompt", url: `file://${REPO}/system/public/ac-native-wasm/index.html`, type: "prompt" },
  { name: "ac-native-notepat", url: `file://${REPO}/system/public/ac-native-wasm/notepat.html`, type: "notepat" },
];

console.log(`▸ capture-ac-native → ${OUT}`);
const browser = await puppeteer.launch({
  headless: "new",
  args: ["--no-sandbox", "--use-gl=swiftshader"],
});

for (const t of targets) {
  const page = await browser.newPage();
  await page.setViewport({ width: 960, height: 640, deviceScaleFactor: 2 });
  page.on("pageerror", (e) => console.log(`  [${t.name}/pageerror] ${e.message}`));
  await page.goto(t.url, { waitUntil: "networkidle0" });
  await page.evaluate(() => {
    const c = document.getElementById("screen");
    if (c) { c.focus(); c.click(); }
  });
  await page.keyboard.press("Space");
  await new Promise((r) => setTimeout(r, 3000));
  if (t.type === "prompt") await page.keyboard.type("notepat", { delay: 80 });
  await new Promise((r) => setTimeout(r, 4000));
  const out = `${OUT}/${t.name}.png`;
  await page.screenshot({ path: out, omitBackground: false });
  console.log(`✓ ${out}`);
  await page.close();
}

await browser.close();
