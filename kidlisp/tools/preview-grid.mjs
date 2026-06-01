#!/usr/bin/env node
// preview-grid.mjs — render a list of KidLisp sources as small thumbnails
// in a contact-sheet grid. Each cell is one source captured at a fixed
// elapsed time after boot so variable-spin angles are non-zero.
//
// Usage:
//   node kidlisp/tools/gen-spinners.mjs | node kidlisp/tools/preview-grid.mjs --out ~/Desktop/spinners.png
//   node kidlisp/tools/preview-grid.mjs --sources sources.txt --cols 16 --cell 96x72
//
// Flags:
//   --sources PATH    file with one source per line (else read stdin)
//   --out PATH        output grid PNG (default: ~/Desktop/spinner-grid.png)
//   --cols N          grid columns (default: 16)
//   --cell WxH        per-cell pixel size (default: 96x72)
//   --base URL        dev server (default: http://localhost:8888)
//   --boot MS         preload wait floor (default: 2500)
//   --settle MS       extra ms after preload before screenshot (default: 1500)
//   --start N         skip first N entries
//   --count N         render only N entries (after start)
//   --label           overlay cell index in top-left of each thumb

import puppeteer from "puppeteer";
import { readFileSync, statSync, existsSync } from "node:fs";
import { join } from "node:path";
import sharp from "sharp";

const args = process.argv.slice(2);
const opts = {};
for (let i = 0; i < args.length; i++) {
  const a = args[i];
  if (!a.startsWith("--")) continue;
  const k = a.slice(2);
  const next = args[i + 1];
  if (next === undefined || next.startsWith("--")) opts[k] = true;
  else { opts[k] = next; i++; }
}

const out = expandHome(opts.out || "~/Desktop/spinner-grid.png");
const cols = Number(opts.cols || 16);
const [cw, ch] = (opts.cell || "96x72").split("x").map(Number);
const base = opts.base || "http://localhost:8888";
const bootMs = Number(opts.boot || 2500);
const settleMs = Number(opts.settle || 1500);
const start = Number(opts.start || 0);
const count = opts.count ? Number(opts.count) : Infinity;
const showLabel = !!opts.label;

let sourcesText;
if (opts.sources) {
  sourcesText = readFileSync(expandHome(opts.sources), "utf8");
} else {
  sourcesText = readFileSync(0, "utf8"); // stdin
}

const allSources = sourcesText
  .split("\n")
  .map((s) => s.trim())
  .filter((s) => s.length > 0 && !s.startsWith("#"));
const sources = count === Infinity
  ? allSources.slice(start)
  : allSources.slice(start, start + count);

console.log(`rendering ${sources.length} cells at ${cw}x${ch}, ${cols} cols`);

const rows = Math.ceil(sources.length / cols);
const pad = 2;
const totalW = (cw + pad) * cols + pad;
const totalH = (ch + pad) * rows + pad;
console.log(`grid: ${cols}×${rows} = ${totalW}×${totalH}px`);

const CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const launchOpts = {
  headless: "new",
  args: ["--no-sandbox", "--disable-setuid-sandbox", "--mute-audio"],
};
if (existsSync(CHROME)) launchOpts.executablePath = CHROME;

let browser = await puppeteer.launch(launchOpts);
let browserDisconnects = 0;
browser.on("disconnected", () => {
  browserDisconnects++;
});

const tiles = []; // { buf, index, source }
const t0 = Date.now();
const blackTile = await sharp({
  create: { width: cw, height: ch, channels: 3, background: { r: 0, g: 0, b: 0 } },
}).png().toBuffer();

for (let i = 0; i < sources.length; i++) {
  const src = sources[i];
  // Ensure browser is alive — relaunch if it died
  if (!browser.connected || browserDisconnects > 0) {
    try { await browser.close(); } catch {}
    console.warn(`  browser disconnected (${browserDisconnects}); relaunching...`);
    browser = await puppeteer.launch(launchOpts);
    browserDisconnects = 0;
    browser.on("disconnected", () => { browserDisconnects++; });
  }

  let page;
  let buf = null;
  try {
    page = await browser.newPage();
    await page.setViewport({ width: cw, height: ch, deviceScaleFactor: 1 });
    const encoded = encodeKidlispForUrl(src);
    const url = `${base}/${encoded}?nogap=true&tv=true&density=1&nolabel=true`;
    await page.goto(url, { waitUntil: "domcontentloaded", timeout: 30000 });
    try {
      await page.waitForFunction(() => window.preloaded === true, { timeout: bootMs * 3 });
    } catch {}
    await new Promise((r) => setTimeout(r, settleMs));
    buf = await page.screenshot({ type: "png" });
  } catch (err) {
    console.warn(`  cell ${i} failed: ${err.message.slice(0, 100)}`);
  } finally {
    if (page) {
      try { await page.close(); } catch {}
    }
  }

  tiles.push({ buf: buf || blackTile, index: start + i, source: src });

  const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
  const eta = (((Date.now() - t0) / (i + 1)) * (sources.length - i - 1) / 1000).toFixed(0);
  if (i % 8 === 0 || i === sources.length - 1) {
    console.log(`  [${i + 1}/${sources.length}] elapsed=${elapsed}s eta=${eta}s`);
  }
}

try { await browser.close(); } catch {}

console.log("compositing grid...");

const composites = [];
for (let i = 0; i < tiles.length; i++) {
  const r = Math.floor(i / cols);
  const c = i % cols;
  const x = pad + c * (cw + pad);
  const y = pad + r * (ch + pad);
  composites.push({ input: tiles[i].buf, left: x, top: y });
  if (showLabel) {
    const label = String(tiles[i].index).padStart(3, "0");
    const svg = Buffer.from(
      `<svg width="${cw}" height="12" xmlns="http://www.w3.org/2000/svg">` +
        `<rect width="20" height="12" fill="rgba(0,0,0,0.6)"/>` +
        `<text x="2" y="9" font-family="Menlo, monospace" font-size="9" fill="white">${label}</text>` +
        `</svg>`,
    );
    composites.push({ input: svg, left: x, top: y });
  }
}

await sharp({
  create: { width: totalW, height: totalH, channels: 4, background: { r: 12, g: 12, b: 16, alpha: 1 } },
})
  .composite(composites)
  .png()
  .toFile(out);

const sizeKb = Math.round(statSync(out).size / 1024);
console.log(`wrote ${out} (${sizeKb} KB)`);

function expandHome(p) {
  return p.startsWith("~/") ? join(process.env.HOME, p.slice(2)) : p;
}

function encodeKidlispForUrl(src) {
  return src
    .replace(/ /g, "_")
    .replace(/\n/g, "§")
    .replace(/%/g, "¤")
    .replace(/;/g, "¨")
    .replace(/#/g, "%23");
}
