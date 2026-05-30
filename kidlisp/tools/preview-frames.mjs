#!/usr/bin/env node
// preview-frames.mjs — render a KidLisp piece at multiple real-time
// elapsed offsets via the local dev server + puppeteer, then stitch
// into a contact sheet.
//
// Usage:
//   node kidlisp/tools/preview-frames.mjs "(wipe fade:black-purple-red-purple-black:(frame))" \
//     --out ~/Desktop/spin-test.png
//
// Flags:
//   --out PATH         output contact-sheet PNG (default: ~/Desktop/kidlisp-preview.png)
//   --delays CSV       per-frame real-time elapsed ms after boot
//                      (default: 500,1000,1500,2000,3000,5000)
//   --cols N           contact-sheet columns (default: 3)
//   --size WxH         per-frame size (default: 512x512)
//   --base URL         dev server base (default: http://localhost:8888)
//   --boot MS          boot settle time before counting frames (default: 2500)
//   --label STR        title strip across top of sheet
//   --keep             keep individual frame PNGs in a sibling dir

import puppeteer from "puppeteer";
import { mkdtempSync, rmSync, mkdirSync, copyFileSync, existsSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import sharp from "sharp";

const args = process.argv.slice(2);
if (!args.length || args[0].startsWith("--")) {
  console.error("usage: preview-frames.mjs '<kidlisp source>' [flags]");
  process.exit(1);
}

const source = args.shift();
const opts = parseFlags(args);
const out = expandHome(opts.out || "~/Desktop/kidlisp-preview.png");
const delays = (opts.delays || "500,1000,1500,2000,3000,5000")
  .split(",")
  .map((s) => Number(s.trim()))
  .filter((n) => n > 0);
const cols = Number(opts.cols || 3);
const [w, h] = (opts.size || "512x512").split("x").map(Number);
const base = opts.base || "http://localhost:8888";
const bootMs = Number(opts.boot || 2500);
const label = opts.label || source;

const encoded = encodeKidlispForUrl(source);
const url = `${base}/${encoded}?nogap=true&density=1`;

const work = mkdtempSync(join(tmpdir(), "kidlisp-preview-"));
const framePaths = [];

console.log("source:", source);
console.log("url:", url);
console.log("delays:", delays, "ms (after", bootMs, "ms boot)");

const CHROME_FALLBACK = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const launchOpts = {
  headless: "new",
  args: ["--no-sandbox", "--disable-setuid-sandbox", "--mute-audio"],
};
if (existsSync(CHROME_FALLBACK)) launchOpts.executablePath = CHROME_FALLBACK;
const browser = await puppeteer.launch(launchOpts);

try {
  for (let i = 0; i < delays.length; i++) {
    const delay = delays[i];
    const outPath = join(work, `frame-${String(i).padStart(2, "0")}-d${delay}.png`);
    console.log(`  [${i + 1}/${delays.length}] elapsed=${delay}ms`);

    const page = await browser.newPage();
    await page.setViewport({ width: w, height: h, deviceScaleFactor: 1 });
    await page.goto(url, { waitUntil: "domcontentloaded", timeout: 30000 });
    // Wait for AC to boot + render `delay` ms of frames
    await new Promise((r) => setTimeout(r, bootMs + delay));
    await page.screenshot({ path: outPath, type: "png" });
    await page.close();
    framePaths.push({ path: outPath, delay });
  }
} finally {
  await browser.close();
}

await buildContactSheet(framePaths, out, { cols, w, h, label });
console.log(`\nwrote ${out}`);

if (opts.keep) {
  const keepDir = out.replace(/\.png$/, "-frames");
  mkdirSync(keepDir, { recursive: true });
  for (const f of framePaths) copyFileSync(f.path, join(keepDir, `d${f.delay}.png`));
  console.log(`kept individual frames in ${keepDir}`);
}

rmSync(work, { recursive: true, force: true });

// ────────────────────────────────────────────────────────────────────

function parseFlags(arr) {
  const o = {};
  for (let i = 0; i < arr.length; i++) {
    const a = arr[i];
    if (!a.startsWith("--")) continue;
    const k = a.slice(2);
    const next = arr[i + 1];
    if (next === undefined || next.startsWith("--")) {
      o[k] = true;
    } else {
      o[k] = next;
      i++;
    }
  }
  return o;
}

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

async function buildContactSheet(frames, outPath, { cols, w, h, label }) {
  const rows = Math.ceil(frames.length / cols);
  const pad = 8;
  const labelH = 36;
  const captionH = 22;
  const cellW = w + pad * 2;
  const cellH = h + pad * 2 + captionH;
  const totalW = cellW * cols;
  const totalH = labelH + cellH * rows;

  const bg = { r: 16, g: 16, b: 20, alpha: 1 };

  const composites = [];
  for (let i = 0; i < frames.length; i++) {
    const r = Math.floor(i / cols);
    const c = i % cols;
    const x = c * cellW + pad;
    const y = labelH + r * cellH + pad;
    composites.push({ input: frames[i].path, left: x, top: y });

    const captionSvg = Buffer.from(
      `<svg width="${w}" height="${captionH}" xmlns="http://www.w3.org/2000/svg">` +
        `<rect width="100%" height="100%" fill="rgb(16,16,20)"/>` +
        `<text x="6" y="16" font-family="Menlo, monospace" font-size="13" fill="rgb(180,180,200)">elapsed ${frames[i].delay}ms</text>` +
        `</svg>`,
    );
    composites.push({ input: captionSvg, left: x, top: y + h + 2 });
  }

  const titleSvg = Buffer.from(
    `<svg width="${totalW}" height="${labelH}" xmlns="http://www.w3.org/2000/svg">` +
      `<rect width="100%" height="100%" fill="rgb(8,8,12)"/>` +
      `<text x="12" y="24" font-family="Menlo, monospace" font-size="15" fill="rgb(220,220,240)">${escapeXml(label)}</text>` +
      `</svg>`,
  );
  composites.unshift({ input: titleSvg, left: 0, top: 0 });

  await sharp({
    create: { width: totalW, height: totalH, channels: 4, background: bg },
  })
    .composite(composites)
    .png()
    .toFile(outPath);
}

function escapeXml(s) {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&apos;");
}
