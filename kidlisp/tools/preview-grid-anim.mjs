#!/usr/bin/env node
// preview-grid-anim.mjs — render N kidlisp sources as small animations,
// composite each timestep into a grid frame, encode as a single mp4.
//
// Workflow per cell: nav → wait for preloaded → screencast at native fps
// for `duration` ms → save frames as JPEGs. Then for each frame index k,
// composite all 256 cell-k frames into one grid frame. Encode grid frames
// as mp4 via ffmpeg.
//
// Usage:
//   node kidlisp/tools/preview-grid-anim.mjs --sources /tmp/spinner-sources.txt \
//     --out ~/Desktop/spinners-anim.mp4 --cols 16 --cell 96x72 --duration 4000 --fps 15

import puppeteer from "puppeteer";
import { readFileSync, mkdtempSync, mkdirSync, rmSync, existsSync, readdirSync, statSync, writeFileSync } from "node:fs";
import { writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawn } from "node:child_process";
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

const out = expandHome(opts.out || "~/Desktop/spinners-anim.mp4");
const cols = Number(opts.cols || 16);
const [cw, ch] = (opts.cell || "96x72").split("x").map(Number);
const base = opts.base || "http://localhost:8888";
const bootMs = Number(opts.boot || 1500);
const settleMs = Number(opts.settle || 500);
const duration = Number(opts.duration || 4000);
const fps = Number(opts.fps || 15);
const quality = Number(opts.quality || 85);
const start = Number(opts.start || 0);
const count = opts.count ? Number(opts.count) : Infinity;
const keep = !!opts.keep;

let sourcesText;
if (opts.sources) sourcesText = readFileSync(expandHome(opts.sources), "utf8");
else sourcesText = readFileSync(0, "utf8");

const all = sourcesText.split("\n").map(s => s.trim()).filter(s => s && !s.startsWith("#"));
const sources = count === Infinity ? all.slice(start) : all.slice(start, start + count);

const rows = Math.ceil(sources.length / cols);
const pad = 2;
const gridW = (cw + pad) * cols + pad;
const gridH = (ch + pad) * rows + pad;
const frameCount = Math.round((duration / 1000) * fps);
const frameInterval = 1000 / fps;

console.log(`grid: ${cols}×${rows}, cell ${cw}×${ch}, total ${gridW}×${gridH}`);
console.log(`capture: ${sources.length} cells × ${frameCount} frames @ ${fps}fps`);
console.log(`out: ${out}`);

const work = mkdtempSync(join(tmpdir(), "spinner-grid-anim-"));
const cellsDir = join(work, "cells");
mkdirSync(cellsDir, { recursive: true });

const CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const launchOpts = {
  headless: "new",
  args: ["--no-sandbox", "--disable-setuid-sandbox", "--mute-audio"],
};
if (existsSync(CHROME)) launchOpts.executablePath = CHROME;

let browser = await puppeteer.launch(launchOpts);

// Pass 1: capture per-cell frame sequences
const t0 = Date.now();
for (let i = 0; i < sources.length; i++) {
  const src = sources[i];
  const cellDir = join(cellsDir, String(i).padStart(4, "0"));
  mkdirSync(cellDir, { recursive: true });

  if (!browser.connected) {
    try { await browser.close(); } catch {}
    console.warn(`  browser died; relaunching at cell ${i}...`);
    browser = await puppeteer.launch(launchOpts);
  }

  let page;
  try {
    page = await browser.newPage();
    await page.setViewport({ width: cw, height: ch, deviceScaleFactor: 1 });
    const encoded = encodeKidlispForUrl(src);
    const url = `${base}/${encoded}?nogap=true&tv=true&density=1&nolabel=true`;
    await page.goto(url, { waitUntil: "domcontentloaded", timeout: 30000 });
    try {
      await page.waitForFunction(() => window.preloaded === true, { timeout: bootMs * 3 });
    } catch {}
    await new Promise(r => setTimeout(r, settleMs));

    // Screencast capture: single persistent CDP connection streams frames
    // at AC's native paint rate (~60fps). everyNthFrame samples it down
    // to the target output fps. This avoids hammering the dev server with
    // 60+ individual screenshot requests per cell.
    const client = await page.target().createCDPSession();
    const everyNth = Math.max(1, Math.round(60 / fps));
    let captured = 0;
    const onFrame = ({ data, sessionId }) => {
      if (captured >= frameCount) return;
      const buf = Buffer.from(data, "base64");
      const fpath = join(cellDir, `f${String(captured).padStart(3, "0")}.jpg`);
      writeFile(fpath, buf).catch(() => {});
      captured++;
      client.send("Page.screencastFrameAck", { sessionId }).catch(() => {});
    };
    client.on("Page.screencastFrame", onFrame);
    await client.send("Page.startScreencast", {
      format: "jpeg",
      quality: quality,
      maxWidth: cw,
      maxHeight: ch,
      everyNthFrame: everyNth,
    });
    await new Promise(r => setTimeout(r, duration));
    await client.send("Page.stopScreencast").catch(() => {});
    client.off("Page.screencastFrame", onFrame);
    await client.detach().catch(() => {});
  } catch (err) {
    console.warn(`  cell ${i} failed: ${err.message.slice(0, 80)}`);
  } finally {
    if (page) { try { await page.close(); } catch {} }
  }

  const elapsed = ((Date.now() - t0) / 1000).toFixed(0);
  const eta = (((Date.now() - t0) / (i + 1)) * (sources.length - i - 1) / 1000).toFixed(0);
  if (i % 8 === 0 || i === sources.length - 1) {
    console.log(`  [${i + 1}/${sources.length}] elapsed=${elapsed}s eta=${eta}s`);
  }
}
try { await browser.close(); } catch {}

// Pass 2: per-frame compositing
console.log("compositing frames...");
const blackCell = await sharp({
  create: { width: cw, height: ch, channels: 3, background: { r: 0, g: 0, b: 0 } },
}).jpeg().toBuffer();

const framesDir = join(work, "frames");
mkdirSync(framesDir, { recursive: true });

for (let k = 0; k < frameCount; k++) {
  const composites = [];
  for (let i = 0; i < sources.length; i++) {
    const cellDir = join(cellsDir, String(i).padStart(4, "0"));
    const tilePath = join(cellDir, `f${String(k).padStart(3, "0")}.jpg`);
    const tile = existsSync(tilePath) ? tilePath : blackCell;
    const r = Math.floor(i / cols);
    const c = i % cols;
    const x = pad + c * (cw + pad);
    const y = pad + r * (ch + pad);
    composites.push({ input: tile, left: x, top: y });
  }
  const framePath = join(framesDir, `g${String(k).padStart(4, "0")}.jpg`);
  await sharp({
    create: { width: gridW, height: gridH, channels: 3, background: { r: 12, g: 12, b: 16 } },
  })
    .composite(composites)
    .jpeg({ quality: quality })
    .toFile(framePath);
  if (k % 10 === 0 || k === frameCount - 1) {
    console.log(`  frame ${k + 1}/${frameCount}`);
  }
}

// Pass 3: encode mp4
console.log("encoding mp4...");
const crf = Math.max(8, Math.min(32, Math.round(30 - quality * 0.2)));
await new Promise((resolve, reject) => {
  const proc = spawn("ffmpeg", [
    "-y",
    "-framerate", String(fps),
    "-i", join(framesDir, "g%04d.jpg"),
    "-c:v", "libx264",
    "-pix_fmt", "yuv420p",
    "-preset", "slow",
    "-crf", String(crf),
    "-tune", "animation",
    "-movflags", "+faststart",
    "-vf", "scale=trunc(iw/2)*2:trunc(ih/2)*2",
    out,
  ], { stdio: ["ignore", "ignore", "inherit"] });
  proc.on("error", reject);
  proc.on("exit", c => c === 0 ? resolve() : reject(new Error(`ffmpeg exit ${c}`)));
});

const sizeKb = Math.round(statSync(out).size / 1024);
console.log(`wrote ${out} (${sizeKb} KB)`);

if (!keep) rmSync(work, { recursive: true, force: true });
else console.log(`kept work dir: ${work}`);

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
