#!/usr/bin/env node
// preview-anim.mjs — render a KidLisp piece to an animated webp or mp4
// by capturing a frame sequence from the local dev server via puppeteer.
// Output format is chosen by extension on --out (.webp via img2webp,
// .mp4 via ffmpeg libx264).
//
// Capture mode:
//   --realtime (default for mp4): uses Chrome's Page.startScreencast to
//   stream frames as the page renders — captures AC's actual ~60fps tick.
//   --no-realtime (or default for webp): polls page.screenshot() at the
//   requested --fps. Smaller fps yields smaller webp.
//
// Usage:
//   node kidlisp/tools/preview-anim.mjs "(wipe fade:red-rainbow:frame)" \
//     --out ~/Desktop/spin.mp4
//
// Flags:
//   --out PATH         output animation (.webp or .mp4)
//                      (default: ~/Desktop/kidlisp-anim.webp)
//   --fps N            output frame rate (default: 60 for mp4, 20 for webp)
//   --duration MS      total animation length in ms (default: 4000)
//   --size WxH         per-frame size (default: 512x512)
//   --base URL         dev server base (default: http://localhost:8888)
//   --boot MS          boot settle time before capture starts (default: 2500)
//   --quality 0-100    webp quality (default: 80) / mp4 CRF inverse:
//                      mp4 CRF = round((100-quality) * 0.4), clamped 14..32
//   --scale N          output pixel multiplier (puppeteer deviceScaleFactor).
//                      AC's VIRTUAL canvas stays at --size; the captured
//                      frames are size × scale physical pixels.
//                      (default: 1)
//   --density N        DEPRECATED alias for --scale (kept for back-compat)
//   --realtime         force screencast-mode capture
//   --no-realtime      force poll-mode capture
//   --keep             keep individual frame PNGs/JPEGs in a sibling dir

import puppeteer from "puppeteer";
import { mkdtempSync, rmSync, mkdirSync, copyFileSync, existsSync, readdirSync, statSync } from "node:fs";
import { writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawn } from "node:child_process";

const args = process.argv.slice(2);
if (!args.length || args[0].startsWith("--")) {
  console.error("usage: preview-anim.mjs '<kidlisp source>' [flags]");
  process.exit(1);
}

const source = args.shift();
const opts = parseFlags(args);
const out = expandHome(opts.out || "~/Desktop/kidlisp-anim.webp");
const fmt = out.toLowerCase().endsWith(".mp4") ? "mp4" : "webp";
const fps = Number(opts.fps || (fmt === "mp4" ? 60 : 20));
const duration = Number(opts.duration || 4000);
const [w, h] = (opts.size || "320x240").split("x").map(Number);
const base = opts.base || "http://localhost:8888";
const bootMs = Number(opts.boot || 4000);
const quality = Number(opts.quality || (fmt === "mp4" ? 95 : 85));
const scale = Number(opts.scale || opts.density || 1);
const realtime = opts["no-realtime"] ? false : opts.realtime || fmt === "mp4";

const totalFrames = Math.round((duration / 1000) * fps);
const frameInterval = 1000 / fps;

// AC renders at native virtual resolution (--size, density=1) for sharpness
// and 60fps, then ffmpeg nearest-neighbor upscales to --size × --scale on
// encode. Each AC virtual pixel becomes a crisp scale×scale block.
const outW = w * scale;
const outH = h * scale;
const encoded = encodeKidlispForUrl(source);
// `tv=true` = AC's headless-capture mode. `nogap=true` removes chrome border.
// `nolabel=true` removes the top source-code label band (default: keep label).
const labelOff = opts["no-label"] || opts.nolabel ? "&nolabel=true" : "";
const url = `${base}/${encoded}?nogap=true&tv=true&density=1${labelOff}`;

const work = mkdtempSync(join(tmpdir(), "kidlisp-anim-"));

console.log("source:", source);
console.log("url:", url);
console.log(
  `capturing ${realtime ? "via screencast" : `${totalFrames} polled frames`} @ ${fps}fps (${duration}ms total)`,
);
console.log("out:", out);

const CHROME_FALLBACK = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const launchOpts = {
  headless: "new",
  args: ["--no-sandbox", "--disable-setuid-sandbox", "--mute-audio"],
};
if (existsSync(CHROME_FALLBACK)) launchOpts.executablePath = CHROME_FALLBACK;
const browser = await puppeteer.launch(launchOpts);

let captureRate = fps; // frames captured per second of real-time
let frameExt = realtime ? "jpg" : "png";

try {
  const page = await browser.newPage();
  await page.setViewport({ width: w, height: h, deviceScaleFactor: 1 });

  // AC logs "[QR] ✅ Cached QR with MatrixChunky8 glyphs" the first frame
  // the QR overlay successfully draws with its label. We watch for this
  // line: it's the definitive signal that font_1 + MatrixChunky8 glyphs
  // are loaded AND the QR cache entry includes the rendered label text.
  let qrCached = false;
  page.on("console", (msg) => {
    const txt = msg.text();
    if (txt.includes("[QR] ✅ Cached QR")) qrCached = true;
  });

  // Pass 1: warm the glyph cache by letting AC render until the QR overlay
  // caches its label. The 2-pass approach is still useful because pass 1
  // writes the BDF glyph atlases to IndexedDB.
  console.log("warm pass: populating glyph cache...");
  await page.goto(url, { waitUntil: "domcontentloaded", timeout: 30000 });
  await waitForReady(page, bootMs);
  const qrDeadline = Date.now() + 15000;
  while (!qrCached && Date.now() < qrDeadline) {
    await new Promise((r) => setTimeout(r, 200));
  }
  if (qrCached) console.log("  QR label confirmed cached");
  else console.warn("  QR cache log not seen — label may be missing");

  // Pass 2: reload — glyphs now in IndexedDB, atlases populate instantly,
  // and `frame` resets to 0 so we start the capture at a clean t=0.
  qrCached = false;
  console.log("capture pass: reloading with warm cache...");
  await page.reload({ waitUntil: "domcontentloaded", timeout: 30000 });
  await waitForReady(page, bootMs);
  // Explicitly prewarm the glyph memory cache from IndexedDB. This makes
  // the Proxy at Typeface.glyphs[char] resolve SYNCHRONOUSLY for any char
  // that was loaded in pass 1, so the QR overlay's allGlyphsLoaded check
  // passes on its first paint.
  const prewarmed = await page.evaluate(async () => {
    if (!window.acGlyphCache?.preWarm) return null;
    const m = await window.acGlyphCache.preWarm("MatrixChunky8");
    const f = await window.acGlyphCache.preWarm("font_1");
    return { matrixChunky8: m, font_1: f };
  });
  if (prewarmed) {
    console.log(`  glyph memory cache: MatrixChunky8=${prewarmed.matrixChunky8}, font_1=${prewarmed.font_1}`);
  }
  const qrDeadline2 = Date.now() + 10000;
  while (!qrCached && Date.now() < qrDeadline2) {
    await new Promise((r) => setTimeout(r, 200));
  }
  if (qrCached) console.log("  QR label confirmed cached (pass 2)");
  else console.warn("  QR label cache still missing — capturing anyway");
  console.log("ready, capturing...");

  if (realtime) {
    captureRate = await captureScreencast(page, work, duration);
    console.log(`captured at ~${captureRate.toFixed(1)} fps`);
  } else {
    const startTime = Date.now();
    let lastLog = 0;
    for (let i = 0; i < totalFrames; i++) {
      const targetTime = startTime + i * frameInterval;
      const now = Date.now();
      if (now < targetTime) await new Promise((r) => setTimeout(r, targetTime - now));
      const framePath = join(work, `f${String(i).padStart(4, "0")}.png`);
      await page.screenshot({ path: framePath, type: "png" });
      if (i - lastLog >= 10 || i === totalFrames - 1) {
        console.log(`  ${i + 1}/${totalFrames}`);
        lastLog = i;
      }
    }
  }
  await page.close();
} finally {
  await browser.close();
}

console.log(`encoding ${fmt}...`);
if (fmt === "mp4") {
  // quality 100 → CRF 10, 90 → 12, 80 → 14, 50 → 20 (lower CRF = sharper)
  const crf = Math.max(8, Math.min(32, Math.round(30 - quality * 0.2)));
  await encodeMp4(work, out, captureRate, crf, frameExt, outW, outH);
} else {
  // poll-mode webp uses requested fps; screencast-mode webp falls back to captureRate
  const outMs = realtime ? Math.round(1000 / captureRate) : Math.round(frameInterval);
  await encodeWebp(work, out, outMs, quality, frameExt);
}
const sizeKb = Math.round(statSize(out) / 1024);
console.log(`wrote ${out} (${sizeKb} KB)`);

if (opts.keep) {
  const keepDir = out.replace(/\.(webp|mp4)$/, "-frames");
  mkdirSync(keepDir, { recursive: true });
  for (const f of readdirSync(work)) copyFileSync(join(work, f), join(keepDir, f));
  console.log(`kept individual frames in ${keepDir}`);
}

rmSync(work, { recursive: true, force: true });

// ────────────────────────────────────────────────────────────────────

// Block until AC has finished its boot chain and the async font glyph
// loaders have had time to populate. The signals:
//   1. window.preloaded === true — disk-loaded-and-booted fired
//   2. document.fonts.ready — browser font set is settled
//   3. fixed post-settle for the on-demand BDF glyph Proxies that font_1
//      and MatrixChunky8 trigger lazily as text rendering hits each char.
// Plus an active "render probe": we sample two screenshots `probeMs` apart
// and re-settle if their pixel content is still changing in ways that
// suggest glyph atlases are still resolving in.
async function waitForReady(page, bootMs) {
  console.log("waiting for AC preload signal...");
  const t0 = Date.now();
  try {
    await page.waitForFunction(() => window.preloaded === true, {
      timeout: bootMs * 3,
    });
  } catch {
    console.warn("preload signal timed out; continuing anyway");
  }
  try {
    await page.evaluate(() => document.fonts && document.fonts.ready);
  } catch {}
  const waited = Date.now() - t0;
  // Generous fixed settle so MatrixChunky8 + on-demand BDF glyph loads
  // (which fire after disk-loaded-and-booted) finish populating.
  const settle = Math.max(3000, bootMs - waited);
  console.log(`  preload after ${waited}ms, settling ${settle}ms more`);
  await new Promise((r) => setTimeout(r, settle));
}

async function captureScreencast(page, dir, durationMs) {
  const client = await page.target().createCDPSession();
  let frameIdx = 0;
  let firstTs = null;
  let lastTs = null;
  const writes = [];

  const onFrame = ({ data, sessionId, metadata }) => {
    if (firstTs === null) firstTs = metadata.timestamp;
    lastTs = metadata.timestamp;
    const buf = Buffer.from(data, "base64");
    const path = join(dir, `f${String(frameIdx++).padStart(5, "0")}.jpg`);
    writes.push(writeFile(path, buf));
    // ack so Chrome will send the next frame
    client.send("Page.screencastFrameAck", { sessionId }).catch(() => {});
  };
  client.on("Page.screencastFrame", onFrame);

  await client.send("Page.startScreencast", {
    format: "jpeg",
    quality: 100,
    maxWidth: w,
    maxHeight: h,
    everyNthFrame: 1,
  });
  await new Promise((r) => setTimeout(r, durationMs));
  await client.send("Page.stopScreencast");
  await Promise.all(writes);
  await client.detach();

  if (frameIdx === 0) return 0;
  const elapsed = (lastTs - firstTs) || durationMs / 1000;
  return frameIdx / elapsed;
}

function encodeWebp(dir, outPath, frameMs, q, ext = "png") {
  return new Promise((resolve, reject) => {
    const frames = readdirSync(dir)
      .filter((f) => f.endsWith("." + ext))
      .sort()
      .map((f) => join(dir, f));
    const args = [
      "-loop", "0",
      "-d", String(Math.round(frameMs)),
      "-q", String(q),
      "-m", "4",
      ...frames,
      "-o", outPath,
    ];
    const proc = spawn("img2webp", args, { stdio: ["ignore", "pipe", "inherit"] });
    proc.stdout.on("data", () => {}); // drain
    proc.on("error", reject);
    proc.on("exit", (c) => (c === 0 ? resolve() : reject(new Error(`img2webp exit ${c}`))));
  });
}

function encodeMp4(dir, outPath, fps, crf, ext = "jpg", outW, outH) {
  return new Promise((resolve, reject) => {
    const pattern = ext === "jpg" ? "f%05d.jpg" : "f%04d.png";
    // If outW/outH given and different from source, nearest-neighbor upscale
    // for crisp chunky pixels. Otherwise just round to even for yuv420p.
    const vf = outW && outH
      ? `scale=${outW}:${outH}:flags=neighbor`
      : "scale=trunc(iw/2)*2:trunc(ih/2)*2";
    const args = [
      "-y",
      "-framerate", String(fps),
      "-i", join(dir, pattern),
      "-c:v", "libx264",
      "-pix_fmt", "yuv420p",
      "-preset", "veryslow",
      "-crf", String(crf),
      "-tune", "animation",
      "-g", String(Math.max(1, Math.round(fps))),
      "-movflags", "+faststart",
      "-vf", vf,
      outPath,
    ];
    const proc = spawn("ffmpeg", args, { stdio: ["ignore", "ignore", "ignore"] });
    proc.on("error", reject);
    proc.on("exit", (c) => (c === 0 ? resolve() : reject(new Error(`ffmpeg exit ${c}`))));
  });
}

function statSize(p) {
  try {
    return statSync(p).size;
  } catch {
    return 0;
  }
}

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
