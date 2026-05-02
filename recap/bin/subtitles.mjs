#!/usr/bin/env node
// subtitles.mjs — group out/words.json into readable phrase chunks (~5 words,
// breaking on long pauses or punctuation), render each as a 1080×220
// transparent PNG with a pill backdrop, and write out/subs.json:
//   [{file, startSec, endSec, text}, ...]
// Compose.fish overlays each PNG at its time window using ffmpeg's `movie`
// + `overlay enable=between(t,a,b)` chain.
// Usage: node bin/subtitles.mjs

import { mkdirSync, writeFileSync, readFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(HERE, "../..");

// Resolve puppeteer from one of the known node_modules locations.
const __PUPPETEER_DIR = [
  resolve(HERE, "../../oven/node_modules/puppeteer"),
  "/opt/oven/node_modules/puppeteer",
  resolve(HERE, "../node_modules/puppeteer"),
].find((p) => existsSync(p));
if (!__PUPPETEER_DIR) {
  throw new Error("puppeteer not found in any known node_modules location");
}
const puppeteer = (await import(`${__PUPPETEER_DIR}/lib/esm/puppeteer/puppeteer.js`)).default;
const SUB_DIR = `${ROOT}/out/subs`;
mkdirSync(SUB_DIR, { recursive: true });

const audienceName = process.argv[2] || "fia";
const { audience } = await import(`${ROOT}/audience/${audienceName}.mjs`);
const fixes = audience.transcriptFixes || {};
function escapeRegex(s) { return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"); }
function applyFixes(text) {
  let out = text;
  for (const [find, replace] of Object.entries(fixes)) {
    out = out.replace(new RegExp(escapeRegex(find), "gi"), replace);
  }
  return out;
}

const FONT_BOLD = `${REPO}/system/public/type/webfonts/ywft-processing-bold.ttf`;
const fontBoldB64 = readFileSync(FONT_BOLD).toString("base64");

const words = JSON.parse(readFileSync(`${ROOT}/out/words.json`, "utf8"));

const MAX_WORDS = 6;
const MAX_GAP_MS = 380; // pause longer than this triggers a chunk break
const SENTENCE_END = /[.!?,—]$/;

const chunks = [];
let cur = [];
let curStart = 0;
for (let i = 0; i < words.length; i++) {
  const w = words[i];
  if (cur.length === 0) curStart = w.fromMs;
  cur.push(w);

  const next = words[i + 1];
  const reachedMax = cur.length >= MAX_WORDS;
  const longPause = next && next.fromMs - w.toMs > MAX_GAP_MS;
  const sentenceEnd = SENTENCE_END.test(w.text.trim()) && cur.length >= 3;
  const isLast = !next;

  if (reachedMax || longPause || sentenceEnd || isLast) {
    chunks.push({
      startMs: curStart,
      endMs: next ? next.fromMs : w.toMs,
      text: applyFixes(cur.map((x) => x.text.trim()).join(" ")),
    });
    cur = [];
  }
}

// Full-frame PNGs (1080×1920, transparent except for the pill at y=1690).
// This replaces the older 1080×220 strip — by baking each subtitle into a
// full-frame transparent PNG, the compose step can stitch them via the
// concat demuxer (one timed PNG sequence) and overlay them as a single
// video stream, eliminating the 135-deep movie= filter chain that
// bottlenecked the oven encode (see feedback_recap_subtitles_required.md).
const FRAME_W = 1080;
const FRAME_H = 1920;
const PILL_Y_TOP = 1690; // matches the SUB_Y in build-filter.mjs

const cssTemplate = `
@font-face {
  font-family: 'ProcessingB';
  src: url(data:font/ttf;base64,${fontBoldB64}) format('truetype');
  unicode-range: U+0020-007E;
}
* { box-sizing: border-box; margin: 0; padding: 0; }
html, body { width: ${FRAME_W}px; height: ${FRAME_H}px; background: transparent; -webkit-font-smoothing: antialiased; }
.wrap {
  position: absolute;
  left: 0;
  top: ${PILL_Y_TOP}px;
  width: ${FRAME_W}px;
  height: 220px;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 0 60px;
}
.pill {
  background: rgba(16, 8, 32, 0.72);
  backdrop-filter: blur(2px);
  border: 3px solid rgba(255, 105, 180, 0.55);
  border-radius: 14px;
  padding: 26px 44px;
  max-width: 100%;
  text-align: center;
  font-family: 'ProcessingB', 'Helvetica Neue', 'Hiragino Sans', monospace;
  font-size: 64px;
  line-height: 1.15;
  color: #fcf7c5;
  letter-spacing: -1px;
  text-shadow: 0 2px 0 rgba(0,0,0,0.5);
  word-wrap: break-word;
}
.pill em { font-style: normal; color: #ff70d0; }
`;

const browser = await puppeteer.launch({
  // Use puppeteer's bundled Chromium — works on local Mac + oven Linux.
  args: ["--no-sandbox"],
});

const out = [];
for (let i = 0; i < chunks.length; i++) {
  const c = chunks[i];
  const file = `${SUB_DIR}/${String(i).padStart(3, "0")}.png`;
  const page = await browser.newPage();
  await page.setViewport({ width: FRAME_W, height: FRAME_H, deviceScaleFactor: 1 });
  const html = `<!doctype html><html><head><meta charset="utf-8"><style>${cssTemplate}</style></head><body><div class="wrap"><div class="pill">${escapeHtml(c.text)}</div></div></body></html>`;
  await page.setContent(html, { waitUntil: "domcontentloaded" });
  await new Promise((r) => setTimeout(r, 80));
  const png = await page.screenshot({ type: "png", omitBackground: true });
  writeFileSync(file, png);
  await page.close();
  out.push({
    file,
    startSec: +(c.startMs / 1000).toFixed(3),
    endSec: +(c.endMs / 1000).toFixed(3),
    text: c.text,
  });
}

// Render a single fully-transparent blank frame the concat demuxer can use
// for gaps between subtitles. One file, reused for every gap entry.
{
  const blankPath = `${SUB_DIR}/blank.png`;
  const page = await browser.newPage();
  await page.setViewport({ width: FRAME_W, height: FRAME_H, deviceScaleFactor: 1 });
  await page.setContent(`<!doctype html><html><body style="margin:0;background:transparent;"></body></html>`, { waitUntil: "domcontentloaded" });
  const blank = await page.screenshot({ type: "png", omitBackground: true });
  writeFileSync(blankPath, blank);
  await page.close();
}
await browser.close();

writeFileSync(`${ROOT}/out/subs.json`, JSON.stringify(out, null, 2));
console.log(`✓ ${out.length} subtitle chunks → ${SUB_DIR}/ (full-frame ${FRAME_W}×${FRAME_H})`);
for (const s of out.slice(0, 5)) console.log(`  ${s.startSec.toFixed(2)}-${s.endSec.toFixed(2)}  "${s.text}"`);
if (out.length > 5) console.log(`  ... (+${out.length - 5} more)`);

function escapeHtml(s) {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}
