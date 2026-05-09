#!/usr/bin/env node
// slides.mjs — render audience slides as 1080x1920 PNGs and write
// out/concat.txt with per-slide durations from out/segments.json.
// Usage: node bin/slides.mjs [audience-name]

import { mkdirSync, writeFileSync, readFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

// Resolve puppeteer from one of the known node_modules locations. Local dev
// uses ../../oven/node_modules; oven prod uses /opt/oven/node_modules.
const __HERE = dirname(fileURLToPath(import.meta.url));
const __PUPPETEER_DIR = [
  resolve(__HERE, "../../oven/node_modules/puppeteer"),
  "/opt/oven/node_modules/puppeteer",
  resolve(__HERE, "../node_modules/puppeteer"),
].find((p) => existsSync(p));
if (!__PUPPETEER_DIR) {
  throw new Error("puppeteer not found in any known node_modules location");
}
const puppeteer = (await import(`${__PUPPETEER_DIR}/lib/esm/puppeteer/puppeteer.js`)).default;

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const audienceName = process.argv[2] || "fia";
const { audience, PALETTE } = await import(`${ROOT}/audience/${audienceName}.mjs`);

const SLIDE_DIR = `${ROOT}/out/slides`;
mkdirSync(SLIDE_DIR, { recursive: true });

const REPO = resolve(HERE, "../..");
const FONT_BOLD = `${REPO}/system/public/type/webfonts/ywft-processing-bold.ttf`;
const FONT_REG = `${REPO}/system/public/type/webfonts/ywft-processing-regular.ttf`;
const PALS_SVG = `${REPO}/system/public/purple-pals.svg`;

const fontBoldB64 = readFileSync(FONT_BOLD).toString("base64");
const fontRegB64 = readFileSync(FONT_REG).toString("base64");
const palsSvgB64 = Buffer.from(readFileSync(PALS_SVG, "utf8"), "utf8").toString("base64");

const segments = JSON.parse(readFileSync(`${ROOT}/out/segments.json`, "utf8"));
const assets = (() => {
  try { return JSON.parse(readFileSync(`${ROOT}/out/assets.json`, "utf8")); }
  catch { return {}; }
})();

const cssTemplate = `
@font-face {
  font-family: 'ProcessingB';
  src: url(data:font/ttf;base64,${fontBoldB64}) format('truetype');
  unicode-range: U+0020-007E;
}
@font-face {
  font-family: 'ProcessingR';
  src: url(data:font/ttf;base64,${fontRegB64}) format('truetype');
  unicode-range: U+0020-007E;
}
* { box-sizing: border-box; margin: 0; padding: 0; }
html, body {
  width: 1080px;
  height: 1920px;
  font-family: 'ProcessingR', 'Helvetica Neue', 'Hiragino Sans', monospace;
  -webkit-font-smoothing: antialiased;
}
body { background: ${PALETTE.bg}; padding: 80px 70px; position: relative; overflow: hidden; }
.frame { width: 100%; height: 100%; display: flex; flex-direction: column; gap: 28px; position: relative; }
.pals {
  background-image: url(data:image/svg+xml;base64,${palsSvgB64});
  background-size: contain; background-repeat: no-repeat; background-position: center;
}
.pals.big { width: 720px; height: 720px; align-self: center; margin-top: 60px; }
.pals.med { width: 480px; height: 480px; align-self: center; margin-top: 80px; }
.pals.bug {
  position: absolute;
  top: 30px;
  left: 30px;
  width: 200px;
  height: 200px;
  opacity: 1;
  /* Rainbow drop shadow — multi-step layered shadows in cycling hues
   * give a soft chromatic aberration / risograph misregister feel. */
  filter:
    drop-shadow(3px  3px 0 rgba(255, 80, 180, 0.85))
    drop-shadow(-3px 3px 0 rgba(80, 200, 255, 0.85))
    drop-shadow(0 -3px 0 rgba(255, 220, 80, 0.85))
    drop-shadow(6px  0  0 rgba(140, 80, 255, 0.55))
    drop-shadow(-6px 0  0 rgba(80, 255, 160, 0.55))
    drop-shadow(0 8px 22px rgba(0, 0, 0, 0.5));
}
/* PALS bug — top-LEFT corner, large with rainbow drop shadow. Top-right
 * is owned by the QR code (audience photoSlide renders that). */
.kicker { font-family: 'ProcessingB'; font-size: 38px; letter-spacing: 8px; text-transform: uppercase; text-align: center; }
.huge { font-family: 'ProcessingB'; font-size: 240px; letter-spacing: -8px; text-align: center; line-height: 0.95; margin-top: 10px; }
.sub { font-family: 'ProcessingB'; font-size: 46px; letter-spacing: 2px; text-align: center; margin-top: 24px; }
.datestamp { position: absolute; bottom: 0; left: 0; right: 0; text-align: center; font-family: 'ProcessingR'; font-size: 30px; letter-spacing: 4px; }
.chapter { font-family: 'ProcessingR'; font-size: 30px; letter-spacing: 6px; text-transform: uppercase; }
.title { font-family: 'ProcessingB'; font-size: 130px; letter-spacing: -3px; line-height: 1.0; margin-top: 6px; }
.body { font-family: 'ProcessingR'; font-size: 56px; line-height: 1.32; margin-top: 32px; }
.body em { font-style: normal; color: ${PALETTE.magenta}; }
.body.small { font-size: 44px; }
.cap { font-family: 'ProcessingB'; font-size: 44px; margin-top: auto; padding-top: 30px; }
.commits { margin-top: 36px; display: flex; flex-direction: column; gap: 14px; }
.commit { font-family: 'ProcessingR'; font-size: 30px; color: ${PALETTE.off}; line-height: 1.3; }
.commit .hash { display: inline-block; width: 90px; color: ${PALETTE.lime}; font-family: 'ProcessingB'; }
.paper-card { margin-top: 50px; padding: 50px 40px; border: 4px solid ${PALETTE.cyan}; border-radius: 6px; background: rgba(112, 240, 224, 0.06); }
.paper-title { font-family: 'ProcessingB'; font-size: 74px; color: ${PALETTE.cyan}; letter-spacing: -1px; }
.paper-sub { font-family: 'ProcessingR'; font-size: 38px; color: ${PALETTE.cream}; margin-top: 18px; }
.paper-meta { font-family: 'ProcessingR'; font-size: 28px; color: ${PALETTE.dim}; margin-top: 14px; letter-spacing: 2px; }
.lang-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 32px; margin-top: 50px; }
.lang { font-family: 'ProcessingB'; font-size: 86px; color: ${PALETTE.cream}; padding: 36px 30px; border: 3px solid rgba(255,255,255,0.15); border-radius: 4px; display: flex; align-items: baseline; gap: 22px; }
.lang .flag { font-family: 'ProcessingB'; font-size: 60px; letter-spacing: -2px; }
.ticker { margin-top: 40px; display: flex; flex-direction: column; gap: 18px; padding: 30px 36px; background: rgba(255, 216, 96, 0.07); border-left: 6px solid ${PALETTE.yellow}; }
.tick { font-family: 'ProcessingR'; font-size: 32px; color: ${PALETTE.cream}; }
.rhyme { margin-top: 220px; text-align: center; display: flex; flex-direction: column; gap: 30px; }
.rhyme .line1, .rhyme .line2 { font-family: 'ProcessingB'; font-size: 92px; line-height: 1.05; }
.rhyme .line2 { margin-top: 60px; }
.endline { font-family: 'ProcessingB'; font-size: 92px; letter-spacing: -2px; text-align: center; margin-top: 40px; }
.endsub { font-family: 'ProcessingR'; font-size: 36px; letter-spacing: 4px; text-align: center; margin-top: 14px; }
/* .cornerbug retired — was overlapping the subtitle pill in the new
 * bottom-chrome layout. The PALS bug + chapter prompt stack handle
 * branding now (rendered in audience photoSlide). */

/* asset-driven slide additions */
.title-row { display: flex; align-items: center; gap: 24px; }
.brand-icon { width: 110px; height: 110px; border-radius: 18px; flex-shrink: 0; }
.row-with-aside { display: flex; gap: 30px; margin-top: 36px; align-items: flex-start; }
.row-with-aside .commits { flex: 1; margin-top: 0; }
.paper-thumb { width: 280px; border: 3px solid ${PALETTE.cyan}; border-radius: 6px; box-shadow: 0 6px 30px rgba(0,0,0,0.4); }
.cover-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 28px; margin-top: 40px; }
.cover { display: flex; flex-direction: column; gap: 14px; align-items: center; }
.cover-img { width: 100%; max-width: 360px; border-radius: 6px; box-shadow: 0 4px 20px rgba(0,0,0,0.5); }
.cover-img.placeholder { aspect-ratio: 0.71; background: rgba(255,255,255,0.06); border: 2px dashed rgba(255,255,255,0.2); }
.cover-label { font-family: 'ProcessingB'; font-size: 38px; color: ${PALETTE.cream}; }
.cover-label span { font-size: 32px; margin-right: 10px; letter-spacing: -1px; }
.pdf-strip { margin-top: 30px; display: flex; flex-wrap: wrap; gap: 12px; }
.pdf-chip { font-family: 'ProcessingR'; font-size: 24px; color: ${PALETTE.cream}; padding: 10px 18px; background: rgba(255, 216, 96, 0.1); border: 2px solid rgba(255, 216, 96, 0.3); border-radius: 4px; }
.tick .hash { color: ${PALETTE.lime}; font-family: 'ProcessingB'; margin-right: 14px; }
`;

const browser = await puppeteer.launch({
  // Use puppeteer's bundled Chromium — works on both local Mac (uses
  // ~/.cache/puppeteer/...) and oven Linux (same).
  args: ["--no-sandbox"],
});

const slidesOrder = audience.segments.map((s) => s.name);
const showBug = (name) => name !== slidesOrder[0] && name !== slidesOrder[slidesOrder.length - 1];

for (const name of slidesOrder) {
  const slide = audience.slides[name];
  if (!slide) {
    console.error(`✗ no slide HTML for segment "${name}" in audience.slides`);
    process.exit(1);
  }
  // Slide can be a string or an object { body, queries }; body can also be a
  // function (assets) => string for query-driven slides.
  const slideAssets = assets[name] || {};
  let body;
  if (typeof slide === "string") body = slide;
  else if (typeof slide.body === "function") body = slide.body(slideAssets);
  else body = slide.body;
  const page = await browser.newPage();
  await page.setViewport({ width: 1080, height: 1920, deviceScaleFactor: 1 });
  const html = `<!doctype html><html><head><meta charset="utf-8"><style>${cssTemplate}</style></head><body>${body}
    ${/* PALS bug + cornerbug are rendered inside audience photoSlide() now
        so they can carry per-slide chapter color (drop shadow, scrim).
        Title + end slide skip them via their own templates. */ ""}
  </body></html>`;
  // `domcontentloaded` instead of `networkidle0` — with multi-MB base64
  // photos embedded inline, networkidle0 sometimes hangs waiting for the
  // browser to settle. domcontentloaded fires once the DOM is parsed,
  // which is all we need before screenshotting (data URLs render
  // synchronously). 90s timeout for safety on slow oven CPU.
  await page.setContent(html, { waitUntil: "domcontentloaded", timeout: 90000 });
  await new Promise((r) => setTimeout(r, 400));
  await new Promise((r) => setTimeout(r, 200));
  const png = await page.screenshot({ type: "png", omitBackground: false });
  writeFileSync(`${SLIDE_DIR}/${name}.png`, png);
  await page.close();
  const seg = segments.find((s) => s.name === name);
  console.log(`✓ ${name}.png · ${seg.durationSec.toFixed(2)}s (${seg.startSec}s → ${seg.endSec}s)`);
}
await browser.close();

// concat.txt with real durations
const lines = [];
for (const seg of segments) {
  lines.push(`file '${SLIDE_DIR}/${seg.name}.png'`);
  lines.push(`duration ${seg.durationSec}`);
}
// concat demuxer needs the last file repeated without duration for proper end
lines.push(`file '${SLIDE_DIR}/${segments[segments.length - 1].name}.png'`);
writeFileSync(`${ROOT}/out/concat.txt`, lines.join("\n") + "\n");

const total = segments[segments.length - 1].endSec;
writeFileSync(`${ROOT}/out/duration.txt`, String(total));
console.log(`✓ ${ROOT}/out/concat.txt · total ${total}s`);
