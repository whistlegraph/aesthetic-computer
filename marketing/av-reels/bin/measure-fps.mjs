#!/usr/bin/env node
// measure-fps.mjs — certify a PAD's real render fps, headless, WITHOUT writing
// frames (disk-safe). lib/pads.mjs logs `[pads:fps] <fps> q=<quality> work=<ms>`
// from the render worker (~every 120 frames); this probe reads those console
// lines over a window and reports avg / min / and the settled quality.
//
//   node marketing/av-reels/bin/measure-fps.mjs prism --base http://localhost:8899
//   node .../measure-fps.mjs molten --base http://localhost:8899 --secs 12 --density 1
//
// Flags: --base URL  --secs N (sample window after warmup, default 10)
//        --warmup N (default 3)  --density N (default 1 = native, hardest)  --w --h
import { existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..", "..");
const argv = process.argv.slice(2);
const flags = {};
let PIECE = null;
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) flags[a.slice(2)] = argv[i + 1]?.startsWith("--") || i + 1 >= argv.length ? true : argv[++i];
  else if (!PIECE) PIECE = a;
}
if (!PIECE) { console.error("usage: measure-fps.mjs '<piece>' [--base URL] [--secs 10] [--density 1]"); process.exit(1); }

const BASE = flags.base || "https://aesthetic.computer";
const SECS = parseFloat(flags.secs || "10");
const WARMUP = parseFloat(flags.warmup || "3");
const DENSITY = flags.density || "1";
const W = parseInt(flags.w || "432", 10);
const H = parseInt(flags.h || "768", 10);

const PUPPETEER_DIR = [
  `${REPO}/node_modules/puppeteer`, `${REPO}/oven/node_modules/puppeteer`, "/opt/oven/node_modules/puppeteer",
].find((p) => existsSync(p));
if (!PUPPETEER_DIR) throw new Error("puppeteer not found");
const puppeteer = (await import(`${PUPPETEER_DIR}/lib/esm/puppeteer/puppeteer.js`)).default;
const CHROME = [
  process.env.PUPPETEER_EXECUTABLE_PATH,
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
].find((p) => p && existsSync(p));

const url = `${BASE}/${encodeURIComponent(PIECE)}?nolabel&nogap&density=${DENSITY}`;
const browser = await puppeteer.launch({
  headless: "new",
  ...(CHROME ? { executablePath: CHROME } : {}),
  args: ["--no-sandbox", "--autoplay-policy=no-user-gesture-required",
    "--use-gl=angle", "--use-angle=metal", "--enable-gpu", "--ignore-gpu-blocklist",
    "--disable-background-timer-throttling", "--disable-backgrounding-occluded-windows",
    "--disable-renderer-backgrounding", "--disable-features=CalculateNativeWinOcclusion",
    `--window-size=${W},${H}`],
});
const page = await browser.newPage();
await page.setViewport({ width: W, height: H, deviceScaleFactor: 1 });
let pageErr = 0;
page.on("pageerror", () => pageErr++);

let collecting = false;
const fps = [];
const qual = [];
const RE = /\[pads:fps\]\s+([\d.]+)\s+q=([\d.]+)/;
const onLine = (t) => {
  const m = RE.exec(t);
  if (m && collecting) { fps.push(parseFloat(m[1])); qual.push(parseFloat(m[2])); }
};
page.on("console", (m) => onLine(m.text()));
// Worker console may arrive on a worker target rather than the page.
page.on("workercreated", (w) => w.on("console", (m) => onLine(m.text())));

try { await page.goto(url, { waitUntil: "networkidle2", timeout: 45000 }); }
catch (e) { console.log(`⚠ goto: ${e.message.slice(0, 70)}`); }

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
await sleep(WARMUP * 1000);
collecting = true;
await sleep(SECS * 1000);
await browser.close();

if (fps.length === 0) {
  console.log(`${PIECE.padEnd(12)} ⚠ no [pads:fps] samples (not a pads.mjs pad? or console not surfaced) · errs=${pageErr}`);
  process.exit(0);
}
fps.sort((a, b) => a - b);
const avg = fps.reduce((a, b) => a + b, 0) / fps.length;
const min = fps[0];
const q = qual.length ? qual[qual.length - 1] : 1; // settled quality
const cert = min >= 58 ? "✅ 60fps" : min >= 45 ? "⚠ 45-60" : "❌ <45";
console.log(
  `${PIECE.padEnd(12)} ${cert}  avg=${avg.toFixed(0)} min=${min.toFixed(0)} fps` +
  `  q=${q.toFixed(2)}  n=${fps.length}  density=${DENSITY}  errs=${pageErr}`,
);
