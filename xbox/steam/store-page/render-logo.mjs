// The library logo: oskiewar's wordmark alone, on transparency.
//
//   node xbox/steam/store-page/render-logo.mjs [--width=1280]
//
// The title screen never draws the wordmark by itself — fighters, motes and a
// version stamp share the frame — so keying a screenshot leaves stowaways.
// Instead the game runs offline (the snapshot.mjs trick), every comicWrite the
// title paint makes is recorded, and only the big lowercase letters of the
// word are replayed onto a transparent canvas with the same font and the same
// canvas calls the page uses. Nothing is traced or resampled.

import { readFile, writeFile } from "node:fs/promises";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = resolve(fileURLToPath(new URL(".", import.meta.url)));
const live = resolve(here, "../../live");
const repo = resolve(live, "../..");
const width = Number((process.argv.find((a) => a.startsWith("--width=")) || "--width=1280").split("=")[1]);
const source = await readFile(join(live, "oskiewar.js"), "utf8");
const viewport = { width: 1920, height: 1080 };
const noOp = () => {};
let clock = 0;
const glyphs = [];
globalThis.comicWrite = (text, x, y, size, ...ink) =>
  glyphs.push({ text: String(text), x, y, size, ink: ink.slice(0, 3) });

const game = new Function(
  "runtime", "gamepad", "capabilities", "telemetry", "gameSignal", "saveReplay",
  "publishLive", "analytics", "drum", "wipe", "box", "line", "triangle",
  "triangle3d", "triangles3d", "write", "systemWrite", "gameView",
  `${source}\nreturn { boot, sim, paint };`
)(
  () => ({ monotonicUs: clock, unixMs: 1785870000000 + Math.floor(clock / 1000),
    simCount: Math.floor(clock / 16667), paintCount: 0, clientErrorReportStatus: "" }),
  () => ({ down: [], leftX: 0, leftY: 0 }),
  () => ({ platform: "web", inputFamily: "keyboard", socialPreview: true, colorScheme: "dark" }),
  noOp, noOp, () => Promise.resolve(true), noOp, noOp, noOp,
  noOp, noOp, noOp, noOp, undefined, undefined, noOp, noOp, () => viewport,
);
game.boot();
for (let step = 0; step < 30; step++) { clock += 16667; game.sim(); }
game.paint();

const word = new Set("oskiewar");
const letters = glyphs.filter((g) => g.text.length === 1 && word.has(g.text) && g.size >= 100);
if (letters.length < 8) throw new Error(`expected the wordmark, recorded ${letters.length} letters`);
const left = Math.min(...letters.map((g) => g.x)) - 12;
const top = Math.min(...letters.map((g) => g.y)) - 12;
const right = Math.max(...letters.map((g) => g.x + g.size * .75)) + 12;
const bottom = Math.max(...letters.map((g) => g.y + g.size * .98)) + 12;
const scale = width / (right - left);
const height = Math.round((bottom - top) * scale);
console.log(`✍️  ${letters.length} glyph draws, band ${Math.round(right - left)}×${Math.round(bottom - top)} → ${width}×${height}`);

const font = (await readFile(join(repo,
  "system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.woff2"))).toString("base64");
const html = `<!doctype html><meta charset="utf-8"><style>
@font-face { font-family: "Comic Relief"; src: url(data:font/woff2;base64,${font}) format("woff2"); }
html, body { margin: 0; background: transparent; }
</style><canvas id="c" width="${width}" height="${height}"></canvas>
<script>
const glyphs = ${JSON.stringify(letters)};
document.fonts.load('200px "Comic Relief"').then(() => {
  const context = document.getElementById("c").getContext("2d");
  context.scale(${scale}, ${scale});
  context.translate(${-left}, ${-top});
  context.textBaseline = "top";
  for (const g of glyphs) {
    context.fillStyle = "rgb(" + g.ink.join(",") + ")";
    context.font = g.size + 'px "Comic Relief"';
    context.fillText(g.text, g.x, g.y);
  }
  document.title = "ready";
});
</script>`;
const page_ = join(here, "assets", "shots", "_logo.html");
await writeFile(page_, html);

const chrome = process.env.PUPPETEER_EXECUTABLE_PATH ||
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const { default: puppeteer } = await import("puppeteer");
const browser = await puppeteer.launch({ headless: true, executablePath: chrome });
try {
  const page = await browser.newPage();
  await page.setViewport({ width, height, deviceScaleFactor: 1 });
  await page.goto(`file://${page_}`);
  await page.waitForFunction(() => document.title === "ready");
  await page.screenshot({ path: join(here, "assets", "library-logo.png"), omitBackground: true });
  console.log(`🖼  library-logo.png ${width}×${height}, transparent`);
} finally { await browser.close(); }
