// 🎪 laklok sisters — visual comparison suite for the two Laer Klokken chat
// interfaces: the AC "raster" piece (disks/laklok.mjs) and the "vector" HTML
// client (system/public/html/index.html).
//
// It renders both surfaces in every theme (plus the media-links filter state),
// screenshots them with ONE headless Chrome (sequentially — 8GB machines),
// and writes a side-by-side report so the sisters can be eyeballed together.
//
// Usage:
//   node toolchain/laklok-sisters/sisters.mjs                 # against production
//   node toolchain/laklok-sisters/sisters.mjs --local         # against localhost:8888
//   node toolchain/laklok-sisters/sisters.mjs --state nat     # only one state
//
// Output: toolchain/laklok-sisters/out/{<state>-raster.png, <state>-vector.png,
// report.html}. Open the report and fail anything that doesn't read as kin.

import puppeteer from "puppeteer";
import { mkdir, writeFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const outDir = join(here, "out");

const args = process.argv.slice(2);
const local = args.includes("--local");
const onlyState = args.includes("--state")
  ? args[args.indexOf("--state") + 1]
  : null;

// Local dev serves https with a self-signed cert (Caddy) — the launch args
// below ignore certificate errors so --local runs work.
const RASTER_BASE = local ? "https://localhost:8888" : "https://aesthetic.computer";
// (netlify dev hangs on the bare /html/ directory path; the explicit
// index.html resolves instantly and is byte-identical to production's /html/.)
const VECTOR_BASE = local
  ? "https://localhost:8888/html/index.html"
  : "https://laklok.com/html/";

// One state per theme, plus the links filter over the default theme. Raster
// takes colon params (`laklok:nat:links`), vector takes query params — both
// were added for exactly this suite.
const STATES = [
  { name: "ler", raster: "laklok:ler", vector: "?theme=ler" },
  { name: "nat", raster: "laklok:nat", vector: "?theme=nat" },
  { name: "skov", raster: "laklok:skov", vector: "?theme=skov" },
  { name: "lakrids", raster: "laklok:lakrids", vector: "?theme=lakrids" },
  { name: "ler-links", raster: "laklok:ler:links", vector: "?theme=ler&links=1" },
].filter((s) => !onlyState || s.name === onlyState);

const VIEWPORT = { width: 390, height: 844, deviceScaleFactor: 2 };
const RASTER_SETTLE_MS = 9000; // AC runtime boot + chat connect + first paint
const VECTOR_SETTLE_MS = 2500; // history fetch + ws connect

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function shoot(page, url, file, settleMs) {
  console.log(`  📸 ${url}`);
  // The AC runtime keeps sockets open (module loader, chat), so networkidle
  // never fires — settle on a fixed delay after DOM load instead.
  await page.goto(url, { waitUntil: "domcontentloaded", timeout: 60000 });
  await sleep(settleMs);
  await page.screenshot({ path: file });
}

async function main() {
  await mkdir(outDir, { recursive: true });
  // Prefer the system Chrome — puppeteer's bundled build isn't downloaded on
  // every machine (PUPPETEER_EXECUTABLE_PATH overrides; bundled is the
  // fallback).
  const macChrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
  const executablePath =
    process.env.PUPPETEER_EXECUTABLE_PATH ||
    (existsSync(macChrome) ? macChrome : undefined);
  const browser = await puppeteer.launch({
    executablePath,
    headless: "new",
    args: [
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--mute-audio",
      "--ignore-certificate-errors",
    ],
  });

  const rows = [];
  try {
    const page = await browser.newPage();
    await page.setViewport(VIEWPORT);
    for (const state of STATES) {
      console.log(`🎪 ${state.name}`);
      const rasterFile = join(outDir, `${state.name}-raster.png`);
      const vectorFile = join(outDir, `${state.name}-vector.png`);
      await shoot(page, `${RASTER_BASE}/${state.raster}`, rasterFile, RASTER_SETTLE_MS);
      await shoot(page, `${VECTOR_BASE}${state.vector}`, vectorFile, VECTOR_SETTLE_MS);
      rows.push(state.name);
    }
  } finally {
    await browser.close(); // never leave headless orphans behind
  }

  const stamp = new Date().toISOString();
  const report = `<!doctype html>
<meta charset="utf-8">
<title>laklok sisters — ${stamp}</title>
<style>
  body { background: #191419; color: #f0eef4; font: 14px ui-monospace, monospace; margin: 20px; }
  h1 { font-size: 18px; } h2 { font-size: 14px; margin: 28px 0 8px; color: #ffd24b; }
  .pair { display: flex; gap: 12px; align-items: flex-start; }
  .pair figure { margin: 0; }
  .pair img { width: 390px; image-rendering: pixelated; border: 1px solid #444; }
  figcaption { opacity: .7; padding: 4px 0; }
</style>
<h1>🎪 laklok sisters — raster vs vector (${stamp})</h1>
<p>Same theme, same filter, two mediums. They should read as kin, not clones.</p>
${rows
  .map(
    (name) => `<h2>${name}</h2>
<div class="pair">
  <figure><img src="${name}-raster.png"><figcaption>raster — disks/laklok.mjs</figcaption></figure>
  <figure><img src="${name}-vector.png"><figcaption>vector — /html/</figcaption></figure>
</div>`,
  )
  .join("\n")}
`;
  await writeFile(join(outDir, "report.html"), report);
  console.log(`\n✅ ${rows.length} state(s) → ${join(outDir, "report.html")}`);
}

main().catch((err) => {
  console.error("❌ sisters failed:", err);
  process.exit(1);
});
