#!/usr/bin/env node
// screenshots.mjs — pre-bake artifact screenshots for the recap.
// For each segment with an `artifact` (or any query whose shape includes
// a `screenshot:` URL), fetch the URL via puppeteer at the requested
// viewport / clip and cache the PNG to recap/out/screenshots/<seg>-<name>.png.
// scout.mjs reads those cached files at slide-build time.
//
// Mirrors the `jeffrey-photos.mjs` cache pattern — re-runs are cheap, and
// `--force` re-fetches everything (or `--only <segName>` re-fetches one).
//
// Query shape (in `audience.slides[seg].queries[name]`):
//   { screenshot: "<URL>",        // required
//     width: 1280, height: 800,   // viewport (defaults 1280x800)
//     clip:  { x, y, width, height }, // optional final crop
//     waitMs: 800,                // optional settle pause after load
//     scrollTo: { x, y },         // optional scroll before screenshot
//     selector: "#some-id"        // optional: screenshot ONLY this element
//   }
//
// Usage:
//   node bin/screenshots.mjs jeffrey-73h-2026-05-02
//   node bin/screenshots.mjs jeffrey-73h-2026-05-02 --force
//   node bin/screenshots.mjs jeffrey-73h-2026-05-02 --only 02_menuband_arc

import { mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

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
const audienceName = process.argv[2] || "jeffrey-24h";
const force = process.argv.includes("--force");
const onlyIdx = process.argv.indexOf("--only");
const only = onlyIdx >= 0 ? process.argv[onlyIdx + 1] : null;

const { audience } = await import(`${ROOT}/audience/${audienceName}.mjs`);

const SHOTS_DIR = `${ROOT}/out/screenshots`;
mkdirSync(SHOTS_DIR, { recursive: true });

// Find every (segName, queryName, query) triple where query has a screenshot URL.
const targets = [];
for (const seg of audience.segments) {
  if (only && seg.name !== only) continue;
  const slide = audience.slides[seg.name];
  if (!slide || typeof slide !== "object" || !slide.queries) continue;
  for (const [name, q] of Object.entries(slide.queries)) {
    if (q && typeof q === "object" && q.screenshot) {
      targets.push({ segName: seg.name, queryName: name, q });
    }
  }
}

if (!targets.length) {
  console.log("(no screenshot queries — nothing to do)");
  process.exit(0);
}

console.log(`▸ ${targets.length} screenshot target(s)`);

const browser = await puppeteer.launch({
  headless: true,
  args: ["--no-sandbox"],
});

let fetched = 0, cached = 0, failed = 0;
for (const { segName, queryName, q } of targets) {
  const outPath = `${SHOTS_DIR}/${segName}-${queryName}.png`;
  if (existsSync(outPath) && !force) {
    console.log(`  · cached: ${segName}/${queryName}`);
    cached++;
    continue;
  }

  const page = await browser.newPage();
  const w = q.width || 1280;
  const h = q.height || 800;
  await page.setViewport({ width: w, height: h, deviceScaleFactor: 2 });

  try {
    console.log(`  → fetching: ${segName}/${queryName} ${q.screenshot}`);
    await page.goto(q.screenshot, { waitUntil: "networkidle2", timeout: 30000 });
    if (q.scrollTo) {
      await page.evaluate(({ x, y }) => window.scrollTo(x, y), q.scrollTo);
    }
    if (q.waitMs) {
      await new Promise((r) => setTimeout(r, q.waitMs));
    } else {
      await new Promise((r) => setTimeout(r, 600)); // default settle
    }

    let buf;
    if (q.selector) {
      const el = await page.$(q.selector);
      if (!el) throw new Error(`selector '${q.selector}' not found`);
      buf = await el.screenshot({ type: "png" });
    } else if (q.clip) {
      buf = await page.screenshot({ type: "png", clip: q.clip });
    } else {
      buf = await page.screenshot({ type: "png", fullPage: false });
    }

    const { writeFileSync } = await import("node:fs");
    writeFileSync(outPath, buf);
    console.log(`  ✓ ${outPath.replace(ROOT + "/", "")} (${(buf.length / 1024).toFixed(0)} KB)`);
    fetched++;
  } catch (e) {
    console.error(`  ✗ ${segName}/${queryName}: ${e.message}`);
    failed++;
  }
  await page.close();
}

await browser.close();
console.log(`✓ done · fetched ${fetched} · cached ${cached} · failed ${failed}`);
