#!/usr/bin/env node
// daily-artifact-check.mjs — watch a daily bundle run the way objkt holds it.
//
// Serves the bundle to a sandboxed iframe (allow-scripts, opaque origin) and
// aborts every other request, then screenshots it for a few seconds: it
// passes when the crawl is lit and keeps repainting. This is the headless
// half of the gate; daily-token.mjs runs the static half (checkBundle) itself,
// since jasellite has no Chrome. Needs puppeteer (oven/node_modules or root).
//
//   node bin/daily-artifact-check.mjs out/daily/daily-2026-09-29.html [--shots dir]

import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";
import { createRequire } from "node:module";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..", "..");
const need = (m) => {
  for (const from of ["oven", "."]) {
    try { return createRequire(resolve(REPO, from, "package.json"))(m); } catch {}
  }
  throw new Error(`${m} isn't installed (npm ci --prefix oven)`);
};
const puppeteer = need("puppeteer"), sharp = need("sharp");

const [file, ...rest] = process.argv.slice(2);
if (!file) { console.error("usage: daily-artifact-check.mjs <bundle.html> [--shots dir]"); process.exit(2); }
const shotsDir = rest.includes("--shots") ? rest[rest.indexOf("--shots") + 1] : null;
const bundle = readFileSync(file);
const ART = "https://art.test/index.html", HOST = "https://host.test/";
const host = `<!doctype html><body style="margin:0;background:#222"><iframe sandbox="allow-scripts" src="${ART}" style="border:0;width:512px;height:512px"></iframe>`;

const browser = await puppeteer.launch({ headless: "new" });
const page = await browser.newPage();
await page.setViewport({ width: 512, height: 512 });
await page.setRequestInterception(true);
const leaked = [];
page.on("request", (r) => {
  const u = r.url();
  if (u === ART) return r.respond({ status: 200, contentType: "text/html", body: bundle });
  if (u === HOST) return r.respond({ status: 200, contentType: "text/html", body: host });
  if (u.startsWith("blob:") || u.startsWith("data:")) return r.continue();
  if (!u.endsWith("/favicon.ico")) leaked.push(u);
  r.abort();
});
await page.goto(HOST);
await new Promise((r) => setTimeout(r, 5000));

const frames = new Set();
let shots = 0, lit = 0;
const t0 = Date.now();
while (Date.now() - t0 < 4000) {
  const png = await page.screenshot();
  const { data } = await sharp(png).raw().toBuffer({ resolveWithObject: true });
  frames.add(createHash("md5").update(data).digest("hex"));
  let l = 0;
  for (let i = 0; i < data.length; i += 3) if (data[i] + data[i + 1] + data[i + 2] > 60) l++;
  lit = Math.max(lit, l);
  if (shotsDir && (shots === 0 || Date.now() - t0 > 3500)) {
    mkdirSync(shotsDir, { recursive: true });
    writeFileSync(resolve(shotsDir, `${basename(file, ".html")}-${shots}.png`), png);
  }
  shots++;
  await new Promise((r) => setTimeout(r, 150));
}
await browser.close();

// A healthy crawl changes on nearly every shot; the stalled one managed two
// frames in five seconds, or none at all.
const moving = frames.size >= Math.min(6, shots - 1);
const ok = lit > 500 && moving && !leaked.length;
console.log(`${ok ? "✓" : "✗"} ${basename(file)}: ${frames.size}/${shots} distinct frames in 4 s, ${lit} lit px, ${leaked.length} network requests`);
for (const u of leaked) console.log(`    leaked: ${u}`);
process.exit(ok ? 0 : 1);
