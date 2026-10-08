#!/usr/bin/env node
// daily-artifact-check.mjs — watch a daily package in the HEN/objkt sandbox.
//
// Serves the bundle to a sandboxed iframe (allow-scripts, opaque origin) and
// aborts every other request, then screenshots it for a few seconds: it
// passes when the crawl is lit and keeps repainting. This is the headless
// half of the gate; daily-token.mjs runs the static half (checkBundle) itself,
// since jasellite has no Chrome. Needs puppeteer (oven/node_modules or root).
//
//   node bin/daily-artifact-check.mjs out/daily/daily-2026-10-06.zip [--shots dir]

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
if (!file) { console.error("usage: daily-artifact-check.mjs <package.zip|bundle.html> [--shots dir]"); process.exit(2); }
const shotsDir = rest.includes("--shots") ? rest[rest.indexOf("--shots") + 1] : null;
const dimension = (name, fallback) => {
  const value = rest.includes(`--${name}`) ? Number(rest[rest.indexOf(`--${name}`) + 1]) : fallback;
  if (!Number.isInteger(value) || value < 64 || value > 4096) throw new Error(`Invalid --${name}`);
  return value;
};
const width = dimension("width", 512), height = dimension("height", 512);
const zip = file.endsWith(".zip") ? new (need("adm-zip"))(readFileSync(file)) : null;
const bundle = zip ? zip.readFile("index.html") : readFileSync(file);
if (!bundle) throw new Error("ZIP is missing root index.html");
const ART = "https://art.test/index.html", HOST = "https://host.test/";
const reader = rest.includes("--reader");
const host = `<!doctype html><body style="margin:0;background:#222"><iframe sandbox="allow-scripts${reader ? ' allow-popups allow-popups-to-escape-sandbox' : ''}" src="${ART}" style="border:0;width:${width}px;height:${height}px"></iframe>`;

const browser = await puppeteer.launch({ headless: "new" });
const page = await browser.newPage();
await page.setViewport({ width, height });
await page.setRequestInterception(true);
const leaked = [];
page.on("request", (r) => {
  const u = r.url();
  if (u === ART) return r.respond({ status: 200, contentType: "text/html", body: bundle });
  if (u === HOST) return r.respond({ status: 200, contentType: "text/html", body: host });
  if (zip && ["https://art.test/cover.gif", "https://art.test/thumbnail.png"].includes(u)) {
    const name = new URL(u).pathname.slice(1);
    return r.respond({ status: 200, contentType: name.endsWith("gif") ? "image/gif" : "image/png", body: zip.readFile(name) });
  }
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

// A healthy crawl changes on nearly every shot; the stalled one managed two
// frames in five seconds, or none at all.
const moving = frames.size >= Math.min(6, shots - 1);
let scrolled = false;
let linked = true;
const expectedURL = rest.includes("--expect-url") ? rest[rest.indexOf("--expect-url") + 1] : null;
if (reader && expectedURL) {
  const x = Number(rest[rest.indexOf("--link-x") + 1]);
  const y = Number(rest[rest.indexOf("--link-y") + 1]);
  if (!Number.isFinite(x) || !Number.isFinite(y) || x < 0 || y < 0 || x >= width || y >= height) throw new Error("Supply visible --link-x and --link-y coordinates");
  const original = page.url();
  const destination = new URL(expectedURL).href;
  try {
    const popup = browser.waitForTarget(target => target.type() === 'page' && target.url() === destination, { timeout: 5000 });
    await page.mouse.click(x, y);
    const target = await popup;
    linked = page.url() === original;
    await (await target.page()).close();
    await page.bringToFront();
  } catch { linked = false; }
}
if (reader) {
  const before = await page.screenshot();
  await page.mouse.move(width / 2, height / 2);
  await page.mouse.wheel({ deltaY: 200 });
  await new Promise(resolve => setTimeout(resolve, 500));
  const after = await page.screenshot();
  scrolled = !before.equals(after);
  if (shotsDir) writeFileSync(resolve(shotsDir, `${basename(file, '.html')}-scroll.png`), after);
}
const ok = lit > 500 && (reader ? scrolled && linked : moving) && !leaked.length;
await browser.close();
console.log(`${ok ? "✓" : "✗"} ${basename(file)}: ${frames.size}/${shots} distinct frames in 4 s, ${lit} lit px, ${leaked.length} network requests`);
for (const u of leaked) console.log(`    leaked: ${u}`);
if (reader) console.log(`    scroll changes the rendered page: ${scrolled}`);
if (expectedURL) console.log(`    link opens a separate tab and preserves the reader: ${linked}`);
process.exit(ok ? 0 : 1);
