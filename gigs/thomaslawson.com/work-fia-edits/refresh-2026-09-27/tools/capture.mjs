// capture.mjs — screenshot every thomaslawson.com page (desktop + mobile) and
// inventory the computed type on each. Optional CSS/JS injection for preview.
//   node capture.mjs <outName> [inject.css] [inject.js]   env TL_ONLY=slug,slug
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import puppeteer from "puppeteer-core";
import sharp from "sharp";

const [outName = "before", cssPath, jsPath] = process.argv.slice(2);
const root = "/Users/jas/aesthetic-computer/gigs/thomaslawson.com/work-fia-edits/refresh-2026-09-27";
const outDir = resolve(root, "evidence", outName);
const css = cssPath ? await readFile(cssPath, "utf8") : null;
const js = jsPath ? await readFile(jsPath, "utf8") : null;
const only = process.env.TL_ONLY ? new Set(process.env.TL_ONLY.split(",")) : null;
const pages = JSON.parse(await readFile(resolve(root, "pages.json"), "utf8"))
  .map((p) => ({ id: p.id, slug: p.slug, url: p.link }))
  .filter((p) => !only || only.has(p.slug))
  .sort((a, b) => a.url.localeCompare(b.url));
const viewports = {
  desktop: { width: 1440, height: 900, deviceScaleFactor: 1 },
  mobile: { width: 390, height: 844, deviceScaleFactor: 2, isMobile: true, hasTouch: true },
};
const MAX_H = 7000;

await mkdir(outDir, { recursive: true });
const browser = await puppeteer.launch({
  executablePath: "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  headless: true,
  args: ["--no-sandbox", "--hide-scrollbars"],
  protocolTimeout: 240_000,
});
const manifest = { capturedAt: new Date().toISOString(), injected: !!css, pages: [] };

async function one(p, vpName, vp) {
  const page = await browser.newPage();
  await page.setViewport(vp);
  // Capture normalisation for BOTH runs: Elementor's entrance animations
  // leave widgets hidden until a scroll waypoint fires, which headless
  // scrolling sometimes misses. Visitors see them; so should the evidence.
  await page.evaluateOnNewDocument(() => {
    document.addEventListener("DOMContentLoaded", () => {
      const s = document.createElement("style");
      s.textContent = ".elementor-invisible{visibility:visible!important;opacity:1!important;transform:none!important;animation:none!important}";
      document.head.appendChild(s);
    });
  });
  if (css || js) {
    // Inject before paint, *after* the site's own late styles, via a DOM hook.
    await page.evaluateOnNewDocument((css, js) => {
      document.addEventListener("DOMContentLoaded", () => {
        if (css) {
          const s = document.createElement("style");
          s.id = "tl-refresh-preview";
          s.textContent = css;
          document.body.appendChild(s);
        }
        if (js) {
          const sc = document.createElement("script");
          sc.textContent = js;
          document.body.appendChild(sc);
        }
      });
    }, css, js);
  }
  await page.goto(p.url, { waitUntil: "networkidle2", timeout: 60_000 }).catch(() => {});
  await page.evaluate(async () => {
    const delay = (ms) => new Promise((r) => setTimeout(r, ms));
    [...document.images].forEach((i) => (i.loading = "eager"));
    for (let y = 0; y < document.documentElement.scrollHeight; y += innerHeight * 0.8) {
      scrollTo(0, y);
      await delay(60);
    }
    scrollTo(0, 0);
    await Promise.race([Promise.all([...document.images].map((i) => i.decode?.().catch(() => {}))), delay(6000)]);
    await document.fonts.ready;
  });
  const type = await page.evaluate(() => {
    const seen = {};
    const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT);
    while (walker.nextNode()) {
      const t = walker.currentNode.textContent.trim();
      if (!t) continue;
      const el = walker.currentNode.parentElement;
      const r = el.getBoundingClientRect();
      if (!r.width || !r.height) continue;
      const cs = getComputedStyle(el);
      if (cs.visibility === "hidden" || cs.display === "none" || +cs.opacity === 0) continue;
      const fam = cs.fontFamily.split(",")[0].replace(/["']/g, "").trim();
      const key = `${fam} | ${cs.fontSize} | ${cs.fontWeight} | ${cs.fontStyle} | ${cs.textTransform} | ${cs.letterSpacing}`;
      (seen[key] ||= { n: 0, sample: t.slice(0, 40), tag: el.tagName.toLowerCase() }).n++;
    }
    return { styles: seen, overflowX: document.documentElement.scrollWidth > innerWidth + 1, height: document.documentElement.scrollHeight };
  });
  const h = Math.min(type.height, MAX_H);
  await page.evaluate(async () => {
    for (let y = 0; y < document.documentElement.scrollHeight; y += 400) { scrollTo(0, y); await new Promise((r) => setTimeout(r, 120)); }
    await Promise.race([Promise.all([...document.images].map((i) => (i.complete ? 0 : new Promise((r) => { i.onload = i.onerror = r; })))), new Promise((r) => setTimeout(r, 8000))]);
    // Retry anything the host dropped (GoDaddy throttles bursts).
    for (let pass = 0; pass < 3; pass++) {
      const bad = [...document.images].filter((i) => i.complete && !i.naturalWidth && (i.currentSrc || i.src));
      if (!bad.length) break;
      await new Promise((r) => setTimeout(r, 1500));
      await Promise.all(bad.map((i) => new Promise((r) => { i.onload = i.onerror = r; i.removeAttribute("srcset"); i.src = (i.currentSrc || i.src).split("?")[0] + "?r=" + pass; setTimeout(r, 10000); })));
    }
    scrollTo(0, 0);
  });
  await new Promise((r) => setTimeout(r, 500));
  type.brokenImages = await page.evaluate(() => [...document.images].filter((i) => !i.naturalWidth && i.getBoundingClientRect().width > 40).length);
  const full = await page.screenshot({ fullPage: true, type: "png" });
  const meta = await sharp(full).metadata();
  const buf = await sharp(full).extract({ left: 0, top: 0, width: meta.width, height: Math.min(meta.height, Math.round(h * (vp.deviceScaleFactor || 1))) }).toBuffer();
  const file = `${vpName}-${p.slug}.jpg`;
  await sharp(buf).resize({ width: vpName === "desktop" ? 1200 : 480 }).jpeg({ quality: 78 }).toFile(resolve(outDir, file));
  await page.close();
  return { slug: p.slug, id: p.id, url: p.url, viewport: vpName, file, ...type };
}

const queue = [];
const onlyVp = process.env.TL_VP;
for (const p of pages) for (const [n, vp] of Object.entries(viewports)) if (!onlyVp || onlyVp === n) queue.push([p, n, vp]);
const workers = Array.from({ length: +(process.env.TL_JOBS || 2) }, async () => {
  while (queue.length) {
    const [p, n, vp] = queue.shift();
    try {
      manifest.pages.push(await one(p, n, vp));
      process.stdout.write(".");
    } catch (e) {
      console.error(`\n${n} ${p.slug}: ${e.message}`);
    }
  }
});
await Promise.all(workers);
await browser.close();
// Merge into an existing manifest so partial re-runs fill gaps.
try {
  const prev = JSON.parse(await readFile(resolve(outDir, "manifest.json"), "utf8"));
  const fresh = new Set(manifest.pages.map((p) => p.viewport + p.slug));
  manifest.pages.push(...prev.pages.filter((p) => !fresh.has(p.viewport + p.slug)));
} catch {}
manifest.pages.sort((a, b) => (a.viewport + a.slug).localeCompare(b.viewport + b.slug));
await writeFile(resolve(outDir, "manifest.json"), JSON.stringify(manifest, null, 1));
console.log(`\n${manifest.pages.length} captures → ${outDir}`);
