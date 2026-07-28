#!/usr/bin/env node

import { mkdir, writeFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import puppeteer from "puppeteer-core";

const here = dirname(fileURLToPath(import.meta.url));
const evidenceName = process.env.TL_EVIDENCE_NAME || "current";
const pageFilter = process.env.TL_PAGE ? new Set(process.env.TL_PAGE.split(",")) : null;
const outDir = resolve(here, "evidence", evidenceName);
const chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";

const pages = {
  about: "https://www.thomaslawson.com/about/",
  news: "https://www.thomaslawson.com/notes/",
  studio: "https://www.thomaslawson.com/in-the-studio/",
  beyond: "https://www.thomaslawson.com/beyond-the-studio/",
  writing: "https://www.thomaslawson.com/bookshelf/",
  broader: "https://www.thomaslawson.com/art-in-a-broader-context/",
};

const viewports = {
  desktop: { width: 1440, height: 1000, deviceScaleFactor: 1 },
  mobile: { width: 390, height: 844, deviceScaleFactor: 1 },
};

await mkdir(outDir, { recursive: true });
const browser = await puppeteer.launch({
  executablePath: chrome,
  headless: true,
  args: ["--no-sandbox", "--disable-setuid-sandbox"],
});

const manifest = {
  schema: "thomas-lawson-live-evidence/v1",
  capturedAt: new Date().toISOString(),
  pages: [],
};

try {
  for (const [viewportName, viewport] of Object.entries(viewports)) {
    for (const [slug, url] of Object.entries(pages).filter(([name]) => !pageFilter || pageFilter.has(name))) {
      const page = await browser.newPage();
      await page.setViewport(viewport);
      await page.goto(url, { waitUntil: "domcontentloaded", timeout: 30_000 });
      await page.addStyleTag({ content: `
        .elementor-invisible {
          visibility: visible !important;
          opacity: 1 !important;
          transform: none !important;
        }
        *, *::before, *::after {
          animation: none !important;
          transition: none !important;
        }
      ` });
      await page.evaluate(async () => {
        const timeout = (ms) => new Promise((done) => setTimeout(done, ms));
        await Promise.race([document.fonts.ready, timeout(3_000)]);
        [...document.images].forEach((image) => { image.loading = "eager"; });
        for (let y = 0; y < document.documentElement.scrollHeight; y += Math.max(500, innerHeight * 0.75)) {
          scrollTo(0, y);
          await timeout(90);
        }
        scrollTo(0, 0);
        const pending = [...document.images]
          .filter((image) => !image.complete)
          .map((image) => new Promise((done) => {
            image.addEventListener("load", done, { once: true });
            image.addEventListener("error", done, { once: true });
          }));
        await Promise.race([Promise.all(pending), timeout(5_000)]);
      });
      await page.addStyleTag({ content: `
        *, *::before, *::after {
          animation-duration: 0s !important;
          animation-delay: 0s !important;
          transition-duration: 0s !important;
          caret-color: transparent !important;
        }
      ` });

      const pageHeight = await page.evaluate(() => document.documentElement.scrollHeight);
      await page.setViewport({ ...viewport, height: Math.min(16_000, Math.max(viewport.height, pageHeight)) });
      await page.evaluate(async () => {
        const timeout = (ms) => new Promise((done) => setTimeout(done, ms));
        await Promise.race([
          Promise.all([...document.images].map((image) => image.decode?.().catch(() => {}))),
          timeout(5_000),
        ]);
        await timeout(250);
      });

      const file = `${viewportName}-${slug}.jpg`;
      await page.screenshot({
        path: resolve(outDir, file),
        type: "jpeg",
        quality: 84,
        fullPage: true,
      });

      const observed = await page.evaluate(() => ({
        title: document.title,
        bodyClasses: document.body.className,
        viewport: { width: innerWidth, height: innerHeight },
        page: { width: document.documentElement.scrollWidth, height: document.documentElement.scrollHeight },
        horizontalOverflow: document.documentElement.scrollWidth > innerWidth,
        headings: [...document.querySelectorAll("h1,h2,h3,h4,h5,h6")].map((heading) => ({
          level: Number(heading.tagName.slice(1)),
          text: heading.textContent.replace(/\s+/g, " ").trim(),
        })).filter((heading) => heading.text),
        links: [...document.querySelectorAll("main a, #primary a, [data-elementor-type='wp-page'] a")].map((link) => ({
          text: link.textContent.replace(/\s+/g, " ").trim(),
          href: link.href,
        })).filter((link) => link.text || link.href),
        images: [...document.images].map((image) => ({
          alt: image.alt,
          src: image.currentSrc || image.src,
          complete: image.complete,
          naturalWidth: image.naturalWidth,
          naturalHeight: image.naturalHeight,
          loading: image.loading,
        })),
      }));

      manifest.pages.push({ slug, url, viewportName, file, ...observed });
      await page.close();
      console.log(`${viewportName}/${slug}: ${observed.page.width}x${observed.page.height}${observed.horizontalOverflow ? " overflow" : ""}`);
    }
  }
  await writeFile(resolve(outDir, "manifest.json"), `${JSON.stringify(manifest, null, 2)}\n`);
} finally {
  await Promise.race([browser.close(), new Promise((done) => setTimeout(done, 5_000))]);
}
