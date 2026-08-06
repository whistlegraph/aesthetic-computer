#!/usr/bin/env node

import { writeFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import puppeteer from "puppeteer-core";

const here = dirname(fileURLToPath(import.meta.url));
const output = resolve(here, "broader-source-pages.json");
const chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const sources = [
  ["Art School", "https://www.thomaslawson.com/elementor-1878/"],
  ["Dissent", "https://www.thomaslawson.com/art-in-context-dissent/"],
  ["The Experimental Impulse", "https://www.thomaslawson.com/art-in-context-hot-coffee/"],
  ["Hot Coffee", "https://www.thomaslawson.com/art-in-context-hot-coffee-2/"],
  ["Shimmer", "https://www.thomaslawson.com/art-in-context-shimmer/"],
  ["The British Art Show", "https://www.thomaslawson.com/art-in-context-the-british-art-show/"],
  ["Nostalgia as Resistance", "https://www.thomaslawson.com/art-in-context-nostalgia-as-reference/"],
  ["Livin' in the USA", "https://www.thomaslawson.com/art-in-context-livin-in-the-usa/"],
  ["Critical Perspectives", "https://www.thomaslawson.com/critical-perspectives-art-in-context/"],
  ["REALIFE | White Columns", "https://www.thomaslawson.com/reallife-magazine-presents-whitecolumns-art-in-context/"],
  ["REALIFE Magazine Presents", "https://www.thomaslawson.com/art-in-context-reallife-presents/"],
  ["Pat Douthwaite", "https://www.thomaslawson.com/pat-douthewaite-art-in-context/"],
];

const browser = await puppeteer.launch({ executablePath: chrome, headless: true, args: ["--no-sandbox"] });
const pages = [];
try {
  for (const [overviewLabel, url] of sources) {
    const page = await browser.newPage();
    try {
      await page.goto(url, { waitUntil: "domcontentloaded", timeout: 30_000 });
    } catch {
      await page.goto(url, { waitUntil: "load", timeout: 30_000 });
    }
    const source = await page.evaluate(() => {
      const clean = (value) => String(value || "").replace(/\s+/g, " ").trim();
      const main = document.querySelector("[data-elementor-type='wp-page'], main, #primary") || document.body;
      return {
        documentTitle: document.title,
        headings: [...main.querySelectorAll("h1,h2,h3,h4,h5,h6")]
          .map((node) => ({ level: Number(node.tagName.slice(1)), text: clean(node.textContent) }))
          .filter((entry) => entry.text),
        textBlocks: [...main.querySelectorAll(".elementor-widget-text-editor, figcaption")]
          .map((node) => clean(node.textContent)).filter(Boolean),
      };
    });
    pages.push({ overviewLabel, url, ...source });
    await page.close();
    console.log(`${overviewLabel}: ${source.headings.length} headings, ${source.textBlocks.length} text blocks`);
  }
  await writeFile(output, `${JSON.stringify({ schema: "thomas-lawson-broader-sources/v1", capturedAt: new Date().toISOString(), pages }, null, 2)}\n`);
} finally {
  await Promise.race([
    browser.close(),
    new Promise((done) => setTimeout(done, 5_000)),
  ]);
}
