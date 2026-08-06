#!/usr/bin/env node

import { writeFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import puppeteer from "puppeteer-core";

const here = dirname(fileURLToPath(import.meta.url));
const output = resolve(here, "beyond-source-pages.json");
const chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const sources = [
  ["Painted Installations", "https://www.thomaslawson.com/beyond-the-studio-painted-installations/"],
  ["Early New York", "https://www.thomaslawson.com/beyond-the-studio-early-new-york/"],
  ["Dark Installations", "https://www.thomaslawson.com/beyond-the-studio-dark-installations/"],
  ["Temporary Murals", "https://www.thomaslawson.com/beyond-the-studio-portraits-of-new-york/"],
  ["Glasgow Projects", "https://www.thomaslawson.com/beyond-the-studio-glasgow-projects/"],
  ["Theatre, Dance & Fashion", "https://www.thomaslawson.com/beyond-the-studio-theatre-dance-fashion/"],
  ["Los Angeles", "https://www.thomaslawson.com/beyond-the-studio-los-angeles/"],
  ["The Scottish Project", "https://www.thomaslawson.com/beyond-the-studio-the-scottish-project/"],
];

const browser = await puppeteer.launch({ executablePath: chrome, headless: true, args: ["--no-sandbox"] });
const pages = [];
try {
  for (const [overviewLabel, url] of sources) {
    const page = await browser.newPage();
    await page.setViewport({ width: 1200, height: 900, deviceScaleFactor: 1 });
    await page.goto(url, { waitUntil: "domcontentloaded", timeout: 30_000 });
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
        images: [...main.querySelectorAll("img")]
          .map((image) => ({ alt: clean(image.alt), title: clean(image.title), src: image.currentSrc || image.src }))
          .filter((image) => image.alt || image.title),
      };
    });
    pages.push({ overviewLabel, url, ...source });
    await page.close();
    console.log(`${overviewLabel}: ${source.headings.length} headings, ${source.textBlocks.length} text blocks`);
  }
} finally {
  await browser.close();
}

await writeFile(output, `${JSON.stringify({ schema: "thomas-lawson-beyond-sources/v1", capturedAt: new Date().toISOString(), pages }, null, 2)}\n`);
