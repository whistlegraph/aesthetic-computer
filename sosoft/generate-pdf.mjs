#!/usr/bin/env node

// Generate the SO SOFT proposal PDF from proposal.html using Puppeteer.
// Usage: node sosoft/generate-pdf.mjs

import puppeteer from "puppeteer";
import { fileURLToPath } from "url";
import path from "path";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const htmlPath = path.join(__dirname, "proposal.html");
const outputPath = path.join(__dirname, "proposal.pdf");

const browser = await puppeteer.launch({
  headless: true,
  args: ["--no-sandbox", "--disable-setuid-sandbox"],
});

const page = await browser.newPage();
await page.goto(`file://${htmlPath}`, { waitUntil: "networkidle0" });

await page.pdf({
  path: outputPath,
  format: "Letter",
  printBackground: true,
  margin: { top: "0.9in", bottom: "0.9in", left: "1in", right: "1in" },
});

await browser.close();
console.log(`PDF written to ${outputPath}`);
