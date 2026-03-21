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
  executablePath: process.env.PUPPETEER_EXECUTABLE_PATH || "/usr/sbin/chromium-browser",
  args: ["--no-sandbox", "--disable-setuid-sandbox"],
});

const page = await browser.newPage();
await page.goto(`file://${htmlPath}`, { waitUntil: "networkidle0" });

await page.pdf({
  path: outputPath,
  format: "Letter",
  printBackground: true,
  margin: { top: "0.65in", bottom: "0.65in", left: "0.75in", right: "0.75in" },
});

await browser.close();
console.log(`PDF written to ${outputPath}`);
