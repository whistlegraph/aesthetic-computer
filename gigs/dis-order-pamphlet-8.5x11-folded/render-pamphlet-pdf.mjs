#!/usr/bin/env node
import path from "node:path";
import { pathToFileURL } from "node:url";
import puppeteer from "puppeteer";

const cwd = process.cwd();
const htmlArg = process.argv[2] ?? "dis-order-pamphlet-v4-bifold.html";
const pdfArg = process.argv[3] ?? "dis-order-pamphlet-v4-bifold.pdf";

const htmlPath = path.isAbsolute(htmlArg) ? htmlArg : path.join(cwd, htmlArg);
const pdfPath = path.isAbsolute(pdfArg) ? pdfArg : path.join(cwd, pdfArg);

const browser = await puppeteer.launch({
  headless: true,
  args: ["--no-sandbox", "--disable-setuid-sandbox"],
});

try {
  const page = await browser.newPage();
  await page.setViewport({ width: 1760, height: 1360, deviceScaleFactor: 1 });
  await page.goto(pathToFileURL(htmlPath).href, { waitUntil: "networkidle0" });
  await page.emulateMediaType("print");

  await page.pdf({
    path: pdfPath,
    width: "11in",
    height: "8.5in",
    printBackground: true,
    margin: { top: "0", right: "0", bottom: "0", left: "0" },
    preferCSSPageSize: true,
  });

  console.log(`wrote ${pdfPath}`);
} finally {
  await browser.close();
}
