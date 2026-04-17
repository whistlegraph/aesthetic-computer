#!/usr/bin/env node
// build-cv-pdf.mjs — Generate jeffrey-alan-scudder-cv.pdf from cv.html.
//
// Renders /cv via a local Caddy dev server (preferred) or falls back to
// rendering the local file directly, then prints to PDF using headless
// Chrome through puppeteer-core. The output lands in
// system/public/papers.aesthetic.computer/ so papers.aesthetic.computer
// serves it at https://papers.aesthetic.computer/jeffrey-alan-scudder-cv.pdf.
//
// Usage:
//   node build-cv-pdf.mjs                      # render http://localhost:8111/cv
//   node build-cv-pdf.mjs --file               # render the local cv.html file
//   node build-cv-pdf.mjs --url https://justanothersystem.org/cv

import { existsSync, mkdirSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { dirname, join, resolve } from "node:path";
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);
const __dirname = dirname(fileURLToPath(import.meta.url));

const SITE_DIR = __dirname;
const PUBLIC_DIR = resolve(SITE_DIR, "..");
const PAPERS_DIR = resolve(PUBLIC_DIR, "papers.aesthetic.computer");
const OUTPUT_PDF = join(PAPERS_DIR, "jeffrey-alan-scudder-cv.pdf");

const DEFAULT_URL = "http://localhost:8111/cv";
const FALLBACK_FILE = pathToFileURL(join(SITE_DIR, "cv.html")).href;

const CHROME_CANDIDATES = [
  process.env.PUPPETEER_EXECUTABLE_PATH,
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Chromium.app/Contents/MacOS/Chromium",
  "/usr/bin/google-chrome",
  "/usr/bin/chromium",
  "/usr/bin/chromium-browser",
].filter(Boolean);

function pickChrome() {
  for (const p of CHROME_CANDIDATES) {
    if (existsSync(p)) return p;
  }
  throw new Error(
    "No Chrome/Chromium found. Set PUPPETEER_EXECUTABLE_PATH to a Chrome binary.",
  );
}

function parseArgs(argv) {
  const args = { url: null, useFile: false };
  for (let i = 2; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--file") args.useFile = true;
    else if (a === "--url") args.url = argv[++i];
  }
  return args;
}

async function main() {
  const { url: urlArg, useFile } = parseArgs(process.argv);

  let puppeteer;
  try {
    puppeteer = require("puppeteer-core");
  } catch {
    puppeteer = require("puppeteer");
  }

  if (!existsSync(PAPERS_DIR)) mkdirSync(PAPERS_DIR, { recursive: true });

  const target = useFile ? FALLBACK_FILE : urlArg || DEFAULT_URL;
  const executablePath = pickChrome();

  console.log(`[cv-pdf] chrome:    ${executablePath}`);
  console.log(`[cv-pdf] target:    ${target}`);
  console.log(`[cv-pdf] output:    ${OUTPUT_PDF}`);

  const browser = await puppeteer.launch({
    executablePath,
    headless: "new",
    args: ["--no-sandbox", "--disable-dev-shm-usage"],
  });

  try {
    const page = await browser.newPage();
    await page.emulateMediaType("print");

    try {
      await page.goto(target, {
        waitUntil: "networkidle0",
        timeout: 60_000,
      });
    } catch (err) {
      if (!useFile && !urlArg) {
        console.warn(
          `[cv-pdf] ${DEFAULT_URL} failed (${err.message}); falling back to ${FALLBACK_FILE}`,
        );
        await page.goto(FALLBACK_FILE, {
          waitUntil: "networkidle0",
          timeout: 60_000,
        });
      } else {
        throw err;
      }
    }

    // Downsample the hero portrait to a small inline JPEG so the PDF
    // doesn't embed 25 MB of CDN source photos. We keep only the first
    // slide, render it to a ~360 px wide canvas, and replace the src
    // with the resulting data URL.
    await page.evaluate(async () => {
      if (document.fonts?.ready) await document.fonts.ready;

      const portrait = document.getElementById("heroSlideshow");
      if (portrait) {
        const first = portrait.querySelector("img");
        portrait
          .querySelectorAll("img:not(:first-child)")
          .forEach((el) => el.remove());

        if (first) {
          if (!first.complete) {
            await new Promise((res) => {
              first.addEventListener("load", res, { once: true });
              first.addEventListener("error", res, { once: true });
            });
          }

          try {
            const maxW = 360;
            const ratio = first.naturalWidth
              ? Math.min(1, maxW / first.naturalWidth)
              : 1;
            const w = Math.max(1, Math.round(first.naturalWidth * ratio));
            const h = Math.max(1, Math.round(first.naturalHeight * ratio));
            const canvas = document.createElement("canvas");
            canvas.width = w;
            canvas.height = h;
            const ctx = canvas.getContext("2d");
            ctx.drawImage(first, 0, 0, w, h);
            first.src = canvas.toDataURL("image/jpeg", 0.82);
          } catch {
            // If canvas taint blocks us (CORS), just drop the portrait.
            portrait.remove();
          }
        }
      }

      const remaining = Array.from(document.images);
      await Promise.all(
        remaining.map((img) =>
          img.complete
            ? null
            : new Promise((res) => {
                img.addEventListener("load", res, { once: true });
                img.addEventListener("error", res, { once: true });
              }),
        ),
      );
    });

    await page.pdf({
      path: OUTPUT_PDF,
      format: "letter",
      printBackground: true,
      preferCSSPageSize: true,
      margin: { top: "0.6in", right: "0.6in", bottom: "0.6in", left: "0.6in" },
    });

    console.log(`[cv-pdf] wrote ${OUTPUT_PDF}`);
  } finally {
    await browser.close();
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
