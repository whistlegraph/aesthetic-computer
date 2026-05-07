#!/usr/bin/env node
// Generate Menu Band Mac App Store screenshot test frames.
//
// Produces 2880x1800 PNGs from screenshots/app-store.json using Chrome
// headless. This is the composition layer; later we can swap the mocked
// Menu Band UI inside screenshots/test/app-store-test.html for Swift-captured
// raw UI while keeping the same manifest and export flow.

import { copyFileSync, existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(__dirname, "..");
const manifestPath = resolve(ROOT, "screenshots/app-store.json");
const templatePath = resolve(ROOT, "screenshots/test/app-store-test.html");
const outDir = resolve(ROOT, "screenshots/app-store");
const desktopDir = resolve(homedir(), "Desktop/MenuBand-AppStore-Test-Screenshots");
const chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";

if (!existsSync(chrome)) {
  console.error(`Google Chrome not found at ${chrome}`);
  process.exit(1);
}

mkdirSync(outDir, { recursive: true });
mkdirSync(desktopDir, { recursive: true });

const frames = JSON.parse(readFileSync(manifestPath, "utf8")).map((frame) => [
  frame.headline,
  frame.body,
  frame.mode,
]);

let html = readFileSync(templatePath, "utf8");
html = html.replace(
  /const frames = \[[\s\S]*?\];/,
  `const frames = ${JSON.stringify(frames, null, 2)};`,
);

const renderHtml = resolve(outDir, ".render.html");
writeFileSync(renderHtml, html);

const renderUrl = pathToFileURL(renderHtml).href;
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));

for (let i = 0; i < manifest.length; i++) {
  const frame = manifest[i];
  const filename = `${frame.slug}-2880x1800.png`;
  const outPath = resolve(outDir, filename);
  const result = spawnSync(chrome, [
    "--headless",
    "--disable-gpu",
    "--hide-scrollbars",
    "--window-size=2880,1800",
    `--screenshot=${outPath}`,
    `${renderUrl}?frame=${i}`,
  ], { encoding: "utf8" });

  if (result.status !== 0) {
    console.error(result.stderr || result.stdout);
    process.exit(result.status ?? 1);
  }

  const desktopPath = resolve(desktopDir, filename);
  copyFileSync(outPath, desktopPath);

  const dim = spawnSync("sips", ["-g", "pixelWidth", "-g", "pixelHeight", outPath], {
    encoding: "utf8",
  });
  const ok = dim.stdout.includes("pixelWidth: 2880") && dim.stdout.includes("pixelHeight: 1800");
  if (!ok) {
    console.error(`Unexpected dimensions for ${outPath}`);
    console.error(dim.stdout || dim.stderr);
    process.exit(1);
  }

  console.log(`✓ ${filename}`);
}

console.log(`\nRepo: ${outDir}`);
console.log(`Desktop: ${desktopDir}`);
