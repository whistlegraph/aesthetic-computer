#!/usr/bin/env node
// The app icon, drawn by the game: the web shell's `?portrait` mode renders
// the pup's face from puppy-flat.lisp through the same frame interpreter,
// full-bleed at 1024, in headless Chrome. From that one picture:
//
//   apple/fiapup/Assets.xcassets/AppIcon.appiconset/
//     ios-1024.png          full-bleed; iOS masks it
//     mac-16…mac-1024.png   the macOS rounded square, inset per Apple's
//                           grid (824 of 1024, 185 corner radius) with a
//                           soft drop shadow, at 16–512 @1x/@2x
//
//   node xbox/fiapup/icon.mjs      (needs ImageMagick's `magick`)

import { mkdirSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import { spawnSync } from "node:child_process";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { serve } from "./serve.mjs";

const here = fileURLToPath(new URL(".", import.meta.url));
const set = resolve(here, "../../apple/fiapup/Assets.xcassets/AppIcon.appiconset");
async function puppeteer() {
  try { return (await import("puppeteer")).default; } catch {}
  const common = spawnSync("git", ["rev-parse", "--git-common-dir"], { cwd: here, encoding: "utf8" }).stdout.trim();
  return createRequire(resolve(here, common, "../package.json"))("puppeteer");
}
function magick(...args) {
  const run = spawnSync("magick", args, { encoding: "utf8" });
  if (run.status !== 0) throw new Error(`magick ${args.join(" ")}: ${run.stderr}`);
}

mkdirSync(set, { recursive: true });
const face = resolve(set, "ios-1024.png");

const server = await serve(8126);
const browser = await (await puppeteer()).launch({ headless: true,
  args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader"] });
try {
  const page = await browser.newPage();
  await page.setViewport({ width: 1024, height: 1024, deviceScaleFactor: 1 });
  await page.goto("http://127.0.0.1:8126/?portrait&nokeys");
  await page.waitForFunction(() => (globalThis.__fiapupFrames || 0) > 3, { timeout: 20000 });
  await page.screenshot({ path: face, clip: { x: 0, y: 0, width: 1024, height: 1024 } });
} finally {
  await browser.close();
  server.close();
}

// The Mac's rounded square: the face inset to 824, corners of 185, a soft
// shadow under it, on a clear 1024 canvas.
const mac = resolve(set, "mac-1024.png");
const mask = resolve(set, ".mask.png"), tile = resolve(set, ".tile.png");
magick("-size", "824x824", "xc:none", "-fill", "white", "-draw", "roundrectangle 0,0 823,823 185,185", mask);
magick(face, "-resize", "824x824", mask, "-alpha", "off", "-compose", "CopyOpacity", "-composite", tile);
magick("-size", "1024x1024", "xc:none",
  "(", tile, "-background", "black", "-shadow", "40x14+0+12", ")", "-geometry", "+64+64", "-compose", "over", "-composite",
  tile, "-geometry", "+100+88", "-compose", "over", "-composite", mac);
spawnSync("rm", ["-f", mask, tile]);

const images = [{ idiom: "universal", platform: "ios", size: "1024x1024", filename: "ios-1024.png" }];
for (const size of [16, 32, 128, 256, 512]) for (const scale of [1, 2]) {
  const px = size * scale, file = `mac-${px}.png`;
  if (px !== 1024) magick(mac, "-resize", `${px}x${px}`, resolve(set, file));
  images.push({ idiom: "mac", size: `${size}x${size}`, scale: `${scale}x`, filename: file });
}
writeFileSync(resolve(set, "Contents.json"), JSON.stringify({ images, info: { author: "xcode", version: 1 } }, null, 2) + "\n");
writeFileSync(resolve(set, "../Contents.json"), JSON.stringify({ info: { author: "xcode", version: 1 } }, null, 2) + "\n");
console.log(`icon → ${face}\n       ${mac}`);
