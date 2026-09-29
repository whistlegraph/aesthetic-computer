#!/usr/bin/env node
// JS per frame in headless Chrome at a phone's size: resting at camp, then
// running up the valley across chunk boundaries (streaming as it goes).
// The shell times sim + paint + the scene's submit each frame; a software
// GPU means GPU time isn't in it.
//
//   node xbox/fiapup/run-perf.mjs [--size=390x844] [--fur]

import { createRequire } from "node:module";
import { spawnSync } from "node:child_process";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { serve } from "./serve.mjs";

const here = fileURLToPath(new URL(".", import.meta.url));
async function puppeteer() {
  try { return (await import("puppeteer")).default; } catch {}
  const common = spawnSync("git", ["rev-parse", "--git-common-dir"], { cwd: here, encoding: "utf8" }).stdout.trim();
  return createRequire(resolve(here, common, "../package.json"))("puppeteer");
}
const flag = (name, fallback) => process.argv.find((a) => a.startsWith(`--${name}=`))?.split("=")[1] ?? fallback;
const [width, height] = flag("size", "390x844").split("x").map(Number);
const fur = process.argv.includes("--fur");
const wait = (ms) => new Promise((r) => setTimeout(r, ms));

const server = await serve(8128);
const browser = await (await puppeteer()).launch({ headless: true,
  args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader"] });
try {
  const page = await browser.newPage();
  await page.setViewport({ width, height, deviceScaleFactor: 2, isMobile: true, hasTouch: true });
  await page.goto(`http://127.0.0.1:8128/?touch&nokeys${fur ? "&fur" : ""}`);
  await page.waitForFunction(() => (globalThis.__fiapupFrames || 0) > 30, { timeout: 60000 });
  // Every frame's JS (the shell keeps the last 120), gathered as we go.
  const sample = async (seconds) => {
    const start = await page.evaluate(() => globalThis.__fiapup.world.chunkBuilds);
    const ms = [], numbers = [];
    for (let t = 0; t < seconds; t += 2) {
      await wait(2000);
      const got = await page.evaluate(() => ({ ms: globalThis.__fiapupFrameTimes().slice(-40),
        numbers: globalThis.__fiapup.stats.numbers }));
      ms.push(...got.ms); numbers.push(got.numbers);
    }
    ms.sort((a, b) => a - b);
    return page.evaluate((r) => {
      const f = globalThis.__fiapup;
      return { ...r, chunkBuilds: f.world.chunkBuilds - r.start, pupAt: [Math.round(f.world.pup.x), Math.round(f.world.pup.z)] };
    }, { start, medianMs: ms[ms.length >> 1], p95Ms: ms[Math.floor(ms.length * .95)], maxMs: ms.at(-1),
      samples: ms.length, programNumbers: Math.max(...numbers) });
  };
  const resting = await sample(4);
  await page.evaluate(() => { globalThis.__fiapup.world.pup.energy = 1; globalThis.__fiapup.world.goSpot = { x: 300, z: -2400 };
    globalThis.__fiapup.world.pup.state = "go"; });
  const running = await sample(10);
  console.log(JSON.stringify({ size: `${width}x${height}@2x`, fur, resting, running }, null, 2));
} finally {
  await browser.close();
  server.close();
}
