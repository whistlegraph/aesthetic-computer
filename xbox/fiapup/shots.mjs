#!/usr/bin/env node
// Headless screenshots of the web shell, one per staged moment, into
// xbox/fiapup/shots/. One browser, closed at the end.
//
//   node xbox/fiapup/shots.mjs [moment…]     (default: all of them)

import { mkdirSync } from "node:fs";
import { createRequire } from "node:module";
import { spawnSync } from "node:child_process";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { serve } from "./serve.mjs";

const here = fileURLToPath(new URL(".", import.meta.url));
// A worktree has no node_modules of its own; borrow the main checkout's.
async function puppeteer() {
  try { return (await import("puppeteer")).default; } catch {}
  const common = spawnSync("git", ["rev-parse", "--git-common-dir"], { cwd: here, encoding: "utf8" }).stdout.trim();
  return createRequire(resolve(here, common, "../package.json"))("puppeteer");
}

const moments = {
  idle: "", fetch: "stage=fetch&seconds=1.05", pet: "stage=pet&seconds=1.1",
  rollover: "stage=pet&seconds=3.2", beg: "stage=beg&seconds=2.6", nap: "stage=nap&seconds=9",
  zoomies: "stage=zoomies&seconds=2.4", tug: "stage=tug&seconds=3.2",
};
const wanted = process.argv.slice(2).length ? process.argv.slice(2) : Object.keys(moments);
const out = resolve(here, "shots");
mkdirSync(out, { recursive: true });

const server = await serve(8125);
const browser = await (await puppeteer()).launch({ headless: true,
  args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--autoplay-policy=no-user-gesture-required"] });
try {
  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 754 });
  page.on("pageerror", (error) => console.error("page error:", error.message));
  for (const name of wanted) {
    await page.goto(`http://127.0.0.1:8125/?pause&${moments[name]}`);
    await page.waitForFunction(() => (globalThis.__fiapupFrames || 0) > 3, { timeout: 20000 });
    const state = await page.evaluate(() => globalThis.__fiapup.world.pup.state);
    const file = resolve(out, `${name}.png`);
    await page.screenshot({ path: file, clip: { x: 0, y: 0, width: 1280, height: 720 } });
    console.log(`${name}: pup is ${state} → ${file}`);
  }
} finally {
  await browser.close();
  server.close();
}
