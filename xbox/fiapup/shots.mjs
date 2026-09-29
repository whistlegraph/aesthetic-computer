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
// --phone shoots each moment as a phone shows it (touch UI), upright and on
// its side, into shots/phone/.
const phone = process.argv.includes("--phone");
const args = process.argv.slice(2).filter((a) => !a.startsWith("--"));
const wanted = args.length ? args : Object.keys(moments);
const out = resolve(here, phone ? "shots/phone" : "shots");
const views = phone
  ? [{ tag: "portrait", width: 390, height: 844, scale: 2 }, { tag: "landscape", width: 844, height: 390, scale: 2 }]
  : [{ tag: "", width: 1280, height: 754, scale: 1, clip: 720 }];
mkdirSync(out, { recursive: true });

const server = await serve(8125);
const browser = await (await puppeteer()).launch({ headless: true,
  args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--autoplay-policy=no-user-gesture-required"] });
try {
  const page = await browser.newPage();
  page.on("pageerror", (error) => console.error("page error:", error.message));
  for (const view of views) {
    await page.setViewport({ width: view.width, height: view.height, deviceScaleFactor: view.scale,
      isMobile: phone, hasTouch: phone });
    for (const name of wanted) {
      await page.goto(`http://127.0.0.1:8125/?pause${phone ? "&touch&nokeys" : ""}&${moments[name]}`);
      await page.waitForFunction(() => (globalThis.__fiapupFrames || 0) > 3, { timeout: 20000 });
      const state = await page.evaluate(() => globalThis.__fiapup.world.pup.state);
      const file = resolve(out, `${[name, view.tag].filter(Boolean).join("-")}.png`);
      await page.screenshot({ path: file, clip: { x: 0, y: 0, width: view.width, height: view.clip || view.height } });
      console.log(`${name}${view.tag ? " " + view.tag : ""}: pup is ${state} → ${file}`);
    }
  }
} finally {
  await browser.close();
  server.close();
}
