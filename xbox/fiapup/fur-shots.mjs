#!/usr/bin/env node
// Flat against fur, side by side, in headless Chrome at a phone's size:
// idle, mid-comb (a finger stroked the pup's back a moment ago), and just
// after zoomies (ruffled). Also the median frame time per look.
//
//   node xbox/fiapup/fur-shots.mjs [--size=390x844] [--shells=16]
// → xbox/fiapup/shots/fur/{idle,combed,ruffled}-{flat,fur}.png

import { mkdirSync } from "node:fs";
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
const shells = Number(flag("shells", 16));
const out = resolve(here, "shots/fur");
mkdirSync(out, { recursive: true });
const wait = (ms) => new Promise((r) => setTimeout(r, ms));

const server = await serve(8127);
const browser = await (await puppeteer()).launch({ headless: true,
  args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader"] });
const report = {};
try {
  const page = await browser.newPage();
  page.on("pageerror", (e) => console.error("page error:", e.message));
  page.on("console", (m) => { if (m.type() === "error") console.error("console:", m.text()); });
  await page.setViewport({ width, height, deviceScaleFactor: 2, isMobile: true, hasTouch: false });
  for (const look of ["flat", "fur"]) {
    await page.goto(`http://127.0.0.1:8127/fur-lab.html?${look === "flat" ? "flat" : `shells=${shells}`}`);
    await page.waitForFunction(() => (globalThis.__fiapupFrames || 0) > 30, { timeout: 60000 });
    // Hold the pup still for the pictures.
    await page.evaluate(() => { const w = globalThis.__fiapup.world; w.pup.energy = .6; w.pup.joy = .7; });
    await wait(1500);
    await page.screenshot({ path: resolve(out, `idle-${look}.png`) });
    report[look] = await page.evaluate(() => globalThis.__fiapupFrameMs());

    // A few strokes along the back, nose to tail, with the mouse.
    const back = await page.evaluate(() => {
      const f = globalThis.__fiapup, p = f.world.pup, c = Math.cos(p.heading), s = Math.sin(p.heading);
      const scale = document.querySelector("#frame").clientHeight / 1080;
      const at = (k) => { const q = f.screenOf(p.x + c * k, 30 + p.pose.bob, p.z + s * k); return [q.x * scale, q.y * scale]; };
      return [at(12), at(-10)];
    });
    // (held just short of a rollover, so the picture is of the comb)
    for (let pass = 0; pass < 3; pass++) {
      await page.evaluate(() => { globalThis.__fiapup.world.pup.pettedFor = -99; });
      await page.mouse.move(...back[0]);
      await page.mouse.down();
      for (let i = 1; i <= 14; i++)
        await page.mouse.move(back[0][0] + (back[1][0] - back[0][0]) * i / 14, back[0][1] + (back[1][1] - back[0][1]) * i / 14);
      await page.mouse.up();
    }
    await wait(250);
    await page.screenshot({ path: resolve(out, `combed-${look}.png`) });

    await page.evaluate(() => { const f = globalThis.__fiapup; f.world.pup.energy = .95; f.stageTap("play"); });
    // Game time runs slower than the wall clock under a software GPU, so
    // wait for the flop after the laps, and a moment for the camera.
    await page.waitForFunction(() => ["flop", "idle"].includes(globalThis.__fiapup.world.pup.state) &&
      globalThis.__fiapup.world.log.includes("zoomies"), { timeout: 60000, polling: 100 });
    await wait(900);
    await page.screenshot({ path: resolve(out, `ruffled-${look}.png`) });
  }
} finally {
  await browser.close();
  server.close();
}
// One sheet: a row per moment, flat on the left, fur on the right, cropped
// to the pup.
const crop = ["-crop", `${Math.round(width * 1.44)}x${Math.round(width * 1.44)}+${Math.round(width * .28)}+${Math.round(height * .62)}`, "+repage"];
const rows = ["idle", "combed", "ruffled"].map((m) => ["(", "(", resolve(out, `${m}-flat.png`), ...crop, ")",
  "(", resolve(out, `${m}-fur.png`), ...crop, ")", "+append", ")"]).flat();
spawnSync("magick", [...rows, "-append", "-resize", "50%", resolve(out, "flat-vs-fur.png")]);
console.log(`${width}×${height} @2x, ${shells} shells — median frame: flat ${report.flat?.toFixed(1)} ms, fur ${report.fur?.toFixed(1)} ms (SwiftShader, CPU)`);
