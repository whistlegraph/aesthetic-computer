#!/usr/bin/env node
// farm-driver.mjs — the fighter for the demo farm.
//
// preview-server (jasellite) saves every round any browser POSTs it, but a
// server with no browser hosts no fights. This is the browser: headless
// Chrome pointed at the farm's self-play address, bots fighting around the
// clock, every demo landing in the farm's JSONL. Run it on any Mac with
// Chrome; the machine plays, the appliance keeps.
//
//   node xbox/live/marketing/farm-driver.mjs                 # → jasellite
//   node xbox/live/marketing/farm-driver.mjs http://host:7899
//
// Deliberately frugal — this box may have 8GB and other work to do: headless,
// a small canvas (the sim doesn't care), no reel dress (replay-oven without
// reel-hud draws the least), and a page reload every 20 minutes so a long
// shift never accumulates into a leak.

import { existsSync } from "node:fs";

const target = (process.argv[2] || "http://jasellite:7899") +
  "/?social-preview&replay-oven&self-play";

const chrome = [
  process.env.PUPPETEER_EXECUTABLE_PATH,
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary",
].find((path) => path && existsSync(path));
if (!chrome) throw new Error("no Chrome found — set PUPPETEER_EXECUTABLE_PATH");

async function loadPuppeteer() {
  const dir = ["/opt/homebrew/lib/node_modules/puppeteer",
    `${process.env.HOME}/aesthetic-computer/node_modules/puppeteer`,
    "/opt/oven/node_modules/puppeteer"].find((path) => existsSync(path));
  if (!dir) throw new Error("puppeteer not found");
  return (await import(`${dir}/lib/esm/puppeteer/puppeteer.js`)).default;
}

const puppeteer = await loadPuppeteer();
const browser = await puppeteer.launch({
  headless: true, executablePath: chrome,
  args: ["--no-sandbox", "--autoplay-policy=no-user-gesture-required",
    "--mute-audio", "--window-size=480,854"],
});
const [page] = await browser.pages();
await page.setViewport({ width: 480, height: 854, deviceScaleFactor: 1 });

let posted = 0;
page.on("response", (response) => {
  if (response.url().includes("/api/oskiewar-replays") &&
      response.request().method() === "POST") {
    posted++;
    if (posted % 10 === 0) console.log(`${posted} rounds posted`);
  }
});
page.on("pageerror", (error) => console.log(`⚠ ${error.message.slice(0, 140)}`));

console.log(`▸ farm driver → ${target}`);
await page.goto(target, { waitUntil: "domcontentloaded" });

// The shift: reload every 20 minutes, forever. Ctrl-C or kill to clock out.
setInterval(async () => {
  try {
    await page.reload({ waitUntil: "domcontentloaded" });
    console.log(`reloaded · ${posted} rounds so far`);
  } catch (error) {
    console.log(`reload failed: ${error.message.slice(0, 100)}`);
  }
}, 20 * 60 * 1000);
