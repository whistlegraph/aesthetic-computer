#!/usr/bin/env node
// reel-watch.mjs — watch the game play itself in REEL dress, live.
//
// The factory takes minutes per reel: source a match, re-sim it frame by
// frame, mux audio, verify sync. That is the right cost for something being
// published and the wrong cost for "is the celebration too slow?". This opens
// the same page the factory does, with the same reel capabilities and the same
// 9:16 frame, and just lets it run — bots fighting match after match at real
// speed, so a camera or HUD change can be judged in seconds.
//
// What it is NOT: a renderer. Nothing is captured, no audio is verified, and
// the matches are live rather than the deterministic re-sim the factory ships.
// Judge FEEL here; judge FOOTAGE with `reel.mjs`.
//
//   node xbox/live/marketing/reel-watch.mjs              # 9:16, reel dress
//   node xbox/live/marketing/reel-watch.mjs --hud        # keep the match HUD
//   node xbox/live/marketing/reel-watch.mjs --landscape  # 16:9 instead
//   node xbox/live/marketing/reel-watch.mjs --scale .4   # smaller window
//
// Edit oskiewar.js and reload the window — the shell serves it off disk, so
// there is no build step between a change and seeing it.

import { existsSync } from "node:fs";
import { serveShell } from "./shell.mjs";

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  if (!argv[i].startsWith("--")) continue;
  const next = argv[i + 1];
  if (next !== undefined && !next.startsWith("--")) { flags[argv[i].slice(2)] = next; i++; }
  else flags[argv[i].slice(2)] = true;
}

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

// A reel is 1080×1920; a display is not. Scale the whole frame down but keep
// the aspect exactly, because aspect is what the reel camera reads — the shot
// widths pull in on portrait via portraitPull(), so a 16:9 window would frame
// the fight differently than the thing being previewed.
const scale = Number(flags.scale) || (flags.landscape ? .5 : .42);
const [fullWidth, fullHeight] = flags.landscape ? [1920, 1080] : [1080, 1920];
const width = Math.round(fullWidth * scale);
const height = Math.round(fullHeight * scale);

const puppeteer = await loadPuppeteer();
const shell = await serveShell({ replays: "stub", log: () => {} });

// `replay-oven` is what silences the player-facing furniture; `reel-hud` is
// what turns the reel's own dress back on. The factory also passes
// `offline-render`, which hands the clock to a frame-stepper — exactly what a
// live preview must not do.
const address = `${shell.origin}/?social-preview&replay-oven` +
  (flags.hud === true ? "" : "&reel-hud");

// `--app` opens a window with no tab strip and no address bar, so the frame on
// screen is the frame being previewed rather than the frame minus Chrome's
// furniture. It has to start on about:blank: an app window loads its URL
// immediately at launch, which would be before the self-play flag below could
// be installed.
const browser = await puppeteer.launch({
  headless: false, executablePath: chrome, defaultViewport: null,
  args: ["--no-sandbox", "--autoplay-policy=no-user-gesture-required",
    "--use-gl=angle", "--use-angle=metal", "--enable-gpu", "--ignore-gpu-blocklist",
    "--disable-background-timer-throttling", "--disable-backgrounding-occluded-windows",
    "--disable-renderer-backgrounding",
    "--disable-features=CalculateNativeWinOcclusion",
    "--app=about:blank", `--window-size=${width},${height}`],
});

const [page] = await browser.pages();
page.on("pageerror", (error) => console.log(`⚠ page ${error.message.slice(0, 160)}`));
// Before a byte of the game runs. Self-play is deliberately unreachable from
// any button, so this is the only way in.
await page.evaluateOnNewDocument(() => { globalThis.__oskiewarSelfPlay = true; });
await page.goto(address, { waitUntil: "domcontentloaded" });
// The sign-in button belongs to a page someone is using, not to a preview of
// footage. Nothing else on the page has chrome of its own — the canvas is
// already pinned to the viewport.
await page.addStyleTag({ content: "#logout { display: none !important; }" });

console.log(`▸ reel watch · ${width}×${height} (${fullWidth}×${fullHeight} at ${scale})`);
console.log(`  ${address}`);
console.log(`  bots fight on a loop. reload the window after editing oskiewar.js.`);
console.log(`  close the window to stop.`);

await new Promise((done) => {
  browser.on("disconnected", done);
  process.on("SIGINT", done);
});
await shell.close?.();
process.exit(0);
