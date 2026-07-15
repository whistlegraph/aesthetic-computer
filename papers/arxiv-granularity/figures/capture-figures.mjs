// capture-figures.mjs — live browser screenshots for the paper's exhibits.
// Uses the repo's playwright + installed Google Chrome (channel: "chrome").
//   node papers/arxiv-granularity/figures/capture-figures.mjs
// Writes PNGs into this figures/ dir. Each capture is isolated so one
// failure does not sink the others. Cookie banners are declined/dismissed.
import { chromium } from "playwright";

const DIR = new URL(".", import.meta.url).pathname;
const browser = await chromium.launch({ channel: "chrome", headless: true });

async function shot(name, fn) {
  const ctx = await browser.newContext({
    viewport: { width: 1280, height: 820 },
    deviceScaleFactor: 2,
    userAgent:
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36",
  });
  const page = await ctx.newPage();
  try {
    await fn(page);
    console.log("OK   " + name);
  } catch (e) {
    console.log("FAIL " + name + " :: " + String(e.message).slice(0, 160));
  } finally {
    await ctx.close();
  }
}

const dismiss = async (page, selectors) => {
  for (const s of selectors) {
    try {
      const el = await page.waitForSelector(s, { timeout: 2500 });
      if (el) { await el.click(); await page.waitForTimeout(600); return true; }
    } catch {}
  }
  return false;
};

// 1) AC dj piece — our own turntable/scratch player.
await shot("fig-ac-dj", async (page) => {
  await page.goto("https://aesthetic.computer/dj", { waitUntil: "load", timeout: 60000 });
  await page.waitForTimeout(9000); // let the piece boot and draw the platter
  await page.screenshot({ path: DIR + "fig-ac-dj.png" });
});

// 2) YouTube playback-speed menu — the coarse itemized control.
await shot("fig-youtube-speed", async (page) => {
  await page.goto("https://www.youtube.com/watch?v=jNQXAC9IVRw", {
    waitUntil: "domcontentloaded",
    timeout: 60000,
  });
  await dismiss(page, [
    'button[aria-label*="Reject"]',
    'button:has-text("Reject all")',
    'button:has-text("Accept all")',
    'tp-yt-paper-button:has-text("Accept all")',
  ]);
  await page.waitForSelector(".ytp-settings-button", { timeout: 30000 });
  await page.hover(".html5-video-player").catch(() => {});
  await page.click(".ytp-settings-button");
  await page.waitForTimeout(500);
  await page.click('.ytp-menuitem:has-text("Playback speed")');
  await page.waitForTimeout(800);
  const menu = await page.$(".ytp-popup.ytp-settings-menu");
  if (menu) await menu.screenshot({ path: DIR + "fig-youtube-speed.png" });
  else await page.screenshot({ path: DIR + "fig-youtube-speed.png" });
});

// 3) Spotify community — a real "add speed control for music" idea thread.
await shot("fig-spotify-demand", async (page) => {
  await page.goto(
    "https://community.spotify.com/t5/Live-Ideas/All-Platforms-Change-playback-speed-when-listening-to-music/idi-p/4794106",
    { waitUntil: "domcontentloaded", timeout: 60000 },
  );
  await dismiss(page, [
    "#onetrust-reject-all-handler",
    "#onetrust-accept-btn-handler",
    'button:has-text("Reject")',
    'button:has-text("Accept")',
  ]);
  await page.waitForTimeout(1500);
  await page.screenshot({ path: DIR + "fig-spotify-demand.png" });
});

await browser.close();
console.log("done");
