import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { extname } from "node:path";
import { chromium } from "playwright";

const live = new URL("../../xbox/live/", import.meta.url);
const pub = new URL("../public/", import.meta.url);
const checkpoint = tick => [tick, 5100 + tick, 12000, 0, 0, 0, 3, 0, 0,
  6900 - tick, 12000, 0, 0, 0, 3, 0, 0, 6000, 11958, 0, 0, 0, 1, 6000, 9600, 2200];
const fixture = { format: "ac.oskiedemo", version: 1, game: "oskiewar", simulation: "oskiewar-physics-1",
  matchId: "ow-zimmu299", matchName: "zimmu299", map: "station", tickRate: 60, durationTicks: 300,
  fighters: ["@JEFFREY", "@FIFI"], checkpoints: [checkpoint(0), checkpoint(300)], events: [], rounds: [[0, 1, 8, 1]] };

test("real Oskiewar replay viewer renders, freezes and resumes without live or account traffic", async () => {
  const replay = process.env.MIME_REPLAY_FIXTURE ? JSON.parse(await readFile(process.env.MIME_REPLAY_FIXTURE, "utf8")) : fixture;
  const browser = await chromium.launch({ channel: process.env.PLAYWRIGHT_CHANNEL, headless: true,
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader"] });
  const page = await browser.newPage({ viewport: { width: 430, height: 860 } });
  const errors = [], writes = [], sockets = [], accounts = [];
  page.on("pageerror", error => errors.push(error.message));
  page.on("websocket", socket => sockets.push(socket.url()));
  page.on("request", request => { if (request.method() !== "GET") writes.push(request.url());
    if (/auth0\.com|\/authorize|\/api\/me\b|device-pair/.test(request.url())) accounts.push(request.url()); });
  await page.route("**/*", async route => {
    const url = new URL(route.request().url());
    if (url.pathname === "/api/oskiewar-replays") {
      assert.equal(url.searchParams.get("id"), replay.matchId);
      return route.fulfill({ contentType: "application/json", body: JSON.stringify({ replay }) });
    }
    if (url.pathname.startsWith("/api/") || url.pathname.includes("/product-analytics-config"))
      return route.fulfill({ contentType: "application/json", body: "{}" });
    let file = new URL("." + url.pathname, live);
    if (url.pathname === "/" + replay.matchName) file = new URL("mac-test.html", live);
    if (url.pathname.startsWith("/aesthetic.computer/")) file = new URL("." + url.pathname, pub);
    if (url.pathname.startsWith("/ComicRelief-Regular.")) file = new URL("papers.aesthetic.computer/foundry/fonts" + url.pathname, pub);
    if (!file.href.startsWith(live.href) && !file.href.startsWith(pub.href)) return route.abort();
    try { return route.fulfill({ body: await readFile(file), contentType: ({ ".mjs": "text/javascript", ".js": "text/javascript", ".html": "text/html", ".json": "application/json", ".ttf": "font/ttf", ".woff2": "font/woff2", ".png": "image/png" })[extname(file.pathname)] || "application/octet-stream" }); }
    catch { return route.fulfill({ status: 404, body: "" }); }
  });
  try {
    await page.goto("https://oskiewar.com/" + replay.matchName + "?mime=1&warm=1");
    await page.waitForFunction(() => window.__mimeReady, null, { timeout: 45000 });
    assert.equal(await page.locator(".mime-replay-error").count(), 0);
    assert.equal(await page.locator("#account").isVisible(), false);
    const parked = await page.evaluate(() => ({ ...__oskiewarFrameStats }));
    await page.waitForTimeout(300);
    assert.equal(await page.evaluate(() => __oskiewarFrameStats.simulationTicks), parked.simulationTicks);
    await page.evaluate(() => __mimePlayerSettle(true));
    await page.waitForFunction(ticks => __oskiewarFrameStats.simulationTicks > ticks + 15, parked.simulationTicks);
    const opening = await page.screenshot({ path: "/tmp/mime15-replay-opening.png" });
    // Look beyond the three-second countdown, where a feed preview must play
    // the saved fight rather than merely advance a clock over a static pose.
    await page.waitForFunction(ticks => __oskiewarFrameStats.simulationTicks > ticks + 240, parked.simulationTicks);
    const fight = await page.screenshot({ path: "/tmp/mime15-replay-player.png" });
    assert.notDeepEqual(fight, opening);
    await page.evaluate(() => __mimePlayerSettle(false));
    const ticks = await page.evaluate(() => __oskiewarFrameStats.simulationTicks);
    await page.waitForTimeout(400);
    assert.equal(await page.evaluate(() => __oskiewarFrameStats.simulationTicks), ticks);
    assert.equal(await page.evaluate(() => __mimeAudioPlaying), false);
    assert.deepEqual(writes, []); assert.deepEqual(sockets, []); assert.deepEqual(accounts, []);
    assert.deepEqual(errors, []);
  } catch (error) {
    console.log({ errors, writes, sockets, accounts });
    await page.screenshot({ path: "/tmp/mime15-replay-error.png" });
    throw error;
  } finally { await browser.close(); }
});
