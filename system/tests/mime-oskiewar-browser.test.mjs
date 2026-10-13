import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { chromium } from "playwright";
import { replayPost } from "../backend/mime-oskiewar.mjs";

test("Oskiewar cards play in the feed, pause offscreen, and retain their permalink", async () => {
  const browser = await chromium.launch({ channel: process.env.PLAYWRIGHT_CHANNEL, headless: true });
  const page = await browser.newPage({ viewport: { width: 430, height: 860 } });
  const html = await readFile(new URL("../public/mime/index.html", import.meta.url), "utf8");
  const posts = ["ow-zimmu299", "ow-chette767"].map(_id => replayPost({ _id,
    fighters: ["@JEFFREY", "@FIFI"], recordedAt: new Date().toISOString(), version: 1 }));
  const errors = [];
  page.on("pageerror", error => errors.push(error.message));
  await page.route("**/*", route => {
    const url = new URL(route.request().url());
    if (url.hostname === "mime.ac" && (url.pathname === "/" || url.pathname.startsWith("/p/")))
      return route.fulfill({ contentType: "text/html", body: html });
    if (url.pathname === "/api/mime") return route.fulfill({ contentType: "application/json",
      body: JSON.stringify(url.searchParams.has("thread") ? { op: posts.find(p => p.code === url.searchParams.get("thread")), replies: [] }
        : { recent: posts, hasMore: false }) });
    if (url.hostname === "oskiewar.com") {
      assert.equal(url.searchParams.get("mime"), "1");
      assert.equal(url.searchParams.get("warm"), "1");
      return route.fulfill({ contentType: "text/html", body: `<body style="background:orange"><canvas aria-label="Oskiewar replay"></canvas><script>
        window.playing=false;
        addEventListener('message', event => { if(event.source===parent&&event.data?.type==='mime:playing') playing=event.data.playing; });
        window.__mimeReady=true;
        setTimeout(()=>parent.postMessage({type:'mime:ready'},'*'),20);
      </script>` });
    }
    return route.fulfill({ body: "" });
  });
  try {
    await page.goto("https://mime.ac/?ac-automation=1");
    await page.waitForFunction(() => document.querySelector("#ow-zimmu299 iframe")?.dataset.ready === "true");
    await page.locator(".mime-startup").waitFor({ state: "detached" });
    const first = page.frames().find(f => f.url().includes("oskiewar.com/zimmu299"));
    const second = page.frames().find(f => f.url().includes("oskiewar.com/chette767"));
    await first.waitForFunction(() => playing);
    assert.equal(await second.evaluate(() => playing), false);
    assert.equal(await page.locator("#ow-zimmu299 .reel-command .media-label").getAttribute("aria-label"), "ow-zimmu299");
    assert.equal(await page.locator("#ow-zimmu299 .reel-type").getAttribute("aria-label"), "application/x-oskiewar-demo");
    // Watched, not played: no toggle, the frame stays inert, a tap changes
    // nothing, and the wheel over the card moves the feed on.
    assert.equal(await page.locator("#ow-zimmu299 .media-toggle").count(), 0);
    assert.equal(await page.locator("#ow-zimmu299 .piece-preview").evaluate(el => el.inert), true);
    await page.locator("#ow-zimmu299 .media-shield").click();
    assert.equal(await page.locator("#ow-zimmu299 .feed-media").getAttribute("data-interacting"), null);
    assert.equal(await page.locator("#ow-zimmu299 .piece-preview").evaluate(el => el.inert), true);
    const card = await page.locator("#ow-zimmu299 .feed-media").boundingBox();
    await page.mouse.move(card.x + card.width / 2, card.y + card.height / 2);
    await page.mouse.wheel(0, 180);
    await page.waitForFunction(() => scrollY > 60);
    await page.locator("#ow-chette767").evaluate(e => e.scrollIntoView());
    await second.waitForFunction(() => playing);
    await first.waitForFunction(() => !playing);
    await page.waitForURL("**/p/ow-chette767?*");
    await page.reload();
    await page.waitForFunction(() => document.querySelector("#ow-chette767 iframe")?.dataset.ready === "true");
    await page.frames().find(f => f.url().includes("oskiewar.com/chette767")).waitForFunction(() => playing);
    assert.deepEqual(errors, []);
  } finally { await browser.close(); }
});
