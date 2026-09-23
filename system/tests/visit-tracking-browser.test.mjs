import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { chromium } from "playwright";

test("browser funnel, automation, privacy opt-out, private SPA routes and duplicate installation", async () => {
  const browser = await chromium.launch({ headless: true, channel: process.env.PLAYWRIGHT_CHANNEL });
  try {
    const context = await browser.newContext();
    const received = [];
    await context.route("**/*", async route => {
      const url = new URL(route.request().url());
      if (url.pathname === "/api/visit-track") {
        received.push(JSON.parse(route.request().postData()));
        return route.fulfill({ status: 204, headers: { "Access-Control-Allow-Origin": "*" } });
      }
      if (/\/visit-(tracker|model)\.mjs$/.test(url.pathname)) {
        const name = url.pathname.split("/").at(-1);
        return route.fulfill({ contentType: "text/javascript", body: await readFile(new URL(`../public/aesthetic.computer/lib/${name}`, import.meta.url), "utf8"), headers: { "Access-Control-Allow-Origin": "*" } });
      }
      return route.fulfill({ contentType: "text/html", body: `<button>play</button><input type="password"><script type="module" src="https://aesthetic.computer/aesthetic.computer/lib/visit-tracker.mjs"></script>` });
    });
    const page = await context.newPage();
    await page.clock.install();
    await page.goto("https://nopaint.art/");
    await page.waitForFunction(() => !!window.acVisits);
    await page.waitForTimeout(50);
    assert.equal(received.length, 1);
    assert.equal(received[0].automated, true);
    await page.evaluate(() => window.dispatchEvent(new KeyboardEvent("keydown", { key: "a" })));
    await page.waitForTimeout(50);
    assert.equal(received.length, 1, "synthetic input is ignored");
    await page.locator("input").fill("secret");
    assert.equal(received.length, 1, "form input is ignored");
    await page.locator("button").click();
    await page.waitForTimeout(50);
    assert.ok(received.some(row => row.interacted));
    await page.clock.runFor(11000);
    await page.waitForTimeout(50);
    assert.ok(received.some(row => row.interacted && row.activeSeconds >= 10));
    const ids = new Set(received.map(row => row.id));
    assert.equal(ids.size, 1);
    await page.evaluate(() => history.replaceState({}, "", "/some-round"));
    await page.clock.runFor(2000);
    assert.equal(new Set(received.map(row => row.id)).size, 1, "automatic round URLs are not new visits");
    assert.ok(!JSON.stringify(received).includes("secret"));
    await page.evaluate(async () => (await import("https://aesthetic.computer/aesthetic.computer/lib/visit-tracker.mjs")).startVisitTracker());
    await page.evaluate(() => history.pushState({}, "", "/mail"));
    await page.clock.runFor(2000);
    const count = received.length;
    await page.locator("button").click();
    await page.clock.runFor(11000);
    assert.equal(received.length, count, "private SPA routes stop sending");
    const optedOut = await context.newPage();
    await optedOut.addInitScript(() => Object.defineProperty(navigator, "globalPrivacyControl", { value: true }));
    await optedOut.goto("https://jas.life/");
    await optedOut.waitForTimeout(100);
    assert.equal(await optedOut.evaluate(() => !!window.acVisits), false);
    assert.equal(received.length, count);
    await context.close();
  } finally { await browser.close(); }
});
