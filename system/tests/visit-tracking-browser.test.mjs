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
    await page.mouse.wheel(0, 150);
    await page.waitForTimeout(50);
    assert.ok(received.some(row => row.inputs.includes("scroll")), "reader scrolling counts without storing movement");
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

test("AC transparent display and runtime actions respect interaction, deduplication and private boundaries", async () => {
  const browser = await chromium.launch({ headless: true, channel: process.env.PLAYWRIGHT_CHANNEL });
  try {
    const context = await browser.newContext();
    const received = [];
    await context.route("**/*", async route => {
      const url = new URL(route.request().url());
      if (url.pathname === "/api/visit-track") {
        received.push(JSON.parse(route.request().postData()));
        return route.fulfill({ status: 204 });
      }
      if (/\/visit-(tracker|model)\.mjs$/.test(url.pathname)) {
        return route.fulfill({ contentType: "text/javascript", body: await readFile(new URL(`../public/aesthetic.computer/lib/${url.pathname.split("/").at(-1)}`, import.meta.url), "utf8") });
      }
      return route.fulfill({ contentType: "text/html", body: `
        <style>body{margin:0}#aesthetic-computer{width:300px;height:300px}canvas{position:absolute;pointer-events:none}button{position:absolute;top:10px;left:10px}</style>
        <div id="aesthetic-computer"><canvas data-ac-visit-canvas width="300" height="300"></canvas><button>control</button></div>
        <input type="password" style="position:absolute;top:50px;left:10px">
        <script type="module" src="/aesthetic.computer/lib/visit-tracker.mjs"></script>` });
    });
    const page = await context.newPage();
    await page.goto("https://aesthetic.computer/notepat");
    await page.waitForFunction(() => !!window.acVisits);
    assert.equal(await page.evaluate(() => acVisits.action("note_played")), false, "an automatic note cannot establish interaction");
    await page.locator("input").fill("secret");
    await page.locator("button").click();
    await page.waitForTimeout(50);
    assert.ok(!received.some(row => row.actions.includes("canvas_interacted")), "overlay controls are not canvas contact");
    await page.mouse.click(350, 350);
    await page.waitForTimeout(50);
    assert.ok(!received.some(row => row.actions.includes("canvas_interacted")), "outside display bounds is not canvas contact");
    await page.mouse.click(150, 150);
    await page.waitForTimeout(50);
    assert.ok(received.some(row => row.actions.includes("canvas_interacted")), "pointer-transparent display is measured");
    assert.equal(await page.evaluate(() => acVisits.action("note_played")), true);
    await page.waitForTimeout(50);
    const notes = received.filter(row => row.actions.includes("note_played")).length;
    await page.evaluate(() => { acVisits.action("note_played"); acVisits.action("arbitrary-private-label"); });
    await page.waitForTimeout(50);
    assert.equal(received.filter(row => row.actions.includes("note_played")).length, notes, "repeated notes do not produce repeated network events");
    assert.equal(await page.evaluate(() => { history.pushState({}, "", "/mail"); return acVisits.action("painting_saved"); }), false, "private navigation blocks actions before the next poll");
    assert.equal(await page.evaluate(() => { history.pushState({}, "", "/nopaint"); return acVisits.action("painting_saved"); }), false, "returning from private routes requires fresh interaction");
    await page.mouse.click(150, 150);
    assert.equal(await page.evaluate(() => acVisits.action("painting_edited")), true);
    assert.equal(await page.evaluate(() => { window.acVisitTrackingDisabled = true; return acVisits.action("painting_saved"); }), false);
    await page.waitForTimeout(50);
    assert.ok(!JSON.stringify(received).includes("secret"));
    assert.ok(!JSON.stringify(received).includes("arbitrary-private-label"));
    assert.ok(received.every(row => row.automated));
    await context.close();
  } finally { await browser.close(); }
});
