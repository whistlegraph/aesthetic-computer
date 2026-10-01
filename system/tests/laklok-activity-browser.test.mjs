import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { chromium } from "playwright";

test("HTML laklok records repeated control use and send requests without exporting text", async () => {
  const browser = await chromium.launch({ headless: true, channel: process.env.PLAYWRIGHT_CHANNEL });
  try {
    const context = await browser.newContext({ userAgent: "Mozilla/5.0 Chrome/135.0.0.0 Safari/537.36" });
    const events = [];
    await context.route("**/*", async route => {
      const req = route.request(), url = new URL(req.url());
      if (url.pathname === "/api/account-activity") {
        if (req.method() === "POST") events.push(JSON.parse(req.postData()));
        return route.fulfill({ status: 204, headers: { "Access-Control-Allow-Origin": "*", "Access-Control-Allow-Headers": "Authorization, Content-Type", "Access-Control-Allow-Methods": "POST, OPTIONS" } });
      }
      if (url.pathname.startsWith("/aesthetic.computer/lib/") && /^[a-z-]+\.mjs$/.test(url.pathname.split("/").at(-1))) {
        return route.fulfill({ contentType: "text/javascript", body: await readFile(new URL(`../public${url.pathname}`, import.meta.url), "utf8") });
      }
      if (url.pathname === "/html/") return route.fulfill({ contentType: "text/html", body: await readFile(new URL("../public/html/index.html", import.meta.url), "utf8") });
      if (url.pathname === "/handle") return route.fulfill({ json: { handle: "fixture" } });
      if (url.pathname === "/api/chat-messages") return route.fulfill({ json: { messages: [] } });
      return route.fulfill({ status: 404 });
    });
    const page = await context.newPage();
    await page.addInitScript(() => {
      // Synthetic auth/automation overrides exist only inside this intercepted fixture.
      Object.defineProperty(navigator, "webdriver", { get: () => false });
      localStorage.setItem("@@auth0spajs@@fixture", JSON.stringify({ expiresAt: Date.now() / 1000 + 3600,
        body: { access_token: "fixture-token", decodedToken: { claims: { sub: "auth0|fixture" } } } }));
      window.WebSocket = class {
        readyState = 1;
        constructor() { setTimeout(() => this.onopen?.(), 0); }
        send() {} close() {}
      };
    });
    await page.goto("https://laklok.com/html/");
    await page.waitForFunction(() => !!window.acAccountActivity && !document.getElementById("input").disabled);
    await page.locator("#gear").click();
    await page.locator('[data-tema="nat"]').click();
    await page.locator('[data-links="1"]').click();
    await page.locator("#gear").click(); await page.locator("#gear").click();
    await page.locator("#input").fill("THIS_STAYS_PRIVATE");
    await page.locator("#input").press("Enter");
    await page.waitForTimeout(150);
    assert.equal(events.filter(row => row.action === "laklok_settings_opened").length, 2);
    for (const action of ["laklok_theme_changed", "laklok_filter_changed", "laklok_message_send_requested"])
      assert.ok(events.some(row => row.action === action), action);
    assert.ok(events.every(row => row.piece === "laklok-vector" && row.featureVersion === 1));
    assert.doesNotMatch(JSON.stringify(events), /THIS_STAYS_PRIVATE|fixture-token|auth0\|fixture/);
    const count = events.length;
    await page.evaluate(() => { window.acVisitTrackingDisabled = true; });
    await page.locator("#gear").click(); await page.locator("#gear").click();
    await page.waitForTimeout(100); assert.equal(events.length, count);
  } finally { await browser.close(); }
});
