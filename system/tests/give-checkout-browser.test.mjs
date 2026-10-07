import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { chromium } from "playwright";

test("Give opens standard and custom Stripe checkout and rejects other destinations", async () => {
  const browser = await chromium.launch({ headless: true, channel: process.env.PLAYWRIGHT_CHANNEL });
  try {
    const context = await browser.newContext();
    let destination;
    const submitted = [];
    await context.route("**/*", async route => {
      const request = route.request(), url = new URL(request.url());
      if (url.pathname === "/api/give") {
        submitted.push(JSON.parse(request.postData()));
        return route.fulfill({ json: { url: destination }, headers: { "Access-Control-Allow-Origin": "*" } });
      }
      if (url.pathname === "/api/gives")
        return route.fulfill({ json: { activeSubscribers: 5 }, headers: { "Access-Control-Allow-Origin": "*" } });
      if (url.hostname === "give.aesthetic.computer" && ["/", "/give.mjs"].includes(url.pathname))
        return route.fulfill({ contentType: url.pathname === "/" ? "text/html" : "text/javascript",
          body: await readFile(new URL(`../public/give.aesthetic.computer/${url.pathname === "/" ? "index.html" : "give.mjs"}`, import.meta.url), "utf8") });
      return route.fulfill({ contentType: "text/html", body: "Checkout destination" });
    });
    const page = await context.newPage();
    for (const host of ["checkout.stripe.com", "pay.aesthetic.computer"]) {
      destination = `https://${host}/test-checkout`;
      await page.goto("https://give.aesthetic.computer/");
      await page.locator("#give").click();
      await page.waitForURL(destination);
      assert.equal(submitted.at(-1).amount, 800);
      assert.equal(submitted.at(-1).recurring, true);
    }
    for (const rejected of ["https://pay.aesthetic.computer.evil.test/", "http://pay.aesthetic.computer/"]) {
      destination = rejected;
      await page.goto("https://give.aesthetic.computer/");
      await page.locator('input[value="once"]').check();
      await page.locator("#give").click();
      await page.waitForFunction(() => document.getElementById("error").textContent.length > 0);
      assert.equal(page.url(), "https://give.aesthetic.computer/");
      assert.equal(submitted.at(-1).recurring, false);
      assert.equal(await page.locator("#give").isEnabled(), true);
    }
    await context.close();
  } finally { await browser.close(); }
});
