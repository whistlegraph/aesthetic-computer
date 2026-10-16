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
    for (const [host, source] of [["checkout.stripe.com", ""], ["pay.aesthetic.computer", "homepage"], ["pay.aesthetic.computer", "unreviewed-private-text"]]) {
      destination = `https://${host}/test-checkout`;
      await page.goto(`https://give.aesthetic.computer/${source ? `?source=${source}` : ""}`);
      await page.locator("#give").click();
      await page.waitForURL(destination);
      assert.equal(submitted.at(-1).amount, 800);
      assert.equal(submitted.at(-1).recurring, true);
      assert.equal(submitted.at(-1).source, source === "homepage" ? "homepage" : undefined);
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

test("handle colors use verified monthly checkout, preserve login selections, and avoid duplicate support", async () => {
  const browser = await chromium.launch({ headless: true, channel: process.env.PLAYWRIGHT_CHANNEL });
  try {
    const context = await browser.newContext({ viewport: { width: 390, height: 844 } });
    const submitted = [];
    let authenticated = true, duplicate = false, callback;
    await context.route("**/*", async route => {
      const request = route.request(), url = new URL(request.url());
      const headers = { "Access-Control-Allow-Origin": "*", "Access-Control-Allow-Headers": "authorization,content-type", "Access-Control-Allow-Methods": "POST,GET,OPTIONS" };
      if (request.method() === "OPTIONS") return route.fulfill({ status: 204, headers });
      if (url.hostname === "cdn.auth0.com") return route.fulfill({ contentType: "text/javascript", body: `window.auth0={createAuth0Client:async()=>({
        isAuthenticated:async()=>${authenticated},getTokenSilently:async()=>"fixture-token",
        loginWithRedirect:async options=>{window.loginOptions=options},
        handleRedirectCallback:async()=>({appState:${JSON.stringify(callback || {})}})
      })}` });
      if (url.pathname === "/api/give") {
        assert.equal(request.headers().authorization, "Bearer fixture-token");
        submitted.push(JSON.parse(request.postData()));
        return route.fulfill({ headers, status: duplicate ? 409 : 200, json: duplicate
          ? { code: "already_supporting" } : { url: "https://pay.aesthetic.computer/monthly" } });
      }
      if (url.pathname === "/api/gives") return route.fulfill({ headers, json: { activeSubscribers: 5 } });
      if ((url.hostname === "give.aesthetic.computer" && url.pathname === "/give.mjs") || (url.hostname === "aesthetic.computer" && url.pathname === "/support.html"))
        return route.fulfill({ contentType: url.pathname === "/support.html" ? "text/html" : "text/javascript",
          body: await readFile(new URL(`../public/give.aesthetic.computer/${url.pathname === "/support.html" ? "index.html" : "give.mjs"}`, import.meta.url), "utf8") });
      return route.fulfill({ contentType: "text/html", body: "Returned" });
    });
    const page = await context.newPage();
    await page.goto("https://aesthetic.computer/support.html?source=signup&email=untrusted@example.test");
    await page.getByText("Monthly support unlocks handle colors across AC.").waitFor();
    assert.equal(await page.locator('input[value="once"]').isVisible(), false);
    await page.locator("#amount").fill("1");
    await page.locator("#give").click();
    await page.waitForURL("https://pay.aesthetic.computer/monthly");
    assert.deepEqual(submitted.at(-1), { amount: 100, currency: "usd", recurring: true, source: "signup" });

    authenticated = false;
    await page.goto("https://aesthetic.computer/support.html?source=handle-colors");
    await page.locator("#amount").fill("3.50");
    await page.locator("#give").click();
    await page.waitForFunction(() => window.loginOptions);
    callback = (await page.evaluate(() => window.loginOptions)).appState;
    assert.equal(callback.give.amount, 350);
    assert.equal(submitted.length, 1);
    authenticated = true;
    await page.goto("https://aesthetic.computer/support.html?code=test&state=test");
    await page.waitForURL("https://pay.aesthetic.computer/monthly");
    assert.deepEqual(submitted.at(-1), { amount: 350, currency: "usd", recurring: true, source: "handle-colors" });

    duplicate = true;
    await page.goto("https://aesthetic.computer/support.html?source=signup");
    await page.locator("#give").click();
    await page.waitForURL("https://aesthetic.computer/handle");
    await context.close();
  } finally { await browser.close(); }
});
