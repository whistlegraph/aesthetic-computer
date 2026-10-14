// Fully intercepted local fixtures: no credentials or traffic reach production.
import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { createHash } from "node:crypto";
import { chromium } from "playwright";
import { handler as bridgePage } from "../netlify/functions/network-recognition-bridge.mjs";

test("two-origin recognition keeps credentials on AC, handles empty caches, and honors browser opt-outs", async () => {
  const browser = await chromium.launch({ headless: true, channel: process.env.PLAYWRIGHT_CHANNEL || "chrome" });
  try {
    for (const mode of ["session", "sso", "consent-required", "no-cache", "blocked-storage", "gpc", "automation"]) {
      const context = await browser.newContext({ userAgent: "Mozilla/5.0 RecognitionFixture" }), requests = [], violations = [], unexpected = [];
      let authorization, tokenExchanges = 0;
      await context.addInitScript(mode => {
        // Only this fully intercepted fixture simulates a non-automated browser.
        if (mode !== "automation") Object.defineProperty(navigator, "webdriver", { get: () => false });
        if (mode === "gpc") Object.defineProperty(navigator, "globalPrivacyControl", { get: () => true });
        if (location.origin === "https://aesthetic.computer") {
          if (mode === "session") localStorage.setItem("session-aesthetic", btoa(JSON.stringify({ accessToken: "fixture-private-token", account: { id: "private-account" } })));
          if (mode === "blocked-storage") Object.defineProperty(window, "localStorage", { get: () => { throw new DOMException("Blocked", "SecurityError"); } });
        }
        window.receivedMessages = [];
        window.addEventListener("message", e => receivedMessages.push(e.data));
        window.cspViolations = [];
        document.addEventListener("securitypolicyviolation", e => cspViolations.push(e.blockedURI));
      }, mode);
      await context.route("**/*", async route => {
        const req = route.request(), url = new URL(req.url());
        if (url.origin === "https://hi.aesthetic.computer" && url.pathname === "/authorize") {
          authorization = url.searchParams;
          assert.equal(authorization.get("prompt"), "none");
          assert.equal(authorization.get("response_type"), "code");
          assert.equal(authorization.get("code_challenge_method"), "S256");
          assert.equal(authorization.get("redirect_uri"), "https://aesthetic.computer");
          const response = { state: authorization.get("state"), ...(mode === "sso" ? { code: "fixture-code" }
            : { error: mode === "consent-required" ? "consent_required" : "login_required" }) };
          return route.fulfill({ contentType: "text/html", body: `<script>parent.postMessage(${JSON.stringify({ type: "authorization_response", response })}, "https://aesthetic.computer")</script>` });
        }
        if (url.origin === "https://hi.aesthetic.computer" && url.pathname === "/oauth/token") {
          if (req.method() === "OPTIONS") return route.fulfill({ status: 204, headers: {
            "Access-Control-Allow-Origin": "https://aesthetic.computer", "Access-Control-Allow-Methods": "POST", "Access-Control-Allow-Headers": "content-type" } });
          tokenExchanges++;
          const body = req.postDataJSON();
          assert.equal(body.grant_type, "authorization_code"); assert.equal(body.code, "fixture-code");
          assert.equal(createHash("sha256").update(body.code_verifier).digest("base64url"), authorization.get("code_challenge"));
          return route.fulfill({ contentType: "application/json", headers: { "Access-Control-Allow-Origin": "https://aesthetic.computer" },
            body: JSON.stringify({ access_token: "fixture-private-token", token_type: "Bearer" }) });
        }
        if (url.hostname === "sotce.net" && url.pathname === "/") return route.fulfill({ contentType: "text/html", body:
          '<!doctype html><html><body><p>Sotce fixture</p><script type="module" src="https://aesthetic.computer/aesthetic.computer/lib/visit-tracker.mjs"></script></body></html>' });
        if (url.pathname === "/api/network-recognition-bridge") {
          const page = await bridgePage({ httpMethod: "GET" });
          return route.fulfill({ status: page.statusCode, headers: page.headers, body: page.body });
        }
        if (url.pathname === "/api/network-recognition") {
          requests.push({ origin: req.headers().origin, authorization: req.headers().authorization, body: JSON.parse(req.postData()) });
          return route.fulfill({ status: 204, headers: { "Access-Control-Allow-Origin": "https://sotce.net" } });
        }
        if (url.pathname === "/api/visit-track") return route.fulfill({ status: 204, headers: { "Access-Control-Allow-Origin": "*" } });
        if (/^\/aesthetic\.computer\/(?:lib\/(?:network-recognition(?:-bridge|-model)?|visit-model|visit-tracker)\.mjs|dep\/auth0-spa-js\.production\.js)$/.test(url.pathname)) {
          return route.fulfill({ contentType: "text/javascript", headers: { "Access-Control-Allow-Origin": "*" },
            body: await readFile(new URL(`../public${url.pathname}`, import.meta.url), "utf8") });
        }
        unexpected.push(req.url()); return route.abort();
      });
      const page = await context.newPage();
      await page.goto("https://sotce.net/");
      if (["gpc", "automation"].includes(mode)) {
        await page.waitForTimeout(100);
        assert.equal(requests.length, 0, mode);
        assert.equal(await page.locator("iframe").count(), 0);
      } else {
        await page.waitForFunction(() => receivedMessages.some(m => m?.type === "ac-recognition:done"), undefined, { timeout: 8000 });
        assert.equal(requests.length, 2, mode);
        assert.equal(requests[0].origin, "https://sotce.net");
        assert.equal(requests[0].authorization, undefined);
        assert.equal(requests[1].origin, "https://aesthetic.computer");
        assert.equal(requests[1].authorization, ["session", "sso"].includes(mode) ? "Bearer fixture-private-token" : undefined);
        assert.equal(requests[0].body.id, requests[1].body.id);
        assert.deepEqual(requests.map(r => r.body.phase), ["started", "checked"]);
        assert.doesNotMatch(JSON.stringify(await page.evaluate(() => receivedMessages)), /private|token|account|recognized/i);
        assert.equal(await page.locator("iframe").count(), 0, "frame is removed after completion");
        assert.equal(await page.evaluate(() => localStorage.length), 0, "no identifier persists on Sotce");
      }
      violations.push(...await page.evaluate(() => cspViolations));
      assert.equal(tokenExchanges, mode === "sso" ? 1 : 0);
      assert.deepEqual(unexpected, [], "no token refresh, redirect or unmocked network request");
      assert.deepEqual(violations, []);
      await context.close();
    }
  } finally { await browser.close(); }
});
