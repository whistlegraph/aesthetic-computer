import { test } from "node:test";
import assert from "node:assert/strict";
import crypto from "node:crypto";
import { assertion, useKeyReader, searchConsole, analytics, searchSummary } from "../../toolchain/analytics/google-search.mjs";

const { privateKey, publicKey } = crypto.generateKeyPairSync("rsa", { modulusLength: 2048 });
const key = { client_email: "reader@aesthetic-computer.iam.gserviceaccount.com", private_key: privateKey.export({ type: "pkcs8", format: "pem" }) };

test("the token assertion is an RS256 JWT for Google's token endpoint with read-only scopes", () => {
  const [head, claims, sig] = assertion(key, { now: 1000 }).split(".");
  assert.ok(crypto.verify("RSA-SHA256", Buffer.from(`${head}.${claims}`), publicKey, Buffer.from(sig, "base64url")));
  const c = JSON.parse(Buffer.from(claims, "base64url"));
  assert.equal(c.iss, key.client_email);
  assert.equal(c.aud, "https://oauth2.googleapis.com/token");
  assert.equal(c.exp - c.iat, 3600);
  assert.ok(c.scope.split(" ").every((s) => s.endsWith(".readonly")));
});

function fakeGoogle(calls) {
  return async (url, init = {}) => {
    calls.push({ url: String(url), body: init.body && typeof init.body === "string" && init.body.startsWith("{") ? JSON.parse(init.body) : null });
    const json = (value) => ({ ok: true, status: 200, json: async () => value });
    if (String(url).includes("oauth2")) return json({ access_token: "t", expires_in: 3600 });
    if (String(url).includes("searchAnalytics")) {
      const body = JSON.parse(init.body);
      return json(body.dimensions ? { rows: [{ keys: ["aesthetic computer"], clicks: 12, impressions: 300, ctr: 0.04, position: 3.26 }] }
        : { rows: [{ clicks: 20, impressions: 900, ctr: 0.0222, position: 8.04 }] });
    }
    if (String(url).includes("analyticsdata")) {
      const body = JSON.parse(init.body);
      const row = (d, v) => ({ dimensionValues: d.map((value) => ({ value })), metricValues: v.map((value) => ({ value: String(value) })) });
      if (!body.dimensions) return json({ rows: [{ metricValues: [{ value: "50" }, { value: "30" }, { value: "40" }] }] });
      return json({ rows: body.dimensions.length === 2 ? [row(["google", "organic"], [20, 12, 18])] : [row(["/laklok"], [9, 6, 8])] });
    }
    throw new Error(`unexpected ${url}`);
  };
}

test("Search Console rows and totals fold, the window ends two days back, and unknown sites are refused", async () => {
  useKeyReader(async () => key);
  const calls = [];
  const r = await searchConsole({ site: "aesthetic.computer", days: 7, fetch: fakeGoogle(calls) });
  assert.deepEqual(r.totals, { clicks: 20, impressions: 900, ctr: 2.22, position: 8 });
  assert.deepEqual(r.query[0], { key: "aesthetic computer", clicks: 12, impressions: 300, ctr: 4, position: 3.3 });
  assert.ok(calls.some((c) => c.url.includes(encodeURIComponent("sc-domain:aesthetic.computer"))));
  const query = calls.find((c) => c.body?.dimensions);
  assert.equal(query.body.endDate, new Date(Date.now() - 2 * 86400_000).toISOString().slice(0, 10));
  await assert.rejects(searchConsole({ site: "example.com", fetch: fakeGoogle([]) }), /site must be one of/);
});

test("Analytics folds totals, sources and landing pages; a failing site is reported, not thrown", async () => {
  useKeyReader(async () => key);
  const r = await analytics({ days: 7, fetch: fakeGoogle([]) });
  assert.deepEqual(r.totals, { sessions: 50, engaged: 30, users: 40 });
  assert.deepEqual(r.bySourceMedium[0], { key: "google / organic", sessions: 20, engaged: 12, users: 18 });
  assert.equal(r.byLandingPage[0].key, "/laklok");
  const broken = async (url) => String(url).includes("oauth2") ? { ok: true, json: async () => ({ access_token: "t", expires_in: 3600 }) }
    : { ok: false, status: 403, statusText: "Forbidden", json: async () => ({ error: { message: "User does not have sufficient permission for site. See also" } }) };
  useKeyReader(async () => key);
  const summary = await searchSummary({ days: 7, fetch: broken });
  assert.equal(summary.sites.length, 10);
  assert.match(summary.sites[0].unavailable, /^403 User does not have sufficient permission for site$/);
});
