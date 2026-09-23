import test from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { validateVisit, visitUpdate, visitProperty, visitSurface, automatedVisit } from "../public/aesthetic.computer/lib/visit-model.mjs";
import { handler } from "../netlify/functions/visit-track.mjs";

const snapshot = () => ({ version: 1, id: randomUUID(), surface: "home",
  activeSeconds: 10, interacted: true, automated: false,
  inputs: ["pointer"], actions: ["link_followed"] });

test("property identity comes from a reviewed HTTPS origin, not submitted data", () => {
  const value = snapshot();
  assert.equal(validateVisit({ ...value, property: "jas.life" }, "https://nopaint.art").property, "nopaint.art");
  for (const origin of ["null", "http://nopaint.art", "https://nopaint.art:8888", "https://nopaint.art.evil.test", "https://mail.aesthetic.computer"])
    assert.equal(validateVisit(value, origin), null);
  assert.equal(visitProperty("www.whistlegraph.org"), "whistlegraph.org");
});

test("private routes are excluded and arbitrary URLs never enter stored data", () => {
  for (const path of ["/mail", "/admin.html", "/chat~private", "/wallet/index.html", "/%61dmin", "/account/reset"])
    assert.equal(visitSurface(path), null, path);
  const value = validateVisit({ ...snapshot(), email: "private", path: "/secret", text: "private" }, "https://jas.life");
  assert.ok(!JSON.stringify(visitUpdate(value)).includes("private"));
});

test("only bounded signals pass and bot evidence cannot be cleared", () => {
  const value = snapshot();
  for (const bad of [{ actions: ["anything"] }, { inputs: ["password"] }, { activeSeconds: 999 }, { interacted: false }, { id: "not-a-uuid" }])
    assert.equal(validateVisit({ ...value, ...bad }, "https://oskiewar.com"), null);
  const bot = validateVisit(value, "https://oskiewar.com", "HeadlessChrome");
  assert.equal(bot.automated, true);
  assert.equal(visitUpdate(bot).$max.automated, true);
  assert.equal(automatedVisit({}, "?offline-render"), true);
});

test("cumulative updates preserve milestones and expire after 35 days", () => {
  const now = new Date("2026-09-23T00:00:00Z");
  const update = visitUpdate(validateVisit(snapshot(), "https://jas.life"), now);
  assert.equal(update.$max.engaged, true);
  assert.equal(update.$max["actions.link_followed"], true);
  assert.equal(+update.$setOnInsert.expiresAt - +now, 35 * 86400000);
  assert.equal(update.$min.startedAt, now);
  assert.equal(update.$set, undefined);
});

test("collector rejects invalid requests before opening the database", async () => {
  for (const event of [
    { httpMethod: "GET" },
    { httpMethod: "POST", body: "{" },
    { httpMethod: "POST", body: "x".repeat(2049) },
    { httpMethod: "POST", body: JSON.stringify(snapshot()), headers: { origin: "https://evil.test" } },
  ]) assert.ok((await handler(event)).statusCode >= 400);
});

test("report defaults to studio and client identity cannot be spoofed", async () => {
  const { visitScopeMatch, visitGroup, visitReportPipeline, CLIENT_VISIT_PROPERTIES } = await import("../public/aesthetic.computer/lib/visit-model.mjs");
  const studio = visitScopeMatch().property.$in;
  for (const property of CLIENT_VISIT_PROPERTIES) {
    assert.ok(!studio.includes(property));
    assert.equal(visitGroup(property), "clients");
    const visit = validateVisit({ ...snapshot(), group: "studio" }, `https://${property}`);
    assert.equal(visitUpdate(visit).$setOnInsert.group, "clients");
  }
  assert.deepEqual(visitScopeMatch("clients").property.$in, [...CLIENT_VISIT_PROPERTIES]);
  assert.ok(visitScopeMatch("all").property.$in.includes("jas.life"));
  assert.throws(() => visitScopeMatch("typo"));
  for (const byPeriod of [false, true]) {
    const pipeline = visitReportPipeline(new Date(0), new Date(), byPeriod);
    assert.deepEqual(pipeline[0].$match.property.$in, studio);
  }
  for (const host of ["labs.regarde.io", "draft.regarde.io", "builds.false.work", "xbq1m1-qa.myshopify.com"])
    assert.equal(visitProperty(host), null);
});

test("Shopify tracking follows analytics consent, including revocation during loading", async () => {
  const { startShopifyVisits } = await import("../public/aesthetic.computer/lib/visit-shopify.mjs");
  let allowed = false, starts = 0, stops = 0, resolve;
  const doc = new EventTarget();
  const win = { Shopify: { customerPrivacy: { analyticsProcessingAllowed: () => allowed } } };
  const module = { startVisitTracker() { starts++; win.acVisits = { stop() { stops++; delete win.acVisits; } }; } };
  let pending = new Promise(r => { resolve = r; });
  startShopifyVisits(win, doc, () => pending);
  const tick = () => new Promise(r => setImmediate(r));
  assert.equal(win.acVisitTrackingDisabled, true);
  allowed = true; doc.dispatchEvent(new Event("visitorConsentCollected"));
  allowed = false; doc.dispatchEvent(new Event("visitorConsentCollected"));
  resolve(module); await tick(); assert.equal(starts, 0);
  allowed = true; doc.dispatchEvent(new Event("visitorConsentCollected"));
  await tick(); assert.equal(starts, 1); assert.equal(win.acVisitTrackingDisabled, false);
  allowed = false; doc.dispatchEvent(new Event("visitorConsentCollected"));
  assert.equal(stops, 1); assert.equal(win.acVisitTrackingDisabled, true);
  allowed = true; doc.dispatchEvent(new Event("visitorConsentCollected"));
  await tick(); assert.equal(starts, 2);
});
