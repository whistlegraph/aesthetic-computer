import test from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { validateVisit, visitUpdate, visitProperty, visitSurface, automatedVisit, visitMediaAction, visitLinkAction, VISIT_ACTIONS } from "../public/aesthetic.computer/lib/visit-model.mjs";
import { handler } from "../netlify/functions/visit-track.mjs";

const snapshot = () => ({ version: 1, id: randomUUID(), surface: "home",
  activeSeconds: 10, interacted: true, automated: false,
  inputs: ["pointer"], actions: ["link_followed"] });

test("Whistlegraph link milestones retain only reviewed actions and measurement coverage", () => {
  assert.equal(visitProperty("www.whistlegraph.app"), "whistlegraph.app");
  assert.equal(visitLinkAction("www.whistlegraph.org", "https://whistlegraph.app/"), "whistlegraph_app_clicked");
  assert.equal(visitLinkAction("whistlegraph.app", "mailto:mail@aesthetic.computer?body=private"), "whistlegraph_access_clicked");
  for (const href of ["https://whistlegraph.app.evil.test", "http://whistlegraph.app", "https://whistlegraph.app:1234", "https://secret@whistlegraph.app", "https://aesthetic.computer/", "invalid"])
    assert.equal(visitLinkAction("whistlegraph.org", href), null);
  assert.equal(visitLinkAction("jas.life", "https://whistlegraph.app"), null);
  assert.equal(visitLinkAction("whistlegraph.app", "mailto:other@example.com"), null);
  const visit = validateVisit({ ...snapshot(), linkVersion: 1, actions: ["whistlegraph_access_clicked"] }, "https://whistlegraph.app");
  assert.equal(visitUpdate(visit).$max.linkVersion, 1);
  assert.equal(visitUpdate(visit).$max["actions.whistlegraph_access_clicked"], true);
  assert.equal(visitUpdate(validateVisit(snapshot(), "https://whistlegraph.org")).$max.linkVersion, undefined,
    "old visits without link measurement must not enter a click-rate denominator");
  assert.equal(validateVisit({ ...snapshot(), linkVersion: 2 }, "https://whistlegraph.app"), null);
});

test("creation milestones require a confirmed media record and carry no content", () => {
  for (const result of [null, {}, { slug: "upload-only" }, { code: "" }, { code: 123 }, { code: "abc", error: "failed" }])
    assert.equal(visitMediaAction("png", result), null);
  assert.equal(visitMediaAction("png", { code: "abc", slug: "private-name" }), "painting_saved");
  for (const ext of ["zip", "mp4", "webm"])
    assert.equal(visitMediaAction(ext, { code: "abc" }), "tape_saved");
  assert.equal(visitMediaAction("mjs", { code: "abc" }), null);
  const value = snapshot();
  value.actions = [...VISIT_ACTIONS];
  assert.ok(Buffer.byteLength(JSON.stringify(value)) < 2048, "all action flags fit the collector limit");
  const visit = validateVisit(value, "https://aesthetic.computer");
  assert.ok(visit);
  assert.equal(visitUpdate(visit).$max["actions.painting_saved"], true);
  assert.equal(validateVisit({ ...value, interacted: false }, "https://aesthetic.computer"), null);
});

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
  assert.equal(automatedVisit({ userAgent: "Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko; compatible; GoogleOther) Chrome/153.0.8010.52 Safari/537.36" }), true);
  assert.equal(automatedVisit({ userAgent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/145.0.0.0 Safari/537.36 (compatible; meta-webindexer/1.1)" }), true);
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

test("MIME controls record only reviewed visit milestones", () => {
  const visit = validateVisit({ ...snapshot(), actions: ["mime_interact", "mime_scroll_feed", "mime_original_open"] }, "https://mime.ac");
  assert.ok(visit);
  const update = visitUpdate(visit);
  assert.equal(update.$max["actions.mime_interact"], true);
  assert.equal(update.$max["actions.mime_scroll_feed"], true);
  assert.equal(update.$max["actions.mime_original_open"], true);
});
