import test from "node:test";
import assert from "node:assert/strict";
import { startSotceActivity, sotceResponseAction } from "../public/aesthetic.computer/lib/sotce-activity.mjs";

test("Sotce milestones require successful saved operations, not a click, duplicate touch or error", () => {
  assert.equal(sotceResponseAction("POST", "/sotce-net/touch-a-page", 200, { touchCreated: true }), "sotce_page_touched");
  for (const result of [{}, { touchCreated: false }, { touches: ["@someone"] }])
    assert.equal(sotceResponseAction("POST", "/sotce-net/touch-a-page", 200, result), null);
  assert.equal(sotceResponseAction("POST", "/sotce-net/touch-a-page", 500, { touchCreated: true }), null);
  assert.equal(sotceResponseAction("POST", "/sotce-net/ask", 200, { success: true, question: "never forwarded" }), "sotce_question_submitted");
  assert.equal(sotceResponseAction("POST", "/sotce-net/ask", 403, { success: true }), null);
  assert.equal(sotceResponseAction("GET", "/sotce-net/asks", 200, { success: true }), null);
});

test("reading measures foreground display, skips editors and prefetch, and never emits page keys", () => {
  let callback, now = 0, page = null, hidden = false;
  const actions = [];
  const classes = { contains: () => hidden };
  const doc = { visibilityState: "visible", body: { classList: classes }, documentElement: { classList: classes } };
  const win = { performance: { now: () => now }, acSotceVisiblePage: () => page,
    acAccountActivity: { action: (...args) => actions.push(args) },
    setInterval: fn => { callback = fn; return 1; }, clearInterval: () => { callback = null; } };
  const api = startSotceActivity(win, doc);
  assert.equal(startSotceActivity(win, doc), api);
  const tick = (ms = 1000) => { now += ms; callback(); };
  for (let i = 0; i < 40; i++) tick();
  assert.equal(actions.length, 0, "no rendered page, no reading evidence");
  page = "private-page-id"; tick(); tick();
  assert.equal(actions.length, 0);
  tick(); assert.deepEqual(actions, [["sotce_page_viewed"]]);
  doc.visibilityState = "hidden";
  for (let i = 0; i < 40; i++) tick();
  doc.visibilityState = "visible"; tick(60000);
  assert.equal(actions.length, 1, "background/sleep gaps are excluded");
  hidden = true; for (let i = 0; i < 40; i++) tick();
  hidden = false; tick(); for (let i = 0; i < 28; i++) tick();
  assert.deepEqual(actions, [["sotce_page_viewed"], ["sotce_page_visible_30s"]]);
  page = "next-private-page"; tick(); tick(); tick();
  assert.equal(actions.at(-1)[0], "sotce_page_viewed");
  api.response("POST", "/sotce-net/ask", 200, { success: true, _id: "secret", question: "secret" });
  assert.deepEqual(actions.at(-1), ["sotce_question_submitted"]);
  assert.doesNotMatch(JSON.stringify(actions), /secret|private/);
  api.stop(); assert.equal(callback, null);
});
