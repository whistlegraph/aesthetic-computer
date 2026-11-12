import test from "node:test";
import assert from "node:assert/strict";
import { curationMenu, curatedModel } from "../backend/aesel-curation.mjs";
import { createHandler } from "../netlify/functions/easel-credits.mjs";
const SONNET = "anthropic/claude-sonnet-5.5", PRO = "deepseek/deepseek-v4-pro";
test("a curated menu keeps known models and a default among them", () => {
  assert.equal(curationMenu(null), null);
  assert.equal(curationMenu({ models: ["nope/model"] }), null);
  assert.deepEqual(curationMenu({ models: [SONNET, PRO, "nope/model"], default: "x", by: "@jeffrey" }),
    { models: [SONNET, PRO], default: SONNET, by: "@jeffrey", note: "" });
});
test("inference under a menu: unnamed takes the default, off-menu is refused", () => {
  const menu = curationMenu({ models: [SONNET, PRO], by: "@jeffrey" });
  assert.equal(curatedModel(menu, undefined), SONNET);
  assert.equal(curatedModel(menu, PRO), PRO);
  assert.throws(() => curatedModel(menu, "anthropic/claude-opus-5.5"), /curated by @jeffrey: Claude Sonnet 5.5, DeepSeek V4 Pro/);
  assert.equal(curatedModel(null, "anthropic/claude-opus-5.5"), "anthropic/claude-opus-5.5");
});
test("the credits reply carries the curation, and a failed lookup leaves it open", async () => {
  const base = { authorize: async () => ({ sub: "u" }), getHandleOrEmail: async () => "@t",
    checkBudget: async () => ({ day: "2026-11-12", used: 0, budget: 1, remaining: 1 }), paidBalance: async () => 0 };
  const event = { httpMethod: "GET", headers: { authorization: "Bearer x" } };
  const menu = { models: [SONNET], default: SONNET, by: "@jeffrey", note: "" };
  assert.deepEqual(JSON.parse((await createHandler({ ...base, curation: async () => menu })(event)).body).curation, menu);
  const failed = await createHandler({ ...base, curation: async () => { throw new Error("down"); } })(event);
  assert.equal(failed.statusCode, 200); assert.equal(JSON.parse(failed.body).curation, null);
});
