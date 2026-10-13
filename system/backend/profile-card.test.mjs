import test from "node:test";
import assert from "node:assert/strict";
import { createHandler } from "../netlify/functions/profile-card.mjs";
import { profilePage } from "./profile-page.mjs";
import { boot, leave } from "../public/aesthetic.computer/disks/profile.mjs";

function fixture({ missing = false, fail = false } = {}) {
  const pipelines = [], matches = [];
  let disconnected = 0;
  const db = { collection(name) {
    if (name === "@handles") return {
      async findOne(query, options) {
        assert.ok(query.handle === "jeffrey" || query.handle.$regex === "^jeffrey$");
        assert.deepEqual(options.projection, { handle: 1, colors: 1 });
        return missing ? null : { _id: "private-user-id", handle: "jeffrey" };
      },
      find() { return { async toArray() { return [{ _id: "private-user-id", handle: "jeffrey" }]; } }; },
    };
    if (name === "moods") return { async countDocuments(query) { matches.push(query); return 288; }, find(query) {
      assert.equal(query.user, "private-user-id");
      assert.deepEqual(query.deleted, { $ne: true });
      return { sort() { return this; }, limit() { return this; }, async toArray() {
        return [{ mood: "hello\\nworld", when: "2026-10-12", secret: "never return" }];
      } };
    } };
    return { async countDocuments(query) { matches.push(query); return name === "paintings" ? 410 : 22; }, aggregate(pipeline) {
      pipelines.push({ name, pipeline });
      return { async toArray() {
        if (fail) throw new Error("database unavailable");
        if (name !== "paintings") return [];
        return Array.from({ length: 1 }, (_, i) => ({ code: "painting_" + i, board: "image/png", when: "2026-10-12",
          _media: { kind: "painting", id: String(i), code: "f" + i, user: "private-user-id" } }));
      } };
    } };
  } };
  return { pipelines, matches, get disconnected() { return disconnected; },
    handler: createHandler(async () => ({ db, disconnect() { disconnected++; } })) };
}
const get = (handler, query = {}) => handler({ httpMethod: "GET", queryStringParameters: { handle: "@Jeffrey", ...query } });

test("bounded public profile uses one identity and never exposes private fields", async () => {
  const f = fixture(), response = await get(f.handler), data = JSON.parse(response.body);
  assert.equal(response.statusCode, 200);
  assert.equal(data.handle, "@jeffrey");
  assert.equal(data.counts.painting, 410);
  assert.equal(data.counts.mood, 288);
  assert.equal(data.latestPainting.media.code, "f0");
  assert.equal(data.onlineTimeMs, null);
  assert.equal(data.posts, undefined);
  assert.equal(f.pipelines.length, 1);
  assert.equal(f.matches.length, 6);
  assert.equal(data.mood.text, "hello\nworld");
  assert.ok(!response.body.includes("private-user-id") && !response.body.includes("never return"));
  assert.equal(f.disconnected, 1);
  for (const { pipeline } of f.pipelines) {
    assert.equal(pipeline[0].$match.user, "private-user-id");
    for (const field of ["nuked", "deleted", "private", "draft", "hidden"]) assert.deepEqual(pipeline[0].$match[field], { $ne: true });
    assert.deepEqual(pipeline[0].$match.visibility, { $in: [null, "public"] });
    assert.deepEqual(pipeline[2], { $limit: 1 });
    assert.ok(!pipeline.some((stage) => stage.$lookup));
  }
});

test("membership fetch resolves the same identity without scanning media", async () => {
  const handler = createHandler(async () => ({ db: { collection(name) {
    assert.equal(name, "@handles");
    return { async findOne() { return { _id: "private-user-id", handle: "jeffrey" }; } };
  } } }), async user => {
    assert.equal(user, "private-user-id");
    return { registeredAt: "2023-02-17", loginCount: 814 };
  });
  const result = await get(handler, { membership: "1" });
  assert.equal(result.statusCode, 200);
  assert.deepEqual(JSON.parse(result.body), { registeredAt: "2023-02-17", loginCount: 814 });
});

test("invalid requests are rejected before opening a database connection", async () => {
  const handler = createHandler(() => { throw new Error("must not connect"); });
  for (const query of [{ membership: "2" }, { handle: "x.*" }, { handle: "mail@example.com" }, { kind: "secrets" }, { page: "1" },
    { kind: "painting", page: "-1" }, { kind: "painting", page: "1.5" }, { kind: "painting", page: "501" }]) {
    assert.equal((await get(handler, query)).statusCode, 400);
  }
  assert.equal((await handler({ httpMethod: "POST" })).statusCode, 405);
});

test("missing profiles and failed queries release the connection", async () => {
  const missing = fixture({ missing: true });
  assert.equal((await get(missing.handler)).statusCode, 404);
  assert.equal(missing.pipelines.length, 0);
  assert.equal(missing.disconnected, 1);
  const failed = fixture({ fail: true });
  assert.equal((await get(failed.handler)).statusCode, 503);
  assert.equal(failed.disconnected, 1);
});

test("direct profile documents skip AC startup and preserve piece routes", () => {
  for (const route of ["/@jeffrey", "/%40Jeffrey/"]) {
    const html = profilePage(route);
    assert.match(html, /<ac-profile handle="@jeffrey">/);
    assert.ok(!html.includes("bios.mjs") && !html.includes("boot.mjs"));
  }
  for (const route of ["/@jeffrey/piece", "/prompt", "/@<script>", "/%xx"]) assert.equal(profilePage(route), null);
  assert.match(profilePage("/@a.b"), /handle="@a.b"/);
});

test("handle lookup prefers exact spelling and escapes dots in the legacy fallback", async () => {
  const calls = [];
  const handler = createHandler(async () => ({ db: { collection(name) {
    assert.equal(name, "@handles");
    return { async findOne(query) { calls.push(query); return null; } };
  } } }));
  assert.equal((await get(handler, { handle: "@a.b" })).statusCode, 404);
  assert.deepEqual(calls, [{ handle: "a.b" }, { handle: { $regex: "^a\\.b$", $options: "i" } }]);
});

test("the in-AC profile mounts the same component and cleans up when leaving", () => {
  let markup, rewritten, cleared = false;
  boot({ params: ["@a.b"], handle: () => "@jeffrey", hud: { label() {} },
    net: { rewrite(value) { rewritten = value; } }, dom: {
      html(parts, ...values) { markup = parts.reduce((out, part, i) => out + part + (values[i] ?? ""), ""); },
      clear() { cleared = true; },
    } });
  assert.equal(rewritten, "@a.b");
  assert.match(markup, /<ac-profile handle="@a.b">/);
  assert.match(markup, /profile-piece.mjs/);
  leave();
  assert.equal(cleared, true);
});
