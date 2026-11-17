// node --test social/lib/pulse.test.mjs
import assert from "node:assert/strict";
import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import { collapseThreads, listGroups, matcher, mergeCrossPosts, readGroup, saveGroup, table, xQueries } from "./pulse.mjs";

test("matcher keeps whole words and phrases, in text, quote or link", () => {
  const keep = matcher(["NSA", "apple pay"]);
  assert.equal(keep({ text: "the NSA network is down" }), true);
  assert.equal(keep({ text: "an unsafe number of people" }), false);
  assert.equal(keep({ text: "lol", quoted: "Apple Pay is failing" }), true);
  assert.equal(keep({ text: "apple pie" }), false);
  assert.equal(matcher([])({ text: "anything" }), true);
});

test("xQueries packs handles under the 512-character limit", () => {
  const handles = Array.from({ length: 60 }, (_, i) => `handle_number_${i}`);
  const queries = xQueries(handles, ["outage", "apple pay"]);
  assert.ok(queries.length > 1);
  for (const query of queries) {
    assert.ok(query.length <= 512, `${query.length} characters`);
    assert.ok(query.endsWith('(outage OR "apple pay") -is:retweet'));
  }
  assert.equal(handles.every((h) => queries.some((q) => q.includes(`from:${h} `) || q.includes(`from:${h})`))), true);
  assert.deepEqual(xQueries([], ["x"]), []);
});

test("saveGroup adds, updates by name and removes", () => {
  const home = mkdtempSync(join(tmpdir(), "pulse-"));
  saveGroup({ slug: "Test Group", title: "Test", keywords: ["a"], add: [{ name: "Ada", bluesky: "@ada.example" }, { name: "Bob", x: "bob" }] }, home);
  saveGroup({ slug: "test-group", add: [{ name: "ada", mastodon: "ada@example.social" }], remove: ["Bob"] }, home);
  const group = readGroup("test-group", home);
  assert.deepEqual(group.members, [{ name: "Ada", bluesky: "ada.example", mastodon: "ada@example.social" }]);
  assert.equal(listGroups(home)[0].members, 1);
  assert.throws(() => readGroup("missing", home), /no group "missing"; have: test-group/);
});

test("table prints both time zones and marks new posts", () => {
  const out = table([{ at: "2026-11-17T16:49:00.000Z", name: "Dustin Volz", role: "NYT", platform: "bluesky", likes: 12, fresh: true,
    text: "NSANet is down | badly", url: "https://bsky.app/x" }]);
  assert.match(out, /Tue Nov 17 16:49 \| Tue Nov 17 08:49 \| Dustin Volz \(NYT\) \| bluesky ·new · 12♥ \| NSANet is down \/ badly — https:\/\/bsky\.app\/x/);
  assert.equal(table([]), "(no posts)");
});

test("collapseThreads folds a burst from one person into its first post", () => {
  const post = (at, handle, likes, id) => ({ platform: "bluesky", handle, at, likes, id, fresh: false, text: id });
  const rows = collapseThreads([post("2026-11-17T15:52:00Z", "a", 2, "a1"), post("2026-11-17T15:52:20Z", "a", 9, "a2"),
    post("2026-11-17T15:53:00Z", "b", 1, "b1"), post("2026-11-17T15:53:30Z", "a", 0, "a3"), post("2026-11-17T18:00:00Z", "a", 0, "a4")]);
  assert.deepEqual(rows.map((r) => [r.id, r.more || 0, r.likes]), [["a1", 2, 9], ["b1", 0, 1], ["a4", 0, 0]]);
  assert.deepEqual(rows[0].ids, ["a1", "a2", "a3"]);
  assert.match(table([{ ...rows[0], name: "A", url: "u" }]), /\(\+2 more in thread\)/);
});

test("mergeCrossPosts keeps the best-liked copy of the same words", () => {
  const post = (platform, at, likes, text = "Geometric Shapes / 261117 #p5js") => ({ platform, name: "Saskia", at, likes, text, ids: [platform], fresh: false });
  const rows = mergeCrossPosts([post("x", "2026-11-17T17:23:00Z", 3), post("mastodon", "2026-11-17T17:24:00Z", 1),
    post("bluesky", "2026-11-17T17:24:30Z", 9), post("bluesky", "2026-11-17T20:00:00Z", 0, "something else")]);
  assert.equal(rows.length, 2);
  assert.equal(rows[0].platform, "bluesky");
  assert.deepEqual(rows[0].also.sort(), ["mastodon", "x"]);
  assert.deepEqual(rows[0].ids, ["x", "mastodon", "bluesky"]);
});
