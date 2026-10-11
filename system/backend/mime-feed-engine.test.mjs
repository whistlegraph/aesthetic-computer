import test from "node:test";
import assert from "node:assert/strict";
import { rankFeed, archiveOrder, nopaintArchive, seeded } from "./mime-feed-engine.mjs";

const now = Date.parse("2026-10-11T12:00:00Z");
const day = 86400000;
const live = [
  ...Array.from({ length: 6 }, (_, i) => ({ code: "painting_" + i, app: "aesthetic", author: "@ana", when: new Date(now - i * 3600000) })),
  ...Array.from({ length: 4 }, (_, i) => ({ code: "mood_" + i, app: "mood", author: "@bo", when: new Date(now - i * 7200000) })),
  { code: "wgpiece_old", app: "whistlegraph", author: "@cy", when: new Date(now - 60 * day) },
  { code: "wgpiece_loved", app: "whistlegraph", author: "@cy", when: new Date(now - 60 * day), dwell: 30 },
];
const archive = Array.from({ length: 30 }, (_, i) => ({ code: "nopaint_" + i, app: "nopaint", author: null, when: "2022-01-01", style: i % 3 ? "abstract" : "portrait" }));

test("a visit's order is stable, and different visits differ", () => {
  const a = rankFeed({ live, archive }, "visit-a", 20, now).map((x) => x.code);
  assert.deepEqual(rankFeed({ live, archive }, "visit-a", 20, now).map((x) => x.code), a);
  assert.notDeepEqual(rankFeed({ live, archive }, "visit-b", 20, now).map((x) => x.code), a);
});

test("archive is braided in, roughly one card in three", () => {
  const feed = rankFeed({ live, archive }, "s", 14, now);
  const archived = feed.filter((x) => x.app === "nopaint").length;
  assert.ok(archived >= 3 && archived <= 6, `archive cards: ${archived}`);
});

test("no author twice in a row while there is a choice", () => {
  // 12 cards: every author still has work left to choose from.
  const feed = rankFeed({ live, archive }, "s", 12, now);
  for (let i = 1; i < feed.length; i++) {
    if (feed[i].author) assert.notEqual(feed[i].author, feed[i - 1].author, `cards ${i - 1} and ${i}`);
  }
});

test("fresh work outranks month-old work, and lingering lifts old work", () => {
  const feed = rankFeed({ live, archive: [] }, "s", live.length, now).map((x) => x.code);
  assert.ok(feed.indexOf("wgpiece_loved") < feed.indexOf("wgpiece_old"));
  assert.ok(feed.indexOf("painting_0") < feed.indexOf("wgpiece_old"));
});

test("once live work runs out the archive keeps going", () => {
  const feed = rankFeed({ live, archive }, "s", live.length + archive.length, now);
  assert.equal(feed.length, live.length + archive.length);
  assert.equal(new Set(feed.map((x) => x.code)).size, feed.length);
});

test("favored archive styles come twice as often as abstracts", () => {
  const order = archiveOrder(archive, "s").slice(0, 9);
  assert.equal(order.filter((x) => x.style === "portrait").length, 6);
});

test("the real No Paint archive loads without blanks", () => {
  const items = nopaintArchive(new URL("..", import.meta.url).pathname);
  assert.ok(items.length > 29000);
  assert.ok(items.every((x) => x.style !== "blank" && x.post.file.url.startsWith("https://pix.nopaint.art/")));
});

test("seeded values spread across 0..1", () => {
  const values = Array.from({ length: 1000 }, (_, i) => seeded("s", i));
  assert.ok(Math.min(...values) < 0.05 && Math.max(...values) > 0.95);
});
