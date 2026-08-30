import assert from "node:assert/strict";
import test from "node:test";

import {
  deletedReason,
  recordDeleted,
  reportFor,
  trimCandidates,
} from "./trim.mjs";

const now = Date.parse("2026-08-30T16:00:00.000Z");

function post(mediaId, publishedAt, views, extra = {}) {
  return {
    mode: "live",
    id: `reel-${mediaId}`,
    mediaId,
    publishedAt,
    insights: views == null ? null : {
      views,
      ig_reels_avg_watch_time: 4200,
      reels_skip_rate: 51.2,
    },
    ...extra,
  };
}

test("trim candidates are measured, sub-500, 24h–30d live reels", () => {
  const eligibleOld = post("old", "2026-08-02T16:00:00.000Z", 499);
  const eligibleNew = post("new", "2026-08-29T16:00:00.000Z", 0);
  const rows = [
    eligibleNew,
    post("exactly-500", "2026-08-20T16:00:00.000Z", 500),
    post("fresh", "2026-08-29T16:00:00.001Z", 12),
    post("expired", "2026-07-31T15:59:59.999Z", 12),
    post("unmeasured", "2026-08-20T16:00:00.000Z", null),
    post("deleted", "2026-08-20T16:00:00.000Z", 12, { deletedAt: "done" }),
    post("draft", "2026-08-20T16:00:00.000Z", 12, { mode: "dry-run" }),
    post("invalid", "not-a-date", 12),
    eligibleOld,
  ];

  assert.deepEqual(trimCandidates(rows, { now }), [eligibleOld, eligibleNew]);
});

test("the report carries exact ids and useful review metrics", () => {
  const ledger = { posts: [post("123", "2026-08-20T16:00:00.000Z", 42)] };
  assert.deepEqual(reportFor(ledger, { now }), [{
    id: "reel-123",
    mediaId: "123",
    publishedAt: "2026-08-20T16:00:00.000Z",
    ageHours: 240,
    views: 42,
    avgWatchMs: 4200,
    skipRate: 51.2,
  }]);
});

test("recording requires confirmation and keeps every ledger row", () => {
  const target = post("123", "2026-08-20T16:00:00.000Z", 42);
  const keeper = post("456", "2026-08-20T16:00:00.000Z", 900);
  const ledger = { format: "test", posts: [target, keeper] };
  const at = "2026-08-30T16:00:00.000Z";

  assert.throws(() => recordDeleted(ledger, ["123"], { at }),
    /confirmed-web-delete/);
  const recorded = recordDeleted(ledger, ["123"], { at, confirmed: true });

  assert.equal(recorded.posts.length, ledger.posts.length);
  assert.equal(recorded.posts[0].deletedAt, at);
  assert.equal(recorded.posts[0].deletedReason, deletedReason);
  assert.strictEqual(recorded.posts[1], keeper);
  assert.equal(target.deletedAt, undefined);
});

test("recording refuses unknown, protected, or stale ids atomically", () => {
  const target = post("123", "2026-08-20T16:00:00.000Z", 42);
  const ledger = {
    posts: [
      target,
      post("keeper", "2026-08-20T16:00:00.000Z", 900),
      post("unmeasured", "2026-08-20T16:00:00.000Z", null),
    ],
  };
  const options = {
    at: "2026-08-30T16:00:00.000Z",
    confirmed: true,
  };

  for (const mediaId of ["missing", "keeper", "unmeasured"])
    assert.throws(() => recordDeleted(ledger, ["123", mediaId], options),
      /not eligible for trim/);
  assert.equal(target.deletedAt, undefined);
});
