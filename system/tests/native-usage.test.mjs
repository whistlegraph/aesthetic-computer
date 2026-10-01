import test from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { validateNativeUsage, nativeUsageWrite, summarizeNativeUsage } from "../backend/native-usage.mjs";
import { handler } from "../netlify/functions/app-session.mjs";
import { rollupMissingDays } from "../backend/metrics-daily.mjs";

const now = new Date("2026-10-01T12:00:30Z");
const sample = () => ({ schema: 1, app: "aestheticcomputer", platform: "ios", version: "1.2", build: "5",
  install: randomUUID(), session: randomUUID(), startedAt: "2026-10-01T12:00:00Z",
  firstObserved: true, ready: true, interacted: true, activeSeconds: 20 });

test("collector bounds dates, time and identities and discards private extras", () => {
  const input = sample();
  for (const bad of [{ platform: "mac" }, { schema: 2 }, { session: "bad" }, { activeSeconds: -1 },
    { activeSeconds: 1000 }, { activeSeconds: 1.5 }, { ready: "true" },
    { startedAt: "2026-09-01T12:00:00Z" }, { startedAt: "2026-10-02T12:00:00Z" }])
    assert.equal(validateNativeUsage({ ...input, ...bad }, now), null);
  const value = validateNativeUsage({ ...input, email: "private", apns: "private", url: "private" }, now);
  assert.ok(value);
  assert.ok(!JSON.stringify(nativeUsageWrite(value, now)).includes("private"));
});

test("offline replay has a stable row, max counters, and event-day retention", () => {
  const input = sample(), nextDay = new Date(+now + 86400000);
  const a = nativeUsageWrite(validateNativeUsage(input, now), now);
  const b = nativeUsageWrite(validateNativeUsage({ ...input, activeSeconds: 25 }, nextDay), nextDay);
  assert.equal(a.id, b.id);
  assert.equal(b.update.$setOnInsert.day, "2026-10-01");
  assert.equal(+b.update.$setOnInsert.expiresAt - +b.update.$setOnInsert.startedAt, 35 * 86400000);
  assert.equal(b.update.$max.activeSeconds, 25);
  assert.equal(b.update.$inc, undefined);
  assert.equal(b.update.$max.ready, true);
});

test("report counts unique installs and return days without conflating sessions", () => {
  const row = (install, day, extra = {}) => ({ _id: { app: "aestheticcomputer", install, day },
    opens: 1, activeSeconds: 20, loaded: 1, interacted: 1, engaged: 1, platform: "ios", ...extra });
  const report = summarizeNativeUsage([
    row("a", "2026-09-30", { opens: 2, firstObserved: true }), row("a", "2026-10-01"),
    row("b", "2026-10-01", { activeSeconds: 0, loaded: 0, interacted: 0, engaged: 0, firstObserved: true }),
  ], new Date("2026-09-30"), now).apps.aestheticcomputer;
  assert.equal(report.activeInstalls, 2);
  assert.equal(report.returningInstalls, 1);
  assert.equal(report.opens, 4);
  assert.equal(report.firstObservedInstalls, 2);
  assert.equal(report.engagedSessions, 2);
  assert.equal(report.daily["2026-10-01"].activeInstalls, 2);
});

test("invalid HTTP requests never need a database", async () => {
  for (const event of [{ httpMethod: "GET" }, { httpMethod: "POST", body: "{" },
    { httpMethod: "POST", body: "x".repeat(1025) }, { httpMethod: "POST", body: "{}" }])
    assert.ok((await handler(event)).statusCode >= 400);
});

test("late iOS snapshots refresh daily totals without rewriting web or legacy app counts", async () => {
  const updates = [];
  const days = Array.from({ length: 8 }, (_, n) => `2026-09-${23 + n}`);
  const db = { collection(name) {
    if (name === "metrics-daily") return {
      distinct: async () => days,
      updateOne: async (filter, update) => updates.push({ filter, update }),
    };
    assert.equal(name, "native-app-sessions", "Existing web/desktop totals must not be recomputed");
    return { aggregate(pipeline) { return { toArray: async () => {
      const day = pipeline[0].$match.startedAt.$gte.toISOString().slice(0, 10);
      return day === "2026-09-25" ? [{ _id: { app: "aestheticcomputer", install: "test", day },
        opens: 2, activeSeconds: 50, loaded: 2, interacted: 1, engaged: 1, firstObserved: true, platform: "ios" }] : [];
    } }; } };
  } };
  await rollupMissingDays(db, new Date("2026-10-01T12:00:00Z"));
  assert.equal(updates.length, 8);
  for (const { update } of updates) assert.deepEqual(Object.keys(update.$set), ["nativeUsage"]);
  const counts = updates.find(x => x.filter._id === "2026-09-25").update.$set.nativeUsage.aestheticcomputer;
  assert.equal(counts.opens, 2); assert.equal(counts.activeInstalls, 1); assert.equal(counts.activeSeconds, 50);
  assert.ok(!JSON.stringify(counts).includes("test"));
});
