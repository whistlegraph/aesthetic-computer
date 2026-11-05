import { test } from "node:test";
import assert from "node:assert/strict";
import { CLASSIC_COLLECTION, TEACHER_COLLECTION, nopaintReport, nopaintReportOptions, nopaintSummary } from "./nopaint.mjs";

// A stand-in database: answers each read with canned rows and keeps what was asked.
function fakeDb() {
  const calls = [];
  const rows = {
    clicks: [{ _id: { automated: false }, visitsWithLink: 100, notepaintOpened: 6, adShown: 40, openedAfterShown: 4,
      acAdShown: 50, acOpened: 5, acOpenedAfterShown: 5, teacherNoteOpened: 3, teacherNoteSent: 1 },
      { _id: { automated: true }, visitsWithLink: 9, adShown: 9, openedAfterShown: 9 }],
    destinations: [{ _id: "notepaint.art", arrivals: 4, engaged: 2, stayed60: 1, painting_edited: 1 },
      { _id: "aesthetic.computer", arrivals: 5, engaged: 3, stayed60: 2, canvas_interacted: 2 }],
    arrivals: [{ _id: { automated: false, from: "nopaint.art" }, visits: 4, interacted: 3, note_written: 2, paint_pressed: 2, painting_edited: 1, braincells_sheet_shown: 1 },
      { _id: { automated: false, from: "direct or unknown" }, visits: 10, braincells_sheet_shown: 2, braincells_purchased: 1 }],
    saves: [{ _id: "2026-11-03", saves: 200, first: new Date("2026-11-03T13:05:00Z"), last: new Date("2026-11-03T21:00:00Z") },
      { _id: "2026-11-04", saves: 131, first: new Date("2026-11-04T13:00:00Z"), last: new Date("2026-11-04T19:40:00Z") }],
    reservations: [{ _id: null, reserved: 345, reservedUnsaved: 14 }],
    notes: [{ createdAt: new Date("2026-11-04T15:00:00Z"), school: "IE 1234", lang: "es" }],
  };
  const cursor = value => ({ sort() { return this; }, limit() { return this; }, toArray: async () => value });
  const db = { collection: name => ({
    aggregate(pipeline) {
      calls.push({ name, pipeline });
      const match = pipeline[0].$match;
      if (name === CLASSIC_COLLECTION) return cursor(match.savedAt ? rows.saves : rows.reservations);
      if (match.property === "notepat.com") return cursor([]);
      if (match.property === "nopaint.art") return cursor(rows.clicks);
      return cursor(match.$expr ? rows.destinations : rows.arrivals);
    },
    find(query, options) { calls.push({ name, query, options }); return cursor(rows.notes); },
  }) };
  return { db, calls };
}

test("options bound the window and reject out-of-range hours", () => {
  const { start, end } = nopaintReportOptions({ hours: "48", end: "2026-11-04T12:00:00Z" });
  assert.equal(end.toISOString(), "2026-11-04T12:00:00.000Z");
  assert.equal(start.toISOString(), "2026-11-02T12:00:00.000Z");
  assert.throws(() => nopaintReportOptions({ hours: 0 }));
  assert.throws(() => nopaintReportOptions({ hours: 841 }));
  assert.throws(() => nopaintReportOptions({ end: "nope" }));
});

test("classic saves fold per UTC day with counts only; teacher notes carry school and language only", async () => {
  const { db, calls } = fakeDb();
  const report = await nopaintReport(db, { hours: 48, end: "2026-11-04T20:00:00Z" });
  assert.deepEqual(report.classicSaves, { total: 331, perDay: { "2026-11-03": 200, "2026-11-04": 131 },
    firstSavedAt: new Date("2026-11-03T13:05:00Z"), lastSavedAt: new Date("2026-11-04T19:40:00Z"), reserved: 345, reservedUnsaved: 14 });
  assert.deepEqual(report.teacherNotes, { count: 1, truncated: false,
    notes: [{ day: "2026-11-04", school: "IE 1234", lang: "es" }], formOpened: 3, formSent: 1 });

  // The classic pipelines group away each code and never project the image.
  for (const { pipeline } of calls.filter(call => call.name === CLASSIC_COLLECTION)) {
    assert.ok(pipeline.every(stage => !stage.$project));
    assert.doesNotMatch(JSON.stringify(pipeline), /png|sha256/);
    assert.ok(pipeline.some(stage => stage.$group));
  }
  const teacher = calls.find(call => call.name === TEACHER_COLLECTION);
  assert.deepEqual(Object.keys(teacher.options.projection).sort(), ["_id", "createdAt", "lang", "school"]);
  assert.equal(teacher.options.projection._id, 0);
});

test("the sticker split and notepaint funnel count human visits only", async () => {
  const { db } = fakeDb();
  const report = await nopaintReport(db, { hours: 24 });
  const [notepaint, ac] = report.stickerSplit;
  assert.deepEqual(notepaint, { arm: "notepaint", shown: 40, opened: 4, clickThroughPercent: 10, arrivals: 4, engaged: 2,
    stayed60: 1, made: 1, engagedPerShownPercent: 5, madePerShownPercent: 2.5 });
  assert.equal(ac.made, 2);
  assert.equal(ac.clickThroughPercent, 10);
  assert.equal(report.nopaintClickThroughOfAllLinkVisitsPercent, 6);
  assert.equal(report.notepaintFunnelFromNopaint.note_written, 2);
  assert.equal(report.notepaintFunnelFromNopaint.piece_made, 0);
  assert.deepEqual(report.braincellsFunnel, { visits: 14, braincells_sheet_shown: 3, braincells_limit_reached: 0,
    braincells_checkout_opened: 0, braincells_purchased: 1 });

  const summary = nopaintSummary(report);
  assert.equal(summary.nopaint, undefined);
  assert.equal(summary.notepaintArrivals, undefined);
  assert.equal(summary.classicSaves.total, 331);
});
