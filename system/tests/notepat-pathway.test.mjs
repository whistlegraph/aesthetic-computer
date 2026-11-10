import test from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { readFile } from "node:fs/promises";
import vm from "node:vm";
import { startVisitTracker } from "../public/aesthetic.computer/lib/visit-tracker.mjs";
import { notepatVisitContext, validateVisit, visitUpdate, visitLinkAction, VISIT_ACTIONS } from "../public/aesthetic.computer/lib/visit-model.mjs";
import { notepatPathway, notepatPipeline } from "../../toolchain/analytics/notepat.mjs";

function harness(hostname = "notepat.com") {
  let now = 0, poll;
  const target = () => {
    const listeners = new Map();
    return { addEventListener(type, fn) { listeners.set(type, [...(listeners.get(type) || []), fn]); },
      removeEventListener(type, fn) { listeners.set(type, (listeners.get(type) || []).filter(f => f !== fn)); },
      emit(type, event) { for (const fn of listeners.get(type) || []) fn(event); } };
  };
  const sent = [], doc = { ...target(), visibilityState: "visible", querySelectorAll: () => [], querySelector: () => null,
    referrer: "https://nopaint.art/?private=secret", hasFocus: () => true };
  const win = { ...target(), navigator: {}, crypto: { randomUUID }, performance: { now: () => now },
    location: { hostname, pathname: "/", search: "?ac_campaign=nopaint-music&private=secret" },
    fetch: async (_url, options) => { sent.push(JSON.parse(options.body)); return { ok: true }; },
    setInterval: fn => { poll = fn; return 1; }, clearInterval() {} };
  win.top = win;
  const api = startVisitTracker(win, doc);
  const interact = (isTrusted = true) => win.emit("keydown", { isTrusted, target: {} });
  return { win, doc, api, sent, interact, advance(ms, step = 1000) {
    const end = now + ms;
    while (now < end) { now = Math.min(end, now + step); poll(); }
  } };
}

test("Notepat readiness precedes interaction; manual milestones stay bounded and private", () => {
  const { api, sent, interact, win, doc } = harness();
  assert.equal(sent[0].campaign, "nopaint-music");
  assert.equal(sent[0].notepatReady, false);
  assert.equal(api.action("notepat_ready"), true);
  assert.equal(sent.at(-1).interacted, false);
  assert.equal(sent.at(-1).notepatReady, true);
  assert.equal(api.action("notepat_note_played"), false);
  interact(false);
  assert.equal(api.action("notepat_note_played"), false);
  interact();
  for (let n = 0; n < 7; n++) api.action("notepat_note_played");
  assert.ok(sent.at(-1).actions.includes("notepat_note_played"));
  assert.ok(sent.at(-1).actions.includes("note_played"));
  assert.ok(!sent.at(-1).actions.includes("notepat_eight_notes"));
  doc.visibilityState = "hidden";
  assert.equal(api.action("notepat_note_played"), false);
  doc.visibilityState = "visible";
  api.action("notepat_note_played");
  assert.ok(sent.at(-1).actions.includes("notepat_eight_notes"));
  const count = sent.length;
  for (let n = 0; n < 50; n++) api.action("notepat_note_played");
  assert.equal(sent.length, count, "no network event per note after milestones");
  assert.ok(sent.every(row => validateVisit(row, "https://notepat.com")));
  assert.doesNotMatch(JSON.stringify(sent), /secret|private|pitch|velocity|melody/);
  win.acVisitTrackingDisabled = true;
  assert.equal(api.action("notepat_recording_started"), false);
  assert.equal(api.action("notepat_ready"), false);
  assert.equal(sent.length, count);
});

test("private route boundaries reset note progress; other properties retain generic notes", () => {
  const { api, win, interact, sent } = harness();
  interact();
  for (let n = 0; n < 7; n++) api.action("notepat_note_played");
  const id = sent.at(-1).id;
  win.location.pathname = "/mail";
  assert.equal(api.action("notepat_ready"), false);
  assert.equal(api.action("notepat_note_played"), false);
  win.location.pathname = "/notepat";
  assert.equal(api.action("notepat_note_played"), false);
  interact(); api.action("notepat_note_played");
  assert.notEqual(sent.at(-1).id, id);
  assert.ok(!sent.at(-1).actions.includes("notepat_eight_notes"));
  const ac = harness("aesthetic.computer");
  ac.interact(); ac.api.action("notepat_note_played");
  assert.equal(ac.api.action("notepat_ready"), false);
  assert.deepEqual(ac.sent.at(-1).actions, ["note_played"]);
});

test("campaign validation and cumulative updates reject private labels and preserve load coverage", () => {
  const { sent } = harness();
  const body = sent[0];
  assert.deepEqual(notepatVisitContext("notepat.com", "?ac_campaign=secret"), { notepatVersion: 1, notepatReady: false, notepatPlayVersion: 1, notepatPlaySeconds: 0, campaign: null });
  assert.deepEqual(notepatVisitContext("jas.life", "?ac_campaign=nopaint-music"), {});
  for (const bad of [{ campaign: "secret" }, { notepatVersion: 2 }, { notepatReady: "secret" }])
    assert.equal(validateVisit({ ...body, ...bad }, "https://notepat.com"), null);
  assert.equal(validateVisit(body, "https://jas.life"), null);
  const update = visitUpdate(validateVisit({ ...body, notepatReady: true }, "https://notepat.com"));
  assert.equal(update.$max.notepatReady, true);
  assert.equal(update.$setOnInsert.notepatVersion, 1);
  assert.equal(update.$setOnInsert.campaign, "nopaint-music");
  assert.ok(Buffer.byteLength(JSON.stringify({ ...body, actions: VISIT_ACTIONS })) < 2048);
  assert.equal(visitLinkAction("nopaint.art", "https://notepat.com/?ac_campaign=nopaint-music"), "notepat_opened");
  for (const url of ["http://notepat.com", "https://notepat.com.evil.test", "https://secret@notepat.com", "https://notepat.com:1234"])
    assert.equal(visitLinkAction("nopaint.art", url), null);
});

test("report separates unmeasured arrivals and zero milestones, and filters automation", () => {
  const rows = [
    { _id: { source: "nopaint-music", measured: 1 }, visits: 10, ready: 8, firstNote: 5, eightNotes: 2, recordingStarted: 2, recordingCompleted: 1 },
    { _id: { source: "nopaint-referrer", measured: 0 }, visits: 30 },
  ];
  const report = notepatPathway(rows, { notepatAdShown: 40, notepatOpenedAfterShown: 4 });
  assert.equal(report.source.clickThroughPercent, 10);
  const [campaign, referrer, other] = report.cohorts;
  assert.equal(campaign.firstNotePercent, 50);
  assert.equal(campaign.eightNotesPerFirstNotePercent, 40);
  assert.equal(campaign.completedPerRecordingPercent, 50);
  assert.equal(campaign.playbackRequested, 0);
  assert.equal(referrer.unmeasuredVisits, 30);
  assert.equal(referrer.firstNote, null);
  assert.equal(other.firstNotePercent, null);
  assert.equal(notepatPipeline(new Date(0), new Date())[0].$match.automated, false);
});

test("empty recordings do not claim completion and phrase data never enters visit messages", async () => {
  const source = await readFile(new URL("../public/aesthetic.computer/disks/notepat.mjs", import.meta.url), "utf8");
  const fn = source.match(/function finishRecordingAndJump\(api\) \{[\s\S]*?\n\}/)[0];
  let result = null;
  const messages = [], jumps = [];
  const api = { send: message => messages.push(message), jump: value => jumps.push(value) };
  const context = vm.createContext({ serializeRecording: () => result, recording: true, recordingActive: {}, recordBuffer: [], api });
  vm.runInContext(fn + "; finishRecordingAndJump(api)", context);
  assert.equal(messages.length, 0);
  result = { melody: "4c4d", timing: 0.5 };
  vm.runInContext("finishRecordingAndJump(api)", context);
  assert.deepEqual(messages.map(m => m.content.action), ["notepat_recording_completed", "notepat_playback_requested"]);
  assert.deepEqual(jumps, ["clock 4c4d:0.5"]);
  assert.doesNotMatch(JSON.stringify(messages), /4c4d|0.5|melody/);
});


test("entry attribution survives runtime URL cleanup and tracker installs before boot", async () => {
  const { win, api, sent } = harness();
  win.acAutomation = true;
  win.location.search = "";
  api.action("notepat_ready");
  assert.equal(sent.at(-1).campaign, "nopaint-music");
  assert.equal(sent.at(-1).notepatReady, true);
  assert.equal(sent.at(-1).automated, true);
  const html = await readFile(new URL("../netlify/functions/index.mjs", import.meta.url), "utf8");
  const tracker = html.indexOf('src="https://aesthetic.computer/aesthetic.computer/lib/visit-tracker.mjs');
  const boot = html.indexOf('src="/aesthetic.computer/boot.mjs"');
  assert.ok(tracker > 0 && tracker < boot, "deferred tracker must execute before URL-rewriting boot");
});

test("play time follows manual notes, not idle tabs, chords, hidden time or suspension", () => {
  const h = harness();
  h.interact(); h.advance(60000);
  assert.equal(h.sent.at(-1).notepatPlaySeconds, 0);
  for (let n = 0; n < 8; n++) h.api.action("notepat_note_played");
  h.advance(4000);
  assert.equal(h.sent.at(-1).notepatPlaySeconds, 0, "round down to five seconds");
  h.advance(1000);
  assert.equal(h.sent.at(-1).notepatPlaySeconds, 5, "chord notes share one clock");
  h.advance(60000);
  assert.equal(h.sent.at(-1).notepatPlaySeconds, 5, "idle time stops after five seconds");
  h.api.action("notepat_note_played"); h.advance(3000);
  h.doc.visibilityState = "hidden"; h.doc.emit("visibilitychange"); h.advance(60000);
  h.doc.visibilityState = "visible"; h.doc.emit("visibilitychange"); h.advance(10000);
  assert.equal(h.sent.at(-1).notepatPlaySeconds, 5, "no hidden or stale resumed time");
  h.api.action("notepat_note_played"); h.advance(2000);
  assert.equal(h.sent.at(-1).notepatPlaySeconds, 10);
  h.api.action("notepat_note_played"); h.advance(60000, 60000);
  assert.equal(h.sent.at(-1).notepatPlaySeconds, 10, "sleep cannot fabricate play");
  h.win.acVisitTrackingDisabled = true;
  const count = h.sent.length; h.advance(10000);
  assert.equal(h.sent.length, count);
  assert.ok(h.sent.every(row => validateVisit(row, "https://notepat.com")));
});

test("continuous playing caps at one hour and private boundaries reset the clock", () => {
  const h = harness(); h.interact();
  for (let n = 0; n < 910; n++) { h.api.action("notepat_note_played"); h.advance(4000); }
  assert.equal(h.sent.at(-1).notepatPlaySeconds, 3600);
  h.win.location.pathname = "/mail"; h.advance(1000);
  h.win.location.pathname = "/"; h.advance(1000);
  assert.equal(h.sent.at(-1).notepatPlaySeconds, 0);
});

test("play duration is bounded, note-qualified and merged monotonically", () => {
  const h = harness(); h.interact(); h.api.action("notepat_note_played"); h.advance(10000);
  const body = h.sent.at(-1);
  for (const bad of [{ notepatPlaySeconds: -5 }, { notepatPlaySeconds: 3 },
    { notepatPlaySeconds: 3605 }, { notepatPlaySeconds: "5" }, { notepatPlayVersion: 2 },
    { notepatPlayVersion: undefined }, { notepatPlaySeconds: undefined }, { actions: {} }, { actions: [] }])
    assert.equal(validateVisit({ ...body, ...bad }, "https://notepat.com"), null);
  const update = visitUpdate(validateVisit(body, "https://notepat.com"));
  assert.equal(update.$max.notepatPlaySeconds, 5);
  assert.equal(update.$max.notepatPlayVersion, 1);
  const { notepatPlayVersion, notepatPlaySeconds, ...old } = body;
  assert.ok(validateVisit(old, "https://notepat.com"), "older clients remain valid");
  assert.equal(visitUpdate(validateVisit(old, "https://notepat.com")).$max.notepatPlaySeconds, undefined);
});

test("duration cohorts exclude missing coverage from means and distinguish zero players", () => {
  const report = notepatPathway([
    { _id: { source: "nopaint-music", measured: 1 }, visits: 8, firstNote: 4 },
    { _id: { source: "nopaint-music", measured: 1, playMeasured: 1 }, visits: 4,
      firstNote: 2, playSeconds: 80, maxPlaySeconds: 65, played10: 2, played30: 1, played60: 1 },
    { _id: { source: "other", measured: 1, playMeasured: 1 }, visits: 3, firstNote: 0 },
  ]);
  const [campaign, missing, idle] = report.cohorts.map(c => c.playTime);
  assert.equal(campaign.measuredVisits, 4);
  assert.equal(campaign.unmeasuredVisits, 8);
  assert.equal(campaign.averageSecondsLowerBoundPerPlayer, 40);
  assert.equal(campaign.maxSecondsLowerBound, 65);
  assert.deepEqual(campaign.playersAtLeast, { 10: 2, 30: 1, 60: 1, 180: 0, 600: 0 });
  assert.equal(missing.totalSecondsLowerBound, null);
  assert.equal(idle.totalSecondsLowerBound, 0);
  assert.equal(idle.averageSecondsLowerBoundPerPlayer, null);
});
