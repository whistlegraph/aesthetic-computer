import test from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { readFile } from "node:fs/promises";
import vm from "node:vm";
import { startVisitTracker } from "../public/aesthetic.computer/lib/visit-tracker.mjs";
import { notepatVisitContext, validateVisit, visitUpdate, visitLinkAction, VISIT_ACTIONS } from "../public/aesthetic.computer/lib/visit-model.mjs";
import { notepatPathway, notepatPipeline } from "../../toolchain/analytics/notepat.mjs";

function harness(hostname = "notepat.com") {
  const target = () => {
    const listeners = new Map();
    return { addEventListener(type, fn) { listeners.set(type, [...(listeners.get(type) || []), fn]); },
      removeEventListener(type, fn) { listeners.set(type, (listeners.get(type) || []).filter(f => f !== fn)); },
      emit(type, event) { for (const fn of listeners.get(type) || []) fn(event); } };
  };
  const sent = [], doc = { ...target(), visibilityState: "visible", querySelectorAll: () => [], querySelector: () => null,
    referrer: "https://nopaint.art/?private=secret", hasFocus: () => true };
  const win = { ...target(), navigator: {}, crypto: { randomUUID }, performance: { now: () => 0 },
    location: { hostname, pathname: "/", search: "?ac_campaign=nopaint-music&private=secret" },
    fetch: async (_url, options) => { sent.push(JSON.parse(options.body)); return { ok: true }; },
    setInterval: () => 1, clearInterval() {} };
  win.top = win;
  const api = startVisitTracker(win, doc);
  const interact = (isTrusted = true) => win.emit("keydown", { isTrusted, target: {} });
  return { win, doc, api, sent, interact };
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
  assert.deepEqual(notepatVisitContext("notepat.com", "?ac_campaign=secret"), { notepatVersion: 1, notepatReady: false, campaign: null });
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
