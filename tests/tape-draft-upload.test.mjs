import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import vm from "node:vm";

const video = readFileSync(new URL("../system/public/aesthetic.computer/disks/video.mjs", import.meta.url), "utf8");
const disk = readFileSync(new URL("../system/public/aesthetic.computer/lib/disk.mjs", import.meta.url), "utf8");
const routingStart = disk.indexOf('  if (type === "recorder:transcode-progress") {');
const routingEnd = disk.indexOf('  if (\n    type === "recorder:rolling:started"', routingStart);
assert.ok(routingStart >= 0 && routingEnd > routingStart);
const routeSource = disk.slice(routingStart, routingEnd);
const postStart = video.indexOf("    postBtn?.act(e, {");
const postEnd = video.indexOf("    // Hidden export options", postStart);
assert.ok(postStart >= 0 && postEnd > postStart);

// Execute the real piece and worker forwarding branch without GPU/audio boot.
function review(mode = "receive") {
  const sent = [];
  const ctx = vm.createContext({
    console: { log() {}, warn() {}, error() {} }, performance,
    send: message => sent.push(message),
    rec: { recorded: true, requestFrames: cb => cb({ frames: [[0, { width: 1, height: 1, data: new Uint8ClampedArray(4) }]] }) },
    triggerRender() {}, e: {},
  });
  vm.runInContext(video.replace(/import\s*\{[\s\S]*?\}\s*from\s*"[^"]+";/, "")
    .replace(/export \{[^}]+\};/, ""), ctx);
  vm.runInContext(`
    apiSend = send;
    postBtn = { disabled: false, act(e, handlers) { this.push = handlers.push; } };
    ${video.slice(postStart, postEnd)}
    beginTapeDraft(rec, send);
  `, ctx);
  const worker = vm.createContext({
    debug: false, getPackMode: () => false,
    console: { log() {}, warn() {} }, booted: true,
    defaults: { receive() {} }, pendingExportEvents: [],
    $commonApi: { rec: {} }, send() {},
    receive: event => { ctx.event = event; return vm.runInContext("receive(event)", ctx); },
    actEvents: mode === "act" ? [] : undefined,
  });
  const dispatch = (type, content) => {
    worker.type = type;
    worker.content = content;
    vm.runInContext(`(function() { ${routeSource} })()`, worker);
    for (const event of worker.actEvents || []) {
      ctx.event = event;
      vm.runInContext("act({ event, rec, sound: {} })", ctx);
    }
    if (worker.actEvents) worker.actEvents.length = 0;
  };
  return {
    sent, worker, dispatch,
    click: () => vm.runInContext("postBtn.push()", ctx),
    state: () => vm.runInContext("({ tapeDraftState, finalizeWhenReady, isPostingTape, isPrinting, currentExportType, printProgress, postedTapeCode, disabled: postBtn.disabled })", ctx),
  };
}

const draft = { id: "test-draft", token: "test-token", metadata: { duration: 8000 } };

for (const mode of ["receive", "act"]) {
  test(`${mode}: Done waits for the draft, finalizes once, and completes`, async () => {
    const r = review(mode);
    assert.equal(r.sent[0].type, "create-and-post-tape");
    assert.equal(r.sent[0].content.draftOnly, true);
    await r.click();
    assert.equal(r.state().finalizeWhenReady, true);
    r.dispatch("tape:draft-progress", 0.5);
    assert.equal(r.state().printProgress, 0.925);
    r.dispatch("tape:draft-ready", draft);
    assert.equal(r.sent.at(-1).type, "tape:draft-finalize");
    assert.equal(r.sent.at(-1).content.id, draft.id);
    assert.equal(r.state().finalizeWhenReady, false);
    await r.click();
    assert.equal(r.sent.filter(m => m.type === "tape:draft-finalize").length, 1);
    r.dispatch("tape:posted", { code: "testcode" });
    assert.equal(r.state().postedTapeCode, "testcode");
    assert.equal(r.state().tapeDraftState, "idle");
    assert.equal(r.state().isPostingTape, false);
  });

  test(`${mode}: a ready draft stays private until Done`, async () => {
    const r = review(mode);
    r.dispatch("tape:draft-ready", draft);
    assert.equal(r.sent.length, 1);
    assert.equal(r.state().tapeDraftState, "ready");
    await r.click();
    assert.equal(r.sent.at(-1).type, "tape:draft-finalize");
  });

  test(`${mode}: a failed pre-upload unlocks Done and retries the normal upload`, async () => {
    const r = review(mode);
    await r.click();
    r.dispatch("tape:draft-error", { error: "Upload failed" });
    assert.equal(r.state().isPostingTape, false);
    assert.equal(r.state().disabled, false);
    assert.equal(r.state().currentExportType, "");
    await r.click();
    assert.equal(r.sent.at(-1).type, "create-and-post-tape");
    assert.equal(r.sent.at(-1).content.draftOnly, undefined);
  });

  test(`${mode}: failed finalization retries the existing uploaded draft`, async () => {
    const r = review(mode);
    r.dispatch("tape:draft-ready", draft);
    await r.click();
    r.dispatch("tape:post-error", { error: "HTTP 503" });
    assert.equal(r.state().tapeDraftState, "ready");
    assert.equal(r.state().isPostingTape, false);
    assert.equal(r.state().disabled, false);
    await r.click();
    assert.equal(r.sent.filter(m => m.type === "create-and-post-tape").length, 1);
    assert.equal(r.sent.filter(m => m.type === "tape:draft-finalize").length, 2);
  });
}

test("draft events queue until the piece has booted", () => {
  const r = review();
  r.worker.booted = false;
  r.dispatch("tape:draft-ready", draft);
  assert.equal(r.worker.pendingExportEvents.length, 1);
  assert.equal(r.state().tapeDraftState, "uploading");
  r.worker.booted = true;
  r.worker.receive(r.worker.pendingExportEvents.shift());
  assert.equal(r.state().tapeDraftState, "ready");
});
