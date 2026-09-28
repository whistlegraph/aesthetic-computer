import assert from "node:assert/strict";
import test from "node:test";
import { Diagnostics } from "../src/diagnostics.mjs";

function listener(options = {}) {
  const sent = [];
  const eyes = new Diagnostics({ channel: "jeffrey/balozo", ...options });
  return { eyes, sent, attach: () => eyes.attach((type, content) => sent.push({ type, content })) };
}

const report = (body) => ({
  type: "diagnostics:report",
  content: { channel: "jeffrey/balozo", ...body },
});

test("it asks to hear the channel, carrying the session's token", async () => {
  const { eyes, sent, attach } = listener({ token: async () => "tok-123" });
  await attach();
  assert.deepEqual(sent[0], {
    type: "diagnostics:listen",
    content: { channel: "jeffrey/balozo", token: "tok-123" },
  });
});

// A session that cannot produce a token still asks; the relay is what decides,
// and its reason is worth printing rather than guessing at.
test("a refusal is kept and explained rather than looking like silence", async () => {
  const { eyes, attach } = listener({ token: async () => { throw new Error("no token"); } });
  await attach();
  eyes.receive({
    type: "diagnostics:listening",
    content: { channel: "jeffrey/balozo", ok: false, reason: "a handled channel needs a signed-in listener" },
  });
  assert.equal(eyes.listening, false);
  assert.match(eyes.refusal, /signed-in listener/);
});

test("a blank frame is reported as blank, with the colour it is stuck on", () => {
  const { eyes } = listener();
  const changes = [];
  eyes.on("change", (r) => changes.push(r));
  eyes.receive(report({ kind: "frame", colors: 1, blank: true, color: [12, 10, 24], width: 396, height: 396 }));
  assert.equal(eyes.frame.blank, true);
  assert.deepEqual(eyes.frame.color, [12, 10, 24]);
  assert.equal(changes.length, 1);

  // The same blank frame again is not news.
  eyes.receive(report({ kind: "frame", colors: 1, blank: true, color: [12, 10, 24] }));
  assert.equal(changes.length, 1, "an unchanged frame does not churn the interface");

  // Coming back to life is.
  eyes.receive(report({ kind: "frame", colors: 40, blank: false, color: null }));
  assert.equal(eyes.frame.blank, false);
  assert.equal(changes.length, 2);
});

test("it keeps the last lines and counts the errors", () => {
  const { eyes } = listener({ kept: 3 });
  eyes.receive(report({ kind: "log", level: "log", text: "one" }));
  eyes.receive(report({ kind: "log", level: "error", text: "boom" }));
  eyes.receive(report({ kind: "log", level: "log", text: "two" }));
  eyes.receive(report({ kind: "log", level: "log", text: "three" }));
  assert.equal(eyes.logs.length, 3, "the buffer is bounded");
  assert.deepEqual(eyes.logs.map((l) => l.text), ["boom", "two", "three"]);
  assert.equal(eyes.errors.length, 1);
});

test("a dropped burst is shown as a gap, not hidden", () => {
  const { eyes } = listener();
  eyes.receive(report({ kind: "dropped", count: 118 }));
  assert.match(eyes.logs.at(-1).text, /118 more lines dropped/);
});

// The reports are somebody else's browser talking. They are text, and long
// text is clipped rather than allowed to run the readout off the screen.
test("a report is treated as untrusted text", () => {
  const { eyes } = listener();
  eyes.receive(report({ kind: "log", level: "error", text: "x".repeat(5000) }));
  assert.ok(eyes.logs.at(-1).text.length <= 400);
  // A report for a channel we are not watching is not ours to show.
  eyes.receive({ type: "diagnostics:report", content: { channel: "someone/else", kind: "log", level: "error", text: "leak" } });
  assert.equal(eyes.logs.length, 1);
});

test("moving to another piece drops what belonged to the last one", async () => {
  const { eyes, sent, attach } = listener({ token: async () => "tok" });
  await attach();
  eyes.receive(report({ kind: "frame", colors: 1, blank: true, color: [0, 0, 0] }));
  eyes.receive(report({ kind: "log", level: "error", text: "boom" }));
  await eyes.watch("jeffrey/murafi");
  assert.equal(eyes.frame, null);
  assert.equal(eyes.logs.length, 0);
  assert.equal(eyes.listening, false);
  assert.equal(sent.at(-1).content.channel, "jeffrey/murafi", "and it asks about the new one");
});

test("messages that are not ours are left alone", () => {
  const { eyes } = listener();
  assert.equal(eyes.receive({ type: "code-channel:info", content: { viewers: 2 } }), false);
  assert.equal(eyes.receive({ type: "connected", content: "{}" }), false);
});
