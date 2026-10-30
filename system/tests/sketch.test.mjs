import test from "node:test";
import assert from "node:assert/strict";
import { safe, musical, tuned, wordTune } from "../netlify/functions/sketch-lib.mjs";

test("a drawing without a tune gets the word's own, quoted and quiet", () => {
  const out = tuned("snake", "(wipe blue)\n(circle w/2 h/2 10)");
  assert.equal(out, '(wipe blue)\n(circle w/2 h/2 10)\n(melody "{triangle:0.35} gbafe")');
  assert.ok(musical(out));
  assert.ok(safe(out));
  assert.equal(wordTune("hot air balloon"), "{triangle:0.35} .cca_adf_baggccb");
});

test("the model's tune is kept, quoted, and brought under conversation volume", () => {
  assert.equal(tuned("x", "(melody cdefg)"), '(melody "{0.4} cdefg")');
  assert.equal(tuned("x", '(melody "{sine:0.9} 5c e g")'), '(melody "{sine:0.5} 5c e g")');
  assert.equal(tuned("x", '(clock "ceg dfa")'), '(clock "{0.4} ceg dfa")');
  assert.equal(tuned("x", '(melody "{harp:0.3} .cdef > gab")'), '(melody "{harp:0.3} .cdef > gab")');
});

test("tunes that reach outside the drawing are unsafe", () => {
  assert.equal(safe('(melody "{say} cde")'), false);
  assert.equal(safe('(melody "{sample} cde")'), false);
  assert.equal(safe('(melody "{#abc123} cde")'), false);
  assert.equal(safe('(melody "cd\\"hello\\"e")'), false);
  assert.equal(safe('(melody "{square:0.3} 5cdeg dfa")'), true);
  assert.equal(safe("(0.5s (pluck c4))\n(melody \"ceg\")"), true);
  assert.equal(safe("(mic)\n(melody \"ceg\")"), false);
  assert.equal(musical("(clock)"), false);
});

const drain = async (stream) => {
  const reader = stream.getReader(), decoder = new TextDecoder();
  let out = "";
  for (;;) { const { done, value } = await reader.read(); if (done) return out; out += decoder.decode(value); }
};

test("a drawing outlives the tab that asked and is shared with the next one", async () => {
  const { share, streamFrom } = await import("../netlify/functions/sketch-lib.mjs");
  let emit, finish;
  const job = share("otter", (onText) => new Promise((resolve) => { emit = onText; finish = resolve; }));
  assert.equal(share("otter", null), job, "a second asker joins the same job");
  assert.equal(share("heron", null), null, "no job is started by a look");

  const first = streamFrom(job);
  const reader = first.getReader();
  emit("(wipe ");
  await reader.cancel("tab closed"); // the first visitor leaves mid-stream
  emit("blue)");
  assert.equal(job.text, "(wipe blue)", "the model kept writing");

  const second = drain(streamFrom(job)); // a late joiner gets the backlog first
  emit("\n(melody \"ceg\")");
  finish({ code: "abc" });
  assert.equal(await second, '(wipe blue)\n(melody "ceg")\n\u0000{"code":"abc"}');
  await job.promise;
  assert.equal(share("otter", null), null, "finished jobs are forgotten");
});
