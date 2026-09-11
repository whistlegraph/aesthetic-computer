import assert from "node:assert/strict";
import test from "node:test";
import { EASEL_HEIGHT, easelDuration, easelFrame, easelNextFrame } from "../src/easel.mjs";

const canvas = { piece: "balozo", address: "prompt.ac/@jeffrey/balozo" };

test("the easel is always the same size, whatever is on it", () => {
  const widths = new Set();
  for (const ms of [0, 100, 500, 1200, 5000]) {
    const lines = easelFrame(ms, canvas);
    assert.equal(lines.length, EASEL_HEIGHT, "a splash that changes height jumps");
    widths.add(lines[0].length);
  }
  assert.equal(widths.size, 1, "the frame must not resize as the name is written");
});

test("the name is written on, then the address follows", () => {
  const early = easelFrame(60, canvas).join("\n");
  assert.match(early, /b▌/, "mid-write shows a cursor");
  assert.ok(!early.includes("prompt.ac"), "the address waits its turn");

  const done = easelFrame(easelDuration(canvas.piece, canvas.address), canvas).join("\n");
  assert.ok(done.includes("balozo"), "the piece name lands");
  assert.ok(done.includes(canvas.address), "and so does the address");
  assert.ok(!done.includes("▌"), "the cursor goes away when writing stops");
});

test("it eventually stops asking to be repainted", () => {
  assert.equal(easelNextFrame(easelDuration(canvas.piece, canvas.address), canvas), null);
  assert.ok(easelNextFrame(0, canvas) > 0);
});

// A session that is not signed in has no address yet, and that is ordinary
// rather than an error state — the canvas just stays empty there.
test("an empty address leaves the canvas blank rather than breaking", () => {
  const lines = easelFrame(5000, { piece: "balozo", address: "" });
  assert.equal(lines.length, EASEL_HEIGHT);
  assert.ok(lines.join("\n").includes("balozo"));
  const lines2 = easelFrame(0, {});
  assert.equal(lines2.length, EASEL_HEIGHT, "and neither does having nothing at all");
});
