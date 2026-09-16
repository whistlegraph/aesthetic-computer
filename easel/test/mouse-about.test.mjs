import test from "node:test";
import assert from "node:assert/strict";
import { InputDecoder, mouseEvent } from "../src/mouse.mjs";
import { headerAction, renderFrame, textWidth, cleanText } from "../src/render.mjs";

test("mouse reports split across reads never become prompt characters", () => {
  const decoder = new InputDecoder();
  assert.deepEqual(decoder.push("\x1b[<35;4"), []);
  assert.deepEqual(decoder.push(";21Mhi"), ["\x1b[<35;4;21M", "h", "i"]);
  assert.deepEqual(mouseEvent("\x1b[<35;4;21M"), { x: 4, y: 21, motion: true, wheel: 0, click: false });
  assert.equal(mouseEvent("\x1b[<0;4;21M").click, true);
  assert.equal(mouseEvent("\x1b[<0;4;21m").click, false);
  assert.equal(mouseEvent("\x1b[<65;4;21M").wheel, 1);
});

test("standalone escape and split arrow/paste sequences are distinct", () => {
  const decoder = new InputDecoder();
  assert.deepEqual(decoder.push("\x1b"), []);
  assert.deepEqual(decoder.escape(), ["\x1b"]);
  assert.deepEqual(decoder.push("\x1b["), []);
  assert.deepEqual(decoder.escape(), []);
  assert.deepEqual(decoder.push("A\x1b[200~hello\x1b[201~"), ["\x1b[A", "\x1b[200~", "h", "e", "l", "l", "o", "\x1b[201~"]);
});

test("only visible header labels are clickable across terminal sizes", () => {
  const state = { account: "@jeffrey", status: "ready", entries: [] };
  for (const width of [32, 40, 80, 120]) {
    const header = cleanText(renderFrame(state, width, 24, false)).split("\n")[20];
    assert.equal(headerAction(state, width, 24, 3, 21), "about");
    assert.equal(headerAction(state, width, 24, 10, 21), header.includes("@jeffrey") ? "profile" : "");
    assert.equal(headerAction(state, width, 24, 3, 20), "");
    assert.equal(headerAction(state, width, 24, 8, 21), "");
  }
});

test("about is a scrollable map that preserves transcript and fits small windows", () => {
  const state = { account: "@jeffrey", entries: [{ kind: "user", text: "keep my drawing" }], about: true };
  const top = renderFrame(state, 80, 24, false);
  assert.match(top, /You → model → artifact/);
  assert.doesNotMatch(top, /keep my drawing/);
  assert.equal(state.entries[0].text, "keep my drawing");
  const bottom = renderFrame({ ...state, aboutScroll: 1000 }, 40, 12, false);
  assert.match(bottom, /Esc returns/);
  for (const width of [32, 40, 80]) {
    for (const row of renderFrame(state, width, 24, false).split("\n")) assert.ok(textWidth(row) <= width);
  }
});
