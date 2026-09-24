import assert from "node:assert/strict";
import test from "node:test";
import { mouseEvent } from "../src/mouse.mjs";
import { normalize, rowSpan, selectedText } from "../src/selection.mjs";
import { renderFrame } from "../src/render.mjs";

test("the mouse reports the three moments of a drag", () => {
  assert.equal(mouseEvent("\x1b[<0;5;3M").press, true, "left button down");
  assert.equal(mouseEvent("\x1b[<32;9;3M").drag, true, "moving with it held");
  assert.equal(mouseEvent("\x1b[<35;9;3M").drag, false, "moving with nothing held is a hover");
  assert.equal(mouseEvent("\x1b[<0;9;4m").release, true, "up");
  assert.equal(mouseEvent("\x1b[<64;9;4M").drag, false, "a wheel is not a drag");
  assert.notEqual(mouseEvent("\x1b[<64;9;4M").wheel, 0);
});

test("a selection reads top-down whichever way it was dragged, and copies the words under it", () => {
  const rows = [" first line here", " second line", " third"];
  const forward = { anchor: [3, 1], head: [8, 2] };
  const backward = { anchor: [8, 2], head: [3, 1] };
  assert.deepEqual(normalize(backward), normalize(forward));
  assert.deepEqual(rowSpan(1, forward, 40), [3, 40]);
  assert.deepEqual(rowSpan(2, forward, 40), [1, 8]);
  assert.equal(rowSpan(3, forward, 40), null);
  assert.equal(selectedText(rows, forward, 40), "irst line here\nsecond", "each row right-trimmed");
  assert.equal(selectedText(rows, { anchor: [2, 3], head: [40, 3] }, 40), "third", "a whole row, right-trimmed, without the margin");
  assert.equal(selectedText(["", "", " words", ""], { anchor: [1, 1], head: [40, 4] }, 40), "words", "blank padding rows at either end are not words");
});

test("the frame paints the selected cells in reverse video and leaves the plain rows for the copy", () => {
  const state = {
    workspace: "/c", mode: "remote", status: "ready", busy: false, input: "", account: "@t",
    profile: { name: "pro" }, entries: [{ id: "a", kind: "assistant", text: "alpha beta gamma" }],
    selection: { anchor: [1, 12], head: [40, 12], active: false },
  };
  const frame = renderFrame(state, 60, 16, true);
  assert.ok(state.pageRows.some((row) => row.includes("alpha beta gamma")), "plain rows carry the words");
  const rows = frame.split("\n");
  assert.match(rows[11], /\x1b\[7m/, "the chosen row wears reverse video");
  assert.doesNotMatch(rows[10], /\x1b\[7m/, "and its neighbour does not");
});
