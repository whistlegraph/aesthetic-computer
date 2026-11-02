import "./own-ink.mjs";
import test from "node:test";
import assert from "node:assert/strict";
import { cleanText, headerAction, renderFrame, textWidth } from "../src/render.mjs";

// The letterhead's parts are places to click, like the handle on the status
// line: the donkey and "Aesel" open Aesel's page, the model its settings,
// the folder the file manager.
const base = { entries: [{ kind: "user", text: "hi" }], input: "", aeselVersion: "0.8.39", modelLabel: "Claude Fable 5.1", workspace: `${process.env.HOME}/aesthetic-computer`, account: "@jeffrey", status: "ready" };

function find(frame, needle) {
  const rows = cleanText(frame).split("\n");
  const y = rows.findIndex(row => row.includes(needle));
  assert.ok(y >= 0, `${needle} is on screen`);
  return { x: textWidth(rows[y].slice(0, rows[y].indexOf(needle))) + 1, y: y + 1, row: rows[y] };
}

for (const pro of [false, true]) test(`letterhead clicks reach the name, model and folder (${pro ? "pro" : "classic"})`, () => {
  const state = { ...base, ...(pro ? { profile: { name: "pro" } } : {}) };
  const frame = renderFrame(state, 80, 24, false);
  const name = find(frame, "Aesel v0.8.39");
  const model = find(frame, "Claude Fable 5.1");
  const folder = find(frame, "~/aesthetic-computer");
  assert.equal(headerAction(state, 80, 24, name.x, name.y), "aesel");
  assert.equal(headerAction(state, 80, 24, name.x + textWidth("Aesel v0.8.39") - 1, name.y), "aesel", "the version is part of the name");
  assert.equal(headerAction(state, 80, 24, name.x + textWidth("Aesel v0.8.39"), name.y), "", "the air after it is nothing");
  assert.equal(headerAction(state, 80, 24, model.x + 3, model.y), "model");
  assert.equal(headerAction(state, 80, 24, folder.x + 3, folder.y), "workspace");
  assert.equal(headerAction(state, 80, 24, 3, name.y), "aesel", "the donkey himself opens the page");
  assert.equal(headerAction(state, 80, 24, 3, name.y - 1), "aesel", "every row of him");
  assert.equal(headerAction(state, 80, 24, 2, model.y + 3), "", "the blank line under him is nothing");
  assert.equal(headerAction(state, 80, 24, folder.x + textWidth("~/aesthetic-computer") + 2, folder.y), "", "the air to the right of the folder is nothing");
});

test("a letterhead that has scrolled away takes no clicks", () => {
  const entries = Array.from({ length: 40 }, (_, i) => ({ kind: i % 2 ? "assistant" : "user", text: `line ${i}` }));
  const state = { ...base, entries };
  const frame = renderFrame(state, 80, 24, false);
  assert.doesNotMatch(cleanText(frame), /Aesel v0\.8\.39/);
  for (let y = 1; y <= 24; y++) for (const x of [3, 21, 30]) assert.notEqual(headerAction(state, 80, 24, x, y), "aesel");
  assert.deepEqual(state.letterhead, []);
});

test("the desktop app and a narrow window keep their own rules", () => {
  const desktop = { ...base, desktop: true };
  renderFrame(desktop, 80, 24, false);
  assert.deepEqual(desktop.letterhead, [], "the desktop draws no letterhead");
  const narrow = { ...base, profile: { name: "pro" } };
  const frame = renderFrame(narrow, 32, 24, false);
  // The facts are clipped to the window, and what shows of them still clicks.
  const name = find(frame, "Aesel v0.8.");
  const donkey = find(frame, "\u2588");
  assert.equal(headerAction(narrow, 32, 24, 3, donkey.y), "aesel", "the donkey still clicks");
  assert.equal(headerAction(narrow, 32, 24, name.x + 2, name.y), "aesel");
  assert.equal(headerAction(narrow, 32, 24, 19, name.y), "", "the gap between him and the name is nothing");
});
