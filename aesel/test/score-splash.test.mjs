import test from "node:test";
import assert from "node:assert/strict";
import { renderFrame } from "../src/render.mjs";

const base = { score: "# Piece score\n\nBuild a piece.\nIt wraps here.\n\n- one\n- two", workspace: "/x", model: "m", status: "ready", input: "", cursor: 0, queued: [] };
const text = (state, rows) => String(renderFrame(state, 80, rows, false));

test("a fresh thread shows the score in the room the letterhead leaves", () => {
  assert.match(text({ ...base, entries: [] }, 30), /PIECE SCORE/);
});

test("the score gives way once anyone speaks", () => {
  assert.doesNotMatch(text({ ...base, entries: [{ kind: "user", text: "hi" }] }, 30), /PIECE SCORE/);
});

test("a short window drops the score rather than crowd the thread", () => {
  assert.doesNotMatch(text({ ...base, entries: [] }, 14), /PIECE SCORE/);
});

test("the masthead is set in capitals over a rule, and hard-wrapped lines are rejoined", () => {
  const page = text({ ...base, entries: [] }, 30);
  assert.match(page, /PIECE SCORE *\n ═+/);
  assert.match(page, /Build a piece\. It wraps here\./);
});

test("only the first paragraph is shown, whole, never cut short", () => {
  const long = { ...base, score: "# Score\n\nThe summary.\n\n## Later\n\nDetail that stays in the file.", entries: [] };
  const page = String(renderFrame(long, 160, 40, false));
  assert.match(page, /The summary\./);
  assert.doesNotMatch(page, /Detail|Later|…/);
});

test("a summary that does not fit is left out, not truncated", () => {
  const long = { ...base, score: `# Score\n\n${"word ".repeat(200)}`, entries: [] };
  assert.doesNotMatch(String(renderFrame(long, 60, 24, false)), /SCORE/);
});

import { setDetail } from "../src/render.mjs";

const shaped = "Board matched in ClickUp; pilot still to do.\nask: reuse the September board?\noptions: ♻️ Reuse = keep it | 🆕 New = build one\n- Company 86bc16y4c\n- Pilot 86bc6kx9y\n<explain>Searched by domain.</explain>";
const reply = { ...base, score: "", entries: [{ kind: "user", text: "go" }, { kind: "assistant", text: shaped }] };

test("a shaped reply is set as signal, ask, options and points with the explain folded", () => {
  setDetail("brief");
  const page = text(reply, 30);
  assert.match(page, /Board matched in ClickUp; pilot still to do\./);
  assert.match(page, /ask reuse the September board\?/);
  assert.match(page, /• .*Reuse — keep it/);
  assert.match(page, /• Pilot 86bc6kx9y/);
  assert.match(page, /▸ explain/);
  assert.doesNotMatch(page, /Searched by domain/);
});

test("full unfolds the explain and signal folds the points", () => {
  setDetail("full");
  assert.match(text(reply, 30), /Searched by domain/);
  setDetail("signal");
  const page = text(reply, 30);
  assert.doesNotMatch(page, /Pilot 86bc6kx9y/);
  assert.match(page, /▸ explain/);
  setDetail("brief");
});

test("a plain reply is left as written", () => {
  const plain = { ...base, score: "", entries: [{ kind: "user", text: "go" }, { kind: "assistant", text: "Done. It works." }] };
  assert.match(text(plain, 30), /Done\. It works\./);
  assert.doesNotMatch(text(plain, 30), /explain/);
});
