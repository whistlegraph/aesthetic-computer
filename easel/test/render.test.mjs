import assert from "node:assert/strict";
import test from "node:test";
import { cleanText, renderFrame, textWidth, wrapText } from "../src/render.mjs";

test("cleans terminal control sequences", () => {
  assert.equal(cleanText("safe\x1b[2J\x00 text"), "safe text");
});

test("wraps content to the available width", () => {
  const lines = wrapText("aesthetic code owns the terminal interface", 12);
  assert.ok(lines.length > 1);
  assert.ok(lines.every((line) => Array.from(line).length <= 12));
});

test("renders one branded interface with privacy state and prompt", () => {
  const frame = renderFrame(
    {
      workspace: "/Users/jas/project",
      mode: "remote",
      status: "ready",
      busy: false,
      input: "make the tests pass",
      entries: [
        { kind: "notice", text: "REMOTE INFERENCE · prompt content may leave this machine" },
        { kind: "user", text: "inspect this repository" },
        { kind: "assistant", text: "I found the failing test." },
      ],
    },
    80,
    20,
    false,
  );
  assert.match(frame, /AESTHETIC CODE/);
  assert.match(frame, /REMOTE · READY/);
  assert.match(frame, /YOU  inspect this repository/);
  assert.match(frame, /AC   I found the failing test/);
  assert.match(frame, /› make the tests pass/);
  assert.equal(frame.split("\n").length, 20);
});

test("shows the signed-in handle and the current piece in the header", () => {
  const frame = renderFrame(
    {
      workspace: "/project",
      mode: "remote",
      status: "ready",
      busy: false,
      input: "",
      entries: [{ id: "p", kind: "publish", text: "https://aesthetic.computer/@tester/smiley" }],
      account: "@tester",
      piece: "smiley",
    },
    70,
    12,
    false,
  );
  assert.match(frame, /AESTHETIC CODE  @tester  smiley/);
  assert.match(frame, /REMOTE · READY/);
  assert.match(frame, /PUB  https:\/\/aesthetic\.computer\/@tester\/smiley/);
  assert.match(renderFrame({ workspace: "/p", mode: "remote", status: "ready", entries: [], input: "" }, 60, 12, false), /not signed in/);
});

test("keeps a QR column on the right and narrows the transcript around it", async () => {
  const { qrBlock } = await import("../src/qr.mjs");
  const qr = qrBlock("aesthetic.computer/prompt~channel%20Ab0-_9Zz~!autorun");
  const state = {
    workspace: "/project",
    mode: "remote",
    status: "ready",
    account: "@tester",
    piece: "movika.mjs",
    input: "",
    entries: [{ id: "a", kind: "user", text: "hello" }],
    qr,
  };
  const wide = renderFrame(state, 100, 26, true);
  const rows = wide.split("\n");
  assert.equal(rows.length, 26);
  for (const row of rows) assert.equal(textWidth(row), 100, "every row fills the frame exactly");
  assert.equal(rows.at(-6).includes("\u2580"), true, "the code sits on the bottom transcript rows");
  assert.equal(rows[2].includes("\u2580"), false, "and not at the top");

  // Too small a window, or no colour, drops the code rather than corrupting the frame.
  const narrow = renderFrame(state, 50, 26, true);
  assert.equal(narrow.includes("\u2580"), false);
  for (const row of narrow.split("\n")) assert.equal(textWidth(row), 50);
  assert.equal(renderFrame(state, 100, 26, false).includes("\u2580"), false, "NO_COLOR keeps the frame plain");

  // No window is too small to draw a whole frame in.
  for (const columns of [32, 40, 50, 64, 80, 140]) {
    for (const row of renderFrame(state, columns, 12, true).split("\n")) {
      assert.equal(textWidth(row), columns, `row overflows at ${columns} columns`);
    }
  }
});

test("no state can make a row wider than the window", async () => {
  const { qrBlock } = await import("../src/qr.mjs");
  const qr = qrBlock("aesthetic.computer/prompt~channel%20Ab0-_9Zz~!autorun");
  const path = "/private/tmp/claude-501/-Users-jas/8e2ce643-970e-478d-bf06-4e3e19671f15/scratchpad/puka.mjs";
  const base = { workspace: path, mode: "remote", status: "ready", account: "@tester", piece: "puka.mjs", input: "", qr, entries: [] };
  // One wrapped row scrolls the whole frame, and the QR code sits on the rows
  // that go first — so every one of these has to come back exactly as wide as
  // the window, escape codes and double-width glyphs and all.
  const states = [
    base,
    { ...base, status: "approval", approval: { subject: path } },
    { ...base, status: "approval", approval: { subject: "x".repeat(400) } },
    { ...base, entries: [{ id: "e", kind: "assistant", text: "Done ✅ it runs 🚀 now — check it ✨" }] },
    { ...base, entries: [{ id: "e", kind: "assistant", text: "这是一个测试 これはテストです 이것은 테스트입니다" }] },
    { ...base, entries: [{ id: "e", kind: "command", text: `${"█".repeat(120)}\n${"—".repeat(200)}` }] },
    { ...base, input: `${"🌈".repeat(60)}?`, cursor: 61 },
  ];
  for (const state of states) {
    for (const [columns, rows] of [[75, 42], [100, 40], [80, 24], [57, 24], [40, 20], [32, 10]]) {
      const frame = renderFrame(state, columns, rows, true);
      assert.equal(frame.split("\n").length, rows);
      for (const row of frame.split("\n")) assert.equal(textWidth(row), columns);
    }
  }
});

test("measures a row in terminal cells, not characters", () => {
  assert.equal(textWidth("ok"), 2);
  assert.equal(textWidth("🌈"), 2, "an emoji takes two columns");
  assert.equal(textWidth("测"), 2, "so does a CJK glyph");
  assert.equal(textWidth("é"), 1, "a combining mark takes none");
  assert.equal(textWidth("▀─·—…"), 5, "the interface's own glyphs stay narrow");
  assert.deepEqual(wrapText("🌈🌈🌈", 4), ["🌈🌈", "🌈"]);
  assert.equal(wrapText("🌈🌈", 1).filter(Boolean).length, 2, "a glyph wider than the column still advances");
});

test("renders approvals inside the interface", () => {
  const frame = renderFrame(
    {
      workspace: "/project",
      mode: "remote",
      status: "approval",
      busy: true,
      input: "",
      entries: [],
      approval: { subject: "npm test" },
    },
    60,
    14,
    false,
  );
  assert.match(frame, /ALLOW npm test/);
  assert.match(frame, /y once/);
  assert.match(frame, /a session/);
  assert.match(frame, /n deny/);
});
