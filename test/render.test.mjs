import assert from "node:assert/strict";
import test from "node:test";
import { cleanText, renderFrame, wrapText } from "../src/render.mjs";

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
