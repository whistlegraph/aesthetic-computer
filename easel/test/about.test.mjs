import assert from "node:assert/strict";
import test from "node:test";
import { conversationHandoff } from "../src/about.mjs";

test("engine handoff carries recent user/assistant context, excluding tools and UI", () => {
  const handoff = conversationHandoff([
    { kind: "user", text: "keep the dots purple" },
    { kind: "command", text: "PRIVATE TOOL OUTPUT" },
    { kind: "assistant", text: "the dots are purple" },
    { kind: "change", text: "TOOL PATH" },
    { kind: "error", text: "INTERNAL ERROR" },
    { kind: "notice", text: "SIGN IN URL" },
  ]);
  assert.match(handoff, /keep the dots purple/);
  assert.match(handoff, /the dots are purple/);
  assert.doesNotMatch(handoff, /PRIVATE|TOOL|INTERNAL|SIGN IN/);
  assert.ok(handoff.indexOf('"role":"user"') < handoff.indexOf('"role":"assistant"'));
});

test("engine handoff bounds context and favors the latest turns", () => {
  const entries = Array.from({ length: 30 }, (_, i) => ({ kind: i % 2 ? "assistant" : "user", text: `turn ${i} ${"x".repeat(50)}` }));
  const handoff = conversationHandoff(entries, 300);
  assert.ok(handoff.length < 400, "context budget plus heading and separators");
  assert.match(handoff, /turn 29/);
  assert.match(handoff, /turn 28/);
  assert.doesNotMatch(handoff, /turn 0 /);
  assert.equal(conversationHandoff([{ kind: "command", text: "tool" }]), "");
});
