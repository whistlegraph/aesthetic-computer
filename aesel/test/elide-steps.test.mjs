import test from "node:test";
import assert from "node:assert/strict";
import { elideOldResults } from "../src/ac-server.mjs";

// A long workspace session: every round is a tool call and a large result.
const session = rounds => [{ role: "user", content: "go" }, ...Array.from({ length: rounds }, (_, i) => [
  { role: "assistant", content: [{ type: "tool_use", id: `t${i}`, name: "read", input: {} }] },
  { role: "user", content: [{ type: "tool_result", tool_use_id: `t${i}`, content: `${i}:`.padEnd(30_000, "x") }] },
]).flat()];
const elided = messages => messages.filter(m => Array.isArray(m.content) && m.content.some(b => /elided from history/.test(b.content || ""))).length;

test("old results are cut in steps, so the start of a long session holds still for the prompt cache", () => {
  assert.equal(elided(elideOldResults(session(7))), 0); // under the size line: untouched
  const counts = Array.from({ length: 20 }, (_, i) => elided(elideOldResults(session(i + 9))));
  assert.deepEqual([...new Set(counts)], [0, 8, 16, 24]);
  // Between steps a request begins exactly as the one before it did.
  const before = elideOldResults(session(13)), after = elideOldResults(session(14));
  assert.deepEqual(after.slice(0, before.length), before);
  // The newest rounds are always whole.
  for (const n of [12, 19, 20, 28]) assert.ok(elideOldResults(session(n)).slice(-8).every(m => elided([m]) === 0));
});
