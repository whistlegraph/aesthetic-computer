import test from "node:test";
import assert from "node:assert/strict";
import { handleLine, rolloutFromTitle } from "../bin/codex-session-watch.mjs";

const event = (type, ctx) => handleLine(JSON.stringify({ type: "event_msg", payload: { type } }), ctx);
test("long Codex turns keep the working heartbeat until completion, approval or interruption", () => {
  for (const end of ["task_complete", "turn_complete", "turn_aborted", "approval_request"]) {
    const ctx = { pending: [], lastUser: "test", turnActive: false };
    event("task_started", ctx);
    assert.equal(ctx.turnActive, true);
    handleLine(JSON.stringify({ type: "response_item", payload: { type: "function_call" } }), ctx);
    assert.equal(ctx.turnActive, true, "Tool calls must not end the active turn");
    event(end, ctx);
    assert.equal(ctx.turnActive, false, end);
    event("task_started", ctx);
    assert.equal(ctx.turnActive, true, "A subsequent turn returns to green");
  }
});
test("reattaching publishes only the final status, without replaying historical work", () => {
  const ctx = { pending: [], lastUser: "", turnActive: false, replaying: true };
  for (const type of ["task_started", "task_complete", "task_started"]) event(type, ctx);
  assert.equal(ctx.pending.length, 1);
  assert.equal(ctx.turnActive, true);
});
test("shared daemon threads bind by this window's exact name and cwd, never recency", () => {
  const rows = [
    { name: "Fix colors", cwd: "/work/repo", rollout_path: "/sessions/ours.jsonl" },
    { name: "Fix status", cwd: "/work/repo", rollout_path: "/sessions/peer.jsonl" },
    { name: "Fix colors", cwd: "/other/repo", rollout_path: "/sessions/other.jsonl" },
  ];
  assert.equal(rolloutFromTitle(rows, "◌ Fix colors | repo", "/work/repo"), rows[0].rollout_path);
  assert.equal(rolloutFromTitle(rows, "◌ Fix status | repo", "/work/repo"), rows[1].rollout_path);
  assert.equal(rolloutFromTitle(rows, "New session | repo", "/work/repo"), null);
  assert.equal(rolloutFromTitle([...rows, {...rows[0], rollout_path: "/sessions/duplicate.jsonl"}],
    "◌ Fix colors | repo", "/work/repo"), null);
});
