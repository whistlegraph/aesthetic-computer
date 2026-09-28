import test from "node:test";
import assert from "node:assert/strict";
import { runWorkspaceTool } from "../src/workspace-tools.mjs";

const approve = async () => true;

test("a command that leaves a server running in the background still returns", async () => {
  const started = Date.now();
  const out = await runWorkspaceTool("bash", { command: "echo up && (sleep 5 &) ; sleep 6 & echo done" }, { cwd: "/tmp", approve });
  assert.match(out, /up\ndone\n\[exit 0\]/);
  assert.ok(Date.now() - started < 3000, "returned without waiting for the background sleeps");
});

test("an interrupt kills the whole pipeline, not just the shell", async () => {
  const started = Date.now();
  await runWorkspaceTool("bash", { command: "sleep 30 | cat" }, { cwd: "/tmp", approve, signal: AbortSignal.timeout(500) });
  assert.ok(Date.now() - started < 3000, "returned once interrupted");
});
