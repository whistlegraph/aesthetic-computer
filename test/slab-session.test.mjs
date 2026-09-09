import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { SlabSession } from "../src/slab-session.mjs";

const readJson = async (path) => JSON.parse(await readFile(path, "utf8"));
const exists = async (path) => stat(path).then(() => true, () => false);

test("publishes the full Slab prompt lifecycle", async (context) => {
  const root = await mkdtemp(join(tmpdir(), "aesthetic-code-slab-"));
  context.after(() => rm(root, { recursive: true, force: true }));
  const session = new SlabSession({
    cwd: "/project",
    pid: process.pid,
    tty: "ttys099",
    sessionId: "ac-session",
    slabHome: root,
  });
  const active = join(root, "state", "active-prompts", "ac-session");
  const awaiting = join(root, "state", "awaiting-prompts", "ac-session");
  const running = join(root, "state", "running-tools", "ac-session");

  session.start();
  let marker = await readJson(active);
  assert.equal(marker.agent_type, "aesthetic-code");
  assert.equal(marker.state, "blank");

  session.connected("00000000-0000-0000-0000-000000000001");
  session.working("make this window visible to prox");
  marker = await readJson(active);
  assert.equal(marker.provider_session_id, "00000000-0000-0000-0000-000000000001");
  assert.equal(marker.subject, "make this window visible to prox");
  assert.equal(marker.state, "working");
  assert.equal(await exists(running), true);

  session.awaitingInput("needs approval");
  assert.equal(await readFile(awaiting, "utf8"), "needs approval\n");
  assert.equal(await exists(running), false);

  session.resumeWork();
  assert.equal(await exists(awaiting), false);
  assert.equal(await exists(running), true);

  session.complete();
  assert.equal(await readFile(awaiting, "utf8"), "turn complete\n");
  assert.equal((await readJson(active)).state, "complete");

  session.close();
  assert.equal(await exists(active), false);
  assert.equal(await exists(awaiting), false);
});
