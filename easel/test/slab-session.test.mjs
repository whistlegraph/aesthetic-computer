import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { SlabSession } from "../src/slab-session.mjs";

const readJson = async (path) => JSON.parse(await readFile(path, "utf8"));
const exists = async (path) => stat(path).then(() => true, () => false);

test("publishes the full Slab prompt lifecycle", async (context) => {
  const root = await mkdtemp(join(tmpdir(), "easel-slab-"));
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
  assert.equal(marker.agent_type, "easel");
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

test("a private marker never carries the prompt, and pro and the inbox socket are advertised", async (context) => {
  const root = await mkdtemp(join(tmpdir(), "easel-slab-"));
  context.after(() => rm(root, { recursive: true, force: true }));
  const session = new SlabSession({
    cwd: "/client",
    pid: process.pid,
    tty: "ttys099",
    sessionId: "ac-private",
    slabHome: root,
    pro: true,
    private: true,
  });
  const active = join(root, "state", "active-prompts", "ac-private");

  session.start();
  let marker = await readJson(active);
  assert.equal(marker.pro, true);
  assert.equal(marker.private, true);
  assert.equal(marker.subject, "private");
  assert.equal(marker.summary, "private");
  assert.equal(marker.inbox_socket, "");

  session.working("rewrite the client's billing brief");
  session.inboxSocket("/tmp/inbox/ac-private/inbox.sock");
  marker = await readJson(active);
  assert.equal(marker.state, "working", "state is still published");
  assert.equal(marker.subject, "private", "the prompt is not");
  assert.equal(marker.summary, "private");
  assert.equal(marker.inbox_socket, "/tmp/inbox/ac-private/inbox.sock");
  assert.equal((await stat(active)).mode & 0o777, 0o600);
  assert.equal(JSON.stringify(marker).includes("billing"), false);

  session.close();
});
