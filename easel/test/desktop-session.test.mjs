import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, stat, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { AcServer } from "../src/ac-server.mjs";
import { desktopSnapshot, readDesktopSession, writeDesktopSession, restoreDesktopEngine, writeDesktopControl, readDesktopIntent } from "../src/desktop-session.mjs";
async function fixture(t) {
  const root = await mkdtemp(join(tmpdir(), "desktop-session-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const engine = new AcServer({ token: async () => "AUTH-TOKEN-MUST-NOT-PERSIST", fetch: async () => new Response('data: {"type":"content_block_delta","delta":{"type":"text_delta","text":"purple dots"}}\n\ndata: {"type":"message_delta","delta":{"stop_reason":"end_turn"}}\n\n') });
  await engine.connect(); await engine.startTurn("paint dots");
  const snapshot = desktopSnapshot({ cwd: root, backend: "ac", model: engine.model,
    live: { file: join(root, "dots.mjs"), runtime: { id: "mjs" }, fallbackChannel: "channel" },
    state: { entries: [{ id: "reply", kind: "assistant", text: "purple dots" }], input: "unfinished 🟣 draft", cursor: 12, history: ["paint dots"], historyIndex: 1, queued: ["make it spin"], medium: "piece", autoAllow: true, account: "PRIVATE-ACCOUNT", token: "PRIVATE-TOKEN" },
    options: { autopublish: false, mouseEnabled: true, secret: "PRIVATE-KEY" }, engine, handoff: "context", archivedConversation: [] });
  return { root, engine, snapshot };
}
test("a desktop restart restores actual AC history, thread ID, draft and queue exactly", async (t) => {
  const { root, engine, snapshot } = await fixture(t);
  const file = join(root, "session.json");
  await writeDesktopSession(file, snapshot);
  const restored = await readDesktopSession(file, root);
  const next = new AcServer(); restoreDesktopEngine(next, restored); await next.connect();
  assert.deepEqual(next.messages, engine.messages); assert.equal(next.threadId, engine.threadId); assert.equal(next.turns, engine.turns);
  assert.deepEqual(restored.ui, snapshot.ui);
  assert.equal((await stat(file)).mode & 0o777, 0o600);
  const raw = await readFile(file, "utf8");
  assert.doesNotMatch(raw, /AUTH-TOKEN|PRIVATE-/);
  assert.equal(restored.live.channel, "channel");
});
test("corrupt and cross-workspace snapshots are rejected without rewriting them", async (t) => {
  const { root, snapshot } = await fixture(t); const file = join(root, "session.json");
  await writeFile(file, "{bad"); await assert.rejects(readDesktopSession(file, root));
  assert.equal(await readFile(file, "utf8"), "{bad");
  await writeDesktopSession(file, snapshot); await assert.rejects(readDesktopSession(file, join(root, "other")), /workspace/);
});
test("failed snapshot writes cannot emit a restart request", async (t) => {
  const { root, snapshot } = await fixture(t); const blocked = join(root, "blocked"); await writeFile(blocked, "file");
  const controlPath = join(root, "control.json");
  await assert.rejects(writeDesktopControl({ sessionPath: join(blocked, "session.json"), controlPath, snapshot, action: "restart" }));
  await assert.rejects(stat(controlPath), { code: "ENOENT" });
  await writeDesktopControl({ sessionPath: join(root, "session.json"), controlPath, snapshot, action: "update" });
  assert.deepEqual(JSON.parse(await readFile(controlPath)), { action: "update" });
  assert.equal((await stat(controlPath)).mode & 0o777, 0o600);
  await writeDesktopControl({ sessionPath: join(root, "session.json"), controlPath, snapshot, action: "home" });
  assert.deepEqual(JSON.parse(await readFile(controlPath)), { action: "home" });
  const saved = await readDesktopSession(join(root, "session.json"), root);
  assert.deepEqual(saved.ui, snapshot.ui);
  assert.deepEqual(saved.engine, snapshot.engine);
});
test("desktop intent accepts restart/update/home and consumes the request", async (t) => {
  const { root } = await fixture(t); const file = join(root, "intent.json");
  assert.equal(await readDesktopIntent(file), "restart");
  await writeFile(file, JSON.stringify({ action: "update" })); assert.equal(await readDesktopIntent(file), "update");
  await assert.rejects(stat(file), { code: "ENOENT" });
  await writeFile(file, JSON.stringify({ action: "home" })); assert.equal(await readDesktopIntent(file), "home");
  await writeFile(file, JSON.stringify({ action: "shell", command: "unsafe" })); await assert.rejects(readDesktopIntent(file), /Invalid/);
});
