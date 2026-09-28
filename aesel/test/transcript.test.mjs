import assert from "node:assert/strict";
import { appendFileSync, existsSync, writeFileSync } from "node:fs";
import { mkdtemp, readFile, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { SUMMARY_MAX, Transcript } from "../src/transcript.mjs";

async function root(context) {
  const dir = await mkdtemp(join(tmpdir(), "easel-transcript-"));
  context.after(() => rm(dir, { recursive: true, force: true }));
  return dir;
}

const readJson = async (path) => JSON.parse(await readFile(path, "utf8"));
const tick = () => new Promise((resolve) => setImmediate(resolve));

test("many events in one tick land in one append and all survive flush", async (context) => {
  const dir = await root(context);
  const transcript = new Transcript({ sessionId: "s1", root: dir });
  assert.equal((await stat(transcript.dir)).mode & 0o777, 0o700);

  transcript.event("user", { text: "draw a circle" });
  transcript.event("turn", { status: "started", engine: "claude", model: "claude-opus-5" });
  for (let i = 0; i < 200; i += 1) transcript.event("assistant", { text: `tok${i}`, final: false });
  transcript.event("assistant", { text: "done", final: true });
  // Nothing has hit the disk yet: the batch waits for the tick.
  assert.equal(existsSync(transcript.eventsPath), false);
  assert.equal(transcript.flush(), 203);
  assert.equal(transcript.flush(), 0);

  const events = Transcript.read("s1", dir);
  assert.equal(events.length, 203);
  assert.equal(events[0].kind, "user");
  assert.equal(events[0].text, "draw a circle");
  assert.ok(Number.isInteger(events[0].ts));
  assert.deepEqual(events.at(-1), { ts: events.at(-1).ts, kind: "assistant", text: "done", final: true });
  assert.equal((await stat(transcript.eventsPath)).mode & 0o777, 0o600);

  // Without an explicit flush the batch still goes out on the next tick.
  transcript.event("notice", { text: "later" });
  await tick();
  assert.equal(Transcript.read("s1", dir).at(-1).text, "later");
  transcript.close();
});

test("tool inputs and results are clipped to the summary length", async (context) => {
  const dir = await root(context);
  const transcript = new Transcript({ sessionId: "s1", root: dir });
  transcript.event("tool_call", { name: "Bash", input: { command: "x".repeat(2000) } });
  transcript.event("tool_result", { name: "Bash", summary: "y".repeat(2000) });
  transcript.event("approval", { subject: "rm -rf build", decision: "once" });
  transcript.close();
  const [call, result, approval] = Transcript.read("s1", dir);
  assert.equal(call.name, "Bash");
  assert.equal(call.input.length, SUMMARY_MAX);
  assert.ok(call.input.startsWith('{"command":"xxx'));
  assert.equal(result.summary.length, SUMMARY_MAX);
  assert.deepEqual(approval, { ts: approval.ts, kind: "approval", subject: "rm -rf build", decision: "once" });
});

test("meta merges patches and keeps session_id and updated", async (context) => {
  const dir = await root(context);
  const transcript = new Transcript({ sessionId: "s1", root: dir });
  let meta = await readJson(transcript.metaPath);
  assert.equal(meta.session_id, "s1");
  assert.ok(meta.started);
  assert.equal(meta.private, false);
  assert.equal((await stat(transcript.metaPath)).mode & 0o777, 0o600);

  transcript.meta({ cwd: "/project", engine: "claude", model: "claude-opus-5", handle: "jeffrey", pro: true });
  transcript.meta({ subject: "draw a circle", model: "claude-sonnet-5" });
  meta = await readJson(transcript.metaPath);
  assert.equal(meta.cwd, "/project");
  assert.equal(meta.engine, "claude");
  assert.equal(meta.model, "claude-sonnet-5");
  assert.equal(meta.handle, "jeffrey");
  assert.equal(meta.pro, true);
  assert.equal(meta.subject, "draw a circle");
  assert.equal(meta.session_id, "s1");
  assert.ok(meta.updated >= meta.started);

  // Reopening the same session picks the record back up rather than resetting it.
  const again = new Transcript({ sessionId: "s1", root: dir });
  assert.equal(again.record.cwd, "/project");
  assert.equal(again.record.started, meta.started);
});

test("a private transcript never writes its subject", async (context) => {
  const dir = await root(context);
  const transcript = new Transcript({ sessionId: "p1", root: dir, private: true });
  transcript.meta({ subject: "client brief for fuser", cwd: "/Users/x/fuser" });
  const meta = await readJson(transcript.metaPath);
  assert.equal(meta.subject, "private");
  assert.equal(meta.private, true);
  assert.equal(meta.cwd, "/Users/x/fuser");
  assert.equal(transcript.meta({ subject: "try again" }).subject, "private");
});

test("list orders sessions by updated and read tolerates a torn tail", async (context) => {
  const dir = await root(context);
  const older = new Transcript({ sessionId: "old", root: dir });
  // meta() stamps its own updated; force the older one back in time.
  writeFileSync(older.metaPath, JSON.stringify({ ...older.record, updated: "2026-01-01T00:00:00.000Z" }));
  const newer = new Transcript({ sessionId: "new", root: dir });
  newer.event("user", { text: "hi" });
  newer.close();
  writeFileSync(join(dir, "stray"), "not a session");
  appendFileSync(newer.eventsPath, '{"ts":1,"kind":"assist');

  const listed = Transcript.list(dir);
  assert.deepEqual(listed.map((m) => m.session_id), ["new", "old"]);
  assert.deepEqual(Transcript.read("new", dir).map((e) => e.kind), ["user"]);
  assert.deepEqual(Transcript.read("missing", dir), []);
  assert.deepEqual(Transcript.list(join(dir, "nowhere")), []);
});
