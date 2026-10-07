import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { SlabSession, previewAddress } from "../src/slab-session.mjs";

const readJson = async (path) => JSON.parse(await readFile(path, "utf8"));
const exists = async (path) => stat(path).then(() => true, () => false);

test("reification keeps Slab identity and a borrowed preview without restoring sockets", async t => {
  const root = await mkdtemp(join(tmpdir(), "aesel-slab-reify-"));
  t.after(()=>rm(root,{recursive:true,force:true}));
  const options = {cwd:"/project",tty:"ttys099",sessionId:"stable-session",slabHome:root,pro:true};
  const before = new SlabSession(options); before.start();
  before.working("improve Aesel");
  before.live("real.mjs", "prompt.ac/@tester/real", "tester/real", {source:"publication"});
  before.preview("notepat"); before.complete(); before.inboxSocket("old.sock");
  const snapshot = before.snapshot(); before.close({preserve:true});
  assert.equal(await exists(before.active), true);
  assert.equal(snapshot.record.inbox_socket, undefined);
  const after = new SlabSession(options); after.start(); after.inboxSocket("new.sock"); after.restore(snapshot);
  const marker = await readJson(after.active);
  assert.equal(marker.subject, "improve Aesel"); assert.equal(marker.state,"complete");
  assert.equal(marker.scan_url,"prompt.ac/notepat"); assert.equal(marker.inbox_socket,"new.sock");
  after.preview("");
  assert.equal((await readJson(after.active)).scan_url,"prompt.ac/@tester/real");
  assert.equal((await readJson(after.active)).preview_source,"publication");
  after.close();
});

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
  assert.equal(marker.agent_type, "aesel");
  assert.equal(marker.state, "blank");

  session.connected("00000000-0000-0000-0000-000000000001", "claude");
  session.working("make this window visible to prox");
  marker = await readJson(active);
  assert.equal(marker.provider_session_id, "00000000-0000-0000-0000-000000000001");
  assert.equal(marker.provider_agent_type, "claude");
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
  assert.equal(marker.preview_source, "");
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

test("tells the menubar when the pointer should be a hand", async (context) => {
  const { createServer } = await import("node:net");
  const { mkdir } = await import("node:fs/promises");
  const root = await mkdtemp(join(tmpdir(), "aesel-cursor-"));
  context.after(() => rm(root, { recursive: true, force: true }));
  await mkdir(join(root, "state"), { recursive: true });
  const lines = [];
  let closed;
  const hung = new Promise((resolve) => { closed = resolve; });
  const server = createServer((client) => {
    let buffer = "";
    client.on("data", (chunk) => {
      buffer += chunk;
      for (let i; (i = buffer.indexOf("\n")) >= 0; buffer = buffer.slice(i + 1)) lines.push(JSON.parse(buffer.slice(0, i)));
    });
    client.on("close", closed);
  });
  await new Promise((resolve) => server.listen(join(root, "state", "cursor.sock"), resolve));
  context.after(() => server.close());
  const session = new SlabSession({ cwd: "/project", tty: "ttys099", sessionId: "ac-session", slabHome: root });
  session.start();
  session.pointer("hand");
  session.pointer("hand"); // unchanged: not sent again
  session.close();
  await hung;
  assert.deepEqual(lines, [
    { cursor: "hand", session: "ac-session", tty: "ttys099" },
    { cursor: "arrow", session: "ac-session", tty: "ttys099" },
  ]);
});

test("a pointer with nobody listening stays quiet", async (context) => {
  const root = await mkdtemp(join(tmpdir(), "aesel-no-slab-"));
  context.after(() => rm(root, { recursive: true, force: true }));
  const session = new SlabSession({ cwd: "/project", tty: "ttys099", sessionId: "nobody", slabHome: root });
  session.start();
  session.pointer("hand");
  session.pointer("arrow");
  session.close();
});

test("previews any piece without taking it as the session's own", async (context) => {
  const root = await mkdtemp(join(tmpdir(), "easel-slab-"));
  context.after(() => rm(root, { recursive: true, force: true }));
  const session = new SlabSession({ cwd: "/project", pid: process.pid, tty: "ttys099", sessionId: "ac-preview", slabHome: root });
  const active = join(root, "state", "active-prompts", "ac-preview");
  session.start();
  session.live("butterfly.mjs", "prompt.ac/@jeffrey/butterfly", "jeffrey/butterfly");
  session.flow("ahead");

  assert.equal(session.preview("https://aesthetic.computer/notepat"), "prompt.ac/notepat");
  let marker = await readJson(active);
  assert.equal(marker.scan_url, "prompt.ac/notepat");
  assert.equal(marker.piece, "");
  assert.equal(marker.flow, "live");

  // The session keeps writing its own piece underneath; the card stays put.
  session.live("moth.mjs", "prompt.ac/@jeffrey/moth", "jeffrey/moth");
  session.flow("pushing");
  marker = await readJson(active);
  assert.equal(marker.scan_url, "prompt.ac/notepat");

  assert.equal(session.preview(""), "");
  marker = await readJson(active);
  assert.equal(marker.scan_url, "prompt.ac/@jeffrey/moth");
  assert.equal(marker.piece, "moth.mjs");
  assert.equal(marker.flow, "pushing");
});

test("names a piece any way it is written", () => {
  assert.equal(previewAddress("notepat"), "prompt.ac/notepat");
  assert.equal(previewAddress("/notepat:c"), "prompt.ac/notepat:c");
  assert.equal(previewAddress("@jeffrey/butterfly"), "prompt.ac/@jeffrey/butterfly");
  assert.equal(previewAddress("$cow"), "prompt.ac/$cow");
  assert.equal(previewAddress("prompt.ac/@jeffrey/butterfly"), "prompt.ac/@jeffrey/butterfly");
  assert.equal(previewAddress("  "), "");
});

test("repo sessions advertise the source of an explicit preview", async (context) => {
  const root = await mkdtemp(join(tmpdir(), "easel-slab-"));
  context.after(() => rm(root, { recursive: true, force: true }));
  const session = new SlabSession({ cwd: "/project", tty: "ttys099", sessionId: "repo", slabHome: root, pro: true });
  context.after(() => session.close());
  session.start();
  const active = join(root, "state/active-prompts/repo");
  session.live("test.mjs", "prompt.ac/@tester/test");
  assert.equal((await readJson(active)).preview_source, "", "a URL alone is not a publication");
  session.live("real.mjs", "prompt.ac/@tester/real", "tester/real", { source: "publication" });
  assert.equal((await readJson(active)).preview_source, "publication");
  session.preview("notepat");
  assert.equal((await readJson(active)).preview_source, "manual");
  session.preview("");
  assert.equal((await readJson(active)).preview_source, "publication");
  session.artifact("picture", { path: "/tmp/real.png", version: 1 });
  assert.equal((await readJson(active)).preview_source, "artifact");
});
