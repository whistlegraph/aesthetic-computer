import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { PieceRevisions, validatePieceSource } from "../src/revisions.mjs";
import { LivePiece } from "../src/live.mjs";

async function setup(t) {
  const root = await mkdtemp(join(tmpdir(), "easel-revision-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const file = join(root, "piece.mjs");
  return { root, file, history: new PieceRevisions(file, { root: join(root, "history") }) };
}

test("revisions survive restart, deduplicate saves, and rollback appends", async (t) => {
  const { root, file, history } = await setup(t);
  const first = "export function paint() {}\n";
  const second = "export function paint({ wipe }) { wipe(0); }\n";
  await writeFile(file, first);
  assert.equal(history.capture(first).version, 1);
  assert.equal(history.capture(first).version, 1);
  history.capture(second);
  await writeFile(file, second);
  const reopened = new PieceRevisions(file, { root: join(root, "history") });
  const restored = await reopened.restore(1);
  assert.equal(restored.version, 3);
  assert.equal(restored.restoredFrom, 1);
  assert.equal(await readFile(file, "utf8"), first);
  assert.deepEqual(reopened.list().map((v) => v.source), [first, second, first]);
  await assert.rejects(reopened.restore(99), /No saved/);
});

test("validation parses without executing and rejects incomplete JavaScript", async () => {
  await validatePieceSource('throw new Error("must not execute"); export const x = 1;', "piece.mjs");
  await assert.rejects(validatePieceSource("export function paint( {", "piece.mjs"), /invalid JavaScript/);
});

test("file watcher versions external edits and never pushes unfinished JavaScript", async (t) => {
  const { root, history } = await setup(t);
  let pushes = 0;
  const live = new LivePiece({ directory: root, slug: "piece", fetch: async () => { pushes++; return new Response("ok"); } });
  Object.defineProperty(live, "history", { get: () => history });
  live.create();
  await live.checkpoint();
  t.after(() => live.unwatch());
  const errors = [];
  live.watch((e) => errors.push(e));
  await writeFile(live.file, "export function paint( {");
  await new Promise((resolve) => setTimeout(resolve, 400));
  assert.equal(pushes, 0);
  assert.equal(history.list().length, 1);
  assert.equal(errors.length, 1);
  const landed = new Promise((resolve) => live.once("push", resolve));
  await writeFile(live.file, "export function paint() {}\n");
  await landed;
  assert.equal(history.list().length, 2);
  await writeFile(live.file, "export function broken(");
  const restored = await live.rollback(1);
  assert.equal(restored.version, 3, "a broken current edit does not prevent recovery");
});

test("live uploads serialize so old saves cannot overtake newer versions", async (t) => {
  const { root, history } = await setup(t);
  let releaseFirst;
  const gate = new Promise((resolve) => { releaseFirst = resolve; });
  const seen = [];
  let firstStarted;
  const started = new Promise((resolve) => { firstStarted = resolve; });
  const live = new LivePiece({ directory: root, slug: "piece", fetch: async (_url, options) => {
    seen.push(JSON.parse(options.body).source);
    if (seen.length === 1) { firstStarted(); await gate; }
    return new Response("ok");
  } });
  Object.defineProperty(live, "history", { get: () => history });
  await writeFile(live.file, "// first\n");
  const first = live.push();
  await started;
  await writeFile(live.file, "// second\n");
  const second = live.push();
  assert.equal(seen.length, 1);
  assert.equal(live.sending, true);
  releaseFirst();
  await Promise.all([first, second]);
  assert.deepEqual(seen, ["// first\n", "// second\n"]);
  assert.equal(live.ahead, false);
  assert.equal(live.sending, false);
});

test("a failed old upload does not discard a queued newer save", async (t) => {
  const { root, history } = await setup(t);
  let failFirst, firstStarted;
  const started = new Promise((resolve) => { firstStarted = resolve; });
  const blocked = new Promise((_resolve, reject) => { failFirst = reject; });
  const seen = [];
  const live = new LivePiece({ directory: root, slug: "piece", fetch: async (_url, options) => {
    seen.push(JSON.parse(options.body).source);
    if (seen.length === 1) { firstStarted(); await blocked; }
    return new Response("ok");
  } });
  Object.defineProperty(live, "history", { get: () => history });
  await writeFile(live.file, "// old\n");
  const old = live.push();
  const failure = assert.rejects(old, /offline/);
  await started;
  await writeFile(live.file, "// latest\n");
  const latest = live.push();
  failFirst(new Error("offline"));
  await failure;
  assert.equal(await latest, true);
  assert.deepEqual(seen, ["// old\n", "// latest\n"]);
});
