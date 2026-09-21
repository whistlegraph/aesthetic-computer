import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, readFile, writeFile, rm, readdir, unlink } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { setTimeout as delay } from "node:timers/promises";
import { localFrame } from "../lib/frame-local.mjs";

test("local Frame waits for completion, never returning previous pixels", async t => {
  const dir = await mkdtemp(join(tmpdir(), "frame-local-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  await writeFile(join(dir, "frame.done"), "");
  await writeFile(join(dir, "frame.out.json"), '{"capture":"old"}');
  await writeFile(join(dir, "frame.out.jpg"), "old");
  const pending = localFrame(dir, "window noocr session=fixture", { timeoutMs: 1000 });
  let request;
  for (let i = 0; i < 100; i++) {
    request = await readFile(join(dir, "frame.req"), "utf8").catch(() => null);
    if (request) break;
    await delay(5);
  }
  assert.equal(request, "window noocr session=fixture");
  await writeFile(join(dir, "frame.out.json"), '{"capture":"ok"}');
  const jpg = Buffer.from([255, 216, 0, 10, 255, 217]);
  await writeFile(join(dir, "frame.out.jpg"), jpg);
  await writeFile(join(dir, "frame.done"), "");
  const result = await pending;
  assert.equal(JSON.parse(result.json).capture, "ok");
  assert.deepEqual(result.jpg, jpg);
  assert.deepEqual((await readdir(dir)).filter(x => /^frame-/.test(x)), []);
});

test("local Frame times out rather than reading stale output or replaying input", async t => {
  const dir = await mkdtemp(join(tmpdir(), "frame-timeout-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  await writeFile(join(dir, "frame.out.json"), '{"capture":"ok"}');
  await writeFile(join(dir, "frame.out.jpg"), "stale");
  const result = localFrame(dir, "press=12,34,1", { timeoutMs: 40 });
  const rejection = assert.rejects(result, /timeout.*not retried/);
  for (let i = 0; i < 100; i++) {
    try { await unlink(join(dir, "frame.req")); break; } catch {}
    await delay(2);
  }
  await rejection;
  assert.equal((await readdir(dir)).includes("frame.req"), false, "no replay after native consumption");
});
