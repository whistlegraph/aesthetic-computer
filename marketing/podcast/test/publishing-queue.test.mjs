import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, existsSync, rmSync, cpSync } from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { enqueueEpisode, publishingQueue, drainPublishingQueue, isBillingFailure } from "../lib/publishing-queue.mjs";
import { deliverDaily } from "../lib/daily-delivery.mjs";

const podcast = "2628235", token = "test-only";
const billing = () => new Response(JSON.stringify({ base: ["Unable to upload new episodes to this subscription."] }), { status: 400 });
function fixture(t) {
  const root = mkdtempSync(resolve(tmpdir(), "podcast-queue-"));
  const out = resolve(root, "out");
  mkdirSync(out);
  t.after(() => rmSync(root, { recursive: true, force: true }));
  const produce = (slug, pubDate = "2026-10-05T00:32:00Z") => {
    writeFileSync(resolve(out, `${slug}.mp3`), `audio:${slug}`);
    writeFileSync(resolve(out, `${slug}.json`), JSON.stringify({ title: slug, description: "Original notes", pubDate }));
    writeFileSync(resolve(out, `${slug}-cover.png`), "original artwork");
  };
  const enqueue = (slug, extra = {}) => enqueueEpisode({ out, podcast, slug, ...extra });
  const drain = (fetch, extra = {}) => drainPublishingQueue({ out, podcast, token, fetch, log() {}, ...extra });
  return { root, out, produce, enqueue, drain };
}

test("billing queues once; recovery uploads the saved audio, art, date and notes only once", async (t) => {
  const f = fixture(t), slug = "daily-2026-10-04";
  f.produce(slug);
  f.enqueue(slug);
  assert.equal((await f.drain(billing)).remaining, 1);
  assert.equal(publishingQueue(f.out)[0].status, "billing");
  writeFileSync(resolve(f.out, `${slug}.mp3`), "changed audio");
  writeFileSync(resolve(f.out, `${slug}.json`), JSON.stringify({ title: "changed title" }));
  f.enqueue(slug);
  assert.equal(publishingQueue(f.out).length, 1);
  let requests = 0;
  const upload = async (_url, { body }) => {
    requests++;
    assert.equal(body.get("title"), slug);
    assert.match(body.get("description"), /Original notes/);
    assert.equal(body.get("published_at"), "2026-10-05T00:32:00.000Z");
    assert.equal(await body.get("audio_file").text(), `audio:${slug}`);
    assert.equal(await body.get("artwork_file").text(), "original artwork");
    return Response.json({ id: 123, private: false });
  };
  assert.equal((await f.drain(upload)).remaining, 0);
  f.enqueue(slug);
  await f.drain(upload);
  assert.equal(requests, 1);
  assert.equal(JSON.parse(readFileSync(resolve(f.out, `${slug}.buzzsprout.json`))).id, 123);
});

test("a billing block stops retries after one rejection, then recovers oldest first", async (t) => {
  const f = fixture(t);
  for (const day of [6, 4, 5]) {
    const slug = `daily-2026-10-0${day}`;
    f.produce(slug, `2026-10-0${day}T20:30:00Z`);
    f.enqueue(slug);
  }
  assert.deepEqual(await f.drain(billing), { attempted: 1, remaining: 3, needsReview: false });
  const order = [];
  const result = await f.drain(async (_url, { body }) => {
    order.push(body.get("title"));
    return Response.json({ id: 100 + order.length });
  }, { limit: 2 });
  assert.deepEqual(order, ["daily-2026-10-04", "daily-2026-10-05"]);
  assert.equal(result.remaining, 1);
});

test("private uploads stay private and cannot be silently promoted on retry", async (t) => {
  const f = fixture(t), slug = "daily-2026-10-04";
  f.produce(slug);
  f.enqueue(slug, { privateEpisode: true });
  assert.throws(() => f.enqueue(slug), /visibility/);
  await f.drain(billing);
  await f.drain(async (_url, { body }) => {
    assert.equal(body.get("private"), "true");
    assert.equal(body.has("published_at"), false);
    return Response.json({ id: 123, private: true });
  });
});

test("a lost response is retained for review instead of risking a duplicate POST", async (t) => {
  const f = fixture(t), slug = "daily-2026-10-04";
  f.produce(slug); f.enqueue(slug);
  await assert.rejects(f.drain(async () => { throw new Error("connection lost"); }), /response lost/);
  assert.equal(publishingQueue(f.out)[0].status, "uncertain");
  const result = await f.drain(() => assert.fail("must not repost"));
  assert.equal(result.needsReview, true);
  // Recovering a receipt after inspecting Buzzsprout cleans up without a POST.
  writeFileSync(resolve(f.out, `${slug}.buzzsprout.json`), JSON.stringify({ id: 123 }));
  assert.equal((await f.drain(() => assert.fail("already accepted"))).remaining, 0);
});

test("unrelated validation errors stay visible and public allowlisting is enforced", async (t) => {
  const f = fixture(t), slug = "daily-2026-10-04";
  assert.equal(isBillingFailure(400, 'invalid audio file'), false);
  assert.equal(isBillingFailure(402, ''), true);
  f.produce("not-cleared");
  assert.throws(() => f.enqueue("not-cleared"), /allowlist/);
  f.produce(slug); f.enqueue(slug);
  await assert.rejects(f.drain(() => new Response('invalid audio file', { status: 400 })), /HTTP 400/);
  assert.equal(publishingQueue(f.out)[0].status, "failed");
});

test("a concurrent retry cannot POST the same job", async (t) => {
  const f = fixture(t), slug = "daily-2026-10-04";
  f.produce(slug); f.enqueue(slug);
  let release;
  const first = f.drain(() => new Promise((resolve) => { release = resolve; }));
  await assert.rejects(f.drain(() => assert.fail("duplicate")), /lock exists/);
  release(Response.json({ id: 123 }));
  await first;
});

test("billing and other delivery failures both allow the independent token stage", (t) => {
  const { root } = fixture(t);
  for (const status of [75, 1]) {
    const calls = [];
    const result = deliverDaily({ root, date: "2026-10-04", mint: true, log() {}, run: (_cmd, args) => {
      calls.push(args);
      return { status: args.includes("retry") ? status : 0 };
    } });
    assert.deepEqual(calls.at(-1), ["bin/daily-token.mjs", "--date", "2026-10-04"]);
    assert.equal(result, status === 75 ? 0 : 1);
  }
});

test("a staged daily never mints a public token", (t) => {
  const { root, out } = fixture(t);
  for (const stage of [true, false]) {
    if (!stage) writeFileSync(resolve(out, 'daily-2026-10-04.buzzsprout.json'), JSON.stringify({ id: 123, private: true }));
    deliverDaily({ root, date: "2026-10-04", stage, mint: true, log() {}, run: (_cmd, args) => {
      assert.notEqual(args[0], "bin/daily-token.mjs");
      return { status: 0 };
    } });
  }
});

test("rerunning a produced daily skips writing and narration, retries delivery, and reaches mint", (t) => {
  const f = fixture(t), slug = "daily-2026-10-04";
  const source = fileURLToPath(new URL("../", import.meta.url));
  mkdirSync(resolve(f.root, "bin")); mkdirSync(resolve(f.root, "lib")); mkdirSync(resolve(f.out, "daily"));
  for (const file of ["bin/daily.mjs", "lib/daily-delivery.mjs", "lib/publishing-queue.mjs", "lib/hosted.mjs"]) cpSync(resolve(source, file), resolve(f.root, file));
  f.produce(slug);
  const script = resolve(f.out, "daily", `${slug}.md`);
  writeFileSync(script, "already produced script");
  writeFileSync(resolve(f.root, "bin/buzzsprout.mjs"), 'process.exit(process.argv.includes("retry") ? 75 : 0);');
  writeFileSync(resolve(f.root, "bin/daily-token.mjs"), 'import {writeFileSync} from "node:fs"; writeFileSync("out/mint-reached", "yes");');
  for (let attempt = 0; attempt < 2; attempt++) {
    const run = spawnSync(process.execPath, [resolve(f.root, "bin/daily.mjs"), "--date", "2026-10-04"], { env: { ...process.env, DAILY_MINT: "1" }, encoding: "utf8" });
    assert.equal(run.status, 0, run.stderr);
    assert.match(run.stdout, /reusing its saved episode/);
  }
  assert.equal(readFileSync(script, "utf8"), "already produced script");
  assert.ok(existsSync(resolve(f.out, "mint-reached")));
});
