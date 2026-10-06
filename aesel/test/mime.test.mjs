import test from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, writeFileSync, truncateSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { planMime, postMime, mimeType, threadUrl, MAX_BYTES } from "../src/mime.mjs";

const dir = mkdtempSync(path.join(tmpdir(), "aesel-mime-"));
const mp3 = path.join(dir, "climb.mp3");
writeFileSync(mp3, Buffer.from("ID3 sound"));

test("a plan names the file, its type and the caption, and refuses what the server would", () => {
  const plan = planMime("climb.mp3", { cwd: dir, caption: "  lift two  " });
  assert.deepEqual({ ...plan, path: path.basename(plan.path) }, { path: "climb.mp3", name: "climb.mp3", type: "audio/mpeg", size: 9, caption: "lift two" });
  assert.equal(mimeType("x.FLAC"), "audio/flac");
  assert.equal(mimeType("x.unknown"), "application/octet-stream");
  writeFileSync(path.join(dir, "empty.png"), "");
  assert.throws(() => planMime("empty.png", { cwd: dir }), /empty/);
  assert.throws(() => planMime("missing.wav", { cwd: dir }), /no such file/);
  assert.throws(() => planMime(".", { cwd: dir }), /not a file/);
  const big = path.join(dir, "big.wav");
  writeFileSync(big, "");
  truncateSync(big, MAX_BYTES + 1);
  assert.throws(() => planMime(big), /8\.0 MB; mime\.ac takes up to 8 MB/);
});

test("a post is an opening post signed by the session's token, and answers with its thread", async () => {
  const sent = [];
  const fetchImpl = async (url, init) => { sent.push({ url, init }); return { ok: true, status: 200, json: async () => ({ code: "abc", board: "audio/mpeg", parent: null }) }; };
  const session = { signedIn: true, token: async () => "tok" };
  const posted = await postMime(planMime(mp3, { caption: "hi" }), { session, fetchImpl, site: "https://ac.test" });
  assert.deepEqual(posted, { code: "abc", board: "audio/mpeg", url: "https://mime.ac/#/t/abc" });
  assert.equal(sent[0].url, "https://ac.test/api/mime");
  assert.equal(sent[0].init.headers.Authorization, "Bearer tok");
  const body = JSON.parse(sent[0].init.body);
  assert.deepEqual({ ...body, file: { ...body.file, data: Buffer.from(body.file.data, "base64").toString() } },
    { parent: null, text: "hi", file: { name: "climb.mp3", type: "audio/mpeg", data: "ID3 sound" } });
  assert.equal(threadUrl("x"), "https://mime.ac/#/t/x");
});

test("signed out or refused, it says why", async () => {
  await assert.rejects(postMime(planMime(mp3), { session: { signedIn: false } }), /ac login/);
  const fetchImpl = async () => ({ ok: false, status: 400, json: async () => ({ error: "file over 8 MB" }) });
  await assert.rejects(postMime(planMime(mp3), { session: { signedIn: true, token: async () => "t" }, fetchImpl }), /file over 8 MB/);
});
