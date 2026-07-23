import assert from "node:assert/strict";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { verifyRepository } from "../src/repository.mjs";
import { createTerrariumServer } from "../src/server.mjs";

test("server refuses every non-loopback binding", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-bind-"));
  await assert.rejects(createTerrariumServer({ root, host: "0.0.0.0" }), /refuses non-loopback/);
});

test("mediorgan authenticates, journals a bounded prod, and never journals capability", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-mediorgan-"));
  const capability = "local-test-capability-never-journal";
  const app = await createTerrariumServer({ root, capabilities: { [capability]: "@alex" }, tickMs: 0 });
  const base = `http://${app.address.address}:${app.address.port}`;
  assert.equal(app.address.address, "127.0.0.1");
  assert.equal((await fetch(`${base}/api/state`)).status, 401);

  const headers = { Authorization: `Bearer ${capability}`, "Content-Type": "application/json" };
  const stateResponse = await fetch(`${base}/api/state`, { headers });
  assert.equal(stateResponse.status, 200);
  assert.equal((await stateResponse.json()).handle, "@alex");
  const prodResponse = await fetch(`${base}/api/prod`, {
    method: "POST",
    headers,
    body: JSON.stringify({ target: "voice", modality: "text", stimulus: "hello organ", position: { x: -2, y: 1, z: 1 } }),
  });
  assert.equal(prodResponse.status, 202);
  const accepted = await prodResponse.json();
  assert.match(accepted.prodId, /^[0-9a-f-]{36}$/);
  assert.equal(accepted.outputs[0].kind, "sonic");
  assert.match(accepted.outputs[0].cause, new RegExp(accepted.prodId));
  const journal = await readFile(app.repository.segmentPath, "utf8");
  assert.match(journal, /"kind":"organ-prod"/);
  assert.match(journal, /"handle":"@alex"/);
  assert.doesNotMatch(journal, new RegExp(capability));
  await app.stop();
});

test("loopback visitor page exposes a WebGL terrarium and mediorgan controls", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-page-"));
  const app = await createTerrariumServer({ root, capabilities: {}, tickMs: 0 });
  const html = await (await fetch(`http://${app.address.address}:${app.address.port}/`)).text();
  assert.match(html, /<canvas[^>]+terrarium/);
  assert.match(html, /Connect mediorgan/);
  assert.match(html, /Prod organ/);
  assert.match(html, /Xbox-compatible controller/);
  await app.stop();
});

test("concurrent ticks and outside prods remain one contiguous replayable journal", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-concurrent-"));
  const capability = "local-concurrency-capability";
  const app = await createTerrariumServer({ root, capabilities: { [capability]: "@alex" }, tickMs: 1 });
  const base = `http://${app.address.address}:${app.address.port}`;
  const responses = await Promise.all(Array.from({ length: 8 }, (_, index) => fetch(`${base}/api/prod`, {
    method: "POST",
    headers: { Authorization: `Bearer ${capability}`, "Content-Type": "application/json" },
    body: JSON.stringify({ target: "memory", modality: "text", stimulus: `prod-${index}` }),
  })));
  assert.deepEqual(responses.map((response) => response.status), Array(8).fill(202));
  const sequences = await Promise.all(responses.map(async (response) => (await response.json()).eventSeq));
  assert.equal(new Set(sequences).size, sequences.length);
  await new Promise((resolveWait) => setTimeout(resolveWait, 20));
  await app.stop();
  const onlineHash = app.repository.stateHash();
  const records = app.repository.records;
  assert.deepEqual(records.map(({ seq }) => seq), Array.from({ length: records.length }, (_, index) => index + 1));
  assert.equal(records[0].prevHash, "0".repeat(64));
  for (let index = 1; index < records.length; index += 1) {
    assert.equal(records[index].prevHash, records[index - 1].recordHash);
  }
  assert.ok(records.some(({ kind }) => kind === "advance"));
  assert.equal(records.filter(({ kind }) => kind === "organ-prod").length, 8);
  const replay = await verifyRepository(root);
  assert.equal(replay.lastSeq, records.length);
  assert.equal(replay.stateHash, onlineHash);
  assert.equal(replay.headRecordHash, records.at(-1).recordHash);
});

test("tick failures are observed without an unhandled rejection", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-tick-error-"));
  const errors = [];
  const app = await createTerrariumServer({ root, tickMs: 1, onTickError: (error) => errors.push(error) });
  const transact = app.repository.transact.bind(app.repository);
  let rejectNextTick = true;
  app.repository.transact = (kind, payload) => {
    if (kind === "advance" && rejectNextTick) {
      rejectNextTick = false;
      return Promise.reject(new Error("injected tick failure"));
    }
    return transact(kind, payload);
  };
  await new Promise((resolveWait) => setTimeout(resolveWait, 20));
  await app.stop();
  assert.equal(errors.length, 1);
  assert.match(errors[0].message, /injected tick failure/);
  assert.ok(app.repository.records.some(({ kind }) => kind === "advance"));
});
