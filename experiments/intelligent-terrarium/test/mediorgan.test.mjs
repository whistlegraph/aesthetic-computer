import assert from "node:assert/strict";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
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

test("concurrent outside prods remain a single ordered journal", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-concurrent-"));
  const capability = "local-concurrency-capability";
  const app = await createTerrariumServer({ root, capabilities: { [capability]: "@alex" }, tickMs: 0 });
  const base = `http://${app.address.address}:${app.address.port}`;
  const responses = await Promise.all(Array.from({ length: 8 }, (_, index) => fetch(`${base}/api/prod`, {
    method: "POST",
    headers: { Authorization: `Bearer ${capability}`, "Content-Type": "application/json" },
    body: JSON.stringify({ target: "memory", modality: "text", stimulus: `prod-${index}` }),
  })));
  assert.deepEqual(responses.map((response) => response.status), Array(8).fill(202));
  const sequences = await Promise.all(responses.map(async (response) => (await response.json()).eventSeq));
  assert.equal(new Set(sequences).size, sequences.length);
  assert.deepEqual([...sequences].sort((a, b) => a - b), Array.from({ length: 8 }, (_, index) => index + 2));
  await app.stop();
});
