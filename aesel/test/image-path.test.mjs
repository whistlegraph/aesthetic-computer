import test from "node:test";
import assert from "node:assert/strict";
import { ImagePath } from "../src/image-path.mjs";

test("No Paint uses its quoted billing endpoint once, preserves receipts after No, and blocks overlapping work", async () => {
  const calls = [], states = [];
  let finish;
  const path = new ImagePath({ path: "nopaint", onchange: state => states.push(state),
    request: (...args) => { calls.push(args); return new Promise(resolve => { finish = resolve; }); } });
  const input = { requestId: "move-1", image: "png", quote: "price-1", model: "chosen", hint: "red" };
  const first = path.run(input);
  assert.equal(path.run({ ...input }), first);
  await assert.rejects(path.run({ ...input, hint: "blue" }), /different input/);
  await assert.rejects(path.run({ ...input, requestId: "move-2" }), /still processing/);
  assert.deepEqual(calls, [["/api/nopaint-inference", input, 200000]]);
  path.cancel();
  path.reset();
  assert.equal(path.state, "interrupted");
  assert.equal(path.pending, true);
  const receipt = { image: "paid-result", billing: { braincells: 4000, free: 4000, paid: 0 } };
  finish(receipt);
  assert.equal(await first, receipt);
  assert.equal(await path.run(input), receipt);
  assert.equal(calls.length, 1);
  assert.equal(path.pending, false);
  assert.deepEqual(states, ["working", "interrupted", "interrupted"]);
});

test("provider and network failures are never automatically retried, including a duplicate click", async () => {
  let calls = 0;
  const failure = Object.assign(Error("Provider limit"), { code: "provider_rate_limited", charged: 0 });
  const path = new ImagePath({ path: "nopaint", request: async () => { calls++; throw failure; } });
  const input = { requestId: "failed-move" };
  for (let i = 0; i < 2; i++) await assert.rejects(path.run(input), error => error === failure);
  assert.equal(calls, 1);
  assert.equal(path.state, "stale");
  path.reset();
  assert.equal(path.state, "blank");
});

test("picture tools use the same lifecycle with their own server contract; observers cannot retry work", async () => {
  let calls = 0;
  const path = new ImagePath({ path: "picture", onchange: () => { throw Error("view gone"); },
    request: async (endpoint, body, timeout) => {
      calls++;
      assert.equal(endpoint, "/api/easel-image");
      assert.equal(body.jobId, "picture-1");
      assert.equal(timeout, 330000);
      return { png: "image" };
    } });
  await assert.rejects(path.run({}), /request ID/);
  assert.equal(calls, 0);
  await path.run({ jobId: "picture-1" });
  assert.equal(path.state, "complete");
  assert.equal(calls, 1);
});
