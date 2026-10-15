import test from "node:test";
import assert from "node:assert/strict";
import { errorNames } from "../diagnostics.mjs";

test("server diagnostics send the private service credential and return only bounded function names", async t => {
  const previous = process.env.LITH_OPERATIONS_TOKEN;
  t.after(() => {
    if (previous === undefined) delete process.env.LITH_OPERATIONS_TOKEN;
    else process.env.LITH_OPERATIONS_TOKEN = previous;
  });
  process.env.LITH_OPERATIONS_TOKEN = "fixture-service";
  t.mock.method(globalThis, "fetch", async (url, options) => {
    assert.equal(url, "http://127.0.0.1:8888/lith/errors?limit=500");
    assert.deepEqual(options.headers, { Authorization: "Bearer fixture-service" });
    return Response.json({ errors: [{ fn: "example", time: "2026-10-14T12:00:00Z", error: "private detail" }], total: 1 });
  });
  assert.deepEqual(await errorNames(new Date("2026-10-14"), new Date("2026-10-15"), 600), { names: ["example"], scanned: 1, truncated: false });
  delete process.env.LITH_OPERATIONS_TOKEN;
  await assert.rejects(errorNames(new Date(), new Date(), 10), /credential unavailable/);
  assert.equal(globalThis.fetch.mock.calls.length, 1);
});
