// node --experimental-vm-modules --test system/tests/og-image.test.mjs
import test from "node:test";
import assert from "node:assert/strict";
import vm from "node:vm";
import { readFile } from "node:fs/promises";

const source = await readFile(new URL("../netlify/functions/og-image.mjs", import.meta.url), "utf8");
async function fixture(fetch) {
  let now = 1000000;
  const context = vm.createContext({ fetch, Response, URL, AbortController,
    Date: { now: () => now }, setTimeout, clearTimeout });
  const module = new vm.SourceTextModule(source, { context });
  await module.link(() => { throw Error("Unexpected import"); }); await module.evaluate();
  return { advance: ms => { now += ms; }, request: (target = "https://example.org/cover.png") =>
    module.namespace.default(new Request("https://aesthetic.computer/api/og-image?url=" + encodeURIComponent(target))) };
}

test("denied or missing preview images stay unavailable and briefly cache the upstream result", async () => {
  for (const status of [401, 403, 404, 410]) {
    let calls = 0, cancelled = 0;
    const f = await fixture(async () => { calls++; return {
      ok: false, status, body: { cancel: async () => { cancelled++; } },
    }; });
    const response = await f.request();
    assert.equal(response.status, 404);
    assert.deepEqual(await response.json(), { error: "Image unavailable", upstreamStatus: status });
    assert.equal(response.headers.get("Cache-Control"), "public, max-age=300");
    assert.equal(response.headers.get("Access-Control-Allow-Origin"), "*");
    f.advance(120000);
    assert.equal((await f.request()).headers.get("Cache-Control"), "public, max-age=180");
    assert.equal(calls, 1); assert.equal(cancelled, 1);
    f.advance(180000); await f.request(); assert.equal(calls, 2);
  }
});

test("upstream server errors remain uncached 502s and successful images retain their bytes", async () => {
  let calls = 0;
  const failed = await fixture(async () => { calls++; return new Response("unavailable", { status: 503 }); });
  assert.equal((await failed.request()).status, 502);
  assert.equal((await failed.request()).status, 502); assert.equal(calls, 2);
  const f = await fixture(async () => new Response(new Uint8Array([1, 2, 3]), { headers: { "Content-Type": "image/png" } }));
  const image = await f.request();
  assert.equal(image.status, 200); assert.equal(image.headers.get("Content-Type"), "image/png");
  assert.deepEqual(new Uint8Array(await image.arrayBuffer()), new Uint8Array([1, 2, 3]));
});
