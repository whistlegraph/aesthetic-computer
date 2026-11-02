import test from "node:test";
import assert from "node:assert/strict";
import { halt } from "../public/aesthetic.computer/disks/prompt.mjs";
globalThis.self = { location: new URL("https://aesthetic.computer/prompt") };

// Exercise the actual prompt and parser, replacing only the host API and HTTP.
function fixture(t, { signedIn = true, failSave = false, streamed = false } = {}) {
  const requests = [], loads = [], notices = [];
  const source = "; giraffe\n(wipe blue)";
  t.mock.method(globalThis, "fetch", async (url, options) => {
    requests.push({ url, options });
    if (url === "/api/prompt-log") return Response.json({});
    if (url === "/api/store-kidlisp") {
      assert.equal(options.headers.Authorization, "Bearer token");
      assert.equal(JSON.parse(options.body).handled, true);
      return failSave ? Response.json({ error: "expired" }, { status: 401 })
        : Response.json({ code: "owned", handled: true, handle: "@alice" });
    }
    assert.match(url, /^\/api\/aesel-sketch\?/);
    const result = { code: "shared", source };
    return streamed ? new Response(`${source}\u0000${JSON.stringify(result)}`,
      { headers: { "Content-Type": "text/plain" } }) : Response.json(result);
  });
  const api = {
    handle: () => signedIn ? "@alice" : null, authorize: async () => "token", store: {},
    notice: text => notices.push(text), text: { capitalize: text => text },
    system: { prompt: { input: { text: "giraffe", snap() {} } } },
    send() {}, leaving() {}, needsPaint() {},
    load: async (...args) => {
      loads.push(args);
      return !!args[0].source || ["$owned", "$shared", "$existing"].includes(args[0].piece);
    },
  };
  return { api, requests, loads, notices, source };
}

test("typed KidLisp saves before opening the verified owner's code", async t => {
  const f = fixture(t);
  assert.deepEqual(await halt(f.api, "(wipe blue)"), { left: true });
  assert.equal(f.requests[0].url, "/api/store-kidlisp");
  assert.equal(f.loads[0][0].piece, "$owned");
  assert.equal(f.requests.some(r => r.url.includes("aesel-sketch")), false);
});

for (const streamed of [false, true]) test(`generated KidLisp joins the handled save (${streamed ? "stream" : "cache"})`, async t => {
  const f = fixture(t, { streamed });
  assert.deepEqual(await halt(f.api, "giraffe"), { left: true });
  assert.deepEqual(f.requests.map(r => r.url), ["/api/aesel-sketch?word=giraffe&stream=1", "/api/store-kidlisp"]);
  assert.equal(JSON.parse(f.requests[1].options.body).source, f.source);
  assert.equal(f.loads.at(-1)[0].piece, "$owned");
  assert.equal(f.loads.at(-1)[0].search, "nosource");
});

test("anonymous entries run inline or use the shared sketch", async t => {
  const f = fixture(t, { signedIn: false });
  assert.deepEqual(await halt(f.api, "(wipe blue)"), { left: true });
  assert.equal(f.loads.at(-1)[0].source, "(wipe blue)");
  assert.equal(f.loads.at(-1)[5], true, "force inline KidLisp");
  assert.deepEqual(await halt(f.api, "giraffe"), { left: true });
  assert.equal(f.loads.at(-1)[0].piece, "$shared");
  assert.equal(f.requests.some(r => r.url === "/api/store-kidlisp"), false);
});

test("a failed handled save still runs typed source and shared sketches", async t => {
  const f = fixture(t, { failSave: true });
  assert.deepEqual(await halt(f.api, "(wipe blue)"), { left: true });
  assert.equal(f.loads.at(-1)[0].source, "(wipe blue)");
  assert.deepEqual(await halt(f.api, "giraffe"), { left: true });
  assert.equal(f.loads.at(-1)[0].piece, "$shared");
  assert.equal(f.notices.filter(n => n.includes("Could not save")).length, 2);
});

test("opening someone else's saved code never requests a handled copy", async t => {
  const f = fixture(t);
  assert.deepEqual(await halt(f.api, "$existing"), { left: true });
  assert.equal(f.loads.at(-1)[0].piece, "$existing");
  assert.deepEqual(f.requests.map(r => r.url), ["/api/prompt-log"]);
});
