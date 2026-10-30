// node --experimental-vm-modules --test system/tests/production-endpoints.test.mjs
import test from "node:test";
import assert from "node:assert/strict";
import vm from "node:vm";
import { readFile, access } from "node:fs/promises";
import path from "node:path";
import * as url from "node:url";
import * as parser from "../public/aesthetic.computer/lib/parse.mjs";
import * as http from "../backend/http.mjs";
import * as helpers from "../public/aesthetic.computer/lib/helpers.mjs";

async function load(name, { env = {}, mocks = {}, ...options } = {}) {
  const filename = new URL(`../netlify/functions/${name}`, import.meta.url);
  const context = vm.createContext({
    process: { env, cwd: () => url.fileURLToPath(new URL("../", import.meta.url)) },
    console: { log() {}, error() {} }, URL, URLSearchParams, Buffer,
  });
  const module = new vm.SourceTextModule(await readFile(filename, "utf8"), {
    context, identifier: filename.href,
    initializeImportMeta(meta) { meta.url = filename.href; }, ...options,
  });
  await module.link((name) => {
    assert.ok(mocks[name], `unexpected import: ${name}`);
    const exports = mocks[name];
    return new vm.SyntheticModule(Object.keys(exports), function () {
      for (const [key, value] of Object.entries(exports)) this.setExport(key, value);
    }, { context });
  });
  await module.evaluate();
  return module.namespace.handler;
}

test("MCP accepts initialized notifications without a JSON-RPC reply and keeps request responses", async () => {
  const handler = await load("mcp-remote.mjs", { mocks: { "../../backend/http.mjs": http } });
  const post = message => handler({ httpMethod: "POST", path: "/mcp", headers: {}, body: JSON.stringify(message) });
  const initialized = await post({ jsonrpc: "2.0", id: 0, method: "initialize", params: {} });
  assert.equal(initialized.statusCode, 200);
  assert.equal(JSON.parse(initialized.body).id, 0);
  for (const params of [undefined, {}, { _meta: { fixture: true } }]) {
    const response = await post({ jsonrpc: "2.0", method: "notifications/initialized", params });
    assert.equal(response.statusCode, 202);
    assert.equal(response.body, "");
    assert.equal(response.headers["Access-Control-Allow-Origin"], "*");
  }
  const tools = await post({ jsonrpc: "2.0", id: "tools", method: "tools/list" });
  assert.equal(tools.statusCode, 200);
  assert.equal(JSON.parse(tools.body).id, "tools");
  assert.ok(JSON.parse(tools.body).result.tools.length > 0);
});

test("MCP does not accept malformed initialized notifications", async () => {
  const handler = await load("mcp-remote.mjs", { mocks: { "../../backend/http.mjs": http } });
  const valid = { jsonrpc: "2.0", method: "notifications/initialized" };
  for (const patch of [{ jsonrpc: "1.0" }, { id: 0 }, { id: null }, { params: null }, { params: [] }, { params: "bad" }]) {
    const response = await handler({ httpMethod: "POST", headers: {}, body: JSON.stringify({ ...valid, ...patch }) });
    assert.equal(response.statusCode, 400);
  }
});

test("mood lists return a successful empty collection for handles without moods", async () => {
  let records = [], failure, disconnected = 0;
  const database = { disconnect: async () => { disconnected++; } };
  const unused = () => { throw Error("Unexpected dependency call"); };
  const handler = await load("mood.mjs", { mocks: {
    "../../backend/authorization.mjs": { authorize: unused, userIDFromHandleOrEmail: unused, getHandleOrEmail: unused },
    "../../backend/database.mjs": {
      connect: async () => database, moodFor: unused, getMoodByRkey: unused,
      allMoods: async (db, handle) => {
        assert.equal(db, database); assert.equal(handle, "@fixture");
        if (failure) throw failure;
        return records;
      },
    },
    "../../backend/http.mjs": http,
    "../../backend/mood-atproto.mjs": { createMoodOnAtproto: unused },
    "../../backend/bluesky-mirror.mjs": { shouldMirror: unused, postMoodToBluesky: unused },
    "../../backend/bluesky-engagement.mjs": { fetchBlueskyEngagement: unused },
    "../../../shared/push.mjs": { broadcastToTopic: unused },
    "../../backend/shell.mjs": { shell: {} },
    "../../backend/profile-stream.mjs": { publishProfileEvent: unused },
    "../../backend/filter.mjs": { filter: unused },
  } });
  const event = { httpMethod: "GET", path: "/api/mood/all", queryStringParameters: { for: "@fixture" } };
  const empty = await handler(event);
  assert.equal(empty.statusCode, 200);
  assert.deepEqual(JSON.parse(empty.body), { moods: [] });
  records = [{ handle: "@fixture", mood: "hello", when: "2026-10-08T12:00:00Z" }];
  const populated = await handler(event);
  assert.equal(populated.statusCode, 200);
  assert.deepEqual(JSON.parse(populated.body), { moods: records });
  assert.equal(disconnected, 2);
  failure = Error("Database unavailable");
  await assert.rejects(handler(event), /Database unavailable/);
});

for (const faster of ["lookup", "gallery"]) test(`painting metadata ${faster} completion preserves another in-flight lookup`, async () => {
  let closed = false, entered, release;
  const started = new Promise(resolve => { entered = resolve; });
  const gate = new Promise(resolve => { release = resolve; });
  const db = { collection: () => ({
    async findOne(query) {
      if (query.$or?.[0]?.slug === "slow") { entered(); await gate; }
      if (closed) throw Error("Cannot use a session that has ended");
      return { slug: "fixture", code: "fixture" };
    },
    find() { return { sort() { return this; }, skip() { return this; }, limit() { return this; }, async toArray() { return []; } }; },
  }) };
  class MongoClient {
    async connect() { closed = false; }
    db() { return db; }
    async close() { closed = true; }
  }
  const handler = await load("painting-metadata.mjs", { mocks: {
    mongodb: { MongoClient },
    "../../backend/database.mjs": { connect: async () => ({ db }) },
    "../../backend/painting-slug-query.mjs": { userPaintingSlugQuery: slug => ({ slug }) },
  } });
  const pending = handler({ httpMethod: "GET", queryStringParameters: { slug: "slow" } });
  await started;
  const first = await handler({ httpMethod: "GET", queryStringParameters: faster === "lookup" ? { slug: "fast" } : {} });
  release();
  assert.equal(first.statusCode, 200);
  assert.equal((await pending).statusCode, 200);
});

test("gives constructs Stripe and returns its feed, then uses the cache", async () => {
  let reads = 0;
  class Stripe {
    constructor(key) { assert.equal(key, "synthetic"); }
    checkout = { sessions: { list: async () => {
      reads++;
      return { data: [{ id: "give-1", metadata: { type: "gift" }, currency: "usd", amount_total: 500, created: 1700000000 }] };
    } } };
    invoices = { list: async () => ({ data: [] }) };
    subscriptions = { list: async () => ({ data: [] }) };
  }
  const handler = await load("gives.mjs", {
    env: { CONTEXT: "production", STRIPE_API_PRIV_KEY: "synthetic" },
    mocks: { stripe: { default: Stripe }, "../../backend/http.mjs": http },
  });
  for (let i = 0; i < 2; i++) {
    const result = await handler({ httpMethod: "GET" });
    assert.equal(result.statusCode, 200);
    assert.equal(JSON.parse(result.body).gives[0].amount, 5);
  }
  assert.equal(reads, 1);
});

test("production discovery uses the live monolith without an external spawn", async () => {
  const handler = await load("session.js");
  for (const queryStringParameters of [undefined, {}, { service: "monolith" }, { service: "undefined" }]) {
    const result = await handler({ path: "/api/session", headers: {}, queryStringParameters });
    assert.equal(result.statusCode, 200);
    assert.deepEqual(JSON.parse(result.body), {
      url: "https://session-server.aesthetic.computer", udp: "https://udp.aesthetic.computer", state: "Ready",
    });
  }
});

test("local discovery and explicit production override retain their routes", async () => {
  const handler = await load("session.js", { env: { NETLIFY_DEV: "true" } });
  const event = { headers: { host: "localhost:8888" }, queryStringParameters: {} };
  assert.equal(JSON.parse((await handler(event)).body).url, "https://localhost:8889");
  event.queryStringParameters.forceProduction = "1";
  assert.equal(JSON.parse((await handler(event)).body).url, "https://session-server.aesthetic.computer");
});

test("landing metadata imports resolve with Lith's system working directory", async () => {
  let rewritten, imported = false;
  const handler = await load("index.mjs", {
    env: { CONTEXT: "production" },
    mocks: {
      https: { default: {} }, path: { default: path }, url,
      fs: { promises: {
        readFile: async () => 'import { isMobile } from "../lib/platform.mjs";\nfunction meta() { return { title: "Resolved metadata" }; }',
        writeFile: async (_path, source) => { rewritten = source; },
        unlink: async () => {}, access: async () => {},
      } },
      he: { default: { encode: (s) => s } },
      "../../public/aesthetic.computer/lib/num.mjs": {},
      "../../public/aesthetic.computer/lib/parse.mjs": parser,
      "../../backend/http.mjs": http,
      "../../backend/authorization.mjs": { handleFromPermahandle() {} },
      "../../backend/database.mjs": { connect: async () => ({ db: {}, disconnect: async () => {} }) },
      "../../backend/piece-hits.mjs": { recordPieceHit: async () => {}, looksAutomated: () => true },
      "../../backend/boot-preloads.mjs": {
        bootPreloads: async () => [], workerBundleFilename: async () => null, builtinPieceUrl: async () => null,
      },
      "../../public/aesthetic.computer/lib/helpers.mjs": helpers,
      os: { networkInterfaces: () => ({}) },
    },
    importModuleDynamically: async (specifier) => {
      const target = rewritten.match(/from "([^"]+)"/)[1];
      await access(url.fileURLToPath(new URL(target, specifier)));
      imported = true;
      const piece = new vm.SyntheticModule(["meta"], function () {
        this.setExport("meta", () => ({ title: "Resolved metadata", desc: "Test" }));
      });
      await piece.link(() => {});
      await piece.evaluate();
      return piece;
    },
  });
  const result = await handler({ path: "/prompt", headers: { host: "aesthetic.computer" }, queryStringParameters: {} });
  assert.equal(imported, true, "the piece's library import must resolve");
  assert.equal(result.statusCode, 200);
  assert.match(result.body, /Resolved metadata/);
});
