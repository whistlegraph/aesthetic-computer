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
