// node --experimental-vm-modules --test system/tests/media-collection.test.mjs
import test from "node:test";
import assert from "node:assert/strict";
import vm from "node:vm";
import { readFile } from "node:fs/promises";

async function fixture({ userSub = "auth0|synthetic", failQuery = false } = {}) {
  const calls = { connects: 0, lookups: 0, labels: 0, disconnects: 0, queries: [] };
  const db = {
    collection(name) {
      return { find(query) {
        calls.queries.push({ name, query });
        return { async toArray() {
          if (failQuery) throw new Error("synthetic database error");
          return [{ slug: "work" }, { slug: "auth0|synthetic/chat/other" }];
        } };
      } };
    },
  };
  const mocks = {
    "../../backend/authorization.mjs": {
      userIDFromHandleOrEmail: async (handle) => {
        calls.lookups++;
        assert.equal(handle, "@synthetic");
        return userSub;
      },
      getHandleOrEmail: async (id) => {
        calls.labels++;
        assert.equal(id, userSub);
        return "@synthetic";
      },
    },
    "../../backend/http.mjs": { respond: (statusCode, body) => ({ statusCode, body }) },
    "../../backend/database.mjs": { connect: async () => {
      calls.connects++;
      return { db, disconnect: async () => { calls.disconnects++; } };
    } },
  };
  const context = vm.createContext({ console: { log() {} } });
  const module = new vm.SourceTextModule(
    await readFile(new URL("../netlify/functions/media-collection.js", import.meta.url), "utf8"),
    { context },
  );
  await module.link((name) => {
    assert.ok(mocks[name], `Unexpected import: ${name}`);
    return new vm.SyntheticModule(Object.keys(mocks[name]), function () {
      for (const [key, value] of Object.entries(mocks[name])) this.setExport(key, value);
    }, { context });
  });
  await module.evaluate();
  return { handler: module.namespace.handler, calls };
}
const request = (path) => ({
  httpMethod: "GET",
  queryStringParameters: path === undefined ? undefined : { for: path },
  headers: { host: "aesthetic.computer" },
});

test("missing and malformed collection paths return 400 without backend operations", async () => {
  const { handler, calls } = await fixture();
  for (const path of [undefined, null, "", " ", "undefined", "%", "%E0%A4%A", "@synthetic", "/painting", "@synthetic/", "@synthetic/painting/extra", ["@synthetic/painting"]]) {
    assert.equal((await handler(request(path))).statusCode, 400, String(path));
  }
  assert.equal((await handler({ ...request(), queryStringParameters: null })).statusCode, 400);
  assert.equal(calls.connects, 0);
  assert.equal(calls.lookups, 0);
  assert.equal(calls.labels, 0);
});

test("valid encoded collection paths preserve bare and storage-qualified URLs", async () => {
  const { handler, calls } = await fixture();
  const response = await handler(request("%40synthetic%2Fpainting"));
  assert.equal(response.statusCode, 200);
  assert.deepEqual(Array.from(response.body.files), [
    "https://aesthetic.computer/media/@synthetic/painting/work.png",
    "https://aesthetic.computer/media/@synthetic/auth0|synthetic/chat/other.png",
  ]);
  assert.equal(calls.queries[0].name, "paintings");
  assert.equal(calls.queries[0].query.user, "auth0|synthetic");
  assert.equal(calls.queries[0].query.nuked.$ne, true);
  assert.equal(calls.queries[0].query.status.$ne, "wip");
  assert.equal(calls.disconnects, 1);
});

test("unknown users return 404 without querying unowned media", async () => {
  const { handler, calls } = await fixture({ userSub: null });
  const response = await handler(request("@synthetic/painting"));
  assert.equal(response.statusCode, 404);
  assert.equal(response.body.files.length, 0);
  assert.equal(calls.labels, 0);
  assert.equal(calls.queries.length, 0);
  assert.equal(calls.disconnects, 1);
});

test("real database errors retain 500 and release the connection", async () => {
  const { handler, calls } = await fixture({ failQuery: true });
  assert.equal((await handler(request("@synthetic/painting"))).statusCode, 500);
  assert.equal(calls.disconnects, 1);
});

test("unsupported methods return 405 before validating or connecting", async () => {
  const { handler, calls } = await fixture();
  assert.equal((await handler({ httpMethod: "POST" })).statusCode, 405);
  assert.equal(calls.connects, 0);
});
