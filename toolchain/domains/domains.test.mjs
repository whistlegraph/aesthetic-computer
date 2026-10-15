import test from "node:test";
import assert from "node:assert/strict";
import { once } from "node:events";
import { createHandler } from "./domains-mcp.mjs";
import * as cf from "./cloudflare.mjs";
import * as pb from "./porkbun.mjs";
import { serveHttp } from "../mcp/http-front.mjs";

function fixture() {
  const writes = [];
  const record = { id: "record-1", name: "example.com", type: "A", content: "192.0.2.1", ttl: 1, proxied: true };
  const cloudflare = {
    zone: async () => ({ id: "zone-1", status: "active", name_servers: ["first.ns.cloudflare.com", "second.ns.cloudflare.com"] }),
    records: async () => [record],
    updateRecord: async (...args) => { writes.push(args); return { id: "record-1", ...args[2] }; },
    createRecord: async (...args) => { writes.push(args); return { id: "new", name: args[1], content: args[2], ...args[3] }; },
  };
  const handler = createHandler({ cloudflare, status: () => ({ services: [{ service: "cloudflare", state: "ready" }] }),
    delegation: async () => ["parking.example.net"], registry: async () => ({ registrar: "External Registrar" }) });
  const call = (name, args = {}) => handler({ jsonrpc: "2.0", id: 1, method: "tools/call", params: { name, arguments: args } });
  return { writes, record, cloudflare, handler, call };
}
const decode = response => JSON.parse(response.result.content[0].text);
const change = { domain: "example.com", name: "@", type: "A", content: "192.0.2.2" };
function mockEnv(t, values) {
  const before = Object.fromEntries(Object.keys(values).map(key => [key, process.env[key]]));
  Object.assign(process.env, values);
  t.after(() => { for (const [key, value] of Object.entries(before)) {
    if (value === undefined) delete process.env[key]; else process.env[key] = value;
  } });
}

test("active Cloudflare zone still reports parking delegation mismatch", async () => {
  const f = fixture();
  const result = decode(await f.call("domains_inspect", { domain: "example.com" }));
  assert.equal(result.cloudflare.status, "active");
  assert.equal(result.delegationMatches, false);
  assert.equal(result.registry.registrar, "External Registrar");
  assert.deepEqual(f.writes, []);
});

test("DNS preview is read-only; applying preserves existing proxy and TTL", async () => {
  const f = fixture();
  const preview = decode(await f.call("domains_dns_set", change));
  assert.equal(preview.applied, false);
  assert.equal(preview.after.proxied, true);
  assert.equal(preview.after.ttl, 1);
  assert.deepEqual(f.writes, []);
  const applied = decode(await f.call("domains_dns_set", { ...change, apply: true }));
  assert.equal(applied.applied, true);
  assert.deepEqual(f.writes[0], ["zone-1", "record-1", preview.after]);
});

test("unchanged records do not produce a write; new records use explicit DNS-only defaults", async () => {
  const f = fixture();
  assert.equal(decode(await f.call("domains_dns_set", { ...change, content: "192.0.2.1", apply: true })).action, "unchanged");
  assert.equal(f.writes.length, 0);
  const result = decode(await f.call("domains_dns_set", { ...change, name: "new.example.com", apply: true }));
  assert.equal(result.action, "create");
  assert.equal(result.record.proxied, false);
  assert.equal(result.record.ttl, 300);
});

test("rejects malformed/out-of-zone writes and arguments before mutation", async () => {
  const f = fixture();
  for (const overrides of [{ name: "evil.example.net" }, { domain: "example.com/other" },
    { content: "not-ip" }, { apply: "true" }, { ttl: -1 }, { type: "TXT" },
    { recordId: "another-record" }, { endpoint: "https://other.test" }]) {
    assert.equal((await f.call("domains_dns_set", { ...change, ...overrides })).result.isError, true);
  }
  assert.equal((await f.call("constructor")).result.isError, true);
  assert.equal(f.writes.length, 0);
});

test("multiple records require an explicit matching id; type changes cannot silently overwrite", async () => {
  const f = fixture();
  f.cloudflare.records = async () => [f.record, { ...f.record, id: "record-2" }];
  assert.equal((await f.call("domains_dns_set", { ...change, apply: true })).result.isError, true);
  assert.equal(decode(await f.call("domains_dns_set", { ...change, recordId: "record-2", apply: true })).applied, true);
  assert.equal(f.writes[0][1], "record-2");
  assert.equal((await f.call("domains_dns_set", { ...change, type: "CNAME", content: "target.example.org", apply: true })).result.isError, true);
  assert.equal(f.writes.length, 1);
});

test("HTTP handshake and tool discovery work; browser origins and rebinding hosts are refused", async t => {
  const { handler } = fixture();
  const server = serveHttp({ handleMessage: handler, port: 0, banner: "test domains" });
  t.after(() => { server.closeAllConnections(); server.close(); });
  await once(server, "listening");
  const endpoint = `http://127.0.0.1:${server.address().port}/mcp`;
  const send = async (method, headers = {}) => (await fetch(endpoint, { method: "POST", headers: { "content-type": "application/json", ...headers },
    body: JSON.stringify({ jsonrpc: "2.0", id: 1, method }) })).json();
  assert.equal((await send("initialize")).result.serverInfo.name, "ac-domains");
  assert.ok((await send("tools/list")).result.tools.some(tool => tool.name === "vault_status"));
  assert.ok((await send("tools/list", { Origin: "https://hostile.test" })).error);
  assert.ok((await handler({ id: 1, method: "tools/list" }, { headers: { host: "hostile.test" } })).error);
  const notification = await fetch(endpoint, { method: "POST", body: JSON.stringify({ jsonrpc: "2.0", method: "notifications/initialized" }) });
  assert.equal(notification.status, 202);
});

test("Cloudflare client sends bearer auth and follows pagination", async t => {
  mockEnv(t, { CLOUDFLARE_API_TOKEN: "test-scoped-token" });
  const requests = [];
  t.mock.method(globalThis, "fetch", async (url, options) => {
    requests.push({ url, options });
    return new Response(JSON.stringify({ success: true, result: [{ id: requests.length }], result_info: { total_pages: 2 } }));
  });
  assert.equal((await cf.records("zone")).length, 2);
  assert.equal(requests[0].options.headers.Authorization, "Bearer test-scoped-token");
  assert.equal(requests[0].options.headers["X-Auth-Key"], undefined);
  assert.match(requests[1].url, /page=2$/);
});

test("Cloudflare mutations never retry ambiguous transport failures and redact credentials", async t => {
  mockEnv(t, { CLOUDFLARE_API_TOKEN: "test-secret-token" });
  let calls = 0;
  t.mock.method(globalThis, "fetch", async () => { calls++; throw new Error("lost response test-secret-token"); });
  await assert.rejects(cf.createRecord("zone", "example.com", "192.0.2.1"), error => {
    assert.doesNotMatch(error.message, /test-secret-token/);
    assert.match(error.message, /\[redacted\]/);
    return true;
  });
  assert.equal(calls, 1);
});

test("provider error echoes never expose Cloudflare or Porkbun credentials", async t => {
  mockEnv(t, { CLOUDFLARE_API_TOKEN: "test-secret-token", PORKBUN_API_KEY: "test-pb-key", PORKBUN_SECRET_API_KEY: "test-pb-secret" });
  t.mock.method(globalThis, "fetch", async () => new Response(JSON.stringify({ success: false, errors: [{ code: 1, message: "test-secret-token" }], status: "ERROR", message: "test-pb-key test-pb-secret" })));
  await assert.rejects(cf.zone("example.com"), error => !error.message.includes("test-secret-token"));
  await assert.rejects(pb.getNs("example.com"), error => !error.message.includes("test-pb-"));
});
