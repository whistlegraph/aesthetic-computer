import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { gzipSync } from "node:zlib";
import { edgeScope, edgeQuery, edgeRows, collectCloudflare, correlateEdge, collectEdge, validateAccess } from "../edge.mjs";
import { collectAccessLogs } from "../lith-access.mjs";
import { validateSignals } from "../signals.mjs";
import { opportunities } from "../queue.mjs";

const window = { start: "2026-10-08T12:15:00.000Z", end: "2026-10-08T14:15:00.000Z" };
const config = { cloudflare: { zones: ["aesthetic.computer"] } };
const scope = edgeScope(config, window), zoneId = "a".repeat(32);
const auth = () => ({ email: "fixture@example.invalid", apiKey: "private-key" });
const response = value => new Response(JSON.stringify(value));
const group = (edge = 522, origin = 0, count = 8) => ({ count, dimensions: {
  datetimeHour: "2026-10-08T13:00:00Z", clientRequestHTTPHost: "aesthetic.computer", edgeResponseStatus: edge, originResponseStatus: origin,
}, avg: { sampleInterval: 4 } });
const result = rows => ({ data: { viewer: { zones: [{ httpRequestsAdaptiveGroups: rows }] } } });
const coverage = [{ zone: "aesthetic.computer", status: "available", groups: 1, truncated: false, estimated: true }];
const access = (errors = 3) => ({ status: "available", rows: [{ host: "aesthetic.computer", hour: "2026-10-08T13:00:00.000Z", requests: 30, errors5xx: errors }], scanned: 30, truncated: true });

test("edge scope rejects client zones and queries only allowed hosts in the exact window", () => {
  assert.throws(() => edgeScope({ cloudflare: { zones: ["false.work"] } }, window), /scope/);
  assert.throws(() => edgeScope({ cloudflare: { zones: ["unrelated.invalid"] } }, window), /scope/);
  const query = edgeQuery(zoneId, ["aesthetic.computer"], scope);
  assert.ok(query.includes(window.start) && query.includes(window.end));
  assert.match(query, /requestSource: "eyeball"/);
  assert.doesNotMatch(query, /clientIP|clientRequestPath|rayName|userAgent|headers/);
  assert.throws(() => edgeQuery(zoneId, ["false.work"], scope), /binding/);
});

test("Cloudflare estimates are not multiplied again and origin status zero is preserved", () => {
  const out = edgeRows(result([group(), group(500, 500, 3), group(200, 503, 4)]), scope.hosts, scope);
  assert.equal(out.rows[0].requests, 15);
  assert.equal(out.rows[0].errors5xx, 11);
  assert.equal(out.rows[0].origin5xx, 7);
  assert.equal(out.rows[0].withoutOrigin5xx, 8);
  assert.equal(out.rows[0].sampled, true);
  const unknown = group(); unknown.dimensions.clientRequestHTTPHost = "private.example";
  assert.throws(() => edgeRows(result([unknown]), scope.hosts, scope), /aggregate/);
  const future = group(); future.dimensions.datetimeHour = "2026-10-08T15:00:00Z";
  assert.throws(() => edgeRows(result([future]), scope.hosts, scope), /aggregate/);
  assert.throws(() => edgeRows({ ...result([]), errors: [{ message: "partial result" }] }, scope.hosts, scope), /unavailable/);
});

test("zone binding, read-only requests, truncation and credential redaction hold", async () => {
  const calls = [];
  const out = await collectCloudflare({ ...scope, limit: 1 }, { auth, env: {}, fetchImpl: async (url, options) => {
    calls.push({ url, options });
    assert.equal(options.redirect, "error");
    if (url.endsWith("/zones?name=aesthetic.computer")) return response({ success: true, result: [{ id: zoneId, name: "aesthetic.computer", status: "active" }] });
    assert.equal(url, "https://api.cloudflare.com/client/v4/graphql");
    assert.equal(options.method, "POST");
    assert.doesNotMatch(options.body, /private-key|fixture@example|mutation/);
    return response(result([group(), group(200, 200, 20)]));
  } });
  assert.equal(calls.length, 2);
  assert.equal(out.coverage[0].truncated, true);
  assert.doesNotMatch(JSON.stringify(out), /private-key|fixture@example/);
  const mismatch = await collectCloudflare(scope, { auth, env: {}, fetchImpl: async () => response({ success: true, result: [{ id: zoneId, name: "false.work", status: "active" }] }) });
  assert.equal(mismatch.coverage[0].status, "unavailable");
  assert.deepEqual(mismatch.rows, []);
});

test("one inaccessible zone cannot erase another zone's evidence", async () => {
  const two = edgeScope({ cloudflare: { zones: ["aesthetic.computer", "laklok.com"] } }, window);
  const out = await collectCloudflare(two, { auth, env: {}, fetchImpl: async url => {
    if (url.endsWith("name=laklok.com")) throw new Error("private-key raw error");
    if (url.includes("/zones?")) return response({ success: true, result: [{ id: zoneId, name: "aesthetic.computer", status: "active" }] });
    return response(result([group()]));
  } });
  assert.deepEqual(out.coverage.map(row => row.status), ["available", "unavailable"]);
  assert.equal(out.rows.length, 1);
  assert.doesNotMatch(JSON.stringify(out), /private-key|raw error/);
});

test("correlation requires the same host and UTC hour and never auto-patches a timing overlap", () => {
  const cf = { ...edgeRows(result([group(500, 500)]), scope.hosts, scope), coverage };
  const joined = correlateEdge(scope, cf, access());
  assert.equal(joined.correlation.buckets[0].assessment, "edge-and-lith-errors");
  assert.equal(joined.correlation.lith.truncated, true);
  validateSignals(joined.signals, []);
  assert.equal(opportunities({ runs: [], transitions: [], minimum: 3 }, joined.signals).length, 0);
  const other = access(); other.rows[0].hour = "2026-10-08T12:00:00.000Z";
  assert.equal(correlateEdge(scope, cf, other).correlation.buckets.some(row => row.assessment === "edge-and-lith-errors"), false);
  const anotherHost = access(); anotherHost.rows[0].host = "www.aesthetic.computer";
  assert.equal(correlateEdge(scope, cf, anotherHost).correlation.buckets.some(row => row.assessment === "edge-and-lith-errors"), false);
});

test("missing sources remain null rather than healthy zero counters", async () => {
  const cf = { ...edgeRows(result([group()]), scope.hosts, scope), coverage };
  const missingLith = correlateEdge(scope, cf, { status: "unavailable", reason: "fixture-unavailable", rows: [] });
  assert.equal(missingLith.correlation.buckets[0].lith, null);
  assert.equal(missingLith.signals.metrics.some(row => row.source === "lith-access"), false);
  const missingCf = await collectEdge(config, window, { auth: () => ({}), env: {}, readAccess: async () => access(), fetchImpl: () => { throw Error("Unexpected request"); } });
  assert.equal(missingCf.correlation.buckets[0].edge, null);
  assert.equal(missingCf.signals.metrics.some(row => row.source === "cloudflare"), false);
  assert.equal(missingCf.signals.leads[0].source, "lith-access");
});

test("Caddy rotation is read locally, filtered before export and bounded to exact timestamps", async t => {
  const dir = await mkdtemp(join(tmpdir(), "aespatcher-access-")); t.after(() => rm(dir, { recursive: true, force: true }));
  const row = (at, host, status) => JSON.stringify({ ts: Date.parse(at) / 1000, status,
    request: { host, remote_ip: "192.0.2.1", uri: "/private?token=hidden", headers: { Authorization: ["secret"] } } });
  await writeFile(join(dir, "access-2026-10-08T12-30-00.000-size.log.gz"), gzipSync([
    row("2026-10-08T12:00:00Z", "aesthetic.computer", 500), row("2026-10-08T12:20:00Z", "aesthetic.computer", 200), "",
  ].join("\n")));
  // Ensure the active file is newer, as on Lith.
  await writeFile(join(dir, "access.log"), [row("2026-10-08T13:00:00Z", "aesthetic.computer:443", 503),
    row("2026-10-08T13:30:00Z", "false.work", 500), row(window.end, "aesthetic.computer", 500), ""].join("\n"));
  const out = validateAccess(await collectAccessLogs(scope, dir), scope);
  assert.equal(out.scanned, 2);
  assert.equal(out.rows.reduce((n, r) => n + r.errors5xx, 0), 1);
  assert.equal(out.truncated, false);
  assert.doesNotMatch(JSON.stringify(out), /false.work|192.0.2.1|private|hidden|Authorization|secret/);
});

test("retention gaps and malformed access rows make coverage incomplete", async t => {
  const dir = await mkdtemp(join(tmpdir(), "aespatcher-access-")); t.after(() => rm(dir, { recursive: true, force: true }));
  await writeFile(join(dir, "access.log"), JSON.stringify({ ts: Date.parse("2026-10-08T13:00:00Z") / 1000, status: 200, request: { host: "aesthetic.computer" } }) + "\nmalformed\n");
  const out = await collectAccessLogs(scope, dir);
  assert.equal(out.truncated, true);
  assert.equal(out.scanned, 1);
  out.rows[0].ip = "private";
  assert.throws(() => validateAccess(out, scope), /aggregates/);
});
