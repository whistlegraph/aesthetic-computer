import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, writeFile, rm } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { releaseStatus, reduceAudit, declaredRuntimes, collectSecurity, securitySummary, validateRuntime, validateIndex, securitySettings } from "../security.mjs";
import { validateSignals } from "../signals.mjs";
import { opportunities, admit } from "../queue.mjs";
import { defaults, validateConfig } from "../config.mjs";
import { privateDir } from "../io.mjs";

const now = new Date("2026-10-23T12:00:00Z");
const index = [
  { version: "v26.13.0", date: "2026-10-22", security: false }, { version: "v26.12.1", date: "2026-10-21", security: true },
  { version: "v24.22.1", date: "2026-10-20", security: true }, { version: "v24.22.0", date: "2026-10-14", security: false },
  { version: "v24.21.0", date: "2026-09-07", security: false }, { version: "v24.18.1", date: "2026-07-28", security: true },
  { version: "v20.20.2", date: "2026-03-24", security: false },
];
const schedule = { v20: { end: "2026-04-30" }, v24: { end: "2028-04-30" }, v26: { end: "2029-04-30" } };
const fetchStub = async url => ({ ok: true, body: [Buffer.from(JSON.stringify(url.includes("schedule") ? schedule : index))] });

test("release status judges a running version against its own line", () => {
  const behind = releaseStatus("v24.18.1", index, schedule, now);
  assert.equal(behind.status, "security-release-available");
  assert.equal(behind.latestSecurity.version, "v24.22.1");
  assert.deepEqual([behind.behind, behind.securityBehind, behind.latest], [3, 1, "v24.22.1"]);
  assert.equal(releaseStatus("v24.22.1", index, schedule, now).status, "current");
  assert.equal(releaseStatus("26.12.1", index, schedule, now).status, "release-available");
  assert.equal(releaseStatus("v20.20.2", index, schedule, now).status, "end-of-life");
  assert.equal(releaseStatus("v99.0.0", index, schedule, now).status, "unknown-line");
  assert.throws(() => releaseStatus("latest", index), /Invalid Node version/);
  assert.throws(() => validateIndex([{ version: "v24.22.1", date: "soon" }]), /Rejected release index/);
  assert.throws(() => validateRuntime({ node: "v24.18.1", shell: "/bin/sh" }), /Rejected runtime facts/);
});

test("audit reduction keeps counts and package names from npm and pnpm reports", () => {
  const npm = JSON.stringify({ metadata: { vulnerabilities: { info: 0, low: 0, moderate: 2, high: 2, critical: 0 } }, vulnerabilities: {
    ws: { name: "ws", severity: "high", isDirect: true, fixAvailable: true, via: [{ title: "memory", url: "https://example.invalid" }] },
    lodash: { name: "lodash", severity: "high", isDirect: false, fixAvailable: { name: "x" } }, ajv: { name: "ajv", severity: "moderate" } } });
  const reduced = reduceAudit(npm, "npm");
  assert.deepEqual(reduced.counts, { critical: 0, high: 2, moderate: 2, low: 0 });
  assert.deepEqual(reduced.advisories.map(a => [a.name, a.direct]), [["lodash", false], ["ws", true]]);
  assert.ok(!JSON.stringify(reduced).includes("example.invalid"));
  const pnpm = JSON.stringify({ advisories: { 1: { module_name: "fastify", severity: "critical" } }, metadata: { vulnerabilities: { info: 0, low: 0, moderate: 0, high: 0, critical: 1 } } });
  assert.equal(reduceAudit(pnpm, "pnpm").advisories[0].name, "fastify");
  assert.throws(() => reduceAudit("{}", "npm"), /Unexpected audit output/);
});

test("declared runtimes read exact pins and the collector turns drift into manual-review leads", async t => {
  const repo = await mkdtemp(join(tmpdir(), "aespatcher-security-"));
  t.after(() => rm(repo, { recursive: true, force: true }));
  await mkdir(join(repo, "session-server"));
  await writeFile(join(repo, ".node-version"), "24.18.1\n");
  await writeFile(join(repo, "session-server/Dockerfile"), "FROM node:20.20.2\nCMD [\"node\", \"session.mjs\"]\n");
  await writeFile(join(repo, "loose"), "FROM node:24\n");
  const declared = await declaredRuntimes(repo, [".node-version", "session-server/Dockerfile", "loose", "missing"]);
  assert.deepEqual(declared.map(r => [r.status, r.node ?? null]), [["available", "v24.18.1"], ["available", "v20.20.2"], ["unavailable", null], ["unavailable", null]]);

  const config = { ...defaults(repo), security: { enabled: true, packages: [], runtimes: [".node-version", "session-server/Dockerfile"] } };
  const { signals, detail } = await collectSecurity(repo, config, { runtime: { node: "v24.18.1", brotli: "1.1.0" }, remote: false, network: false, fetchImpl: fetchStub, now });
  validateSignals(signals, []);
  assert.deepEqual(signals.leads.map(l => l.kind), ["runtime-security-release", "runtime-security-release", "runtime-end-of-life"]);
  assert.ok(signals.leads.every(l => l.route === null));
  assert.match(signals.leads[0].summary, /Lith runs Node v24.18.1; v24.22.1 \(2026-10-20\)/);
  assert.deepEqual(signals.coverage, [{ source: "runtime", status: "available", scanned: 3, truncated: false }, { source: "dependencies", status: "unavailable", scanned: 0, reason: "No package audit completed; no clean-dependency claim." }, { source: "network", status: "disabled" }, { source: "mail", status: "disabled" }, { source: "api", status: "disabled" }, { source: "servers", status: "disabled" }]);
  assert.equal(signals.metrics.find(m => m.target === "lith" && m.metric === "security-releases-behind").count, 1);
  const summary = securitySummary(detail, signals);
  assert.equal(summary.required, true);
  assert.equal(summary.hosts[0].latestSecurity, "v24.22.1");
  assert.equal(summary.declared[1].running, "v20.20.2");

  // Drift never becomes an automatic piece worker task; it stays in the manual-review count.
  const report = { format: "aespatcher.signals.v1", start: "2026-10-22T12:00:00.000Z", end: now.toISOString(), minimum: 3, runs: [], transitions: [], visits: [], unavailable: [], truncated: [] };
  assert.equal(opportunities(report, signals).length, 0);
  const home = await privateDir(join(repo, ".state"));
  const admitted = await admit(home, { report, signals }, config.bounds, now);
  assert.deepEqual([admitted.admitted.length, admitted.manualReview], [0, 3]);

  const current = await collectSecurity(repo, config, { runtime: { node: "v24.22.1" }, remote: false, network: false, fetchImpl: fetchStub, now });
  assert.equal(securitySummary(current.detail, current.signals).hosts[0].status, "current");
  const offline = await collectSecurity(repo, config, { runtime: null, remote: false, network: false, fetchImpl: async () => ({ ok: false }), now });
  assert.equal(offline.signals.coverage[0].status, "unavailable");
  assert.equal(offline.signals.leads.length, 0);
});

test("security settings are validated with the runner config and default on", () => {
  assert.equal(securitySettings({}).enabled, true);
  assert.doesNotThrow(() => validateConfig(defaults("/repo")));
  assert.throws(() => validateConfig({ ...defaults("/repo"), security: { enabled: true, packages: ["../lith"], runtimes: [] } }), /Invalid security settings/);
  assert.throws(() => validateConfig({ ...defaults("/repo"), security: { enabled: true, packages: [], runtimes: ["/etc/passwd"] } }), /Invalid repository file/);
});
