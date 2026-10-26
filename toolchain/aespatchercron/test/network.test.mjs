import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, writeFile, rm } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { directoryHosts, networkSettings, serverSettings, networkFindings, serverFindings, describe, validateServerFacts, httpProbe, plainProbe, pathProbe, dnsProbe, auditServers, defaultNetwork } from "../network.mjs";
import { collectSecurity, securitySummary } from "../security.mjs";
import { validateSignals } from "../signals.mjs";
import { defaults } from "../config.mjs";

const now = new Date("2026-10-26T12:00:00Z");
const ok = (host, over = {}) => ({ host, tls: { status: "available", protocol: "TLSv1.3", authorized: true, error: null, daysLeft: 60, issuer: "Let's Encrypt" },
  http: { status: "available", code: 200, html: true, hstsMaxAge: 31536000, csp: true, frameProtection: true, noSniff: true, referrerPolicy: true, serverVersion: false, poweredBy: false },
  plain: { status: "available", code: 308, redirectsToTls: true, served: false }, paths: { status: "available", exposed: [], unavailable: [] },
  dns: { status: "available", resolved: true, cname: null, dangling: false, caa: true }, ...over });
const response = (status, headers = {}, body = "") => ({ status, headers: new Headers(headers), body: body ? [Buffer.from(body)] : null });

test("the machine directory is the only host inventory and settings stay bounded", async t => {
  const repo = await mkdtemp(join(tmpdir(), "aespatcher-network-"));
  t.after(() => rm(repo, { recursive: true, force: true }));
  await mkdir(join(repo, "system/public"), { recursive: true });
  await writeFile(join(repo, "system/public/network.json"), JSON.stringify({ properties: [
    { url: "https://aesthetic.computer/", aliases: ["www.aesthetic.computer"] }, { url: "https://KIDLISP.com/", aliases: ["bad host"] }] }));
  const settings = networkSettings({ ...defaultNetwork(), extra: ["prompt.ac"], exclude: ["www.aesthetic.computer"] });
  assert.deepEqual(await directoryHosts(repo, settings), ["aesthetic.computer", "kidlisp.com", "prompt.ac"]);
  assert.throws(() => networkSettings({ ...defaultNetwork(), concurrency: 99 }), /Invalid network audit settings/);
  assert.throws(() => networkSettings({ ...defaultNetwork(), extra: ["localhost"] }), /Invalid network audit settings/);
  assert.throws(() => networkSettings({ ...defaultNetwork(), directory: "../etc/hosts" }), /Invalid repository file/);
  assert.throws(() => serverSettings([{ name: "lith", host: "root@lith; rm", ports: [22] }]), /Invalid server audit settings/);
  assert.throws(() => serverSettings([{ name: "a", host: "x", ports: [22] }, { name: "a", host: "y", ports: [22] }]), /Invalid server audit settings/);
});

test("http, plain and path probes keep headers, statuses and signature matches but never bodies", async () => {
  const headers = { "content-type": "text/html", "strict-transport-security": "max-age=63072000; includeSubDomains", server: "Caddy/2.8.4", "x-content-type-options": "nosniff" };
  const http = await httpProbe("example.test", async () => response(200, headers, "<html>secret body</html>"));
  assert.deepEqual([http.code, http.html, http.hstsMaxAge, http.serverVersion, http.noSniff, http.csp], [200, true, 63072000, true, true, false]);
  assert.ok(!JSON.stringify(http).includes("secret"));
  const plain = await plainProbe("example.test", async () => response(308, { location: "https://example.test/" }));
  assert.deepEqual([plain.redirectsToTls, plain.served], [true, false]);
  const leak = await plainProbe("example.test", async () => response(200, {}, "hello"));
  assert.deepEqual([leak.redirectsToTls, leak.served], [false, true]);
  const paths = await pathProbe("example.test", async url => url.endsWith("/.git/HEAD") ? response(200, {}, "ref: refs/heads/main\n") : response(200, { "content-type": "text/html" }, "<html>spa</html>"));
  assert.deepEqual(paths.exposed, ["/.git/HEAD"]);
  assert.ok(!JSON.stringify(paths).includes("refs/heads"));
  const down = await httpProbe("example.test", async () => { throw Object.assign(new Error("fetch failed"), { cause: { code: "ECONNREFUSED" } }); });
  assert.deepEqual(down, { status: "unavailable", reason: "ECONNREFUSED" });
});

test("dns probe reports dangling CNAMEs, unresolved hosts and CAA presence", async () => {
  const fail = code => Promise.reject(Object.assign(new Error(code), { code }));
  // Thunks: an eagerly rejected promise the probe never awaits would surface as an unhandled rejection.
  const resolver = (cname, a, caa, aaaa = () => fail("ENODATA")) => ({ resolveCname: cname, resolve4: a, resolve6: aaaa, resolveCaa: caa });
  const dangling = await dnsProbe("old.aesthetic.computer", resolver(() => Promise.resolve(["gone.example.net"]), () => fail("ENOTFOUND"), () => Promise.resolve([{ issue: "letsencrypt.org" }])));
  assert.deepEqual([dangling.dangling, dangling.resolved, dangling.caa, dangling.cname], [true, false, true, "gone.example.net"]);
  const healthy = await dnsProbe("aesthetic.computer", resolver(() => fail("ENODATA"), () => Promise.resolve(["203.0.113.5"]), () => fail("ENODATA")));
  assert.deepEqual([healthy.dangling, healthy.resolved, healthy.caa], [false, true, false]);
  const outage = await dnsProbe("aesthetic.computer", resolver(() => fail("ETIMEOUT"), () => fail("ETIMEOUT"), () => fail("ETIMEOUT"), () => fail("ETIMEOUT")));
  assert.deepEqual(outage, { status: "unavailable", reason: "ETIMEOUT" });
});

test("network findings group hosts by kind and rank the exposed path above everything", () => {
  const rows = [ok("aesthetic.computer"),
    ok("www.aesthetic.computer", { http: { ...ok("x").http, hstsMaxAge: null, serverVersion: true } }),
    ok("kidlisp.com", { http: { ...ok("x").http, hstsMaxAge: 3600 }, paths: { status: "available", exposed: ["/.env"], unavailable: [] } }),
    ok("old.kidlisp.com", { tls: { ...ok("x").tls, daysLeft: 3 }, dns: { status: "available", resolved: false, cname: "gone.example.net", dangling: true, caa: false } }),
    ok("legacy.kidlisp.com", { tls: { ...ok("x").tls, authorized: false, error: "CERT_HAS_EXPIRED", protocol: "TLSv1.1" }, plain: { status: "available", code: 200, redirectsToTls: false, served: true } }),
    ok("down.kidlisp.com", { tls: { status: "unavailable", reason: "ECONNREFUSED" }, http: { status: "unavailable", reason: "ECONNREFUSED" }, plain: { status: "unavailable", reason: "ECONNREFUSED" }, paths: { status: "unavailable", exposed: [], unavailable: ["/.env"] }, dns: { status: "available", resolved: false, cname: null, dangling: false, caa: false } })];
  const groups = networkFindings(rows, { expiryDays: 14 });
  assert.deepEqual(Object.fromEntries(groups.map(g => [g.kind, g.hosts])), {
    "hsts-weak-or-missing": ["www.aesthetic.computer", "kidlisp.com"], "server-version-disclosed": ["www.aesthetic.computer"], "sensitive-path-exposed": ["kidlisp.com"],
    "tls-expiring": ["old.kidlisp.com"], "dangling-cname": ["old.kidlisp.com"], "tls-invalid": ["legacy.kidlisp.com"], "tls-legacy-protocol": ["legacy.kidlisp.com"],
    "plain-http-served": ["legacy.kidlisp.com"], "dns-unresolved": ["down.kidlisp.com"] });
  const text = describe({ kind: "hsts-weak-or-missing", hosts: ["a.test", "b.test", "c.test", "d.test", "e.test"] });
  assert.match(text, /^a\.test, b\.test, c\.test and 2 more sends no Strict-Transport-Security/);
  assert.ok(text.length <= 300 && !/[\r\n<>{}@]/.test(text));
});

test("server facts are validated, judged against the allowed ports and never carry raw sockets", async () => {
  const facts = { kernel: "6.8.0-85-generic", listening: [{ port: 22, scope: "public", process: "sshd" }, { port: 443, scope: "public", process: "caddy" }, { port: 8888, scope: "public", process: "node" }, { port: 27017, scope: "loopback", process: "mongod" }, { port: 41641, scope: "tailnet", process: "tailscaled" }],
    sshd: { passwordAuthentication: "yes", permitRootLogin: "prohibit-password" }, pendingSecurityUpgrades: 2, rebootRequired: true };
  validateServerFacts(facts);
  assert.throws(() => validateServerFacts({ ...facts, listening: [{ port: 22, scope: "public", process: "sshd <script>" }] }), /Rejected server facts/);
  assert.throws(() => validateServerFacts({ ...facts, kernel: "x\ny" }), /Rejected server facts/);
  const rows = await auditServers([{ name: "lith", host: "root@lith.test", ports: [22, 80, 443] }, { name: "oven", host: "root@oven.test", ports: [22] }], "/repo",
    async server => server.name === "lith" ? { name: "lith", status: "available", ...facts } : { name: "oven", status: "unavailable", reason: "SSH transport failed." });
  const findings = serverFindings(rows);
  assert.deepEqual(findings.map(f => [f.kind, f.hosts[0]]), [["unexpected-listener", "lith"], ["ssh-password-auth", "lith"], ["pending-security-upgrades", "lith"], ["reboot-required", "lith"]]);
  assert.match(describe(findings[0]), /^lith TCP 8888=node listening on every interface/);
  // An active ufw that blocks the port turns the exposure into a binding advisory.
  const guarded = serverFindings([{ ...rows[0], firewall: { tool: "ufw", active: true, allowed: [22, 80, 443] }, sshd: null, pendingSecurityUpgrades: null, rebootRequired: false }]);
  assert.deepEqual(guarded.map(f => f.kind), ["listener-filtered"]);
  assert.throws(() => validateServerFacts({ ...facts, listening: [{ port: 53, scope: "resolver", process: "systemd-resolve" }] }), /Rejected server facts/);
  assert.equal(serverFindings([{ ...rows[0], allowed: [22, 80, 443, 8888], sshd: null, pendingSecurityUpgrades: null, rebootRequired: false }]).length, 0);
});

test("the collector folds the network and server audits into leads, metrics and a paging summary", async t => {
  const repo = await mkdtemp(join(tmpdir(), "aespatcher-network-collect-"));
  t.after(() => rm(repo, { recursive: true, force: true }));
  await mkdir(join(repo, "system/public"), { recursive: true });
  await writeFile(join(repo, "system/public/network.json"), JSON.stringify({ properties: [{ url: "https://aesthetic.computer/", aliases: ["www.aesthetic.computer"] }, { url: "https://kidlisp.com/" }] }));
  const config = { ...defaults(repo), security: { enabled: true, packages: [], runtimes: [], network: { ...defaultNetwork(), infrastructure: [], zones: false, email: false, api: false }, servers: [{ name: "lith", host: "root@lith.test", ports: [22, 80, 443] }] } };
  const fetchStub = async () => ({ ok: false });
  const probes = { tls: async host => ({ ...ok(host).tls, daysLeft: host === "kidlisp.com" ? 5 : 40 }), http: async host => ({ ...ok(host).http, hstsMaxAge: null }),
    plain: async host => ok(host).plain, paths: async host => ok(host).paths, dns: async host => ok(host).dns,
    server: async () => ({ name: "lith", status: "available", kernel: "6.8.0", listening: [{ port: 22, scope: "public", process: "sshd" }], sshd: { passwordAuthentication: "no", permitRootLogin: "prohibit-password" }, pendingSecurityUpgrades: 0, rebootRequired: false }) };
  const { signals, detail } = await collectSecurity(repo, config, { runtime: null, remote: true, fetchImpl: fetchStub, probes, now });
  validateSignals(signals, []);
  assert.deepEqual(signals.coverage.filter(c => ["network", "servers"].includes(c.source)), [{ source: "network", status: "available", scanned: 3, truncated: false }, { source: "servers", status: "available", scanned: 1, truncated: false }]);
  assert.deepEqual(signals.leads.map(l => [l.source, l.kind, l.count]), [["network", "hsts-weak-or-missing", 3], ["network", "tls-expiring", 1]]);
  assert.equal(signals.metrics.find(m => m.metric === "tls-days-left" && m.target === "kidlisp.com").count, 5);
  assert.equal(signals.metrics.find(m => m.metric === "public-listeners" && m.target === "lith").count, 1);
  const summary = securitySummary(detail, signals);
  assert.deepEqual([summary.required, summary.urgent], [true, true]);
  assert.deepEqual(summary.network.findings, [{ kind: "hsts-weak-or-missing", hosts: 3 }, { kind: "tls-expiring", hosts: 1 }]);
  assert.deepEqual(summary.servers[0].publicPorts, [22]);
  assert.match(summary.actions[0], /^advisory: /); assert.match(summary.actions[1], /^urgent: /);

  // An advisory alone never pages; a disabled audit says so instead of claiming a clean surface.
  const calm = await collectSecurity(repo, config, { runtime: null, remote: false, fetchImpl: fetchStub, probes: { ...probes, tls: async host => ok(host).tls }, now });
  assert.deepEqual([securitySummary(calm.detail, calm.signals).urgent, calm.signals.coverage.at(-1)], [false, { source: "servers", status: "disabled" }]);
  const off = await collectSecurity(repo, { ...config, security: { ...config.security, network: { ...defaultNetwork(), enabled: false } } }, { remote: false, network: false, fetchImpl: fetchStub, now });
  assert.deepEqual(off.signals.coverage.filter(c => c.source === "network"), [{ source: "network", status: "disabled" }]);
});
