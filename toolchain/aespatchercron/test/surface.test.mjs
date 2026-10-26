import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, writeFile, rm } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { zoneInventory, emailProbe, emailFindings, validateServerFacts, serverFindings, describe, serverSettings, networkSettings, defaultNetwork } from "../network.mjs";
import { classifyHandler, mapHandlers, apiFindings, apiMetrics } from "../api.mjs";
import { collectSecurity, securitySummary } from "../security.mjs";
import { validateSignals } from "../signals.mjs";
import { defaults } from "../config.mjs";

const now = new Date("2026-10-26T12:00:00Z");
const fail = code => Promise.reject(Object.assign(new Error(code), { code }));

test("zone inventory keeps names, types and the proxied flag, skips wildcards and unreachable zones", async () => {
  const api = { zone: async apex => apex === "kidlisp.com" ? { id: "z1", name: apex } : apex === "jas.life" ? null : Promise.reject(new Error("zone read failed")),
    records: async () => [{ type: "A", name: "kidlisp.com", content: "203.0.113.5", proxied: true }, { type: "CNAME", name: "old.kidlisp.com", content: "gone.example.net", proxied: false },
      { type: "A", name: "*.kidlisp.com", content: "203.0.113.5" }, { type: "TXT", name: "kidlisp.com", content: "v=spf1 -all" }, { type: "AAAA", name: "KIDLISP.com", content: "2001:db8::1", proxied: true }] };
  const result = await zoneInventory(["kidlisp.com", "jas.life", "sotce.net"], { api });
  assert.deepEqual(result.zones.map(z => [z.apex, z.status, z.records ?? z.reason]), [["kidlisp.com", "available", 3], ["jas.life", "unavailable", "not a Cloudflare zone here"], ["sotce.net", "unavailable", "zone read failed"]]);
  assert.deepEqual(result.hosts.map(h => [h.host, h.proxied, h.types]), [["kidlisp.com", true, ["A", "AAAA"]], ["old.kidlisp.com", false, ["CNAME"]]]);
  assert.ok(!JSON.stringify(result).includes("spf1"));
});

test("email probe judges SPF and DMARC strength per apex and groups the findings", async () => {
  const resolver = answers => ({ resolveTxt: async name => { const v = answers[name]; if (v === undefined) return fail("ENODATA"); if (v === null) return fail("ETIMEOUT"); return v.map(s => [s]); } });
  const strict = await emailProbe("aesthetic.computer", resolver({ "aesthetic.computer": ["v=spf1 include:_spf.google.com -all"], "_dmarc.aesthetic.computer": ["v=DMARC1; p=reject; rua=mailto:x"], "_mta-sts.aesthetic.computer": ["v=STSv1; id=1"] }));
  assert.deepEqual([strict.spf, strict.dmarc, strict.mtaSts], ["strict", "reject", true]);
  const soft = await emailProbe("kidlisp.com", resolver({ "kidlisp.com": ["something", "v=spf1 ~all"], "_dmarc.kidlisp.com": ["v=DMARC1; p=none"] }));
  assert.deepEqual([soft.spf, soft.dmarc, soft.mtaSts], ["soft", "none", false]);
  const bare = await emailProbe("notepat.com", resolver({}));
  assert.deepEqual([bare.spf, bare.dmarc], ["missing", "missing"]);
  const down = await emailProbe("sotce.net", resolver({ "sotce.net": null }));
  assert.equal(down.status, "unavailable");
  const groups = emailFindings([strict, soft, bare, down]);
  assert.deepEqual(Object.fromEntries(groups.map(g => [g.kind, g.hosts])), { "spf-soft": ["kidlisp.com"], "dmarc-monitor-only": ["kidlisp.com"], "spf-missing": ["notepat.com"], "dmarc-missing": ["notepat.com"] });
  assert.match(describe(groups[2]), /^notepat\.com publishes no SPF record/);
});

test("handler map reads the real idioms: writes, authorize(), other gates and wildcard CORS", async t => {
  const auth = classifyHandler("a", 'import { authorize } from "../../backend/authorization.mjs";\nif (event.httpMethod !== "POST") return respond(405);\nconst user = await authorize(event.headers);\n"Access-Control-Allow-Origin": "*"');
  assert.deepEqual([auth.writes, auth.authorized, auth.ungatedWrite, auth.wildcardCors], [true, true, false, true]);
  const webhook = classifyHandler("b", 'if (event.httpMethod === "POST") { const sig = event.headers["stripe-signature"]; stripe.webhooks.constructEvent(body, sig, secret); }');
  assert.deepEqual([webhook.writes, webhook.authorized, webhook.ungatedWrite], [true, true, false]);
  const keyed = classifyHandler("c", 'if (event.httpMethod !== "POST") return; if (event.headers["x-api-key"] !== process.env.KEY) return respond(401);');
  assert.deepEqual([keyed.authorized, keyed.gated, keyed.ungatedWrite], [false, true, false]);
  const open = classifyHandler("d", 'if (event.httpMethod === "POST") { await db.insertOne(JSON.parse(event.body)); }');
  assert.equal(open.ungatedWrite, true);
  const read = classifyHandler("e", 'if (event.httpMethod !== "GET") return respond(405);');
  assert.deepEqual([read.writes, read.ungatedWrite], [false, false]);
  const repo = await mkdtemp(join(tmpdir(), "aespatcher-api-"));
  t.after(() => rm(repo, { recursive: true, force: true }));
  await mkdir(join(repo, "system/netlify/functions"), { recursive: true });
  await writeFile(join(repo, "system/netlify/functions/open-sink.mjs"), 'if (event.httpMethod === "POST") store(event.body);');
  await writeFile(join(repo, "system/netlify/functions/Bad Name.mjs"), "");
  const rows = await mapHandlers(repo);
  assert.deepEqual(rows.map(r => r.name), ["open-sink"]);
  assert.deepEqual(apiMetrics(rows), { handlers: 1, writes: 1, authorized: 0, ungatedWrites: 1, wildcardCors: 0 });
  assert.match(describe(apiFindings(rows)[0]), /^open-sink accepts writes with no authorize\(\)/);
  assert.equal(apiFindings([auth]).length, 0);
});

test("deeper server facts validate and judge UDP, upgrades and leftover secrets", () => {
  const facts = { kernel: "6.8.0", listening: [{ port: 22, scope: "public", process: "sshd" }], udp: [{ port: 41641, scope: "public", process: "tailscaled" }, { port: 8891, scope: "public", process: "node" }, { port: 5353, scope: "public", process: "avahi" }, { port: 443, scope: "public", process: "caddy" }, { port: 53, scope: "loopback", process: "systemd-resolved" }],
    sshd: { passwordAuthentication: "no", permitRootLogin: "prohibit-password" }, pendingSecurityUpgrades: 0, rebootRequired: false, firewall: null,
    authorizedKeys: ["jeffrey@neo", "deploy-key", "(no comment)"], unattendedUpgrades: false, fail2ban: null, staleSecrets: ["aesthetic-computer/session-server/.env.bak-2026-08-12"] };
  validateServerFacts(facts);
  assert.throws(() => validateServerFacts({ ...facts, authorizedKeys: ["ssh-ed25519 AAAA..."] }), /Rejected server facts/);
  assert.throws(() => validateServerFacts({ ...facts, udp: [{ port: 70000, scope: "public", process: "x" }] }), /Rejected server facts/);
  const [row] = [{ name: "session", status: "available", ...facts, allowed: [22], allowedUdp: [8891] }];
  const kinds = serverFindings([row]).map(f => f.kind);
  assert.deepEqual(kinds, ["unexpected-udp-listener", "unattended-upgrades-off", "stale-secret-files"]);
  assert.match(describe(serverFindings([row])[0]), /^session UDP 5353=avahi bound on every interface/);
  assert.equal(serverFindings([{ ...row, allowedUdp: [8891, 5353], unattendedUpgrades: true, staleSecrets: [] }]).length, 0);
  serverSettings([{ name: "s", host: "root@x", ports: [22], udp: [8891] }]);
  assert.throws(() => serverSettings([{ name: "s", host: "root@x", ports: [22], udp: [0] }]), /Invalid server audit settings/);
  assert.throws(() => networkSettings({ ...defaultNetwork(), infrastructure: ["bad host"] }), /Invalid network audit settings/);
});

test("the collector unions directory, infrastructure and zone hosts, then folds mail, origin and api into the report", async t => {
  const repo = await mkdtemp(join(tmpdir(), "aespatcher-surface-"));
  t.after(() => rm(repo, { recursive: true, force: true }));
  await mkdir(join(repo, "system/public"), { recursive: true });
  await mkdir(join(repo, "system/netlify/functions"), { recursive: true });
  await writeFile(join(repo, "system/public/network.json"), JSON.stringify({ properties: [{ url: "https://aesthetic.computer/", aliases: [] }] }));
  await writeFile(join(repo, "system/netlify/functions/sink.mjs"), 'if (event.httpMethod === "POST") store(event.body);');
  await writeFile(join(repo, "system/netlify/functions/safe.mjs"), 'if (event.httpMethod !== "POST") return; await authorize(event.headers);');
  const net = { ...defaultNetwork(), infrastructure: ["lith.aesthetic.computer"] };
  const config = { ...defaults(repo), security: { enabled: true, packages: [], runtimes: [], network: net, servers: [{ name: "lith", host: "root@lith.aesthetic.computer", ports: [22, 80, 443] }] } };
  const ok = host => ({ host, tls: { status: "available", protocol: "TLSv1.3", authorized: true, error: null, daysLeft: 60, issuer: "LE" },
    http: { status: "available", code: 200, html: true, hstsMaxAge: 31536000, csp: true, frameProtection: true, noSniff: true, referrerPolicy: true, serverVersion: false, poweredBy: false },
    plain: { status: "available", code: 308, redirectsToTls: true, served: false }, paths: { status: "available", exposed: [], unavailable: [] }, dns: { status: "available", resolved: true, cname: null, dangling: false, caa: true } });
  const probed = [];
  const probes = { tls: async h => ok(h).tls, http: async h => ok(h).http, plain: async h => ok(h).plain, paths: async h => ok(h).paths, dns: async h => { probed.push(h); return ok(h).dns; },
    zones: async apexes => ({ zones: apexes.map(apex => ({ apex, status: "available", records: 2 })), hosts: [{ host: "aesthetic.computer", proxied: true, types: ["A"], targets: [] }, { host: "forgotten.aesthetic.computer", proxied: false, types: ["A"], targets: [] }] }),
    resolve: async () => ["203.0.113.9"], origin: async (address, host) => ({ host, status: "available", code: 200, answers: true }),
    email: async apex => ({ apex, status: "available", spf: "missing", dmarc: "missing", mtaSts: false }),
    server: async () => ({ name: "lith", status: "available", kernel: "6.8.0", listening: [], udp: [], sshd: null, pendingSecurityUpgrades: 0, rebootRequired: false, firewall: null, authorizedKeys: ["a"], unattendedUpgrades: true, fail2ban: true, staleSecrets: [] }) };
  const { signals, detail } = await collectSecurity(repo, config, { runtime: null, remote: true, fetchImpl: async () => ({ ok: false }), probes, now });
  validateSignals(signals, []);
  assert.deepEqual(probed.sort(), ["aesthetic.computer", "forgotten.aesthetic.computer", "lith.aesthetic.computer"]);
  assert.deepEqual(detail.inventory, { directory: 1, infrastructure: 1, dns: 1, apexes: ["aesthetic.computer"] });
  assert.deepEqual(signals.leads.map(l => [l.source, l.kind, l.count]), [["network", "origin-bypass", 1], ["mail", "spf-missing", 1], ["mail", "dmarc-missing", 1], ["api", "ungated-write-handler", 1]]);
  assert.deepEqual(signals.coverage.filter(c => ["network", "mail", "api", "servers"].includes(c.source)).map(c => [c.source, c.status, c.scanned]), [["network", "available", 3], ["mail", "available", 1], ["api", "available", 2], ["servers", "available", 1]]);
  const summary = securitySummary(detail, signals);
  assert.deepEqual([summary.urgent, summary.network.originBypass, summary.mail[0].spf, summary.api.ungated, summary.servers[0].authorizedKeys], [false, ["aesthetic.computer"], "missing", ["sink"], ["a"]]);
  assert.match(summary.actions[0], /^advisory: aesthetic\.computer answers its front page straight from the origin/);

  // Every new leg can be switched off and says so instead of claiming clean.
  const off = await collectSecurity(repo, { ...config, security: { ...config.security, network: { ...net, zones: false, email: false, api: false } } }, { runtime: null, remote: false, fetchImpl: async () => ({ ok: false }), probes, now });
  assert.deepEqual(off.signals.coverage.filter(c => ["mail", "api"].includes(c.source)), [{ source: "mail", status: "disabled" }, { source: "api", status: "disabled" }]);
  assert.deepEqual(off.detail.inventory, { directory: 1, infrastructure: 1, dns: 0, apexes: ["aesthetic.computer"] });
});
