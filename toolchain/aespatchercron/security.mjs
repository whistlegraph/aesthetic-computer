import { readFile, access } from "node:fs/promises";
import { join } from "node:path";
import { run, relativeFile } from "./io.mjs";
import { emptySignals } from "./signals.mjs";
import { defaultNetwork, defaultServers, networkSettings, serverSettings, directoryHosts, auditNetwork, auditServers, networkFindings, serverFindings, describe,
  zoneInventory, apexOf, emailProbe, emailFindings, originBypassProbe, hostName } from "./network.mjs";
import { mapHandlers, apiFindings, apiMetrics } from "./api.mjs";
import { promises as dns } from "node:dns";

// Runtime and dependency drift for the stack Lith runs, plus the public
// surface of every host in the AC network (network.mjs). Versions, counts,
// hostnames and package names are the only facts collected; advisory prose
// and response bodies stay upstream.
export const releaseIndex = "https://nodejs.org/dist/index.json";
export const releaseSchedule = "https://raw.githubusercontent.com/nodejs/Release/main/schedule.json";
export const defaultSecurity = () => ({ enabled: true, packages: ["lith", "system", "oven", "session-server"], runtimes: [".node-version", "session-server/Dockerfile"],
  network: defaultNetwork(), servers: defaultServers() });

const semver = /^v?(\d+)\.(\d+)\.(\d+)$/;
const severities = ["critical", "high", "moderate", "low"];
// Which audit kinds page an operator under --strict; the rest are advisories.
const urgentKinds = new Set(["runtime-security-release", "runtime-end-of-life", "dependency-advisory", "sensitive-path-exposed", "tls-invalid", "tls-expiring",
  "tls-legacy-protocol", "plain-http-served", "dangling-cname", "unexpected-listener", "ssh-password-auth"]);

export function securitySettings(config) {
  const settings = config.security ?? defaultSecurity();
  if (!settings || typeof settings !== "object" || typeof settings.enabled !== "boolean" ||
      !Array.isArray(settings.packages) || settings.packages.length > 10 || settings.packages.some(name => !/^[a-z0-9-]+$/.test(name)) ||
      !Array.isArray(settings.runtimes) || settings.runtimes.length > 10) throw new Error("Invalid security settings");
  settings.runtimes.forEach(relativeFile);
  // Older config files predate the network audit; the defaults fill in.
  settings.network = networkSettings(settings.network ?? defaultNetwork());
  settings.servers = serverSettings(settings.servers ?? defaultServers());
  return settings;
}

export function parseVersion(value) {
  const match = semver.exec(String(value).trim());
  if (!match) throw new Error("Invalid Node version");
  return match.slice(1).map(Number);
}

export function compareVersions(a, b) {
  const x = parseVersion(a), y = parseVersion(b);
  return x[0] - y[0] || x[1] - y[1] || x[2] - y[2];
}

// Travels over stdin and runs inside Lith's node. Only version strings return.
export function runtimeFacts() {
  const v = process.versions;
  return { node: process.version, brotli: v.brotli, openssl: v.openssl, undici: v.undici, ares: v.ares };
}

export function validateRuntime(value) {
  const parts = ["brotli", "openssl", "undici", "ares"];
  const part = s => s === undefined || (typeof s === "string" && /^[0-9a-z][0-9a-z.+-]{0,39}$/i.test(s));
  if (!value || typeof value !== "object" || Array.isArray(value) || !semver.test(value.node) ||
      Object.keys(value).some(key => key !== "node" && !parts.includes(key)) || !parts.every(key => part(value[key]))) throw new Error("Rejected runtime facts");
  return value;
}

export function validateIndex(rows) {
  if (!Array.isArray(rows) || !rows.length || rows.length > 5000) throw new Error("Rejected release index");
  return rows.map(row => {
    if (!row || typeof row !== "object" || !semver.test(row.version) || !/^\d{4}-\d{2}-\d{2}$/.test(row.date)) throw new Error("Rejected release index");
    return { version: row.version, date: row.date, security: row.security === true };
  });
}

export function validateSchedule(value) {
  if (!value || typeof value !== "object" || Array.isArray(value)) throw new Error("Rejected release schedule");
  const schedule = {};
  for (const [line, row] of Object.entries(value)) {
    if (!/^v\d+$/.test(line) || !row || typeof row.end !== "string" || !/^\d{4}-\d{2}-\d{2}$/.test(row.end)) throw new Error("Rejected release schedule");
    schedule[line] = { end: row.end };
  }
  return schedule;
}

export function releaseStatus(running, index, schedule = {}, now = new Date()) {
  const parsed = parseVersion(running), line = `v${parsed[0]}`;
  const releases = index.filter(row => parseVersion(row.version)[0] === parsed[0]).sort((a, b) => compareVersions(b.version, a.version));
  const newer = releases.filter(row => compareVersions(row.version, running) > 0), security = newer.filter(row => row.security);
  const end = schedule[line]?.end ?? null;
  const status = !releases.length ? "unknown-line" : end && Date.parse(end) < +now ? "end-of-life"
    : security.length ? "security-release-available" : newer.length ? "release-available" : "current";
  return { running: `v${parsed.join(".")}`, line, status, latest: releases[0]?.version ?? null, latestSecurity: security[0] ?? null,
    behind: newer.length, securityBehind: security.length, endOfLife: end };
}

export async function declaredRuntimes(repo, files) {
  const rows = [];
  for (const file of files) {
    let text;
    try { text = await readFile(join(repo, relativeFile(file)), "utf8"); }
    catch { rows.push({ file, status: "unavailable", reason: "File unreadable." }); continue; }
    const found = file.endsWith("Dockerfile") ? /^FROM\s+node:(\d+\.\d+\.\d+)\b/m.exec(text)?.[1] : /^v?(\d+\.\d+\.\d+)\s*$/m.exec(text)?.[1];
    rows.push(found ? { file, status: "available", node: `v${found}` } : { file, status: "unavailable", reason: "No exact Node version declared." });
  }
  return rows;
}

const packageName = s => typeof s === "string" && /^(@[a-z0-9._-]+\/)?[a-z0-9._-]{1,214}$/i.test(s);

export function reduceAudit(text, tool) {
  const data = JSON.parse(text), counts = data.metadata?.vulnerabilities;
  if (!counts || severities.some(key => !Number.isSafeInteger(counts[key]) || counts[key] < 0)) throw new Error("Unexpected audit output");
  const advisories = [];
  const rows = tool === "npm" ? Object.values(data.vulnerabilities || {}) : Object.values(data.advisories || {});
  for (const row of rows) {
    const name = tool === "npm" ? row.name : row.module_name;
    if (!row || !["critical", "high"].includes(row.severity) || !packageName(name)) continue;
    advisories.push({ name, severity: row.severity, direct: tool === "npm" ? row.isDirect === true : null, fixAvailable: tool === "npm" ? row.fixAvailable !== false : null });
  }
  advisories.sort((a, b) => a.severity.localeCompare(b.severity) || a.name.localeCompare(b.name));
  return { counts: Object.fromEntries(severities.map(key => [key, counts[key]])), advisories: advisories.slice(0, 50), truncated: advisories.length > 50 };
}

const exists = path => access(path).then(() => true, () => false);

export async function auditPackage(repo, name) {
  if (!/^[a-z0-9-]+$/.test(name)) throw new Error("Invalid package directory");
  const cwd = join(repo, name);
  const tool = await exists(join(cwd, "pnpm-lock.yaml")) ? "pnpm" : await exists(join(cwd, "package-lock.json")) ? "npm" : null;
  if (!tool) return { package: name, status: "unavailable", reason: "No lockfile; production dependency audit skipped." };
  const argv = tool === "pnpm" ? ["pnpm", "audit", "--prod", "--json"] : ["npm", "audit", "--omit=dev", "--json"];
  try {
    // Both tools exit nonzero when advisories exist; the JSON report is still complete.
    const result = await run(argv, { cwd, timeout: 120000, limit: 16 * 1024 * 1024 });
    return { package: name, tool, status: "available", ...reduceAudit(result.stdout, tool) };
  } catch { return { package: name, tool, status: "unavailable", reason: "Audit command failed or exceeded its bound." }; }
}

async function readBounded(fetchImpl, url, limit) {
  const response = await fetchImpl(url, { redirect: "error", signal: AbortSignal.timeout(20000) });
  if (!response.ok) throw new Error("Release read unavailable");
  const chunks = []; let bytes = 0;
  for await (const chunk of response.body) {
    bytes += chunk.byteLength;
    if (bytes > limit) throw new Error("Release index exceeds bound");
    chunks.push(chunk);
  }
  return JSON.parse(Buffer.concat(chunks).toString("utf8"));
}

export async function lithRuntime(remote) {
  if (!/^[a-zA-Z0-9_.@-]+$/.test(remote.host)) throw new Error("Invalid Lith transport");
  const cmd = ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=10", ...(remote.identity ? ["-i", remote.identity] : []), remote.host, "node --input-type=module"];
  const result = await run(cmd, { input: `process.stdout.write(JSON.stringify((${runtimeFacts.toString()})()));`, timeout: 30000 });
  if (result.code !== 0) throw new Error("Lith unavailable");
  return validateRuntime(JSON.parse(result.stdout));
}

const target = file => file.replace(/^\.+/, "");

// Public hosts are probed over the open internet; servers over their own SSH
// key, so `remote` gates them with the Lith runtime read.
const lead = (signals, source, group) => {
  signals.metrics.push({ source, metric: group.kind, target: null, count: group.hosts.length });
  signals.leads.push({ source, kind: group.kind, route: null, count: group.hosts.length, evidenceRefs: [], summary: describe(group) });
};

// The host inventory is the public directory, the infrastructure names it
// omits, and every A/AAAA/CNAME name in the Cloudflare zones those apexes
// belong to, so a forgotten subdomain is audited like a product.
async function inventory(repo, settings, probes, detail) {
  const net = settings.network, sources = new Map();
  const add = (host, source) => { if (hostName.test(host) && !net.exclude.includes(host) && !sources.has(host)) sources.set(host, source); };
  for (const host of await directoryHosts(repo, net)) add(host, "directory");
  for (const host of net.infrastructure) add(host, "infrastructure");
  const apexes = [...new Set([...sources.keys()].map(apexOf))].sort();
  detail.zones = []; detail.proxied = [];
  if (net.zones) {
    try {
      const zones = await (probes.zones || zoneInventory)(apexes);
      detail.zones = zones.zones;
      for (const row of zones.hosts) { add(row.host, "dns"); if (row.proxied) detail.proxied.push(row.host); }
    } catch (error) { detail.zones = apexes.map(apex => ({ apex, status: "unavailable", reason: String(error.message || "zone read failed").slice(0, 80) })); }
  }
  detail.inventory = { directory: [...sources.values()].filter(s => s === "directory").length, infrastructure: [...sources.values()].filter(s => s === "infrastructure").length,
    dns: [...sources.values()].filter(s => s === "dns").length, apexes };
  return { hosts: [...sources.keys()].sort(), apexes };
}

async function collectSurface(repo, settings, signals, detail, { remote, network, probes, now }) {
  if (!settings.network.enabled || !network) { signals.coverage.push({ source: "network", status: "disabled" }, { source: "mail", status: "disabled" }, { source: "api", status: "disabled" }); }
  else {
    let found = null;
    try { found = await inventory(repo, settings, probes, detail); } catch {}
    if (!found) signals.coverage.push({ source: "network", status: "unavailable", reason: "Machine directory unreadable; no host inventory to audit." }, { source: "mail", status: "unavailable", reason: "No apexes without a host inventory." });
    else {
      const rows = await auditNetwork(found.hosts, settings.network, probes, { now });
      detail.network = rows;
      const partial = rows.filter(row => ["tls", "http", "plain", "paths", "dns"].some(key => row[key].status !== "available")).length;
      const zonesRead = detail.zones.filter(z => z.status === "available").length;
      signals.coverage.push({ source: "network", status: "available", scanned: rows.length, truncated: partial > 0 || (settings.network.zones && zonesRead < detail.zones.length) });
      for (const row of rows) if (row.tls.status === "available" && row.tls.daysLeft !== null) signals.metrics.push({ source: "network", metric: "tls-days-left", target: row.host, count: Math.max(0, row.tls.daysLeft) });
      signals.metrics.push({ source: "network", metric: "hosts-partially-unreachable", target: null, count: partial });
      signals.metrics.push({ source: "network", metric: "hosts-from-dns-only", target: null, count: detail.inventory.dns });
      for (const group of networkFindings(rows, settings.network)) lead(signals, "network", group);
      // Origin bypass: the first configured server's address, asked for up to five proxied hostnames.
      detail.origin = [];
      const origin = settings.servers[0];
      if (origin && detail.proxied.length) {
        let addresses = [];
        try { addresses = await (probes.resolve || (name => dns.resolve4(name)))(origin.host.replace(/^.*@/, "")); } catch {}
        if (/^\d+\.\d+\.\d+\.\d+$/.test(origin.host.replace(/^.*@/, ""))) addresses = [origin.host.replace(/^.*@/, "")];
        for (const host of detail.proxied.slice(0, 5)) if (addresses[0]) detail.origin.push(await (probes.origin || originBypassProbe)(addresses[0], host));
        const open = detail.origin.filter(row => row.status === "available" && row.answers);
        if (open.length) lead(signals, "network", { kind: "origin-bypass", severity: "moderate", hosts: open.map(row => row.host) });
      }
      // Mail authentication at every apex.
      if (settings.network.email) {
        detail.email = [];
        for (const apex of found.apexes) detail.email.push(await (probes.email || emailProbe)(apex));
        const read = detail.email.filter(row => row.status === "available");
        signals.coverage.push(read.length ? { source: "mail", status: "available", scanned: read.length, truncated: read.length < detail.email.length }
          : { source: "mail", status: "unavailable", reason: "Resolver answered for no apex." });
        for (const group of emailFindings(detail.email)) lead(signals, "mail", group);
      } else signals.coverage.push({ source: "mail", status: "disabled" });
    }
  }
  // Handler map: a static read of the checkout, no network; it rides the same switch.
  if (!settings.network.enabled || !network) {}
  else if (!settings.network.api) signals.coverage.push({ source: "api", status: "disabled" });
  else {
    let handlers = null;
    try { handlers = await mapHandlers(repo); } catch {}
    if (!handlers) signals.coverage.push({ source: "api", status: "unavailable", reason: "Function directory unreadable." });
    else {
      detail.api = { ...apiMetrics(handlers), ungated: handlers.filter(row => row.ungatedWrite).map(row => row.name) };
      signals.coverage.push({ source: "api", status: "available", scanned: handlers.length });
      for (const [metric, count] of Object.entries(apiMetrics(handlers))) signals.metrics.push({ source: "api", metric: `handlers-${metric}`, target: null, count });
      for (const group of apiFindings(handlers)) lead(signals, "api", group);
    }
  }
  if (!remote || !settings.servers.length) { signals.coverage.push({ source: "servers", status: "disabled" }); return; }
  const rows = await auditServers(settings.servers, repo, probes.server);
  detail.servers = rows;
  const reached = rows.filter(row => row.status === "available");
  signals.coverage.push(reached.length ? { source: "servers", status: "available", scanned: reached.length, truncated: reached.length < rows.length }
    : { source: "servers", status: "unavailable", reason: "No server answered over SSH; no clean-surface claim." });
  for (const row of reached) {
    signals.metrics.push({ source: "servers", metric: "public-listeners", target: row.name, count: row.listening.filter(s => s.scope === "public").length });
    if (row.pendingSecurityUpgrades !== null) signals.metrics.push({ source: "servers", metric: "pending-security-upgrades", target: row.name, count: row.pendingSecurityUpgrades });
  }
  for (const group of serverFindings(rows)) signals.leads.push({ source: "servers", kind: group.kind, route: null, count: group.hosts.length, evidenceRefs: [], summary: describe(group) });
}

export async function collectSecurity(repo, config, { runtime = null, remote = true, network = true, fetchImpl = fetch, probes = {}, now = new Date() } = {}) {
  const settings = securitySettings(config), signals = emptySignals();
  const detail = { format: "aespatcher.security.v3", at: now.toISOString(), index: null, hosts: [], declared: [], dependencies: [], network: [], servers: [], zones: [], email: [], origin: [], api: null };
  if (!settings.enabled) {
    for (const source of ["runtime", "dependencies", "network", "mail", "api", "servers"]) signals.coverage.push({ source, status: "disabled" });
    return { signals, detail };
  }
  let index = null, schedule = {};
  try { index = validateIndex(await readBounded(fetchImpl, releaseIndex, 4 * 1024 * 1024)); detail.index = { releases: index.length }; } catch {}
  try { schedule = validateSchedule(await readBounded(fetchImpl, releaseSchedule, 256 * 1024)); } catch {}
  let facts = runtime;
  if (!facts && remote) { try { facts = await lithRuntime(config.lith); } catch { facts = null; } }
  const hosts = facts ? [{ host: "lith", ...validateRuntime(facts) }] : [];
  const declared = await declaredRuntimes(repo, settings.runtimes);
  const judged = [];
  if (index) {
    for (const row of hosts) judged.push({ ...row, kind: "host", label: row.host, ...releaseStatus(row.node, index, schedule, now) });
    for (const row of declared.filter(r => r.status === "available")) judged.push({ ...row, kind: "declared", label: target(row.file), ...releaseStatus(row.node, index, schedule, now) });
  }
  detail.hosts = judged.filter(r => r.kind === "host"); detail.declared = [...judged.filter(r => r.kind === "declared"), ...declared.filter(r => r.status !== "available")];
  signals.coverage.push(!index ? { source: "runtime", status: "unavailable", reason: "Release index unreadable; version drift cannot be judged." }
    : { source: "runtime", status: "available", scanned: judged.length, truncated: !facts || declared.some(r => r.status !== "available") });
  for (const row of judged) {
    signals.metrics.push({ source: "runtime", metric: "security-releases-behind", target: row.label, count: row.securityBehind });
    signals.metrics.push({ source: "runtime", metric: "releases-behind", target: row.label, count: row.behind });
    const where = row.kind === "host" ? `Lith runs Node ${row.running}` : `${row.file} pins Node ${row.running}`;
    const act = row.kind === "host" ? "Upgrade the host package and restart lith.service; operator action, not a piece patch." : "Bump the pin and redeploy; operator action, not a piece patch.";
    if (row.status === "security-release-available") signals.leads.push({ source: "runtime", kind: "runtime-security-release", route: null, count: row.securityBehind, evidenceRefs: [],
      summary: `${where}; ${row.latestSecurity.version} (${row.latestSecurity.date}) is a newer security release on the ${row.line} line. ${act}` });
    else if (row.status === "end-of-life") signals.leads.push({ source: "runtime", kind: "runtime-end-of-life", route: null, count: 1, evidenceRefs: [],
      summary: `${where}; the ${row.line} line ended on ${row.endOfLife} and receives no fixes. Move to a supported line. ${act}` });
  }
  for (const name of settings.packages) detail.dependencies.push(await auditPackage(repo, name));
  const audited = detail.dependencies.filter(r => r.status === "available");
  signals.coverage.push({ source: "dependencies", status: audited.length ? "available" : "unavailable", scanned: audited.length,
    ...(audited.length ? { truncated: audited.length < detail.dependencies.length } : { reason: "No package audit completed; no clean-dependency claim." }) });
  for (const row of audited) {
    for (const key of severities) signals.metrics.push({ source: "dependencies", metric: `audit-${key}`, target: row.package, count: row.counts[key] });
    const urgent = row.counts.critical + row.counts.high;
    if (urgent) signals.leads.push({ source: "dependencies", kind: "dependency-advisory", route: null, count: urgent, evidenceRefs: [],
      summary: `${row.package} production lockfile carries ${row.counts.critical} critical and ${row.counts.high} high advisories (${row.advisories.filter(a => a.direct).length} direct); update the lockfile and review before deploying.` });
  }
  await collectSurface(repo, settings, signals, detail, { remote, network, probes, now });
  return { signals, detail };
}

export function securitySummary(detail, signals) {
  const leads = (signals?.leads || []).filter(lead => ["runtime", "dependencies", "network", "mail", "api", "servers"].includes(lead.source));
  const network = detail?.network || [], servers = detail?.servers || [];
  return { required: leads.length > 0, urgent: leads.some(lead => urgentKinds.has(lead.kind)),
    actions: leads.map(lead => `${urgentKinds.has(lead.kind) ? "urgent" : "advisory"}: ${lead.summary}`),
    network: { hosts: network.length, inventory: detail?.inventory || null, zones: (detail?.zones || []).map(({ apex, status, records, reason }) => ({ apex, status, ...(records !== undefined ? { records } : {}), ...(reason ? { reason } : {}) })),
      unreachable: network.filter(row => row.tls.status !== "available" && row.http.status !== "available").map(row => row.host),
      originBypass: (detail?.origin || []).filter(row => row.status === "available" && row.answers).map(row => row.host),
      findings: leads.filter(lead => lead.source === "network").map(({ kind, count }) => ({ kind, hosts: count })) },
    mail: (detail?.email || []).map(({ apex, status, spf, dmarc, mtaSts, reason }) => ({ apex, status, ...(status === "available" ? { spf, dmarc, mtaSts } : { reason }) })),
    api: detail?.api || null,
    servers: servers.map(row => ({ name: row.name, status: row.status, ...(row.status === "available" ? { kernel: row.kernel,
      publicPorts: [...new Set(row.listening.filter(s => s.scope === "public").map(s => s.port))].sort((a, b) => a - b),
      publicUdp: [...new Set((row.udp || []).filter(s => s.scope === "public").map(s => `${s.port}=${s.process}`))].sort(),
      authorizedKeys: row.authorizedKeys ?? null, unattendedUpgrades: row.unattendedUpgrades ?? null, fail2ban: row.fail2ban ?? null, staleSecrets: row.staleSecrets ?? null,
      passwordAuthentication: row.sshd?.passwordAuthentication ?? null, pendingSecurityUpgrades: row.pendingSecurityUpgrades, rebootRequired: row.rebootRequired,
      firewall: row.firewall ? (row.firewall.active ? `ufw active, allows ${row.firewall.allowed.join(" ") || "nothing"}` : "ufw inactive") : "none read" } : { reason: row.reason }) })),
    hosts: (detail?.hosts || []).map(({ host, running, status, latest, latestSecurity }) => ({ host, running, status, latest, latestSecurity: latestSecurity?.version ?? null })),
    declared: (detail?.declared || []).map(({ file, status, running, latestSecurity, reason }) => ({ file, status, running: running ?? null, latestSecurity: latestSecurity?.version ?? null, ...(reason ? { reason } : {}) })),
    dependencies: (detail?.dependencies || []).map(({ package: name, status, counts, reason }) => ({ package: name, status, ...(counts ? { counts } : {}), ...(reason ? { reason } : {}) })) };
}
