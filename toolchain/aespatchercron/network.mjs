import { connect } from "node:tls";
import { promises as dns } from "node:dns";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { run, relativeFile } from "./io.mjs";

// Public-surface audit of the AC network: every host in the curated machine
// directory is judged on TLS, transport headers, exposed repository/secret
// paths and DNS; each AC server is judged on its listening sockets, sshd
// policy and pending security upgrades. Only statuses, versions, counts and
// hostnames are kept; response bodies, raw sockets and addresses never are.
export const hostName = /^(?=.{1,253}$)(?:[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?\.)+[a-z]{2,24}$/;
export const sensitivePaths = [
  { path: "/.env", signature: /^\s*[A-Z][A-Z0-9_]*=/m },
  { path: "/.git/HEAD", signature: /^ref: refs\//m },
  { path: "/.git/config", signature: /\[core\]/ },
];
// Infrastructure names the public directory deliberately omits but the audit must not.
export const infrastructureHosts = ["lith.aesthetic.computer", "oven.aesthetic.computer", "session-server.aesthetic.computer", "relay-sfo.aesthetic.computer",
  "knot.aesthetic.computer", "inbound.aesthetic.computer", "releases.aesthetic.computer"];
export const defaultNetwork = () => ({ enabled: true, directory: "system/public/network.json", extra: [], exclude: [], concurrency: 6, expiryDays: 14,
  infrastructure: infrastructureHosts, zones: true, email: true, api: true });
// 4001 is the IPFS swarm port on the two hosts that run a daemon; it must face peers.
export const defaultServers = () => [
  { name: "lith", host: "root@lith.aesthetic.computer", identity: "aesthetic-computer-vault/home/.ssh/id_rsa", ports: [22, 80, 443, 4001] },
  { name: "oven", host: "root@137.184.237.166", identity: "aesthetic-computer-vault/oven/ssh/oven-deploy-key", ports: [22, 80, 443, 4001] },
  { name: "session", host: "root@157.245.134.225", identity: "~/.ssh/session_server", ports: [22, 80, 443], udp: [8891, 10000, 10001, 10002, 10003, 10004, 10005, 10006, 10007] },
];

export function networkSettings(value = defaultNetwork()) {
  const list = rows => Array.isArray(rows) && rows.length <= 100 && rows.every(h => typeof h === "string" && hostName.test(h));
  // Older config files predate the infrastructure, zone, email and api legs; the defaults fill in.
  for (const [key, fallback] of Object.entries({ infrastructure: infrastructureHosts, zones: true, email: true, api: true })) if (value?.[key] === undefined) value[key] = fallback;
  if (!value || typeof value !== "object" || typeof value.enabled !== "boolean" || !list(value.extra) || !list(value.exclude) || !list(value.infrastructure) ||
      !Number.isInteger(value.concurrency) || value.concurrency < 1 || value.concurrency > 16 ||
      !Number.isInteger(value.expiryDays) || value.expiryDays < 1 || value.expiryDays > 90 ||
      ["zones", "email", "api"].some(key => typeof value[key] !== "boolean")) throw new Error("Invalid network audit settings");
  relativeFile(value.directory);
  return value;
}

// Every A/AAAA/CNAME name in the zones the directory's apexes belong to, read
// through the existing vault credential. Names, types and the proxied flag are
// kept; record contents stay unless they name one of the audited servers.
export async function zoneInventory(apexes, { api = null } = {}) {
  const cf = api || await import("../domains/cloudflare.mjs");
  const zones = [], hosts = new Map();
  for (const apex of apexes) {
    let found;
    try { found = await cf.zone(apex); } catch (error) { zones.push({ apex, status: "unavailable", reason: String(error.message || "zone read failed").slice(0, 80) }); continue; }
    if (!found?.id) { zones.push({ apex, status: "unavailable", reason: "not a Cloudflare zone here" }); continue; }
    let rows;
    try { rows = await cf.records(found.id); } catch { zones.push({ apex, status: "unavailable", reason: "records read failed" }); continue; }
    let kept = 0;
    for (const row of rows) {
      if (!["A", "AAAA", "CNAME"].includes(row.type) || typeof row.name !== "string" || row.name.includes("*")) continue;
      const name = row.name.toLowerCase();
      if (!hostName.test(name)) continue;
      const entry = hosts.get(name) || { host: name, proxied: false, types: [], targets: [] };
      entry.proxied = entry.proxied || row.proxied === true; entry.types.push(row.type);
      if (typeof row.content === "string") entry.targets.push(row.content.toLowerCase().slice(0, 120));
      hosts.set(name, entry); kept++;
    }
    zones.push({ apex, status: "available", records: kept });
  }
  return { zones, hosts: [...hosts.values()].sort((a, b) => a.host.localeCompare(b.host)) };
}

export const apexOf = host => host.split(".").slice(-2).join(".");

// SPF, DMARC and MTA-STS at each apex. A zone that sends no mail still wants
// SPF -all and DMARC reject, or anyone can send as it.
export async function emailProbe(apex, resolver = dns) {
  const txt = async name => { try { return (await resolver.resolveTxt(name)).map(parts => parts.join("")); } catch (error) { return ["ENODATA", "ENOTFOUND"].includes(error.code) ? [] : null; } };
  const root = await txt(apex), dmarc = await txt(`_dmarc.${apex}`), sts = await txt(`_mta-sts.${apex}`);
  if (root === null || dmarc === null) return { apex, status: "unavailable", reason: "resolver failed" };
  const spf = root.find(value => /^v=spf1\b/i.test(value)) || null;
  const policy = dmarc.find(value => /^v=DMARC1\b/i.test(value)) || null;
  return { apex, status: "available", spf: spf ? (/[~?+]all\b/i.test(spf) ? "soft" : /-all\b/i.test(spf) ? "strict" : "open") : "missing",
    dmarc: policy ? ((/\bp=reject\b/i.exec(policy) && "reject") || (/\bp=quarantine\b/i.exec(policy) && "quarantine") || "none") : "missing",
    mtaSts: sts !== null && sts.some(value => /^v=STSv1\b/i.test(value)) };
}

export function emailFindings(rows) {
  const groups = new Map();
  const note = (kind, apex, severity) => { if (!groups.has(kind)) groups.set(kind, { kind, severity, hosts: [] }); groups.get(kind).hosts.push(apex); };
  for (const row of rows) {
    if (row.status !== "available") continue;
    if (row.spf === "missing") note("spf-missing", row.apex, "moderate"); else if (row.spf !== "strict") note("spf-soft", row.apex, "low");
    if (row.dmarc === "missing") note("dmarc-missing", row.apex, "moderate"); else if (row.dmarc === "none") note("dmarc-monitor-only", row.apex, "low");
  }
  return [...groups.values()];
}

// Can the origin be reached around Cloudflare? Connect to the server's own
// address with a proxied hostname as SNI and ask for its front page. A 2xx or
// 3xx means every edge rule can be skipped by anyone who learns the address.
export function originBypassProbe(address, host, { timeout = 10000 } = {}) {
  return new Promise(resolve => {
    const socket = connect({ host: address, port: 443, servername: host, rejectUnauthorized: false, timeout });
    let data = ""; const finish = row => { socket.destroy(); resolve(row); };
    socket.once("secureConnect", () => socket.write(`GET / HTTP/1.1\r\nHost: ${host}\r\nUser-Agent: aespatchercron-security/1\r\nConnection: close\r\n\r\n`));
    socket.on("data", chunk => { data += chunk.toString("latin1"); const match = /^HTTP\/1\.[01] (\d{3})/.exec(data); if (match) finish({ host, status: "available", code: Number(match[1]), answers: Number(match[1]) < 400 }); });
    socket.once("timeout", () => finish({ host, status: "unavailable", reason: "timeout" }));
    socket.once("error", error => finish({ host, status: "unavailable", reason: String(error.code || "error").slice(0, 40) }));
    socket.once("close", () => finish({ host, status: "unavailable", reason: "closed" }));
  });
}

export function serverSettings(rows = defaultServers()) {
  const ports = list => Array.isArray(list) && list.length <= 64 && list.every(p => Number.isInteger(p) && p >= 1 && p <= 65535);
  if (!Array.isArray(rows) || rows.length > 10 || rows.some(row => !row || typeof row !== "object" || !/^[a-z0-9-]{1,32}$/.test(row.name) ||
      !/^[a-zA-Z0-9_.@-]+$/.test(row.host) || (row.identity !== undefined && (typeof row.identity !== "string" || row.identity.includes("\0"))) ||
      !ports(row.ports) || (row.udp !== undefined && !ports(row.udp))) ||
      new Set(rows.map(row => row.name)).size !== rows.length) throw new Error("Invalid server audit settings");
  return rows;
}

// The directory is the public product list; it is the only host inventory read.
export async function directoryHosts(repo, settings) {
  const data = JSON.parse(await readFile(join(repo, relativeFile(settings.directory)), "utf8"));
  if (!Array.isArray(data?.properties)) throw new Error("Unreadable machine directory");
  const hosts = new Set();
  for (const property of data.properties) {
    hosts.add(new URL(property.url).host.toLowerCase());
    for (const alias of property.aliases || []) hosts.add(String(alias).toLowerCase());
  }
  for (const host of settings.extra) hosts.add(host);
  for (const host of settings.exclude) hosts.delete(host);
  const rows = [...hosts].filter(host => hostName.test(host)).sort();
  if (!rows.length || rows.length > 200) throw new Error("Unreadable machine directory");
  return rows;
}

export function tlsProbe(host, { timeout = 10000, now = new Date() } = {}) {
  return new Promise(resolve => {
    const socket = connect({ host, port: 443, servername: host, rejectUnauthorized: false, timeout });
    const finish = row => { socket.destroy(); resolve(row); };
    socket.once("secureConnect", () => {
      const cert = socket.getPeerCertificate();
      const validTo = Date.parse(cert?.valid_to);
      finish({ status: "available", protocol: socket.getProtocol(), authorized: socket.authorized,
        error: socket.authorized ? null : String(socket.authorizationError || "unauthorized").slice(0, 60),
        daysLeft: Number.isFinite(validTo) ? Math.floor((validTo - +now) / 86400000) : null, issuer: String(cert?.issuer?.O || "").slice(0, 60) });
    });
    socket.once("timeout", () => finish({ status: "unavailable", reason: "timeout" }));
    socket.once("error", error => finish({ status: "unavailable", reason: String(error.code || "error").slice(0, 40) }));
  });
}

async function read(fetchImpl, url, { limit = 64 * 1024, timeout = 10000 } = {}) {
  const response = await fetchImpl(url, { redirect: "manual", signal: AbortSignal.timeout(timeout), headers: { "user-agent": "aespatchercron-security/1 (+https://aesthetic.computer)" } });
  const chunks = []; let bytes = 0;
  if (response.body) {
    for await (const chunk of response.body) {
      bytes += chunk.byteLength;
      if (bytes > limit) break;
      chunks.push(chunk);
    }
    try { await response.body.cancel(); } catch {}
  }
  return { status: response.status, headers: response.headers, text: Buffer.concat(chunks).toString("utf8") };
}

const maxAge = value => Number(/max-age=(\d+)/i.exec(value || "")?.[1] ?? NaN);
const redirectsTo = (headers, host) => { try { const url = new URL(headers.get("location") || ""); return url.protocol === "https:" && url.host.toLowerCase() === host; } catch { return false; } };

export async function httpProbe(host, fetchImpl = fetch) {
  try {
    const { status, headers, text } = await read(fetchImpl, `https://${host}/`);
    const hsts = headers.get("strict-transport-security"), server = headers.get("server") || "";
    const csp = headers.get("content-security-policy") || "";
    return { status: "available", code: status, html: /text\/html/i.test(headers.get("content-type") || "") && text.length > 0,
      hstsMaxAge: hsts ? maxAge(hsts) : null, csp: csp.length > 0, frameProtection: /frame-ancestors/i.test(csp) || headers.has("x-frame-options"),
      noSniff: /nosniff/i.test(headers.get("x-content-type-options") || ""), referrerPolicy: headers.has("referrer-policy"),
      serverVersion: /\d/.test(server), poweredBy: headers.has("x-powered-by") };
  } catch (error) { return { status: "unavailable", reason: String(error.cause?.code || error.name || "error").slice(0, 40) }; }
}

export async function plainProbe(host, fetchImpl = fetch) {
  try {
    const { status, headers } = await read(fetchImpl, `http://${host}/`, { limit: 1024 });
    const redirect = status >= 300 && status < 400;
    return { status: "available", code: status, redirectsToTls: redirect && redirectsTo(headers, host), served: status >= 200 && status < 300 };
  } catch (error) { return { status: "unavailable", reason: String(error.cause?.code || error.name || "error").slice(0, 40) }; }
}

export async function pathProbe(host, fetchImpl = fetch) {
  const exposed = [], unavailable = [];
  for (const { path, signature } of sensitivePaths) {
    try {
      const { status, text } = await read(fetchImpl, `https://${host}${path}`, { limit: 4096 });
      // A match is the only fact kept; the body itself is dropped here.
      if (status === 200 && signature.test(text)) exposed.push(path);
    } catch { unavailable.push(path); }
  }
  return { status: unavailable.length === sensitivePaths.length ? "unavailable" : "available", exposed, unavailable };
}

const apex = host => host.split(".").slice(-2).join(".");

export async function dnsProbe(host, resolver = dns) {
  const row = { status: "available", resolved: false, cname: null, dangling: false, caa: null };
  const lookup = (method, name) => resolver[method](name).catch(error => ({ code: error.code }));
  const cname = await lookup("resolveCname", host);
  if (Array.isArray(cname) && cname.length) row.cname = String(cname[0]).toLowerCase().slice(0, 120);
  const [a, aaaa] = await Promise.all([lookup("resolve4", host), lookup("resolve6", host)]);
  row.resolved = (Array.isArray(a) && a.length > 0) || (Array.isArray(aaaa) && aaaa.length > 0);
  const failed = value => !Array.isArray(value) && !["ENODATA", "ENOTFOUND"].includes(value?.code);
  if (!row.resolved && failed(a) && failed(aaaa)) return { status: "unavailable", reason: String(a.code || aaaa.code || "resolver").slice(0, 40) };
  row.dangling = row.cname !== null && !row.resolved;
  const caa = await lookup("resolveCaa", apex(host));
  row.caa = Array.isArray(caa) ? caa.length > 0 : caa?.code === "ENODATA" || caa?.code === "ENOTFOUND" ? false : null;
  return row;
}

const timedOut = row => row.status === "unavailable" && /timeout|timedout/i.test(row.reason || "");
// One Cloudflare-fronted host timed out under the parallel first pass and
// answered a second later; a lone timeout gets one more try before it is a fact.
const retry = async (probe, row, delay) => timedOut(row) ? (await new Promise(r => setTimeout(r, delay)), probe()) : row;

export async function auditHost(host, probes = {}, options = {}) {
  const tls = () => (probes.tls || tlsProbe)(host, options), http = () => (probes.http || httpProbe)(host), plain = () => (probes.plain || plainProbe)(host);
  const [tlsRow, httpRow, plainRow, paths, dnsRow] = await Promise.all([tls(), http(), plain(), (probes.paths || pathProbe)(host), (probes.dns || dnsProbe)(host)]);
  const delay = options.retryDelay ?? 2000;
  return { host, tls: await retry(tls, tlsRow, delay), http: await retry(http, httpRow, delay), plain: await retry(plain, plainRow, delay), paths, dns: dnsRow };
}

// Travels over stdin and runs as root inside each server's node. Ports, short
// process names, sshd policy answers and counts are the only facts returned.
export function serverFacts() {
  const { execFileSync } = require("node:child_process"), { existsSync, readFileSync } = require("node:fs");
  const sh = (cmd, args) => { try { return execFileSync(cmd, args, { encoding: "utf8", timeout: 15000, stdio: ["ignore", "pipe", "ignore"] }); } catch { return null; } };
  // Loopback covers the whole 127/8 block (systemd-resolved sits on 127.0.0.53);
  // a tailnet bind is private to the mesh; everything else faces the internet.
  // ss writes the bind as 127.0.0.53%lo; the interface suffix is not part of the address.
  const scopeOf = address => /^(\[?::1\]?|127\.\d+\.\d+\.\d+)$/.test(address.replace(/%[a-z0-9]+$/i, "")) ? "loopback"
    : /^(100\.(6[4-9]|[7-9]\d|1[01]\d|12[0-7])\.|\[fd7a:115c:a1e0:)/.test(address) ? "tailnet" : "public";
  // ss names the main thread, and node names its own "MainThread"; the executable name is the program.
  const { readlinkSync } = require("node:fs"), { basename } = require("node:path");
  const command = (name, pid) => { try { return pid ? basename(readlinkSync(`/proc/${pid}/exe`)).replace(/ \(deleted\)$/, "") || name : name; } catch { return name; } };
  const listening = [];
  for (const line of (sh("ss", ["-H", "-tlnp"]) || "").split("\n")) {
    const match = /^LISTEN\s+\S+\s+\S+\s+(\S+):(\d+)\s+\S+(?:\s+users:\(\("([^"]+)",pid=(\d+))?/.exec(line.trim());
    if (!match) continue;
    listening.push({ port: Number(match[2]), scope: scopeOf(match[1]), process: command(match[3] || "", match[4]).slice(0, 32) });
  }
  // UDP sockets the same way: geckos media, the oskiewar relay and IPFS live here.
  const udp = [];
  for (const line of (sh("ss", ["-H", "-ulnp"]) || "").split("\n")) {
    const match = /^UNCONN\s+\S+\s+\S+\s+(\S+):(\d+)\s+\S+(?:\s+users:\(\("([^"]+)",pid=(\d+))?/.exec(line.trim());
    if (match) udp.push({ port: Number(match[2]), scope: scopeOf(match[1]), process: command(match[3] || "", match[4]).slice(0, 32) });
  }
  const sshd = sh("sshd", ["-T"]) || "";
  const option = key => (new RegExp(`^${key}\\s+(\\S+)`, "mi").exec(sshd) || [])[1] || null;
  const upgradable = sh("apt-get", ["-s", "upgrade"]);
  const pending = upgradable === null ? null : (upgradable.match(/^Inst .*-security/gm) || []).length;
  // ufw's numbered listing: "[ 1] 443/tcp  ALLOW IN  Anywhere". Only allowed TCP ports are kept.
  const ufw = sh("ufw", ["status", "numbered"]);
  const firewall = ufw === null ? null : { tool: "ufw", active: /^Status:\s+active/m.test(ufw),
    allowed: [...new Set([...ufw.matchAll(/^\[\s*\d+\]\s+(\d+)(?:\/tcp)?\s+ALLOW IN/gm)].map(m => Number(m[1])))].slice(0, 64) };
  // Who can log in: the key comments only, never the key material.
  let keys = null;
  try { keys = readFileSync("/root/.ssh/authorized_keys", "utf8").split("\n").filter(line => /^(ssh-|ecdsa-|sk-)/.test(line.trim())).map(line => (line.trim().split(/\s+/)[2] || "(no comment)").replace(/[^\w.@:/-]/g, "_").slice(0, 48)); } catch {}
  const enabled = unit => { const out = sh("systemctl", ["is-enabled", unit]); return out === null ? null : /^enabled/.test(out.trim()); };
  const active = unit => { const out = sh("systemctl", ["is-active", unit]); return out === null ? null : out.trim() === "active"; };
  // Leftover secret copies beside the deployed trees: names only, two levels deep.
  const stale = [];
  for (const root of ["/opt/ac", "/opt/oven", "/home/aesthetic-computer", "/root"]) {
    const out = sh("find", [root, "-maxdepth", "3", "(", "-name", ".env.*", "-o", "-name", "*.env.bak*", "-o", "-name", "*.pem.bak", "-o", "-name", "id_rsa.bak", ")", "!", "-name", "*.example", "!", "-name", "*.template", "-type", "f"]);
    if (out) for (const path of out.split("\n").filter(Boolean).slice(0, 20)) stale.push(path.replace(/^\/(opt|home|root)\//, "").slice(0, 80));
  }
  return { kernel: sh("uname", ["-r"])?.trim() || null, listening, udp, sshd: sshd ? { passwordAuthentication: option("passwordauthentication"), permitRootLogin: option("permitrootlogin") } : null,
    pendingSecurityUpgrades: pending, rebootRequired: existsSync("/var/run/reboot-required"), firewall,
    authorizedKeys: keys, unattendedUpgrades: enabled("unattended-upgrades"), fail2ban: active("fail2ban"), staleSecrets: stale.slice(0, 40) };
}

export function validateServerFacts(value) {
  const short = (s, n) => s === null || (typeof s === "string" && s.length <= n && !/[\r\n<>{}@]/.test(s));
  const port = p => Number.isInteger(p) && p >= 1 && p <= 65535;
  if (!value || typeof value !== "object" || Array.isArray(value) || !short(value.kernel, 64) || !Array.isArray(value.listening) || value.listening.length > 200 ||
      value.listening.some(row => !row || !port(row.port) || !["loopback", "tailnet", "public"].includes(row.scope) || !short(row.process, 32) || row.process === null) ||
      (value.sshd !== null && (!value.sshd || typeof value.sshd !== "object" || !short(value.sshd.passwordAuthentication, 16) || !short(value.sshd.permitRootLogin, 32))) ||
      (value.pendingSecurityUpgrades !== null && (!Number.isSafeInteger(value.pendingSecurityUpgrades) || value.pendingSecurityUpgrades < 0)) ||
      typeof value.rebootRequired !== "boolean" ||
      (value.firewall !== undefined && value.firewall !== null && (typeof value.firewall !== "object" || value.firewall.tool !== "ufw" || typeof value.firewall.active !== "boolean" ||
        !Array.isArray(value.firewall.allowed) || value.firewall.allowed.length > 64 || !value.firewall.allowed.every(port))) ||
      (value.udp !== undefined && (!Array.isArray(value.udp) || value.udp.length > 200 || value.udp.some(row => !row || !port(row.port) || !["loopback", "tailnet", "public"].includes(row.scope) || !short(row.process, 32) || row.process === null))) ||
      (value.authorizedKeys !== undefined && value.authorizedKeys !== null && (!Array.isArray(value.authorizedKeys) || value.authorizedKeys.length > 64 || !value.authorizedKeys.every(s => typeof s === "string" && /^[\w.@:/_-]{1,48}$/.test(s) || s === "(no comment)"))) ||
      ["unattendedUpgrades", "fail2ban"].some(key => value[key] !== undefined && value[key] !== null && typeof value[key] !== "boolean") ||
      (value.staleSecrets !== undefined && (!Array.isArray(value.staleSecrets) || value.staleSecrets.length > 40 || !value.staleSecrets.every(s => short(s, 80) && s !== null)))) throw new Error("Rejected server facts");
  return value;
}

export async function serverProbe(server, repo) {
  const identity = server.identity ? server.identity.startsWith("~/") ? join(process.env.HOME || "", server.identity.slice(2)) : server.identity.startsWith("/") ? server.identity : join(repo, relativeFile(server.identity)) : null;
  // The session droplet keeps node under fnm, which a non-login shell cannot see.
  const remote = `sh -c 'command -v node >/dev/null 2>&1 || { export PATH=/root/.local/share/fnm:$PATH; eval "$(fnm env)"; }; node --input-type=commonjs'`;
  const cmd = ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=10", "-o", "IdentitiesOnly=yes", ...(identity ? ["-i", identity] : []), server.host, remote];
  try {
    const result = await run(cmd, { input: `process.stdout.write(JSON.stringify((${serverFacts.toString()})()));`, timeout: 60000 });
    if (result.code !== 0) throw new Error("Server unavailable");
    return { name: server.name, status: "available", ...validateServerFacts(JSON.parse(result.stdout)) };
  } catch (error) { return { name: server.name, status: "unavailable", reason: /Rejected/.test(error.message) ? "Facts failed validation." : "SSH transport failed." }; }
}

export async function auditServers(servers, repo, probe = serverProbe) {
  const rows = [];
  for (const server of servers) rows.push({ ...(await probe(server, repo)), allowed: server.ports, allowedUdp: server.udp || [] });
  return rows;
}

async function pooled(items, size, work) {
  const results = new Array(items.length); let next = 0;
  await Promise.all(Array.from({ length: Math.min(size, items.length) }, async () => {
    while (next < items.length) { const i = next++; results[i] = await work(items[i]); }
  }));
  return results;
}

export async function auditNetwork(hosts, settings, probes = {}, options = {}) {
  return pooled(hosts, settings.concurrency, host => auditHost(host, probes, options));
}

// Findings are grouped by kind across hosts so one broken policy is one lead,
// not seventy; the metric rows keep the per-host facts.
export function networkFindings(rows, settings) {
  const groups = new Map();
  const note = (kind, host, severity) => { if (!groups.has(kind)) groups.set(kind, { kind, severity, hosts: [] }); groups.get(kind).hosts.push(host); };
  for (const row of rows) {
    const { host, tls, http, plain, paths, dns: dnsRow } = row;
    if (paths.status === "available" && paths.exposed.length) note("sensitive-path-exposed", host, "critical");
    if (tls.status === "available") {
      if (!tls.authorized) note("tls-invalid", host, "high");
      else if (tls.daysLeft !== null && tls.daysLeft <= settings.expiryDays) note("tls-expiring", host, "high");
      if (tls.protocol && !/^TLSv1\.[23]$/.test(tls.protocol)) note("tls-legacy-protocol", host, "high");
    }
    if (plain.status === "available" && plain.served) note("plain-http-served", host, "high");
    if (dnsRow.status === "available") {
      if (dnsRow.dangling) note("dangling-cname", host, "high");
      else if (!dnsRow.resolved) note("dns-unresolved", host, "moderate");
    }
    if (http.status === "available" && http.code < 400) {
      if (http.hstsMaxAge === null || http.hstsMaxAge < 15552000) note("hsts-weak-or-missing", host, "moderate");
      if (http.serverVersion || http.poweredBy) note("server-version-disclosed", host, "low");
      if (http.html && !http.noSniff) note("nosniff-missing", host, "low");
    }
  }
  return [...groups.values()];
}

const findingText = {
  "sensitive-path-exposed": "serves a repository or environment file over HTTPS; remove the file or deny the path in Caddy before anything else",
  "tls-invalid": "presents a certificate the client rejects (expired, wrong name or untrusted chain); check Caddy's issuance log and DNS for the host",
  "tls-expiring": "has a certificate within the expiry window; confirm Caddy renewal is not blocked by DNS or rate limits",
  "tls-legacy-protocol": "negotiates a TLS version older than 1.2; raise the minimum protocol in the Caddy tls block",
  "plain-http-served": "answers plain HTTP with content instead of redirecting to HTTPS; add the redirect so no session starts in the clear",
  "dangling-cname": "is a CNAME whose target no longer resolves; remove the record or restore the target before someone else claims it",
  "dns-unresolved": "does not resolve; remove it from the directory or restore its records",
  "hsts-weak-or-missing": "sends no Strict-Transport-Security header of at least 180 days; add it in the Caddy header block",
  "server-version-disclosed": "discloses a server or framework version header; strip it in Caddy",
  "nosniff-missing": "serves HTML without X-Content-Type-Options nosniff; add it in the Caddy header block",
  "spf-missing": "publishes no SPF record, so any server may send mail as it; add v=spf1 with the real senders and -all",
  "spf-soft": "has an SPF record that ends in ~all, ?all or +all, which receivers treat as advice; end it in -all",
  "dmarc-missing": "publishes no DMARC record, so SPF and DKIM failures are not enforced; add _dmarc with p=reject or p=quarantine",
  "dmarc-monitor-only": "has DMARC p=none, which only reports; move to p=quarantine or p=reject once the senders are listed",
  "origin-bypass": "answers its front page straight from the origin address with a proxied hostname as SNI, so Cloudflare's rules can be skipped by anyone who learns the address; restrict 443 to Cloudflare's ranges or require authenticated origin pulls",
};

export function serverFindings(rows) {
  const findings = [];
  for (const row of rows) {
    if (row.status !== "available") continue;
    const open = row.listening.filter(s => s.scope === "public" && !row.allowed.includes(s.port));
    const ports = rows => [...new Set(rows.map(s => `${s.port}=${s.process || "?"}`))].slice(0, 12).join(" ");
    // An active ufw that does not allow the port keeps it off the internet; that is an advisory, not an exposure.
    const filtered = row.firewall?.active ? open.filter(s => !row.firewall.allowed.includes(s.port)) : [];
    const exposed = open.filter(s => !filtered.includes(s));
    if (exposed.length) findings.push({ kind: "unexpected-listener", severity: "high", hosts: [row.name], detail: `TCP ${ports(exposed)} listening on every interface beyond the allowed list; bind it to loopback, close it, or allow it in security.servers` });
    if (filtered.length) findings.push({ kind: "listener-filtered", severity: "low", hosts: [row.name], detail: `TCP ${ports(filtered)} listening on every interface but blocked by ufw; bind it to loopback so the firewall is not the only guard` });
    // UDP: tailscaled, IPFS and Caddy's HTTP/3 on 443 bind every interface by design; anything else public and unlisted is a finding.
    const udpOpen = (row.udp || []).filter(s => s.scope === "public" && !["tailscaled", "ipfs"].includes(s.process) && !(s.process === "caddy" && s.port === 443) && !(row.allowedUdp || []).includes(s.port));
    if (udpOpen.length) findings.push({ kind: "unexpected-udp-listener", severity: "moderate", hosts: [row.name], detail: `UDP ${ports(udpOpen)} bound on every interface beyond the allowed list; confirm it is meant to face the internet or add it to security.servers udp` });
    if (row.sshd && /^yes$/i.test(row.sshd.passwordAuthentication || "")) findings.push({ kind: "ssh-password-auth", severity: "high", hosts: [row.name], detail: "sshd accepts passwords; set PasswordAuthentication no and reload sshd" });
    if (row.unattendedUpgrades === false) findings.push({ kind: "unattended-upgrades-off", severity: "moderate", hosts: [row.name], detail: "unattended-upgrades is not enabled, so security packages wait for a hand; enable the timer" });
    if (row.staleSecrets?.length) findings.push({ kind: "stale-secret-files", severity: "moderate", hosts: [row.name], detail: `${row.staleSecrets.length} leftover secret copies beside the deployed tree (${row.staleSecrets.slice(0, 3).join(" ")}${row.staleSecrets.length > 3 ? " and more" : ""}); delete what is not needed` });
    if (row.pendingSecurityUpgrades) findings.push({ kind: "pending-security-upgrades", severity: "moderate", hosts: [row.name], detail: `${row.pendingSecurityUpgrades} security package upgrades pending; apt-get upgrade and restart the affected services` });
    if (row.rebootRequired) findings.push({ kind: "reboot-required", severity: "moderate", hosts: [row.name], detail: "a kernel or libc update awaits a reboot; schedule one" });
  }
  return findings;
}

export function describe(group) {
  const names = group.hosts.slice(0, 3).join(", ") + (group.hosts.length > 3 ? ` and ${group.hosts.length - 3} more` : "");
  const text = group.detail || findingText[group.kind] || group.kind;
  return `${names} ${text}. Operator action, not a piece patch.`.slice(0, 300);
}
