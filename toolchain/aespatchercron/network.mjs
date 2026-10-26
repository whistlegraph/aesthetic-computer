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
export const defaultNetwork = () => ({ enabled: true, directory: "system/public/network.json", extra: [], exclude: [], concurrency: 6, expiryDays: 14 });
// 4001 is the IPFS swarm port on the two hosts that run a daemon; it must face peers.
export const defaultServers = () => [
  { name: "lith", host: "root@lith.aesthetic.computer", identity: "aesthetic-computer-vault/home/.ssh/id_rsa", ports: [22, 80, 443, 4001] },
  { name: "oven", host: "root@137.184.237.166", identity: "aesthetic-computer-vault/oven/ssh/oven-deploy-key", ports: [22, 80, 443, 4001] },
  { name: "session", host: "root@157.245.134.225", identity: "~/.ssh/session_server", ports: [22, 80, 443] },
];

export function networkSettings(value = defaultNetwork()) {
  const list = rows => Array.isArray(rows) && rows.length <= 100 && rows.every(h => typeof h === "string" && hostName.test(h));
  if (!value || typeof value !== "object" || typeof value.enabled !== "boolean" || !list(value.extra) || !list(value.exclude) ||
      !Number.isInteger(value.concurrency) || value.concurrency < 1 || value.concurrency > 16 ||
      !Number.isInteger(value.expiryDays) || value.expiryDays < 1 || value.expiryDays > 90) throw new Error("Invalid network audit settings");
  relativeFile(value.directory);
  return value;
}

export function serverSettings(rows = defaultServers()) {
  if (!Array.isArray(rows) || rows.length > 10 || rows.some(row => !row || typeof row !== "object" || !/^[a-z0-9-]{1,32}$/.test(row.name) ||
      !/^[a-zA-Z0-9_.@-]+$/.test(row.host) || (row.identity !== undefined && (typeof row.identity !== "string" || row.identity.includes("\0"))) ||
      !Array.isArray(row.ports) || row.ports.length > 64 || row.ports.some(p => !Number.isInteger(p) || p < 1 || p > 65535)) ||
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
  const sshd = sh("sshd", ["-T"]) || "";
  const option = key => (new RegExp(`^${key}\\s+(\\S+)`, "mi").exec(sshd) || [])[1] || null;
  const upgradable = sh("apt-get", ["-s", "upgrade"]);
  const pending = upgradable === null ? null : (upgradable.match(/^Inst .*-security/gm) || []).length;
  // ufw's numbered listing: "[ 1] 443/tcp  ALLOW IN  Anywhere". Only allowed TCP ports are kept.
  const ufw = sh("ufw", ["status", "numbered"]);
  const firewall = ufw === null ? null : { tool: "ufw", active: /^Status:\s+active/m.test(ufw),
    allowed: [...new Set([...ufw.matchAll(/^\[\s*\d+\]\s+(\d+)(?:\/tcp)?\s+ALLOW IN/gm)].map(m => Number(m[1])))].slice(0, 64) };
  return { kernel: sh("uname", ["-r"])?.trim() || null, listening, sshd: sshd ? { passwordAuthentication: option("passwordauthentication"), permitRootLogin: option("permitrootlogin") } : null,
    pendingSecurityUpgrades: pending, rebootRequired: existsSync("/var/run/reboot-required"), firewall };
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
        !Array.isArray(value.firewall.allowed) || value.firewall.allowed.length > 64 || !value.firewall.allowed.every(port)))) throw new Error("Rejected server facts");
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
  for (const server of servers) rows.push({ ...(await probe(server, repo)), allowed: server.ports });
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
    if (row.sshd && /^yes$/i.test(row.sshd.passwordAuthentication || "")) findings.push({ kind: "ssh-password-auth", severity: "high", hosts: [row.name], detail: "sshd accepts passwords; set PasswordAuthentication no and reload sshd" });
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
