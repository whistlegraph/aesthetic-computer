import { readFile, access } from "node:fs/promises";
import { join } from "node:path";
import { run, relativeFile } from "./io.mjs";
import { emptySignals } from "./signals.mjs";

// Runtime and dependency drift for the stack Lith runs. Versions, counts and
// package names are the only facts collected; advisory prose stays upstream.
export const releaseIndex = "https://nodejs.org/dist/index.json";
export const releaseSchedule = "https://raw.githubusercontent.com/nodejs/Release/main/schedule.json";
export const defaultSecurity = () => ({ enabled: true, packages: ["lith", "system", "oven", "session-server"], runtimes: [".node-version", "session-server/Dockerfile"] });

const semver = /^v?(\d+)\.(\d+)\.(\d+)$/;
const severities = ["critical", "high", "moderate", "low"];

export function securitySettings(config) {
  const settings = config.security ?? defaultSecurity();
  if (!settings || typeof settings !== "object" || typeof settings.enabled !== "boolean" ||
      !Array.isArray(settings.packages) || settings.packages.length > 10 || settings.packages.some(name => !/^[a-z0-9-]+$/.test(name)) ||
      !Array.isArray(settings.runtimes) || settings.runtimes.length > 10) throw new Error("Invalid security settings");
  settings.runtimes.forEach(relativeFile);
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

export async function collectSecurity(repo, config, { runtime = null, remote = true, fetchImpl = fetch, now = new Date() } = {}) {
  const settings = securitySettings(config), signals = emptySignals();
  const detail = { format: "aespatcher.security.v1", at: now.toISOString(), index: null, hosts: [], declared: [], dependencies: [] };
  if (!settings.enabled) {
    signals.coverage.push({ source: "runtime", status: "disabled" }, { source: "dependencies", status: "disabled" });
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
  return { signals, detail };
}

export function securitySummary(detail, signals) {
  const leads = (signals?.leads || []).filter(lead => ["runtime", "dependencies"].includes(lead.source));
  return { required: leads.length > 0, actions: leads.map(lead => lead.summary),
    hosts: (detail?.hosts || []).map(({ host, running, status, latest, latestSecurity }) => ({ host, running, status, latest, latestSecurity: latestSecurity?.version ?? null })),
    declared: (detail?.declared || []).map(({ file, status, running, latestSecurity, reason }) => ({ file, status, running: running ?? null, latestSecurity: latestSecurity?.version ?? null, ...(reason ? { reason } : {}) })),
    dependencies: (detail?.dependencies || []).map(({ package: name, status, counts, reason }) => ({ package: name, status, ...(counts ? { counts } : {}), ...(reason ? { reason } : {}) })) };
}
