import { credentials } from "../domains/cloudflare.mjs";
import { VISIT_PROPERTIES, visitGroup } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";
import { run } from "./io.mjs";
import { collectAccessLogs } from "./lith-access.mjs";

export const edgeZones = ["aesthetic.computer", "laklok.com", "kidlisp.com", "notepat.com", "oskiewar.com", "nopaint.art"];
const api = "https://api.cloudflare.com/client/v4";
const hour = value => new Date(Math.floor(Date.parse(value) / 3600000) * 3600000).toISOString();
const integer = value => Number.isSafeInteger(value) && value >= 0;

export function edgeScope(config, window) {
  const zones = config.cloudflare?.zones || edgeZones;
  const directOriginZones = config.cloudflare?.directOriginZones ?? ["nopaint.art"];
  if (!Array.isArray(zones) || !zones.length || zones.length > 8 || new Set(zones).size !== zones.length ||
      !Array.isArray(directOriginZones) || directOriginZones.length > 8 ||
      directOriginZones.some(zone => !Object.hasOwn(VISIT_PROPERTIES, zone) || visitGroup(zone) !== "studio") ||
      zones.some(zone => !Object.hasOwn(VISIT_PROPERTIES, zone) || visitGroup(zone) !== "studio") ||
      !Number.isFinite(Date.parse(window.start)) || !Number.isFinite(Date.parse(window.end)) ||
      Date.parse(window.end) <= Date.parse(window.start) || Date.parse(window.end) - Date.parse(window.start) > 168 * 3600000)
    throw new Error("Invalid edge correlation scope");
  return { ...window, zones, directOriginZones: directOriginZones.filter(zone => zones.includes(zone)),
    hosts: [...new Set(zones.flatMap(zone => [zone, ...VISIT_PROPERTIES[zone]]))], limit: 1000 };
}

async function readCloudflare(path, headers, fetchImpl, body) {
  const response = await fetchImpl(api + path, { method: body ? "POST" : "GET", headers, redirect: "error",
    body: body ? JSON.stringify(body) : undefined, signal: AbortSignal.timeout(20000) });
  if (!response.ok) throw new Error("Cloudflare read unavailable");
  const chunks = []; let bytes = 0;
  for await (const chunk of response.body) {
    bytes += chunk.byteLength;
    if (bytes > 2 * 1024 * 1024) throw new Error("Cloudflare response exceeds bound");
    chunks.push(chunk);
  }
  return JSON.parse(Buffer.concat(chunks).toString("utf8"));
}

export function edgeQuery(zoneId, hosts, scope) {
  if (!/^[a-f0-9]{32}$/.test(zoneId) || !hosts.length || hosts.some(host => !scope.hosts.includes(host))) throw new Error("Invalid edge query binding");
  return `query { viewer { zones(filter: {zoneTag: ${JSON.stringify(zoneId)}}) {
    httpRequestsAdaptiveGroups(limit: ${scope.limit + 1}, orderBy: [datetimeHour_ASC], filter: {
      datetime_geq: ${JSON.stringify(scope.start)}, datetime_lt: ${JSON.stringify(scope.end)},
      clientRequestHTTPHost_in: ${JSON.stringify(hosts)}, requestSource: "eyeball"
    }) { count dimensions { datetimeHour clientRequestHTTPHost edgeResponseStatus originResponseStatus } avg { sampleInterval } }
  } } }`;
}

export function edgeRows(result, hosts, scope) {
  if (result.errors?.length || result.data?.viewer?.zones?.length !== 1) throw new Error("Cloudflare query unavailable");
  const rows = result.data.viewer.zones[0].httpRequestsAdaptiveGroups;
  if (!Array.isArray(rows) || rows.length > scope.limit + 1) throw new Error("Invalid edge groups");
  const buckets = new Map();
  for (const row of rows.slice(0, scope.limit)) {
    const d = row.dimensions, interval = row.avg?.sampleInterval;
    if (!d || !hosts.includes(d.clientRequestHTTPHost) || !integer(row.count) || !integer(d.edgeResponseStatus) ||
        (d.edgeResponseStatus !== 0 && (d.edgeResponseStatus < 100 || d.edgeResponseStatus > 599)) || !integer(d.originResponseStatus) ||
        (d.originResponseStatus !== 0 && (d.originResponseStatus < 100 || d.originResponseStatus > 599)) ||
        !Number.isFinite(interval) || interval < 1 || !Number.isFinite(Date.parse(d.datetimeHour)) ||
        Date.parse(d.datetimeHour) !== Date.parse(hour(d.datetimeHour)) ||
        Date.parse(d.datetimeHour) < Date.parse(hour(scope.start)) || Date.parse(d.datetimeHour) >= Date.parse(scope.end))
      throw new Error("Invalid edge aggregate");
    const host = d.clientRequestHTTPHost, time = hour(d.datetimeHour), key = `${host}:${time}`;
    const bucket = buckets.get(key) || { host, hour: time, requests: 0, errors5xx: 0, origin5xx: 0, withoutOrigin5xx: 0, afterOriginSuccess5xx: 0, sampled: false };
    bucket.requests += row.count;
    if (d.edgeResponseStatus >= 500) {
      bucket.errors5xx += row.count;
      if (d.originResponseStatus === 0) bucket.withoutOrigin5xx += row.count;
      if (d.originResponseStatus >= 200 && d.originResponseStatus < 400) bucket.afterOriginSuccess5xx += row.count;
    }
    if (d.originResponseStatus >= 500) bucket.origin5xx += row.count;
    bucket.sampled ||= interval > 1;
    if (![bucket.requests, bucket.errors5xx, bucket.origin5xx].every(integer)) throw new Error("Edge count exceeds bound");
    buckets.set(key, bucket);
  }
  return { rows: [...buckets.values()], groups: rows.length, truncated: rows.length > scope.limit };
}

export async function collectCloudflare(scope, { fetchImpl = globalThis.fetch, auth = credentials, env = process.env } = {}) {
  const token = env.AESPATCHER_CLOUDFLARE_API_TOKEN;
  const direct = zone => scope.directOriginZones?.includes(zone);
  const { email, apiKey } = token || scope.zones.every(direct) ? {} : auth();
  const headers = { "content-type": "application/json", ...(token ? { Authorization: `Bearer ${token}` } : { "X-Auth-Email": email, "X-Auth-Key": apiKey }) };
  const zones = await Promise.all(scope.zones.map(async zone => {
    if (direct(zone)) return { rows: [], coverage: { zone, status: "disabled", reason: "direct-origin; Lith access logs only" } };
    if (!token && (!email || !apiKey)) return { rows: [], coverage: { zone, status: "unavailable", reason: "missing-credential" } };
    try {
      const result = await readCloudflare(`/zones?name=${encodeURIComponent(zone)}`, headers, fetchImpl);
      const matches = result.success && Array.isArray(result.result) ? result.result.filter(row => row.name === zone && row.status === "active") : [];
      if (matches.length !== 1) return { rows: [], coverage: { zone, status: "unavailable", reason: "zone-not-accessible" } };
      const hosts = [zone, ...VISIT_PROPERTIES[zone]];
      const groups = edgeRows(await readCloudflare("/graphql", headers, fetchImpl, { query: edgeQuery(matches[0].id, hosts, scope) }), hosts, scope);
      return { rows: groups.rows, coverage: { zone, status: "available", groups: groups.groups, truncated: groups.truncated, estimated: true } };
    } catch { return { rows: [], coverage: { zone, status: "unavailable", reason: "analytics-read-unavailable" } }; }
  }));
  return { rows: zones.flatMap(zone => zone.rows), coverage: zones.map(zone => zone.coverage) };
}

export function validateAccess(data, scope) {
  const fields = ["host", "hour", "requests", "errors5xx"];
  if (data.status !== "available" || !Array.isArray(data.rows) || data.rows.length > 2000 || !integer(data.scanned) || typeof data.truncated !== "boolean" ||
      data.rows.some(row => Object.keys(row).length !== fields.length || !fields.every(field => Object.hasOwn(row, field)) ||
        !scope.hosts.includes(row.host) || !Number.isFinite(Date.parse(row.hour)) || row.hour !== hour(row.hour) ||
        Date.parse(row.hour) < Date.parse(hour(scope.start)) || Date.parse(row.hour) >= Date.parse(scope.end) ||
        !integer(row.requests) || !integer(row.errors5xx) || row.errors5xx > row.requests)) throw new Error("Invalid Lith access aggregates");
  return { status: "available", rows: data.rows, scanned: data.scanned, truncated: data.truncated };
}

async function readLithAccess(config, scope) {
  if (!/^[a-zA-Z0-9_.@-]+$/.test(config.lith?.host || "")) throw new Error("Invalid Lith host");
  const program = `import { readdir, lstat } from "node:fs/promises"; import { createReadStream } from "node:fs"; import { createGunzip } from "node:zlib";
    (${collectAccessLogs.toString()})(${JSON.stringify(scope)}).then(result => process.stdout.write(JSON.stringify(result))).catch(() => { process.exitCode = 1; });`;
  try {
    const result = await run(["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=10", ...(config.lith.identity ? ["-i", config.lith.identity] : []),
      config.lith.host, "node --input-type=module"], { input: program, timeout: 90000 });
    if (result.code !== 0) throw new Error("Access log unavailable");
    return validateAccess(JSON.parse(result.stdout), scope);
  } catch { return { status: "unavailable", rows: [], reason: "access-log-read-unavailable" }; }
}

export function correlateEdge(scope, cloudflare, access, minimum = 3) {
  const buckets = new Map();
  for (const row of cloudflare.rows) buckets.set(`${row.host}:${row.hour}`, { host: row.host, hour: row.hour, edge: { ...row }, lith: null });
  for (const row of access.rows) {
    const key = `${row.host}:${row.hour}`, bucket = buckets.get(key) || { host: row.host, hour: row.hour, edge: null, lith: null };
    bucket.lith = { requests: row.requests, errors5xx: row.errors5xx }; buckets.set(key, bucket);
  }
  const signals = { format: "aespatcher.supplement.v1", coverage: [], metrics: [], leads: [], privateReports: [] };
  for (const entry of cloudflare.coverage) signals.coverage.push({ source: "cloudflare", status: entry.status, reason: `${entry.zone}: ${entry.reason || "adaptive estimates; host/hour comparison only"}`,
    ...(entry.status === "available" ? { scanned: entry.groups, truncated: entry.truncated } : {}) });
  signals.coverage.push({ source: "lith-access", status: access.status, ...(access.status === "available"
    ? { scanned: access.scanned, truncated: access.truncated, reason: "Retained Caddy logs; direct traffic and edge traffic differ." } : { reason: access.reason }) });
  const findings = new Map(), totals = new Map();
  for (const bucket of buckets.values()) {
    const { edge, lith, host } = bucket;
    bucket.assessment = edge?.errors5xx >= minimum && lith?.errors5xx >= minimum ? "edge-and-lith-errors"
      : edge?.origin5xx >= minimum ? "origin-error-at-edge"
        : edge?.withoutOrigin5xx >= minimum ? "edge-without-origin-response"
          : edge?.errors5xx >= minimum ? "edge-error" : lith?.errors5xx >= minimum ? "lith-error" : "no-error-signal";
    const total = totals.get(host) || { edgeRequests: 0, edge5xx: 0, origin5xx: 0, lithRequests: 0, lith5xx: 0, hasEdge: false, hasLith: false };
    total.hasEdge ||= edge !== null; total.hasLith ||= lith !== null;
    total.edgeRequests += edge?.requests || 0; total.edge5xx += edge?.errors5xx || 0; total.origin5xx += edge?.origin5xx || 0;
    total.lithRequests += lith?.requests || 0; total.lith5xx += lith?.errors5xx || 0; totals.set(host, total);
    if (bucket.assessment !== "no-error-signal") {
      const key = `${host}:${bucket.assessment}`, finding = findings.get(key) || { host, assessment: bucket.assessment, hours: 0, count: 0 };
      finding.hours++;
      finding.count += bucket.assessment === "origin-error-at-edge" ? edge.origin5xx
        : bucket.assessment === "edge-without-origin-response" ? edge.withoutOrigin5xx
          : bucket.assessment === "lith-error" ? lith.errors5xx : edge.errors5xx;
      findings.set(key, finding);
    }
  }
  for (const [host, total] of totals) for (const metric of ["edgeRequests", "edge5xx", "origin5xx", "lithRequests", "lith5xx"]) {
    const lith = metric.startsWith("lith");
    if (lith ? total.hasLith : total.hasEdge) signals.metrics.push({ source: lith ? "lith-access" : "cloudflare", metric, target: host, count: total[metric] });
  }
  for (const finding of findings.values()) signals.leads.push({ source: finding.assessment === "lith-error" ? "lith-access" : "cloudflare", kind: "edge-correlation", route: null,
    count: finding.count, evidenceRefs: [], summary: `${finding.host}: ${finding.assessment} in ${finding.hours} UTC hour buckets. Aggregate timing overlap is a lead, not proof of cause; review coverage and reproduce.` });
  return { signals, correlation: { format: "aespatcher.edge.v1", start: scope.start, end: scope.end, bucketSeconds: 3600,
    cloudflare: cloudflare.coverage, lith: { status: access.status, ...(access.status === "available" ? { truncated: access.truncated } : { reason: access.reason }) },
    buckets: [...buckets.values()].sort((a, b) => a.hour.localeCompare(b.hour) || a.host.localeCompare(b.host)) } };
}

export async function collectEdge(config, window, dependencies = {}) {
  if (config.cloudflare?.enabled === false) return { signals: { format: "aespatcher.supplement.v1", coverage: [{ source: "cloudflare", status: "disabled", reason: "operator-disabled" }], metrics: [], leads: [], privateReports: [] }, correlation: null };
  const scope = edgeScope(config, window);
  const [cloudflare, access] = await Promise.all([collectCloudflare(scope, dependencies), (dependencies.readAccess || readLithAccess)(config, scope)]);
  return correlateEdge(scope, cloudflare, access, config.collection?.minimum || 3);
}
