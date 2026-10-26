import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";

// A static map of the HTTP handlers lith serves from system/netlify/functions:
// which accept writes, which call the shared authorization helpers, which
// carry any other gate (signatures, keys, passwords) and which open CORS to
// every origin. Names and booleans only; no handler source leaves this module.
export const functionsDir = "system/netlify/functions";
const writes = /httpMethod\s*(?:===?\s*"(?:POST|DELETE|PUT|PATCH)"|!==?\s*"POST")/;
const authorized = /\b(?:authorize|hasAdmin)\s*\(/;
const signed = /\b(?:constructEvent|verifySignature|verifyWebhook|LITH_OPERATIONS_TOKEN|x-hub-signature)\b/;
const gated = /\b(?:secret|api[_-]?key|token|password|admin|signature|hmac|bearer)\b/i;
const wildcard = /"Access-Control-Allow-Origin":\s*"\*"/;

export function classifyHandler(name, source) {
  const row = { name, writes: writes.test(source), authorized: authorized.test(source) || signed.test(source), gated: gated.test(source), wildcardCors: wildcard.test(source) };
  row.ungatedWrite = row.writes && !row.authorized && !row.gated;
  return row;
}

export async function mapHandlers(repo) {
  const dir = join(repo, functionsDir);
  const names = (await readdir(dir)).filter(name => /^[a-z0-9-]+\.mjs$/.test(name)).sort();
  const rows = [];
  for (const name of names) rows.push(classifyHandler(name.replace(/\.mjs$/, ""), await readFile(join(dir, name), "utf8")));
  return rows;
}

export function apiFindings(rows) {
  const open = rows.filter(row => row.ungatedWrite);
  // Writes with no visible gate are worth a human read, not a page: telemetry
  // sinks are meant to be open, and the regexes cannot tell those apart.
  return open.length ? [{ kind: "ungated-write-handler", severity: "moderate", hosts: open.map(row => row.name),
    detail: "accepts writes with no authorize(), signature or key check visible in the handler; confirm each is meant to be open" }] : [];
}

export function apiMetrics(rows) {
  return { handlers: rows.length, writes: rows.filter(r => r.writes).length, authorized: rows.filter(r => r.authorized).length,
    ungatedWrites: rows.filter(r => r.ungatedWrite).length, wildcardCors: rows.filter(r => r.wildcardCors).length };
}
