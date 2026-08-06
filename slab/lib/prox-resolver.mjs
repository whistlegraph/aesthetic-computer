export const FRESH_LEDGER_MS = 60_000;
export const STALE_LEDGER_MS = 120_000;

export const RUNNING_STATUSES = new Set(["working", "awaiting", "rendering"]);

export function isoTime(ms) {
  const value = Number(ms);
  return Number.isFinite(value) && value > 0 ? new Date(value).toISOString() : "unknown";
}

export function ledgerFreshness(updatedAt, now = Date.now()) {
  const value = Number(updatedAt);
  if (!Number.isFinite(value) || value <= 0) return { state: "unknown", ageMs: Infinity };
  const ageMs = Math.max(0, now - value);
  if (ageMs <= FRESH_LEDGER_MS) return { state: "fresh", ageMs };
  if (ageMs <= STALE_LEDGER_MS) return { state: "delayed", ageMs };
  return { state: "stale", ageMs };
}

export function shortSessionId(id, length = 8) {
  return String(id || "unknown").slice(0, length);
}

export function canonicalHandle(rock) {
  return `${rock.host}:${rock.name}#${shortSessionId(rock.id)}`;
}

function groupDuplicates(rocks, keyFor) {
  const groups = new Map();
  for (const rock of rocks) {
    const key = keyFor(rock);
    if (!key) continue;
    const group = groups.get(key) || [];
    group.push(rock);
    groups.set(key, group);
  }
  return [...groups.entries()]
    .filter(([, matches]) => matches.length > 1)
    .map(([key, matches]) => ({ key, matches }));
}

export function duplicateReport(rocks) {
  return {
    ids: groupDuplicates(rocks, (r) => String(r.id || "").toLowerCase()),
    hostNames: groupDuplicates(rocks, (r) => `${r.host || ""}:${r.name || ""}`.toLowerCase()),
    names: groupDuplicates(rocks, (r) => String(r.name || "").toLowerCase()),
  };
}

// Resolution is deliberately tiered and labeled. Subject text is a discovery
// hint, never identity, and side-effecting tools reject it through
// actionableTarget below.
export function resolveRocks(rocks, handle) {
  if (!handle) return { hits: rocks, matchType: "all", query: "" };
  const query = String(handle).trim().toLowerCase();
  let host = null;
  let nameAndId = query;
  if (query.includes(":")) [host, nameAndId] = query.split(":", 2);

  let name = nameAndId;
  let idPrefix = "";
  if (nameAndId.includes("#")) [name, idPrefix] = nameAndId.split("#", 2);

  const inHost = (r) => !host || (host === "local" ? r.self : String(r.host).toLowerCase() === host);
  const result = (hits, matchType) => ({ hits, matchType, query });

  if (idPrefix) {
    const canonical = rocks.filter((r) => inHost(r) && String(r.name).toLowerCase() === name &&
      String(r.id).toLowerCase().startsWith(idPrefix));
    return result(canonical, "canonical-handle");
  }

  const id = rocks.filter((r) => inHost(r) && String(r.id).toLowerCase() === name);
  if (id.length) return result(id, "session-id");

  const exact = rocks.filter((r) => inHost(r) && String(r.name).toLowerCase() === name);
  if (exact.length) return result(exact, host ? "exact-host-name" : "exact-name");

  const prefix = rocks.filter((r) => inHost(r) && String(r.name).toLowerCase().startsWith(name));
  if (prefix.length) return result(prefix, "name-prefix");

  const nameSubstring = rocks.filter((r) => inHost(r) && String(r.name).toLowerCase().includes(name));
  if (nameSubstring.length) return result(nameSubstring, "name-substring");

  return result(rocks.filter((r) => inHost(r) &&
    String(r.subject || "").toLowerCase().includes(name)), "subject-substring");
}

const ACTIONABLE_MATCHES = new Set([
  "session-id", "canonical-handle", "exact-host-name", "exact-name",
]);

export function actionableTarget(resolution, { now = Date.now(), verb = "act on" } = {}) {
  const { hits, matchType, query } = resolution;
  const checked = `checked_at: ${isoTime(now)}`;
  if (!hits.length) throw new Error(`${checked}\nno rock resolves «${query}» to ${verb}`);
  if (hits.length > 1) {
    throw new Error(`${checked}\n«${query}» is ambiguous (${hits.map(canonicalHandle).join(", ")}); use a session id or canonical host:name#id`);
  }
  if (!ACTIONABLE_MATCHES.has(matchType)) {
    throw new Error(`${checked}\n«${query}» matched by ${matchType}, which is discovery-only; use ${canonicalHandle(hits[0])} or its session id to ${verb}`);
  }
  const rock = hits[0];
  const freshness = ledgerFreshness(rock.ledgerUpdatedAt, now);
  if (freshness.state !== "fresh") {
    throw new Error(`${checked}\n${canonicalHandle(rock)} has a ${freshness.state} ledger (${isoTime(rock.ledgerUpdatedAt)}); refusing to ${verb}`);
  }
  return rock;
}
