// cloudflare.mjs — a small, knowable client for the Cloudflare DNS API.
//
// Every AC vanity domain ends up here: the registrar only has to point its
// nameservers at Cloudflare once, and after that a new host is one API call
// rather than a trip through somebody's web UI.
//
// Endpoints we touch (v4):
//   /accounts                      — the account a new zone is created under
//   /zones?name={d}                — find a zone by name
//   /zones                         — adopt a domain (returns its nameservers)
//   /zones/{id}/dns_records        — list and create records
//
// Credentials resolve automatically through the shared vault service registry.
// Accept a scoped API token or the existing email + Global API Key pair.
//
// Records are created DNS-only ("grey cloud") on purpose. Proxying puts
// Cloudflare's certificate in front of lith, which would force each Caddy host
// block off its Let's Encrypt issuer and onto the origin-certificate pattern —
// a deliberate migration, never a side effect of adding a domain.

import { requireCredentials, redactCredentials } from "../vault/credentials.mjs";

const BASE = "https://api.cloudflare.com/client/v4";
export function credentials() {
  const { values } = requireCredentials("cloudflare");
  return { token: values.CLOUDFLARE_API_TOKEN, email: values.CLOUDFLARE_EMAIL, apiKey: values.CLOUDFLARE_API_KEY };
}

const sleep = (ms) => new Promise((done) => setTimeout(done, ms));

// Cloudflare's API resolves to IPv6 first on some networks here, and a stalled
// v6 route surfaces as a bare "fetch failed". Retry GET transport failures only.
// A lost mutation response may already have committed: never repeat the write.
async function call(method, path, body, attempt = 1) {
  const keys = credentials();
  const { token, email, apiKey } = keys;
  let response;
  try {
    response = await fetch(BASE + path, {
      method,
      headers: {
        ...(token ? { Authorization: `Bearer ${token}` } : { "X-Auth-Email": email, "X-Auth-Key": apiKey }),
        "Content-Type": "application/json",
      },
      body: body ? JSON.stringify(body) : undefined,
      signal: AbortSignal.timeout(30000),
      redirect: "error",
    });
  } catch (error) {
    if (method !== "GET" || attempt >= 4) throw new Error(redactCredentials(error.message, keys));
    await sleep(attempt * 1500);
    return call(method, path, body, attempt + 1);
  }
  let data;
  try { data = JSON.parse(redactCredentials(await response.text(), keys)); }
  catch { throw new Error(`Cloudflare returned invalid JSON (HTTP ${response.status})`); }
  if (!response.ok && data.success !== false) throw new Error(`Cloudflare HTTP ${response.status}`);
  return data;
}

export const why = (result) =>
  (result.errors || []).map((e) => `${e.code}: ${e.message}`).join("; ");

// Throw on anything the API refused, so callers can just await.
function must(result, what) {
  if (!result.success) throw new Error(`${what} — ${why(result)}`);
  return result.result;
}

export const account = async () => (await must(
  await call("GET", "/accounts"), "accounts"))[0];

export async function zone(domain) {
  const found = must(await call("GET", `/zones?name=${encodeURIComponent(domain)}`), "zones");
  return found[0] || null;
}

// Adopt a domain. Inert until the registrar's nameservers are repointed at the
// pair this returns, and deletable in the dashboard if it was a mistake.
export async function createZone(domain) {
  const owner = await account();
  const made = await call("POST", "/zones",
    { name: domain, account: { id: owner.id }, type: "full" });
  if (made.success) return made.result;
  const existing = await zone(domain);
  if (existing) return existing;
  throw new Error(`create zone ${domain} — ${why(made)}`);
}

export async function records(zoneId) {
  const records = [];
  for (let page = 1; ; page++) {
    const response = await call("GET", `/zones/${zoneId}/dns_records?per_page=200&page=${page}`);
    records.push(...must(response, "records"));
    if (page >= (response.result_info?.total_pages || 1)) return records;
  }
}

export const registrationStatus = async (accountId, domain) => must(await call("GET",
  `/accounts/${accountId}/registrar/registrations/${encodeURIComponent(domain)}/registration-status`), "registrar status");

export const updateRecord = async (zoneId, recordId, changes) => must(await call("PATCH",
  `/zones/${zoneId}/dns_records/${recordId}`, changes), "update record");

// `host` is the bare domain for the apex, or a bare label like "midi".
export async function createRecord(zoneId, host, content,
  { type = "A", ttl = 300, proxied = false, comment = "lith" } = {}) {
  const made = await call("POST", `/zones/${zoneId}/dns_records`,
    { type, name: host, content, ttl, proxied, comment });
  if (!made.success) throw new Error(`create ${type} ${host} — ${why(made)}`);
  return made.result;
}
