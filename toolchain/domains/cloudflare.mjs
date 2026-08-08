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
// Credentials are the account-wide Global API Key (email + key) rather than a
// scoped token, because that is what the vault already carries and what
// lith/deploy.fish already reads. They are looked up across the same candidate
// files deploy.fish uses, since no single env file has been canonical.
//
// Records are created DNS-only ("grey cloud") on purpose. Proxying puts
// Cloudflare's certificate in front of lith, which would force each Caddy host
// block off its Let's Encrypt issuer and onto the origin-certificate pattern —
// a deliberate migration, never a side effect of adding a domain.

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";

const BASE = "https://api.cloudflare.com/client/v4";
const HERE = dirname(fileURLToPath(import.meta.url));
const VAULT = resolve(HERE, "../../aesthetic-computer-vault");

// The same list lith/deploy.fish walks. Keys have historically lived in more
// than one of these, so the first hit wins rather than one file being canonical.
const ENV_FILES = [
  "lith/.env",
  ".devcontainer/envs/devcontainer.env",
  "oven/deploy.env",
  "nanos/conductor.env",
  "help/deploy.env",
  "at/deploy.env",
];

function key(name) {
  if (process.env[name]) return process.env[name].trim();
  for (const file of ENV_FILES) {
    try {
      const line = readFileSync(resolve(VAULT, file), "utf8").split("\n")
        .find((l) => new RegExp(`^(export )?${name}=`).test(l));
      if (line) {
        return line.replace(new RegExp(`^(export )?${name}=`), "").trim()
          .replace(/^["']|["']$/g, "");
      }
    } catch {
      // Not every machine has every vault file — keep looking.
    }
  }
  return undefined;
}

export function credentials() {
  return { email: key("CLOUDFLARE_EMAIL"), apiKey: key("CLOUDFLARE_API_KEY") };
}

const sleep = (ms) => new Promise((done) => setTimeout(done, ms));

// Cloudflare's API resolves to IPv6 first on some networks here, and a stalled
// v6 route surfaces as a bare "fetch failed". Retry transport failures only —
// anything the API actually answers is returned for the caller to judge, so a
// retry can never turn a declined create into a second zone.
async function call(method, path, body, attempt = 1) {
  const { email, apiKey } = credentials();
  if (!email || !apiKey) {
    throw new Error(
      "Missing Cloudflare keys. Set CLOUDFLARE_EMAIL and CLOUDFLARE_API_KEY " +
        "(env, or a vault env file such as .devcontainer/envs/devcontainer.env).",
    );
  }
  try {
    const response = await fetch(BASE + path, {
      method,
      headers: {
        "X-Auth-Email": email,
        "X-Auth-Key": apiKey,
        "Content-Type": "application/json",
      },
      body: body ? JSON.stringify(body) : undefined,
      signal: AbortSignal.timeout(30000),
    });
    return response.json();
  } catch (error) {
    if (attempt >= 4) throw error;
    await sleep(attempt * 1500);
    return call(method, path, body, attempt + 1);
  }
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
  const found = must(await call("GET", `/zones?name=${domain}`), "zones");
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

export const records = async (zoneId) =>
  must(await call("GET", `/zones/${zoneId}/dns_records?per_page=200`), "records");

// `host` is the bare domain for the apex, or a bare label like "midi".
export async function createRecord(zoneId, host, content,
  { type = "A", ttl = 300, proxied = false, comment = "lith" } = {}) {
  const made = await call("POST", `/zones/${zoneId}/dns_records`,
    { type, name: host, content, ttl, proxied, comment });
  if (!made.success) throw new Error(`create ${type} ${host} — ${why(made)}`);
  return made.result;
}
