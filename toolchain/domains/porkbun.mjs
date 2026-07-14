// porkbun.mjs — a small, knowable client for the Porkbun domain API.
//
// Porkbun speaks JSON-over-POST. Every call carries the account keys in its
// body; registration additionally borrows the account's default WHOIS contact,
// so there are no per-call contact fields to thread through here.
//
// Endpoints we touch (v3):
//   /ping                       — auth smoke test, echoes your IP
//   /account/balance            — account credit (undocumented, but live)
//   /pricing/get                — every TLD's register/renew/transfer price
//   /domain/checkDomain/{d}     — availability + live price for one domain
//   /domain/create/{d}          — register (cost guard in cents + agreeToTerms)
//   /domain/getNs/{d}           — current nameservers
//   /domain/updateNs/{d}        — repoint nameservers
//   /dns/create/{d}             — add a DNS record
//
// Keys come from PORKBUN_API_KEY / PORKBUN_SECRET_API_KEY. If those aren't in
// the environment we fall back to the plaintext vault .env, matching the rest
// of scripts/ (which expect `source aesthetic-computer-vault/.env`).

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";

const BASE = "https://api.porkbun.com/api/json/v3";
const HERE = dirname(fileURLToPath(import.meta.url));
const VAULT_ENV = resolve(HERE, "../../aesthetic-computer-vault/.env");

// Pull a key from the environment, or from the vault .env as a fallback.
function key(name) {
  if (process.env[name]) return process.env[name].trim();
  try {
    const line = readFileSync(VAULT_ENV, "utf8")
      .split("\n")
      .find((l) => l.startsWith(name + "="));
    if (line) return line.slice(name.length + 1).trim().replace(/^["']|["']$/g, "");
  } catch {
    // No vault on this machine — that's fine, the caller reports the miss.
  }
  return undefined;
}

export function credentials() {
  const apikey = key("PORKBUN_API_KEY");
  const secretapikey = key("PORKBUN_SECRET_API_KEY");
  return { apikey, secretapikey };
}

// One POST. Throws on transport failure or a non-SUCCESS body so callers can
// just `await` and trust the result.
async function call(path, body = {}) {
  const { apikey, secretapikey } = credentials();
  if (!apikey || !secretapikey) {
    throw new Error(
      "Missing Porkbun keys. Set PORKBUN_API_KEY and PORKBUN_SECRET_API_KEY " +
        "(env or aesthetic-computer-vault/.env).",
    );
  }
  const res = await fetch(BASE + path, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ apikey, secretapikey, ...body }),
  });
  const data = await res.json().catch(() => ({}));
  if (data.status !== "SUCCESS") {
    throw new Error(data.message || `Porkbun ${path} failed (HTTP ${res.status})`);
  }
  return data;
}

export const ping = () => call("/ping");
export const pricing = () => call("/pricing/get");

// Registration spends account credit, never a card, so the balance is the one
// number that decides whether a buy can go through. Porkbun doesn't document
// this endpoint; it answers anyway, returning cents plus a preformatted string.
export const balance = () => call("/account/balance");

export const check = (domain) => call(`/domain/checkDomain/${domain}`);
export const getNs = (domain) => call(`/domain/getNs/${domain}`);

// Register a domain. `costInCents` is Porkbun's own guard: pass the price from
// a fresh availability check so the buy aborts if the price moved under us.
export const register = (domain, costInCents) =>
  call(`/domain/create/${domain}`, { cost: costInCents, agreeToTerms: "yes" });

// Repoint nameservers (e.g. to Cloudflare's pair). Porkbun wants an `ns` array.
export const updateNs = (domain, nameservers) =>
  call(`/domain/updateNs/${domain}`, { ns: nameservers });

export const dnsCreate = (domain, record) => call(`/dns/create/${domain}`, record);
