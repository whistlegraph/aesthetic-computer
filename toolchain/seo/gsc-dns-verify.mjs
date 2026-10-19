#!/usr/bin/env node
// gsc-dns-verify — put Google Search Console's DNS verification record on an
// AC apex, at whichever DNS host serves it. Google issues the token; this only
// writes it. The flow per domain:
//
//   1. search.google.com/search-console → Add property → Domain → <apex>;
//      copy the `google-site-verification=…` value it shows.
//   2. node toolchain/seo/gsc-dns-verify.mjs <apex> google-site-verification=… --apply
//   3. Press Verify in Search Console (DNS can take a few minutes).
//
//   node toolchain/seo/gsc-dns-verify.mjs              status of every apex (public DNS, no writes)
//   node toolchain/seo/gsc-dns-verify.mjs <apex> <token>          dry run: show the record
//   node toolchain/seo/gsc-dns-verify.mjs <apex> <token> --apply  write it (idempotent)
//
// Hosts: Cloudflare (vault cloudflare service), DigitalOcean (doctl's own
// auth), Porkbun (vault porkbun service). Adds a TXT record only; it never
// edits or deletes existing records.
import { execFileSync } from "node:child_process";
import { requireCredentials } from "../vault/credentials.mjs";

export const APEXES = {
  "aesthetic.computer": "cloudflare", "sotce.net": "cloudflare", "laklok.com": "cloudflare",
  "oskiewar.com": "cloudflare", "prompt.ac": "cloudflare", "whistlegraph.org": "cloudflare",
  "mime.ac": "cloudflare", "jas.life": "cloudflare",
  "nopaint.art": "digitalocean", "notepaint.art": "porkbun",
};
const TOKEN = /^google-site-verification=[A-Za-z0-9_-]{20,100}$/;

// Public DNS over HTTPS, so a local resolver that filters a domain cannot lie.
export async function txtRecords(domain, fetch = globalThis.fetch) {
  const res = await fetch(`https://cloudflare-dns.com/dns-query?name=${domain}&type=TXT`, { headers: { accept: "application/dns-json" } });
  const body = await res.json();
  return (body.Answer || []).filter(a => a.type === 16).map(a => a.data.replace(/^"|"$/g, "").replace(/" "/g, ""));
}

async function cloudflare(domain, token, apply) {
  const cred = requireCredentials("cloudflare");
  const headers = cred.auth === "api-token" ? { Authorization: `Bearer ${cred.values.CLOUDFLARE_API_TOKEN}` }
    : { "X-Auth-Email": cred.values.CLOUDFLARE_EMAIL, "X-Auth-Key": cred.values.CLOUDFLARE_API_KEY };
  const api = async (path, init = {}) => {
    const res = await fetch(`https://api.cloudflare.com/client/v4${path}`, { ...init, headers: { ...headers, "Content-Type": "application/json" } });
    const body = await res.json();
    if (!body.success) throw new Error(`Cloudflare: ${JSON.stringify(body.errors)}`);
    return body.result;
  };
  const [zone] = await api(`/zones?name=${domain}`);
  if (!zone) throw new Error(`Cloudflare has no zone for ${domain}`);
  const existing = await api(`/zones/${zone.id}/dns_records?type=TXT&name=${domain}&content=${encodeURIComponent(token)}`);
  if (existing.length) return "already present";
  if (!apply) return `would add TXT ${domain} "${token}" in Cloudflare zone ${zone.name}`;
  await api(`/zones/${zone.id}/dns_records`, { method: "POST", body: JSON.stringify({ type: "TXT", name: domain, content: token, ttl: 3600, comment: "Google Search Console domain verification" }) });
  return "added (Cloudflare)";
}

function digitalocean(domain, token, apply) {
  const rows = JSON.parse(execFileSync("doctl", ["compute", "domain", "records", "list", domain, "--output", "json"], { encoding: "utf8" }));
  if (rows.some(r => r.type === "TXT" && r.data === token)) return "already present";
  if (!apply) return `would add TXT @ "${token}" in DigitalOcean domain ${domain}`;
  execFileSync("doctl", ["compute", "domain", "records", "create", domain, "--record-type", "TXT", "--record-name", "@", "--record-data", token, "--record-ttl", "3600"], { stdio: "ignore" });
  return "added (DigitalOcean)";
}

async function porkbun(domain, token, apply) {
  const { values } = requireCredentials("porkbun");
  const auth = { apikey: values.PORKBUN_API_KEY, secretapikey: values.PORKBUN_SECRET_API_KEY };
  const post = async (path, extra = {}) => {
    const res = await fetch(`https://api.porkbun.com/api/json/v3${path}`, { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ ...auth, ...extra }) });
    const body = await res.json();
    if (body.status !== "SUCCESS") throw new Error(`Porkbun: ${body.message || res.status}`);
    return body;
  };
  const { records = [] } = await post(`/dns/retrieveByNameType/${domain}/TXT`);
  if (records.some(r => r.content === token)) return "already present";
  if (!apply) return `would add TXT @ "${token}" in Porkbun domain ${domain}`;
  await post(`/dns/create/${domain}`, { type: "TXT", name: "", content: token, ttl: "3600" });
  return "added (Porkbun)";
}

const [domain, token] = process.argv.slice(2).filter(a => !a.startsWith("--"));
const apply = process.argv.includes("--apply");
if (import.meta.url === `file://${process.argv[1]}`) {
  if (!domain) {
    for (const [apex, host] of Object.entries(APEXES)) {
      const txt = await txtRecords(apex).catch(() => null);
      const verified = txt?.some(t => t.startsWith("google-site-verification="));
      console.log(`${apex.padEnd(20)} ${host.padEnd(13)} ${txt === null ? "dns lookup failed" : verified ? "✓ has a Google verification record" : "· no Google verification record"}`);
    }
  } else {
    if (!APEXES[domain]) throw new Error(`unknown apex ${domain}; known: ${Object.keys(APEXES).join(", ")}`);
    if (!TOKEN.test(token || "")) throw new Error("second argument must be the full google-site-verification=… value from Search Console");
    const host = APEXES[domain];
    const result = host === "cloudflare" ? await cloudflare(domain, token, apply)
      : host === "digitalocean" ? digitalocean(domain, token, apply) : await porkbun(domain, token, apply);
    console.log(`${domain}: ${result}`);
  }
}
