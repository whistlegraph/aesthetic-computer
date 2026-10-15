#!/usr/bin/env node
// Local domain operations with credentials resolved inside the server.
import { isIP } from "node:net";
import { pathToFileURL } from "node:url";
import * as cf from "./cloudflare.mjs";
import * as pb from "./porkbun.mjs";
import { vaultStatus } from "../vault/credentials.mjs";
import { httpPort, serveHttp, serveStdio } from "../mcp/http-front.mjs";

const domainSchema = { type: "string", description: "Bare zone name, e.g. kidlisp.com. No URL or path." };
const schema = (properties, required = []) => ({ type: "object", properties, required, additionalProperties: false });
const readOnly = { readOnlyHint: true, destructiveHint: false, idempotentHint: true };
export const TOOLS = [
  {
    name: "vault_status",
    description: "Discover registered MCP credentials in aesthetic-computer-vault: Cloudflare, Porkbun, OpenAI/Frame/Illy, fal, Exa web search, Slack, Instagram, X, Bluesky, and Apple notarization. Reports availability and source only; never returns keys. Each provider tool automatically uses its own registered credentials. No recursive client-folder scans.",
    inputSchema: schema({}), annotations: { ...readOnly, openWorldHint: false },
  },
  {
    name: "domains_vault_status",
    description: "Discover Aesthetic Computer's Cloudflare and Porkbun credential availability in aesthetic-computer-vault. Reports ready/missing/locked and the source file, never keys. All domains tools use these credentials automatically; do not ask the user to paste keys. Client accounts are not searched.",
    inputSchema: schema({}), annotations: { ...readOnly, openWorldHint: false },
  },
  {
    name: "domains_inspect",
    description: "Inspect a Cloudflare zone and its DNS records using vault credentials automatically. Compare Cloudflare's assigned nameservers against public DNS; an active zone does not prove correct registrar delegation. Also reads registry registrar details for .com/.net. No changes.",
    inputSchema: schema({ domain: domainSchema }, ["domain"]), annotations: readOnly,
  },
  {
    name: "domains_dns_set",
    description: "Preview or apply one Cloudflare A, AAAA, or CNAME record using vault credentials automatically. Defaults to preview; apply:true writes only the named record. Inspect first and apply only within the user's requested DNS change. Preserves existing TTL/proxy settings unless specified. Does not change registrar delegation, buy domains, or deploy code.",
    inputSchema: schema({ domain: domainSchema, name: { type: "string", description: "Full DNS name within the zone, or @ for the apex." },
      type: { type: "string", enum: ["A", "AAAA", "CNAME"] }, content: { type: "string" },
      recordId: { type: "string", description: "Required when multiple records have the same name and type." },
      ttl: { type: "integer", description: "1 for automatic, otherwise 60–86400 seconds." },
      proxied: { type: "boolean" }, apply: { type: "boolean", default: false } }, ["domain", "name", "type", "content"]),
    annotations: { readOnlyHint: false, destructiveHint: true, idempotentHint: false },
  },
  {
    name: "domains_porkbun_nameservers",
    description: "Read registrar nameservers for a domain held at Porkbun, using its vault keys automatically. Porkbun credentials cannot manage domains at other registrars. No changes.",
    inputSchema: schema({ domain: domainSchema }, ["domain"]), annotations: readOnly,
  },
];

function domainName(value) {
  if (typeof value !== "string") throw new Error("A bare domain name is required");
  const name = value.toLowerCase().replace(/\.$/, "");
  if (name.length > 253 || !name.includes(".") || isIP(name) || !name.split(".").every(
    label => /^[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?$/.test(label))) {
    throw new Error("Expected a bare ASCII domain name, without a URL or path");
  }
  return name;
}

function validateArguments(name, args) {
  const definition = TOOLS.find(tool => tool.name === name)?.inputSchema;
  if (!definition) throw new Error("Unknown domains tool");
  if (!args || typeof args !== "object" || Array.isArray(args)) throw new Error("Expected an arguments object");
  for (const field of definition.required) if (!(field in args)) throw new Error(`Missing ${field}`);
  for (const [key, value] of Object.entries(args)) {
    const property = definition.properties[key];
    if (!property) throw new Error("Unexpected argument");
    if (property.type === "integer" ? !Number.isInteger(value) : typeof value !== property.type) throw new Error(`Invalid ${key}`);
    if (property.enum && !property.enum.includes(value)) throw new Error(`Invalid ${key}`);
  }
}

async function publicJson(url, headers = {}) {
  const response = await fetch(url, { headers, signal: AbortSignal.timeout(15000) });
  if (!response.ok) throw new Error(`Public lookup failed (HTTP ${response.status})`);
  return response.json();
}

export async function publicDelegation(domain) {
  const answer = await publicJson(`https://cloudflare-dns.com/dns-query?name=${domain}&type=NS`, { accept: "application/dns-json" });
  if (answer.Status !== 0) throw new Error(`Public DNS status ${answer.Status}`);
  return (answer.Answer || []).filter(row => row.type === 2).map(row => row.data.toLowerCase().replace(/\.$/, "")).sort();
}

export async function registryInfo(domain) {
  const tld = domain.split(".").at(-1);
  if (!["com", "net"].includes(tld)) return { status: "not_checked", reason: "Registry lookup currently supports .com and .net" };
  const source = `https://rdap.verisign.com/${tld}/v1/domain/${domain}`;
  const data = await publicJson(source);
  const registrar = data.entities?.find(entity => entity.roles?.includes("registrar"));
  return { status: "checked", source,
    registrar: registrar?.vcardArray?.[1]?.find(row => row[0] === "fn")?.[3] || null,
    nameservers: (data.nameservers || []).map(row => row.ldhName.toLowerCase()).sort() };
}

const recordView = ({ id, name, type, content, ttl, proxied }) => ({ id, name, type, content, ttl, proxied });

// Dependencies are injectable for isolated tests; MCP clients cannot override them.
export function createHandler({ cloudflare = cf, porkbun = pb, status = vaultStatus,
  delegation = publicDelegation, registry = registryInfo } = {}) {
  const handlers = {
    vault_status: () => status(),
    domains_vault_status: () => ({ services: status().services.filter(row => ["cloudflare", "porkbun"].includes(row.service)) }),
    async domains_inspect({ domain }) {
      domain = domainName(domain);
      const zone = await cloudflare.zone(domain);
      const [publicNS, registrar] = await Promise.all([
        delegation(domain).then(nameservers => ({ status: "checked", nameservers })).catch(error => ({ status: "unavailable", error: error.message })),
        registry(domain).catch(error => ({ status: "unavailable", error: error.message })),
      ]);
      const expected = [...(zone?.name_servers || [])].map(name => name.toLowerCase().replace(/\.$/, "")).sort();
      return { domain, checkedAt: new Date().toISOString(),
        cloudflare: zone ? { id: zone.id, status: zone.status, nameservers: expected,
          records: (await cloudflare.records(zone.id)).map(recordView) } : null,
        publicDNS: publicNS, registry: registrar,
        delegationMatches: !zone || publicNS.status !== "checked" ? null : JSON.stringify(expected) === JSON.stringify(publicNS.nameservers),
        delegationNote: "Parent delegation is controlled at the registrar. Changing apex NS records in Cloudflare does not update it." };
    },
    async domains_dns_set(args) {
      const domain = domainName(args.domain);
      const name = args.name === "@" ? domain : domainName(args.name);
      if (name !== domain && !name.endsWith(`.${domain}`)) throw new Error("Record name must be within the requested zone");
      const { type, content, recordId } = args;
      if ((type === "A" && isIP(content) !== 4) || (type === "AAAA" && isIP(content) !== 6)) throw new Error("Record content must be an IP address of the requested type");
      const target = type === "CNAME" ? domainName(content) : content;
      if (args.ttl !== undefined && args.ttl !== 1 && (args.ttl < 60 || args.ttl > 86400)) throw new Error("TTL must be 1 or 60–86400");
      const zone = await cloudflare.zone(domain);
      if (!zone) throw new Error("No Cloudflare zone found for this domain");
      const records = await cloudflare.records(zone.id);
      const matches = records.filter(record => record.name === name && record.type === type);
      let before;
      if (recordId) {
        before = matches.find(record => record.id === recordId);
        if (!before) throw new Error("recordId does not match the zone, name, and type");
      } else {
        if (matches.length > 1) throw new Error("Multiple matching records: inspect and supply recordId");
        before = matches[0];
      }
      if (records.some(record => record.name === name && record.type !== type && (type === "CNAME" || record.type === "CNAME"))) throw new Error("CNAME conflicts with existing records; inspect before changing record types");
      const after = { name, type, content: target, ttl: args.ttl ?? before?.ttl ?? 300,
        proxied: args.proxied ?? before?.proxied ?? false };
      // Cloudflare enforces automatic TTL for proxied records.
      if (after.proxied) after.ttl = 1;
      const unchanged = before && Object.keys(after).every(key => before[key] === after[key]);
      const action = unchanged ? "unchanged" : before ? "update" : "create";
      const preview = { domain, action, before: before ? recordView(before) : null, after };
      if (!args.apply || unchanged) return { ...preview, applied: false };
      const written = before ? await cloudflare.updateRecord(zone.id, before.id, after)
        : await cloudflare.createRecord(zone.id, name, target, { type, ttl: after.ttl, proxied: after.proxied });
      return { ...preview, applied: true, record: recordView(written) };
    },
    async domains_porkbun_nameservers({ domain }) {
      domain = domainName(domain);
      return { domain, nameservers: (await porkbun.getNs(domain)).ns };
    },
  };

  return async function handleMessage(message, context) {
    const { id, method, params } = message;
    if (id === undefined || id === null) return null;
    try {
      // Shared daemon is for local agents. Reject browser-origin requests and
      // hostile Host headers before touching credentials (CSRF / DNS rebinding).
      if (context && (context.headers.origin || !/^(127\.0\.0\.1|localhost|\[::1\])(?::\d+)?$/.test(context.headers.host || ""))) {
        return { jsonrpc: "2.0", id, error: { code: -32000, message: "Local agent clients only" } };
      }
      let result;
      if (method === "initialize") result = { protocolVersion: "2024-11-05", capabilities: { tools: {} },
        serverInfo: { name: "ac-domains", version: "1.0.0" },
        instructions: "Use vault-backed domain tools automatically for AC DNS work. Credentials remain inside the server. Inspect delegation before proposing a repair; Cloudflare DNS access does not imply registrar access." };
      else if (method === "ping") result = {};
      else if (method === "tools/list") result = { tools: TOOLS };
      else if (method === "tools/call") {
        const args = params?.arguments ?? {};
        validateArguments(params?.name, args);
        const value = await handlers[params.name](args);
        result = { content: [{ type: "text", text: JSON.stringify(value, null, 2) }] };
      } else return { jsonrpc: "2.0", id, error: { code: -32601, message: "Method not found" } };
      return { jsonrpc: "2.0", id, result };
    } catch (error) {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: error.message }] } };
    }
  };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const handleMessage = createHandler();
  const port = httpPort(process.argv, 7782);
  if (port) serveHttp({ handleMessage, port, banner: "domains-mcp" });
  else serveStdio({ handleMessage, banner: "domains-mcp (vault-backed Cloudflare and Porkbun)" });
}
