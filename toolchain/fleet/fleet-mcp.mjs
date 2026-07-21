#!/usr/bin/env node
// fleet-mcp.mjs — a single source of truth for "what machines do I have access
// to, their capabilities, designations, and live status." Any agent (Claude
// Code, or a hermes-agent instance) can discover the fleet by name over MCP.
//
// It merges STATIC registry data (from the private vault) with LIVE liveness
// (from `tailscale status --json`, matched by magic-DNS short name). The code is
// public-safe (slab/toolchain style); the machine DATA — IPs, ssh keys, roles —
// stays in the vault. Point $FLEET_MACHINES at the file; nothing sensitive is
// baked into this source.
//
// Tools:
//   fleet_list                    — every machine: name, designation, online?, one-line caps
//   fleet_machine(name)           — full detail for one machine + live status
//   fleet_find(capability)        — which machines advertise a capability
//   fleet_designations            — the controlled vocabularies (designations + capabilities)
//   fleet_cleaner                 — run/report the local Mac's safe Cleaner
//
// Hand-rolled JSON-RPC over stdio (newline-delimited), matching the house style
// of slab/bin/frame-mcp.mjs + puppet-mcp.mjs — no SDK, node builtins only.
import { execFile } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { httpPort, serveHttp, serveStdio } from "../mcp/http-front.mjs";

const HOME = homedir();
// Prefer the normalized/proposal file, fall back to canonical. Override with env.
const CANDIDATES = [
  process.env.FLEET_MACHINES,
  join(HOME, "aesthetic-computer-vault", "machines.normalized.json"),
  join(HOME, "aesthetic-computer-vault", "machines.json"),
].filter(Boolean);

const TAILSCALE_BINS = [
  process.env.TAILSCALE_BIN,
  "/Applications/Tailscale.app/Contents/MacOS/Tailscale",
  "tailscale",
].filter(Boolean);

const CLEANER_BINS = [
  join(HOME, ".local/bin/cleaner"),
  join(HOME, "aesthetic-computer/toolchain/macos/cleaner.sh"),
];

// ── static registry ─────────────────────────────────────────────────────────
function registryPath() {
  return CANDIDATES.find((p) => existsSync(p)) || null;
}
function loadRegistry() {
  const path = registryPath();
  if (!path) throw new Error(`no machine registry found (looked in: ${CANDIDATES.join(", ")})`);
  const data = JSON.parse(readFileSync(path, "utf8"));
  return { path, machines: data.machines || {}, schema: data._schema || null };
}

// A machine's tailnet short name: explicit tailscale.name, else status.key, else
// the machine key itself (many keys already match the tailnet name).
function tailKey(name, m) {
  return m.tailscale?.name || m.status?.key || name;
}

// ── live status via tailscale ───────────────────────────────────────────────
function tailscaleStatus() {
  return new Promise((resolve) => {
    const tryBin = (i) => {
      if (i >= TAILSCALE_BINS.length) return resolve(null); // no tailscale — degrade gracefully
      execFile(TAILSCALE_BINS[i], ["status", "--json"], { timeout: 8000, maxBuffer: 8 * 1024 * 1024 }, (err, stdout) => {
        if (err || !stdout) return tryBin(i + 1);
        try {
          const j = JSON.parse(stdout);
          const nodes = {};
          const short = (p) => (p.DNSName || p.HostName || "").split(".")[0];
          const add = (p, isSelf) => {
            if (!p) return;
            const k = short(p);
            if (!k) return;
            nodes[k] = {
              online: isSelf ? true : !!p.Online,
              self: !!isSelf,
              os: p.OS || null,
              ip: (p.TailscaleIPs || [])[0] || null,
              lastSeen: p.LastSeen && !p.LastSeen.startsWith("0001") ? p.LastSeen : null,
            };
          };
          add(j.Self, true);
          for (const p of Object.values(j.Peer || {})) add(p, false);
          resolve(nodes);
        } catch {
          tryBin(i + 1);
        }
      });
    };
    tryBin(0);
  });
}

function liveFor(name, m, nodes) {
  const src = m.status?.source;
  if (src !== "tailscale" || !nodes) {
    return { source: src || "none", online: null, note: src && src !== "tailscale" ? `liveness via ${src} (not probed here)` : "no tailnet liveness source" };
  }
  const node = nodes[tailKey(name, m)];
  if (!node) return { source: "tailscale", online: null, note: `tailnet node '${tailKey(name, m)}' not found in status` };
  return { source: "tailscale", online: node.online, self: node.self, ip: node.ip, lastSeen: node.lastSeen };
}

function statusGlyph(live) {
  if (live.online === true) return live.self ? "🟢(self)" : "🟢";
  if (live.online === false) return "⚪";
  return "❔";
}

// ── tool implementations ────────────────────────────────────────────────────
async function toolList() {
  const { machines, path } = loadRegistry();
  const nodes = await tailscaleStatus();
  const lines = [`fleet (${Object.keys(machines).length} machines) — source: ${path}`, ""];
  // group by designation for legibility
  const groups = {};
  for (const [name, m] of Object.entries(machines)) (groups[m.designation || "unclassified"] ||= []).push([name, m]);
  for (const [designation, entries] of Object.entries(groups)) {
    lines.push(`── ${designation} ──`);
    for (const [name, m] of entries) {
      const live = liveFor(name, m, nodes);
      const caps = (m.capabilities || []).join(",") || "—";
      const flag = m._review ? " ⚠review" : "";
      lines.push(`  ${statusGlyph(live)} ${(m.emoji || "").padEnd(2)} ${name} — [${caps}]${flag}`);
    }
    lines.push("");
  }
  return [{ type: "text", text: lines.join("\n").trimEnd() }];
}

async function toolMachine({ name }) {
  if (!name) throw new Error("`name` is required (see fleet_list)");
  const { machines } = loadRegistry();
  const m = machines[name] || Object.entries(machines).find(([k, v]) => v.tailscale?.name === name || k.toLowerCase() === name.toLowerCase())?.[1];
  if (!m) throw new Error(`unknown machine: ${name} — see fleet_list`);
  const nodes = await tailscaleStatus();
  const live = liveFor(name, m, nodes);
  return [{ type: "text", text: JSON.stringify({ ...m, _live: live }, null, 2) }];
}

async function toolFind({ capability }) {
  if (!capability) throw new Error("`capability` is required (see fleet_designations for the vocabulary)");
  const cap = capability.toLowerCase();
  const { machines } = loadRegistry();
  const nodes = await tailscaleStatus();
  const hits = [];
  for (const [name, m] of Object.entries(machines)) {
    const caps = (m.capabilities || []).map((c) => c.toLowerCase());
    if (!caps.includes(cap)) continue;
    const live = liveFor(name, m, nodes);
    hits.push(`  ${statusGlyph(live)} ${name} (${m.designation || "?"}) — ${m.fleetRole || m.role || ""}`.trimEnd());
  }
  const head = hits.length ? `machines with capability '${capability}':` : `no machines advertise capability '${capability}' — see fleet_designations for valid tags.`;
  return [{ type: "text", text: [head, ...hits].join("\n") }];
}

async function toolDesignations() {
  const { schema } = loadRegistry();
  if (!schema) return [{ type: "text", text: "registry has no _schema block (canonical machines.json?) — run toolchain/fleet/normalize-machines.mjs to generate the normalized file with vocabularies." }];
  const L = ["DESIGNATIONS (primary fleet role, one per machine):"];
  for (const [k, v] of Object.entries(schema.designations || {})) L.push(`  ${k} — ${v}`);
  L.push("", "CAPABILITIES (composable tags):");
  for (const [k, v] of Object.entries(schema.capabilities || {})) L.push(`  ${k} — ${v}`);
  return [{ type: "text", text: L.join("\n") }];
}

async function toolCleaner({ apply = true, thinSnapshots = false } = {}) {
  if (process.platform !== "darwin") throw new Error("Cleaner is currently defined for fleet Macs only.");
  const bin = CLEANER_BINS.find((path) => existsSync(path));
  if (!bin) throw new Error("Cleaner is not installed and no repository copy was found.");
  const args = [];
  if (apply) args.push("--apply");
  if (thinSnapshots) {
    if (!apply) throw new Error("thinSnapshots requires apply=true");
    args.push("--thin-snapshots");
  }
  return new Promise((resolve, reject) => {
    execFile(bin, args, { timeout: 10 * 60_000, maxBuffer: 8 * 1024 * 1024 }, (error, stdout, stderr) => {
      if (error) return reject(new Error((stderr || stdout || error.message).trim()));
      resolve([{ type: "text", text: stdout.trim() }]);
    });
  });
}

const TOOLS = [
  {
    name: "fleet_list",
    description: "List every machine @jeffrey has access to, grouped by fleet designation, with a live online/offline glyph (🟢 online · ⚪ offline · ❔ unknown) and a one-line capability summary. The single source of truth for 'what machines do I have?'. Merges the private vault registry with live `tailscale status`.",
    inputSchema: { type: "object", properties: {} },
  },
  {
    name: "fleet_machine",
    description: "Full detail for ONE machine (hardware, os, ssh, repoPath, designation, capabilities, notes) plus its live tailnet status. Accepts the registry key or the tailnet short name. Names come from fleet_list.",
    inputSchema: { type: "object", properties: { name: { type: "string", description: "Machine name, e.g. poorslice, jastow, neo." } }, required: ["name"] },
  },
  {
    name: "fleet_find",
    description: "Find which machines can do X — returns every machine advertising a given capability tag (e.g. gpu, mlx, chromium-pool, build-macos, always-on), with live status. Use fleet_designations for the capability vocabulary.",
    inputSchema: { type: "object", properties: { capability: { type: "string", description: "A capability tag, e.g. gpu, mlx, macos-automation, chromium-pool, ffmpeg-render, always-on, git-remote." } }, required: ["capability"] },
  },
  {
    name: "fleet_designations",
    description: "Explain the controlled vocabularies: the fleet designations (agent-endpoint, compute-node, control, build, service, display, legacy) and the capability tags. Read this to know what fleet_find accepts.",
    inputSchema: { type: "object", properties: {} },
  },
  {
    name: "fleet_cleaner",
    description: "Call the Cleaner on the local fleet Mac. By default applies the canonical safe cache cleanup; set apply=false for a report only. It protects repositories, node_modules, Downloads, models, agent state, and active-app caches. APFS snapshot thinning is separately opt-in.",
    inputSchema: { type: "object", properties: {
      apply: { type: "boolean", description: "Apply safe cleanup (default true); false reports only." },
      thinSnapshots: { type: "boolean", description: "Also request APFS local-snapshot thinning (default false, requires apply)." },
    } },
  },
];

const HANDLERS = {
  fleet_list: toolList,
  fleet_machine: toolMachine,
  fleet_find: toolFind,
  fleet_designations: toolDesignations,
  fleet_cleaner: toolCleaner,
};

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return { jsonrpc: "2.0", id, result: { protocolVersion: "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "fleet-mcp", version: "1.0.0" } } };
      case "initialized":
      case "notifications/initialized":
        return null;
      case "ping":
        return { jsonrpc: "2.0", id, result: {} };
      case "tools/list":
        return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const fn = HANDLERS[params?.name];
        if (!fn) throw new Error(`Unknown tool: ${params?.name}`);
        const content = await fn(params.arguments || {});
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default:
        return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call")
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

// Allow a quick CLI smoke test: `node fleet-mcp.mjs list|find <cap>|machine <name>`.
if (process.argv[2] && process.argv[2] !== "--http") {
  const [cmd, arg] = process.argv.slice(2);
  const map = { list: () => toolList(), find: () => toolFind({ capability: arg }), machine: () => toolMachine({ name: arg }), designations: () => toolDesignations(), cleaner: () => toolCleaner({ apply: arg !== "report" }) };
  const fn = map[cmd];
  if (!fn) {
    console.error("usage: fleet-mcp.mjs [list | find <capability> | machine <name> | designations | cleaner [report]]");
    process.exit(1);
  }
  fn().then((c) => console.log(c.map((x) => x.text).join("\n"))).catch((e) => { console.error(String(e.message || e)); process.exit(1); });
} else {
  const port = httpPort(process.argv, 7776);
  if (port) serveHttp({ handleMessage, port, banner: "🛰  fleet-mcp shared daemon" });
  else serveStdio({ handleMessage, banner: "🛰  fleet-mcp server started (fleet_list, fleet_machine, fleet_find, fleet_designations, fleet_cleaner)" });
}
