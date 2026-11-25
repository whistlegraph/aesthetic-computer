#!/usr/bin/env node
// room-mcp.mjs — what shares a fleet Mac's room: the LAN and Bluetooth.
//
// The wallpaper's feeder already probes the room every 30 s and writes it to
// ~/.local/share/blueberry-wallpaper/fishery.json as `local`
// (toolchain/analytics/ac-local-probe.mjs); the wallpaper app's CoreBluetooth
// scan writes radio.json beside it. This server reads those, so a session gets
// the same picture the wallpaper draws without a second ping sweep. On this
// Mac a stale or missing reading is probed fresh; on another Mac it is only
// read over ssh, never probed, so asking never sets off a sweep elsewhere.
//
// Names and addresses are read at call time and never written anywhere.

import { execFile } from "node:child_process";
import { readFile } from "node:fs/promises";
import { homedir, hostname } from "node:os";
import { join } from "node:path";
import { promisify } from "node:util";
import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";

const pexec = promisify(execFile);
const DIR = join(homedir(), ".local/share/blueberry-wallpaper");
const HOST = hostname().split(".")[0].toLowerCase();
// The feeder ticks every 30 s; past three ticks the reading has stopped.
const FRESH_S = 90;

const now = () => Date.now() / 1000;
const ago = (t) => {
  if (!t) return "";
  const s = Math.max(0, now() - t);
  return s < 90 ? `${Math.round(s)}s` : s < 5400 ? `${Math.round(s / 60)}m` : s < 172800 ? `${Math.round(s / 3600)}h` : `${Math.round(s / 86400)}d`;
};
const cell = (v) => String(v ?? "").replace(/\|/g, "/").replace(/\n/g, " ");
const table = (head, rows) => [`| ${head.join(" | ")} |`, `|${head.map(() => "---").join("|")}|`, ...rows.map((r) => `| ${r.map(cell).join(" | ")} |`)].join("\n");

const parse = (text) => { try { return JSON.parse(text); } catch { return null; } };
const readLocal = async (name) => parse(await readFile(join(DIR, name), "utf8").catch(() => ""));
async function readRemote(machine, name) {
  // Fixed path, no shell from the caller: the machine name is checked below.
  const { stdout } = await pexec("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=6", machine, `cat ~/.local/share/blueberry-wallpaper/${name}`], { timeout: 15000, maxBuffer: 16 * 1024 * 1024 }).catch(() => ({ stdout: "" }));
  return parse(stdout);
}

const isHere = (machine) => !machine || machine.toLowerCase() === HOST || machine === "localhost";

function render(machine, local, radio, source) {
  const out = [];
  const net = local?.network;
  out.push(`## ${machine} — ${source}`);
  if (!local) out.push("No room reading.");
  else if (!net) out.push("Not on a LAN (no default route).");
  else {
    out.push(`${net.iface} ${net.self} via ${net.gateway}/${net.bits} · ${net.answered} answered ping · read ${ago(local.observedAt)} ago`);
    const rows = (local.devices || []).map((d) => [
      d.kind, d.short || d.name, d.ip || "", d.ms ?? "",
      d.online ? "online" : `away ${ago(d.lastSeen)}`, d.new ? "new" : "", d.model || "",
    ]);
    out.push("", table(["kind", "name", "ip", "ms", "state", "new", "model"], rows));
  }
  const bt = local?.bluetooth;
  out.push("", "### Bluetooth");
  if (!bt) out.push("No paired-device reading.");
  else {
    const on = bt.devices.filter((d) => d.connected);
    out.push(`${bt.power ? "on" : "OFF"} · ${on.length} connected of ${bt.devices.length} paired`);
    if (on.length) out.push("", table(["kind", "name", "battery"], on.map((d) => [d.kind, d.name, d.battery ? `${d.battery}%` : ""])));
    const off = bt.devices.filter((d) => !d.connected).map((d) => d.name);
    if (off.length) out.push("", `Paired, not connected: ${off.join(", ")}`);
  }
  out.push("", "### Nearby (Bluetooth scan)");
  if (!radio) out.push("No scan yet (the wallpaper app writes radio.json once Bluetooth is allowed).");
  else {
    const near = (radio.nearby || []).slice().sort((a, b) => (b.rssi ?? -999) - (a.rssi ?? -999));
    out.push(`${radio.power === false ? "radio OFF · " : ""}${near.length} nearby · scanned ${ago(radio.observedAt)} ago`);
    if (near.length) out.push("", table(["name", "vendor", "rssi", "seen"], near.map((d) => [d.name || "—", d.vendor || "", d.rssi ?? "", ago(d.lastSeen)])));
  }
  return out.join("\n");
}

async function roomDevices({ machine, refresh } = {}) {
  if (machine && !/^[A-Za-z0-9._-]+$/.test(machine)) throw new Error(`not a machine name: ${machine}`);
  if (isHere(machine)) {
    const feed = await readLocal("fishery.json");
    let local = feed?.local, source = "from the wallpaper feed";
    if (refresh || !local || now() - (local.observedAt || 0) > FRESH_S) {
      // Imported late: the probe module is only needed when the feed is stale.
      const { probeLocal } = await import("../analytics/ac-local-probe.mjs");
      local = await probeLocal({});
      source = refresh ? "probed fresh (asked)" : "probed fresh (feed stale or missing)";
    }
    return render(HOST, local, await readLocal("radio.json"), source);
  }
  // A site this Mac's feeder maps (ac-sites-probe.mjs) answers from its kept map.
  const site = ((await readLocal("fishery.json"))?.sites || []).find((x) => x.machine === machine);
  if (site?.local) {
    const how = site.stale ? `KEPT MAP from ${ago(site.at)} ago (site unreachable: ${site.error || "no answer"})` : `site probe, ${ago(site.at)} ago`;
    return render(site.place ? `${machine} (${site.place})` : machine, site.local, null, how);
  }
  const [feed, radio] = await Promise.all([readRemote(machine, "fishery.json"), readRemote(machine, "radio.json")]);
  const local = feed?.local;
  if (!local) return `## ${machine}\nNo room reading there: its feeder does not write \`local\` yet (or ssh failed). Nothing was probed remotely.`;
  const stale = now() - (local.observedAt || 0) > FRESH_S;
  return render(machine, local, radio, stale ? `STALE feed, last read ${ago(local.observedAt)} ago` : "from its wallpaper feed");
}

// 🗺 Every room at once: this Mac's, then each mapped site's, one line each.
async function roomMap() {
  const feed = await readLocal("fishery.json");
  const rows = [];
  const line = (where, l, how) => {
    const named = (l?.devices || []).filter((d) => d.kind !== "unknown" && d.online);
    const bt = l?.bluetooth?.devices || [];
    rows.push([where, l?.network?.ssid || "", l?.network ? `${l.network.answered}` : "", named.map((d) => d.short || d.name).join(", "),
      `${bt.filter((d) => d.connected).length}/${bt.length}`, how]);
  };
  if (feed?.local) line(HOST, feed.local, `${ago(feed.local.observedAt)} ago`);
  for (const s of feed?.sites || []) line(s.place ? `${s.machine} (${s.place})` : s.machine, s.local, s.local ? `${s.stale ? "KEPT " : ""}${ago(s.at)} ago` : `never reached: ${s.error}`);
  if (!rows.length) return "No room feed on this Mac.";
  return [`## Rooms seen from ${HOST}`, table(["room", "wi-fi", "answered", "named devices online", "bt conn/paired", "as of"], rows)].join("\n");
}

async function roomHistory({ hours = 24 } = {}) {
  const seen = await readLocal("local-seen.json");
  if (!seen?.networks) return "No device history on this Mac yet.";
  const since = now() - hours * 3600, gone = now() - 300;
  const out = [`## ${HOST} — last ${hours} h`];
  for (const [key, n] of Object.entries(seen.networks)) {
    const devices = Object.values(n.devices || {}).filter((d) => d.kind !== "unknown" || d.first >= since);
    // A network first known inside the window: everything on it "arrived" then, which says nothing.
    const young = n.first >= since;
    const arrived = young ? [] : devices.filter((d) => d.first >= since).sort((a, b) => b.first - a.first);
    // Unnamed addresses come and go with DHCP; only named things are said to leave.
    const left = devices.filter((d) => d.kind !== "unknown" && d.last && d.last >= since && d.last < gone).sort((a, b) => b.last - a.last);
    out.push("", `### ${key}${young ? ` (first seen ${ago(n.first)} ago)` : ""}`);
    if (!arrived.length && !left.length) { out.push("No arrivals or departures."); continue; }
    if (arrived.length) out.push("", "Arrived:", table(["kind", "name", "ip", "when"], arrived.map((d) => [d.kind, d.short || d.name, d.ip || "", `${ago(d.first)} ago`])));
    if (left.length) out.push("", "Left:", table(["kind", "name", "ip", "last seen"], left.map((d) => [d.kind, d.short || d.name, d.ip || "", `${ago(d.last)} ago`])));
  }
  return out.join("\n");
}

const TOOLS = [
  {
    name: "room_devices",
    description: "What is on a fleet Mac's LAN and Bluetooth right now: every device answering on the subnet (named and typed from Bonjour, or by address), paired Bluetooth devices, and nearby Bluetooth from the wallpaper's scan. Defaults to this Mac. Another Mac (e.g. blueberry, panda) is read over ssh from its wallpaper feed, never probed.",
    inputSchema: {
      type: "object",
      properties: {
        machine: { type: "string", description: "Fleet machine (ssh name). Omit for this Mac." },
        refresh: { type: "boolean", description: "This Mac only: probe now instead of reading the feed (about 8 s)." },
      },
    },
  },
  {
    name: "room_map",
    description: "Every room at once: this Mac's LAN plus each site its wallpaper feeder maps over ssh (a Linux box at home, a laptop elsewhere …), with Wi-Fi name, device counts and what is online. Unreachable sites show their kept map's age.",
    inputSchema: { type: "object", properties: {} },
  },
  {
    name: "room_history",
    description: "Devices that arrived on or left this Mac's networks in the last N hours, from the probe's first/last-seen book.",
    inputSchema: { type: "object", properties: { hours: { type: "number", description: "Window in hours (default 24)" } } },
  },
];

async function callTool(name, args = {}) {
  const text = name === "room_devices" ? await roomDevices(args)
    : name === "room_history" ? await roomHistory(args)
    : name === "room_map" ? await roomMap()
    : (() => { throw new Error(`Unknown tool: ${name}`); })();
  return [{ type: "text", text }];
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: params?.protocolVersion || "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "room-mcp", version: "1.0.0" },
          },
        };
      case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": return { jsonrpc: "2.0", id, result: { content: await callTool(params?.name, params?.arguments) } };
      default: return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 0);
if (port) serveHttp({ handleMessage, port, banner: "📶 room-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "📶 room-mcp started (room_devices, room_history)" });
