#!/usr/bin/env node
// tailnet-prune.mjs — list (and, with --delete, remove) stale tailnet devices.
//
// A device is a prune candidate when ALL of these hold:
//   1. it is offline,
//   2. it was last seen more than --days ago (default 30),
//   3. no registry machine claims it as its tailnet name (tailscale.name /
//      status.key / the machine key itself — the same lookup fleet-mcp.mjs uses).
//
// Reads the admin API key from $TAILSCALE_API_KEY, else the vault's
// fleet/tailscale.env (unlock the vault first). Dry run by default.
//
//   node toolchain/fleet/tailnet-prune.mjs             # show candidates
//   node toolchain/fleet/tailnet-prune.mjs --delete    # remove them
//   node toolchain/fleet/tailnet-prune.mjs --days 60   # stricter staleness
//
// House style: node builtins only, no deps.
import { existsSync, readFileSync } from "node:fs";
import { join, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const VAULT = join(REPO, "aesthetic-computer-vault");
const args = process.argv.slice(2);
const DELETE = args.includes("--delete");
const daysIdx = args.indexOf("--days");
const DAYS = daysIdx >= 0 ? Number(args[daysIdx + 1]) : 30;

function loadEnv(file) {
  if (!existsSync(file)) return {};
  const out = {};
  for (const line of readFileSync(file, "utf8").split("\n")) {
    const m = line.match(/^\s*([A-Z0-9_]+)\s*=\s*(.*)\s*$/);
    if (m) out[m[1]] = m[2].replace(/^["']|["']$/g, "");
  }
  return out;
}

const env = { ...loadEnv(join(VAULT, "fleet", "tailscale.env")), ...process.env };
const KEY = env.TAILSCALE_API_KEY;
const TAILNET = env.TAILSCALE_TAILNET || "-";
if (!KEY) {
  console.error("no TAILSCALE_API_KEY — set it or unlock the vault (fleet/tailscale.env)");
  process.exit(2);
}

// Registry: which tailnet names are spoken for.
const REG_CANDIDATES = [
  process.env.FLEET_MACHINES,
  join(VAULT, "machines.normalized.json"),
  join(VAULT, "machines.json"),
].filter(Boolean);
const regPath = REG_CANDIDATES.find((p) => existsSync(p));
const machines = regPath ? JSON.parse(readFileSync(regPath, "utf8")).machines || {} : {};
const claimed = new Map(); // tailnet short name -> registry key
for (const [k, m] of Object.entries(machines)) {
  // Same resolution as fleet-mcp.mjs: an explicit tailnet name wins, else the
  // registry key. A machine re-enrolled under a new name (chicken -> chicken-1)
  // therefore stops claiming its old node, which is exactly what makes the old
  // node prunable.
  const n = m.tailscale?.name || (m.status?.source === "tailscale" ? m.status?.key : null) || k;
  claimed.set(String(n).toLowerCase(), k);
}

const api = (path, init = {}) =>
  fetch(`https://api.tailscale.com/api/v2${path}`, {
    ...init,
    headers: { Authorization: `Bearer ${KEY}`, ...(init.headers || {}) },
  });

const res = await api(`/tailnet/${encodeURIComponent(TAILNET)}/devices?fields=default`);
if (!res.ok) {
  console.error(`device list failed: ${res.status} ${await res.text()}`);
  process.exit(1);
}
const { devices } = await res.json();
const now = Date.now();
const short = (d) => (d.name || d.hostname || "").split(".")[0].toLowerCase();

const rows = devices.map((d) => {
  const name = short(d);
  const lastSeen = d.lastSeen ? new Date(d.lastSeen) : null;
  // Clocks skew; clamp so a device seen "in the future" reads as just now.
  const ageMs = lastSeen ? Math.max(0, now - lastSeen.getTime()) : null;
  const ageDays = ageMs === null ? null : Math.floor(ageMs / 86400000);
  // Tailscale's API has no plain "online" flag; a device seen in the last
  // few minutes is treated as online.
  const online = ageMs !== null && ageMs < 5 * 60 * 1000;
  const owner = claimed.get(name) || null;
  const stale = !online && ageDays !== null && ageDays > DAYS && !owner;
  return { id: d.id, name, os: d.os, lastSeen: d.lastSeen, ageDays, online, owner, stale };
});

const pad = (s, n) => String(s ?? "").padEnd(n);
console.log(`${pad("device", 30)}${pad("last seen", 12)}${pad("registry", 18)}verdict`);
for (const r of rows.sort((a, b) => a.name.localeCompare(b.name))) {
  const seen = r.online ? "online" : r.ageDays === null ? "never" : `${r.ageDays}d ago`;
  const verdict = r.stale ? "PRUNE" : r.owner ? "keep (registry)" : r.online ? "keep (online)" : `keep (< ${DAYS}d)`;
  console.log(`${pad(r.name, 30)}${pad(seen, 12)}${pad(r.owner || "-", 18)}${verdict}`);
}

const prune = rows.filter((r) => r.stale);
if (!prune.length) {
  console.log("\nnothing to prune");
  process.exit(0);
}
if (!DELETE) {
  console.log(`\n${prune.length} candidate(s). Re-run with --delete to remove them.`);
  process.exit(0);
}
for (const r of prune) {
  const del = await api(`/device/${r.id}`, { method: "DELETE" });
  console.log(`${del.ok ? "deleted" : `FAILED ${del.status}`}  ${r.name} (${r.id})`);
}
