#!/usr/bin/env node
// AC infra probe: is each public host answering, and how long did it take from
// this machine? Plus who is on the tailnet. One small snapshot per call, for
// the wallpaper's constellation (blueberry-wallpaper/Sources/Fishery.swift),
// carried as the `infra` field of the fishery feed.
//
// Only what any visitor could do: one HTTPS GET and one HEAD per host. No SSH, no
// secrets. Tailnet names are read from the local `tailscale` CLI at run time
// and never written into this repo; addresses are dropped.
//
//   node toolchain/analytics/ac-infra-probe.mjs   one snapshot to stdout
import { execFile } from "node:child_process";
import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";

const TIMEOUT_MS = 10000;
// A host is slow past this warm round trip, unless the whole uplink is (see `medianMs`).
const SLOW_MS = 600;
// One lost request reads as late, not down.
const DOWN_AFTER = 2;

// The hosts toolchain/doctor.mjs checks under "Production" (its Vercel-parked
// ai. and help. left out), plus the health routes each service keeps.
// `parent` is the link drawn: edge → lith → everything lith leans on.
// `health` wants a 200; the rest count as up under the doctor's rule (< 500).
export const HOSTS = [
  { id: "edge", url: "https://aesthetic.computer/.commit-ref", parent: "", health: true, kind: "proxy" },
  { id: "lith", url: "https://aesthetic.computer/rax/health", parent: "edge", health: true, kind: "kernel" },
  { id: "oven", url: "https://oven.aesthetic.computer/papers-build", parent: "lith", health: true, kind: "dev" },
  { id: "session", url: "https://session-server.aesthetic.computer", parent: "lith", kind: "bridge" },
  { id: "relay", url: "https://relay-sfo.aesthetic.computer/health", parent: "lith", health: true, kind: "bridge" },
  { id: "silo", url: "https://silo.aesthetic.computer", parent: "lith", kind: "db" },
  { id: "assets", url: "https://assets.aesthetic.computer", parent: "lith", kind: "db" },
  { id: "papers", url: "https://papers.aesthetic.computer", parent: "lith", kind: "dev" },
  { id: "knot", url: "https://knot.aesthetic.computer", parent: "lith", kind: "dev" },
  { id: "feed", url: "https://feed.aesthetic.computer", parent: "lith", kind: "proxy" },
];

const AGENT = { "user-agent": "ac-infra-probe" };

async function get(host) {
  const t0 = performance.now();
  try {
    const res = await fetch(host.url, { signal: AbortSignal.timeout(TIMEOUT_MS), redirect: "manual", headers: AGENT });
    // Two bodies are worth a glance: what is deployed, and whether the oven is baking.
    const body = await res.text();
    const coldMs = Math.round(performance.now() - t0);
    let note = "";
    if (res.ok && host.id === "edge") note = body.trim().slice(0, 7);
    else if (res.ok && host.id === "oven") note = /"activeJobId":\s*"/.test(body.slice(0, 400)) ? "baking" : "";
    // The GET paid for DNS, TCP and TLS. A HEAD down the same warm connection
    // is the round trip itself; any answer to it counts, whatever the status.
    let ms = coldMs;
    try {
      const t1 = performance.now();
      const head = await fetch(host.url, { method: "HEAD", signal: AbortSignal.timeout(TIMEOUT_MS), redirect: "manual", headers: AGENT });
      await head.arrayBuffer();
      ms = Math.round(performance.now() - t1);
    } catch {}
    return { ok: host.health ? res.status === 200 : res.status > 0 && res.status < 500, ms, coldMs, status: res.status, note };
  } catch (error) {
    const code = error.name === "TimeoutError" || error.name === "AbortError" ? "timeout" : error.cause?.code || error.code || "unreachable";
    return { ok: false, ms: 0, coldMs: Math.round(performance.now() - t0), status: 0, note: String(code).slice(0, 24) };
  }
}

const TAILSCALE = ["/opt/homebrew/bin/tailscale", "/usr/local/bin/tailscale", "/usr/bin/tailscale",
  "/Applications/Tailscale.app/Contents/MacOS/Tailscale"];

function tailscale() {
  const bin = TAILSCALE.find(existsSync);
  if (!bin) return Promise.resolve(null);
  return new Promise(resolve => execFile(bin, ["status", "--json"], { timeout: 5000, maxBuffer: 8 * 1024 * 1024 },
    (error, stdout) => { try { resolve(error ? null : JSON.parse(stdout)); } catch { resolve(null); } }));
}

const short = p => String(p.DNSName || "").split(".")[0] || String(p.HostName || "").toLowerCase().replace(/[^a-z0-9]+/g, "-");
// What the wallpaper prints: the name without the tailnet's own decorations.
const plain = name => name.replace(/(^|-)aesthetic-computer(-|$)/, (m, a, b) => (a && b ? "-" : "")).replace(/-\d$/, "") || name;
const seen = at => { const t = Date.parse(at || ""); return t > 86400000 ? Math.round(t / 1000) : 0; };

/** One snapshot. `state` remembers consecutive failures between calls. */
export async function probeInfra(state = {}) {
  const fails = (state.fails ||= new Map());
  const [results, ts] = await Promise.all([Promise.all(HOSTS.map(get)), tailscale()]);
  const answered = results.filter(r => r.ok).map(r => r.ms).sort((a, b) => a - b);
  const medianMs = answered.length ? answered[answered.length >> 1] : 0;
  const nodes = HOSTS.map((host, i) => {
    const r = results[i];
    const n = r.ok ? 0 : (fails.get(host.id) || 0) + 1;
    fails.set(host.id, n);
    const state = !r.ok ? (n >= DOWN_AFTER ? "down" : "late") : r.ms > SLOW_MS && r.ms > 2 * medianMs ? "slow" : "up";
    return { id: host.id, parent: host.parent, kind: host.kind, state, ms: r.ok ? r.ms : null, coldMs: r.coldMs, status: r.status, note: r.note };
  });

  let tailnet = null;
  if (ts?.Self) {
    const peers = Object.values(ts.Peer || {}).map(p => {
      const name = short(p);
      // A tailnet machine that is one of the hosts above rides on that star.
      const node = nodes.find(n => name.split("-").includes(n.id));
      if (node) node.tailnet = Boolean(p.Online);
      return { name: plain(name), os: p.OS || "", online: Boolean(p.Online), lastSeen: p.Online ? 0 : seen(p.LastSeen),
        direct: Boolean(p.CurAddr), node: node?.id || "" };
    }).sort((a, b) => b.online - a.online || b.lastSeen - a.lastSeen || a.name.localeCompare(b.name));
    tailnet = { state: ts.BackendState || "", self: { name: plain(short(ts.Self)), online: Boolean(ts.Self.Online) }, peers };
  }
  return { observedAt: Date.now() / 1000, medianMs, uplink: medianMs > SLOW_MS ? "slow" : "ok", nodes, tailnet };
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) {
  console.log(JSON.stringify(await probeInfra(), null, 2));
}
