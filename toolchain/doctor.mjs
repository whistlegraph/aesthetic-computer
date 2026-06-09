#!/usr/bin/env node
// toolchain/doctor.mjs — one preflight that pings every moving part of AC.
//
// Tells you *which* layer is sick before you waste time debugging the wrong
// one: local dev servers, production hosts, the asset CDN, and the host tools
// the pipelines lean on. Dependency-free (Node built-ins + global fetch).
//
//   npm run doctor            full sweep
//   npm run doctor -- --local only local dev servers + host tooling
//   npm run doctor -- --prod  only production reachability
//   npm run doctor -- --strict exit non-zero if any CRITICAL check fails
//
// Checks degrade gracefully: a stopped dev server is a warning, not a failure.
// Only checks marked `critical` (prod site + CDN) can fail --strict.

import net from "node:net";
import { execFile } from "node:child_process";

const args = new Set(process.argv.slice(2));
const ONLY_LOCAL = args.has("--local");
const ONLY_PROD = args.has("--prod");
const STRICT = args.has("--strict");

// ── probes ──────────────────────────────────────────────────────────────────

// Is a TCP port accepting connections? (the truest "is it up" for local servers)
function tcp(host, port, timeout = 1500) {
  return new Promise((resolve) => {
    const t0 = Date.now();
    const sock = new net.Socket();
    const done = (ok, note) => {
      sock.destroy();
      resolve({ ok, ms: Date.now() - t0, note });
    };
    sock.setTimeout(timeout);
    sock.once("connect", () => done(true));
    sock.once("timeout", () => done(false, "timeout"));
    sock.once("error", (e) => done(false, e.code || "error"));
    sock.connect(port, host);
  });
}

// HTTP(S) reachability — reports the status code and round-trip latency.
async function http(url, { method = "HEAD", timeout = 6000, expect } = {}) {
  const t0 = Date.now();
  const ctrl = new AbortController();
  const timer = setTimeout(() => ctrl.abort(), timeout);
  try {
    let res = await fetch(url, { method, signal: ctrl.signal, redirect: "manual" });
    // Some hosts reject HEAD — retry once with GET before giving up.
    if (method === "HEAD" && (res.status === 405 || res.status === 501)) {
      res = await fetch(url, { method: "GET", signal: ctrl.signal, redirect: "manual" });
    }
    const ms = Date.now() - t0;
    const reachable = res.status > 0 && res.status < 500;
    const ok = expect ? res.status === expect : reachable;
    return { ok, ms, note: `HTTP ${res.status}` };
  } catch (e) {
    return { ok: false, ms: Date.now() - t0, note: e.name === "AbortError" ? "timeout" : (e.cause?.code || e.code || "unreachable") };
  } finally {
    clearTimeout(timer);
  }
}

// Is a command-line tool on PATH? (host tooling the pipelines shell out to)
function bin(name) {
  return new Promise((resolve) => {
    execFile("command", ["-v", name], { shell: "/bin/sh" }, (err, out) => {
      resolve({ ok: !err && !!out.trim(), note: err ? "not on PATH" : out.trim() });
    });
  });
}

// ── the checklist ────────────────────────────────────────────────────────────
// group · label · run() → {ok, ms?, note?} · critical? · scope (local|prod|tool)

const CHECKS = [
  // Local dev servers — advisory: down just means you haven't started them.
  { group: "Local dev", label: "site (8888)",      scope: "local", run: () => tcp("127.0.0.1", 8888) },
  { group: "Local dev", label: "session (8889)",   scope: "local", run: () => tcp("127.0.0.1", 8889) },
  { group: "Local dev", label: "redis (6379)",     scope: "local", run: () => tcp("127.0.0.1", 6379) },

  // Production reachability — these are the real signal.
  { group: "Production", label: "aesthetic.computer (lith)", scope: "prod", critical: true, run: () => http("https://aesthetic.computer") },
  { group: "Production", label: "assets CDN (DO Spaces)",    scope: "prod", critical: true, run: () => http("https://assets.aesthetic.computer") },
  { group: "Production", label: "oven (OTA builds)",         scope: "prod", run: () => http("https://oven.aesthetic.computer") },
  { group: "Production", label: "ai.aesthetic.computer",     scope: "prod", run: () => http("https://ai.aesthetic.computer") },
  { group: "Production", label: "help (aa bridge)",          scope: "prod", run: () => http("https://help.aesthetic.computer") },

  // Host tooling — the binaries pipelines shell out to.
  { group: "Host tooling", label: "node",         scope: "tool", run: () => bin("node") },
  { group: "Host tooling", label: "redis-server", scope: "tool", run: () => bin("redis-server") },
  { group: "Host tooling", label: "ffmpeg",       scope: "tool", run: () => bin("ffmpeg") },
  { group: "Host tooling", label: "doctl (CDN flush)", scope: "tool", run: () => bin("doctl") },
  { group: "Host tooling", label: "gh",           scope: "tool", run: () => bin("gh") },
  { group: "Host tooling", label: "jq",           scope: "tool", run: () => bin("jq") },
];

// ── run ──────────────────────────────────────────────────────────────────────

const scopeWanted = (s) =>
  (!ONLY_LOCAL && !ONLY_PROD) ||
  (ONLY_LOCAL && (s === "local" || s === "tool")) ||
  (ONLY_PROD && s === "prod");

const checks = CHECKS.filter((c) => scopeWanted(c.scope));

console.log("\n🩺 aesthetic.computer doctor\n");

const results = await Promise.all(
  checks.map(async (c) => ({ ...c, ...(await c.run()) })),
);

let group = null;
let criticalFailed = false;
for (const r of results) {
  if (r.group !== group) {
    group = r.group;
    console.log(`  ${group}`);
  }
  // Down-but-non-critical (e.g. a dev server you didn't start) reads as ⚠️.
  const icon = r.ok ? "✅" : r.critical ? "❌" : "⚠️ ";
  if (!r.ok && r.critical) criticalFailed = true;
  const ms = r.ms != null ? ` ${String(r.ms).padStart(4)}ms` : "";
  const note = r.note ? `  ${r.note}` : "";
  console.log(`    ${icon} ${r.label.padEnd(28)}${ms}${note}`);
}

const down = results.filter((r) => !r.ok);
console.log(
  `\n  ${results.length - down.length}/${results.length} healthy` +
    (down.length ? `  ·  ${down.length} need attention` : "  ·  all green") +
    "\n",
);

if (STRICT && criticalFailed) process.exit(1);
