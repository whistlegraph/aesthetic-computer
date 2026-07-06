#!/usr/bin/env node
// normalize-machines.mjs — enrich the canonical fleet registry into the
// normalized fleet schema WITHOUT overwriting the source of truth.
//
// Reads  $FLEET_SRC  (default ~/aesthetic-computer-vault/machines.json)
// Writes $FLEET_OUT  (default ~/aesthetic-computer-vault/machines.normalized.json)
//
// The source stays exactly as authored — this is additive. Every machine keeps
// its original fields; we layer on the fleet vocabulary: `designation`,
// `capabilities[]`, a normalized `status` pointer (how liveness is resolved),
// and a `tailscale` cross-reference so live `tailscale status` can be matched
// to a registry entry by its magic-DNS short name. New tailnet machines that
// exist on the wire but were never in the vault are ADDED as review stubs
// (flagged `_review`), so a human can promote or correct them.
//
// House style: node builtins only, no deps. Data lives in the vault (private);
// this code can be public. Run it, eyeball machines.normalized.json, then (if
// happy) a human folds the new fields back into the canonical machines.json.
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

const HOME = homedir();
const SRC = process.env.FLEET_SRC || join(HOME, "aesthetic-computer-vault", "machines.json");
const OUT = process.env.FLEET_OUT || join(HOME, "aesthetic-computer-vault", "machines.normalized.json");

// ── controlled vocabularies (documented, so agents can reason over them) ─────
const SCHEMA = {
  version: 1,
  generatedBy: "toolchain/fleet/normalize-machines.mjs",
  // A machine's PRIMARY fleet role. One per machine.
  designations: {
    "agent-endpoint": "Hosts a hermes gateway + a stable identity. Where an autonomous agent lives on the tailnet.",
    "compute-node": "Headless capability provider. No identity of its own; advertises capabilities (GPU/MLX/automation) over HTTP MCP on the tailnet.",
    "control": "Author / control box driven by a human (or by an agent-endpoint). Holds git auth, drives the fleet.",
    "build": "Build / CI box — compiles native + web targets.",
    "service": "Hosts a long-lived network service (db, session server, mail, web).",
    "display": "Art / signage display device.",
    "legacy": "Archived or legacy host; kept for reference, not active fleet compute.",
  },
  // Composable capability tags. A machine has zero or more.
  capabilities: {
    gpu: "Discrete GPU available for compute/render.",
    cuda: "NVIDIA CUDA stack present.",
    mlx: "Apple-silicon MLX local-model inference.",
    unreal: "Unreal Engine toolchain installed.",
    docker: "Can run Docker containers.",
    "macos-automation": "Scriptable native macOS UI (osascript, screen+app automation via slab).",
    "screen-capture": "Screen Recording granted — frame/puppet can see + drive it.",
    "chromium-pool": "Can host headless/headed Chrome for CDP automation.",
    "ffmpeg-render": "ffmpeg media rendering (video/audio pipelines).",
    "always-on": "Runs 24/7 — safe to schedule background work on.",
    "git-remote": "Holds GitHub / knot credentials for pushing.",
    mongodb: "Self-hosted MongoDB.",
    redis: "Redis instance.",
    mail: "Mail appliance (isync/MCP).",
    "web-host": "Serves public HTTP(S).",
    "build-macos": "Builds macOS/native AC targets.",
    "build-ios": "Builds iOS targets.",
    "tailnet-api": "Exposes authed HTTP APIs over the tailnet.",
    "art-display": "Drives an art-display surface.",
  },
  statusSources: {
    tailscale: "Liveness from `tailscale status --json`, matched by tailscale.name (magic-DNS short name).",
    http: "Liveness from an HTTP(S) health probe (not wired into fleet-mcp yet).",
    lan: "LAN-only host; no continuous liveness source.",
    none: "No liveness source (legacy/archived).",
  },
};

// ── per-machine enrichment, keyed by the vault's machine key ─────────────────
// Only the fleet-vocabulary fields live here; everything else is inherited from
// the source entry untouched.
const ENRICH = {
  "jeffrey-windows": {
    designation: "compute-node",
    capabilities: ["gpu", "cuda", "unreal", "docker", "screen-capture"],
    tailscale: { name: "windows-tower" },
    status: { source: "tailscale", key: "windows-tower" },
    fleetRole: "RTX GPU + Unreal render/compute node. On the tailnet as 'windows-tower'.",
    _resolves: "vault key 'jeffrey-windows' === tailnet node 'windows-tower'.",
  },
  "jeffrey-macbook": {
    designation: "control",
    capabilities: ["macos-automation", "git-remote"],
    status: { source: "lan", key: null },
    fleetRole: "Portable author box (LAN jas-mbp.local).",
    _review: "Possibly stale / superseded by 'neo'. Confirm whether this is a distinct machine.",
  },
  "jas-fedora": {
    designation: "control",
    capabilities: ["docker"],
    status: { source: "lan", key: null },
    fleetRole: "Personal Linux laptop (intermittent).",
  },
  "mac-mini": {
    designation: "build",
    capabilities: ["build-macos", "build-ios"],
    status: { source: "lan", key: null },
    fleetRole: "Older Mac mini build server (spiderlily Unreal Mac/iOS builds).",
  },
  "legacy-2016": {
    designation: "legacy",
    capabilities: ["web-host", "legacy"],
    status: { source: "http", key: null },
    fleetRole: "Legacy 2016 DO droplet hosting archived *.jas.life sites.",
  },
  "session-server": {
    designation: "service",
    capabilities: ["web-host", "redis", "always-on"],
    status: { source: "http", key: null },
    fleetRole: "Real-time session/chat/clock backend (Caddy + pm2). 1GB box, OOM-fragile.",
  },
  silo: {
    designation: "service",
    capabilities: ["mongodb", "web-host", "always-on"],
    status: { source: "http", key: null },
    fleetRole: "Self-hosted MongoDB + data/storage dashboard.",
  },
  "ff1-dvveklza": {
    designation: "display",
    capabilities: ["art-display"],
    status: { source: "lan", key: null },
    fleetRole: "Feral File FF1 art-display device.",
  },
  "x1-nano-g2": {
    designation: "control",
    capabilities: ["docker"],
    status: { source: "lan", key: null },
    fleetRole: "Ultralight Fedora laptop (intermittent).",
  },
  blueberry: {
    designation: "control",
    capabilities: ["macos-automation", "screen-capture"],
    tailscale: { name: "blueberry", ip: "100.79.75.53" },
    status: { source: "tailscale", key: "blueberry" },
    fleetRole: "Lightweight (8GB) control / macpal box. Too small to host hermes — author + drive only.",
  },
  "macbook-pro-clam": {
    designation: "compute-node",
    capabilities: ["mlx", "macos-automation", "screen-capture", "chromium-pool", "ffmpeg-render", "always-on"],
    tailscale: { name: "macbook-pro-clam", ip: "100.86.206.3" },
    status: { source: "tailscale", key: "macbook-pro-clam" },
    fleetRole: "Always-on macOS media-gen compute node (M1 Pro/16GB): Chromium pool, MLX local models, screen + native-app automation. Also the strongest agent-endpoint CANDIDATE among the Macs.",
  },
};

// ── tailnet machines present on the wire but missing from the vault ──────────
// Added as review stubs so the fleet view is complete. Promote/correct by hand.
const ADD = {
  neo: {
    label: "💻 Jeffrey's MacBook Neo",
    emoji: "💻",
    os: "macOS",
    user: "jas",
    hostname: "neo.local",
    repoPath: "/Users/jas/aesthetic-computer",
    designation: "control",
    capabilities: ["git-remote", "macos-automation", "screen-capture"],
    tailscale: { name: "neo", ip: "100.108.5.81" },
    status: { source: "tailscale", key: "neo" },
    fleetRole: "Primary control MacBook — holds GitHub auth, live-shared author box for the fleet.",
    _review: "Added by fleet normalization 2026-07-06 (on the tailnet, absent from vault). Fill in hardware/ssh.",
  },
  chicken: {
    label: "🐔 Chicken (Mac mini)",
    emoji: "🐔",
    os: "macOS",
    user: "jas",
    hostname: "chicken.local",
    designation: "build",
    capabilities: ["build-macos", "chromium-pool", "macos-automation", "always-on"],
    hardware: { model: "Mac mini", memory: "16 GB" },
    ssh: { enabled: true, via: "tailnet SSH", alias: "chicken" },
    tailscale: { name: "chicken-1", ip: "100.98.158.126" },
    status: { source: "tailscale", key: "chicken-1" },
    fleetRole: "16GB mini build/run box; hosts a Chrome CDP pool for fuser App-Node testing.",
    _review: "Added by fleet normalization 2026-07-06. Active tailnet node is 'chicken-1'; a stale offline 'chicken' node also exists — ignore it.",
  },
  panda: {
    label: "🐼 Panda (Mac mini)",
    emoji: "🐼",
    os: "macOS",
    user: "jas",
    hostname: "panda.local",
    designation: "build",
    capabilities: ["build-macos", "chromium-pool", "macos-automation", "always-on"],
    hardware: { model: "Mac mini", memory: "16 GB" },
    ssh: { enabled: true, via: "tailnet SSH", alias: "panda" },
    tailscale: { name: "panda-1", ip: "100.88.155.94" },
    status: { source: "tailscale", key: "panda-1" },
    fleetRole: "16GB mini build/run box; PR-viewing + fuser typecheck/build.",
    _review: "Added by fleet normalization 2026-07-06. Active tailnet node is 'panda-1'; a stale offline 'panda' node also exists — ignore it.",
  },
  jasellite: {
    label: "🛰️ Jasellite (services appliance)",
    emoji: "🛰️",
    os: "Linux (DigitalOcean)",
    user: "root",
    ip: "24.144.92.66",
    designation: "agent-endpoint",
    capabilities: ["always-on", "mail", "tailnet-api", "docker", "web-host"],
    ssh: { enabled: true, via: "tailnet + public IP" },
    tailscale: { name: "jasellite", ip: "100.72.36.78" },
    status: { source: "tailscale", key: "jasellite" },
    fleetRole: "Always-on Linux services appliance + PRIMARY hermes agent-endpoint host. Runs the mail appliance and authed tailnet APIs.",
    _review: "Added by fleet normalization 2026-07-06. This is the intended home for the hermes gateway + fleet identity.",
  },
  "jas-nzxt": {
    label: "🖥️ jas-nzxt (GPU tower)",
    emoji: "🖥️",
    os: "Linux",
    designation: "compute-node",
    capabilities: ["gpu", "cuda", "docker", "ffmpeg-render"],
    tailscale: { name: "jas-nzxt", ip: "100.103.42.46" },
    status: { source: "tailscale", key: "jas-nzxt" },
    fleetRole: "Linux GPU tower for heavy compute / render.",
    _review: "Added by fleet normalization 2026-07-06 (on the tailnet, absent from vault). Fill in hardware/ssh/user.",
  },
};

function main() {
  if (!existsSync(SRC)) {
    console.error(`source not found: ${SRC}`);
    process.exit(1);
  }
  const src = JSON.parse(readFileSync(SRC, "utf8"));
  const out = {
    _schema: SCHEMA,
    _note: "NORMALIZED / PROPOSAL — regenerate with toolchain/fleet/normalize-machines.mjs. Do not treat as canonical until reviewed and folded back into machines.json.",
    machines: {},
    detection: src.detection,
    tailscale: src.tailscale,
  };

  // 1) enrich existing entries (additive; original fields preserved)
  for (const [key, entry] of Object.entries(src.machines || {})) {
    const extra = ENRICH[key] || { _review: "No fleet enrichment mapping — classify me." };
    out.machines[key] = { name: key, ...entry, ...extra, _normalized: true };
  }
  // 2) add tailnet machines missing from the vault
  for (const [key, entry] of Object.entries(ADD)) {
    if (out.machines[key]) continue;
    out.machines[key] = { name: key, ...entry, _normalized: true, _added: true };
  }

  writeFileSync(OUT, JSON.stringify(out, null, 2) + "\n");
  const machines = Object.values(out.machines);
  const byDesignation = {};
  for (const m of machines) (byDesignation[m.designation] ||= []).push(m.name);
  console.log(`wrote ${OUT}`);
  console.log(`${machines.length} machines:`);
  for (const [d, names] of Object.entries(byDesignation)) console.log(`  ${d}: ${names.join(", ")}`);
  const review = machines.filter((m) => m._review).map((m) => m.name);
  if (review.length) console.log(`needs review: ${review.join(", ")}`);
}

main();
