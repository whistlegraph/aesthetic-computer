#!/usr/bin/env node
// arena-probe — a text-only spectator for any WorldManager world (arena, land).
//
// Connects to a session server over WebSocket, joins as a "probe", receives
// snapshots, and reports latency / jitter / peer activity. Use to smoke-test
// connectivity end-to-end (dev + prod), measure wire timing after a deploy,
// or just watch a world from a terminal.
//
// Usage:
//   node session-server/arena-probe.mjs                  # defaults to prod
//   node session-server/arena-probe.mjs --url wss://session.aesthetic.computer
//   node session-server/arena-probe.mjs --url ws://localhost:8889 --handle probe1
//   AC_PROBE_URL=ws://localhost:8889 node session-server/arena-probe.mjs
//
//   Flags:
//     --url <ws-url>     default: wss://session.aesthetic.computer
//     --world <name>     default: arena (also: land)
//     --handle <name>    default: probe_<random4>
//     --ping <ms>        default: 2000 (ping interval)
//     --status <ms>      default: 1000 (status line interval)
//     --quiet            suppress per-event logs; only print the status line
//
// Ctrl-C to exit.

import { WebSocket } from "ws";

// --- Args ---

const argv = process.argv.slice(2);
const flag = (name, fallback) => {
  const i = argv.indexOf("--" + name);
  if (i === -1) return fallback;
  return argv[i + 1];
};
const has = (name) => argv.indexOf("--" + name) !== -1;

const URL = flag("url", process.env.AC_PROBE_URL || "wss://session.aesthetic.computer");
const WORLD = flag("world", process.env.AC_PROBE_WORLD || "arena");
const HANDLE = flag("handle", "probe_" + Math.random().toString(36).slice(2, 6));
const PING_MS = +flag("ping", 2000);
const STATUS_MS = +flag("status", 1000);
const QUIET = has("quiet");

// --- State ---

let ws = null;
let connectedAt = 0;
let openedMs = 0;

const stats = {
  snapsRx: 0,
  lastSnapAt: 0,
  snapIntervals: [],     // recent gaps between snap arrivals (ms)
  pingRttSamples: [],    // last N RTTs (ms)
  lastPingSentAt: 0,
  welcomed: false,
  yourTick: 0,
  serverCfg: null,
  players: [],           // latest player blob from server
  events: [],             // recent join/leave messages (scrolls)
};

function ring(buf, v, cap = 30) { buf.push(v); while (buf.length > cap) buf.shift(); }
function mean(a) { return a.length ? a.reduce((s, x) => s + x, 0) / a.length : 0; }
function stddev(a) {
  if (a.length < 2) return 0;
  const m = mean(a);
  return Math.sqrt(mean(a.map((x) => (x - m) * (x - m))));
}

// --- Wire helpers (match session.mjs's pack() format) ---

function send(type, content) {
  if (!ws || ws.readyState !== WebSocket.OPEN) return;
  ws.send(JSON.stringify({ type, content: JSON.stringify(content) }));
}

function log(...args) { if (!QUIET) console.log(...args); }

// --- Connect + wire up ---

function connect() {
  // Local dev session servers run TLS with a self-signed cert — accept it.
  ws = new WebSocket(URL, { rejectUnauthorized: false });

  ws.on("open", () => {
    connectedAt = Date.now();
    openedMs = connectedAt;
    log(`\n🛰️  connected to ${URL} as "${HANDLE}"`);
    send(`${WORLD}:hello`, { handle: HANDLE, probe: true });
    // Start ping loop
    tickPing();
  });

  ws.on("message", (data) => {
    let msg;
    try { msg = JSON.parse(data.toString()); } catch { return; }
    const t = msg.type;
    const body = typeof msg.content === "string" ? safeParse(msg.content) : msg.content;
    if (!t || !body) return;

    if (t === `${WORLD}:welcome`) {
      stats.welcomed = true;
      stats.serverCfg = body.cfg;
      log(`🛬 welcome: you=${body.you} probe=${!!body.probe} roster=[${(body.roster||[]).join(", ")}] tick=${body.tick} serverMs=${body.serverMs}`);
      return;
    }
    if (t === `${WORLD}:snap`) {
      const now = Date.now();
      stats.snapsRx++;
      if (stats.lastSnapAt) ring(stats.snapIntervals, now - stats.lastSnapAt);
      stats.lastSnapAt = now;
      stats.yourTick = body.tick;
      stats.players = body.players || [];
      return;
    }
    if (t === `${WORLD}:join`) {
      const ev = `join ${body.handle}`;
      ring(stats.events, ev, 8);
      log(`🟢 ${ev}`);
      return;
    }
    if (t === `${WORLD}:leave`) {
      const ev = `leave ${body.handle}`;
      ring(stats.events, ev, 8);
      log(`🔴 ${ev}`);
      return;
    }
    if (t === `${WORLD}:pong`) {
      const rtt = Date.now() - body.ts;
      ring(stats.pingRttSamples, rtt, 20);
      return;
    }
  });

  ws.on("close", () => {
    log("🚪 disconnected — retrying in 2s…");
    stats.welcomed = false;
    setTimeout(connect, 2000);
  });

  ws.on("error", (err) => {
    log("❌ ws error:", err.message);
    // `close` will follow; don't reconnect here.
  });
}

function safeParse(s) { try { return JSON.parse(s); } catch { return s; } }

// --- Timers ---

function tickPing() {
  setInterval(() => {
    if (!ws || ws.readyState !== WebSocket.OPEN) return;
    const ts = Date.now();
    stats.lastPingSentAt = ts;
    send(`${WORLD}:ping`, { handle: HANDLE, ts });
  }, PING_MS);
}

function formatStatusLine() {
  const upMs = openedMs ? Date.now() - openedMs : 0;
  const up = (upMs / 1000).toFixed(1) + "s";
  const rtt = stats.pingRttSamples.length
    ? `${Math.round(mean(stats.pingRttSamples))}ms±${Math.round(stddev(stats.pingRttSamples))}`
    : "--";
  const snapRate = stats.snapIntervals.length
    ? (1000 / mean(stats.snapIntervals)).toFixed(1) + "Hz"
    : "--";
  const jitter = stats.snapIntervals.length >= 2
    ? Math.round(stddev(stats.snapIntervals)) + "ms"
    : "--";
  const peers = stats.players.map((p) => p.h).join(",") || "none";
  const youTick = stats.yourTick;
  return `[${up}] rtt=${rtt}  snaps=${stats.snapsRx} @ ${snapRate} (±${jitter})  tick=${youTick}  peers=[${peers}]`;
}

setInterval(() => {
  if (!ws || ws.readyState !== WebSocket.OPEN) {
    process.stdout.write(`\r[…] connecting to ${URL}            `);
    return;
  }
  process.stdout.write(`\r${formatStatusLine()}          `);
}, STATUS_MS);

// --- Graceful exit ---

process.on("SIGINT", () => {
  console.log("\n👋 bye");
  try { send(`${WORLD}:bye`, { handle: HANDLE }); } catch {}
  try { ws?.close(); } catch {}
  process.exit(0);
});

// --- Go ---

log(`🛰️  world-probe [${WORLD}] → ${URL}`);
connect();
