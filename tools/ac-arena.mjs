#!/usr/bin/env node
// ac-arena — multiplayer end-to-end test client for arena.mjs.
//
//   node tools/ac-arena.mjs swarm [--count=4] [--duration=30] [--target=...]
//   node tools/ac-arena.mjs watch [--target=...] [--duration=30]
//
// `swarm` opens N WebSocket-only fake clients that send arena:hello +
// arena:cmd at 60 Hz with mild random motion. They do not connect over
// UDP/geckos (Node has no built-in WebRTC), which exercises the WS
// fallback paths server-side. Use it to load-test arena-manager and
// confirm the cmd direction is reaching receiveCmd.
//
// `watch` opens a single passive probe and prints arena:welcome roster +
// per-second snap counts. Useful for diagnosing whether snaps are
// flowing at all without involving the browser/PWA.

// Uses Node's built-in WebSocket (>=22). No `ws` dep required.
import { packCmd, BTN } from "../system/public/aesthetic.computer/lib/pmove.mjs";

const DEFAULT_TARGET = "wss://session-server.aesthetic.computer";
const CMD_HZ = 60;
const CMD_BACKUP = 3;

function parseArgs(argv) {
  const out = { _: [] };
  for (const arg of argv) {
    if (arg.startsWith("--")) {
      const [k, v] = arg.slice(2).split("=");
      out[k] = v ?? true;
    } else {
      out._.push(arg);
    }
  }
  return out;
}

function rngHandle(prefix) {
  return `${prefix}_${Math.floor(Math.random() * 9_000) + 1000}`;
}

class ArenaWSClient {
  constructor({ target, handle, onSnap, onWelcome, onClose, onError, mode = "active" }) {
    this.target = target;
    this.handle = handle;
    this.mode = mode; // "active" | "probe"
    this.onSnap = onSnap || (() => {});
    this.onWelcome = onWelcome || (() => {});
    this.onClose = onClose || (() => {});
    this.onError = onError || ((e) => console.error("ws err:", e?.message || e));
    this.connectedAt = 0;
    this.cmdSeq = 0;
    this.cmdOutbox = []; // last CMD_BACKUP cmds
    this.lastSnapAck = 0;
    this.cmdInterval = null;
    this.flushAcc = 0;
    this.snapCount = 0;
    this.cmdSent = 0;
    this.wantClose = false;
    this.ws = null;
    this.helloSent = false;
  }

  connect() {
    this.ws = new WebSocket(this.target);
    this.ws.addEventListener("open", () => this._onOpen());
    this.ws.addEventListener("message", (ev) => this._onMessage(ev.data));
    this.ws.addEventListener("close", () => this._onClose());
    this.ws.addEventListener("error", (ev) => this.onError(ev?.error || ev?.message || ev));
  }

  _onOpen() {
    this.connectedAt = Date.now();
    this._send("arena:hello", { handle: this.handle, ...(this.mode === "probe" ? { probe: true } : {}) });
    this.helloSent = true;
    if (this.mode === "active") {
      // 60 Hz tick: produce one cmd per tick, flush every 2 ticks (30 Hz frames).
      this.cmdInterval = setInterval(() => this._tick(), 1000 / CMD_HZ);
    }
  }

  _onClose() {
    if (this.cmdInterval) { clearInterval(this.cmdInterval); this.cmdInterval = null; }
    this.onClose();
  }

  _onMessage(raw) {
    let msg;
    try { msg = JSON.parse(raw); } catch { return; }
    if (msg.type === "connected" || msg.type === "joined") return;
    const content = typeof msg.content === "string" ? safeParse(msg.content) : msg.content;
    if (msg.type === "arena:welcome") {
      this.onWelcome(content);
      return;
    }
    if (msg.type === "arena:snap") {
      this.snapCount++;
      if (typeof content?.messageNum === "number" && content.messageNum > this.lastSnapAck) {
        this.lastSnapAck = content.messageNum;
      }
      this.onSnap(content);
      return;
    }
    if (msg.type === "arena:takeover") {
      console.warn(`[${this.handle}] kicked to spectator (taken over by another tab)`);
    }
  }

  _tick() {
    // Compose a usercmd. Random walk: change direction every ~0.5s.
    if (!this._dir || Math.random() < 1 / (CMD_HZ * 0.5)) {
      this._dir = {
        fwd: Math.random() < 0.4 ? (Math.random() < 0.5 ? -1 : 1) : 0,
        right: Math.random() < 0.4 ? (Math.random() < 0.5 ? -1 : 1) : 0,
        yaw: Math.random() * 360 - 180,
        jump: Math.random() < 0.05,
      };
    }
    const ms = Date.now() - this.connectedAt;
    const cmd = packCmd({
      ms,
      fwd: this._dir.fwd,
      right: this._dir.right,
      yaw: this._dir.yaw,
      pitch: 0,
      buttons: this._dir.jump ? BTN.JUMP : 0,
    });
    this.cmdSeq++;
    this.cmdOutbox.push({ seq: this.cmdSeq, cmd });
    while (this.cmdOutbox.length > CMD_BACKUP) this.cmdOutbox.shift();

    this.flushAcc++;
    if (this.flushAcc >= 2) { // 30 Hz frame rate
      this.flushAcc = 0;
      this._flush();
    }
  }

  _flush() {
    if (this.cmdOutbox.length === 0) return;
    const frame = {
      handle: this.handle,
      firstSeq: this.cmdOutbox[0].seq,
      ack: this.lastSnapAck,
      cmds: this.cmdOutbox.map((e) => e.cmd),
    };
    this._send("arena:cmd", frame);
    this.cmdSent++;
  }

  _send(type, content) {
    if (this.ws?.readyState !== WebSocket.OPEN) return;
    this.ws.send(JSON.stringify({ type, content }));
  }

  bye() {
    this.wantClose = true;
    if (this.helloSent) this._send("arena:bye", { handle: this.handle });
    setTimeout(() => this.ws?.close(), 100);
  }
}

function safeParse(s) {
  try { return JSON.parse(s); } catch { return s; }
}

async function cmdSwarm(opts) {
  const target = opts.target || DEFAULT_TARGET;
  const count = parseInt(opts.count, 10) || 4;
  const duration = parseInt(opts.duration, 10) || 30;
  console.log(`🏟️  swarm: ${count} clients → ${target} for ${duration}s`);

  const clients = [];
  const stats = { totalSnaps: 0, totalCmds: 0, lastReport: Date.now() };

  for (let i = 0; i < count; i++) {
    const handle = rngHandle("swarm");
    const c = new ArenaWSClient({
      target,
      handle,
      onWelcome: (msg) => {
        const peers = (msg.roster || []).filter((h) => h !== handle).length;
        console.log(`✅ ${handle} welcome — ${peers} peer(s) already in arena`);
      },
      onClose: () => console.log(`👋 ${handle} closed (snaps=${c.snapCount} cmds=${c.cmdSent})`),
    });
    clients.push(c);
    // Stagger connects so we don't burst the server.
    setTimeout(() => c.connect(), i * 80);
  }

  const reporter = setInterval(() => {
    const totalSnaps = clients.reduce((s, c) => s + c.snapCount, 0);
    const totalCmds = clients.reduce((s, c) => s + c.cmdSent, 0);
    const dSnaps = totalSnaps - stats.totalSnaps;
    const dCmds = totalCmds - stats.totalCmds;
    const dt = (Date.now() - stats.lastReport) / 1000;
    stats.totalSnaps = totalSnaps;
    stats.totalCmds = totalCmds;
    stats.lastReport = Date.now();
    const live = clients.filter((c) => c.ws?.readyState === WebSocket.OPEN).length;
    console.log(`📊 live=${live}/${count} cmd-tx=${(dCmds / dt).toFixed(0)}/s snap-rx=${(dSnaps / dt).toFixed(0)}/s`);
  }, 5000);

  const stopAt = Date.now() + duration * 1000;
  await new Promise((resolve) => {
    const tick = () => {
      if (Date.now() >= stopAt) return resolve();
      setTimeout(tick, 250);
    };
    tick();
    process.on("SIGINT", () => { console.log("\n^C received"); resolve(); });
  });

  clearInterval(reporter);
  console.log(`🛑 closing ${clients.length} clients...`);
  for (const c of clients) c.bye();
  await new Promise((r) => setTimeout(r, 800));

  // Final summary.
  let snaps = 0, cmds = 0;
  for (const c of clients) { snaps += c.snapCount; cmds += c.cmdSent; }
  console.log(`✓ done — total snap rx=${snaps}, cmd tx=${cmds}`);
}

async function cmdWatch(opts) {
  const target = opts.target || DEFAULT_TARGET;
  const duration = parseInt(opts.duration, 10) || 0; // 0 = until SIGINT
  const handle = rngHandle("watch");
  console.log(`👀 watch ${handle} → ${target}${duration ? ` for ${duration}s` : ""}`);

  let lastSnapMs = 0;
  let snapsThisInterval = 0;
  const c = new ArenaWSClient({
    target,
    handle,
    mode: "probe",
    onWelcome: (msg) => {
      const roster = msg.roster || [];
      console.log(`✅ welcome — ${roster.length} player(s) in arena: ${roster.join(", ") || "(empty)"}`);
    },
    onSnap: (snap) => {
      snapsThisInterval++;
      lastSnapMs = Date.now();
      const players = snap.players || [];
      if (snap.tick % 30 === 0) {
        const heads = players.slice(0, 6).map((p) => `${p.h}@(${p.x.toFixed(1)},${p.z.toFixed(1)})`);
        console.log(`📸 t=${snap.tick} players=${players.length} ${heads.join(" ")}${players.length > 6 ? " …" : ""}`);
      }
    },
    onClose: () => {},
  });
  c.connect();

  const reporter = setInterval(() => {
    const ageMs = lastSnapMs ? Date.now() - lastSnapMs : -1;
    console.log(`📊 snaps/5s=${snapsThisInterval} last-snap-age=${ageMs}ms`);
    snapsThisInterval = 0;
  }, 5000);

  await new Promise((resolve) => {
    if (duration > 0) setTimeout(resolve, duration * 1000);
    process.on("SIGINT", () => { console.log("\n^C received"); resolve(); });
  });

  clearInterval(reporter);
  c.bye();
  await new Promise((r) => setTimeout(r, 300));
  console.log(`✓ done`);
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const sub = args._[0];
  if (sub === "swarm") return cmdSwarm(args);
  if (sub === "watch") return cmdWatch(args);
  console.log(`Usage:
  node tools/ac-arena.mjs swarm [--count=N] [--duration=Ns] [--target=wss://...]
  node tools/ac-arena.mjs watch [--target=wss://...] [--duration=Ns]

Default target: ${DEFAULT_TARGET}`);
  process.exit(1);
}

main().catch((err) => {
  console.error("fatal:", err);
  process.exit(2);
});
