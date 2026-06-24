#!/usr/bin/env node
// puppet.mjs — a persistent CDP multiplexer for driving browsers on many machines.
//
// One daemon on the controlling machine holds open Chrome DevTools Protocol
// WebSockets to every registered browser (local or remote-over-tunnel), so a
// command costs one LAN round-trip instead of a process spawn + fresh WS
// handshake (~400ms → ~5ms). A thin CLI talks to the daemon over a unix
// socket. Same spirit as a game server: persistent connections, broadcast,
// live event streams.
//
// This file ships in the PUBLIC aesthetic.computer repo, so it carries NO
// machine names or addresses: the machine registry lives in an UNTRACKED
// config at ~/.config/slab/puppet.json (see `puppet config`).
//
//   puppet daemon                     run the daemon (foreground)
//   puppet list                       machines, connection state, page targets
//   puppet eval <machine> <js>        evaluate an expression in the active page
//   puppet evalall <machine> <js...>  N expressions, one round-trip
//   puppet batch                      JSON-lines requests on stdin, one connection
//   puppet broadcast <js>             evaluate on ALL connected machines at once
//   puppet nav <machine> <url>        navigate the active page
//   puppet shot <machine> <out.png|.jpg>  screenshot (jpeg reads live watch frame)
//     --fullscreen|--screen  WHOLE screen (all windows) + OCR via frame.mjs;
//                            writes <out>.jpg + <out>.jpg.ocr.json (--fast,--no-ocr)
//   puppet watch <machine> <out.jpg>  screencast → latest frame on disk (--nth=N)
//   puppet stroke <machine> x1,y1 x2,y2 [...]   trusted mouse drag through points
//   puppet cursor <machine> <x> <y>   show/move a virtual cursor overlay
//   puppet tail <machine>             stream console messages (ctrl-c to stop)
//   puppet status                     daemon status (same JSON slab reads)
//   puppet config                     print/create the config stub path
//
// Target selection: most-recently-active http(s) page, like cdp.mjs. Override
// per-call with --target=<url or id substring>.
//
// Config shape (~/.config/slab/puppet.json):
//   { "machines": { "<name>": { "cdpUrl": "http://127.0.0.1:9223",
//                               "tunnelCmd": "ssh -fN -L 9223:127.0.0.1:9222 <host>" } } }
// `tunnelCmd` is optional — run when the cdpUrl is unreachable, then retried.

import { execFileSync, execSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import net from "node:net";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { logHit, overlayDrawExpr, overlayClearExpr, scanExpr } from "./analysis-layer.mjs";

const HOME = homedir();
const CONFIG_PATH =
  process.env.SLAB_PUPPET_CONFIG || join(HOME, ".config", "slab", "puppet.json");
const RUN_DIR = join(HOME, ".local", "share", "puppet");
const SOCK_PATH = process.env.SLAB_PUPPET_SOCK || join(RUN_DIR, "puppet.sock");
const STATUS_PATH = join(RUN_DIR, "status.json");

const CONFIG_STUB = {
  _README:
    "slab puppet config — UNTRACKED, never committed. Register machines " +
    "whose Chrome runs with --remote-debugging-port (reachable from here, " +
    "e.g. via an ssh -L tunnel since CDP binds loopback).",
  machines: {},
};

// ─── config ──────────────────────────────────────────────────────────────

function loadConfig() {
  if (!existsSync(CONFIG_PATH)) return null;
  try {
    return JSON.parse(readFileSync(CONFIG_PATH, "utf8"));
  } catch {
    return null;
  }
}

function cmdConfig() {
  if (!existsSync(CONFIG_PATH)) {
    mkdirSync(dirname(CONFIG_PATH), { recursive: true });
    writeFileSync(CONFIG_PATH, JSON.stringify(CONFIG_STUB, null, 2) + "\n");
    console.log(`created stub: ${CONFIG_PATH}`);
  } else {
    console.log(CONFIG_PATH);
  }
}

// ─── daemon: one Machine per registry entry ──────────────────────────────

class Machine {
  constructor(name, spec) {
    this.name = name;
    this.spec = spec; // { cdpUrl, tunnelCmd? }
    this.browser = null; // browser-level WebSocket
    this.nextId = 1;
    this.pending = new Map(); // id -> {resolve, reject}
    this.sessions = new Map(); // targetId -> sessionId
    this.targets = new Map(); // targetId -> {url, title, attached}
    this.consoleSubs = new Set(); // client sockets tailing console
    this.watches = new Map(); // sessionId -> live frame path (shot fast-path)
    this.frameHandlers = new Map(); // sessionId -> screencast frame sink
    this.connected = false;
    this.lastError = null;
    this.triedTunnel = false;
    // analysis layer: log + flash interaction points. On by default now
    // (@jeffrey) so the hit-scan overlay survives daemon restarts; the
    // overlays self-clear so always-on costs nothing visible when idle.
    // Override per machine at runtime with `puppet analysis off <machine>`.
    this.analysisOn = true;
  }

  // Analysis layer: when toggled on for this machine, record every trusted
  // interaction point (for the out-of-page frame viewer) and flash it on the
  // page-side SVG overlay — the realtime "hit scan" visualization. No-op when
  // off, so normal driving pays nothing. See analysis-layer.mjs.
  analysisFlash(sessionId, points, label = "hit") {
    if (!this.analysisOn || !points?.length) return;
    for (const [x, y] of points) logHit(this.name, { x, y, kind: label, label });
    this.callNoWait(
      "Runtime.evaluate",
      {
        expression: overlayDrawExpr(
          [],
          points.map(([x, y], i) => ({ x, y, label: i === 0 ? label : "" })),
        ),
      },
      sessionId,
    );
  }

  log(msg) {
    console.log(`[${this.name}] ${msg}`);
  }

  async connectLoop() {
    for (;;) {
      try {
        await this.connect();
        this.triedTunnel = false;
        await new Promise(res => (this.onClose = res)); // park until close
      } catch (err) {
        this.lastError = String(err?.message || err);
        if (this.spec.tunnelCmd && !this.triedTunnel) {
          this.triedTunnel = true;
          this.log(`unreachable, running tunnelCmd`);
          try {
            execSync(this.spec.tunnelCmd, { stdio: "ignore", timeout: 15000 });
          } catch {}
        }
      }
      this.connected = false;
      this.browser = null;
      this.sessions.clear();
      this.targets.clear();
      writeStatus();
      await new Promise(res => setTimeout(res, 3000));
    }
  }

  async connect() {
    const ver = await fetch(`${this.spec.cdpUrl}/json/version`).then(r => r.json());
    const ws = new WebSocket(ver.webSocketDebuggerUrl);
    await new Promise((res, rej) => {
      ws.onopen = res;
      ws.onerror = () => rej(new Error("ws connect failed"));
    });
    this.browser = ws;
    ws.onmessage = e => this.onMessage(JSON.parse(e.data));
    ws.onclose = () => {
      this.log("browser socket closed");
      for (const p of this.pending.values()) p.reject(new Error("socket closed"));
      this.pending.clear();
      this.connected = false;
      this.onClose?.();
    };
    await this.call("Target.setDiscoverTargets", { discover: true });
    this.connected = true;
    this.lastError = null;
    this.log(`connected (${ver.Browser})`);
    writeStatus();
  }

  call(method, params = {}, sessionId) {
    return new Promise((resolve, reject) => {
      const id = this.nextId++;
      const timer = setTimeout(() => {
        if (this.pending.delete(id)) reject(new Error(`${method} timed out`));
      }, 20000);
      this.pending.set(id, {
        resolve: v => {
          clearTimeout(timer);
          resolve(v);
        },
        reject: e => {
          clearTimeout(timer);
          reject(e);
        },
      });
      this.browser.send(JSON.stringify({ id, method, params, sessionId }));
    });
  }

  // Fire-and-forget CDP send — no pending entry, replies are ignored. CDP
  // preserves ordering on the socket, so streams of input events stay in
  // sequence; await only the calls whose results matter.
  callNoWait(method, params = {}, sessionId) {
    this.browser.send(JSON.stringify({ id: this.nextId++, method, params, sessionId }));
  }

  onMessage(msg) {
    if (msg.id && this.pending.has(msg.id)) {
      const p = this.pending.get(msg.id);
      this.pending.delete(msg.id);
      if (msg.error) p.reject(new Error(msg.error.message));
      else p.resolve(msg.result);
      return;
    }
    const { method, params } = msg;
    if (method === "Target.targetCreated" || method === "Target.targetInfoChanged") {
      const t = params.targetInfo;
      if (t.type === "page") this.targets.set(t.targetId, { url: t.url, title: t.title });
    } else if (method === "Target.targetDestroyed") {
      this.targets.delete(params.targetId);
      this.sessions.delete(params.targetId);
    } else if (method === "Page.screencastFrame" && msg.sessionId) {
      const h = this.frameHandlers?.get(msg.sessionId);
      if (h) h(msg);
    } else if (method === "Runtime.consoleAPICalled" && msg.sessionId) {
      const line = params.args
        .map(a => a.value ?? a.description ?? "")
        .join(" ");
      const evt = JSON.stringify({
        machine: this.name,
        type: params.type,
        line,
      });
      this.pushToSubs(evt);
    } else if (method === "Runtime.bindingCalled" && params.name === "__puppetEvent") {
      // push-style events from in-page harnesses — no polling, no console noise
      const evt = JSON.stringify({ machine: this.name, type: "event", line: params.payload });
      this.pushToSubs(evt);
    }
  }

  // Write to console subscribers, EVICTING the dead and the backed-up. A
  // SIGKILLed tail leaves a half-open socket whose writes buffer in daemon
  // memory forever — this fed a machine-wide RAM leak on 2026-06-12 (hours
  // of console events × several abandoned tails on an 8 GB controller).
  pushToSubs(evt) {
    for (const sock of this.consoleSubs) {
      if (sock.destroyed || sock.writableLength > 1 << 20) {
        this.consoleSubs.delete(sock);
        try { sock.destroy(); } catch {}
        continue;
      }
      try {
        sock.write(evt + "\n");
      } catch {
        this.consoleSubs.delete(sock);
      }
    }
  }

  pickTarget(filter) {
    const pages = [...this.targets.entries()].map(([id, t]) => ({ id, ...t }));
    if (filter) {
      const m = pages.find(p => p.id.includes(filter) || (p.url || "").includes(filter));
      if (m) return m;
    }
    return pages.filter(p => /^https?:/.test(p.url || "")).pop() || pages.pop();
  }

  async session(filter) {
    const target = this.pickTarget(filter);
    if (!target) throw new Error("no page target");
    let sessionId = this.sessions.get(target.id);
    if (!sessionId) {
      const r = await this.call("Target.attachToTarget", {
        targetId: target.id,
        flatten: true,
      });
      sessionId = r.sessionId;
      this.sessions.set(target.id, sessionId);
      // enable once per attach — nav/shot/reload used to re-send Page.enable
      // every call, one wasted tunnel round-trip each (6-17ms here).
      await this.call("Runtime.enable", {}, sessionId).catch(() => {});
      await this.call("Page.enable", {}, sessionId).catch(() => {});
      // push channel: pages call window.__puppetEvent(str) → tail stream
      await this.call("Runtime.addBinding", { name: "__puppetEvent" }, sessionId).catch(() => {});
    }
    return sessionId;
  }

  async eval(expr, filter) {
    const sessionId = await this.session(filter);
    const r = await this.call(
      "Runtime.evaluate",
      { expression: expr, returnByValue: true, awaitPromise: true },
      sessionId,
    );
    if (r.exceptionDetails) throw new Error(r.exceptionDetails.exception?.description || "eval threw");
    return r.result?.value;
  }

  // Open a new tab or positioned window (Target.createTarget). Returns targetId.
  async newtab(url, win) {
    const params = { url };
    if (win) Object.assign(params, { newWindow: true, ...win });
    const r = await this.call("Target.createTarget", params);
    return r.targetId;
  }

  // Close a tab/window by target filter (Target.closeTarget). STRICT match
  // only — never falls back to "most recent page" like pickTarget does
  // (a close that guesses closes the wrong window; learned 2026-06-12).
  async closeTarget(filter) {
    if (!filter) throw new Error("close requires a target filter");
    const target = [...this.targets.entries()]
      .map(([id, t]) => ({ id, ...t }))
      .find(t => t.id.includes(filter) || (t.url || "").includes(filter));
    if (!target) throw new Error("no matching target");
    await this.call("Target.closeTarget", { targetId: target.id });
    this.sessions.delete(target.id);
    this.targets.delete(target.id);
    return `closed ${target.url?.slice(0, 60) || target.id}`;
  }

  // Hard refresh: cache-bypassing reload (what ⌘⇧R does).
  async reload(filter) {
    const sessionId = await this.session(filter);
    await this.call("Page.reload", { ignoreCache: true }, sessionId);
    return "hard-reloaded";
  }

  async nav(url, filter) {
    const sessionId = await this.session(filter);
    await this.call("Page.navigate", { url }, sessionId);
    return url;
  }

  // Screenshot. opts: { format: "png"|"jpeg", quality (jpeg), fresh: true to
  // force a capture even when a live watch frame could answer instantly }.
  // PNG encode of a big window costs Chrome 50-150ms and 5-10× the tunnel
  // bytes of JPEG — prefer .jpg outputs for iteration loops.
  async shot(filter, opts = {}) {
    const sessionId = await this.session(filter);
    const live = this.watches?.get(sessionId);
    if (live && opts.format === "jpeg" && !opts.fresh) {
      try {
        const fs = await import("node:fs");
        const data = fs.readFileSync(live).toString("base64"); // 0ms: latest screencast frame
        this.analysisScan(sessionId);
        return data;
      } catch {}
    }
    const params = { format: opts.format ?? "png" };
    if (params.format === "jpeg") params.quality = opts.quality ?? 80;
    const r = await this.call("Page.captureScreenshot", params, sessionId);
    // After the capture (so the shot stays clean), light up the observation
    // overlay on the LIVE page — a watcher sees what was just looked at.
    this.analysisScan(sessionId);
    return r.data; // base64
  }

  // Fire the observation scan (text + interactive boxes) on the page, fire-
  // and-forget. No-op unless analysis is on for this machine.
  analysisScan(sessionId) {
    if (!this.analysisOn) return;
    this.callNoWait("Runtime.evaluate", { expression: scanExpr() }, sessionId);
  }

  // Trusted mouse drag through points, virtual cursor overlay riding along.
  async stroke(points, filter) {
    const sessionId = await this.session(filter);
    await this.cursorEval(sessionId);
    const [x0, y0] = points[0];
    await this.cursorMove(sessionId, x0, y0, true);
    await this.call(
      "Input.dispatchMouseEvent",
      { type: "mousePressed", x: x0, y: y0, button: "left", clickCount: 1 },
      sessionId,
    );
    for (const [x, y] of points.slice(1)) {
      this.callNoWait(
        "Input.dispatchMouseEvent",
        { type: "mouseMoved", x, y, button: "left", buttons: 1 },
        sessionId,
      );
      await new Promise(res => setTimeout(res, 12));
    }
    const [xn, yn] = points[points.length - 1];
    await this.call(
      "Input.dispatchMouseEvent",
      { type: "mouseReleased", x: xn, y: yn, button: "left", clickCount: 1 },
      sessionId,
    );
    await this.cursorMove(sessionId, xn, yn, false);
    this.analysisFlash(sessionId, points, "stroke");
    return points.length;
  }

  // "Finger gesture": a trusted drag from start to goal that moves like a
  // turtle — heading steered toward the goal with a limited turn rate,
  // eased velocity (slow-fast-slow), micro-jitter — instead of a polyline.
  // opts: { speed: px/s cruise (default 900), bend: initial heading offset
  // in degrees (default random ±40), press: drag with button held (true),
  // wobble: 0..1 hand tremor (heading noise + tiny speed flutter, default
  // 0.3), hesitation: 0..1 likelihood of brief mid-gesture pauses (default
  // 0.15) — parametric "finger personalities" for latency/sync testing }.
  async gesture(from, to, opts = {}, filter) {
    const sessionId = await this.session(filter);
    await this.cursorEval(sessionId);
    const speed = opts.speed ?? 900;
    const press = opts.press !== false;
    const [gx, gy] = to;
    let [x, y] = from;
    const bearing = Math.atan2(gy - y, gx - x);
    const bend = ((opts.bend ?? (Math.random() * 80 - 40)) * Math.PI) / 180;
    let heading = bearing + bend;
    const DT = 0.008; // ~120Hz event clock
    const MAX_TURN = 4.2 * DT; // rad per tick
    const total = Math.hypot(gx - x, gy - y);
    const ease = p => 0.25 + 0.75 * Math.sin(Math.min(1, Math.max(0, p)) * Math.PI); // slow-fast-slow
    await this.cursorMove(sessionId, x, y, press);
    if (press)
      await this.call(
        "Input.dispatchMouseEvent",
        { type: "mousePressed", x, y, button: "left", clickCount: 1 },
        sessionId,
      );
    let steps = 0;
    for (; steps < 2000; steps++) {
      const dist = Math.hypot(gx - x, gy - y);
      if (dist < 3) break;
      const want = Math.atan2(gy - y, gx - x);
      let dh = want - heading;
      while (dh > Math.PI) dh -= 2 * Math.PI;
      while (dh < -Math.PI) dh += 2 * Math.PI;
      // tighten steering as the goal nears so the turtle can't orbit it
      const turn = Math.min(Math.abs(dh), MAX_TURN * (1 + (8 * (total - dist)) / total));
      const wobble = opts.wobble ?? 0.3;
      heading += Math.sign(dh) * turn + (Math.random() - 0.5) * 0.08 * wobble;
      const hesitation = opts.hesitation ?? 0.15;
      if (Math.random() < hesitation * 0.015)
        await new Promise(r => setTimeout(r, 60 + Math.random() * 180));
      const flutter = 1 + (Math.random() - 0.5) * 0.3 * wobble;
      const v = speed * ease(1 - dist / total) * DT * flutter;
      x += Math.cos(heading) * Math.min(v, dist);
      y += Math.sin(heading) * Math.min(v, dist);
      // fire-and-forget: the DT tick is the clock; awaiting each dispatch
      // dragged the "120Hz" loop down to 35–55Hz over the tunnels. The
      // page-side follower in cursorEval moves the overlay for free.
      this.callNoWait(
        "Input.dispatchMouseEvent",
        {
          type: "mouseMoved",
          x: Math.round(x),
          y: Math.round(y),
          button: press ? "left" : "none",
          buttons: press ? 1 : 0,
        },
        sessionId,
      );
      await new Promise(r => setTimeout(r, DT * 1000));
    }
    if (press)
      await this.call(
        "Input.dispatchMouseEvent",
        { type: "mouseReleased", x: Math.round(x), y: Math.round(y), button: "left", clickCount: 1 },
        sessionId,
      );
    await this.cursorMove(sessionId, Math.round(x), Math.round(y), false);
    this.analysisFlash(sessionId, [from, [Math.round(x), Math.round(y)]], "gesture");
    return steps;
  }

  // Trusted key press (Input.dispatchKeyEvent). `key` is a DOM key name
  // ("Backspace", "Enter", "a"); modifiers bitmask: 1 alt, 2 ctrl, 4 meta, 8 shift.
  async key(key, modifiers = 0, filter) {
    const sessionId = await this.session(filter);
    const printable = key.length === 1;
    // keyCode AND code matter: apps read e.key, e.keyCode, or e.code — a 0
    // keyCode + missing code made arrow keys invisible to games (stayon).
    const CODES = {
      Backspace: 8, Tab: 9, Enter: 13, Escape: 27, Space: 32, Delete: 46,
      ArrowLeft: 37, ArrowUp: 38, ArrowRight: 39, ArrowDown: 40,
    };
    const base = {
      key,
      modifiers,
      code: printable
        ? (/[a-z]/i.test(key) ? "Key" + key.toUpperCase() : /\d/.test(key) ? "Digit" + key : undefined)
        : key === " " ? "Space" : key,
      windowsVirtualKeyCode:
        CODES[key] ?? (printable ? key.toUpperCase().charCodeAt(0) : 0),
      nativeVirtualKeyCode:
        CODES[key] ?? (printable ? key.toUpperCase().charCodeAt(0) : 0),
    };
    await this.call(
      "Input.dispatchKeyEvent",
      { type: printable ? "keyDown" : "rawKeyDown", ...base, text: printable ? key : undefined },
      sessionId,
    );
    await this.call("Input.dispatchKeyEvent", { type: "keyUp", ...base }, sessionId);
    return key;
  }

  cursorEval(sessionId) {
    return this.call(
      "Runtime.evaluate",
      {
        expression: `(()=>{let c=document.getElementById("__puppet_cursor");
if(!c){c=document.createElement("div");c.id="__puppet_cursor";
c.style.cssText="position:fixed;z-index:2147483647;width:18px;height:18px;border-radius:50%;border:2px solid #fff;background:rgba(255,64,129,.8);pointer-events:none;transform:translate(-50%,-50%);box-shadow:0 0 8px rgba(0,0,0,.5);transition:opacity .4s";
document.documentElement.appendChild(c);
// idle auto-fade: show on movement, fade out 1.2s after the last move so the
// dot never lingers on the page once driving stops (shared by cursorMove).
window.__pcFade=()=>{clearTimeout(window.__pcTimer);c.style.opacity=1;window.__pcTimer=setTimeout(()=>{c.style.opacity=0;},1200);};
// page-side follower: the overlay rides the (trusted) pointer stream itself,
// so drivers never spend a round-trip moving it
document.addEventListener("mousemove",e=>{c.style.left=e.clientX+"px";c.style.top=e.clientY+"px";window.__pcFade();},{passive:true,capture:true});
}return true;})()`,
        returnByValue: true,
      },
      sessionId,
    );
  }

  cursorMove(sessionId, x, y, pressed) {
    return this.call(
      "Runtime.evaluate",
      {
        expression: `(()=>{const c=document.getElementById("__puppet_cursor");
if(c){c.style.left=${x}+"px";c.style.top=${y}+"px";
c.style.background=${pressed} ? "rgba(255,64,129,.9)" : "rgba(255,64,129,.45)";
(window.__pcFade||(()=>{c.style.opacity=1;}))();}return true;})()`,
        returnByValue: true,
      },
      sessionId,
    );
  }

  // Live screencast: Chrome pushes JPEG frames; we keep the LATEST on disk
  // (atomic rename) so "give me a screenshot" is a zero-latency file read.
  async watch(filter, outPath, opts = {}) {
    const sessionId = await this.session(filter);
    const fs = await import("node:fs");
    fs.mkdirSync(dirname(outPath), { recursive: true });
    await this.call("Page.enable", {}, sessionId).catch(() => {});
    // Occluded windows stop painting on macOS → zero damage frames. Surface
    // the page so the screencast actually flows (chicken streamed only
    // because its window happened to be frontmost).
    await this.call("Page.bringToFront", {}, sessionId).catch(() => {});
    this.frameHandlers ??= new Map();
    this.watches ??= new Map(); // sessionId -> outPath (lets shot read the live frame)
    this.watches.set(sessionId, outPath);
    this.frameHandlers.set(sessionId, msg => {
      fs.writeFileSync(outPath + ".tmp", Buffer.from(msg.params.data, "base64"));
      fs.renameSync(outPath + ".tmp", outPath);
      this.call("Page.screencastFrameAck", { sessionId: msg.params.sessionId }, sessionId).catch(() => {});
    });
    await this.call(
      "Page.startScreencast",
      {
        format: "jpeg",
        quality: opts.quality ?? 60, // q60 @ 900w verified legible for terminal text
        maxWidth: opts.maxWidth ?? 900,
        everyNthFrame: opts.everyNthFrame ?? 1, // 2-3 halves encode CPU on animated pieces
      },
      sessionId,
    );
    return `watching → ${outPath}`;
  }

  async unwatch(filter) {
    const sessionId = await this.session(filter);
    await this.call("Page.stopScreencast", {}, sessionId).catch(() => {});
    this.frameHandlers?.delete(sessionId);
    this.watches?.delete(sessionId);
    return "stopped";
  }

  async tailConsole(sock, filter) {
    const sessionId = await this.session(filter);
    await this.call("Runtime.enable", {}, sessionId).catch(() => {});
    this.consoleSubs.add(sock);
    sock.on("close", () => this.consoleSubs.delete(sock));
    sock.on("error", () => this.consoleSubs.delete(sock));
  }

  info() {
    return {
      connected: this.connected,
      lastError: this.lastError,
      targets: [...this.targets.values()].map(t => t.url),
    };
  }
}

const machines = new Map();

function writeStatus() {
  mkdirSync(RUN_DIR, { recursive: true });
  const status = { updatedAt: new Date().toISOString(), machines: {} };
  for (const [name, m] of machines) status.machines[name] = m.info();
  writeFileSync(STATUS_PATH, JSON.stringify(status, null, 2));
}

async function handleRequest(req, sock) {
  const { cmd, machine, args = {} } = req;
  const one = name => {
    const m = machines.get(name);
    if (!m) throw new Error(`unknown machine: ${name}`);
    if (!m.connected) throw new Error(`${name} not connected`);
    return m;
  };
  switch (cmd) {
    case "list": {
      const out = {};
      for (const [name, m] of machines) out[name] = m.info();
      return out;
    }
    case "eval":
      return one(machine).eval(args.expr, args.target);
    // N expressions against one target, one client round-trip (CDP calls
    // pipeline on the open browser socket) — assertion loops in one shot.
    // Poll an expression until truthy — replaces fixed client-side sleeps at
    // coordination points (page loaded? composer open? node count right?).
    // One client round-trip; the daemon polls the open CDP socket at
    // `interval` ms (default 150) until `timeout` (default 15000).
    case "waitFor": {
      const m = one(machine);
      const t0 = Date.now();
      const interval = args.interval ?? 150;
      const timeout = args.timeout ?? 15000;
      for (;;) {
        let v;
        try {
          v = await m.eval(args.expr, args.target);
        } catch {}
        if (v) return { ok: true, value: v, ms: Date.now() - t0 };
        if (Date.now() - t0 > timeout) return { ok: false, ms: Date.now() - t0 };
        await new Promise(r => setTimeout(r, interval));
      }
    }
    case "evalAll": {
      const m = one(machine);
      return Promise.all(
        (args.exprs || []).map(e => m.eval(e, args.target).catch(x => `ERR: ${x.message}`)),
      );
    }
    // Simultaneous action against an explicit set of {machine, target}
    // pairs — the multi-window fan-out broadcast can't express (broadcast
    // picks each machine's most-recent page; fanout pins every window).
    case "fanout": {
      const { pairs, action, expr, points, key } = args;
      const results = await Promise.all(
        pairs.map(async p => {
          try {
            const m = one(p.machine);
            if (action === "eval") return await m.eval(expr, p.target);
            if (action === "stroke") return await m.stroke(points, p.target);
            if (action === "key") return await m.key(key, args.modifiers ?? 0, p.target);
            throw new Error(`unknown fanout action: ${action}`);
          } catch (e) {
            return `ERR: ${e.message}`;
          }
        }),
      );
      return Object.fromEntries(pairs.map((p, i) => [`${p.machine}:${(p.target || "").slice(0, 8)}`, results[i]]));
    }
    case "broadcast": {
      const out = {};
      await Promise.all(
        [...machines.entries()]
          .filter(([, m]) => m.connected)
          .map(async ([name, m]) => {
            out[name] = await m.eval(args.expr, args.target).catch(e => `ERR: ${e.message}`);
          }),
      );
      return out;
    }
    case "nav":
      return one(machine).nav(args.url, args.target);
    case "newtab":
      return one(machine).newtab(args.url, args.window);
    case "close":
      return one(machine).closeTarget(args.target);
    case "reload": {
      if (machine) return one(machine).reload(args.target);
      const out = {};
      await Promise.all(
        [...machines.entries()]
          .filter(([, m]) => m.connected)
          .map(async ([name, m]) => {
            out[name] = await m.reload(args.target).catch(e => `ERR: ${e.message}`);
          }),
      );
      return out;
    }
    case "shot":
      return one(machine).shot(args.target, args);
    case "stroke":
      return one(machine).stroke(args.points, args.target);
    case "key":
      return one(machine).key(args.key, args.modifiers ?? 0, args.target);
    case "gesture":
      return one(machine).gesture(args.from, args.to, args.opts ?? {}, args.target);
    case "cursor": {
      const m = one(machine);
      const sessionId = await m.session(args.target);
      await m.cursorEval(sessionId);
      await m.cursorMove(sessionId, args.x, args.y, false);
      m.analysisFlash(sessionId, [[args.x, args.y]], "cursor");
      return true;
    }
    // analysis on|off [machine|all] — toggle the realtime hit-scan overlay +
    // hit logging per machine. Works on disconnected machines too (just sets
    // the flag); turning off clears any live page-side overlay.
    // scan: draw the observation overlay (text + interactive boxes) on the
    // page so a watcher sees what the agent is reading. Returns {text,targets}.
    case "scan":
      return one(machine).eval(scanExpr(args.ttl ?? 7000), args.target);
    case "analysis": {
      const want = args.on !== false && args.on !== "off";
      const names = !machine || machine === "all" ? [...machines.keys()] : [machine];
      const out = {};
      for (const name of names) {
        const m = machines.get(name);
        if (!m) { out[name] = "unknown"; continue; }
        m.analysisOn = want;
        out[name] = want ? "on" : "off";
        if (!want && m.connected) {
          m.session(args.target)
            .then(sid => m.callNoWait("Runtime.evaluate", { expression: overlayClearExpr() }, sid))
            .catch(() => {});
        }
      }
      return out;
    }
    case "watch":
      return one(machine).watch(args.target, args.out, args.opts ?? {});
    case "unwatch":
      return one(machine).unwatch(args.target);
    case "tail":
      await one(machine).tailConsole(sock, args.target);
      return "__stream__"; // keep socket open
    default:
      throw new Error(`unknown cmd: ${cmd}`);
  }
}

async function cmdDaemon() {
  const cfg = loadConfig();
  if (!cfg || !Object.keys(cfg.machines || {}).length) {
    console.error(`no machines registered — edit ${CONFIG_PATH} (puppet config)`);
    process.exit(1);
  }
  mkdirSync(RUN_DIR, { recursive: true });
  try { unlinkSync(SOCK_PATH); } catch {}
  for (const [name, spec] of Object.entries(cfg.machines)) {
    const m = new Machine(name, spec);
    machines.set(name, m);
    m.connectLoop();
  }
  const server = net.createServer(sock => {
    let buf = "";
    sock.on("data", async chunk => {
      buf += chunk;
      let nl;
      while ((nl = buf.indexOf("\n")) >= 0) {
        const line = buf.slice(0, nl);
        buf = buf.slice(nl + 1);
        if (!line.trim()) continue;
        let req;
        try {
          req = JSON.parse(line);
        } catch {
          sock.write(JSON.stringify({ error: "bad json" }) + "\n");
          continue;
        }
        try {
          const result = await handleRequest(req, sock);
          if (result !== "__stream__")
            sock.write(JSON.stringify({ id: req.id, result }) + "\n");
        } catch (err) {
          sock.write(JSON.stringify({ id: req.id, error: String(err.message || err) }) + "\n");
        }
      }
    });
    sock.on("error", () => {});
  });
  server.listen(SOCK_PATH, () => console.log(`puppet daemon listening on ${SOCK_PATH}`));
  writeStatus();
  setInterval(writeStatus, 10000);
}

// ─── CLI client ──────────────────────────────────────────────────────────

function rpc(req, { stream = false, timeoutMs = 20000 } = {}) {
  return new Promise((resolve, reject) => {
    const sock = net.createConnection(SOCK_PATH);
    // A daemon mid-reconnect (or a stale socket) that never replies must
    // FAIL FAST — an untimed rpc wedged an 8-window fan-out script forever.
    const timer = stream
      ? null
      : setTimeout(() => {
          sock.destroy();
          reject(new Error(`rpc timeout after ${timeoutMs}ms (${req.cmd})`));
        }, timeoutMs);
    sock.on("error", () => {
      if (timer) clearTimeout(timer);
      reject(new Error("daemon not running — start with: puppet daemon"));
    });
    let buf = "";
    sock.on("data", chunk => {
      buf += chunk;
      let nl;
      while ((nl = buf.indexOf("\n")) >= 0) {
        const line = buf.slice(0, nl);
        buf = buf.slice(nl + 1);
        if (stream) {
          console.log(line);
          continue;
        }
        sock.end();
        if (timer) clearTimeout(timer);
        const msg = JSON.parse(line);
        if (msg.error) reject(new Error(msg.error));
        else resolve(msg.result);
      }
    });
    sock.write(JSON.stringify(req) + "\n");
    if (stream) sock.on("close", () => resolve());
  });
}

function parseFlags(argv) {
  const flags = {};
  const rest = [];
  for (const a of argv) {
    const m = a.match(/^--([^=]+)=(.*)$/);
    if (m) flags[m[1]] = m[2];
    else if (/^--[^=]+$/.test(a)) flags[a.slice(2)] = "1"; // bare boolean flag
    else rest.push(a);
  }
  return { flags, rest };
}

async function main() {
  const { flags, rest } = parseFlags(process.argv.slice(2));
  const [cmd, ...args] = rest;
  switch (cmd) {
    case "daemon":
      return cmdDaemon();
    case "config":
      return cmdConfig();
    case "status":
      console.log(readFileSync(STATUS_PATH, "utf8"));
      return;
    case "list":
      console.log(JSON.stringify(await rpc({ cmd: "list" }), null, 2));
      return;
    case "eval":
      console.log(
        JSON.stringify(
          await rpc({ cmd: "eval", machine: args[0], args: { expr: args[1], target: flags.target } }),
        ),
      );
      return;
    // puppet waitfor <machine> <expr> [--timeout=15000] [--interval=150]
    case "waitfor":
      console.log(
        JSON.stringify(
          await rpc({
            cmd: "waitFor",
            machine: args[0],
            args: { expr: args[1], target: flags.target, timeout: Number(flags.timeout || 15000), interval: Number(flags.interval || 150) },
          }),
        ),
      );
      return;
    // puppet evalall <machine> <expr1> <expr2> ... — one round-trip
    case "evalall":
      console.log(
        JSON.stringify(
          await rpc({ cmd: "evalAll", machine: args[0], args: { exprs: args.slice(1), target: flags.target } }),
          null,
          2,
        ),
      );
      return;
    // puppet batch — JSON-lines requests on stdin, replies on stdout,
    // all over ONE daemon connection (kills the per-call spawn cost for
    // scripted loops: pipe a heredoc of {cmd,machine,args} lines in).
    case "batch": {
      const { createInterface } = await import("node:readline");
      const sock2 = net.createConnection(SOCK_PATH);
      let buf2 = "";
      let received = 0;
      sock2.on("data", c => {
        buf2 += c;
        let nl;
        while ((nl = buf2.indexOf("\n")) >= 0) {
          console.log(buf2.slice(0, nl));
          buf2 = buf2.slice(nl + 1);
          received++;
        }
      });
      const rl = createInterface({ input: process.stdin });
      let sent = 0;
      for await (const line of rl) {
        if (line.trim()) {
          sock2.write(line + "\n");
          sent++;
        }
      }
      // exit as soon as every request has answered (10s safety net)
      await new Promise(r => {
        const deadline = setTimeout(r, 10000);
        const tick = setInterval(() => {
          if (received >= sent) { clearTimeout(deadline); clearInterval(tick); r(); }
        }, 5);
      });
      sock2.end();
      return;
    }
    case "broadcast":
      console.log(
        JSON.stringify(
          await rpc({ cmd: "broadcast", args: { expr: args[0], target: flags.target } }),
          null,
          2,
        ),
      );
      return;
    // puppet fanout eval '<js>' machine:target [machine:target ...]
    // puppet fanout stroke x1,y1[ x2,y2 ...] machine:target [...]
    // puppet fanout key <Key> machine:target [...]
    case "fanout": {
      const action = args[0];
      let expr, points, key, pairStart;
      if (action === "eval") { expr = args[1]; pairStart = 2; }
      else if (action === "key") { key = args[1]; pairStart = 2; }
      else if (action === "stroke") {
        points = [];
        let i = 1;
        while (args[i] && /^[\d.]+,[\d.]+$/.test(args[i])) points.push(args[i++].split(",").map(Number));
        pairStart = i;
      } else { console.error("fanout action must be eval|stroke|key"); return; }
      const pairs = args.slice(pairStart).map(s => {
        const i = s.indexOf(":");
        return { machine: s.slice(0, i), target: s.slice(i + 1) };
      });
      console.log(
        JSON.stringify(
          await rpc({ cmd: "fanout", args: { pairs, action, expr, points, key, modifiers: Number(flags.mod || 0) } }),
          null,
          2,
        ),
      );
      return;
    }
    case "nav":
      console.log(await rpc({ cmd: "nav", machine: args[0], args: { url: args[1], target: flags.target } }));
      return;
    case "reload":
      console.log(
        JSON.stringify(await rpc({ cmd: "reload", machine: args[0], args: { target: flags.target } })),
      );
      return;
    case "close":
      console.log(await rpc({ cmd: "close", machine: args[0], args: { target: args[1] || flags.target } }));
      return;
    case "newtab": {
      let win;
      if (flags.window) {
        const [left, top, width, height] = flags.window.split(",").map(Number);
        win = { left, top, width, height };
      }
      console.log(await rpc({ cmd: "newtab", machine: args[0], args: { url: args[1], window: win } }));
      return;
    }
    case "shot": {
      const out = args[1];
      // --fullscreen / --screen: capture the WHOLE screen (all windows, any
      // native app) + OCR via the SlabMenubar frame mechanism, not the CDP
      // per-page screenshot. Delegates to the sibling frame.mjs (which talks to
      // the on-target app that holds Screen Recording + AX trust — plain ssh
      // screencapture can't reach the WindowServer). Writes the JPEG to `out`
      // and an `<out>.ocr.json` sidecar; prints an OCR summary.
      if (flags.fullscreen || flags.screen) {
        const framePath = join(dirname(fileURLToPath(import.meta.url)), "frame.mjs");
        const jpgOut = /\.jpe?g$/i.test(out || "") ? out : (out || "frame") + ".jpg";
        const fArgs = [framePath, args[0], "--out", jpgOut, "--json"];
        if (flags.fast) fArgs.push("--fast");
        if (flags["no-ocr"]) fArgs.push("--no-ocr");
        let envJson;
        try {
          envJson = execFileSync("node", fArgs, { encoding: "utf8", maxBuffer: 64 * 1024 * 1024 });
        } catch (e) {
          console.error(`fullscreen capture failed: ${String(e.message).split("\n")[0]}`);
          process.exit(1);
        }
        let env = {};
        try { env = JSON.parse(envJson); } catch {}
        // The frame envelope carries the image as base64 (thumb_jpg_b64); the
        // ACF1 binary jpg can be empty, so frame.mjs may not have written a
        // file — decode + write it here when needed.
        if (!existsSync(jpgOut) && env.thumb_jpg_b64) {
          writeFileSync(jpgOut, Buffer.from(env.thumb_jpg_b64, "base64"));
        }
        const ocr = env.ocr || [];
        if (ocr.length) writeFileSync(jpgOut + ".ocr.json", JSON.stringify(ocr, null, 2));
        const wrote = existsSync(jpgOut);
        console.log(`${wrote ? "wrote " + jpgOut : "(no pixels)"} (fullscreen) — ${ocr.length} OCR runs${ocr.length ? `, → ${jpgOut}.ocr.json` : ""}` +
          (env.capture === "permission_needed" ? " ⚠ Screen Recording not granted (frame setup " + args[0] + ")" : ""));
        return;
      }
      // format inferred from extension (.jpg → jpeg); --fresh=1 forces a
      // real capture instead of returning a live watch frame.
      const format = flags.format || (/\.jpe?g$/i.test(out || "") ? "jpeg" : "png");
      const b64 = await rpc({
        cmd: "shot",
        machine: args[0],
        args: { target: flags.target, format, quality: Number(flags.quality || 80), fresh: flags.fresh === "1" },
      });
      writeFileSync(out, Buffer.from(b64, "base64"));
      console.log(`wrote ${out}`);
      return;
    }
    case "stroke": {
      const points = args.slice(1).map(p => p.split(",").map(Number));
      console.log(
        await rpc({ cmd: "stroke", machine: args[0], args: { points, target: flags.target } }),
        "points dispatched",
      );
      return;
    }
    case "key":
      console.log(
        await rpc({
          cmd: "key",
          machine: args[0],
          args: { key: args[1], modifiers: Number(flags.mod || 0), target: flags.target },
        }),
      );
      return;
    case "gesture": {
      const from = args[1].split(",").map(Number);
      const to = args[2].split(",").map(Number);
      const opts = {};
      for (const k of ["speed", "bend", "wobble", "hesitation"])
        if (flags[k] !== undefined) opts[k] = Number(flags[k]);
      console.log(
        await rpc({ cmd: "gesture", machine: args[0], args: { from, to, opts, target: flags.target } }),
        "steps",
      );
      return;
    }
    case "cursor":
      await rpc({
        cmd: "cursor",
        machine: args[0],
        args: { x: Number(args[1]), y: Number(args[2]), target: flags.target },
      });
      console.log("cursor placed");
      return;
    // puppet scan <machine> [--ttl=2600]  — draw the observation overlay
    case "scan":
      console.log(
        await rpc({ cmd: "scan", machine: args[0], args: { ttl: flags.ttl ? Number(flags.ttl) : undefined, target: flags.target } }),
      );
      return;
    // puppet analysis on|off [machine|all]  — realtime hit-scan overlay layer
    case "analysis": {
      const on = (args[0] || "on").toLowerCase() !== "off";
      const machine = args[1] || "all";
      console.log(
        JSON.stringify(await rpc({ cmd: "analysis", machine, args: { on, target: flags.target } }), null, 2),
      );
      return;
    }
    case "watch":
      console.log(
        await rpc({
          cmd: "watch",
          machine: args[0],
          args: { target: flags.target, out: args[1], opts: { quality: Number(flags.quality || 60), maxWidth: Number(flags.maxwidth || 900), everyNthFrame: Number(flags.nth || 1) } },
        }),
      );
      return;
    case "unwatch":
      console.log(await rpc({ cmd: "unwatch", machine: args[0], args: { target: flags.target } }));
      return;
    case "tail":
      await rpc({ cmd: "tail", machine: args[0], args: { target: flags.target } }, { stream: true });
      return;
    default:
      console.log(
        "usage: puppet daemon|config|status|list|eval|evalall|batch|broadcast|fanout|nav|newtab|close|reload|shot|watch|unwatch|stroke|gesture|key|cursor|scan|analysis|tail (see header)",
      );
  }
}

main().catch(err => {
  console.error(String(err.message || err));
  process.exit(1);
});
