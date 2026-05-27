#!/usr/bin/env node
// arena-probe.mjs — record an ac-electron arena.mjs session via CDP and
// dump everything an offline analysis needs to diagnose movement jerkiness.
//
// What it captures, per run:
//   • frames/NNNN.jpg              CDP Page.startScreencast at 30 fps
//   • console.log                  Runtime.consoleAPICalled lines
//   • perfstats.json               polled snapshots of window.__arena_perfStats()
//   • report.txt                   terminal summary of the above
//
// Prereqs:
//   1. ac-electron running with --remote-debugging-port=9222 (the default
//      from `npm start` / `npm start:prod`).
//   2. The AC pane is on the `arena` piece. The probe does NOT navigate;
//      keeping it minimal means the script reports cleanly when no perf
//      data is being published rather than fighting your live state.
//
// Usage:
//   node artery/arena-probe.mjs [--duration 5] [--port 9222] [--out path]
//                               [--fps 30] [--quality 70]
//                               [--max-width 960] [--no-frames]
//                               [--enter arena] [--reset]
//                               [--pattern square|wasd-burst|spin|none]
//
// --reset       Unregister service workers, drop caches, hard-reload the
//               webview (cache-bypass) before navigating. Useful when a stale
//               arena.mjs is being served from the SW.

import WebSocket from "ws";
import fetch from "node-fetch";
import fs from "fs/promises";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const ARGS = parseArgs(process.argv.slice(2));
const PORT = ARGS.port ? parseInt(ARGS.port, 10) : 9222;
const DURATION_S = ARGS.duration ? parseFloat(ARGS.duration) : 5;
const FPS = ARGS.fps ? parseInt(ARGS.fps, 10) : 30;
const QUALITY = ARGS.quality ? parseInt(ARGS.quality, 10) : 70;
const MAX_WIDTH = ARGS["max-width"] ? parseInt(ARGS["max-width"], 10) : 960;
const CAPTURE_FRAMES = !ARGS["no-frames"];
const ENTER_PIECE = typeof ARGS.enter === "string" ? ARGS.enter : null;
const PATTERN = typeof ARGS.pattern === "string" ? ARGS.pattern : "square";
const RESET_BEFORE = !!ARGS.reset;
const OUT_BASE = ARGS.out
  ? path.resolve(process.cwd(), ARGS.out)
  : path.join(__dirname, "runs", isoStamp());

function parseArgs(argv) {
  const out = {};
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (!a.startsWith("--")) continue;
    const key = a.replace(/^--/, "");
    const next = argv[i + 1];
    if (next === undefined || next.startsWith("--")) {
      out[key] = true;
    } else {
      out[key] = next;
      i++;
    }
  }
  return out;
}

function isoStamp() {
  const d = new Date();
  return d
    .toISOString()
    .replace(/[:.]/g, "-")
    .replace(/Z$/, "");
}

async function findArenaTarget(port, { retries = 20, retryDelayMs = 500 } = {}) {
  for (let attempt = 0; attempt < retries; attempt++) {
    let targets;
    try {
      const res = await fetch(`http://localhost:${port}/json/list`);
      if (!res.ok) throw new Error(`CDP /json not reachable (${res.status})`);
      targets = await res.json();
    } catch (e) {
      if (attempt === retries - 1) throw e;
      await new Promise((r) => setTimeout(r, retryDelayMs));
      continue;
    }
    // Service workers and other non-page contexts don't carry the AC page
    // state — exclude them outright. Only webview/page are valid here.
    const ranked = targets
      .filter((t) => t.webSocketDebuggerUrl)
      .filter((t) => t.type === "webview" || t.type === "page")
      .map((t) => {
        let score = 0;
        const url = t.url || "";
        if (t.type === "webview") score += 4;
        if (t.type === "page" && !url.startsWith("file://")) score += 1;
        if (url.includes("aesthetic.computer")) score += 2;
        if (url.includes("localhost:8888")) score += 2;
        return { t, score };
      })
      .filter((r) => r.score > 0)
      .sort((a, b) => b.score - a.score);
    if (ranked.length) return ranked[0].t;
    await new Promise((r) => setTimeout(r, retryDelayMs));
  }
  // Final attempt: report everything we saw to help debug.
  const res = await fetch(`http://localhost:${port}/json/list`);
  const targets = await res.json();
  throw new Error(
    "No aesthetic.computer page/webview CDP target found after retries.\n" +
      `targets discovered:\n${targets
        .map((t) => `  - ${t.type} ${t.url}`)
        .join("\n")}`,
  );
}

class CDPClient {
  constructor(wsUrl) {
    this.ws = new WebSocket(wsUrl);
    this.nextId = 1;
    this.pending = new Map();
    this.eventListeners = new Map();
    this._ready = new Promise((resolve, reject) => {
      this.ws.once("open", resolve);
      this.ws.once("error", reject);
    });
    this.ws.on("message", (raw) => this._onMessage(raw));
  }
  ready() {
    return this._ready;
  }
  _onMessage(raw) {
    let msg;
    try { msg = JSON.parse(raw); } catch { return; }
    if (msg.id !== undefined) {
      const cb = this.pending.get(msg.id);
      if (cb) {
        this.pending.delete(msg.id);
        if (msg.error) cb.reject(new Error(msg.error.message));
        else cb.resolve(msg.result);
      }
      return;
    }
    if (msg.method) {
      const ls = this.eventListeners.get(msg.method);
      if (ls) for (const fn of ls) fn(msg.params || {});
    }
  }
  send(method, params = {}) {
    const id = this.nextId++;
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }
  on(method, fn) {
    if (!this.eventListeners.has(method)) this.eventListeners.set(method, []);
    this.eventListeners.get(method).push(fn);
  }
  close() {
    try { this.ws.close(); } catch {}
  }
}

// CDP key descriptors — what Chromium expects per Input.dispatchKeyEvent.
const KEYS = {
  w: { key: "w", code: "KeyW", windowsVirtualKeyCode: 87, nativeVirtualKeyCode: 87 },
  a: { key: "a", code: "KeyA", windowsVirtualKeyCode: 65, nativeVirtualKeyCode: 65 },
  s: { key: "s", code: "KeyS", windowsVirtualKeyCode: 83, nativeVirtualKeyCode: 83 },
  d: { key: "d", code: "KeyD", windowsVirtualKeyCode: 68, nativeVirtualKeyCode: 68 },
  space: { key: " ", code: "Space", windowsVirtualKeyCode: 32, nativeVirtualKeyCode: 32 },
};

async function keyDown(cdp, name) {
  const k = KEYS[name];
  if (!k) return;
  await cdp.send("Input.dispatchKeyEvent", {
    type: "keyDown",
    key: k.key,
    code: k.code,
    text: k.key,
    windowsVirtualKeyCode: k.windowsVirtualKeyCode,
    nativeVirtualKeyCode: k.nativeVirtualKeyCode,
  });
}
async function keyUp(cdp, name) {
  const k = KEYS[name];
  if (!k) return;
  await cdp.send("Input.dispatchKeyEvent", {
    type: "keyUp",
    key: k.key,
    code: k.code,
    windowsVirtualKeyCode: k.windowsVirtualKeyCode,
    nativeVirtualKeyCode: k.nativeVirtualKeyCode,
  });
}
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Drive a movement pattern for the full duration. Returns when done.
// Patterns:
//   square    — hold W, D, S, A each for legSec seconds, loop
//   wasd-burst — quick 250ms taps cycling W A S D, loop
//   spin      — D held while tapping W (mouse-yaw substitute via repeated turns)
//   none      — no input (passive recording, user drives)
async function runMovementPattern(cdp, pattern, durationMs, log) {
  if (pattern === "none") return;
  const deadline = Date.now() + durationMs;
  const legSec = 1.0;
  const sequences = {
    square: ["w", "d", "s", "a"],
    "wasd-burst": ["w", "a", "s", "d", "w", "d"],
    spin: ["d"], // D held, W tapped via tapKey
  };
  const seq = sequences[pattern] || sequences.square;
  log(`[input] pattern=${pattern} keys=${seq.join("")}`);
  const held = new Set();
  try {
    while (Date.now() < deadline) {
      for (const k of seq) {
        if (Date.now() >= deadline) break;
        held.add(k);
        await keyDown(cdp, k);
        const legDur = Math.min(legSec * 1000, deadline - Date.now());
        if (legDur <= 0) break;
        if (pattern === "wasd-burst") {
          await sleep(Math.min(250, legDur));
        } else if (pattern === "spin") {
          // hold D, tap W repeatedly to interleave forward bursts
          const wDeadline = Date.now() + legDur;
          while (Date.now() < wDeadline) {
            await keyDown(cdp, "w");
            await sleep(120);
            await keyUp(cdp, "w");
            await sleep(60);
          }
        } else {
          await sleep(legDur);
        }
        await keyUp(cdp, k);
        held.delete(k);
      }
    }
  } finally {
    for (const k of [...held]) {
      try { await keyUp(cdp, k); } catch {}
    }
  }
}

async function resetWebview(cdp, log) {
  log("[reset] unregistering service workers + dropping caches...");
  await cdp.send("Runtime.evaluate", {
    expression: `(async () => {
      try {
        const rs = await navigator.serviceWorker.getRegistrations();
        await Promise.all(rs.map(r => r.unregister()));
      } catch (e) {}
      try {
        if (window.caches && caches.keys) {
          const ks = await caches.keys();
          await Promise.all(ks.map(k => caches.delete(k)));
        }
      } catch (e) {}
    })()`,
    returnByValue: true, awaitPromise: true,
  });
  log("[reset] hard-reloading webview (ignoreCache)...");
  await cdp.send("Page.reload", { ignoreCache: true });
  // Wait for the JS context to come back. We re-probe via /json/list rather
  // than reusing the WS — Page.reload can invalidate the existing one.
  await sleep(1500);
  return null; // caller reconnects.
}

async function enterPiece(cdp, piece, log) {
  // bios.mjs sets up window.acDISK_SEND early — it queues messages until the
  // worker is consumed-ready, then flushes them. Plain window.acSEND can
  // arrive at the worker before $commonApi.jump is wired and silently drop.
  const senderDeadline = Date.now() + 12000;
  let useDiskSend = false;
  while (Date.now() < senderDeadline) {
    const r = await cdp.send("Runtime.evaluate", {
      expression: `JSON.stringify({ diskSend: typeof window.acDISK_SEND, acSend: typeof window.acSEND })`,
      returnByValue: true,
    });
    const v = JSON.parse(r.result?.value || "{}");
    if (v.diskSend === "function") { useDiskSend = true; break; }
    if (v.acSend === "function") break;
    await sleep(150);
  }
  log(`[nav] sending navigate via ${useDiskSend ? "acDISK_SEND (queued)" : "acSEND (raw)"} → ${piece}`);
  const fn = useDiskSend ? "window.acDISK_SEND" : "window.acSEND";
  await cdp.send("Runtime.evaluate", {
    expression: `${fn}({type:'navigate', content:{to: ${JSON.stringify(piece)}}})`,
  });
  // Wait for arena.mjs perf hook to publish — that's the readiness signal.
  const start = Date.now();
  while (Date.now() - start < 12000) {
    const r = await cdp.send("Runtime.evaluate", {
      expression: `JSON.stringify({hook: typeof window.__arena_perfStats, latest: !!window.__arena_perfStats_latest, title: document.title})`,
      returnByValue: true,
    });
    try {
      const v = JSON.parse(r.result?.value || "{}");
      if (v.hook === "function") {
        log(`[nav] arena perf hook live after ${Date.now() - start}ms (title="${v.title}")`);
        return true;
      }
    } catch {}
    await sleep(250);
  }
  log("[nav] WARN arena perf hook not detected after 12s — continuing anyway");
  return false;
}

async function main() {
  await fs.mkdir(OUT_BASE, { recursive: true });
  if (CAPTURE_FRAMES) await fs.mkdir(path.join(OUT_BASE, "frames"), { recursive: true });

  let target = await findArenaTarget(PORT);
  console.log(`[probe] target: ${target.type} ${target.url}`);
  console.log(`[probe] run dir: ${OUT_BASE}`);

  let cdp = new CDPClient(target.webSocketDebuggerUrl);
  await cdp.ready();
  await cdp.send("Runtime.enable");
  await cdp.send("Page.enable");

  if (RESET_BEFORE) {
    await resetWebview(cdp, console.log);
    cdp.close();
    // Re-discover after the reload (target id can change on Page.reload).
    let retries = 30;
    while (retries-- > 0) {
      try {
        target = await findArenaTarget(PORT);
        cdp = new CDPClient(target.webSocketDebuggerUrl);
        await cdp.ready();
        await cdp.send("Runtime.enable");
        await cdp.send("Page.enable");
        const probe = await cdp.send("Runtime.evaluate", {
          expression: `typeof window.acSEND`,
          returnByValue: true,
        });
        if (probe.result?.value === "function") break;
      } catch {}
      await sleep(500);
    }
    console.log(`[reset] reconnected target: ${target.url}`);
  }

  // Auto-navigate if requested.
  if (ENTER_PIECE) {
    await enterPiece(cdp, ENTER_PIECE, console.log);
    // Settle pause so initial-load frame stutter doesn't pollute the report.
    await sleep(500);
  }

  // Console capture.
  const consoleLines = [];
  cdp.on("Runtime.consoleAPICalled", (params) => {
    const text = (params.args || [])
      .map((a) => {
        if (a.type === "string") return a.value;
        if (a.unserializableValue) return a.unserializableValue;
        if (a.value !== undefined) return JSON.stringify(a.value);
        return a.description || "";
      })
      .join(" ");
    const line = `[${new Date(params.timestamp).toISOString()}] [${params.type}] ${text}`;
    consoleLines.push(line);
  });
  cdp.on("Runtime.exceptionThrown", (params) => {
    const d = params.exceptionDetails || {};
    const text = d.exception?.description || d.text || "<exception>";
    consoleLines.push(`[exception] ${text}`);
  });

  // Screencast.
  let frameIdx = 0;
  if (CAPTURE_FRAMES) {
    cdp.on("Page.screencastFrame", async (params) => {
      const idx = ++frameIdx;
      const buf = Buffer.from(params.data, "base64");
      try {
        await fs.writeFile(
          path.join(OUT_BASE, "frames", `${String(idx).padStart(5, "0")}.jpg`),
          buf,
        );
      } catch (e) {
        console.warn("[probe] frame write failed:", e.message);
      }
      try {
        await cdp.send("Page.screencastFrameAck", { sessionId: params.sessionId });
      } catch (e) {
        // session can already be gone if we stopped — ignore.
      }
    });
    await cdp.send("Page.startScreencast", {
      format: "jpeg",
      quality: QUALITY,
      maxWidth: MAX_WIDTH,
      everyNthFrame: Math.max(1, Math.round(60 / FPS)),
    });
  }

  // Poll perfStats once per ~250ms. Keep all snapshots — analysis is offline.
  const perfPolls = [];
  const POLL_INTERVAL_MS = 250;
  let perfMissingWarned = false;
  const pollPerf = async () => {
    try {
      const r = await cdp.send("Runtime.evaluate", {
        expression:
          "(typeof window.__arena_perfStats === 'function' ? window.__arena_perfStats() : { samples: [], meta: { absent: true } })",
        returnByValue: true,
      });
      const value = r.result?.value;
      if (value?.meta?.absent && !perfMissingWarned) {
        perfMissingWarned = true;
        console.warn(
          "[probe] window.__arena_perfStats not found — is the arena piece loaded?",
        );
      }
      if (value) {
        perfPolls.push({ pollT: Date.now(), ...value });
      }
    } catch (e) {
      // Eval can fail mid-navigation — silent retry.
    }
  };
  await pollPerf(); // initial sample so we always have at least one
  const pollTimer = setInterval(pollPerf, POLL_INTERVAL_MS);

  console.log(
    `[probe] recording ${DURATION_S}s ${PATTERN === "none" ? "(passive)" : `(driving '${PATTERN}')`} ...`,
  );
  // Drive input + wait on the duration in parallel so input runs are bounded
  // by the same deadline as recording.
  await Promise.all([
    sleep(DURATION_S * 1000),
    runMovementPattern(cdp, PATTERN, DURATION_S * 1000, console.log),
  ]);

  clearInterval(pollTimer);
  await pollPerf(); // final sample

  if (CAPTURE_FRAMES) {
    try { await cdp.send("Page.stopScreencast"); } catch {}
  }
  cdp.close();

  // Write artifacts.
  await fs.writeFile(
    path.join(OUT_BASE, "console.log"),
    consoleLines.join("\n") + "\n",
  );
  await fs.writeFile(
    path.join(OUT_BASE, "perfstats.json"),
    JSON.stringify(perfPolls, null, 2),
  );

  // Summary.
  const summary = summarize(perfPolls);
  const report = buildReport({
    target,
    out: OUT_BASE,
    durationS: DURATION_S,
    fps: FPS,
    quality: QUALITY,
    maxWidth: MAX_WIDTH,
    frames: frameIdx,
    consoleLines: consoleLines.length,
    summary,
  });
  await fs.writeFile(path.join(OUT_BASE, "report.txt"), report);

  console.log("\n" + report);
  console.log(`\n[probe] frames=${frameIdx} consoleLines=${consoleLines.length}`);
  console.log(`[probe] artifacts in ${OUT_BASE}`);
}

function summarize(perfPolls) {
  // Stitch samples — different polls overlap; use the latest perfStats blob
  // because the ring is monotonic in `t` and de-duplicated by it.
  const seen = new Map(); // t -> sample
  for (const blob of perfPolls) {
    for (const s of blob.samples || []) {
      if (s && typeof s.t === "number") seen.set(s.t, s);
    }
  }
  const samples = [...seen.values()].sort((a, b) => a.t - b.t);

  if (samples.length === 0) {
    return {
      frames: 0,
      note: "no perf samples — arena.mjs may not be the active piece, or the perf hook didn't initialize",
    };
  }

  const dts = samples.map((s) => s.dt).filter((x) => Number.isFinite(x));
  const reconDists = samples.map((s) => s.reconDist || 0);
  const pings = samples.map((s) => s.ping).filter((x) => x > 0);
  const snapAges = samples.map((s) => s.snapAge).filter((x) => x >= 0);
  const pending = samples.map((s) => s.pending || 0);
  const reconKinds = {};
  for (const s of samples) {
    const k = s.reconKind || "none";
    reconKinds[k] = (reconKinds[k] || 0) + 1;
  }

  return {
    frames: samples.length,
    spanS: samples.length > 1
      ? (samples[samples.length - 1].t - samples[0].t) / 1000
      : 0,
    dt: stat(dts),
    fps: dts.length
      ? { mean: 1000 / mean(dts), min: 1000 / max(dts), max: 1000 / min(dts) }
      : null,
    reconDist: stat(reconDists),
    reconKinds,
    pendingCmds: stat(pending),
    ping: stat(pings),
    snapAge: stat(snapAges),
    final: samples[samples.length - 1],
  };
}

function mean(xs) { return xs.reduce((a, b) => a + b, 0) / xs.length; }
function min(xs) { return Math.min(...xs); }
function max(xs) { return Math.max(...xs); }
function pct(xs, p) {
  const s = [...xs].sort((a, b) => a - b);
  const i = Math.min(s.length - 1, Math.floor((p / 100) * s.length));
  return s[i];
}
function std(xs) {
  if (xs.length < 2) return 0;
  const m = mean(xs);
  return Math.sqrt(xs.reduce((a, b) => a + (b - m) ** 2, 0) / (xs.length - 1));
}
function stat(xs) {
  if (!xs.length) return null;
  return {
    n: xs.length,
    mean: round(mean(xs)),
    std: round(std(xs)),
    min: round(min(xs)),
    p50: round(pct(xs, 50)),
    p95: round(pct(xs, 95)),
    max: round(max(xs)),
  };
}
function round(x) {
  if (!Number.isFinite(x)) return null;
  return Math.round(x * 100) / 100;
}

function buildReport(ctx) {
  const s = ctx.summary;
  const lines = [];
  lines.push(`arena-probe report — ${new Date().toISOString()}`);
  lines.push(
    `target:  ${ctx.target.type} ${ctx.target.url}`,
  );
  lines.push(`run:     ${ctx.out}`);
  lines.push(
    `params:  duration=${ctx.durationS}s screencast=${ctx.fps}fps q=${ctx.quality} maxW=${ctx.maxWidth}`,
  );
  lines.push(`frames:  ${ctx.frames}   consoleLines: ${ctx.consoleLines}`);
  lines.push("");
  if (s.note) {
    lines.push(`! ${s.note}`);
    return lines.join("\n");
  }
  lines.push(`perf samples: ${s.frames}    span: ${s.spanS}s`);
  lines.push("");
  lines.push(`frame dt (ms):    ${fmt(s.dt)}`);
  if (s.fps) {
    lines.push(
      `fps:              mean=${round(s.fps.mean)}  min=${round(s.fps.min)}  max=${round(s.fps.max)}`,
    );
  }
  lines.push(`pending cmds:     ${fmt(s.pendingCmds)}`);
  lines.push(`ping (ms):        ${fmt(s.ping)}`);
  lines.push(`snap age (ms):    ${fmt(s.snapAge)}`);
  lines.push(`reconcile dist:   ${fmt(s.reconDist)}`);
  lines.push(`reconcile kinds:  ${JSON.stringify(s.reconKinds)}`);
  lines.push("");
  lines.push(`tail sample:      ${JSON.stringify(s.final)}`);
  lines.push("");
  lines.push("interpretation cheats:");
  lines.push("  • dt.std high (>5ms) with low max ⇒ render stutter / GC");
  lines.push("  • snap kind > 0           ⇒ hard server reconcile snaps (teleport-feel)");
  lines.push("  • reconDist p95 high (>0.6) with mostly soft ⇒ persistent prediction drift");
  lines.push("  • snapAge p95 ≫ 33ms     ⇒ UDP/WS snapshot starvation");
  lines.push("  • pending > 6 sustained  ⇒ input lag (cmds backed up unacked)");
  return lines.join("\n");
}

function fmt(s) {
  if (!s) return "(no data)";
  return `n=${s.n}  mean=${s.mean}  std=${s.std}  min=${s.min}  p50=${s.p50}  p95=${s.p95}  max=${s.max}`;
}

main().catch((err) => {
  console.error("[probe] error:", err.stack || err.message || err);
  process.exit(1);
});
