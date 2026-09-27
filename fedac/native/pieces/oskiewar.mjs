// Oskiewar, 26.09.24
// Live native client. Build/upload with tools/oskiewar-live.mjs.
import { createNativeHost } from "../lib/oskiewar-host.mjs";
import { createOskiewar, sourceHash } from "../lib/oskiewar-game.mjs";

let host, game, lastSimAt, pendingMs;
let profile = { simMs: 0, paintMs: 0, flushMs: 0, frames: 0, startedAt: 0 };
let recentPerf = {}, frameIntervals = [], lastPaintAt = 0, netBaseline = null;
function boot(api) {
  globalThis.__oskiewarNativeMath = api.oskiewarMath;
  host = createNativeHost(api);
  globalThis.__oskiewarOpponent = "dummy";
  globalThis.__oskiewarDebugOverlay = api.system.readFile("/tmp/oskiewar-debug") === "1";
  globalThis.__oskiewarLanTest = null;
  globalThis.__oskiewarNetInbox = [];
  globalThis.__oskiewarNetStats = null;
  globalThis.__oskiewarLanStatus = null;
  globalThis.__oskiewarLanMismatch = null;
  try {
    const config = JSON.parse(api.system.readFile("/tmp/oskiewar-lan-test.json") || "null");
    if (config?.seat === 1 && /^ow-[a-z]{4,7}[0-9]{1,3}$/.test(config.room) &&
        Number.isFinite(config.expires) && Date.now() < config.expires) {
      globalThis.__oskiewarLanTest = config;
      host.connectNet(config.room);
    }
  } catch (_) {}
  globalThis.__oskiewarNetSend = (room, packet) => host.sendNet(room, packet);
  game = createOskiewar(host);
  game.boot();
  game.enterGame();
  lastSimAt = Date.now(); pendingMs = 0;
  profile = { simMs: 0, paintMs: 0, flushMs: 0, frames: 0, startedAt: lastSimAt };
  recentPerf = {}; frameIntervals = []; lastPaintAt = 0; netBaseline = null;
}
function sim(api) {
  host.update(api);
  host.readGamepads();
  const now = Date.now();
  pendingMs += Math.max(0, Math.min(100, now - lastSimAt));
  lastSimAt = now;
  while (pendingMs >= 1000 / 60) {
    host.step(); game.sim(); pendingMs -= 1000 / 60;
  }
  profile.simMs += Date.now() - now;
}
function paint(api) {
  host.update(api);
  host.readNet();
  host.begin();
  const start = Date.now();
  if (lastPaintAt) frameIntervals.push(start - lastPaintAt);
  lastPaintAt = start;
  game.paint();
  const painted = Date.now();
  profile.paintMs += painted - start;
  profile.frames++;
  const n = profile.frames;
  const state = game.state(), windowMs = painted - profile.startedAt;
  const sampleReady = windowMs >= 2000;
  if (sampleReady) {
    const intervals = frameIntervals.slice().sort((a, b) => a - b);
    const percentile = p => intervals[Math.min(intervals.length - 1,
      Math.floor(intervals.length * p))] || 0;
    const net = state.net;
    const continuous = net && netBaseline && net.frame >= netBaseline.frame &&
      net.sent >= netBaseline.sent;
    recentPerf = {
      windowMs, fps: n * 1000 / windowMs,
      simMs: profile.simMs / n, paintMs: profile.paintMs / n, flushMs: profile.flushMs / n,
      frameP50Ms: percentile(.5), frameP95Ms: percentile(.95),
      frameP99Ms: percentile(.99), frameMaxMs: percentile(1),
      gameFps: continuous ? (net.frame - netBaseline.frame) * 1000 / windowMs : null,
      waits: continuous ? net.waits - netBaseline.waits : null,
      stalls: continuous ? net.stalls - netBaseline.stalls : null,
    };
    netBaseline = net ? { frame: net.frame, sent: net.sent, waits: net.waits, stalls: net.stalls } : null;
  }
  host.flush({ sourceHash, ...state, perf: recentPerf });
  profile.flushMs += Date.now() - painted;
  if (sampleReady) {
    profile = { simMs: 0, paintMs: 0, flushMs: 0, frames: 0, startedAt: painted };
    frameIntervals = [];
  }
}
function act(api) {
  host.update(api);
  host.act(api.event);
}
function leave() { game?.leave(); host?.closeNet(); host?.clearInput(); }
export { boot, sim, paint, act, leave };

// A gamepad/display surface has no mouse crosshair. Keyboard escape still works.
export const surface = "display";
