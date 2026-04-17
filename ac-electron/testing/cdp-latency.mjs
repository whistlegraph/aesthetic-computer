#!/usr/bin/env node
// CDP latency probe for the running Electron app.
//
// Connects to the Electron CDP endpoint (localhost:9222, enabled by the
// --remote-debugging-port=9222 flag the app already sets in package.json
// scripts), finds the notepat webview, dispatches keydown events, reads
// `window.__notepat_perfStats.latency`, and reports stats.
//
// Prerequisite: the app must be running. Install/launch via:
//   open "/Applications/Aesthetic Computer.app"
// Or for the dev source:
//   cd ac-electron && npm start
//
// Usage:
//   node testing/cdp-latency.mjs [--iterations=20] [--cooldown=200]
//                                 [--key=a] [--port=9222]
//                                 [--open-notepat]  open the notepat window first
//
// Notes:
//   - Uses native fetch + WebSocket (Node 22+). No external deps.
//   - Measures JS-side latency: keydown → notepat's `sound.synth` call.
//   - Audio buffer/worklet latency is NOT included (typically +10-30ms).

const DEFAULTS = {
  iterations: 20,
  cooldownMs: 200,
  key: "a",
  port: 9222,
  openNotepat: false,
};

function parseArgs(argv) {
  const out = { ...DEFAULTS };
  for (const raw of argv.slice(2)) {
    const [k, v] = raw.replace(/^--/, "").split("=");
    if (k === "iterations") out.iterations = Number(v);
    else if (k === "cooldown") out.cooldownMs = Number(v);
    else if (k === "key") out.key = v;
    else if (k === "port") out.port = Number(v);
    else if (k === "open-notepat") out.openNotepat = true;
    else throw new Error(`unknown arg: ${raw}`);
  }
  return out;
}

async function listTargets(port) {
  const r = await fetch(`http://127.0.0.1:${port}/json`);
  if (!r.ok) throw new Error(`CDP /json returned ${r.status}. Is the app running with --remote-debugging-port=${port}?`);
  return r.json();
}

// Minimal CDP client over a single target's WebSocket. Keeps the wire
// protocol in one place (id → promise map + event listeners) so each
// target we talk to is just a session object.
class CdpSession {
  constructor(ws) {
    this.ws = ws;
    this.nextId = 1;
    this.pending = new Map();
    this.listeners = new Map();
    ws.addEventListener("message", (m) => this.onMessage(m));
  }

  static async connect(url) {
    const ws = new WebSocket(url);
    await new Promise((res, rej) => {
      ws.addEventListener("open", res, { once: true });
      ws.addEventListener("error", rej, { once: true });
    });
    return new CdpSession(ws);
  }

  onMessage(m) {
    const msg = JSON.parse(m.data);
    if (msg.id && this.pending.has(msg.id)) {
      const { resolve, reject } = this.pending.get(msg.id);
      this.pending.delete(msg.id);
      if (msg.error) reject(new Error(`${msg.error.code}: ${msg.error.message}`));
      else resolve(msg.result);
    } else if (msg.method) {
      const fns = this.listeners.get(msg.method);
      if (fns) for (const fn of fns) fn(msg.params);
    }
  }

  send(method, params = {}) {
    const id = this.nextId++;
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }

  on(event, fn) {
    if (!this.listeners.has(event)) this.listeners.set(event, new Set());
    this.listeners.get(event).add(fn);
  }

  close() { this.ws.close(); }
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function percentile(sorted, p) {
  if (sorted.length === 0) return 0;
  const idx = Math.min(sorted.length - 1, Math.floor((p / 100) * sorted.length));
  return sorted[idx];
}

function stats(samples) {
  const s = [...samples].sort((a, b) => a - b);
  const n = s.length;
  const mean = s.reduce((a, b) => a + b, 0) / n;
  const variance = s.reduce((acc, v) => acc + (v - mean) ** 2, 0) / n;
  return {
    n,
    mean,
    median: n ? s[Math.floor(n / 2)] : 0,
    min: s[0] ?? 0,
    max: s[n - 1] ?? 0,
    p95: percentile(s, 95),
    p99: percentile(s, 99),
    stdDev: Math.sqrt(variance),
  };
}

// Key name → VK code + DOM-style key. `Input.dispatchKeyEvent` wants a
// `windowsVirtualKeyCode` for modifier-free letter keys. For lowercase
// a–z, VK = uppercase ASCII code.
function keyToInput(k) {
  const ch = k.toLowerCase();
  const vk = ch.toUpperCase().charCodeAt(0);
  return { key: ch, code: `Key${ch.toUpperCase()}`, windowsVirtualKeyCode: vk };
}

async function findNotepatTarget(port) {
  const targets = await listTargets(port);
  // Prefer a target whose URL is the notepat piece itself; fall back to
  // any aesthetic.computer page (whatever the AC Pane/Notepat window
  // happens to be showing).
  // AC content runs inside a <webview> element, so the CDP target type
  // is "webview" (not "page"). The outer BrowserWindow is "page" and
  // loads a local file:// harness — skip those.
  const isAcContent = (t) => /aesthetic\.computer/.test(t.url || "") && (t.type === "webview" || t.type === "page");
  const byPiece = targets.find((t) => isAcContent(t) && /\/notepat(\W|$)/.test(t.url || ""));
  if (byPiece) return byPiece;
  return targets.find(isAcContent);
}

async function openNotepatViaMain(port) {
  // Find the main-process BrowserWindow target (notepat-view.html or
  // flip-view.html) so we can invoke openNotepatWindow() indirectly via
  // a helper exposed at runtime. For now we just log a hint; opening
  // the notepat window is one click on the piano tray.
  console.log("⚠️  --open-notepat: click the piano tray icon to open Notepat.");
  console.log("   Waiting up to 20s for a target matching /notepat to appear...");
  const deadline = Date.now() + 20_000;
  while (Date.now() < deadline) {
    const t = await findNotepatTarget(port);
    if (t && /\/notepat/.test(t.url || "")) return t;
    await sleep(500);
  }
  return null;
}

async function main() {
  const opts = parseArgs(process.argv);
  console.log(`CDP latency probe — iterations=${opts.iterations} cooldown=${opts.cooldownMs}ms key=${opts.key}`);

  let target;
  if (opts.openNotepat) target = await openNotepatViaMain(opts.port) || await findNotepatTarget(opts.port);
  else target = await findNotepatTarget(opts.port);

  if (!target) {
    console.error("❌ No aesthetic.computer page target found on CDP :" + opts.port);
    console.error("   Make sure the app is running and a Notepat/AC window is open.");
    process.exit(1);
  }
  console.log(`→ Attaching to: ${target.url}`);

  const cdp = await CdpSession.connect(target.webSocketDebuggerUrl);
  await cdp.send("Runtime.enable");
  await cdp.send("Input.enable").catch(() => {}); // Input domain doesn't exist in all CDP versions
  await cdp.send("Page.enable").catch(() => {});

  // If we grabbed some other AC page (e.g. /prompt), navigate it to the
  // notepat piece via the in-app router so boot() runs and perfStats
  // becomes available. `jump` is the AC convention for client-side piece
  // navigation and works from any piece.
  if (!/\/notepat/.test(target.url || "")) {
    console.log("→ Navigating this webview to /notepat via jump('notepat')...");
    await cdp.send("Runtime.evaluate", {
      expression: `(window.jump || (s) => (location.href = '/' + s))('notepat')`,
    });
  }

  // Wait up to 8s for `window.__notepat_perfStats` to appear.
  const bootDeadline = Date.now() + 8000;
  let perfReady = false;
  while (Date.now() < bootDeadline) {
    const r = await cdp.send("Runtime.evaluate", {
      expression: `(typeof window.__notepat_perfStats === 'object')`,
      returnByValue: true,
    });
    if (r.result.value) { perfReady = true; break; }
    await sleep(150);
  }
  if (!perfReady) {
    console.error("❌ `window.__notepat_perfStats` not found after boot wait.");
    process.exit(1);
  }
  // Notepat needs a user gesture to unlock the AudioContext. Without one,
  // sound.synth calls queue silently and latency reads won't be meaningful.
  // Dispatch a synthetic mouseDown/mouseUp on the center of the viewport
  // to satisfy the gesture requirement before the test loop starts.
  await cdp.send("Input.dispatchMouseEvent", { type: "mousePressed", x: 100, y: 100, button: "left", clickCount: 1 }).catch(() => {});
  await sleep(30);
  await cdp.send("Input.dispatchMouseEvent", { type: "mouseReleased", x: 100, y: 100, button: "left", clickCount: 1 }).catch(() => {});
  await sleep(300);

  const input = keyToInput(opts.key);
  const samples = [];

  for (let i = 0; i < opts.iterations; i++) {
    // Reset the stat so a stale value doesn't bleed through.
    await cdp.send("Runtime.evaluate", {
      expression: `window.__notepat_perfStats && (window.__notepat_perfStats.latency = null, window.__notepat_perfStats.lastKeyTime = null)`,
    });

    await cdp.send("Input.dispatchKeyEvent", {
      type: "keyDown",
      key: input.key,
      code: input.code,
      windowsVirtualKeyCode: input.windowsVirtualKeyCode,
    });

    // Wait up to 80ms for the sound trigger to update perfStats.
    let latency = null;
    for (let tries = 0; tries < 16; tries++) {
      await sleep(5);
      const r = await cdp.send("Runtime.evaluate", {
        expression: `window.__notepat_perfStats?.latency ?? null`,
        returnByValue: true,
      });
      if (typeof r.result.value === "number") { latency = r.result.value; break; }
    }

    await cdp.send("Input.dispatchKeyEvent", {
      type: "keyUp",
      key: input.key,
      code: input.code,
      windowsVirtualKeyCode: input.windowsVirtualKeyCode,
    });

    if (typeof latency === "number") {
      samples.push(latency);
      process.stdout.write(`${latency.toFixed(1)}ms `);
    } else {
      process.stdout.write("MISS ");
    }
    await sleep(opts.cooldownMs);
  }
  console.log();

  const s = stats(samples);
  console.log(`\nresults (n=${s.n}/${opts.iterations}):`);
  console.log(`  mean:   ${s.mean.toFixed(1)}ms`);
  console.log(`  median: ${s.median.toFixed(1)}ms`);
  console.log(`  min:    ${s.min.toFixed(1)}ms`);
  console.log(`  max:    ${s.max.toFixed(1)}ms`);
  console.log(`  stdDev: ${s.stdDev.toFixed(1)}ms`);
  console.log(`  p95:    ${s.p95.toFixed(1)}ms`);
  console.log(`  p99:    ${s.p99.toFixed(1)}ms`);

  cdp.close();
}

main().catch((e) => {
  console.error("fatal:", e.message);
  process.exit(1);
});
