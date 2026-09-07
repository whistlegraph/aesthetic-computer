#!/usr/bin/env node
// bot-harness.mjs — claude into a live oskiewar versus match, bot a fighter, and
// read the authoritative fight back so an agent can close a read→decide→inject
// loop. 26.09.07
//
// WHY this exists. oskiewar is always-multiplayer versus (see mac-test.html and
// round-room.mjs): the first tab at a room URL HOSTS the authoritative sim, the
// second takes the CHALLENGER chair, the rest spectate. An agent that wants to
// "keep botting" needs three things this module gives it: a way in (CDP), a way
// to move a fighter (held key events), and a way to see the result (the round
// bridge's state frames). Everything here is one of those three.
//
// HOW a fighter is driven. The shell (mac-test.html) maps the keyboard onto a
// virtual gamepad and — this is the load-bearing detail — its keydown/keyup
// handlers read only `event.code`; they never check `isTrusted`. So a synthetic
// `KeyboardEvent` dispatched on `window` moves a fighter exactly like a real key.
// KeyD→right, KeyA→left, KeyW→up/jump, KeyS→down, Space→A(attack), Enter→B,
// ShiftLeft→X, AltLeft→Y. On the CHALLENGER tab those presses are sampled at
// 60Hz, deduped, and shipped up the wire to the host, which applies them to the
// challenger's fighter and echoes the whole fight back.
//
// THE TIMING GOTCHA (do not remove the holds). The input sampler runs once per
// 60Hz sim tick and the versus wire only re-sends on change (min ~33ms apart).
// A sub-frame tap — keydown immediately followed by keyup — lands entirely
// between two samples and is simply never seen. Every press here is therefore
// HELD: keydown now, keyup scheduled later on the PAGE's own clock (so the hold
// spans real frames no matter how slow the CDP round-trip is), with a 150ms
// floor. Movement uses explicit hold()/release() so a walk stays down across
// many frames instead of stuttering under overlapping timers.
//
// WHO can be botted. Only a tab whose fighter is authoritative:
//   - seat === "challenger"  → injecting drives fighters[1] via the wire.
//   - the host/publisher tab → injecting drives fighters[0] locally (its own
//     pad). The host's state is NOT in its bridge (it is the source, not a
//     viewer), so read the challenger tab for a full picture.
//   - a pure spectator (seat "" and not publishing) → injecting drives nothing
//     authoritative; connect() warns and behaviors refuse.
//
// TRANSPORT. Self-contained minimal CDP over Node's built-in WebSocket — no
// deps. Resolve a machine name to a cdpUrl from ~/.config/slab/puppet.json (the
// same registry `puppet` reads), or pass a raw --cdp URL. We deliberately do NOT
// route through the puppet daemon: a bot loop wants a private, long-lived socket
// it fully controls, and the direct client is one file with nothing to install.
//
// CLI:
//   node xbox/live/bot-harness.mjs --machine blueberry --tab guest --behavior aggress --seconds 15
//   node xbox/live/bot-harness.mjs --machine blueberry --tab guest --behavior pace --seconds 10
//   node xbox/live/bot-harness.mjs --machine blueberry --tab guest --behavior info
//   node xbox/live/bot-harness.mjs --cdp http://127.0.0.1:9225 --tab guest --behavior watch --seconds 8

import { readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

const CONFIG_PATH = process.env.SLAB_PUPPET_CONFIG ||
  join(homedir(), ".config", "slab", "puppet.json");

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Friendly move names → keyboard codes the shell maps to player-0's pad. An
// agent choreographs a fight in these words; the shell speaks `event.code`.
export const KEY = {
  left: "KeyA", right: "KeyD", up: "KeyW", down: "KeyS", jump: "KeyW",
  a: "Space", attack: "Space", b: "Enter", x: "ShiftLeft", y: "AltLeft",
};

// ── machine registry ────────────────────────────────────────────────────────

function cdpUrlForMachine(name) {
  let cfg;
  try { cfg = JSON.parse(readFileSync(CONFIG_PATH, "utf8")); }
  catch { throw new Error(`no puppet config at ${CONFIG_PATH} — pass --cdp instead`); }
  const spec = cfg?.machines?.[name];
  if (!spec?.cdpUrl)
    throw new Error(`unknown machine: ${name} — known: ${Object.keys(cfg?.machines || {}).join(", ")}`);
  return { cdpUrl: spec.cdpUrl, tunnelCmd: spec.tunnelCmd };
}

// ── the page-side agent ──────────────────────────────────────────────────────
// Injected once into the page's main world. It taps the round bridge's state
// stream, holds/releases keys on the page's own clock, and hands cheap snapshots
// back. Kept in one string with no backticks so it survives being passed through
// Runtime.evaluate. Re-evaluating it is a no-op except for the arm() at the end,
// so connect() and any reconnect can call it freely.
const AGENT_SOURCE = `(function () {
  var g = globalThis;
  if (!g.__botHarness) {
    var H = { ring: [], nextIdx: 0, held: {}, armedBridge: null };
    var RING_MAX = 300;

    H.bridge = function () { return g.__oskiewarRoundBridge || null; };

    // Only "state" frames carry fighters; distill them into a small, stable row
    // so the loop never has to know the whole spectator schema.
    H.record = function (msg) {
      if (!msg || msg.type !== "state" || !msg.content) return;
      var c = msg.content;
      H.ring.push({
        idx: H.nextIdx++, at: c.at || 0, seq: c.seq || 0,
        phase: c.phase || "", live: !!msg.live,
        fighters: (c.fighters || []).map(function (f) {
          return { name: f.name, x: f.x, y: f.y, z: f.z, vx: f.vx, vy: f.vy,
            facing: f.facing, alive: f.alive, attack: f.attack || "",
            grounded: f.grounded, blocking: f.blocking, ducking: f.ducking,
            score: f.score, roundWins: f.roundWins };
        }),
        round: c.round || null,
        camera: c.camera ? { x: c.camera.x, y: c.camera.y, width: c.camera.width } : null,
      });
      if (H.ring.length > RING_MAX) H.ring.splice(0, H.ring.length - RING_MAX);
    };

    // Tap the bridge's listener via an accessor. The game reassigns .listener on
    // every start()/stop() (round changes reuse the same RoundRoom instance), so
    // a plain wrapper would be shed the next round; a getter/setter that stores
    // the game's real listener and always returns our tap survives all of it.
    H.arm = function () {
      var b = H.bridge();
      if (!b) return false;
      if (H.armedBridge === b) return true;
      var real = b.listener;
      try { delete b.listener; } catch (e) {}
      Object.defineProperty(b, "listener", {
        configurable: true,
        get: function () { return wrapped; },
        set: function (fn) { real = fn; },
      });
      function wrapped(msg) { try { if (real) real(msg); } finally { H.record(msg); } };
      H.armedBridge = b;
      return true;
    };

    H.isOskiewar = function () {
      return ("__oskiewarRoundBridge" in g) || ("__oskiewarFrameStats" in g);
    };

    H.info = function () {
      var b = H.bridge();
      return {
        isOskiewar: H.isOskiewar(),
        hasBridge: !!b,
        seat: b ? (b.seat || "") : "",
        role: b ? (b.role || "") : "",
        room: b ? (b.name || "") : "",
        live: b ? !!b.live : false,
        versusRoom: g.__oskiewarVersusRoom || "",
        publisher: !!g.__oskiewarVersusRoom && (!b || b.seat !== "challenger"),
        capable: g.__oskiewarVersusCapable === true,
        armed: !!b && H.armedBridge === b,
        frameCount: H.nextIdx,
        held: Object.keys(H.held),
      };
    };

    // The freshest authoritative snapshot, straight off the bridge (always
    // current, even before the ring has an entry).
    H.state = function () {
      var b = H.bridge();
      var c = b && b.lastState;
      if (!c) return null;
      return {
        at: c.at || 0, seq: c.seq || 0, phase: c.phase || "", live: !!(b && b.live),
        fighters: (c.fighters || []).map(function (f) {
          return { name: f.name, x: f.x, y: f.y, z: f.z, vx: f.vx, vy: f.vy,
            facing: f.facing, alive: f.alive, attack: f.attack || "",
            grounded: f.grounded, blocking: f.blocking, score: f.score,
            roundWins: f.roundWins };
        }),
        round: c.round || null,
        camera: c.camera ? { x: c.camera.x, y: c.camera.y, width: c.camera.width } : null,
      };
    };

    // Drain the ring from a caller-held index so a poller never sees a frame
    // twice and never misses one between polls.
    H.frames = function (since) {
      since = Number(since) || 0;
      return H.ring.filter(function (fr) { return fr.idx >= since; });
    };

    var KEYNAME = { KeyW: "w", KeyS: "s", KeyA: "a", KeyD: "d", Space: " ",
      Enter: "Enter", ShiftLeft: "Shift", AltLeft: "Alt", ArrowUp: "ArrowUp",
      ArrowDown: "ArrowDown", ArrowLeft: "ArrowLeft", ArrowRight: "ArrowRight" };
    H.dispatch = function (type, code) {
      window.dispatchEvent(new KeyboardEvent(type, {
        code: code, key: KEYNAME[code] || code, bubbles: true, cancelable: true }));
    };
    H.hold = function (code) {
      if (!H.held[code]) { H.held[code] = 1; H.dispatch("keydown", code); }
      return Object.keys(H.held);
    };
    H.release = function (code) {
      if (H.held[code]) { delete H.held[code]; H.dispatch("keyup", code); }
      return Object.keys(H.held);
    };
    H.releaseAll = function () {
      Object.keys(H.held).forEach(function (c) { H.release(c); });
      return true;
    };
    // A discrete press, held long enough for the 60Hz sampler to see it. The
    // keyup rides the PAGE clock so latency on our side can't shorten the hold
    // below the floor.
    H.press = function (code, holdMs) {
      holdMs = Math.max(150, Number(holdMs) || 180);
      H.hold(code);
      setTimeout(function () { H.release(code); }, holdMs);
      return holdMs;
    };

    // A cheap downscaled JPEG of the game canvas — a frame for the loop or a
    // before/after proof, far lighter than a full CDP screenshot.
    H.snapshot = function (maxWidth) {
      var src = document.getElementById("screen");
      if (!src) return null;
      maxWidth = Number(maxWidth) || 320;
      var scale = Math.min(1, maxWidth / src.width);
      var w = Math.max(1, Math.round(src.width * scale));
      var h = Math.max(1, Math.round(src.height * scale));
      var c = document.createElement("canvas");
      c.width = w; c.height = h;
      c.getContext("2d").drawImage(src, 0, 0, w, h);
      return c.toDataURL("image/jpeg", 0.6);
    };

    g.__botHarness = H;
  }
  g.__botHarness.arm();
  return g.__botHarness.info();
})()`;

// ── minimal CDP client ───────────────────────────────────────────────────────

class CDP {
  constructor(wsUrl) {
    this.wsUrl = wsUrl;
    this.ws = null;
    this.id = 0;
    this.pending = new Map();
  }

  // List page targets and pick one. `filter` is ergonomic: "guest" / "host"
  // pick the two versus chairs by URL shape; anything else is a substring match
  // on url or id; empty falls to the first oskiewar page.
  static async pickTarget(cdpUrl, filter) {
    let list;
    try { list = await (await fetch(cdpUrl + "/json/list")).json(); }
    catch (e) {
      throw new Error(`cannot reach CDP at ${cdpUrl} — is the tunnel up? (${e.message})`);
    }
    const pages = list.filter((t) => t.type === "page" && t.webSocketDebuggerUrl);
    const osk = (t) => /oskiewar/i.test(t.url);
    let pick;
    if (!filter) pick = pages.find(osk);
    else if (filter === "guest") pick = pages.find((t) => osk(t) && /#guest/.test(t.url));
    else if (filter === "host") pick = pages.find((t) => osk(t) && !/#guest/.test(t.url));
    else {
      const f = filter.toLowerCase();
      pick = pages.find((t) => (t.url + " " + t.id).toLowerCase().includes(f));
    }
    if (!pick) {
      const seen = pages.map((t) => t.url).join("\n  ");
      throw new Error(`no tab matched "${filter}". pages:\n  ${seen}`);
    }
    return pick;
  }

  open() {
    return new Promise((resolve, reject) => {
      const ws = new WebSocket(this.wsUrl);
      this.ws = ws;
      ws.addEventListener("open", () => resolve());
      ws.addEventListener("error", (e) =>
        reject(new Error("CDP socket error: " + (e.message || "open failed"))));
      ws.addEventListener("message", (e) => {
        let msg; try { msg = JSON.parse(e.data); } catch { return; }
        if (msg.id && this.pending.has(msg.id)) {
          const { resolve: res, reject: rej } = this.pending.get(msg.id);
          this.pending.delete(msg.id);
          if (msg.error) rej(new Error(msg.error.message || "CDP error"));
          else res(msg.result);
        }
      });
    });
  }

  send(method, params) {
    const id = ++this.id;
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }

  // Evaluate an expression in the page and return its value. Awaits promises,
  // returns by value, and surfaces page exceptions as thrown errors — a
  // swallowed page error here would strand the loop reacting to stale state.
  async evaluate(expr) {
    const r = await this.send("Runtime.evaluate", {
      expression: expr, returnByValue: true, awaitPromise: true,
    });
    if (r.exceptionDetails) {
      const d = r.exceptionDetails;
      throw new Error("page eval threw: " + (d.exception?.description || d.text));
    }
    return r.result?.value;
  }

  close() { try { this.ws?.close(); } catch {} }
}

// ── the harness ──────────────────────────────────────────────────────────────

export class BotHarness {
  constructor({ machine, cdp, tab } = {}) {
    if (!machine && !cdp) throw new Error("pass { machine } or { cdp }");
    this.machine = machine;
    this.cdpUrl = cdp || null;
    this.tab = tab || "";
    this.cdp = null;
    this.target = null;
    this.info = null;
  }

  async connect() {
    if (!this.cdpUrl) this.cdpUrl = cdpUrlForMachine(this.machine).cdpUrl;
    this.target = await CDP.pickTarget(this.cdpUrl, this.tab);
    this.cdp = new CDP(this.target.webSocketDebuggerUrl);
    await this.cdp.open();
    this.info = await this.cdp.evaluate(AGENT_SOURCE); // installs + arms the agent
    if (!this.info?.isOskiewar)
      throw new Error(`tab is not oskiewar: ${this.target.url}`);
    // The seat decides whose fighter injection actually moves. Report it plainly
    // so a caller (or a human reading the log) knows whether the loop can bite.
    this.myIndex = this.info.seat === "challenger" ? 1 : 0;
    return this.info;
  }

  // True when injecting keys drives an authoritative fighter (challenger chair
  // or the publishing host). A pure spectator can watch but not fight.
  get canBot() {
    return this.info?.seat === "challenger" || this.info?.publisher === true;
  }

  refreshInfo() { return this.cdp.evaluate("__botHarness.arm(), __botHarness.info()"); }
  state() { return this.cdp.evaluate("(__botHarness.arm(), __botHarness.state())"); }
  frames(since = 0) { return this.cdp.evaluate(`(__botHarness.arm(), __botHarness.frames(${since}))`); }
  snapshot(maxWidth = 320) { return this.cdp.evaluate(`__botHarness.snapshot(${maxWidth})`); }

  hold(code) { return this.cdp.evaluate(`__botHarness.hold(${JSON.stringify(code)})`); }
  release(code) { return this.cdp.evaluate(`__botHarness.release(${JSON.stringify(code)})`); }
  releaseAll() { return this.cdp.evaluate("__botHarness.releaseAll()"); }
  press(code, holdMs = 180) {
    return this.cdp.evaluate(`__botHarness.press(${JSON.stringify(code)}, ${holdMs})`);
  }
  // Accept a move name or a raw code, so callers can say press("attack").
  move(name, holdMs) { return this.press(KEY[name] || name, holdMs); }

  // Stream authoritative frames to a callback. Polls the drained ring so no
  // frame repeats and none is lost between polls. Returns a stop function.
  onState(cb, { intervalMs = 100 } = {}) {
    let since = this.info?.frameCount || 0;
    let stopped = false;
    let timer = null;
    const tick = async () => {
      if (stopped) return;
      try {
        const batch = await this.frames(since);
        for (const fr of batch) { since = fr.idx + 1; cb(fr); }
      } catch { /* a reload/close ends the stream; the caller's timeout wins */ }
      if (!stopped) timer = setTimeout(tick, intervalMs);
    };
    tick();
    return () => { stopped = true; clearTimeout(timer); };
  }

  // ── example behaviors (each closes some of the loop) ────────────────────────

  // Open-loop: walk right then left on a timer. The simplest proof that
  // injection moves a fighter — hold a direction, watch x travel, flip.
  async pace({ seconds = 10, stepMs = 700, log = () => {} } = {}) {
    const end = Date.now() + seconds * 1000;
    let code = KEY.right;
    while (Date.now() < end) {
      await this.hold(code);
      await sleep(stepMs);
      await this.release(code);
      const s = await this.state();
      const me = s?.fighters?.[this.myIndex];
      log(`pace ${code === KEY.right ? "→" : "←"} me.x=${Math.round(me?.x ?? NaN)} phase=${s?.phase}`);
      code = code === KEY.right ? KEY.left : KEY.right;
    }
    await this.releaseAll();
  }

  // Closed-loop: read the fight, steer toward the rival, swing when close. This
  // is the read→decide→inject cycle an agent extends. Movement is a held
  // direction (only changed when the decision changes) so the walk is smooth;
  // the attack is a discrete press behind a cooldown.
  async aggress({ seconds = 15, tickMs = 120, range = 320, cooldownMs = 380,
    log = () => {} } = {}) {
    const rivalIndex = this.myIndex === 1 ? 0 : 1;
    const end = Date.now() + seconds * 1000;
    let heldDir = null;
    let lastAttack = 0;
    while (Date.now() < end) {
      const s = await this.state();
      const me = s?.fighters?.[this.myIndex];
      const rival = s?.fighters?.[rivalIndex];
      if (me && rival) {
        const dx = rival.x - me.x;               // world x grows to the right
        const near = Math.abs(dx) < range;
        const wantDir = near ? null : (dx > 0 ? KEY.right : KEY.left);
        if (wantDir !== heldDir) {
          if (heldDir) await this.release(heldDir);
          if (wantDir) await this.hold(wantDir);
          heldDir = wantDir;
        }
        let act = wantDir ? (wantDir === KEY.right ? "walk →" : "walk ←") : "in range";
        if (near && Date.now() - lastAttack > cooldownMs) {
          await this.press(KEY.attack, 170);
          lastAttack = Date.now();
          act = "ATTACK";
        }
        log(`me.x=${Math.round(me.x)} rival.x=${Math.round(rival.x)} dx=${Math.round(dx)} ` +
          `alive=${me.alive}/${rival.alive} → ${act}`);
      } else {
        log(`waiting for both fighters (phase=${s?.phase ?? "?"})`);
      }
      await sleep(tickMs);
    }
    await this.releaseAll();
  }

  async close() {
    try { await this.releaseAll(); } catch {}
    this.cdp?.close();
  }
}

// ── CLI ──────────────────────────────────────────────────────────────────────

function parseArgs(argv) {
  const a = { behavior: "info", seconds: 15, tab: "" };
  for (let i = 0; i < argv.length; i++) {
    const k = argv[i];
    if (!k.startsWith("--")) continue;
    const name = k.slice(2);
    const next = argv[i + 1];
    if (next === undefined || next.startsWith("--")) { a[name] = true; }
    else { a[name] = next; i++; }
  }
  return a;
}

async function main() {
  const a = parseArgs(process.argv.slice(2));
  if (a.help) {
    console.log("usage: bot-harness.mjs --machine <name>|--cdp <url> --tab <guest|host|substr> " +
      "--behavior <info|watch|pace|aggress> [--seconds N] [--range 320] [--me 0|1]");
    return;
  }
  const bot = new BotHarness({ machine: a.machine, cdp: a.cdp, tab: a.tab });
  const info = await bot.connect();
  console.error(`connected: ${bot.target.url}`);
  console.error(`  seat=${info.seat || "(none)"} role=${info.role} room=${info.room} ` +
    `live=${info.live} publisher=${info.publisher} canBot=${bot.canBot}`);
  if (a.me !== undefined) bot.myIndex = Number(a.me);
  const seconds = Number(a.seconds) || 15;
  const log = (line) => console.log(`[${new Date().toISOString().slice(11, 19)}] ${line}`);

  try {
    if (a.behavior === "info") {
      console.log(JSON.stringify({ info, state: await bot.state() }, null, 2));
    } else if (a.behavior === "watch") {
      const stop = bot.onState((fr) => {
        const f = fr.fighters.map((x) => `${x.name}@${Math.round(x.x)}${x.attack ? ":" + x.attack : ""}`).join("  ");
        log(`#${fr.idx} ${fr.phase} ${f}`);
      });
      await sleep(seconds * 1000);
      stop();
    } else if (a.behavior === "pace") {
      if (!bot.canBot) console.error("WARNING: this tab is a spectator — pacing will move nothing authoritative.");
      await bot.pace({ seconds, stepMs: Number(a.step) || 700, log });
    } else if (a.behavior === "aggress") {
      if (!bot.canBot) console.error("WARNING: this tab is a spectator — aggress will move nothing authoritative.");
      await bot.aggress({ seconds, range: Number(a.range) || 320,
        tickMs: Number(a.tick) || 120, log });
    } else {
      console.error(`unknown behavior: ${a.behavior}`);
    }
  } finally {
    await bot.close();
    console.error("done (keys released, socket closed).");
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((e) => { console.error("ERROR:", e.message); process.exit(1); });
}
