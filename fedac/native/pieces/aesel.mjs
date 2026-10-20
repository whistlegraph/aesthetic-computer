// aesel.mjs — Aesel, on AC Native.
//
// The desktop Aesel is an Electron shell around a conversation with an agent
// and the piece it is writing. This machine has no Electron and no browser;
// it has a framebuffer, a keyboard, QuickJS, and the Claude Code binary. So
// this is the same session drawn as a piece, in the order of the terminal's
// pro mode (aesel/src/render.mjs, layouts/pro.json) — conversation, the line
// being typed, one status line — set classic: black, white borders, big
// type, room around everything, and the session's egg in the corner the way
// Slab gives it one on the Mac. Colour is a keypress's, briefly.
//
// The bridge is Claude's headless stream-json protocol over a raw PTY
// (system.pty2 with { raw: true }); lib/aesel-bridge.mjs translates it. The
// child stays alive when the user leaves for the prompt or runs the piece —
// `aesel` from the prompt reattaches to the same conversation.

import {
  Bridge, DEFAULT_MODEL, blankPiece, compact, creatureLook, creatureSeed, dollars, instructionsFor,
  keyColor, markdown, petName, pieceFactory, pipedCommand, randomSlug, toolLabel, turnUsageText, uuid, wrapRows,
} from "../lib/aesel-bridge.mjs";
import { AC_MODELS, AcBridge, DEFAULT_AC_MODEL, acModelLabel, resolveAcModel, streamFile } from "../lib/aesel-ac.mjs";

// The terminal Aesel's palette (aesel/src/render.mjs, after the AC prompt's
// dark scheme): pink prompt, orange highlight, light-purple soft and muted.
// page, text and stroke follow the session's state each paint (PAGES), so
// the whole screen turns the way Slab turns an Aesel window on the Mac.
const C = {
  page: [32, 4, 32],
  text: [201, 233, 255],
  stroke: [167, 195, 232],
  prompt: [200, 30, 100],
  highlight: [255, 100, 0],
  soft: [220, 180, 255],
  muted: [170, 150, 205],
  dim: [120, 100, 160],
  status: [0, 255, 0],
  error: [255, 90, 90],
  run: [255, 160, 60],
  code: [235, 225, 255],
  ground: [20, 12, 30],
};

// Slab's page for each session state (its published dark theme.json): what
// an Aesel window on the Mac turns while it works, asks, finishes or fails.
const PAGES = {
  blank: { bg: [32, 4, 32], text: [201, 233, 255], shadow: [167, 195, 232] },
  working: { bg: [2, 47, 24], text: [134, 249, 219], shadow: [47, 241, 143] },
  awaiting: { bg: [64, 41, 14], text: [219, 226, 179], shadow: [219, 179, 54] },
  complete: { bg: [66, 7, 12], text: [255, 175, 167], shadow: [255, 43, 39] },
  interrupted: { bg: [28, 11, 70], text: [181, 179, 255], shadow: [134, 86, 255] },
  stale: { bg: [57, 5, 22], text: [219, 163, 196], shadow: [219, 78, 99] },
};
const FADE = 450; // ms for a key's colour to go back to the page's

// AC's character palette for a handle with no saved colours (handle-colors.mjs).
const NOM = [[255, 105, 97], [255, 170, 80], [255, 225, 90], [150, 225, 90], [90, 220, 140], [90, 215, 210], [95, 180, 255], [150, 150, 255], [200, 140, 255], [255, 140, 210]];

const FONT = "6x10";
const CLAUDE = "/bin/claude";
const PIECES = "/pieces";
// The hosted engine: aesthetic.computer buys the inference and meters it
// against the device's @handle. Its stream lands in this file, re-read once a
// frame (see acTransport), because the runtime's fetch is curl, not fetch().
const SITE = "https://aesthetic.computer";
const STREAM_FILE = "/tmp/aesel-inference.sse";
const SAY_FILE = "/tmp/aesel-say";
const STATE_FILE = "/tmp/aesel-state.json";
const MAX_ENTRIES = 400;
const MAX_VERSIONS = 40;
const SEP = " · ";

const SHIFT_MAP = {
  "1": "!", "2": "@", "3": "#", "4": "$", "5": "%", "6": "^", "7": "&", "8": "*", "9": "(", "0": ")",
  "-": "_", "=": "+", "[": "{", "]": "}", ";": ":", "'": '"', ",": "<", ".": ">", "/": "?", "\\": "|", "`": "~",
};

// The one-row guy from aesel/src/mascot.mjs.
const GUY_DANCE = ["//(o.o)>", "//(o.o)>", "\\\\(o.o)>", "//(^.^)>", "//(o.o)>", "//(-.-)>"];

const HELP = "/ask [on|off] · /provider ac|claude · /model [name] · /new · /clear · /latest · /run · /preview · /piece [name] · /versions · /rollback vN · /retry · /whoami · /energy · /quit   ctrl-c interrupts a running turn · pgup/pgdn scroll · ↑↓ history";

// Session state lives on globalThis so it survives a jump to the prompt or
// to the piece being written; the module itself is re-evaluated on return.
let S = null;
let shiftHeld = false;
let ctrlHeld = false;
// The system object of the current paint, for the hosted transport's polling.
let SYS = null;
let SCREEN = null;
let frame = 0;
// What the kept bridge reads, shared across re-evaluations. A session outlives
// this module (tab runs the piece, escape goes to the prompt), and closures
// made by the old copy would otherwise watch a frame count that stopped and
// a system object from its last paint — a hosted turn then never finishes.
const LIVE = globalThis.__aeselLive || (globalThis.__aeselLive = { frame: 0, system: null });
let pageRows = 20; // transcript rows on the last paint, for PgUp/PgDn
let P = null; // the corner preview: {source, piece, buf, w, h, count, error}

// `aesel` → the hosted engine (it signs in with the device's AC handle, so it
// works without a Claude token); `aesel:ac:qwen` → hosted on that model;
// `aesel:claude` or `aesel:claude:<model>` → Claude Code on the device.
function parseParams(params) {
  const [first, second] = params || [];
  if (first === "claude") return { backend: "claude", model: second || DEFAULT_MODEL };
  if (!first || first === "ac" || first === "aesthetic" || first === "hosted") {
    return { backend: "ac", model: resolveAcModel(second || "") };
  }
  return { backend: "ac", model: resolveAcModel(first) };
}

// Everything a session holds besides its identity. A session kept from an
// older build of this piece is topped up from here on reattach.
function blankSession() {
  return {
    bridge: null,
    entries: [],     // {kind, text}
    input: "",
    cursor: 0,
    history: [],
    historyIndex: 0,
    queued: [],      // lines typed while a turn ran; sent together after it
    scroll: 0,       // rows scrolled back from the bottom
    lastTotal: 0,
    status: "starting",
    approvals: [],   // waiting for y / a / n, oldest first
    approvalIndex: 0,
    lastTurn: "",    // how the last turn ended, for the page colour
    autoAllow: false,
    turn: null,      // {startedAt, lastEventAt, tools, feed, tokens, usd}
    spend: { tokens: 0, usd: 0, turns: 0 },
    versions: [],
    pieceDirty: false,
    preview: true,   // the piece, live, in the transcript's top-left corner
    logTail: [],
    flash: null,
    stoppedByUser: false,
    exitCode: null,
  };
}

function freshState(system, params) {
  const slug = randomSlug();
  const { backend, model } = parseParams(params);
  return {
    ...blankSession(),
    ...readAccount(system),
    token: readToken(system),
    // The egg is the session's, as Slab's is the terminal's: /new keeps it.
    egg: eggFor(uuid()),
    backend,
    slug,
    file: `${PIECES}/${slug}.mjs`,
    model,
  };
}

// The handle and its saved colours: the device's config, else the one the
// flash baked into the image.
function readAccount(system) {
  for (const file of ["/mnt/config.json", "/default-config.json"]) {
    try {
      const raw = system.readFile(file);
      const config = raw ? JSON.parse(raw) : null;
      const handle = String(config?.handle || "").replace(/^@/, "");
      if (handle && handle !== "anonymous") return { handle, colors: handleColors(`@${handle}`, config.colors) };
    } catch (_) {}
  }
  return { handle: "", colors: [] };
}

function handleColors(account, saved) {
  return Array.from(account).map((ch, i) => {
    const c = Array.isArray(saved) ? saved[i] : null;
    if (c && [c.r, c.g, c.b].every((n) => Number.isFinite(n) && n >= 0 && n <= 255)) return [c.r, c.g, c.b].map(Math.round);
    const l = ch.toLowerCase();
    return l >= "a" && l <= "z" ? NOM[(l.charCodeAt(0) - 97) % 10] : l >= "0" && l <= "9" ? NOM[Number(l)] : C.soft;
  });
}

function eggFor(id) {
  const seed = creatureSeed(id);
  return { name: petName(seed), look: creatureLook(seed) };
}

// The AC access token the `link` pairing wrote into config.json. It pays for
// the hosted engine; a device flashed without linking has none.
function readToken(system) {
  try {
    const raw = system.readFile("/mnt/config.json");
    if (raw) return JSON.parse(raw).token || "";
  } catch (_) {}
  return "";
}

// The hosted engine's transport over the runtime's curl-backed fetch. start()
// hands curl a file to stream into; poll() reads what has arrived since
// (streamFile). The request is not "done" on the frame it started, because
// the system object of that paint was built before curl existed.
function acTransport() {
  let startedAt = -1;
  const stream = streamFile(() => LIVE.system.readFileBytes(STREAM_FILE));
  return {
    start(body, headers) {
      startedAt = LIVE.frame;
      stream.reset();
      const ok = LIVE.system.fetchPost(`${SITE}/api/easel-inference`, body, JSON.stringify(headers), { out: STREAM_FILE, timeout: 600 });
      return ok === true;
    },
    poll() {
      const sys = LIVE.system;
      const done = LIVE.frame > startedAt && !sys.fetchPending;
      const text = stream.read(done); // after `done`, so the last bytes are in
      return { text, done, error: done ? (sys.fetchError || "") : "" };
    },
    cancel() {
      LIVE.system.fetchCancel();
    },
  };
}

function note(text, kind = "note") {
  S.entries.push({ kind, text: String(text), id: nextId() });
  if (S.entries.length > MAX_ENTRIES) S.entries.splice(0, S.entries.length - MAX_ENTRIES);
}

// Entries are numbered for the state file, so a reader elsewhere can ask
// for what is new since the last id it saw.
function nextId() {
  S.seq = (S.seq || 0) + 1;
  return S.seq;
}

// What the screen says, as JSON, for another machine: written when it
// changes, at most four times a second. `echo … > /tmp/aesel-say` is the
// other half.
let stateSig = "";
// Paint timing, a second at a time, for the state file: how often paint
// runs, what it costs, and how much of that is the preview.
const PERF = { frames: 0, paint: 0, paintMax: 0, preview: 0, previewMax: 0, since: Date.now(), last: null };
function perfFrame(paintMs, previewMs) {
  PERF.frames++;
  PERF.paint += paintMs;
  PERF.paintMax = Math.max(PERF.paintMax, paintMs);
  PERF.preview += previewMs;
  PERF.previewMax = Math.max(PERF.previewMax, previewMs);
  const span = Date.now() - PERF.since;
  if (span < 1000) return;
  const n = PERF.frames;
  PERF.last = { fps: Math.round((n * 10000) / span) / 10, paintAvg: +(PERF.paint / n).toFixed(1), paintMax: PERF.paintMax,
    previewAvg: +(PERF.preview / n).toFixed(1), previewMax: PERF.previewMax, entries: S.entries.length };
  Object.assign(PERF, { frames: 0, paint: 0, paintMax: 0, preview: 0, previewMax: 0, since: Date.now() });
  stateSig = ""; // a new second is news
}
function writeState(system) {
  const last = S.entries[S.entries.length - 1];
  const t = S.turn;
  const sig = [S.status, S.entries.length, last && last.text.length, S.slug, S.model, S.approvals.length, t && t.tools.size, t && t.feed.length, S.lastTurn].join("|");
  if (sig === stateSig) return;
  stateSig = sig;
  const state = {
    at: Date.now(),
    status: S.status,
    working: busy(),
    lastTurn: S.lastTurn,
    piece: S.file,
    model: S.model,
    backend: S.backend,
    handle: S.handle || "",
    approvals: S.approvals.map((a) => a.label || ""),
    spend: S.spend,
    perf: PERF.last,
    preview: S.preview ? { focus: !!(P && P.focus), error: (P && P.error) || "" } : null,
    feed: t ? t.feed.slice(-3).map((r) => `${r.status} ${r.text}`) : [],
    entries: S.entries.slice(-30).map((e) => ({ id: e.id || 0, kind: e.kind, text: e.text.length > 4000 ? e.text.slice(0, 4000) + "…" : e.text })),
  };
  system.writeFile(STATE_FILE, JSON.stringify(state), false);
}

function flash(text, ms = 2500) {
  S.flash = { text, until: Date.now() + ms };
}

function ensurePiece(system) {
  if (!system.readFile(S.file)) system.writeFile(S.file, blankPiece(S.slug));
  S.versions = [{ source: system.readFile(S.file) || "", note: "opened" }];
}

// A save the agent made, or a restore, kept so /rollback can bring it back.
function keepVersion(source, why = "") {
  const last = S.versions[S.versions.length - 1];
  if (!source || (last && last.source === source)) return;
  S.versions.push({ source, note: why, at: Date.now() });
  if (S.versions.length > MAX_VERSIONS) S.versions.splice(1, 1); // v0 stays
}

function spawnBridge(system, screen, resume = "") {
  SYS = system;
  LIVE.system = system;
  if (S.bridge instanceof AcBridge && S.bridge.busy) S.bridge.interrupt();
  S.approvals = [];
  S.turn = null;
  S.status = "starting";
  S.exitCode = null;
  if (S.backend === "ac") {
    if (system.pty2.active) system.pty2.kill(); // a Claude child from before the switch
    S.bridge = new AcBridge({
      transport: acTransport(),
      token: S.token,
      model: S.model,
      instructions: instructionsFor({ handle: S.handle, slug: S.slug, file: S.file, screen, writeTool: true }),
      file: S.file,
      readPiece: () => LIVE.system.readFile(S.file) || "",
      writePiece: (source) => LIVE.system.writeFile(S.file, source) === true,
    });
    S.bridge.launch();
    S.bridge.handshake();
    return;
  }
  S.bridge = new Bridge({
    send: (line) => system.pty2.write(line + "\n"),
    model: S.model,
    cwd: PIECES,
    instructions: instructionsFor({ handle: S.handle, slug: S.slug, file: S.file, screen }),
  });
  const args = S.bridge.launch(resume);
  S.logTail = [];
  const ok = system.pty2.spawn("/bin/sh", pipedCommand(CLAUDE, args), 200, 50, { raw: true, cwd: PIECES });
  if (!ok) {
    S.status = "error";
    note("could not start " + CLAUDE, "error");
    return;
  }
  S.bridge.handshake();
}

function boot({ system, screen, params }) {
  SCREEN = screen;
  LIVE.system = system;
  const kept = globalThis.__aesel;
  if (kept && kept.bridge && (kept.backend === "ac" || (system.pty2 && system.pty2.active))) {
    S = kept; // Reattach: the conversation kept running while we were away.
    // The transport is this copy's, so a reattach after an update reads the
    // stream with the current code rather than the code the bridge began with.
    if (S.backend === "ac" && S.bridge && "transport" in S.bridge) S.bridge.transport = acTransport();
    for (const [key, value] of Object.entries(blankSession())) if (!(key in S)) S[key] = value;
    if (!S.egg) S.egg = eggFor(uuid());
    if (!S.colors) Object.assign(S, readAccount(system));
    S.input = "";
    S.cursor = 0;
    S.scroll = 0;
    return;
  }
  S = freshState(system, params);
  globalThis.__aesel = S;
  ensurePiece(system);
  // The child signs in with the OAuth token the flash baked in. Without one
  // the first turn fails with an auth error that says nothing about why.
  if (S.backend === "claude" && !system.readFile("/claude-token")) {
    note("no Claude token on this device: flash it with ac-os (not AC_ANON) to bake one", "error");
  }
  spawnBridge(system, screen);
}

// ── events from the bridge ──────────────────────────────────────────────

// A turn the child died in is not running, whatever the bridge last heard.
function busy() {
  return Boolean(S.bridge && S.bridge.busy) && S.status !== "closed" && S.status !== "error";
}

function absorb(event) {
  if (S.turn) S.turn.lastEventAt = Date.now();
  switch (event.type) {
    case "ready":
      S.status = "ready";
      drain();
      return;
    case "turn":
      if (event.status === "started") {
        S.turn = S.turn || newTurn();
        S.status = "waiting";
        return;
      }
      endTurn(event);
      return;
    case "phase":
      S.status = event.phase;
      return;
    case "delta": {
      S.status = "generating";
      const last = S.entries[S.entries.length - 1];
      if (last && last.kind === "agent" && last.open) last.text += event.text;
      else S.entries.push({ kind: "agent", text: event.text, open: true, id: nextId() });
      return;
    }
    case "message":
      closeAgent();
      note(event.text, "agent");
      return;
    case "tool":
      closeAgent();
      tool(event);
      return;
    case "usage":
      if (S.turn) { S.turn.tokens += event.tokens; S.turn.usd += event.usd; }
      S.spend.tokens += event.tokens;
      S.spend.usd += event.usd;
      return;
    case "approval":
      if (S.autoAllow) {
        S.bridge.approve(event.id, "accept");
        return;
      }
      S.approvals.push({ id: event.id, kind: event.kind, label: event.label });
      if (S.approvals.length === 1) S.approvalIndex = 0;
      S.status = "approval";
      return;
    case "approval-resolved":
      S.approvals = S.approvals.filter((a) => a.id !== event.id);
      return;
    case "warning":
      note(event.message);
      return;
    case "error":
      note(event.message, "error");
      if (event.fatal) S.status = "error";
      return;
    case "log":
      S.logTail.push(event.line);
      if (S.logTail.length > 6) S.logTail.shift();
      return;
    default:
  }
}

function newTurn() {
  const now = Date.now();
  return { startedAt: now, lastEventAt: now, tools: new Set(), feed: [], tokens: 0, usd: 0 };
}

// The live feed: what the machine is doing now, in words, never arguments.
function tool(event) {
  const turn = S.turn || (S.turn = newTurn());
  turn.tools.add(event.id);
  let row = turn.feed.find((r) => r.id === event.id);
  if (!row) {
    row = { id: event.id, text: toolLabel(event.name || (event.kind === "command" ? "Bash" : event.kind === "file" ? "Write" : "")), status: "running" };
    turn.feed.push(row);
    if (turn.feed.length > 8) turn.feed.shift();
  }
  if (event.status === "started") {
    if (row.status === "running") S.status = event.kind === "file" ? "writing" : "tool";
    return;
  }
  row.status = event.status === "failed" ? "failed" : "done";
  if (!turn.feed.some((r) => r.status === "running") && !S.approvals.length) S.status = "waiting";
  if (event.kind === "file" && event.status === "completed" && event.path === S.file) {
    S.pieceDirty = true;
    keepVersion(SYS.readFile(S.file) || "", "saved");
  }
}

function endTurn(event) {
  closeAgent();
  const turn = S.turn;
  S.turn = null;
  S.approvals = [];
  S.lastTurn = event.status;
  if (turn) {
    S.spend.turns += 1;
    note(turnUsageText({ status: event.status, tools: turn.tools.size, ms: Date.now() - turn.startedAt, tokens: turn.tokens, usd: turn.usd }), "usage");
  }
  S.status = event.status === "failed" ? "failed" : "ready";
  if (event.status === "failed") note(event.error || "turn failed", "error");
  // An interrupt is a decision about everything you were going to say, not
  // just the turn that was running, so ctrl-c drops the queue with it.
  if (event.status === "interrupted" && S.stoppedByUser && S.queued.length) {
    note(`Stopped · ${S.queued.length} follow-up message${S.queued.length === 1 ? " was" : "s were"} not sent.`);
    S.queued = [];
  }
  S.stoppedByUser = false;
  drain();
}

function closeAgent() {
  const last = S.entries[S.entries.length - 1];
  if (last && last.kind === "agent") last.open = false;
}

// Follow-ups typed during a turn go as one message once it ends.
function drain() {
  if (!S.queued.length || !S.bridge || !S.bridge.ready || busy()) return;
  const batch = S.queued.splice(0);
  startTurn(batch.join("\n"));
}

function startTurn(text) {
  closeAgent();
  S.turn = newTurn();
  S.status = "waiting";
  S.bridge.say(text);
}

// ── keyboard ────────────────────────────────────────────────────────────

function command(line, system, screen) {
  const [name, ...rest] = line.slice(1).split(/\s+/);
  const arg = rest.join(" ").trim();
  switch (name) {
    case "help":
      note(HELP);
      return;
    case "backend":
    case "provider":
    case "engine": {
      const target = arg === "aesthetic" || arg === "hosted" || arg === "free" ? "ac" : arg;
      if (target !== "ac" && target !== "claude") { note(`engine ${S.backend === "ac" ? "aesthetic" : "claude"} · /backend ac|claude`); return; }
      if (target === S.backend) { note(`already on ${target}`); return; }
      S.backend = target;
      S.model = target === "ac" ? DEFAULT_AC_MODEL : DEFAULT_MODEL;
      note(`engine ${target === "ac" ? "aesthetic" : "claude"}`);
      spawnBridge(system, screen, "");
      return;
    }
    case "new":
      S.queued = [];
      note("new conversation");
      spawnBridge(system, screen, "");
      return;
    case "clear":
      S.entries = [];
      S.scroll = 0;
      return;
    case "latest":
      S.scroll = 0;
      return;
    case "run":
      runPiece(system);
      return;
    case "preview":
      S.preview = arg === "on" ? true : arg === "off" ? false : !S.preview;
      note(S.preview ? "Preview on · the piece runs in the corner" : "Preview off · tab still runs the piece");
      return;
    case "piece":
      if (!arg) { note(`Working on ${S.file}`); return; }
      if (!/^[a-z0-9-]+$/.test(arg)) { note("usage: /piece <name> · lowercase letters, digits, dashes", "error"); return; }
      S.slug = arg;
      S.file = `${PIECES}/${arg}.mjs`;
      S.pieceDirty = false;
      ensurePiece(system);
      note(`Working on ${S.file}`);
      spawnBridge(system, screen, ""); // the instructions name the file
      return;
    case "versions":
      note(S.versions.map((v, i) => `v${i} · ${v.note}${v.at ? ` · ${clock(v.at)}` : ""} · ${v.source.split("\n").length} lines`).join("\n"));
      return;
    case "rollback": {
      const n = Number(/^v?(\d+)$/.exec(arg)?.[1] ?? NaN);
      const v = S.versions[n];
      if (!v) { note("Use /rollback v0 · /versions lists saved versions."); return; }
      if (busy()) { note("Wait for the current turn to finish before rolling back."); return; }
      if (system.writeFile(S.file, v.source) !== true) { note(`Could not write ${S.file}`, "error"); return; }
      keepVersion(v.source, `restored v${n}`);
      S.pieceDirty = true;
      note(`Restored v${n} as v${S.versions.length - 1}.`);
      return;
    }
    case "model":
    case "models":
      if (!arg) {
        note(S.backend === "ac"
          ? `model ${acModelLabel(S.model)} · /model ${Object.keys(AC_MODELS).join(" | ")}`
          : `model ${S.model} · /model opus | sonnet | haiku | a full model id`);
        return;
      }
      S.model = S.backend === "ac" ? resolveAcModel(arg) : arg;
      note(`Model · ${arg}`);
      spawnBridge(system, screen, "");
      return;
    case "ask": {
      const want = arg.toLowerCase();
      S.autoAllow = want === "on" ? false : want === "off" ? true : !S.autoAllow;
      note(S.autoAllow ? "Running without asking · /ask on to be asked first" : "Asking before each action · /ask off to stop asking");
      return;
    }
    case "retry":
      if (busy()) { flash("wait for the current turn, or ctrl-c to stop it"); return; }
      if (S.backend === "ac") {
        if (!S.bridge || !S.bridge.retry()) note("nothing to retry");
        else S.turn = newTurn();
        return;
      }
      note("reconnecting");
      spawnBridge(system, screen, S.bridge && S.bridge.turns ? S.bridge.sessionId : "");
      return;
    case "whoami":
      note(S.handle ? `You are @${S.handle}.` : "No handle on this device · type `link` at the prompt");
      return;
    case "energy":
    case "power":
      note(S.spend.tokens
        ? `${compact(S.spend.tokens)} tokens${S.spend.usd > 0 ? ` · ${dollars(S.spend.usd)}` : ""} across ${S.spend.turns} turn${S.spend.turns === 1 ? "" : "s"}`
        : "nothing spent yet");
      return;
    case "quit":
    case "exit":
      if (busy()) S.bridge.interrupt();
      if (system.pty2.active) system.pty2.kill();
      globalThis.__aesel = null;
      system.jump("prompt");
      return;
    default:
      note(`Unknown command: ${line}`, "error");
  }
}

function clock(ms) {
  const d = new Date(ms);
  return `${String(d.getHours()).padStart(2, "0")}:${String(d.getMinutes()).padStart(2, "0")}`;
}

function runPiece(system) {
  if (!system.readFile(S.file)) { note("nothing to run yet", "error"); return; }
  S.pieceDirty = false;
  system.jump(S.slug);
}

function submit(system, screen) {
  const text = S.input.trim();
  setInput("");
  if (!text) return;
  if (S.history[S.history.length - 1] !== text) S.history.push(text);
  S.historyIndex = S.history.length;
  if (text.startsWith("/")) { command(text, system, screen); return; }
  if (S.status === "error" || S.status === "closed" || !S.bridge) {
    note("the engine is not running · /retry or /new", "error");
    setInput(text);
    return;
  }
  S.scroll = 0;
  closeAgent();
  note(text, "user");
  if (busy() || !S.bridge.ready || S.queued.length) { S.queued.push(text); return; }
  startTurn(text);
}

function setInput(value) {
  S.input = value;
  S.cursor = value.length;
}

function answer(decision) {
  const pending = S.approvals.shift();
  if (!pending) return;
  S.bridge.approve(pending.id, decision);
  S.approvalIndex = 0;
  flash(`${decision === "decline" ? "denied" : "allowed"} · ${clip(pending.label.replace(/\s+/g, " "), 60)}`);
  S.status = S.approvals.length ? "approval" : busy() ? "tool" : "ready";
}

function act(api) {
  const { event: e, system, screen } = api;
  if (S && P && playPreview(api, e)) return;
  if (e.is("keyboard:down:shift")) { shiftHeld = true; return; }
  if (e.is("keyboard:up:shift")) { shiftHeld = false; return; }
  if (e.is("keyboard:down:control")) { ctrlHeld = true; return; }
  if (e.is("keyboard:up:control")) { ctrlHeld = false; return; }
  if (!e.is("keyboard:down") || !S) return;
  const key = e.key;

  // The question takes the page: arrows choose, enter answers, esc says no;
  // y, a and n still answer directly.
  if (S.approvals.length && !ctrlHeld) {
    const choices = ["accept", "acceptForSession", "decline"];
    if (key === "arrowup" || key === "arrowdown") { S.approvalIndex = (S.approvalIndex + (key === "arrowup" ? 2 : 1)) % 3; return; }
    if (key === "enter" || key === "return") { answer(choices[S.approvalIndex]); return; }
    if (key === "escape") { answer("decline"); return; }
  }

  if (key === "escape") { system.jump("prompt"); return; }

  if (ctrlHeld) {
    if (key === "c") {
      if (S.approvals.length) {
        // A question still on screen is answered no; the turn it belongs to stops.
        while (S.approvals.length) answer("decline");
      }
      if (busy()) {
        S.stoppedByUser = true;
        S.status = "interrupting";
        S.bridge.interrupt();
      } else if (S.queued.length) S.queued = [];
      else if (S.input) setInput("");
      else flash("/quit ends the session · esc goes to the prompt");
      return;
    }
    if (key === "d") { if (!S.input) command("/quit", system, screen); return; }
    if (key === "a") { S.cursor = 0; return; }
    if (key === "e") { S.cursor = S.input.length; return; }
    if (key === "u") { setInput(""); return; }
    if (key === "k") { S.input = S.input.slice(0, S.cursor); return; }
    return; // the system keeps ctrl with = - 0 for pixel size
  }

  // An approval owns y / a / n until it is answered.
  if (S.approvals.length && S.input === "") {
    const decision = key === "y" ? "accept" : key === "a" ? "acceptForSession" : key === "n" ? "decline" : null;
    if (decision) { answer(decision); return; }
  }

  if (key === "tab") { runPiece(system); return; }
  if (key === "pageup") { S.scroll += Math.max(1, pageRows - 1); return; }
  if (key === "pagedown") { S.scroll = Math.max(0, S.scroll - Math.max(1, pageRows - 1)); return; }
  if (shiftHeld && key === "arrowup") { S.scroll += 1; return; }
  if (shiftHeld && key === "arrowdown") { S.scroll = Math.max(0, S.scroll - 1); return; }
  if (key === "arrowup") {
    if (S.historyIndex > 0) setInput(S.history[--S.historyIndex]);
    return;
  }
  if (key === "arrowdown") {
    if (S.historyIndex < S.history.length - 1) setInput(S.history[++S.historyIndex]);
    else { S.historyIndex = S.history.length; setInput(""); }
    return;
  }
  if (key === "end") { if (S.input) S.cursor = S.input.length; else S.scroll = 0; return; }
  if (key === "home") { S.cursor = 0; return; }
  if (key === "arrowleft") { if (S.cursor > 0) S.cursor--; return; }
  if (key === "arrowright") { if (S.cursor < S.input.length) S.cursor++; return; }

  if (key === "enter" || key === "return") { submit(system, screen); return; }
  if (key === "backspace") {
    if (S.cursor > 0) { S.input = S.input.slice(0, S.cursor - 1) + S.input.slice(S.cursor); S.cursor--; }
    return;
  }
  if (key === "delete") { S.input = S.input.slice(0, S.cursor) + S.input.slice(S.cursor + 1); return; }

  let ch = null;
  if (key === "space") ch = " ";
  else if (key.length === 1) ch = shiftHeld ? (SHIFT_MAP[key] ?? key.toUpperCase()) : key;
  if (ch !== null) {
    S.input = S.input.slice(0, S.cursor) + ch + S.input.slice(S.cursor);
    S.cursor++;
    // A letter plays its notepat note's colour, the way a key lights there.
    const rgb = keyColor(key);
    if (rgb) S.keyFlash = { rgb, at: Date.now(), index: S.cursor - 1 };
  }
}

// ── drawing ─────────────────────────────────────────────────────────────

// Metrics for this screen: the 6x10 face at the largest integer scale that
// still leaves a conversation's worth of room, so strokes stay crisp and
// thick. 640x400 gets scale 2: 12x20 cells on 26 px rows.
function metrics(W, H) {
  const z = W >= 560 && H >= 340 ? 2 : 1;
  const cw = 6 * z;
  const gh = 10 * z;
  const lh = Math.round(gh * 1.3);
  return { z, cw, gh, lh, margin: 4 * z, pad: 5 * z, stroke: z };
}

function clean(text) {
  return String(text).replace(/\r/g, "").replace(/\t/g, "  ").replace(/[\x00-\x08\x0b-\x1f\x7f]/g, "").replace(/[\t ]+$/gm, "").replace(/^\n+|\n+$/g, "");
}

function clip(text, cols) {
  return text.length > cols ? text.slice(0, Math.max(0, cols - 1)) + "…" : text;
}

const mix = (a, b, k) => a.map((v, i) => Math.round(v + (b[i] - v) * k));

// A key's colour, faded toward white by its age; null once it is white.
function keyTint(now) {
  const f = S.keyFlash;
  if (!f) return null;
  const k = (now - f.at) / FADE;
  if (k >= 1) { S.keyFlash = null; return null; }
  return mix(f.rgb, C.stroke, k * k);
}

const TONES = {
  heading: { rgb: C.highlight, bold: true },
  bold: { bold: true },
  code: { rgb: C.soft },
  link: { rgb: C.soft, under: true },
};

// Lay spans back over a wrapped row as runs; a run with no rgb is white.
function runs(row, spans, base) {
  const out = [];
  const end = row.start + row.text.length;
  let at = row.start;
  for (const s of spans) {
    if (s.end <= at || s.start >= end) continue;
    const from = Math.max(at, s.start);
    if (from > at) out.push({ text: row.text.slice(at - row.start, from - row.start), rgb: base });
    const to = Math.min(end, s.end);
    out.push({ text: row.text.slice(from - row.start, to - row.start), ...TONES[s.tone] });
    at = to;
  }
  if (at < end) out.push({ text: row.text.slice(at - row.start), rgb: base });
  return out;
}

// Your words in a white outline, replies flush, a notice or an error behind
// a one-cell mark. Rows carry a height in rows: borders and the air between
// messages are half a row, so the page breathes without losing lines.
function entryRows(entry, cols) {
  const key = `${entry.kind}:${cols}:${entry.text.length}`;
  if (entry._key === key) return entry._rows;
  const rows = [];
  const text = clean(entry.text);
  if (entry.kind === "user") {
    const inner = Math.max(1, cols - 4);
    const wrapped = wrapRows(text, inner);
    const width = Math.min(inner, Math.max(1, ...wrapped.map((r) => r.text.length)));
    rows.push({ box: "top", width, h: 0.5 });
    for (const r of wrapped) rows.push({ box: "row", width, typed: r.text, h: 1 });
    rows.push({ box: "bottom", width, h: 0.5 });
  } else if (entry.kind === "usage") {
    for (const r of wrapRows(text, cols)) rows.push({ runs: [{ text: r.text, rgb: C.dim }], h: 1 });
  } else if (entry.kind === "agent") {
    // Code fences stand on their own ground, the fence lines dim.
    let fenced = false;
    for (const part of text.split(/(^[ \t]*```[^\n]*$)/m)) {
      if (/^[ \t]*```/.test(part)) {
        fenced = !fenced;
        rows.push({ runs: [{ text: part.trim(), rgb: C.dim }], h: 1 });
        continue;
      }
      const chunk = part.replace(/^\n|\n$/g, "");
      if (!chunk && part !== text) continue;
      if (fenced) {
        for (const r of wrapRows(chunk, cols)) rows.push({ runs: [{ text: r.text, rgb: C.code }], ground: true, h: 1 });
        continue;
      }
      const read = markdown(chunk);
      for (const r of wrapRows(read.text, cols)) rows.push({ runs: runs(r, read.spans), h: 1 });
    }
  } else {
    const mark = entry.kind === "error" ? "!" : "·";
    const rgb = entry.kind === "error" ? C.error : C.muted;
    wrapRows(text, cols - 2).forEach((r, i) => rows.push({ runs: [{ text: i ? "  " : `${mark} `, rgb, bold: entry.kind === "error" }, { text: r.text, rgb }], h: 1 }));
  }
  entry._key = key;
  entry._rows = rows;
  return rows;
}

// A run of typed lines is one outline: what you said before the machine
// answered, together, rather than a stack of boxes.
function grouped(entries) {
  const out = [];
  for (const e of entries) {
    const last = out[out.length - 1];
    if (e.kind === "user" && last && last.kind === "user") {
      if (!last.merged) out[out.length - 1] = { kind: "user", text: last.text, merged: true };
      out[out.length - 1].text += "\n" + e.text;
      continue;
    }
    out.push(e);
  }
  return out;
}

function frame4(box, x, y, w, h, t) {
  box(x, y, w, t);
  box(x, y + h - t, w, t);
  box(x, y, t, h);
  box(x + w - t, y, t, h);
}

function drawRuns(write, ink, line, list, x, y, m) {
  for (const r of list) {
    if (!r.text) continue;
    ink(...(r.rgb || C.text));
    write(r.text, { x, y, size: m.z, font: FONT });
    if (r.bold) write(r.text, { x: x + 1, y, size: m.z, font: FONT });
    if (r.under) line(x, y + m.gh, x + r.text.length * m.cw - 1, y + m.gh);
    x += r.text.length * m.cw;
  }
  return x;
}

function modelLabel() {
  if (S.backend === "ac") return `aesthetic · ${S.model.split("/").pop() || "automatic"}`;
  // claude-opus-4-8 → Claude Opus 4.8, the catalog's display name.
  const words = [];
  for (const p of S.model.split("-")) {
    if (/^\d+$/.test(p) && /\d$/.test(words[words.length - 1] || "")) words[words.length - 1] += "." + p;
    else words.push(/^\d/.test(p) ? p : p[0].toUpperCase() + p.slice(1));
  }
  return words.join(" ");
}

// Which of Slab's pages the session is on.
function pageState() {
  if (S.status === "error" || S.status === "closed") return "stale";
  if (S.approvals.length) return "awaiting";
  if (busy()) return "working";
  return { completed: "complete", interrupted: "interrupted", failed: "stale" }[S.lastTurn] || "blank";
}

// How the egg holds itself: Slab's moods, from this session's state.
function mood() {
  if (S.status === "error" || S.status === "closed") return "sleeping";
  if (S.approvals.length || S.lastTurn === "interrupted" && !busy()) return "waiting";
  if (busy()) return "working";
  return S.lastTurn === "completed" ? "resting" : "sleeping";
}

// The egg, as ProxCreatureFrames lights it, in greys: a shell that narrows
// toward the top, breathing, freckled, with a face that sleeps, rests,
// works or waits. Returns the row under it.
function drawEgg(ink, box, plot, cx, top, U, now) {
  const { look } = S.egg;
  const m = mood();
  const energy = m === "working" ? 1 : m === "waiting" ? 0.75 : 0.35;
  const period = m === "working" ? 2400 : m === "waiting" ? 3600 : 5000;
  const t = (now % period) / period;
  const a = t * Math.PI * 2;
  const sx = 1 - Math.sin(a) * 0.012 * energy;
  const sy = 1 + Math.sin(a) * 0.018 * energy;
  const cy = top + 1.02 * U + Math.sin(a * 2) * 0.032 * energy * U;
  const half = 1.02 * U * sy;
  const span = (yy) => {
    const v = (cy - yy) / half;
    if (v <= -1 || v >= 1) return 0;
    return look.width * U * sx * Math.sqrt(1 - v * v) * (1 - 0.2 * v);
  };
  for (let yy = Math.floor(cy - half); yy <= Math.ceil(cy + half); yy++) {
    const w = span(yy);
    if (w < 0.5) continue;
    const v = (cy - yy) / half;
    const x0 = Math.round(cx - w);
    const x1 = Math.round(cx + w);
    let runStart = x0;
    let runLevel = -1;
    for (let xx = x0; xx <= x1 + 1; xx++) {
      const nx = (xx + 0.5 - cx) / w;
      const nz = Math.sqrt(Math.max(0, 1 - nx * nx - v * v * 0.6));
      const d = Math.max(0, -0.36 * nx + 0.62 * v + 0.64 * nz);
      const level = xx > x1 ? -1 : xx === x1 || xx === x0 ? 1 : Math.min(5, Math.round((0.5 + 0.55 * d) * 5));
      if (level !== runLevel) {
        if (runLevel > 0) {
          const g = Math.round(255 * runLevel / 5);
          ink(g, g, g);
          box(runStart, yy, xx - runStart, 1);
        }
        runStart = xx;
        runLevel = level;
      }
    }
  }
  const at = (ux, uy) => [Math.round(cx + ux * U * sx), Math.round(cy - uy * U * sy)];
  ink(...C.muted);
  for (const f of look.freckles) {
    const [fx, fy] = at(f.x * 0.82, f.y);
    if (Math.abs(fx - cx) < span(fy) - 1) plot(fx, fy);
  }
  // Eyes: open, blinking, or the two short lines of a sleeping egg.
  const blink = Math.max(0, 1 - Math.abs(t - look.blinkAt) / 0.045);
  const open = m !== "sleeping" && blink < 0.5;
  const e = Math.max(1, Math.round(U / 10));
  ink(...C.page);
  for (const sign of [-1, 1]) {
    const [ex, ey] = at(sign * look.eyeSpread, 0.10);
    if (open) box(ex - e, ey - e * 1.5, e * 2, e * 3);
    else box(ex - e * 1.5, ey, e * 3, e);
  }
  const [mx, my] = at(0, -0.2);
  if (m === "waiting") box(mx - e, my - e, e * 2, e * 2);
  else { box(mx - e * 2, my, e, e); box(mx - e, my + e, e * 2, e); box(mx + e, my, e, e); }
  return Math.ceil(cy + half);
}

// `28.3s…`, and `· quiet 20s` once nothing has arrived for a while.
function workingTimer(now) {
  const t = S.turn;
  if (!t) return "";
  const quiet = Math.floor((now - t.lastEventAt) / 1000);
  return `${((now - t.startedAt) / 1000).toFixed(1)}s…${quiet >= 15 && S.status !== "approval" ? ` · quiet ${quiet}s` : ""}`;
}

function paint(api) {
  const t0 = Date.now();
  const previewMs = paintSession(api);
  if (S) perfFrame(Date.now() - t0, previewMs);
}

function paintSession(api) {
  const { wipe, ink, box, line, plot, write, screen, system, wifi } = api;
  if (!S) return 0;
  frame++;
  SYS = system;
  LIVE.frame++;
  LIVE.system = system;
  SCREEN = screen;
  const W = screen.width;
  const H = screen.height;
  const now = Date.now();

  // Pump the bridge. Claude's lines are handed out once per paint, so every
  // paint reads them; the hosted engine polls its stream file on tick().
  const pty = system.pty2;
  if (S.bridge) {
    if (S.backend === "ac") {
      S.bridge.tick();
    } else if (pty) {
      if (pty.lines && pty.lines.length) for (const l of pty.lines) S.bridge.feed(l);
      if (pty.overflow) note("bridge output overflowed; a message was lost", "error");
      if (!pty.active && S.status !== "error" && S.status !== "closed") {
        for (const event of S.bridge.take()) absorb(event);
        S.status = "closed";
        S.exitCode = pty.exitCode;
        S.turn = null;
        note(`engine bridge closed (${pty.exitCode}) · /retry reconnects · /new starts over`, "error");
        // Whatever the child said on its way out is the only explanation there is.
        for (const l of S.logTail.slice(-3)) note(l, "soft");
        console.log(`[aesel] bridge closed (${pty.exitCode}): ${S.logTail.join(" | ") || "no output"}`);
      }
    }
    for (const event of S.bridge.take()) absorb(event);
  }

  // A line from another machine: `echo "make it rain" > /tmp/aesel-say` over
  // ssh is submitted as if typed here, slash commands included.
  if (frame % 15 === 0) {
    const said = system.readFile(SAY_FILE);
    if (said && said.trim()) {
      system.deleteFile(SAY_FILE);
      const typed = S.input;
      setInput(said.trim().replace(/\s*\n\s*/g, " "));
      submit(system, screen);
      if (typed) setInput(typed);
    }
  }
  if (frame % 15 === 0) writeState(system);

  const m = metrics(W, H);
  const { z, cw, gh, lh, margin, pad, stroke } = m;
  const page = PAGES[pageState()];
  S.ground = S.ground ? mix(S.ground, page.bg, 0.12) : page.bg.slice();
  S.ink = S.ink ? mix(S.ink, page.text, 0.12) : page.text.slice();
  S.shade = S.shade ? mix(S.shade, page.shadow, 0.12) : page.shadow.slice();
  C.page = S.ground.map(Math.round);
  C.text = S.ink.map(Math.round);
  C.stroke = S.shade.map(Math.round);
  C.ground = mix(C.page, [0, 0, 0], 0.45).map(Math.round);
  wipe(...C.page);
  const tint = keyTint(now);
  const working = busy();
  const offline = wifi && !wifi.connected;
  const inner = W - 2 * (margin + stroke + pad);
  const cols = Math.max(16, Math.floor(inner / cw));
  const textX = margin + stroke + pad;

  // Bottom up: the status line under a rule, the input box, the transcript box.
  const statusY = H - margin - gh;
  const ruleY = statusY - pad - stroke;
  const inputH = gh + 2 * (pad + stroke);
  const inputY = ruleY - margin - inputH;
  const boxY = margin;
  const boxH = inputY - margin - boxY;

  // ── the status line: handle · piece · model · activity, spend flush right.
  ink(...C.stroke);
  box(margin, ruleY, W - 2 * margin, stroke);
  const account = S.handle ? `@${S.handle}` : "";
  const spend = S.spend.tokens ? `${compact(S.spend.tokens)} tok${S.spend.usd > 0 ? ` · ${dollars(S.spend.usd)}` : ""}` : "";
  const flashing = S.flash && S.flash.until > now ? S.flash.text : "";
  const guy = GUY_DANCE[Math.floor(now / 320) % GUY_DANCE.length];
  const statusCols = Math.floor((W - 2 * margin) / cw);
  const activity = working ? `${guy} ${workingTimer(now)}`
    : flashing || (S.status === "starting" ? "connecting…" : offline ? "offline"
      : S.status === "closed" || S.status === "error" ? "engine closed · /retry"
      : S.scroll ? `${S.scroll} lines above · End latest` : "");
  const facts = [
    ["handle", account],
    ["workspace", clip(S.file.replace(/^\/pieces\//, ""), Math.max(8, Math.floor(statusCols / 4)))],
    ["model", modelLabel()],
    ["activity", activity],
  ];
  // Short of room, the piece goes first, then the model, then the handle.
  // At this size the spend gives way before anything on the left does.
  const keep = new Set(facts.map(([n]) => n));
  const left = () => facts.reduce((n, [name, text]) => (keep.has(name) && text ? n + text.length + (n ? SEP.length : 0) : n), 0);
  const showSpend = spend && left() + spend.length + 2 <= statusCols;
  for (const name of ["workspace", "model", "handle"]) if (left() > statusCols) keep.delete(name);
  let x = margin;
  let first = true;
  for (const [name, text] of facts) {
    if (!keep.has(name) || !text) continue;
    if (!first) { ink(...C.dim); write(SEP, { x, y: statusY, size: z, font: FONT }); x += SEP.length * cw; }
    first = false;
    if (name === "handle") {
      // The handle in its own colours, as the terminal draws it.
      for (let i = 0; i < text.length; i++) {
        ink(...(S.colors[i] || C.soft));
        write(text[i], { x: x + i * cw, y: statusY, size: z, font: FONT });
      }
    } else {
      ink(...(name === "model" ? C.soft : name === "workspace" ? C.muted
        : offline || S.status === "closed" || S.status === "error" ? C.error : S.status === "starting" ? C.run : C.status));
      write(text, { x, y: statusY, size: z, font: FONT });
    }
    x += text.length * cw;
  }
  if (showSpend) {
    ink(...C.muted);
    write(spend, { x: W - margin - spend.length * cw, y: statusY, size: z, font: FONT });
  }

  // ── the input box: white, or the colour of the key just played.
  const pending = S.approvals[0];
  ink(...(tint || C.stroke));
  frame4(box, margin, inputY, W - 2 * margin, inputH, stroke);
  const inY = inputY + stroke + pad;
  if (pending) {
    ink(...C.muted);
    write(clip("↑↓ choose · enter answers · esc denies", cols), { x: textX, y: inY, size: z, font: FONT });
  } else {
    const start = Math.max(0, S.cursor - cols + 1);
    const lead = start > 0 ? "‹" : "";
    const shown = S.input.slice(start, start + cols - lead.length);
    ink(...C.text);
    write(lead + shown, { x: textX, y: inY, size: z, font: FONT });
    // The letter just typed keeps its note's colour while it fades.
    const fi = S.keyFlash ? S.keyFlash.index - start : -1;
    if (tint && fi >= 0 && fi < shown.length) {
      ink(...tint);
      write(shown[fi], { x: textX + (lead.length + fi) * cw, y: inY, size: z, font: FONT });
    }
    const cx = textX + (lead.length + S.cursor - start) * cw;
    ink(...C.prompt);
    box(cx, inY - z, cw, gh + 2 * z);
    const under = S.input[S.cursor];
    if (under) { ink(...C.page); write(under, { x: cx, y: inY, size: z, font: FONT }); }
  }

  // ── the transcript box, the conversation bottom-aligned inside it.
  ink(...C.stroke);
  frame4(box, margin, boxY, W - 2 * margin, boxH, stroke);
  // The conversation runs full width; the preview sits on top of it.
  const top = boxY + stroke + pad;
  const bottom = boxY + boxH - stroke - pad;
  const feedSize = Math.min(3, Math.max(1, Math.floor((bottom - top) / lh / 5)));
  const feed = working && S.turn ? S.turn.feed.slice(-feedSize) : [];
  const rows = [];
  const list = grouped(S.entries.filter((e) => e.kind !== "tool"));
  let lastTyped = -1;
  list.forEach((e, i) => { if (e.kind === "user") lastTyped = i; });
  list.forEach((e, i) => {
    if (i) rows.push({ air: true, h: 0.5 }); // room between messages
    for (const r of entryRows(e, cols)) rows.push(working && i === lastTyped && r.typed !== undefined ? { ...r, wave: true } : r);
  });
  const room = Math.floor((bottom - top) / lh) - feed.length;
  pageRows = Math.max(1, room);
  // Scrolled back, new rows arriving below keep the view where it was.
  if (S.scroll > 0 && rows.length > S.lastTotal) S.scroll += rows.length - S.lastTotal;
  S.lastTotal = rows.length;
  S.scroll = Math.min(S.scroll, Math.max(0, rows.length - room));

  if (pending) {
    drawApproval(ink, box, write, line, pending, textX, cols, top, bottom, m);
  } else {
    const end = rows.length - S.scroll;
    let begin = end;
    let used = 0;
    while (begin > 0 && used + rows[begin - 1].h <= room) used += rows[--begin].h;
    let y = bottom - Math.round((used + feed.length) * lh);
    const head = Math.floor(now / 120);
    for (let i = begin; i < end; i++) {
      drawRow(ink, box, write, line, rows[i], textX, y, cols, head, m);
      y += Math.round(rows[i].h * lh);
    }
    for (const r of feed) {
      const mark = r.status === "failed" ? "!" : r.status === "done" ? "·" : "›";
      ink(...(r.status === "running" ? C.text : C.muted));
      write(clip(`${mark} ${r.text}`, cols), { x: textX, y: y + (lh - gh) / 2, size: z, font: FONT });
      y += lh;
    }
  }

  // ── the piece, live, in the transcript's top-left corner.
  let previewMs = 0;
  if (S.preview && !pending) {
    const t = Date.now();
    drawPreview(api, margin + stroke, boxY + stroke, m);
    previewMs = Date.now() - t;
  }

  // ── the egg and its name, in the transcript's top-right corner, on black.
  const U = 9 * z;
  const name = S.egg.name;
  const ew = Math.max(2 * U, name.length * cw) + 2 * pad;
  const ex = W - margin - stroke - ew;
  ink(...C.page);
  box(ex, boxY + stroke, ew, Math.round(2.1 * U) + gh + 2 * pad + z);
  const ecx = ex + ew / 2;
  const below = drawEgg(ink, box, plot, ecx, boxY + stroke + pad, U, now);
  ink(...C.muted);
  write(name, { x: Math.round(ecx - (name.length * cw) / 2), y: below + z * 2, size: z, font: FONT });
  return previewMs;
}

// The preview can be played where it sits. A touch inside the frame hands it
// the pointer and the keyboard; its events arrive in the frame's own
// coordinates, with sound, since now someone is playing it. Escape gives the
// keyboard back and tab still runs the piece full-screen. Returns true when
// the event was the preview's.
function playPreview(api, e) {
  const r = P.rect;
  const pointer = e.is("touch") || e.is("draw") || e.is("lift") || e.is("move");
  if (pointer && r) {
    const inside = e.x >= r.x && e.y >= r.y && e.x < r.x + r.w && e.y < r.y + r.h;
    if (e.is("touch")) { P.focus = inside && S.preview; P.held = P.focus; }
    if (!(P.held || (P.focus && inside))) return false;
    if (e.is("lift")) P.held = false;
    return forward(api, e, e.x - r.x, e.y - r.y);
  }
  if (!P.focus || !(e.is("keyboard:down") || e.is("keyboard:up"))) return false;
  if (e.is("keyboard:down") && (e.key === "escape" || !S.preview)) { P.focus = false; return true; }
  if (e.is("keyboard:down") && e.key === "tab") { P.focus = false; runPiece(api.system); return true; }
  return forward(api, e);
}

function forward(api, e, x, y) {
  if (!P.piece || !P.piece.act) return true;
  const event = Object.create(e);
  if (x !== undefined) { event.x = x; event.y = y; event.pointer = { x, y }; }
  try {
    P.piece.act({ ...P.api, event, sound: api.sound, screen: { width: P.w, height: P.h } });
  } catch (err) {
    P.piece = null;
    P.error = String(err && err.message || err);
  }
  return true;
}

// Sound and leaving stay with the real run: the preview is seen, not heard,
// and a piece that jumps away would take Aesel with it.
const SILENT = new Proxy(function () {}, { get: (t, k) => (k === Symbol.toPrimitive ? undefined : SILENT), apply: () => SILENT });
const QUIET = ["jump", "writeFile", "deleteFile", "reboot", "poweroff"];

// The session's piece, compiled in here (pieceFactory) and painted into its
// own buffer at a third of the screen, re-read twice a second so every save
// the agent makes shows up without a key. Errors land in the frame.
function drawPreview(api, x0, y0, m) {
  const { ink, box, write, painting, page, paste, system, screen } = api;
  const { z, cw, gh, pad, stroke } = m;
  if (!painting || !page || !paste) return;
  const { w, h } = previewBox(screen, m, x0, y0);
  if (!P || P.w !== w || P.h !== h) P = { source: null, piece: null, buf: painting(w, h), w, h, count: 0, error: "" };
  const fx = x0 + pad, fy = y0 + pad;

  if (P.source === null || frame % 30 === 0) {
    const source = system.readFile(S.file) || "";
    if (source !== P.source) {
      P.source = source;
      P.piece = null;
      P.error = "";
      P.count = 0;
      page(P.buf).wipe(0, 0, 0);
      page();
      const made = pieceFactory(source);
      if (made.error) P.error = made.error;
      else {
        P.api = previewApi(api, w, h);
        try {
          P.piece = made.make();
          page(P.buf);
          if (P.piece.boot) P.piece.boot({ ...P.api, params: [] });
        } catch (err) {
          P.piece = null;
          P.error = String(err && err.message || err);
        } finally {
          page();
        }
      }
    }
  }

  if (P.piece && P.piece.paint) {
    P.api.screen = { width: w, height: h };
    P.api.paintCount = P.count++;
    try {
      page(P.buf);
      if (P.piece.sim) P.piece.sim(P.api);
      P.piece.paint(P.api);
    } catch (err) {
      P.piece = null;
      P.error = String(err && err.message || err);
      console.log(`[aesel] preview: ${P.error}`);
    } finally {
      page();
    }
  }

  // Black around it, a white frame, and under it the piece's name.
  ink(...C.page);
  box(x0, y0, w + 2 * (pad + stroke), h + 2 * (pad + stroke) + gh + pad);
  paste(P.buf, fx + stroke, fy + stroke);
  P.rect = { x: fx + stroke, y: fy + stroke, w, h };
  ink(...(P.error ? C.error : P.focus ? C.highlight : C.stroke));
  frame4(box, fx, fy, w + 2 * stroke, h + 2 * stroke, stroke);
  if (P.error) {
    ink(...C.page);
    box(fx + stroke, fy + stroke, w, h);
    ink(...C.muted);
    const cols = Math.max(4, Math.floor((w - 2 * z) / cw));
    const rows = wrapRows(P.error, cols).slice(0, Math.max(1, Math.floor((h - 2 * z) / (gh + z))));
    rows.forEach((r, i) => write(r.text, { x: fx + stroke + z, y: fy + stroke + z + i * (gh + z), size: z, font: FONT }));
  }
  ink(...C.muted);
  if (P.focus) ink(...C.highlight);
  write(clip(P.focus ? "playing · esc returns" : `${S.slug} · tab runs`, Math.floor((w + 2 * stroke) / cw)), { x: fx, y: fy + h + 2 * stroke + pad, size: z, font: FONT });
}

// Where the preview sits: the frame a third of the screen wide at (x0, y0),
// with its caption under it; `bottom` is the first row free below.
function previewBox(screen, m, x0, y0) {
  const { pad, stroke, gh } = m;
  const w = Math.max(48, Math.round(screen.width / 3));
  const h = Math.max(27, Math.round((w * screen.height) / screen.width));
  return { w, h, bottom: y0 + 2 * pad + h + 2 * stroke + pad + gh };
}

// What the previewed piece is handed: Aesel's own paint API, with the screen
// shrunk to the frame, sound silenced and the system's exits closed.
function previewApi(api, w, h) {
  const system = Object.create(api.system || {});
  for (const k of QUIET) system[k] = () => false;
  return { ...api, system, sound: SILENT, screen: { width: w, height: h }, paintCount: 0 };
}

function drawRow(ink, box, write, line, r, x, y, cols, head, m) {
  const { z, cw, gh, lh } = m;
  const ty = y + Math.round((lh - gh) / 2);
  if (r.air) return;
  if (r.box) {
    // Your words in a pink outline, squared and drawn, not typed.
    const left = x;
    const w = (r.width + 2) * cw;
    const h = Math.round(lh / 2);
    ink(...C.prompt);
    if (r.box === "top") { box(left, y + h - z, w, z); box(left, y + h - z, z, h + z); box(left + w - z, y + h - z, z, h + z); }
    else if (r.box === "bottom") { box(left, y, w, z); box(left, y - z, z, z); box(left + w - z, y - z, z, z); box(left, y, z, z); box(left + w - z, y, z, z); }
    else {
      box(left, y, z, lh);
      box(left + w - z, y, z, lh);
      const tx = left + cw;
      if (!r.wave) { ink(...C.text); write(r.typed, { x: tx, y: ty, size: z, font: FONT }); return; }
      // While the machine has the floor a spark travels through what you
      // said, three letters wide, pink and orange.
      const spark = [C.prompt, C.highlight, C.prompt];
      for (let i = 0; i < r.typed.length; i++) {
        const d = i - (head % Math.max(1, r.typed.length + 3));
        ink(...(d >= -2 && d <= 0 ? spark[d + 2] : C.text));
        write(r.typed[i], { x: tx + i * cw, y: ty, size: z, font: FONT });
      }
    }
    return;
  }
  if (r.ground) {
    ink(...C.ground);
    box(x - z * 2, y, cols * cw + z * 4, lh);
  }
  drawRuns(write, ink, line, r.runs, x, ty, m);
}

// The question, centred, with its three answers under it; the one in hand
// is white with black words, and y, a and n follow each answer.
function drawApproval(ink, box, write, line, pending, x0, cols, top, bottom, m) {
  const { z, cw, gh, lh } = m;
  const inner = Math.min(cols - 4, 76);
  const subject = wrapRows(clean(pending.label) || "requested action", inner).map((r) => r.text).slice(0, 4);
  const choices = [["Allow once", "y"], ["Allow every time this session", "a"], ["Deny", "n"]];
  const count = 2 + subject.length + 1 + choices.length;
  let y = top + Math.max(0, Math.floor(((bottom - top) / lh - count) / 2)) * lh;
  const left = x0 + Math.max(0, Math.floor((cols - inner) / 2)) * cw;
  const ty = (lh - gh) / 2;
  drawRuns(write, ink, line, [{ text: "allow?", rgb: C.highlight, bold: true }], left, y + ty, m);
  y += 2 * lh;
  for (const l of subject) { ink(...C.text); write(l, { x: left, y: y + ty, size: z, font: FONT }); y += lh; }
  y += lh;
  choices.forEach(([label, key], i) => {
    const text = ` ${clip(label, inner - 4)} `;
    if (i === S.approvalIndex) {
      ink(...C.prompt);
      box(left, y, text.length * cw, lh);
      drawRuns(write, ink, line, [{ text, rgb: [255, 255, 255], bold: true }], left, y + ty, m);
    } else drawRuns(write, ink, line, [{ text, rgb: C.muted }], left, y + ty, m);
    ink(...C.dim);
    write(`  ${key}`, { x: left + text.length * cw, y: y + ty, size: z, font: FONT });
    y += lh;
  });
}

function leave() {
  // The bridge child stays alive on purpose: leaving for the prompt or to run
  // the piece is not the end of the conversation. /quit ends it.
}

export { boot, act, paint, leave };
