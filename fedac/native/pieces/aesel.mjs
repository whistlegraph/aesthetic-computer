// aesel.mjs — Aesel, on AC Native.
//
// The desktop Aesel is an Electron shell around a conversation with an agent
// and the piece it is writing. This machine has no Electron and no browser;
// it has a framebuffer, a keyboard, QuickJS, and the Claude Code binary. So
// this is the same session drawn as a piece: the transcript in the middle, a
// pink prompt at the bottom, approvals answered with y / a / n, and the piece
// the agent writes sitting in /pieces where one key runs it on this machine.
//
// The bridge is Claude's headless stream-json protocol over a raw PTY
// (system.pty2 with { raw: true }); lib/aesel-bridge.mjs translates it. The
// child stays alive when the user leaves for the prompt or runs the piece —
// `aesel` from the prompt reattaches to the same conversation.

import { Bridge, DEFAULT_MODEL, blankPiece, instructionsFor, randomSlug } from "../lib/aesel-bridge.mjs";
import { AcBridge, DEFAULT_AC_MODEL, acModelLabel, resolveAcModel } from "../lib/aesel-ac.mjs";

const PALETTE = {
  background: [70, 50, 100],
  text: [255, 255, 255],
  prompt: [200, 30, 100],
  highlight: [255, 100, 0],
  handle: [255, 100, 255],
  soft: [220, 180, 255],
  muted: [170, 150, 205],
  status: [0, 255, 0],
  error: [255, 90, 90],
};

const FONT = "6x10";
const CW = 6;
const CH = 10;
const PAD = 4;
const CLAUDE = "/bin/claude";
const PIECES = "/pieces";
// The hosted engine: aesthetic.computer buys the inference and meters it
// against the device's @handle. Its stream lands in this file, re-read once a
// frame (see acTransport), because the runtime's fetch is curl, not fetch().
const SITE = "https://aesthetic.computer";
const STREAM_FILE = "/tmp/aesel-inference.sse";
const MAX_ENTRIES = 400;

const SHIFT_MAP = {
  "1": "!", "2": "@", "3": "#", "4": "$", "5": "%", "6": "^", "7": "&", "8": "*", "9": "(", "0": ")",
  "-": "_", "=": "+", "[": "{", "]": "}", ";": ":", "'": '"', ",": "<", ".": ">", "/": "?", "\\": "|", "`": "~",
};

// Session state lives on globalThis so it survives a jump to the prompt or
// to the piece being written; the module itself is re-evaluated on return.
let S = null;
let shiftHeld = false;
let ctrlHeld = false;
// The system object of the current paint, for the hosted transport's polling.
let SYS = null;
let frame = 0;

// `aesel` → Claude on the default model; `aesel:ac` or `aesel:ac:qwen` → the
// hosted engine; `aesel:claude:<model>` or `aesel:<model>` → Claude on that model.
function parseParams(params) {
  const [first, second] = params || [];
  if (first === "ac" || first === "aesthetic" || first === "hosted") {
    return { backend: "ac", model: resolveAcModel(second || "") };
  }
  if (first === "claude") return { backend: "claude", model: second || DEFAULT_MODEL };
  return { backend: "claude", model: first || DEFAULT_MODEL };
}

function freshState(system, params) {
  const handle = readHandle(system);
  const slug = randomSlug();
  const file = `${PIECES}/${slug}.mjs`;
  const { backend, model } = parseParams(params);
  return {
    handle,
    token: readToken(system),
    backend,
    slug,
    file,
    model,
    bridge: null,
    entries: [],     // {kind, text, id?, status?, label?}
    input: "",
    cursor: 0,
    scroll: 0,       // lines scrolled back from the bottom
    status: "starting",
    pending: null,   // approval waiting for y / a / n
    pieceDirty: false,
    pieceWrites: 0,
    exitCode: null,
  };
}

function readHandle(system) {
  let handle = system?.config?.handle || "";
  if (!handle) {
    try {
      const raw = system.readFile("/mnt/config.json");
      if (raw) handle = JSON.parse(raw).handle || "";
    } catch (_) {}
  }
  return handle === "anonymous" ? "" : handle;
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
// hands curl a file to stream into; poll() re-reads that file. The request is
// not "done" on the frame it started, because the system object of that paint
// was built before curl existed.
function acTransport() {
  let startedAt = -1;
  return {
    start(body, headers) {
      startedAt = frame;
      const ok = SYS.fetchPost(`${SITE}/api/easel-inference`, body, JSON.stringify(headers), { out: STREAM_FILE, timeout: 600 });
      return ok === true;
    },
    poll() {
      const text = SYS.readFile(STREAM_FILE) || "";
      const done = frame > startedAt && !SYS.fetchPending;
      return { text, done, error: done ? (SYS.fetchError || "") : "" };
    },
    cancel() {
      SYS.fetchCancel();
    },
  };
}

function note(text, kind = "note") {
  S.entries.push({ kind, text });
  if (S.entries.length > MAX_ENTRIES) S.entries.splice(0, S.entries.length - MAX_ENTRIES);
  S.scroll = 0;
}

function ensurePiece(system) {
  const existing = system.readFile(S.file);
  if (existing) return;
  system.writeFile(S.file, blankPiece(S.slug));
}

function spawnBridge(system, screen, resume = "") {
  SYS = system;
  if (S.bridge instanceof AcBridge && S.bridge.busy) S.bridge.interrupt();
  if (S.backend === "ac") {
    if (system.pty2.active) system.pty2.kill(); // a Claude child from before the switch
    S.bridge = new AcBridge({
      transport: acTransport(),
      token: S.token,
      model: S.model,
      instructions: instructionsFor({ handle: S.handle, slug: S.slug, file: S.file, screen, writeTool: true }),
      file: S.file,
      readPiece: () => SYS.readFile(S.file) || "",
      writePiece: (source) => SYS.writeFile(S.file, source) === true,
    });
    S.bridge.launch();
    S.status = "starting";
    S.exitCode = null;
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
  S.status = "starting";
  S.exitCode = null;
  const ok = system.pty2.spawn(CLAUDE, args, 200, 50, { raw: true, cwd: PIECES });
  if (!ok) {
    S.status = "error";
    note("could not start " + CLAUDE, "error");
    return;
  }
  S.bridge.handshake();
}

function boot({ system, screen, params }) {
  const kept = globalThis.__aesel;
  if (kept && kept.bridge && (kept.backend === "ac" || (system.pty2 && system.pty2.active))) {
    S = kept; // Reattach: the conversation kept running while we were away.
    S.input = "";
    S.cursor = 0;
    return;
  }
  S = freshState(system, params);
  globalThis.__aesel = S;
  ensurePiece(system);
  note(`piece ${S.file} · engine ${S.backend === "ac" ? "aesthetic" : "claude"}`);
  note("type to talk · enter sends · tab runs the piece here · esc back to prompt · /help");
  // The child signs in with the OAuth token the flash baked in. Without one
  // the first turn fails with an auth error that says nothing about why.
  if (S.backend === "claude" && !system.readFile("/claude-token")) {
    note("no Claude token on this device: flash it with ac-os (not AC_ANON) to bake one", "error");
  }
  spawnBridge(system, screen);
}

// ── events from the bridge ──────────────────────────────────────────────

function absorb(event) {
  switch (event.type) {
    case "ready":
      S.status = "ready";
      return;
    case "turn":
      if (event.status === "started") S.status = "thinking";
      else {
        S.status = "ready";
        if (event.status === "failed") note(event.error || "turn failed", "error");
        else if (event.status === "interrupted") note("stopped", "note");
      }
      return;
    case "delta": {
      const last = S.entries[S.entries.length - 1];
      if (last && last.kind === "agent" && last.open) last.text += event.text;
      else S.entries.push({ kind: "agent", text: event.text, open: true });
      S.scroll = 0;
      return;
    }
    case "message":
      closeAgent();
      note(event.text, "agent");
      return;
    case "tool": {
      closeAgent();
      const existing = S.entries.find((e) => e.kind === "tool" && e.id === event.id);
      if (existing) {
        existing.status = event.status;
      } else {
        S.entries.push({ kind: "tool", id: event.id, tool: event.kind, label: event.label, status: event.status });
      }
      if (event.kind === "file" && event.status === "completed" && event.path === S.file) {
        S.pieceDirty = true;
        S.pieceWrites += 1;
      }
      S.scroll = 0;
      return;
    }
    case "approval":
      S.pending = { id: event.id, kind: event.kind, label: event.label };
      return;
    case "approval-resolved":
      if (S.pending && S.pending.id === event.id) S.pending = null;
      return;
    case "warning":
      note(event.message, "soft");
      return;
    case "error":
      note(event.message, "error");
      if (event.fatal) S.status = "error";
      return;
    case "log":
      S.lastLog = event.line;
      return;
    default:
  }
}

function closeAgent() {
  const last = S.entries[S.entries.length - 1];
  if (last && last.kind === "agent") last.open = false;
}

// ── keyboard ────────────────────────────────────────────────────────────

function command(line, system, screen) {
  const [name, ...rest] = line.slice(1).split(/\s+/);
  const arg = rest.join(" ").trim();
  switch (name) {
    case "help":
      note("/new fresh conversation · /run · /piece <name> · /model <name> · /backend ac|claude · /quit");
      return;
    case "backend": {
      const target = arg === "aesthetic" || arg === "hosted" || arg === "free" ? "ac" : arg;
      if (target !== "ac" && target !== "claude") { note(`engine ${S.backend} · /backend ac|claude`); return; }
      if (target === S.backend) { note(`already on ${target}`); return; }
      S.backend = target;
      S.model = target === "ac" ? DEFAULT_AC_MODEL : DEFAULT_MODEL;
      note(`engine ${target === "ac" ? "aesthetic" : "claude"}`);
      spawnBridge(system, screen, "");
      return;
    }
    case "new":
      note("new conversation");
      spawnBridge(system, screen, "");
      return;
    case "run":
      runPiece(system);
      return;
    case "piece":
      if (!/^[a-z0-9-]+$/.test(arg)) { note("usage: /piece <name>", "error"); return; }
      S.slug = arg;
      S.file = `${PIECES}/${arg}.mjs`;
      S.pieceDirty = false;
      ensurePiece(system);
      note(`piece ${S.file}`);
      spawnBridge(system, screen, ""); // the instructions name the file
      return;
    case "model":
      if (!arg) { note(`model ${S.model}`); return; }
      S.model = S.backend === "ac" ? resolveAcModel(arg) : arg;
      note(`model ${arg}`);
      spawnBridge(system, screen, "");
      return;
    case "quit":
      if (S.bridge && S.bridge.busy) S.bridge.interrupt();
      if (system.pty2.active) system.pty2.kill();
      globalThis.__aesel = null;
      system.jump("prompt");
      return;
    default:
      note(`unknown command /${name}`, "error");
  }
}

function runPiece(system) {
  if (!system.readFile(S.file)) { note("nothing to run yet", "error"); return; }
  S.pieceDirty = false;
  system.jump(S.slug);
}

function act({ event: e, system, screen }) {
  if (e.is("keyboard:down:shift")) { shiftHeld = true; return; }
  if (e.is("keyboard:up:shift")) { shiftHeld = false; return; }
  if (e.is("keyboard:down:control")) { ctrlHeld = true; return; }
  if (e.is("keyboard:up:control")) { ctrlHeld = false; return; }
  if (!e.is("keyboard:down") || !S) return;
  const key = e.key;

  if (key === "escape") { system.jump("prompt"); return; }

  if (ctrlHeld && key === "c") {
    if (S.bridge && S.bridge.busy) { S.bridge.interrupt(); note("stopping…", "soft"); }
    else { S.input = ""; S.cursor = 0; }
    return;
  }

  // An approval owns the keyboard until it is answered.
  if (S.pending && S.input === "") {
    const decision = key === "y" ? "accept" : key === "a" ? "acceptForSession" : key === "n" ? "decline" : null;
    if (decision) {
      const pending = S.pending;
      S.pending = null;
      S.bridge.approve(pending.id, decision);
      note(`${decision === "decline" ? "denied" : "allowed"} ${pending.label}`, "soft");
      return;
    }
  }

  if (key === "tab") { runPiece(system); return; }
  if (key === "arrowup") { S.scroll += 1; return; }
  if (key === "arrowdown") { S.scroll = Math.max(0, S.scroll - 1); return; }
  if (key === "pageup") { S.scroll += 10; return; }
  if (key === "pagedown") { S.scroll = Math.max(0, S.scroll - 10); return; }

  if (key === "enter" || key === "return") {
    const text = S.input.trim();
    S.input = "";
    S.cursor = 0;
    if (!text) return;
    if (text.startsWith("/")) { command(text, system, screen); return; }
    if (!S.bridge || !S.bridge.ready) { note("the engine is still starting", "error"); return; }
    if (S.bridge.busy) { note("wait for the turn to finish, or ctrl+c", "error"); return; }
    closeAgent();
    note(text, "user");
    S.bridge.say(text);
    return;
  }

  if (key === "backspace") {
    if (S.cursor > 0) { S.input = S.input.slice(0, S.cursor - 1) + S.input.slice(S.cursor); S.cursor--; }
    return;
  }
  if (key === "arrowleft") { if (S.cursor > 0) S.cursor--; return; }
  if (key === "arrowright") { if (S.cursor < S.input.length) S.cursor++; return; }
  if (key === "home") { S.cursor = 0; return; }
  if (key === "end") { S.cursor = S.input.length; return; }

  let ch = null;
  if (key === "space") ch = " ";
  else if (key.length === 1) ch = shiftHeld ? (SHIFT_MAP[key] ?? key.toUpperCase()) : key;
  if (ch !== null) {
    S.input = S.input.slice(0, S.cursor) + ch + S.input.slice(S.cursor);
    S.cursor++;
  }
}

// ── drawing ─────────────────────────────────────────────────────────────

function wrap(text, cols) {
  const out = [];
  for (const para of String(text).split("\n")) {
    let line = "";
    for (const word of para.split(" ")) {
      if (word.length > cols) {
        if (line) { out.push(line); line = ""; }
        for (let i = 0; i < word.length; i += cols) out.push(word.slice(i, i + cols));
        continue;
      }
      const next = line ? line + " " + word : word;
      if (next.length > cols) { out.push(line); line = word; }
      else line = next;
    }
    out.push(line);
  }
  return out;
}

// Turn the transcript into coloured lines, newest last.
function layout(cols) {
  const lines = [];
  for (const entry of S.entries) {
    if (entry.kind === "user") {
      wrap(entry.text, cols - 2).forEach((l, i) => lines.push({ text: (i ? "  " : "> ") + l, rgb: PALETTE.text, mark: i ? null : PALETTE.prompt }));
    } else if (entry.kind === "agent") {
      wrap(entry.text, cols).forEach((l) => lines.push({ text: l, rgb: PALETTE.text }));
    } else if (entry.kind === "tool") {
      const verb = entry.tool === "file" ? "write" : entry.tool === "command" ? "$" : "·";
      const tail = entry.status === "started" ? " …" : entry.status === "failed" ? " ✗" : "";
      const rgb = entry.status === "failed" ? PALETTE.error : PALETTE.muted;
      wrap(`${verb} ${entry.label}${tail}`, cols).forEach((l) => lines.push({ text: l, rgb }));
    } else if (entry.kind === "error") {
      wrap(entry.text, cols).forEach((l) => lines.push({ text: l, rgb: PALETTE.error }));
    } else if (entry.kind === "soft") {
      wrap(entry.text, cols).forEach((l) => lines.push({ text: l, rgb: PALETTE.soft }));
    } else {
      wrap(entry.text, cols).forEach((l) => lines.push({ text: l, rgb: PALETTE.muted }));
    }
  }
  return lines;
}

function paint({ wipe, ink, box, write, screen, system, wifi, paintCount }) {
  if (!S) return;
  frame++;
  SYS = system;
  const W = screen.width;
  const H = screen.height;

  // Pump the bridge. Claude's lines are handed out once per paint, so every
  // paint reads them; the hosted engine polls its stream file on tick().
  const pty = system.pty2;
  if (S.bridge) {
    if (S.backend === "ac") {
      S.bridge.tick();
    } else if (pty) {
      if (pty.lines && pty.lines.length) for (const line of pty.lines) S.bridge.feed(line);
      if (pty.overflow) note("bridge output overflowed; a message was lost", "error");
      if (!pty.active && S.status !== "error" && S.status !== "closed") {
        S.status = "closed";
        S.exitCode = pty.exitCode;
        note(`engine bridge closed (${pty.exitCode}) · /new to restart`, "error");
      }
    }
    for (const event of S.bridge.take()) absorb(event);
  }

  wipe(...PALETTE.background);

  // Header: who, which piece, which engine, and whether it is busy.
  const top = 2;
  let x = PAD;
  if (S.handle) {
    ink(...PALETTE.handle);
    write("@" + S.handle, { x, y: top, size: 1, font: FONT });
    x += (S.handle.length + 2) * CW;
  }
  ink(...PALETTE.highlight);
  write(S.slug, { x, y: top, size: 1, font: FONT });
  if (S.pieceDirty) {
    ink(...PALETTE.soft);
    write("· tab runs it", { x: x + (S.slug.length + 1) * CW, y: top, size: 1, font: FONT });
  }

  const offline = wifi && !wifi.connected;
  const statusWord = offline && S.status === "ready" ? "offline" : S.status;
  const dot = S.status === "thinking" ? (paintCount % 40 < 20 ? PALETTE.highlight : PALETTE.soft)
    : S.status === "ready" ? (offline ? PALETTE.highlight : PALETTE.status)
    : S.status === "starting" ? PALETTE.soft
    : PALETTE.error;
  const modelShort = S.backend === "ac" ? `aesthetic ${acModelLabel(S.model)}` : S.model.replace(/^claude-/, "");
  const right = `${modelShort} ${statusWord}`;
  const rx = W - PAD - right.length * CW;
  ink(...PALETTE.muted);
  write(modelShort, { x: rx, y: top, size: 1, font: FONT });
  ink(...dot);
  write(statusWord, { x: rx + (modelShort.length + 1) * CW, y: top, size: 1, font: FONT });

  // Prompt block at the bottom; an approval takes it over.
  const promptH = CH + 4;
  const promptY = H - promptH;
  const cols = Math.max(10, Math.floor((W - PAD * 2) / CW));

  if (S.pending) {
    ink(...PALETTE.highlight);
    box(0, promptY, W, promptH);
    ink(...PALETTE.text);
    const verb = S.pending.kind === "file" ? "write" : "run";
    const keys = "  y once · a always · n no";
    const room = cols - verb.length - 1 - keys.length;
    const label = S.pending.label.length > room ? S.pending.label.slice(0, Math.max(0, room - 1)) + "…" : S.pending.label;
    write(`${verb} ${label}${keys}`, { x: PAD, y: promptY + 2, size: 1, font: FONT });
  } else {
    ink(...PALETTE.prompt);
    box(0, promptY, W, promptH);
    ink(...PALETTE.text);
    // Keep the cursor in view when the input outgrows the row.
    const visible = cols - 1;
    let start = 0;
    if (S.cursor > visible) start = S.cursor - visible;
    const shown = S.input.slice(start, start + visible);
    write(shown, { x: PAD, y: promptY + 2, size: 1, font: FONT });
    if (paintCount % 60 < 30) {
      const cx = PAD + (S.cursor - start) * CW;
      ink(...PALETTE.text);
      box(cx, promptY + 2, CW, CH);
      const under = S.input[S.cursor];
      if (under) { ink(...PALETTE.prompt); write(under, { x: cx, y: promptY + 2, size: 1, font: FONT }); }
    }
  }

  // Transcript between header and prompt, bottom-aligned, scrolled back by S.scroll lines.
  const bodyTop = top + CH + 4;
  const bodyBottom = promptY - 2;
  const rows = Math.max(1, Math.floor((bodyBottom - bodyTop) / CH));
  const lines = layout(cols);
  const maxScroll = Math.max(0, lines.length - rows);
  if (S.scroll > maxScroll) S.scroll = maxScroll;
  const end = lines.length - S.scroll;
  const begin = Math.max(0, end - rows);
  let y = bodyBottom - (end - begin) * CH;
  for (let i = begin; i < end; i++) {
    const l = lines[i];
    if (l.mark) {
      ink(...l.mark);
      write(">", { x: PAD, y, size: 1, font: FONT });
      ink(...l.rgb);
      write(l.text.slice(2), { x: PAD + 2 * CW, y, size: 1, font: FONT });
    } else {
      ink(...l.rgb);
      write(l.text, { x: PAD, y, size: 1, font: FONT });
    }
    y += CH;
  }
  if (S.scroll > 0) {
    ink(...PALETTE.muted);
    const tag = `↓ ${S.scroll}`;
    write(tag, { x: W - PAD - tag.length * CW, y: bodyBottom - CH, size: 1, font: FONT });
  }
}

function leave() {
  // The bridge child stays alive on purpose: leaving for the prompt or to run
  // the piece is not the end of the conversation. /quit ends it.
}

export { boot, act, paint, leave };
