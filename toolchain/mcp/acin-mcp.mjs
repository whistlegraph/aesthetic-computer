#!/usr/bin/env node
// acin-mcp.mjs — "claude in" to aesthetic.computer.
//
// One front door for agent presence: attach this session as an agent to a
// handle's presence room (session-server /agent-presence) so every AC surface
// signed in as that handle lights its linked-agent mark, and optionally open
// a Chrome window already pointed at aesthetic.computer — clauded in from the
// first frame. The socket lives as long as this MCP process does, so presence
// holds for the whole Claude session and drops cleanly when it ends.

import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { createRequire } from "node:module";
import { homedir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";
import { loadTokens } from "./ac-token.mjs";
import { pairScreen } from "./acin-pair.mjs";
import { appDoor } from "./acin-app.mjs";
import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";

const pexec = promisify(execFile);
// Resolve puppeteer against the repo root (this file lives at toolchain/mcp/),
// not the process cwd, so the daemon finds it wherever it was launched from.
const requireRepo = createRequire(
  join(fileURLToPath(import.meta.url), "../../../package.json"),
);
const RELAY =
  process.env.AC_PRESENCE_RELAY || "wss://session-server.aesthetic.computer";
const DEFAULT_HANDLE = process.env.AC_HANDLE || "jeffrey";
const DEFAULT_URL = "https://aesthetic.computer/prompt";
const CHROME_BIN =
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const PROFILE_DIR = join(homedir(), ".acin-chrome");

// One attachment at a time — acin_in to a new room supersedes the old one.
const state = {
  socket: null,
  room: null,
  label: null,
  lastStatus: null,
  connectedAt: null,
  wantRoom: null, // survives socket drops so we can quietly re-attach
  wantLabel: null,
  reconnectTimer: null,
  browser: null, // puppeteer handle to the dedicated signed-in window
  page: null,
};

function canonicalRoom(value) {
  const name = String(value || "").toLowerCase().replace(/^@/, "");
  return /^[a-z0-9_-]{1,32}$/.test(name) ? name : null;
}

function detach() {
  if (state.socket) {
    try { state.socket.close(); } catch { /* already gone */ }
  }
  state.socket = null;
  state.room = null;
  state.label = null;
  state.lastStatus = null;
  state.connectedAt = null;
  state.wantRoom = null;
  state.wantLabel = null;
  clearTimeout(state.reconnectTimer);
  state.reconnectTimer = null;
}

// A dropped socket (relay restart, network blip) re-attaches quietly every 5s
// for as long as the attachment is still wanted — acin_out is the only way out.
function scheduleReconnect() {
  if (state.reconnectTimer || !state.wantRoom) return;
  state.reconnectTimer = setTimeout(async () => {
    state.reconnectTimer = null;
    if (!state.wantRoom || state.socket) return;
    try {
      await attach(state.wantRoom, state.wantLabel);
    } catch {
      scheduleReconnect();
    }
  }, 5000);
}

// Attach and resolve on the first status frame, so the caller learns how many
// surfaces saw the mark light before the tool returns.
function attach(room, label) {
  return new Promise((resolvePromise, reject) => {
    const url = `${RELAY}/agent-presence?room=${room}&role=agent&label=${
      encodeURIComponent(label)}`;
    const socket = new WebSocket(url);
    const timeout = setTimeout(() => {
      try { socket.close(); } catch { /* unreachable relay */ }
      reject(new Error("Presence relay did not answer within 10s"));
    }, 10000);

    socket.onmessage = (event) => {
      let message;
      try { message = JSON.parse(event.data); } catch { return; }
      if (message.type === "agent-presence:error") {
        clearTimeout(timeout);
        reject(new Error(message.content?.message || "Relay refused"));
        return;
      }
      if (message.type !== "agent-presence:status") return;
      state.lastStatus = message.content;
      if (state.socket !== socket) {
        // First frame: adopt this socket as the live attachment.
        if (state.socket) {
          try { state.socket.close(); } catch { /* superseded */ }
        }
        state.socket = socket;
        state.room = room;
        state.label = label;
        state.wantRoom = room;
        state.wantLabel = label;
        state.connectedAt = new Date().toISOString();
        clearTimeout(timeout);
        resolvePromise(message.content);
      }
    };
    socket.onclose = () => {
      if (state.socket === socket) {
        state.socket = null;
        scheduleReconnect();
      }
    };
    socket.onerror = () => {
      clearTimeout(timeout);
      try { socket.close(); } catch { /* already closing */ }
      reject(new Error("Could not reach the presence relay"));
    };
  });
}

// The dedicated window: a separate Chrome instance on its own profile
// (~/.acin-chrome) so jeffrey's daily browser is never driven. The shared
// ac-login session (~/.ac-token) is seeded through boot.mjs's first-party
// `session-aesthetic` pickup before any page script runs; `state=acin` in the
// URL defeats the fast-boot "no auth cache → skip auth" gate (boot cleans it
// off after pickup). The profile keeps the session, so even plain opens of
// this profile stay signed in.
async function openSignedInChrome(url) {
  const tokens = await loadTokens();
  const session = {
    accessToken: tokens.access_token,
    account: { label: tokens.user.email, id: tokens.user.sub },
  };
  const encoded = encodeURIComponent(
    Buffer.from(JSON.stringify(session)).toString("base64"),
  );
  const target = new URL(url);
  target.searchParams.set("state", "acin");

  const alive = state.browser &&
    (state.browser.connected ?? state.browser.isConnected?.());
  if (!alive) {
    const puppeteer = requireRepo("puppeteer");
    try {
      state.browser = await puppeteer.launch({
        executablePath: CHROME_BIN,
        headless: false,
        userDataDir: PROFILE_DIR,
        defaultViewport: null,
        args: [
          "--no-first-run",
          "--no-default-browser-check",
          "--window-size=800,900",
        ],
      });
    } catch {
      // Profile already held by a running acin window (SingletonLock) — hand
      // the URL to that instance; its persisted session keeps it signed in.
      await pexec(CHROME_BIN, [`--user-data-dir=${PROFILE_DIR}`, url]);
      state.browser = null;
      state.page = null;
      return { url, handedOff: true };
    }
    state.page =
      (await state.browser.pages())[0] || (await state.browser.newPage());
  }

  await state.page.evaluateOnNewDocument((enc) => {
    try { localStorage.setItem("session-aesthetic", enc); } catch { /* blocked */ }
  }, encoded);
  await state.page.goto(target.href, {
    waitUntil: "networkidle2",
    timeout: 45000,
  });
  const check = await state.page.evaluate(() => ({
    signedIn: !!window.acUSER,
    crab:
      document.querySelector("[data-ac-agent-mark]")?.style.display || "absent",
  }));
  return { url: target.href, ...check };
}

function text(value) {
  return [{ type: "text", text: typeof value === "string"
    ? value : JSON.stringify(value, null, 2) }];
}

async function toolIn(args) {
  const room = canonicalRoom(args.handle || DEFAULT_HANDLE);
  if (!room) throw new Error("Invalid handle");
  const label = /^[a-z0-9 _-]{1,24}$/i.test(args.label || "")
    ? args.label : "claude";
  const status = await attach(room, label);
  let opened = null;
  if (args.open !== false) {
    opened = await openSignedInChrome(args.url || DEFAULT_URL);
  }
  return text({
    linked: true,
    room: `@${room}`,
    label,
    surfaces: status.surfaces,
    agents: status.agents,
    opened,
    note: opened
      ? "Dedicated Chrome window opened signed in via ~/.ac-token; the claude crab lights top-right once the surface joins the room."
      : "Attached without opening a window.",
  });
}

function toolStatus() {
  if (!state.socket) {
    return text(
      state.wantRoom
        ? { linked: false, reconnecting: true, room: `@${state.wantRoom}` }
        : { linked: false },
    );
  }
  return text({
    linked: true,
    room: `@${state.room}`,
    label: state.label,
    connectedAt: state.connectedAt,
    lastStatus: state.lastStatus,
    window: state.browser &&
      (state.browser.connected ?? state.browser.isConnected?.())
      ? "open"
      : null,
  });
}

function toolOut() {
  if (!state.socket) return text({ linked: false, note: "Nothing to detach." });
  const room = state.room;
  detach();
  return text({ linked: false, note: `Detached from @${room}; the mark goes dark on every surface.` });
}

// A rax screen with nobody signed in is seated as the shared session's handle
// (acin-pair.mjs), and this session is linked to that handle's room after.
async function toolPair(args) {
  const said = await pairScreen({ image: args.image, url: args.url, code: args.code });
  if (!said.ok) throw new Error(said.why);
  let linked = null;
  const room = canonicalRoom(said.handle);
  if (args.link !== false && room && state.room !== room) {
    try { linked = await attach(room, "claude"); } catch { /* the screen is signed in all the same */ }
  }
  return text({
    paired: true,
    handle: `@${said.handle}`,
    server: said.server,
    linked: !!state.socket,
    surfaces: (linked || state.lastStatus)?.surfaces,
    note: "That one screen is seated as the handle until it reloads or its connection drops; then capture its QR and pair again.",
  });
}

// The native Mac app's own door (acin-app.mjs): where it is, a line of
// JavaScript in its page, a picture of it.
async function toolApp(args) {
  const said = await appDoor({ machine: args.machine, action: args.action || "info", js: args.js });
  if (said.png) {
    const file = join(await mkdtemp(join(tmpdir(), "acin-app-")), "app.png");
    await writeFile(file, said.png);
    return text({ ok: true, image: file });
  }
  if (!said.ok) throw new Error(said.error || "The app did not answer");
  return text(said);
}

const TOOLS = [
  {
    name: "acin_in",
    description:
      "Claude in: attach this session as an agent to an aesthetic.computer " +
      "handle's presence room (lights the claude crab top-right on every AC " +
      "surface signed in as that handle) and open a dedicated Chrome window " +
      "already signed in via the shared ~/.ac-token session. Presence holds " +
      "until acin_out or the session ends, surviving relay restarts.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "Handle whose room to join (default @jeffrey)" },
        label: { type: "string", description: "Agent label shown in the mark tooltip (default claude)" },
        open: { type: "boolean", description: "Open a Chrome window (default true)" },
        url: { type: "string", description: "URL to open (default https://aesthetic.computer/prompt)" },
      },
    },
  },
  {
    name: "acin_pair",
    description:
      "Claude in to a rax screen that is signed out (the desktop app driven " +
      "by a controller, a TV, a kiosk): reads the pairing QR it shows bottom " +
      "left out of a picture of that screen and signs that one screen in as " +
      "the shared ~/.ac-token handle, as a phone scanning it would, then " +
      "links this session to the handle's room. Capture the screen with " +
      "frame_focus on the QR and pass the image path it returns. A code " +
      "lives two minutes and works once, so pair straight after capturing.",
    inputSchema: {
      type: "object",
      properties: {
        image: { type: "string", description: "Path to a picture of the screen showing the QR (a Frame capture)" },
        url: { type: "string", description: "The pairing address, if already read (…/rax/pair#code)" },
        code: { type: "string", description: "The bare pairing code" },
        link: { type: "boolean", description: "Also attach agent presence to the handle's room (default true)" },
      },
    },
  },
  {
    name: "acin_app",
    description:
      "Claude in to the native Aesthetic Computer Mac app (a WKWebView, so " +
      "it has no debugging port): `info` says where it is, `eval` runs " +
      "JavaScript in its page (the inside of an async function: `return` " +
      "what is wanted), `shot` saves a picture of it and returns the path. " +
      "`machine` is the ssh name of the Mac it runs on (blueberry); left " +
      "out, this Mac. The app signs itself in from ~/.ac-token at launch.",
    inputSchema: {
      type: "object",
      properties: {
        action: { type: "string", enum: ["info", "eval", "shot"], description: "default info" },
        js: { type: "string", description: "For eval: the body of an async function" },
        machine: { type: "string", description: "ssh name of the Mac the app runs on (default: this one)" },
      },
    },
  },
  {
    name: "acin_status",
    description: "Report the current claude-in attachment: room, label, and the latest presence counts (surfaces watching, agents linked).",
    inputSchema: { type: "object", properties: {} },
  },
  {
    name: "acin_out",
    description: "Claude out: detach from the presence room so the linked-agent mark goes dark. Does not close any Chrome window.",
    inputSchema: { type: "object", properties: {} },
  },
];

async function callTool(name, args) {
  switch (name) {
    case "acin_in": return toolIn(args || {});
    case "acin_pair": return toolPair(args || {});
    case "acin_app": return toolApp(args || {});
    case "acin_status": return toolStatus();
    case "acin_out": return toolOut();
    default: throw new Error(`Unknown tool: ${name}`);
  }
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "acin-mcp", version: "1.0.0" },
            instructions:
              "Claude in / claude out of aesthetic.computer. acin_in attaches " +
              "agent presence to a handle's room and opens a dedicated Chrome " +
              "window signed in via ~/.ac-token; the claude crab shows " +
              "top-right while attached. Presence drops when this session ends. " +
              "A signed-out rax screen is signed in with acin_pair from a Frame " +
              "capture of its QR.",
          },
        };
      case "initialized":
      case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default: return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 0);
if (port) serveHttp({ handleMessage, port, banner: "🛰️ acin-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "🛰️ acin-mcp started (acin_in, acin_pair, acin_status, acin_out)" });
