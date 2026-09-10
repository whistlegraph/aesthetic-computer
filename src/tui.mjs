#!/usr/bin/env node

import { spawn } from "node:child_process";
import path from "node:path";
import process from "node:process";
import { ACSession } from "./ac-session.mjs";
import { backendFor, backendMenu, DEFAULT_BACKEND } from "./backends.mjs";
import { LivePiece } from "./live.mjs";
import { publishPiece } from "./publish.mjs";
import { qrBlock } from "./qr.mjs";
import { cleanText, renderBoot, renderFrame } from "./render.mjs";
import { mascotNextFrameIn } from "./mascot.mjs";
import { DEFAULT_RUNTIME, runtimeMenu } from "./runtimes.mjs";
import { SlabSession } from "./slab-session.mjs";

const arguments_ = process.argv.slice(2);
const option = (name) => {
  const index = arguments_.indexOf(name);
  return index >= 0 ? arguments_[index + 1] || "" : "";
};
const cwd = path.resolve(option("--cwd") || process.cwd());
const resumeThreadId = option("--resume");
const initialPrompt = option("--prompt");
// Which engine bridge drives the conversation, and on which model. The bridge
// can be swapped mid-session with /backend, so neither is a constant.
let backend = backendFor(option("--backend") || process.env.AESTHETIC_CODE_BACKEND || DEFAULT_BACKEND);
let model = option("--model") || backend.defaultModel;

const session = new ACSession();
// Every session opens on a new blank piece with a random name. It is a real
// file in the workspace, and every edit is pushed to whatever scanned the QR.
const live = new LivePiece({ cwd, runtime: option("--runtime") || DEFAULT_RUNTIME });
const state = {
  workspace: cwd,
  mode: "remote",
  status: "starting",
  busy: false,
  input: "",
  cursor: 0,
  history: [],
  historyIndex: 0,
  approval: null,
  account: session.label(),
  piece: "",
  // What the bridge said it is running, once it has said so.
  model: "",
  qr: null,
  showQr: true,
  entries: [
    {
      id: "privacy",
      kind: "notice",
      text: "REMOTE INFERENCE · prompt content may leave this machine",
    },
  ],
};

// The model must never mistake a file on disk for a published piece.
function developerInstructions() {
  const account = session.handle
    ? `The user is signed in to Aesthetic Computer as @${session.handle}.`
    : "The user is not signed in to Aesthetic Computer; /login signs them in.";
  // A live push carries no file extension, so a Processing piece is recognised
  // by its own opening lines. Rewriting them silently takes the phone dark.
  const dialect =
    live.runtime.id === "lua"
      ? [
          "This is a Processing (L5) piece: write Processing, not Aesthetic Computer JavaScript — `setup` and `draw`, `background`, `fill`, `circle`, `text`, `width`, `height`, `frameCount`, `mouseX`, `mouseY`, `mouseIsPressed`. There is no `paint`, `wipe`, or `ink`.",
          "Keep the file's first line a `--` comment and keep a top-level `function setup(` or `function draw(`. The live channel sends no file extension, so those two things are the only way the piece is recognised as Lua rather than compiled as JavaScript — drop either and the phone goes blank.",
        ]
      : [];
  return [
    "You are running inside Aesthetic Code, a terminal interface for Aesthetic Computer (AC) work.",
    account,
    `This session's piece is ${live.file} (${live.runtime.label}). It already exists as a blank piece. Edit that file unless the user asks for something else.`,
    ...dialect,
    "Every save of that file is pushed live to a phone that scanned the interface's QR code, so small frequent edits are better than one big rewrite.",
    "Publishing: writing a file under system/public/aesthetic.computer/disks/ or anywhere else does NOT make a piece live.",
    "A piece is live only after the user runs the Aesthetic Code command `/publish <file> [slug]`, which uploads it under their @handle at https://aesthetic.computer/@handle/slug.",
    "When you finish a piece, end with the exact /publish command for the user to run. Never tell the user to visit a route that has not been published.",
    "Dev servers: do not stop a dev server you were asked to start; say that it is still running.",
  ].join("\n");
}

const slabSession = new SlabSession({ cwd });
slabSession.start();
slabSession.identity(session.handle);

// One engine at a time, wired to the same handlers however it was built.
function openEngine({ resume = "" } = {}) {
  const opened = new backend.Engine({
    cwd,
    resumeThreadId: resume,
    model,
    developerInstructions: developerInstructions(),
    environment: {
      SLAB_PROMPT_SESSION_ID: slabSession.sessionId,
      SLAB_TERMINAL_TTY: slabSession.tty,
      SLAB_AGENT_TYPE: "aesthetic-code",
    },
  });
  opened.on("notification", handleNotification);
  opened.on("request", handleRequest);
  opened.on("protocolError", (error) => {
    addEntry("error", errorText(error));
    redraw();
  });
  opened.on("fatal", (error) => {
    if (closing || opened !== engine) return;
    state.status = "offline";
    addEntry("error", errorText(error));
    slabSession.awaitingInput("aesthetic code engine bridge is offline");
    redraw();
  });
  return opened;
}

let engine = openEngine({ resume: resumeThreadId });
let drawing = false;
let closing = false;
let streamedMessageId = null;
let pasteBuffer = null;

function addEntry(kind, text, id = `entry-${Date.now()}-${Math.random()}`) {
  state.entries.push({ id, kind, text: cleanText(text) });
  if (state.entries.length > 300) state.entries.splice(0, state.entries.length - 300);
  return id;
}

function updateEntry(id, kind, text) {
  const entry = state.entries.find((candidate) => candidate.id === id);
  if (entry) {
    entry.kind = kind;
    entry.text = cleanText(text);
  } else {
    addEntry(kind, text, id);
  }
}

// The guard keeps a redraw from re-entering itself; `finally` is what keeps a
// single bad frame from latching it shut and freezing the screen for good.
function redraw() {
  if (closing || drawing) return;
  drawing = true;
  try {
    const frame = renderFrame(state, process.stdout.columns, process.stdout.rows, process.env.NO_COLOR !== "1");
    process.stdout.write(`\x1b[H\x1b[2J${frame}`);
  } finally {
    drawing = false;
  }
}

function finish(code = 0) {
  if (closing) return;
  closing = true;
  session.unwatch();
  live.cleanup();
  slabSession.close();
  engine.close();
  process.stdin.setRawMode(false);
  process.stdin.pause();
  process.stdout.write("\x1b[?2004l\x1b[?25h\x1b[?1049l");
  process.exitCode = code;
}

function errorText(error) {
  return cleanText(error?.message || error || "unknown error");
}

// Track the piece under work from the files the agent touches. The QR code
// addresses the channel rather than the file, so it stays valid across a
// retarget; only the name in the header changes.
function notePiece(file) {
  if (!file) return;
  if (live.retarget(file)) live.watch(liveError);
  state.piece = `${live.slug}${live.runtime.extension}`;
}

function liveError(error) {
  addEntry("error", `Live push failed: ${errorText(error)}`);
  redraw();
}

function refreshQr() {
  state.qr = state.showQr ? qrBlock(live.scanUrl) : null;
}

function itemSummary(item) {
  if (!item) return null;
  if (item.type === "commandExecution") return { kind: "command", text: item.command };
  if (item.type === "fileChange") {
    const paths = (item.changes || []).map((change) => change.path).filter(Boolean);
    for (const file of paths) notePiece(file);
    return { kind: "change", text: paths.join(", ") || "workspace files" };
  }
  if (item.type === "mcpToolCall") return { kind: "command", text: `${item.server} · ${item.tool}` };
  if (item.type === "dynamicToolCall") return { kind: "command", text: item.tool };
  return null;
}

function restoreThread(thread) {
  const restored = [];
  for (const turn of thread?.turns || []) {
    for (const item of turn.items || []) {
      if (item.type === "userMessage") {
        const text = (item.content || [])
          .filter((content) => content.type === "text")
          .map((content) => content.text)
          .join("\n");
        if (text) restored.push({ id: item.id, kind: "user", text: cleanText(text) });
      } else if (item.type === "agentMessage" && item.text) {
        restored.push({ id: item.id, kind: "assistant", text: cleanText(item.text) });
      } else if (item.type === "fileChange") {
        for (const change of item.changes || []) notePiece(change.path);
      }
    }
  }
  state.entries.push(...restored.slice(-80));
  return restored.length;
}

function handleNotification({ method, params = {} }) {
  switch (method) {
    case "turn/started":
      state.busy = true;
      state.status = "working";
      engine.turnId = params.turn?.id || engine.turnId;
      slabSession.working();
      break;
    case "item/agentMessage/delta":
      if (!streamedMessageId || streamedMessageId !== params.itemId) {
        streamedMessageId = params.itemId;
        addEntry("assistant", "", params.itemId);
      }
      {
        const entry = state.entries.find((candidate) => candidate.id === params.itemId);
        if (entry) entry.text += cleanText(params.delta);
      }
      break;
    case "item/started": {
      const summary = itemSummary(params.item);
      if (summary) updateEntry(params.item.id, summary.kind, summary.text);
      break;
    }
    case "item/completed": {
      const item = params.item;
      if (item?.type === "agentMessage") updateEntry(item.id, "assistant", item.text);
      const summary = itemSummary(item);
      if (summary) {
        let suffix = "";
        if (item.type === "commandExecution") {
          suffix = item.exitCode === null || item.exitCode === 0 ? " · done" : ` · exit ${item.exitCode}`;
        } else if (item.status) {
          suffix = ` · ${item.status}`;
        }
        updateEntry(item.id, summary.kind, `${summary.text}${suffix}`);
      }
      break;
    }
    case "item/commandExecution/outputDelta": {
      const entry = state.entries.find((candidate) => candidate.id === params.itemId);
      if (entry && params.delta) {
        const lastLine = cleanText(params.delta).trim().split("\n").at(-1);
        if (lastLine) entry.text = `${entry.text.split("\n")[0]}\n${lastLine}`;
      }
      break;
    }
    case "turn/completed": {
      state.busy = false;
      state.status = params.turn?.status === "failed" ? "failed" : "ready";
      engine.turnId = null;
      streamedMessageId = null;
      const failure = params.turn?.error;
      if (failure) addEntry("error", failure.message || JSON.stringify(failure));
      if (params.turn?.status === "interrupted") slabSession.interrupted();
      else if (params.turn?.status === "failed") slabSession.awaitingInput("aesthetic code turn failed");
      else slabSession.complete();
      break;
    }
    case "warning":
      addEntry("notice", params.message || "Engine warning");
      break;
    case "error":
      addEntry("error", params.error?.message || "Engine error");
      if (!params.willRetry) state.status = "failed";
      break;
  }
  redraw();
}

function approvalSubject(method, params) {
  if (method === "item/commandExecution/requestApproval") {
    return params.command || params.reason || "command";
  }
  if (method === "item/fileChange/requestApproval") {
    return params.reason || params.grantRoot || "file change";
  }
  return method;
}

function handleRequest(request) {
  if (
    request.method === "item/commandExecution/requestApproval" ||
    request.method === "item/fileChange/requestApproval"
  ) {
    state.approval = {
      id: request.id,
      method: request.method,
      subject: approvalSubject(request.method, request.params || {}),
    };
    slabSession.awaitingInput(
      request.method === "item/commandExecution/requestApproval"
        ? "aesthetic code needs command approval"
        : "aesthetic code needs file approval",
    );
    state.status = "approval";
    redraw();
    return;
  }
  engine.reject(request.id, -32601, `Aesthetic Code does not support ${request.method} yet`);
}

function answerApproval(character) {
  const approval = state.approval;
  if (!approval) return false;
  const key = character.toLowerCase();
  const decision =
    key === "y"
      ? "accept"
      : key === "a"
        ? "acceptForSession"
        : key === "n"
          ? "decline"
          : character === "\u0003"
            ? "cancel"
            : null;
  if (!decision) return true;
  engine.respond(approval.id, { decision });
  const result = decision === "decline" ? "Denied" : decision === "cancel" ? "Cancelled" : "Allowed";
  addEntry("notice", `${result}: ${approval.subject}`);
  state.approval = null;
  if (decision === "cancel") slabSession.interrupted();
  else slabSession.resumeWork();
  state.status = state.busy ? "working" : "ready";
  redraw();
  return true;
}

// ── account + publish commands ──────────────────────────────────────────

function refreshAccount(announce = false) {
  const previous = state.account;
  state.account = session.label();
  slabSession.identity(session.handle);
  if (announce && previous !== state.account) {
    addEntry("notice", session.signedIn ? `Signed in as ${state.account}` : "Signed out");
  }
}

async function commandLogin() {
  if (session.signingIn) {
    addEntry("notice", "A sign-in is already waiting on the browser.");
    return redraw();
  }
  const id = addEntry("notice", "Opening the browser to sign in…");
  redraw();
  try {
    const handle = await session.login({
      onUrl: (url) => {
        updateEntry(id, "notice", `Sign in at ${url}`);
        redraw();
      },
    });
    refreshAccount();
    updateEntry(
      id,
      "notice",
      handle ? `Signed in as @${handle}` : "Signed in · claim a handle at aesthetic.computer/handle",
    );
  } catch (error) {
    updateEntry(id, "error", `Sign-in failed: ${errorText(error)}`);
  }
  redraw();
}

function commandLogout() {
  const removed = session.logout();
  refreshAccount();
  addEntry("notice", removed ? "Signed out" : "Already signed out");
  redraw();
}

async function commandPublish(argumentText) {
  const [file = live.file, slug = ""] = argumentText.split(/\s+/).filter(Boolean);
  if (!file) {
    addEntry("error", "Usage: /publish <file> [slug] — no piece has been touched yet.");
    return redraw();
  }
  const id = addEntry("publish", `Publishing ${path.basename(file)}…`);
  redraw();
  try {
    const result = await publishPiece({
      file,
      slug,
      session,
      cwd,
      onStep: (step) => {
        updateEntry(id, "publish", `Publishing ${path.basename(file)} · ${step}…`);
        redraw();
      },
    });
    notePiece(result.path);
    updateEntry(id, "publish", `${result.route}${result.verified ? "" : " · uploaded, not yet readable"}`);
  } catch (error) {
    updateEntry(id, "error", `Publish failed: ${errorText(error)}`);
  }
  redraw();
}

// ── engine commands ─────────────────────────────────────────────────────

function engineLabel() {
  return `${backend.label} · ${state.model || model || backend.modelSource}`;
}

// Open a thread on the current bridge, replacing whatever is running. This is
// what /new, /backend and /model all come down to: the conversation restarts,
// the piece and the QR code do not.
async function restartEngine(note) {
  state.status = "starting";
  redraw();
  const previous = engine;
  engine = openEngine();
  previous.close();
  try {
    const connection = await engine.connect();
    slabSession.connected(engine.threadId);
    state.model = connection?.model || model;
    state.entries = [{ kind: "notice", text: `${note} · ${engineLabel()}`, id: `thread-${Date.now()}` }];
    state.status = "ready";
  } catch (error) {
    addEntry("error", errorText(error));
    state.status = "failed";
  }
  redraw();
}

async function commandBackend(rest) {
  if (!rest) {
    addEntry("notice", `${engineLabel()} · backends: ${backendMenu()}`);
    return redraw();
  }
  if (state.busy) {
    addEntry("error", "Interrupt the current turn before switching engines.");
    return redraw();
  }
  const [wanted, wantedModel = ""] = rest.split(/\s+/).filter(Boolean);
  let next;
  try {
    next = backendFor(wanted);
  } catch (error) {
    addEntry("error", errorText(error));
    return redraw();
  }
  backend = next;
  model = wantedModel || next.defaultModel;
  state.model = "";
  return restartEngine("Engine");
}

async function commandModel(rest) {
  if (!rest) {
    addEntry("notice", engineLabel());
    return redraw();
  }
  if (state.busy) {
    addEntry("error", "Interrupt the current turn before switching models.");
    return redraw();
  }
  model = rest.split(/\s+/)[0];
  state.model = "";
  return restartEngine("Model");
}

async function submitInput() {
  const text = state.input.trim();
  state.input = "";
  state.cursor = 0;
  state.historyIndex = state.history.length;
  if (!text) return redraw();

  if (text.startsWith("/")) {
    const [command, ...restWords] = text.split(/\s+/);
    const rest = restWords.join(" ");
    if (command === "/quit" || command === "/exit") return finish();
    if (command === "/clear") {
      state.entries = [];
      return redraw();
    }
    if (command === "/help") {
      addEntry(
        "notice",
        "/login · /logout · /whoami · /publish [file] · /piece [name] · /runtime [id] · /backend [id] · /model [name] · /open · /qr · /live · /new · /clear · /quit   ctrl-c interrupts a running turn",
      );
      return redraw();
    }
    if (command === "/login") return commandLogin();
    if (command === "/logout") return commandLogout();
    if (command === "/whoami") {
      refreshAccount();
      addEntry("notice", state.account);
      return redraw();
    }
    if (command === "/publish") return commandPublish(rest);
    if (command === "/backend" || command === "/engine") return commandBackend(rest);
    if (command === "/model") return commandModel(rest);
    if (command === "/piece") {
      if (rest) {
        try {
          live.rename(rest.split(/\s+/)[0]);
          live.watch(liveError);
          state.piece = `${live.slug}${live.runtime.extension}`;
          addEntry("notice", `Working on ${live.file}`);
        } catch (error) {
          addEntry("error", errorText(error));
        }
      } else {
        addEntry("notice", `Working on ${live.file}`);
      }
      return redraw();
    }
    if (command === "/runtime") {
      if (!rest) {
        addEntry("notice", `${live.runtime.label} · runtimes: ${runtimeMenu()}`);
        return redraw();
      }
      try {
        live.rename(live.slug, rest.split(/\s+/)[0]);
        live.watch(liveError);
        state.piece = `${live.slug}${live.runtime.extension}`;
        addEntry("notice", `${live.runtime.label} · ${live.file}`);
        if (!live.runtime.routable) {
          addEntry("notice", `${live.runtime.label} runs live but has no @handle route yet`);
        }
      } catch (error) {
        addEntry("error", errorText(error));
      }
      return redraw();
    }
    if (command === "/open") {
      // The code is for a phone. This is for the machine the session is already
      // running on: same URL, same channel, same autorun — the piece opens in a
      // browser here and updates on every save exactly as the phone does.
      const url = `https://${live.scanUrl}`;
      const opener = process.platform === "darwin"
        ? "open"
        : process.platform === "win32"
          ? "explorer"
          : "xdg-open";
      try {
        // Detached and fully redirected: a browser launcher that inherits this
        // terminal can print into the frame, and anything printed into the
        // frame scrolls it.
        const child = spawn(opener, [url], { stdio: "ignore", detached: true });
        child.on("error", (error) => {
          addEntry("error", `Could not open a browser: ${errorText(error)}`);
          redraw();
        });
        child.unref();
        addEntry("notice", `Opening ${url}`);
      } catch (error) {
        addEntry("error", `Could not open a browser: ${errorText(error)}`);
      }
      return redraw();
    }
    if (command === "/qr") {
      state.showQr = !state.showQr;
      refreshQr();
      addEntry("notice", state.showQr ? live.scanUrl : "QR hidden");
      return redraw();
    }
    if (command === "/live") {
      addEntry("notice", `Pushing ${live.file} to ${live.scanUrl}`);
      live.push().then(
        () => {
          addEntry("notice", `Pushed · ${live.pushes} total`);
          redraw();
        },
        (error) => liveError(error),
      );
      return redraw();
    }
    if (command === "/new") {
      if (state.busy) {
        addEntry("error", "Interrupt the current turn before starting a new thread.");
      } else {
        state.status = "starting";
        redraw();
        try {
          engine.developerInstructions = developerInstructions();
          await engine.newThread();
          slabSession.connected(engine.threadId);
          state.entries = [
            { kind: "notice", text: `New thread · ${engineLabel()}`, id: `thread-${Date.now()}` },
          ];
          state.status = "ready";
        } catch (error) {
          addEntry("error", errorText(error));
          state.status = "failed";
        }
      }
      return redraw();
    }
    addEntry("error", `Unknown command: ${text}`);
    return redraw();
  }

  if (state.busy) {
    addEntry("error", "A turn is already running. Press ctrl-c to interrupt it.");
    return redraw();
  }

  state.history.push(text);
  state.historyIndex = state.history.length;
  addEntry("user", text);
  slabSession.working(text);
  state.busy = true;
  state.status = "working";
  redraw();
  try {
    await engine.startTurn(text);
  } catch (error) {
    state.busy = false;
    state.status = "failed";
    addEntry("error", errorText(error));
    redraw();
  }
}

function replaceInput(value) {
  state.input = value;
  state.cursor = Array.from(value).length;
}

function insertText(value) {
  const characters = Array.from(state.input);
  const inserted = Array.from(cleanText(value.replace(/\x1b\[200~|\x1b\[201~/g, "")));
  characters.splice(state.cursor, 0, ...inserted);
  state.input = characters.join("");
  state.cursor += inserted.length;
}

function handleKey(input) {
  if (answerApproval(input)) return;

  if (input === "\u0003") {
    if (state.busy) {
      state.status = "interrupting";
      redraw();
      engine.interrupt().catch((error) => addEntry("error", errorText(error)));
    } else {
      finish();
    }
    return;
  }
  if (input === "\u0004" && !state.input) return finish();
  if (input === "\u000c") return redraw();
  if (input === "\u0001") state.cursor = 0;
  else if (input === "\u0005") state.cursor = Array.from(state.input).length;
  else if (input === "\u0015") replaceInput("");
  else if (input === "\u000b") state.input = Array.from(state.input).slice(0, state.cursor).join("");
  else if (input === "\r" || input === "\n") return void submitInput();
  else if (input === "\x1b[A") {
    if (state.historyIndex > 0) replaceInput(state.history[--state.historyIndex]);
  } else if (input === "\x1b[B") {
    if (state.historyIndex < state.history.length - 1) replaceInput(state.history[++state.historyIndex]);
    else {
      state.historyIndex = state.history.length;
      replaceInput("");
    }
  } else if (input === "\x1b[D") state.cursor = Math.max(0, state.cursor - 1);
  else if (input === "\x1b[C") state.cursor = Math.min(Array.from(state.input).length, state.cursor + 1);
  else if (input === "\x7f" || input === "\b") {
    if (state.cursor > 0) {
      const characters = Array.from(state.input);
      characters.splice(--state.cursor, 1);
      state.input = characters.join("");
    }
  } else if (input === "\x1b[3~") {
    const characters = Array.from(state.input);
    characters.splice(state.cursor, 1);
    state.input = characters.join("");
  } else if (!input.startsWith("\x1b")) {
    insertText(input);
  }
  redraw();
}

function handleKeys(buffer) {
  const tokens = buffer.toString("utf8").match(/\x1b\[[0-9;]*[~A-Za-z]|./gsu) || [];
  for (const token of tokens) {
    if (token === "\x1b[200~") {
      pasteBuffer = "";
    } else if (token === "\x1b[201~") {
      if (pasteBuffer !== null) insertText(pasteBuffer);
      pasteBuffer = null;
      redraw();
    } else if (pasteBuffer !== null) {
      pasteBuffer += token;
    } else {
      handleKey(token);
    }
  }
}

process.stdout.write("\x1b[?1049h\x1b[?25l\x1b[?2004h");
process.stdin.setRawMode(true);
process.stdin.resume();
process.stdin.on("data", handleKeys);
process.stdout.on("resize", redraw);
process.on("SIGTERM", () => finish(143));
process.on("SIGHUP", () => finish(129));

// A sign-in or sign-out anywhere in the AC suite shows up here live.
session.watch().on("change", () => {
  refreshAccount(true);
  redraw();
});

// Mint this session's blank piece and the QR code that opens it on a phone.
live.create();
live.watch(liveError);
state.piece = `${live.slug}${live.runtime.extension}`;
refreshQr();

// The entrance plays across the bridge handshake instead of in front of it.
// The handshake is most of a second of nothing; the little guy walks in over
// it, and the interface replaces him mid-stride the moment the bridge answers.
const bootAt = Date.now();
let bootTimer = null;
function bootFrame() {
  if (closing) return;
  const elapsed = Date.now() - bootAt;
  // A frame skipped because a redraw is in flight must still schedule the next
  // one, or the entrance stops mid-stride and never resumes.
  if (drawing) {
    bootTimer = setTimeout(bootFrame, mascotNextFrameIn(elapsed));
    bootTimer.unref?.();
    return;
  }
  const frame = renderBoot(elapsed, process.stdout.columns, process.stdout.rows,
    process.env.NO_COLOR !== "1");
  process.stdout.write(`\x1b[H\x1b[2J${frame}`);
  bootTimer = setTimeout(bootFrame, mascotNextFrameIn(elapsed));
  bootTimer.unref?.();
}
function bootDone() {
  clearTimeout(bootTimer);
  bootTimer = null;
}
bootFrame();
try {
  const connection = await engine.connect();
  bootDone();
  slabSession.connected(connection.thread.id);
  state.status = "ready";
  state.model = connection?.model || model;
  if (resumeThreadId && !restoreThread(connection.thread)) {
    addEntry("notice", `Resumed thread · ${engineLabel()}`);
  } else {
    addEntry("notice", `Ready · ${engineLabel()}`);
  }
  if (!session.signedIn) {
    addEntry("notice", "Not signed in to Aesthetic Computer · /login to publish under your @handle");
  }
  addEntry("notice", `${live.slug}${live.runtime.extension} · scan the code or open ${live.scanUrl}`);
  live.push().catch(() => {});
  redraw();
  if (initialPrompt) {
    replaceInput(initialPrompt);
    await submitInput();
  }
} catch (error) {
  bootDone();
  state.status = "offline";
  addEntry("error", errorText(error));
  redraw();
}
