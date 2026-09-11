#!/usr/bin/env node

import { spawn } from "node:child_process";
import path from "node:path";
import process from "node:process";
import { ACSession } from "./ac-session.mjs";
import { AutoPublisher } from "./autopublish.mjs";
import { backendFor, backendMenu, DEFAULT_BACKEND } from "./backends.mjs";
import { LivePiece } from "./live.mjs";
import { publishPiece } from "./publish.mjs";
import { qrBlock } from "./qr.mjs";
import { cleanText, renderBoot, renderFrame } from "./render.mjs";
import { mascotNextFrameIn, mascotRowNextFrameIn } from "./mascot.mjs";
import { DEFAULT_RUNTIME, runtimeMenu } from "./runtimes.mjs";
import { SlabSession } from "./slab-session.mjs";

const arguments_ = process.argv.slice(2);
const option = (name) => {
  const index = arguments_.indexOf(name);
  return index >= 0 ? arguments_[index + 1] || "" : "";
};
const flag = (name) => arguments_.includes(name);
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
const live = new LivePiece({
  cwd,
  runtime: option("--runtime") || DEFAULT_RUNTIME,
  // `/run` accepts a push only from the handle that owns the channel, so a
  // push carries the session's own token. A signed-out session resolves null
  // here and simply does not push.
  token: async () => {
    if (!session.signedIn) return null;
    try {
      return await session.token();
    } catch {
      return null;
    }
  },
});
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
  // The prompt rock in the menu bar draws this session's code at real pixel
  // resolution, so the transcript does not spend seventeen rows on a worse
  // copy of it. `/qr` still brings it back — on a machine with no Slab menu
  // bar the code in here is the only way onto a phone.
  showQr: false,
  // Actions run without stopping to ask, and are reported once they have. The
  // engine has no OS sandbox of its own, so what still holds a session in is
  // narrower than a prompt: file tools confined to this directory, the fetching
  // tools withheld, and none of the user's own settings or servers in scope.
  // `/ask on` trades the speed back for the question.
  autoAllow: true,
  entries: [
    {
      id: "privacy",
      kind: "notice",
      text: "REMOTE INFERENCE · prompt content may leave this machine",
    },
  ],
};

// Publishing on every save, when the session asked for it. The token stays in
// here — this is the interface publishing on its own schedule, not a tool the
// agent can reach — and the piece keeps its own name, so a session's URL is
// settled the moment auto-publish is on.
const autopublish = new AutoPublisher({
  // On by default. The scanned address is the published one, so a session
  // that does not publish has nothing to point a camera at; `--no-autopublish`
  // and `AESTHETIC_CODE_AUTOPUBLISH=0` both opt out, and a signed-out session
  // never reaches the attempt.
  enabled:
    !flag("--no-autopublish") &&
    !/^(0|off|false|no)$/i.test(process.env.AESTHETIC_CODE_AUTOPUBLISH || ""),
  publish: () => publishPiece({ file: live.file, slug: live.slug, session, cwd }),
});

// Why a save might not be publishable. Auto-publish stays quiet about all of
// these until something asks it to publish — an unsigned-in session should not
// narrate a failure on every keystroke.
function autopublishBlocker() {
  if (!session.signedIn) return "not signed in · /login to publish";
  if (!session.handle) return "this account has no @handle yet";
  if (!live.runtime.routable) return `${live.runtime.label} has no @handle route yet`;
  return "";
}

function autopublishRoute() {
  return session.handle ? live.publishedUrl(session.handle) : "";
}

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
  // With auto-publish on, telling the user to run /publish is wrong twice: the
  // work is already done, and the URL it would print is one they already have.
  const publishing =
    autopublish.enabled && !autopublishBlocker()
      ? [
          `Auto-publish is ON for this session: the interface publishes ${live.file} to ${autopublishRoute()} a couple of seconds after every save. That URL is live and stays live after this session ends.`,
          "So do NOT end with a /publish command and do NOT tell the user to publish — say the piece is live and name that URL. Only mention /publish if a publish is reported as failing.",
        ]
      : [
          "Publishing: writing a file under system/public/aesthetic.computer/disks/ or anywhere else does NOT make a piece live.",
          "A piece is live only after the user runs the Aesthetic Code command `/publish <file> [slug]`, which uploads it under their @handle at https://aesthetic.computer/@handle/slug.",
          "When you finish a piece, end with the exact /publish command for the user to run. Never tell the user to visit a route that has not been published.",
        ];
  return [
    "You are running inside Aesthetic Code, a terminal interface for Aesthetic Computer (AC) work.",
    account,
    `This session's piece is ${live.file} (${live.runtime.label}). It already exists as a blank piece. Edit that file unless the user asks for something else.`,
    ...dialect,
    "Every save of that file is pushed live to a phone that scanned the interface's QR code, so small frequent edits are better than one big rewrite.",
    ...publishing,
    "Dev servers: do not stop a dev server you were asked to start; say that it is still running.",
  ].join("\n");
}

const slabSession = new SlabSession({ cwd });
slabSession.start();
slabSession.identity(session.handle);
live.handle = session.handle || "";

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
let danceTimer = null;
const danceStartedAt = Date.now();
// While the machine has the floor the footer figure moves, and a turn that is
// thinking rather than printing sends no events to repaint on — so the dance
// keeps its own slow tick and drops it the moment the turn ends.
function danceTick() {
  danceTimer = null;
  if (closing) return;
  state.mascotMs = Date.now() - danceStartedAt;
  const next = mascotRowNextFrameIn(state.mascotMs, state.busy);
  if (next === null) return;
  redraw();
  danceTimer = setTimeout(danceTick, next);
  danceTimer.unref?.();
}
function startDance() {
  state.mascotMs = Date.now() - danceStartedAt;
  if (!danceTimer) danceTick();
}

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

async function finish(code = 0) {
  if (closing) return;
  closing = true;
  session.unwatch();
  const pending = autopublish.pending || autopublish.running;
  live.unwatch();
  slabSession.close();
  engine.close();
  process.stdin.setRawMode(false);
  process.stdin.pause();
  process.stdout.write("\x1b[?2004l\x1b[?25h\x1b[?1049l");
  process.exitCode = code;
  // The last save has to land. Quitting a second after an edit would otherwise
  // drop it — auto-publish coalesces, and the timer it was waiting on dies with
  // the process. This runs after the screen is handed back, so it prints as
  // ordinary terminal output rather than into a frame that is already gone.
  if (pending) {
    process.stdout.write("publishing the last save…\n");
    try {
      const result = await autopublish.flush();
      process.stdout.write(result ? `${result.route}\n` : "the last save did not publish\n");
    } catch {
      process.stdout.write("the last save did not publish\n");
    }
  }
  // The blank goes last: an untouched piece is deleted, and deleting it before
  // a flush would publish an empty file or nothing at all.
  live.cleanup();
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

// One line in the transcript, rewritten in place. Auto-publish runs on its own
// every few seconds for a whole session; it does not get to push the
// conversation off the screen doing it.
const AUTOPUBLISH_ENTRY = "autopublish";

autopublish.on("start", () => {
  updateEntry(AUTOPUBLISH_ENTRY, "publish", `Publishing ${live.slug}…`);
  redraw();
});

autopublish.on("published", (result) => {
  updateEntry(
    AUTOPUBLISH_ENTRY,
    "publish",
    `${result.route} · auto${result.verified ? "" : " · uploaded, not yet readable"}`,
  );
  redraw();
});

autopublish.on("failed", (error) => {
  updateEntry(AUTOPUBLISH_ENTRY, "error", `Auto-publish failed: ${errorText(error)}`);
  redraw();
});

function refreshQr() {
  state.qr = state.showQr ? qrBlock(live.scanUrl) : null;
  // The rock in the menu bar carries the same address. `/qr` hides the code in
  // here, not out there — the rock is a different surface with its own room,
  // and hiding one is no reason to blank the other.
  slabSession.live(`${live.slug}${live.runtime.extension}`, live.scanUrl);
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
      startDance();
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
    const subject = approvalSubject(request.method, request.params || {});
    // Allowed without asking, by default. The first real session spent two of
    // its two hours and nineteen minutes parked on prompts with nobody sitting
    // in front of them, and that is the failure this default answers.
    //
    // The interface stays the approver rather than handing the decision down to
    // the engine, so every action still arrives here and is still written into
    // the transcript: you read what ran instead of being asked about it first.
    // `/ask on` puts the question back for the rest of the session.
    if (state.autoAllow) {
      engine.respond(request.id, { decision: "accept" });
      addEntry("notice", `Ran: ${subject}`);
      redraw();
      return;
    }
    state.approval = { id: request.id, method: request.method, subject };
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
  live.handle = session.handle || "";
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

function commandAutopublish(argumentText) {
  const word = argumentText.trim().toLowerCase();
  if (word && !/^(on|off|yes|no|true|false|1|0)$/.test(word)) {
    addEntry("error", "Usage: /autopublish [on|off]");
    return redraw();
  }
  const wanted = word ? /^(on|yes|true|1)$/.test(word) : !autopublish.enabled;
  autopublish.set(wanted);
  const blocker = autopublishBlocker();
  if (!wanted) {
    addEntry("notice", "Auto-publish off · /publish puts the piece live");
  } else if (blocker) {
    addEntry("notice", `Auto-publish on · nothing will publish yet: ${blocker}`);
  } else {
    addEntry("notice", `Auto-publish on · every save goes to ${autopublishRoute()}`);
    // Turning it on mid-session should publish what is already written, not
    // wait for the next keystroke to notice the piece exists.
    if (!live.pristine) autopublish.note(live.source());
  }
  // How publishing works is part of the developer instructions, and those are
  // written once when the thread opens. A mid-session toggle is real
  // immediately for the interface and only reaches the model on a new thread —
  // say so, rather than letting it keep recommending /publish for a piece that
  // is already live.
  addEntry("notice", "The model is told when a thread opens · /new to tell it now");
  return redraw();
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
        "/login · /logout · /whoami · /publish [file] · /autopublish [on|off] · /ask [on|off] · /piece [name] · /runtime [id] · /backend [id] · /model [name] · /open · /qr · /live · /new · /clear · /quit   ctrl-c interrupts a running turn",
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
    if (command === "/autopublish" || command === "/auto") return commandAutopublish(rest);
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
    // The way back. Auto-allow is the default, so this is the control that
    // matters most in here: one word returns the question for the rest of the
    // session, and the notice says which way it went rather than assuming the
    // reader remembers which way it was.
    if (command === "/ask") {
      const want = rest.trim().toLowerCase();
      state.autoAllow = want === "on" ? false : want === "off" ? true : !state.autoAllow;
      addEntry(
        "notice",
        state.autoAllow
          ? "Running without asking · /ask on to be asked first"
          : "Asking before each action · /ask off to stop asking",
      );
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
  startDance();
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
// Every save that reaches the phone is a candidate for the public URL too. The
// blank is not: an untouched session should leave nothing behind, out there or
// in the workspace.
live.on("push", () => {
  if (live.pristine || autopublishBlocker()) return;
  autopublish.note(live.source());
});
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
  addEntry(
    "notice",
    `${live.slug}${live.runtime.extension} · scan the rock, /open in a browser, ` +
      `or /qr for a code · ${live.scanUrl}`,
  );
  if (autopublish.enabled) {
    const blocker = autopublishBlocker();
    addEntry(
      "notice",
      blocker
        ? `Auto-publish on · nothing will publish yet: ${blocker}`
        : `Auto-publish on · every save goes to ${autopublishRoute()}`,
    );
  }
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
