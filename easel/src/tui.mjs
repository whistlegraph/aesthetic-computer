#!/usr/bin/env node

import { spawn } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import process from "node:process";
import { StringDecoder } from "node:string_decoder";
import { aboutMap, conversationHandoff } from "./about.mjs";
import { InputDecoder, mouseEvent, MOUSE_ON, MOUSE_OFF } from "./mouse.mjs";
import { ACSession } from "./ac-session.mjs";
import { Audience } from "./audience.mjs";
import { AutoPublisher } from "./autopublish.mjs";
import { Diagnostics } from "./diagnostics.mjs";
import { EASEL_HEIGHT, easelFrame, easelNextFrame, easelWidth } from "./easel.mjs";
import { backendFor, backendMenu, DEFAULT_BACKEND } from "./backends.mjs";
import { LivePiece } from "./live.mjs";
import { applyUpdate, checkForUpdate, currentVersion, installed } from "./updates.mjs";
import { publishPiece } from "./publish.mjs";
import { qrBlock } from "./qr.mjs";
import { cleanText, color, easelInk, renderBoot, renderFrame, headerAction, wrapText, transcriptLineCount } from "./render.mjs";
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
const initialPiece = option("--piece");
// Which engine bridge drives the conversation, and on which model. The bridge
// can be swapped mid-session with /backend, so neither is a constant.
let backend = backendFor(option("--backend") || process.env.EASEL_BACKEND || DEFAULT_BACKEND);
let model = option("--model") || backend.defaultModel;
let handoff = "";
let archivedConversation = [];
let mouseEnabled = process.env.EASEL_MOUSE !== "0";

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
if (initialPiece) {
  const file = path.resolve(cwd, initialPiece);
  if (!existsSync(file) || !live.retarget(file)) throw new Error("--piece must name an existing supported piece file");
}
const state = {
  workspace: cwd,
  mode: "remote",
  status: "starting",
  busy: false,
  input: "",
  cursor: 0,
  history: [],
  historyIndex: 0,
  // Prompts typed while a turn was running. Thinking ahead of the machine is
  // the normal way to use this thing — you read the first half of an answer and
  // already know the next instruction — and refusing that keystroke threw the
  // sentence away and made you wait to retype it.
  queued: [],
  approval: null,
  account: session.label(),
  piece: "",
  // What the bridge said it is running, once it has said so.
  model: "",
  // Who is watching the piece, once the session server has said. Null until
  // then — see `audience.mjs` on why that is not zero.
  audience: null,
  // What those people's browsers are actually showing — a blank frame, an
  // uncaught error. Null until the relay lets this session listen.
  health: null,
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
  // and `EASEL_AUTOPUBLISH=0` both opt out, and a signed-out session
  // never reaches the attempt.
  enabled:
    !flag("--no-autopublish") &&
    !/^(0|off|false|no)$/i.test(process.env.EASEL_AUTOPUBLISH || ""),
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
// The repo's style guides, named only when the session is actually running in
// the repository that holds them. Naming a path that isn't there teaches the
// model to ignore the whole instruction.
const STYLE_GUIDES = [
  ["system/public/aesthetic.computer/disks/CLAUDE.md", "the piece authoring guide"],
  ["SCREEN.md", "how a piece draws on the AC canvas"],
  ["HAND.md", "how the code reads"],
];

// The same knowledge, carried inside the install. A session opened in the
// Aesthetic Computer repository reads the repo's own copies, which are newer by
// definition; a session opened anywhere else — which is every session, once this
// is installed rather than cloned — reads these. Without them Easel is a general
// editor that happens to publish to a URL, and there is no reason to install it
// over the vendor CLI it is already driving.
const BUNDLED_CONTEXT = [
  ["context/pieces.md", "the piece authoring guide"],
  ["context/screen.md", "how a piece draws on the AC canvas"],
  ["context/hand.md", "how the code reads"],
  ["context/kidlisp.md", "the KidLisp language"],
];

const easelRoot = path.join(path.dirname(fileURLToPath(import.meta.url)), "..");

function styleInstructions() {
  // The working directory wins when it has the guides: inside the monorepo they
  // are the living documents and the bundle is a stale copy of them.
  const present = STYLE_GUIDES.filter(([file]) => existsSync(path.join(cwd, file)));
  const source = present.length
    ? present.map(([file, subject]) => [file, subject])
    : BUNDLED_CONTEXT.map(([file, subject]) => [path.join(easelRoot, file), subject]).filter(
        ([file]) => existsSync(file),
      );
  if (source.length === 0) return [];
  // Inlined rather than named. Every session so far opened by reading these
  // three files — three tool calls and ten seconds before the first thought
  // about the piece — and the bytes cost the same either way. Here they arrive
  // with the first turn and are cached for every turn after it.
  const inlined = source
    .map(([file, subject]) => {
      try {
        return `## ${subject} (${path.relative(cwd, file) || file})\n\n${readFileSync(file, "utf8").trim()}`;
      } catch {
        return "";
      }
    })
    .filter(Boolean);
  const lines = [
    "Style: the Aesthetic Computer guides follow. They are the house rules for a piece and win over your own defaults. Do not re-read them from disk; they are already here.",
    ...inlined,
  ];
  // The one rule that gets broken on a first draft, inlined because a model
  // that skips the read still has to know it. Lua pieces draw through
  // Processing and never see the hud/ui API, so it would only mislead them.
  if (live.runtime.id !== "lua") {
    lines.push(
      "Above all: the system paints its own corner label at (6, 6) in a 6x10 font, and tapping it is how the user gets back. Keep the top-left ~20 rows clear — put readouts along the bottom or right-aligned — or take the label over deliberately with hud.label().",
    );
  }
  return lines;
}

// The native tools, named so the model reaches for them instead of the shell.
// The pattern being replaced is specific: grep graph.mjs for a signature, sed a
// window of disk.mjs, grep disks/ for a call site, page a 9,000-line piece in
// 80-line slices. Each of those is one call here.
function toolInstructions() {
  if (!backend.Engine || backend.id !== "claude") return [];
  return [
    "Tools: you have ac_api (the piece API — signatures, docs and real call sites for circle, line, box, write, sound.synth, ui.Button, pens, events…), ac_examples (pieces that call a symbol), ac_outline (a piece's top-level symbols with line spans) and ac_symbol (one symbol's source). Use them instead of grep/sed/head over lib/ and disks/: ask ac_api before opening graph.mjs or disk.mjs, and outline a large piece before reading any of it. Start writing the piece as soon as the request is clear — the guides above are already the context.",
  ];
}

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
          "A piece is live only after the user runs the Easel command `/publish <file> [slug]`, which uploads it under their @handle at https://aesthetic.computer/@handle/slug.",
          "When you finish a piece, end with the exact /publish command for the user to run. Never tell the user to visit a route that has not been published.",
        ];
  return [
    "You are running inside Easel, a terminal interface for Aesthetic Computer (AC) work.",
    account,
    `This session's piece is ${live.file} (${live.runtime.label}). Its current source is the source of truth; read it before editing and preserve existing work. Edit that file unless the user asks for something else.`,
    "Do not write the piece's name onto the screen: the system already shows it in the corner label. If the file still carries a placeholder that writes its own name, remove it in your first edit.",
    ...dialect,
    ...styleInstructions(),
    "Every save of that file is pushed live to a phone that scanned the interface's QR code, so small frequent edits are better than one big rewrite.",
    ...toolInstructions(),
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
    developerInstructions: [developerInstructions(), handoff].filter(Boolean).join("\n\n"),
    // The hosted bridge has no subprocess and no file tools, so it needs the
    // two things a CLI would have found for itself: which file is the piece,
    // and a token to pay for the turn. The other bridges ignore both.
    piece: live,
    token: async () => {
      if (!session.signedIn) return null;
      try {
        return await session.token();
      } catch {
        return null;
      }
    },
    environment: {
      SLAB_PROMPT_SESSION_ID: slabSession.sessionId,
      SLAB_TERMINAL_TTY: slabSession.tty,
      SLAB_AGENT_TYPE: "easel",
    },
  });
  opened.on("notification", (...args) => { if (!closing && opened === engine) handleNotification(...args); });
  opened.on("request", (...args) => { if (!closing && opened === engine) handleRequest(...args); });
  opened.on("protocolError", (error) => {
    if (closing || opened !== engine) return;
    addEntry("error", errorText(error));
    redraw();
  });
  opened.on("fatal", (error) => {
    if (closing || opened !== engine) return;
    state.status = "offline";
    addEntry("error", errorText(error));
    slabSession.awaitingInput("easel engine bridge is offline");
    redraw();
  });
  return opened;
}

let engine = openEngine({ resume: resumeThreadId });
let drawing = false;
let redrawTimer = null;
let lastDrawAt = 0;
let lastTranscriptLines = 0;
let closing = false;
// The startup easel owns the screen until it is done or dismissed. Declared
// here rather than beside the splash itself because redraw() reads it, and
// redraw() can be called before that block is reached.
let splashing = false;
let splashTimer = null;
let streamedMessageId = null;
let pasteBuffer = null;
let performanceAbort = null;

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
  if (closing || drawing || splashing) return;
  // Token bursts coalesce into at most 30 terminal frames/second.
  const remaining = 33 - (Date.now() - lastDrawAt);
  if (remaining > 0) {
    if (!redrawTimer) redrawTimer = setTimeout(() => { redrawTimer = null; redraw(); }, remaining);
    return;
  }
  lastDrawAt = Date.now();
  drawing = true;
  try {
    const count = transcriptLineCount(state, process.stdout.columns || 80, process.stdout.rows || 24, process.env.NO_COLOR !== "1");
    if (state.scrollOffset) state.scrollOffset = Math.max(0, state.scrollOffset + count - lastTranscriptLines);
    lastTranscriptLines = count;
    const frame = renderFrame(state, process.stdout.columns, process.stdout.rows, process.env.NO_COLOR !== "1");
    process.stdout.write(`\x1b[H\x1b[2J${frame}`);
  } finally {
    drawing = false;
  }
}

async function finish(code = 0) {
  if (closing) return;
  closing = true;
  performanceAbort?.abort();
  session.unwatch();
  const pending = autopublish.pending || autopublish.running;
  live.unwatch();
  audience.close();
  slabSession.close();
  engine.close();
  process.stdin.setRawMode(false);
  process.stdin.pause();
  process.stdout.write(MOUSE_OFF + "\x1b[?2004l\x1b[?25h\x1b[?1049l");
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

// Who is watching. The channel is the piece's public route once a handle has
// resolved, which is the same name the published address carries — so the count
// is of people at the address on the splash, not of some private side channel.
const audience = new Audience({ channel: live.channel });

audience.on("change", (report) => {
  state.audience = report;
  redraw();
});

// What the piece looks like from inside the browsers running it. Rides the
// audience's socket — one connection, two readouts.
const health = new Diagnostics({
  channel: live.channel,
  token: async () => {
    if (!session.signedIn) return null;
    try {
      return await session.token();
    } catch {
      return null;
    }
  },
});

audience.on("open", () => {
  health.attach((type, content) => audience.send(type, content)).catch(() => {});
});
audience.on("message", (message) => health.receive(message));

health.on("change", (report) => {
  state.health = report;
  redraw();
});

// An error from the piece is news, so it goes in the transcript rather than
// only into a counter the user has to notice.
health.on("log", (line) => {
  if (line.level !== "error") return;
  addEntry("error", `Piece: ${line.text}`);
  redraw();
});

// Follow the piece: a sign-in turns the fallback channel into `@handle/slug`,
// and a rename or a retarget moves it again.
function refreshAudience() {
  audience.watch(live.channel);
  health.watch(live.channel).catch(() => {});
}

function refreshQr() {
  state.qr = state.showQr ? qrBlock(live.scanUrl) : null;
  // The rock in the menu bar carries the same address. `/qr` hides the code in
  // here, not out there — the rock is a different surface with its own room,
  // and hiding one is no reason to blank the other.
  slabSession.live(`${live.slug}${live.runtime.extension}`, live.scanUrl);
  // The scanned address and the watched channel are the same name, so whatever
  // moved one moved the other.
  refreshAudience();
}

function itemSummary(item) {
  if (!item) return null;
  if (item.type === "commandExecution") return { kind: "command", text: item.command };
  if (item.type === "fileChange") {
    const paths = (item.changes || []).map((change) => change.path).filter(Boolean);
    if (item.path) paths.push(item.path);
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
      state.status = "waiting";
      state.progressBytes = 0;
      engine.turnId = params.turn?.id || engine.turnId;
      slabSession.working();
      break;
    case "turn/progress":
      state.status = params.phase || "working";
      state.progressBytes = params.bytes || state.progressBytes || 0;
      break;
    case "item/agentMessage/delta":
      state.status = "generating";
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
      if (params.item?.type === "fileChange") state.status = "writing";
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
      else if (params.turn?.status === "failed") slabSession.awaitingInput("easel turn failed");
      else slabSession.complete();
      // Whatever the turn wrote goes out now rather than on the coalescing
      // timer. An interrupted turn publishes too — the user stopped the agent,
      // not the file, and what is on disk is still what they are looking at.
      publishTurn();
      // An interrupt is a decision about everything you were going to say, not
      // just the turn that was running, so ctrl-c drops the queue with it.
      if (params.turn?.status === "interrupted" && state.queued.length) {
        const dropped = state.queued.length;
        state.queued.length = 0;
        addEntry("notice", `Interrupted · dropped ${dropped} queued`);
      }
      drainQueue();
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
        ? "easel needs command approval"
        : "easel needs file approval",
    );
    state.status = "approval";
    redraw();
    return;
  }
  engine.reject(request.id, -32601, `Easel does not support ${request.method} yet`);
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

// The address on the rock is the piece's published address, so it has to exist
// before anyone scans it — including in the first seconds of a session, before
// a single edit. Publishing the blank is what makes the code on the rock point
// at a page instead of a 404.
//
// It cannot run at startup: the handle arrives asynchronously, and without one
// there is no route to publish to. So it is armed here instead and fires on
// whichever comes first — a session that was already signed in, or the moment a
// sign-in resolves.
let blankPublished = false;

function publishBlankOnce() {
  if (blankPublished) return;
  if (!autopublish.enabled || autopublishBlocker()) return;
  blankPublished = true;
  autopublish.note(live.source());
}

// A turn is the natural moment to publish. Mid-turn the agent may write a file
// five times in ten seconds, so the coalescing window is doing real work and
// should be left alone; but once the turn is over the file is as finished as it
// is going to get, and waiting out the rest of `minGap` only means the address
// the user is about to open still answers with the previous version.
function publishTurn() {
  if (!autopublish.enabled || autopublishBlocker()) return;
  // The file watcher debounces its own save notice, so a write from the last
  // moments of the turn may not have been noted yet. Read it here instead of
  // racing that timer.
  autopublish.note(live.source());
  // Failures already reach the transcript through the `failed` event.
  autopublish.flush().catch(() => {});
}

function refreshAccount(announce = false) {
  const previous = state.account;
  state.account = session.label();
  slabSession.identity(session.handle);
  live.handle = session.handle || "";
  publishBlankOnce();
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
      handle
        ? `Signed in as @${handle}`
        : "Signed in · you have no handle yet. Type /handle <name> to claim one — it is what pays for hosted inference and what your pieces publish under.",
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
    // wait for the next keystroke to notice the piece exists. That includes an
    // untouched blank: the point of publishing is that the address answers.
    blankPublished = true;
    autopublish.note(live.source());
  }
  // How publishing works is part of the developer instructions, and those are
  // written once when the thread opens. A mid-session toggle is real
  // immediately for the interface and only reaches the model on a new thread —
  // say so, rather than letting it keep recommending /publish for a piece that
  // is already live.
  addEntry("notice", "The model is told when a thread opens · /new to tell it now");
  return redraw();
}

let manualPublishInFlight = false;
async function commandPublish(argumentText) {
  if (manualPublishInFlight || autopublish.running || state.status === "restoring") {
    addEntry("notice", "Wait for the current upload or rollback to finish before publishing.");
    return redraw();
  }
  const [file = live.file, slug = ""] = argumentText.split(/\s+/).filter(Boolean);
  if (!file) {
    addEntry("error", "Usage: /publish <file> [slug] — no piece has been touched yet.");
    return redraw();
  }
  manualPublishInFlight = true;
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
  } finally {
    manualPublishInFlight = false;
  }
  redraw();
}

// ── engine commands ─────────────────────────────────────────────────────

function engineLabel() {
  return `${backend.label} · ${state.model || model || backend.modelSource}`;
}

// Provider thread IDs cannot cross engines; carry recent conversation and
// keep the old connection available until the replacement connects.
async function restartEngine(note, nextBackend = backend, nextModel = model) {
  if (nextBackend.models && !Object.hasOwn(nextBackend.models, nextModel)
      && !Object.values(nextBackend.models).includes(nextModel)) {
    addEntry("error", "Unknown hosted model. Use /model to see available choices.");
    return redraw();
  }
  const previousBackend = backend, previousModel = model, previousLabel = state.model;
  const previousHandoff = handoff;
  handoff = conversationHandoff([...archivedConversation, ...state.entries]);
  backend = nextBackend;
  model = nextModel;
  state.status = "starting";
  state.busy = true;
  redraw();
  const previous = engine;
  try {
    engine = openEngine();
    const connection = await engine.connect();
    previous.close();
    slabSession.connected(engine.threadId);
    state.model = connection?.model || model;
    addEntry("notice", `${note} · ${engineLabel()} · current piece and recent conversation carried over`);
    state.status = "ready";
  } catch (error) {
    const failed = engine;
    engine = previous;
    if (failed !== previous) failed.close();
    backend = previousBackend;
    model = previousModel;
    state.model = previousLabel;
    handoff = previousHandoff;
    addEntry("error", errorText(error));
    state.status = "ready";
  }
  state.busy = false;
  redraw();
  drainQueue();
}

async function commandBackend(rest) {
  if (!rest) {
    addEntry("notice", `${engineLabel()}\n/backend ac — AC hosted, handle budget\n/backend claude — your Claude CLI sign-in\n/backend codex — your Codex CLI sign-in\n/model — models on the selected engine`);
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
  return restartEngine("Engine", next, wantedModel || next.defaultModel);
}

async function commandModel(rest) {
  if (!rest) {
    const choices = backend.models ? Object.entries(backend.models).map(([alias, id]) => `/model ${alias} — ${id}${["sonnet", "gpt"].includes(alias) ? " · premium, uses budget faster" : ""}`).join("\n")
      : "/model NAME — a model supported by your signed-in CLI";
    addEntry("notice", `${engineLabel()}\n${choices}\n/backend — switch between AC hosted and your own Claude/Codex`);
    return redraw();
  }
  if (state.busy) {
    addEntry("error", "Interrupt the current turn before switching models.");
    return redraw();
  }
  return restartEngine("Model", backend, rest.split(/\s+/)[0]);
}

async function commandPerformance(rest) {
  if (state.busy) { addEntry("notice", "Wait for the current turn before benchmarking."); return redraw(); }
  performanceAbort = new AbortController();
  state.busy = true;
  state.status = "benchmarking";
  const id = addEntry("notice", `Measuring ${state.piece} · headless logic…`);
  redraw();
  try {
    const { benchmarkPiece } = await import("./perf.mjs");
    const result = await benchmarkPiece({ file: live.file, frames: rest ? Number(rest) : 600, signal: performanceAbort.signal });
    const calls = Object.entries(result.drawCalls).map(([name, count]) => `${Number(count).toFixed(1)} ${name}`).join(" · ");
    updateEntry(id, "notice", `Headless logic · ${result.msPerFrame.toFixed(3)} ms/frame · ${result.frames} frames at ${result.width}×${result.height}\nPer frame: ${calls}\nExcludes browser rendering, rasterization and display latency.`);
  } catch (error) { updateEntry(id, "error", errorText(error)); }
  finally { performanceAbort = null; state.busy = false; state.status = "ready"; }
  redraw();
  drainQueue();
}

// Start the next queued line, if the turn that just ended left one. Routed back
// through the same path a typed line takes, so a queued `/command` still behaves
// like a command rather than becoming a prompt.
function drainQueue() {
  if (state.busy || !state.queued.length) return;
  const next = state.queued.shift();
  state.input = next;
  state.cursor = Array.from(next).length;
  submitInput().catch((error) => {
    addEntry("error", errorText(error));
    redraw();
  });
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
    if (command === "/about") {
      state.about = !state.about;
      state.aboutScroll = 0;
      return redraw();
    }
    if (command === "/mouse") {
      mouseEnabled = rest !== "off";
      process.stdout.write(mouseEnabled ? MOUSE_ON : MOUSE_OFF);
      state.hover = "";
      addEntry("notice", `Mouse ${mouseEnabled ? "on · shift-drag selects in supporting terminals" : "off · terminal selection restored"}`);
      return redraw();
    }
    if (command === "/profile") return openProfile();
    if (command === "/performance" || command === "/perf") return commandPerformance(rest);
    if (command === "/latest") { state.scrollOffset = 0; return redraw(); }
    if (command === "/clear") {
      archivedConversation.push(...state.entries.filter(({ kind }) => kind === "user" || kind === "assistant"));
      state.entries = [];
      state.scrollOffset = 0;
      return redraw();
    }
    if (command === "/handle") {
      if (!session.signedIn) {
        addEntry("notice", "Sign in first with /login.");
        return redraw();
      }
      if (!rest) {
        addEntry(
          "notice",
          session.handle
            ? `You are @${session.handle}.`
            : "No handle yet. /handle <name> claims one — letters and digits, up to 16.",
        );
        return redraw();
      }
      const claiming = addEntry("notice", `Claiming @${rest.replace(/^@/, "")}…`);
      redraw();
      try {
        const claimed = await session.claimHandle(rest);
        refreshAccount();
        updateEntry(claiming, "notice", `You are @${claimed}. Pieces publish at aesthetic.computer/@${claimed}/…`);
      } catch (error) {
        updateEntry(claiming, "error", errorText(error));
      }
      return redraw();
    }
    if (command === "/update") {
      if (!installed()) {
        addEntry("notice", `Easel ${currentVersion()} — running from a checkout, so there is nothing to update. Use git.`);
        return redraw();
      }
      addEntry("notice", "Checking for a newer Easel…");
      redraw();
      try {
        const update = await checkForUpdate({ force: true });
        if (!update) {
          addEntry("notice", `Easel ${currentVersion()} is the latest.`);
          return redraw();
        }
        addEntry("notice", `Installing Easel ${update.version}…`);
        redraw();
        const version = await applyUpdate({ manifest: update });
        addEntry("notice", `Easel ${version} installed. Restart to run it.`);
      } catch (error) {
        addEntry("error", `Update failed: ${errorText(error)}`);
      }
      return redraw();
    }
    if (command === "/versions") {
      const versions = live.history.list();
      addEntry("notice", versions.length ? versions.map((entry) => `v${entry.version} · ${entry.updatedAt}${entry.restoredFrom ? ` · restored v${entry.restoredFrom}` : ""}`).join("\n") : "No saved versions yet.");
      return redraw();
    }
    if (command === "/rollback") {
      if (state.busy || manualPublishInFlight || autopublish.running || live.sending) {
        addEntry("notice", "Wait for the current turn and uploads to finish before rolling back.");
        return redraw();
      }
      const version = /^v?([1-9]\d*)$/.exec(rest.trim())?.[1];
      if (!version) { addEntry("notice", "Use /rollback v1 · /versions lists saved versions."); return redraw(); }
      state.busy = true;
      state.status = "restoring";
      autopublish.cancel();
      try {
        const revision = await live.rollback(Number(version));
        addEntry("notice", `Restored v${version} as v${revision.version}.`);
        await live.push();
        publishTurn();
      } catch (error) { addEntry("error", errorText(error)); }
      finally { state.busy = false; state.status = "ready"; }
      drainQueue();
      return redraw();
    }
    if (command === "/help") {
      addEntry(
        "notice",
        "/about · /profile · /mouse [on|off] · /performance [frames] · /latest · /login · /logout · /whoami · /publish [file] · /autopublish [on|off] · /ask [on|off] · /piece [name] · /versions · /rollback vN · /runtime [id] · /backend [id] · /model [name] · /handle [name] · /update · /open · /qr · /live · /new · /clear · /quit   ctrl-c interrupts a running turn",
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
    if (["/backend", "/engine", "/mode"].includes(command)) return commandBackend(rest);
    if (command === "/model" || command === "/models") return commandModel(rest);
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
          handoff = "";
          archivedConversation = [];
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
    state.queued.push(text);
    const place = state.queued.length > 1 ? ` (${state.queued.length} queued)` : "";
    addEntry("notice", `Queued${place} · ${text}`);
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
    // A turn that never started emits no turn/completed, so the queue has to be
    // let go from here too or it waits for a turn that will never come.
    drainQueue();
  }
}

function replaceInput(value) {
  state.input = value;
  state.cursor = Array.from(value).length;
}

function openProfile() {
  if (!session.handle) {
    addEntry("notice", "Sign in with /login to open your profile.");
    return redraw();
  }
  const url = `https://aesthetic.computer/@${encodeURIComponent(session.handle)}`;
  const opener = process.platform === "darwin" ? "open" : process.platform === "win32" ? "explorer" : "xdg-open";
  const child = spawn(opener, [url], { stdio: "ignore", detached: true });
  child.on("error", error => { addEntry("error", `Could not open profile: ${errorText(error)}`); redraw(); });
  child.unref();
}

function scrollAbout(delta) {
  const height = Math.max(10, process.stdout.rows || 24);
  const width = Math.max(32, process.stdout.columns || 80) - 2;
  const count = aboutMap().flatMap(line => wrapText(line, width)).length;
  state.aboutScroll = Math.max(0, Math.min(Math.max(0, count - (height - 5)), (state.aboutScroll || 0) + delta));
  redraw();
}

function scrollTranscript(delta) {
  const height = Math.max(10, process.stdout.rows || 24);
  const count = transcriptLineCount(state, process.stdout.columns || 80, height, process.env.NO_COLOR !== "1");
  const offset = state.scrollOffset || 0;
  state.scrollOffset = Math.max(0, Math.min(Math.max(0, count - (height - 5)),
    offset + (offset ? count - lastTranscriptLines : 0) + delta));
  lastTranscriptLines = count;
  redraw();
}

function insertText(value) {
  const characters = Array.from(state.input);
  const inserted = Array.from(cleanText(value.replace(/\x1b\[200~|\x1b\[201~/g, "")));
  characters.splice(state.cursor, 0, ...inserted);
  state.input = characters.join("");
  state.cursor += inserted.length;
}

function handleKey(input) {
  if (state.about && ["\x1b", "\x1b[A", "\x1b[B", "\x1b[5~", "\x1b[6~"].includes(input)) {
    if (input === "\x1b") { state.about = false; return redraw(); }
    return scrollAbout(input === "\x1b[A" ? -1 : input === "\x1b[B" ? 1 : input === "\x1b[5~" ? -8 : 8);
  }
  if (input === "\x1b[5~") return scrollTranscript(Math.max(1, (process.stdout.rows || 24) - 7));
  if (input === "\x1b[6~") return scrollTranscript(-Math.max(1, (process.stdout.rows || 24) - 7));
  if (["\x1b[F", "\x1b[4~", "\x1b[1;2F"].includes(input) && !state.input) {
    state.scrollOffset = 0;
    return redraw();
  }
  // The easel is a greeting, not a gate. Any key puts it away.
  if (splashing) {
    splashing = false;
    clearTimeout(splashTimer);
    splashTimer = null;
    redraw();
  }
  if (answerApproval(input)) return;

  if (input === "\u0003") {
    if (performanceAbort) { performanceAbort.abort(new Error("Benchmark cancelled.")); return; }
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

const inputDecoder = new InputDecoder();
const utf8Decoder = new StringDecoder("utf8");
let escapeTimer;
function handleKeys(buffer) {
  clearTimeout(escapeTimer);
  const tokens = inputDecoder.push(utf8Decoder.write(buffer));
  escapeTimer = setTimeout(() => inputDecoder.escape().forEach(handleKey), 35);
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
      const mouse = mouseEvent(token);
      if (mouse) {
        if (!mouseEnabled || splashing) continue;
        if (state.about && mouse.wheel) { scrollAbout(mouse.wheel * 3); continue; }
        if (mouse.wheel) { scrollTranscript(-mouse.wheel * 3); continue; }
        const action = headerAction(state, process.stdout.columns || 80, process.stdout.rows || 24, mouse.x, mouse.y);
        if (state.hover !== action) { state.hover = action; redraw(); }
        if (mouse.click && action === "about") { state.about = !state.about; state.aboutScroll = 0; redraw(); }
        if (mouse.click && action === "profile") openProfile();
        continue;
      }
      handleKey(token);
    }
  }
}

process.stdout.write("\x1b[?1049h\x1b[?25l\x1b[?2004h" + (mouseEnabled ? MOUSE_ON : ""));

// 🎨 Stand the easel up. Each frame reads the live values rather than a
// snapshot, so the address is written onto the canvas at whatever moment the
// sign-in resolves — which is the honest thing to show, since that is exactly
// when the piece's address starts answering.
function splashTick() {
  const elapsed = Date.now() - splashStartedAt;
  const canvas = { piece: live.slug, address: live.scanUrl };
  const columns = process.stdout.columns || 80;
  // NO_COLOR gets the plain frame, the same as the entrance does.
  const lines = easelFrame(elapsed, canvas, process.env.NO_COLOR === "1" ? null : easelInk);
  // Measured, not counted: the painted lines carry escapes that take no columns.
  const pad = Math.max(0, Math.floor((columns - easelWidth(canvas.piece, canvas.address)) / 2));
  const gap = Math.max(0, Math.floor(((process.stdout.rows || 24) - EASEL_HEIGHT) / 2));
  const body = lines.map((line) => " ".repeat(pad) + line).join("\n");
  process.stdout.write(`\x1b[H\x1b[2J${color.ground}${"\n".repeat(gap)}${body}`);

  const next = easelNextFrame(elapsed, canvas);
  if (next === null) {
    splashing = false;
    splashTimer = null;
    return redraw();
  }
  splashTimer = setTimeout(splashTick, next);
  splashTimer.unref?.();
}

// Skip it on a screen too small to hold it — a clipped easel is worse than
// none — and whenever output is not a terminal at all.
const splashStartedAt = Date.now();
if (process.stdout.isTTY && (process.stdout.rows || 0) >= EASEL_HEIGHT + 4) {
  splashing = true;
  splashTick();
}

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
if (!initialPiece) live.create();
live.watch(liveError);
publishBlankOnce();

// 🆕 Ask once a day, in the background, and say nothing unless there is news.
// Deliberately not automatic: replacing the tool someone is mid-sentence with
// is the wrong kind of surprise, and a line they can ignore costs nothing.
checkForUpdate()
  .then((update) => {
    if (!update) return;
    addEntry(
      "notice",
      `Easel ${update.version} is out — you have ${update.current}. Run /update to install it.`,
    );
    redraw();
  })
  .catch(() => {});
// Every save that reaches the phone is a candidate for the public URL too, and
// so is the blank. That reverses an earlier rule — an untouched session used to
// leave nothing behind, out there or in the workspace — because the address on
// the rock is now the published one, and a code that resolves to a 404 until
// someone types is worse than a published blank. The local file is still
// discarded on exit if it was never edited; the published copy stays.
live.on("push", (_count, source) => {
  slabSession.flow(live.ahead ? "ahead" : "live");
  if (live.pristine || autopublishBlocker()) return;
  autopublish.note(source);
});
// A save has landed and the channel has not heard about it yet. The rock's
// neighbour — the preview of the very address the rock encodes — says so, so
// that an old frame never passes for the current one.
live.on("dirty", () => slabSession.flow("ahead"));
live.on("revision", (revision) => {
  state.pieceVersion = revision.version;
  slabSession.revision(revision);
  redraw();
});
live.checkpoint().catch(liveError);
state.piece = `${live.slug}${live.runtime.extension}`;
refreshQr();
audience.start();

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
