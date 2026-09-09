#!/usr/bin/env node

import path from "node:path";
import process from "node:process";
import { AppServer } from "./app-server.mjs";
import { cleanText, renderFrame } from "./render.mjs";
import { SlabSession } from "./slab-session.mjs";

const arguments_ = process.argv.slice(2);
const option = (name) => {
  const index = arguments_.indexOf(name);
  return index >= 0 ? arguments_[index + 1] || "" : "";
};
const cwd = path.resolve(option("--cwd") || process.cwd());
const resumeThreadId = option("--resume");
const initialPrompt = option("--prompt");
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
  entries: [
    {
      id: "privacy",
      kind: "notice",
      text: "REMOTE INFERENCE · prompt content may leave this machine",
    },
  ],
};

const slabSession = new SlabSession({ cwd });
slabSession.start();
const engine = new AppServer({
  cwd,
  resumeThreadId,
  environment: {
    SLAB_PROMPT_SESSION_ID: slabSession.sessionId,
    SLAB_TERMINAL_TTY: slabSession.tty,
    SLAB_AGENT_TYPE: "aesthetic-code",
  },
});
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

function redraw() {
  if (closing || drawing) return;
  drawing = true;
  const frame = renderFrame(state, process.stdout.columns, process.stdout.rows, process.env.NO_COLOR !== "1");
  process.stdout.write(`\x1b[H\x1b[2J${frame}`);
  drawing = false;
}

function finish(code = 0) {
  if (closing) return;
  closing = true;
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

function itemSummary(item) {
  if (!item) return null;
  if (item.type === "commandExecution") return { kind: "command", text: item.command };
  if (item.type === "fileChange") {
    const files = (item.changes || []).map((change) => change.path).join(", ");
    return { kind: "change", text: files || "workspace files" };
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

async function submitInput() {
  const text = state.input.trim();
  state.input = "";
  state.cursor = 0;
  state.historyIndex = state.history.length;
  if (!text) return redraw();

  if (text.startsWith("/")) {
    if (text === "/quit" || text === "/exit") return finish();
    if (text === "/clear") {
      state.entries = [];
      return redraw();
    }
    if (text === "/help") {
      addEntry("notice", "/new · /clear · /quit   ctrl-c interrupts a running turn");
      return redraw();
    }
    if (text === "/new") {
      if (state.busy) {
        addEntry("error", "Interrupt the current turn before starting a new thread.");
      } else {
        state.status = "starting";
        redraw();
        try {
          await engine.newThread();
          slabSession.connected(engine.threadId);
          state.entries = [{ kind: "notice", text: "New thread", id: `thread-${Date.now()}` }];
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

engine.on("notification", handleNotification);
engine.on("request", handleRequest);
engine.on("protocolError", (error) => {
  addEntry("error", errorText(error));
  redraw();
});
engine.on("fatal", (error) => {
  if (!closing) {
    state.status = "offline";
    addEntry("error", errorText(error));
    slabSession.awaitingInput("aesthetic code engine bridge is offline");
    redraw();
  }
});

redraw();
try {
  const connection = await engine.connect();
  slabSession.connected(connection.thread.id);
  state.status = "ready";
  if (resumeThreadId && !restoreThread(connection.thread)) addEntry("notice", "Resumed thread");
  else if (connection?.model) addEntry("notice", `Ready · ${connection.model}`);
  redraw();
  if (initialPrompt) {
    replaceInput(initialPrompt);
    await submitInput();
  }
} catch (error) {
  state.status = "offline";
  addEntry("error", errorText(error));
  redraw();
}
