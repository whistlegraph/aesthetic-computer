// claude-server.mjs — the Claude engine bridge.
//
// The same shape as app-server.mjs: a subprocess on stdio, one thread, turns
// inside it, streamed text, and every approval coming back to this terminal.
// Where Codex speaks its own JSON-RPC, Claude Code speaks the headless
// stream-json protocol — `claude --print --input-format stream-json
// --output-format stream-json` — so this file is a translator. It reads
// Claude's messages and emits the notifications the interface already listens
// for, and it answers Claude's permission requests with the interface's own
// y/a/n.
//
// Two flags carry the approval contract, and both are load-bearing:
//
//   --permission-prompt-tool stdio  tells Claude that this process answers
//   permission prompts. Without it the CLI has nobody to ask, every prompt is
//   auto-denied, and the model simply narrates that it lacks permission — the
//   session looks broken rather than gated.
//
//   --setting-sources ""  keeps the user's own settings out of a piece-writing
//   session: their allow-lists, hooks and MCP servers. Codex's bridge does the
//   same thing by overriding a permissive ~/.codex/config.toml with
//   `on-request` and `workspace-write` at thread/start. The person watching
//   this terminal should be the only thing that can approve a command in it.
//
// What this bridge cannot carry is the other half of Codex's posture: an
// operating-system sandbox. Codex runs commands with `workspace-write` and
// `networkAccess: false`; Claude Code has no equivalent, so a command reaches
// the network once it is approved. Reads and writes are confined to the
// workspace, WebFetch and WebSearch are removed, and network commands do
// prompt — but the prompt, not the kernel, is the boundary. See
// docs/local-contract.md.
import { spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import { EventEmitter } from "node:events";
import { createInterface } from "node:readline";

const VERSION = "0.4.0";

// What a session opens on. Fable is the model this harness was built for and
// where it wants to land, but the account it runs on cannot bill Fable today
// (`out_of_credits`, seven-day overage exhausted), and a default that greets
// every session with a red error line is not a default. Opus until the credits
// come back, then this goes straight back to "claude-fable-5-1".
export const DEFAULT_CLAUDE_MODEL = "claude-opus-5";

// Tools whose work is a file change, and the input field naming the file. The
// interface watches these to follow the piece the agent is writing.
const FILE_TOOLS = new Map([
  ["Write", "file_path"],
  ["Edit", "file_path"],
  ["MultiEdit", "file_path"],
  ["NotebookEdit", "notebook_path"],
]);

const COMMAND_TOOLS = new Set(["Bash", "BashOutput", "KillShell"]);

// Tools this interface has no room for: two that leave the machine on their
// own, and one that hides a whole second conversation behind a single line.
const WITHHELD_TOOLS = ["WebFetch", "WebSearch", "Task"];

// A short second line for a tool that is neither a command nor a file change.
function toolDetail(input = {}) {
  for (const key of ["pattern", "query", "path", "file_path", "url", "description"]) {
    const value = input[key];
    if (typeof value === "string" && value.trim()) return value.trim();
  }
  return "";
}

export class ClaudeServer extends EventEmitter {
  constructor({
    cwd,
    resumeThreadId = "",
    command = "claude",
    args = [],
    environment = {},
    developerInstructions = "",
    model = DEFAULT_CLAUDE_MODEL,
  }) {
    super();
    this.cwd = cwd;
    this.command = command;
    this.args = args;
    this.environment = environment;
    this.resumeThreadId = resumeThreadId;
    this.developerInstructions = developerInstructions;
    this.model = model || DEFAULT_CLAUDE_MODEL;
    this.child = null;
    this.threadId = null;
    this.turnId = null;
    this.closed = false;
    this.restarting = false;
    this.turns = 0;
    this.controls = 0;
    this.handshakeId = "";
    this.announced = false;
    this.ready = null;
    // Tool calls in flight, so a tool result can complete the line the tool
    // call opened. Approvals in flight, so a y/a/n can find its request.
    this.items = new Map();
    this.approvals = new Map();
    // Text streaming state for the message being written right now.
    this.messageId = "";
    this.textItems = new Map();
    this.streamedText = false;
  }

  async connect() {
    return this.#launch(this.resumeThreadId);
  }

  // Codex starts a fresh thread inside one long-lived process; a headless
  // Claude process is bound to one session, so a new thread is a new process.
  // The conversation lives in Claude's own session file either way.
  async newThread() {
    return this.#launch("");
  }

  async resumeThread(threadId) {
    return this.#launch(threadId);
  }

  async startTurn(text) {
    if (!this.child) throw new Error("thread is not ready");
    this.turnId = `turn-${++this.turns}`;
    const turn = { id: this.turnId, status: "inProgress", items: [] };
    this.#send({
      type: "user",
      message: { role: "user", content: [{ type: "text", text }] },
      parent_tool_use_id: null,
      session_id: this.threadId,
    });
    this.emit("notification", { method: "turn/started", params: { turn } });
    return { turn };
  }

  async interrupt() {
    if (!this.child || !this.turnId) return;
    this.#send({
      type: "control_request",
      request_id: `ac-${++this.controls}`,
      request: { subtype: "interrupt" },
    });
  }

  // The interface answers an approval with the same four decisions it gives
  // Codex. `acceptForSession` takes the rules Claude itself suggests for the
  // call and pins them to the session, so nothing is written to a settings
  // file on disk.
  respond(id, result) {
    const approval = this.approvals.get(id);
    if (!approval) return;
    this.approvals.delete(id);
    const decision = result?.decision || "decline";
    let response;
    if (decision === "accept" || decision === "acceptForSession") {
      response = { behavior: "allow", updatedInput: approval.input };
      if (decision === "acceptForSession") {
        response.updatedPermissions = this.#sessionRules(approval);
      }
    } else {
      response = {
        behavior: "deny",
        message:
          decision === "cancel"
            ? "Cancelled in Easel."
            : "Denied in Easel.",
      };
    }
    this.#send({
      type: "control_response",
      response: { subtype: "success", request_id: id, response },
    });
    if (decision === "cancel") this.interrupt().catch(() => {});
  }

  reject(id, _code, message) {
    if (this.approvals.delete(id)) {
      this.#send({
        type: "control_response",
        response: {
          subtype: "success",
          request_id: id,
          response: { behavior: "deny", message: message || "Denied." },
        },
      });
      return;
    }
    this.#send({
      type: "control_response",
      response: { subtype: "error", request_id: id, error: message || "unsupported" },
    });
  }

  close() {
    if (this.closed) return;
    this.closed = true;
    this.child?.kill("SIGTERM");
  }

  #sessionRules({ tool, suggestions }) {
    const rules = (suggestions || [])
      .filter((suggestion) => suggestion?.type === "addRules" || suggestion?.type === "setMode")
      .map((suggestion) => ({ ...suggestion, destination: "session" }));
    if (rules.length) return rules;
    return [
      {
        type: "addRules",
        rules: [{ toolName: tool }],
        behavior: "allow",
        destination: "session",
      },
    ];
  }

  #launchArguments(resume) {
    const args = [
      "--print",
      "--input-format",
      "stream-json",
      "--output-format",
      "stream-json",
      // Text arrives token by token rather than a paragraph at a time, which
      // is what makes the transcript read like Codex's.
      "--include-partial-messages",
      "--verbose",
      "--model",
      this.model,
      "--permission-mode",
      "manual",
      "--permission-prompts",
      "host",
      "--permission-prompt-tool",
      "stdio",
      "--setting-sources",
      "",
      "--strict-mcp-config",
      "--disallowed-tools",
      ...WITHHELD_TOOLS,
      "--add-dir",
      this.cwd,
    ];
    if (this.developerInstructions) {
      args.push("--append-system-prompt", this.developerInstructions);
    }
    // The thread id is minted here rather than read back, so the interface
    // knows what to call this conversation before the first turn.
    if (resume) args.push("--resume", resume);
    else args.push("--session-id", this.threadId);
    // `args` is a prefix — a script for the command to run before its own
    // flags — which is how a test points this at a stand-in for the CLI.
    return [...this.args, ...args];
  }

  #launch(resume) {
    if (this.child) {
      this.restarting = true;
      this.child.kill("SIGTERM");
      this.child = null;
    }
    this.items.clear();
    this.approvals.clear();
    this.turnId = null;
    this.threadId = resume || randomUUID();
    this.announced = false;

    const child = spawn(this.command, this.#launchArguments(resume), {
      cwd: this.cwd,
      env: {
        ...process.env,
        ...this.environment,
        EASEL: "1",
        EASEL_VERSION: VERSION,
      },
      stdio: ["pipe", "pipe", "pipe"],
    });
    this.child = child;
    this.restarting = false;

    let settle;
    let fail;
    const ready = {
      promise: new Promise((resolve, reject) => {
        settle = resolve;
        fail = reject;
      }),
    };
    ready.resolve = settle;
    ready.reject = fail;
    this.ready = ready;

    child.once("error", (error) => {
      ready.reject(error);
      this.#fatal(error);
    });
    child.once("exit", (code, signal) => {
      if (child !== this.child) return; // Replaced by /new; not a failure.
      const suffix = signal ? ` (${signal})` : code === null ? "" : ` (${code})`;
      const error = new Error(`engine bridge closed${suffix}`);
      ready.reject(error);
      this.#fatal(error);
      this.emit("exit", { code, signal });
    });

    createInterface({ input: child.stdout }).on("line", (line) => {
      if (!line.trim()) return;
      try {
        this.#receive(JSON.parse(line));
      } catch (error) {
        this.emit("protocolError", new Error(`invalid engine message: ${error.message}`));
      }
    });
    createInterface({ input: child.stderr }).on("line", (line) => {
      if (line.trim()) this.emit("log", line.trim());
    });

    // The handshake that makes this process a host: it registers us for the
    // control channel the permission prompts come back on, and its answer is
    // the bridge saying it is ready.
    this.handshakeId = `ac-${++this.controls}`;
    this.#send({
      type: "control_request",
      request_id: this.handshakeId,
      request: { subtype: "initialize", hooks: {} },
    });

    return ready.promise;
  }

  #send(message) {
    if (!this.child?.stdin.writable) throw new Error("engine bridge is not writable");
    this.child.stdin.write(`${JSON.stringify(message)}\n`);
  }

  #fatal(error) {
    if (this.closed) return;
    this.emit("fatal", error);
  }

  #receive(message) {
    switch (message.type) {
      case "system":
        return this.#system(message);
      case "stream_event":
        return this.#streamEvent(message.event || {});
      case "assistant":
        return this.#assistant(message);
      case "user":
        return this.#toolResults(message);
      case "control_request":
        return this.#controlRequest(message);
      case "control_response":
        return this.#controlResponse(message);
      case "result":
        return this.#result(message);
      default:
    }
  }

  // The handshake is what says the bridge is up: `system/init` does not
  // arrive until the first turn begins, so waiting for it would deadlock a
  // session that has not been typed into yet.
  #controlResponse(message) {
    const response = message.response || {};
    if (response.request_id !== this.handshakeId) return;
    if (response.subtype === "error") {
      this.ready?.reject(new Error(response.error || "engine bridge refused the handshake"));
      return;
    }
    this.ready?.resolve({
      thread: { id: this.threadId, turns: [] },
      model: this.model,
      cwd: this.cwd,
    });
  }

  // `system/init` opens each turn, and carries what the CLI resolved the
  // requested model and session to. Say so once if either differs.
  #system(message) {
    if (message.subtype !== "init" || this.announced) return;
    this.announced = true;
    if (message.session_id) this.threadId = message.session_id;
    if (message.model && message.model !== this.model) {
      this.model = message.model;
      this.emit("notification", { method: "warning", params: { message: `Running ${message.model}` } });
    }
  }

  #streamEvent(event) {
    if (event.type === "message_start") {
      this.messageId = event.message?.id || `message-${Date.now()}`;
      this.textItems.clear();
      this.streamedText = false;
      return;
    }
    if (event.type === "content_block_start" && event.content_block?.type === "text") {
      this.textItems.set(event.index, `${this.messageId}:${event.index}`);
      return;
    }
    if (event.type === "content_block_delta" && event.delta?.type === "text_delta") {
      const itemId = this.textItems.get(event.index);
      if (!itemId) return;
      this.streamedText = true;
      this.emit("notification", {
        method: "item/agentMessage/delta",
        params: { itemId, delta: event.delta.text },
      });
    }
  }

  #assistant(message) {
    const content = message.message?.content || [];
    // A rate limit, a usage cap, or a refused model comes back as a synthetic
    // assistant message with no stream behind it. It reads as an error, not as
    // an answer — and it is the only explanation there will be: the turn that
    // follows reports `interrupted`, and an interrupted turn carries no error.
    // Dropping it left a session that simply stopped, for no stated reason.
    if (message.is_api_error_message) {
      const text = content
        .filter((block) => block.type === "text" && block.text)
        .map((block) => block.text)
        .join(" ")
        .trim();
      this.emit("notification", {
        method: "error",
        params: {
          error: { message: text || "the engine refused the turn" },
          // The turn's own result decides whether the session is done for; this
          // is the reason, not the verdict.
          willRetry: true,
        },
      });
      return;
    }
    let index = 0;
    for (const block of content) {
      if (block.type === "tool_use") {
        const item = this.#toolItem(block);
        this.items.set(block.id, item);
        this.emit("notification", { method: "item/started", params: { item } });
      } else if (block.type === "text" && block.text && !this.streamedText) {
        // Belt and braces: text that never streamed still reaches the screen.
        this.emit("notification", {
          method: "item/completed",
          params: {
            item: { id: `${message.message?.id || this.messageId}:t${index}`, type: "agentMessage", text: block.text },
          },
        });
      }
      index += 1;
    }
  }

  #toolItem(block) {
    const field = FILE_TOOLS.get(block.name);
    if (field) {
      const path = block.input?.[field];
      return { id: block.id, type: "fileChange", changes: path ? [{ path }] : [] };
    }
    if (COMMAND_TOOLS.has(block.name)) {
      return { id: block.id, type: "commandExecution", command: block.input?.command || block.name };
    }
    const detail = toolDetail(block.input);
    return { id: block.id, type: "dynamicToolCall", tool: detail ? `${block.name} · ${detail}` : block.name };
  }

  #toolResults(message) {
    for (const block of message.message?.content || []) {
      if (block.type !== "tool_result") continue;
      const item = this.items.get(block.tool_use_id);
      if (!item) continue;
      this.items.delete(block.tool_use_id);
      const failed = Boolean(block.is_error);
      const completed =
        item.type === "commandExecution"
          ? { ...item, exitCode: failed ? 1 : 0 }
          : { ...item, status: failed ? "failed" : "completed" };
      this.emit("notification", { method: "item/completed", params: { item: completed } });
    }
  }

  #controlRequest(message) {
    const request = message.request || {};
    if (request.subtype !== "can_use_tool") {
      this.#send({
        type: "control_response",
        response: {
          subtype: "error",
          request_id: message.request_id,
          error: `Easel does not support ${request.subtype} yet`,
        },
      });
      return;
    }
    const id = message.request_id;
    this.approvals.set(id, {
      tool: request.tool_name,
      input: request.input,
      suggestions: request.permission_suggestions || [],
    });
    const field = FILE_TOOLS.get(request.tool_name);
    if (field) {
      this.emit("request", {
        id,
        method: "item/fileChange/requestApproval",
        params: { reason: request.input?.[field] || request.description || request.tool_name },
      });
      return;
    }
    const detail = COMMAND_TOOLS.has(request.tool_name)
      ? request.input?.command
      : `${request.tool_name}${request.description ? ` · ${request.description}` : ""}`;
    this.emit("request", {
      id,
      method: "item/commandExecution/requestApproval",
      params: { command: detail || request.tool_name },
    });
  }

  #result(message) {
    const id = this.turnId || `turn-${this.turns}`;
    this.turnId = null;
    this.textItems.clear();
    const aborted = String(message.terminal_reason || "").startsWith("aborted");
    const status = aborted ? "interrupted" : message.is_error ? "failed" : "completed";
    const turn = { id, status, items: [] };
    if (status === "failed") {
      turn.error = { message: String(message.result || message.terminal_reason || "turn failed") };
    }
    this.emit("notification", { method: "turn/completed", params: { turn } });
  }
}
