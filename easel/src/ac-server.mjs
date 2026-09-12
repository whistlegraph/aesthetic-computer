// The Aesthetic Computer bridge — inference without a vendor CLI.
//
// The other two bridges spawn `claude` or `codex` and speak a line protocol to
// a subprocess. That is why an installed Easel does nothing for someone holding
// neither subscription: the interface is complete and there is no engine under
// it. This bridge talks HTTP to aesthetic.computer instead, which buys the
// inference on its own account and meters it against the caller's @handle. An
// install needs a handle and nothing else.
//
// Two consequences follow from there being no vendor CLI, and both shape this
// file more than the transport does.
//
// The agent loop lives here. A CLI runs its own loop — call the model, execute
// the tools it asks for, feed the results back, repeat — and hands the
// interface a finished turn. Nothing is running that loop for us, so `startTurn`
// is that loop: stream a response, run any tool the model called, append the
// result, and go round again until it stops asking.
//
// And the context has to travel in the prompt. The other bridges name the
// Aesthetic Computer guides and let the model open them, because a CLI has file
// tools. This one has no file tools, so naming a path would be telling the model
// about a document it cannot read. The bundled guides are inlined instead — the
// same 24 KB that ships in easel/context, spent once per thread as cached
// prefix rather than fetched per question.
//
// The tool set is deliberately one tool. Easel is an editor for one piece, and a
// turn's whole job is to produce that piece's next version. A general file API
// would be a larger surface to secure, a larger prompt to pay for, and no closer
// to what the session is for. `write_piece` is what the loop exists to serve.

import { EventEmitter } from "node:events";
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { randomUUID } from "node:crypto";

const ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
const SITE = process.env.EASEL_SITE || "https://aesthetic.computer";

export const DEFAULT_AC_MODEL = "z-ai/glm-4.6";

// Names a person would type, mapped to what the endpoint allowlists. The server
// decides in the end; these exist so `/model glm` works.
export const AC_MODELS = {
  glm: "z-ai/glm-4.6",
  qwen: "qwen/qwen3-coder",
  deepseek: "deepseek/deepseek-chat-v3.1",
};

// The guides, in the order a model should meet them: what a piece is, then how
// it draws, then how the code should read. KidLisp last because most sessions
// are JavaScript and it is the longest.
const CONTEXT_FILES = ["pieces.md", "screen.md", "hand.md", "kidlisp.md"];

function bundledContext() {
  const parts = [];
  for (const name of CONTEXT_FILES) {
    const path = join(ROOT, "context", name);
    if (!existsSync(path)) continue;
    try {
      parts.push(`# ${name}\n\n${readFileSync(path, "utf8")}`);
    } catch {}
  }
  if (!parts.length) return "";
  return [
    "Here are the Aesthetic Computer guides. They are the house rules for a",
    "piece and they win over your own defaults.",
    "",
    parts.join("\n\n---\n\n"),
  ].join("\n");
}

const WRITE_PIECE = {
  name: "write_piece",
  description:
    "Write the complete new source of the session's piece. Always send the whole file, never a patch or a fragment — what you send replaces the file exactly. Saving pushes it live to anyone watching, so prefer several small writes over one large one.",
  input_schema: {
    type: "object",
    properties: {
      source: { type: "string", description: "The entire contents of the piece file." },
      note: { type: "string", description: "One short line on what changed, for the person watching." },
    },
    required: ["source"],
  },
};

export class AcServer extends EventEmitter {
  constructor({
    cwd = process.cwd(),
    resumeThreadId = "",
    model = DEFAULT_AC_MODEL,
    developerInstructions = "",
    // How the bridge reaches the piece on disk and the token that pays for the
    // turn. Both are injected so this file can be tested without either.
    piece = null,
    token = null,
    fetch = globalThis.fetch,
    site = SITE,
  } = {}) {
    super();
    this.cwd = cwd;
    this.model = AC_MODELS[model] || model || DEFAULT_AC_MODEL;
    this.developerInstructions = developerInstructions;
    this.piece = piece;
    this.token = token;
    this.fetch = fetch;
    this.site = site;
    this.threadId = resumeThreadId || "";
    this.turnId = null;
    this.turns = 0;
    // The conversation. Held here because there is no process holding it for us
    // — closing Easel loses it, which is honest: nothing was written anywhere.
    this.messages = [];
    this.controller = null;
  }

  get #system() {
    const context = bundledContext();
    return [this.developerInstructions, context].filter(Boolean).join("\n\n");
  }

  async connect() {
    if (!this.threadId) this.threadId = randomUUID();
    return { model: this.model };
  }

  async newThread() {
    this.threadId = randomUUID();
    this.messages = [];
    return { model: this.model };
  }

  async resumeThread(threadId) {
    // Honest about what it cannot do: the conversation lived in this process.
    this.threadId = threadId || randomUUID();
    return { model: this.model };
  }

  async interrupt() {
    this.controller?.abort();
  }

  // No approvals: the one tool writes the session's own piece, which is the
  // thing the user asked for. Kept so the interface can treat every bridge the
  // same way.
  respond() {}
  reject() {}

  close() {
    this.controller?.abort();
    this.controller = null;
  }

  async startTurn(text) {
    this.turnId = `turn-${++this.turns}`;
    const turn = { id: this.turnId, status: "inProgress", items: [] };
    this.emit("notification", { method: "turn/started", params: { turn } });

    this.messages.push({ role: "user", content: text });

    try {
      // Round and round until the model stops asking for tools. Bounded because
      // a model that loops is a model spending someone's daily budget on a loop.
      for (let round = 0; round < 12; round += 1) {
        const result = await this.#round();
        if (result.stop !== "tool_use") {
          this.emit("notification", {
            method: "turn/completed",
            params: { turn: { ...turn, status: "completed" } },
          });
          return { turn };
        }
      }
      this.emit("notification", {
        method: "turn/completed",
        params: { turn: { ...turn, status: "failed", error: { message: "Stopped after 12 tool rounds." } } },
      });
    } catch (error) {
      const aborted = error?.name === "AbortError";
      this.emit("notification", {
        method: "turn/completed",
        params: {
          turn: {
            ...turn,
            status: aborted ? "interrupted" : "failed",
            error: aborted ? undefined : { message: error.message },
          },
        },
      });
    }
    return { turn };
  }

  // One request, streamed. Returns why the model stopped.
  async #round() {
    this.controller = new AbortController();
    const token = await this.token?.();
    if (!token) {
      throw new Error("Hosted inference needs an Aesthetic Computer handle — run /login.");
    }

    const response = await this.fetch(`${this.site}/api/easel-inference`, {
      method: "POST",
      signal: this.controller.signal,
      headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}` },
      body: JSON.stringify({
        model: this.model,
        system: this.#system,
        messages: this.messages,
        tools: [WRITE_PIECE],
        max_tokens: 8192,
      }),
    });

    if (!response.ok) {
      let message = `inference failed (HTTP ${response.status})`;
      try {
        const body = await response.json();
        if (body?.error?.message) message = body.error.message;
      } catch {}
      throw new Error(message);
    }

    const messageId = `msg-${this.turns}-${Date.now()}`;
    const blocks = [];
    let stop = "end_turn";
    let text = "";
    // Tool arguments arrive as a JSON string in fragments, so they are gathered
    // per block index and parsed only once the block closes.
    const partials = new Map();

    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    let tail = "";

    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;
      tail += decoder.decode(value, { stream: true });
      let cut = tail.indexOf("\n");
      while (cut !== -1) {
        const line = tail.slice(0, cut).trim();
        tail = tail.slice(cut + 1);
        cut = tail.indexOf("\n");
        if (!line.startsWith("data: ")) continue;
        const payload = line.slice(6);
        if (payload === "[DONE]") continue;
        let event;
        try {
          event = JSON.parse(payload);
        } catch {
          continue;
        }

        if (event.type === "content_block_start") {
          const block = event.content_block;
          if (block?.type === "tool_use") {
            partials.set(event.index, { id: block.id, name: block.name, json: "" });
          }
        } else if (event.type === "content_block_delta") {
          const delta = event.delta;
          if (delta?.type === "text_delta" && delta.text) {
            text += delta.text;
            this.emit("notification", {
              method: "item/agentMessage/delta",
              params: { itemId: messageId, delta: delta.text },
            });
          } else if (delta?.type === "input_json_delta") {
            const partial = partials.get(event.index);
            if (partial) partial.json += delta.partial_json || "";
          }
        } else if (event.type === "content_block_stop") {
          const partial = partials.get(event.index);
          if (partial) {
            let input = {};
            try {
              input = JSON.parse(partial.json || "{}");
            } catch {}
            blocks.push({ type: "tool_use", id: partial.id, name: partial.name, input });
            partials.delete(event.index);
          }
        } else if (event.type === "message_delta") {
          if (event.delta?.stop_reason) stop = event.delta.stop_reason;
        } else if (event.type === "error") {
          throw new Error(event.error?.message || "inference error");
        }
      }
    }

    if (text) {
      this.emit("notification", {
        method: "item/completed",
        params: { item: { id: messageId, type: "agentMessage", text } },
      });
    }

    const assistant = [];
    if (text) assistant.push({ type: "text", text });
    for (const block of blocks) assistant.push(block);
    if (assistant.length) this.messages.push({ role: "assistant", content: assistant });

    if (stop !== "tool_use" || !blocks.length) return { stop: "end_turn" };

    const results = [];
    for (const block of blocks) {
      results.push(await this.#runTool(block));
    }
    this.messages.push({ role: "user", content: results });
    return { stop: "tool_use" };
  }

  async #runTool(block) {
    const itemId = `tool-${block.id}`;
    const note = String(block.input?.note || "").trim();
    this.emit("notification", {
      method: "item/started",
      params: { item: { id: itemId, type: "fileChange", path: this.piece?.file || "piece", summary: note } },
    });

    if (block.name !== "write_piece") {
      this.emit("notification", {
        method: "item/completed",
        params: { item: { id: itemId, type: "fileChange", path: block.name, status: "unknown tool" } },
      });
      return {
        type: "tool_result",
        tool_use_id: block.id,
        is_error: true,
        content: `No tool named ${block.name}. The only tool is write_piece.`,
      };
    }

    const source = block.input?.source;
    if (typeof source !== "string" || !source.trim()) {
      return {
        type: "tool_result",
        tool_use_id: block.id,
        is_error: true,
        content: "write_piece needs the complete source of the file.",
      };
    }

    try {
      const file = this.piece?.file;
      if (!file) throw new Error("no piece is open in this session");
      writeFileSync(file, source.endsWith("\n") ? source : `${source}\n`);
      this.emit("notification", {
        method: "item/completed",
        params: { item: { id: itemId, type: "fileChange", path: file, status: note || "written" } },
      });
      // The watcher pushes it live and auto-publish takes it from there, so the
      // model is told it landed rather than told to publish.
      return {
        type: "tool_result",
        tool_use_id: block.id,
        content: "Saved. It is live for anyone watching, and published if auto-publish is on.",
      };
    } catch (error) {
      this.emit("notification", {
        method: "item/completed",
        params: { item: { id: itemId, type: "fileChange", path: "piece", status: `failed: ${error.message}` } },
      });
      return {
        type: "tool_result",
        tool_use_id: block.id,
        is_error: true,
        content: `Could not write the piece: ${error.message}`,
      };
    }
  }
}
