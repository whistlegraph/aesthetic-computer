// The Aesthetic Computer bridge — inference without a vendor CLI.
//
// The other two bridges spawn `claude` or `codex` and speak a line protocol to
// a subprocess. That is why an installed Aesel does nothing for someone holding
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
// The tool set is deliberately one tool. Aesel is an editor for one piece, and a
// turn's whole job is to produce that piece's next version. A general file API
// would be a larger surface to secure, a larger prompt to pay for, and no closer
// to what the session is for. `write_piece` is what the loop exists to serve.

import { EventEmitter } from "node:events";
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { validatePieceSource } from "./revisions.mjs";
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
  sonnet: "anthropic/claude-sonnet-4.6",
  gpt: "openai/gpt-5.4",
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
    "Write the complete new source of the session's piece. Always send the whole file, never a patch or a fragment — what you send replaces the file exactly. Saving pushes it live to anyone watching. Build the request in several small, complete working checkpoints: send each checkpoint as a separate write_piece call as soon as it is ready, then continue improving it. Never send unfinished syntax.",
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
    this.model = (Object.hasOwn(AC_MODELS, model) ? AC_MODELS[model] : model) || DEFAULT_AC_MODEL;
    this.developerInstructions = developerInstructions;
    this.piece = piece;
    this.token = token;
    this.fetch = fetch;
    this.site = site;
    this.threadId = resumeThreadId || "";
    this.turnId = null;
    this.turns = 0;
    // The conversation. Held here because there is no process holding it for us
    // — closing Aesel loses it, which is honest: nothing was written anywhere.
    this.messages = [];
    this.controller = null;
  }

  // The system prompt, as blocks rather than a string, so the guides can be
  // marked cacheable.
  //
  // This matters more than it looks. The bundle is about six thousand tokens and
  // it is re-sent on every round of the tool loop — a first measurement spent
  // 6,786 tokens answering "fill the screen with red", which at a 25,000-token
  // day is three questions. Cached, that prefix is read at roughly a tenth the
  // price and the same day holds dozens.
  //
  // The order is deliberate: the guides are identical for every session and go
  // first, so the cache breakpoint falls after them and a changing instruction
  // line cannot invalidate the expensive half.
  get #system() {
    const blocks = [];
    const context = bundledContext();
    if (context) {
      blocks.push({
        type: "text",
        text: context,
        cache_control: { type: "ephemeral" },
      });
    }
    if (this.developerInstructions) {
      blocks.push({ type: "text", text: this.developerInstructions });
    }
    if (this.piece?.file && existsSync(this.piece.file)) {
      blocks.push({ type: "text", text: `Current piece (${this.piece.file}); preserve the user's existing work unless asked to change it:\n\n${readFileSync(this.piece.file, "utf8")}` });
    }
    return blocks;
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
    const controller = this.controller = new AbortController();
    this.emit("notification", { method: "turn/progress", params: { phase: "connecting" } });
    const token = await this.token?.();
    if (!token) {
      throw new Error("Hosted inference needs an Aesthetic Computer handle — run /login.");
    }

    const response = await this.fetch(`${this.site}/api/easel-inference`, {
      method: "POST",
      signal: controller.signal,
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

    this.emit("notification", { method: "turn/progress", params: { phase: "waiting" } });
    const messageId = `msg-${this.turns}-${Date.now()}`;
    const blocks = [];
    const results = [];
    let received = 0;
    let finished = false;
    let stop = "end_turn";
    let text = "";
    // Tool arguments arrive as a JSON string in fragments, so they are gathered
    // per block index and parsed only once the block closes.
    const partials = new Map();
    // What this round cost. The counts arrive split across two events —
    // `message_start` knows the prompt, `message_delta` knows the answer — and
    // each is cumulative for its own field, so later values replace rather than
    // add. The interface turns this into watt-hours; see energy.mjs.
    const usage = {};

    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    let tail = "";

    try {
      for (;;) {
        controller.signal.throwIfAborted();
        const { done, value } = await reader.read();
        controller.signal.throwIfAborted();
        if (done) break;
        received += value.byteLength;
        tail += decoder.decode(value, { stream: true });
        let cut = tail.indexOf("\n");
        while (cut !== -1) {
          const line = tail.slice(0, cut).trim();
          tail = tail.slice(cut + 1);
          cut = tail.indexOf("\n");
          if (!line.startsWith("data:")) continue;
          const payload = line.slice(5).trimStart();
          if (payload === "[DONE]") continue;
          let event;
          try {
            event = JSON.parse(payload);
          } catch {
            continue;
          }

          const counts = event.usage || event.message?.usage;
          if (counts) Object.assign(usage, counts);

          if (event.type === "content_block_start" || event.type === "content_block_delta") {
            this.emit("notification", { method: "turn/progress", params: {
              phase: event.delta?.type === "input_json_delta" || event.content_block?.type === "tool_use" ? "composing" : "generating",
              bytes: received,
            } });
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
              const block = { type: "tool_use", id: partial.id, name: partial.name, input };
              blocks.push(block);
              // A complete tool block is a checkpoint; do not wait for the next
              // explanation or the end of this response before showing it.
              results.push(await this.#runTool(block));
              partials.delete(event.index);
            }
          } else if (event.type === "message_delta") {
            if (event.delta?.stop_reason) { stop = event.delta.stop_reason; finished = true; }
          } else if (event.type === "error") {
            throw new Error(event.error?.message || "inference error");
          }
        }
      }

      if (!finished || partials.size) throw new Error("Inference stream ended before the response completed. Saved checkpoints are preserved.");
    } finally {
      await reader.cancel?.().catch(() => {});
      reader.releaseLock?.();
    }

    // Reported per round rather than per turn: a turn that called a tool paid
    // for two responses, and a readout that showed one of them would understate
    // the expensive kind of turn.
    if (Object.keys(usage).length) {
      this.emit("notification", {
        method: "turn/usage",
        params: { model: this.model, usage },
      });
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

    this.messages.push({ role: "user", content: results });
    return { stop: "tool_use" };
  }

  async #runTool(block) {
    const signal = this.controller?.signal;
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
      this.emit("notification", { method: "turn/progress", params: { phase: "writing" } });
      await validatePieceSource(source, file);
      signal?.throwIfAborted();
      writeFileSync(file, source.endsWith("\n") ? source : `${source}\n`);
      await this.piece?.checkpoint?.();
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
