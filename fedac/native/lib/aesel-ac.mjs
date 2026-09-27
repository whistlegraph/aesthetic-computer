// aesel-ac.mjs — the Aesthetic Computer engine bridge, for a piece.
//
// easel/src/ac-server.mjs without Node. The hosted engine needs no vendor
// binary: the piece POSTs to aesthetic.computer, which buys the inference and
// meters it against the device's @handle, and the reply is a server-sent-event
// stream in Anthropic's shape. Nothing runs the agent loop for us, so the loop
// lives here: send the conversation, read the stream, run the one tool the
// model may call, append the result, go round again until it stops asking.
//
// The transport is injected because the runtime's HTTP is not fetch(): it is
// curl started by system.fetchPost({ out }) and a file the piece re-reads once
// a frame. So the bridge is driven by tick(), not by promises — the piece calls
// tick() from paint, the bridge polls the transport, parses whatever new lines
// landed, and emits the same events the Claude bridge does.
//
// One tool, on purpose. The session exists to produce the next version of one
// piece; write_piece is that, and a wider file API would be a larger surface to
// secure and a larger prompt to pay for on a metered tier.

import { uuid as defaultUuid } from "./aesel-bridge.mjs";

export const SITE = "https://aesthetic.computer";
export const DEFAULT_AC_MODEL = "z-ai/glm-4.6";

// Names a person would type, mapped to what the endpoint allowlists. The
// server decides in the end; these exist so `/model glm` works.
export const AC_MODELS = {
  glm: "z-ai/glm-4.6",
  qwen: "qwen/qwen3-coder",
  deepseek: "deepseek/deepseek-chat-v3.1",
};

export function resolveAcModel(name) {
  return AC_MODELS[name] || name || DEFAULT_AC_MODEL;
}

// The short name for the header: "glm" rather than "z-ai/glm-4.6".
export function acModelLabel(model) {
  for (const [label, id] of Object.entries(AC_MODELS)) if (id === model) return label;
  return model.split("/").pop();
}

export const WRITE_PIECE = {
  name: "write_piece",
  description:
    "Write the complete new source of the session's piece. Always send the whole file, never a patch or a fragment — what you send replaces the file exactly. The person can run it on this machine as soon as it is saved, so prefer several small writes over one large one.",
  input_schema: {
    type: "object",
    properties: {
      source: { type: "string", description: "The entire contents of the piece file." },
      note: { type: "string", description: "One short line on what changed, for the person watching." },
    },
    required: ["source"],
  },
};

const MAX_ROUNDS = 12;
const MAX_TOKENS = 8192;

// Parse the complete `data:` lines of a server-sent-event stream from
// `offset` on. Returns the parsed payloads and where the next parse starts;
// a partial last line waits for more bytes.
export function parseSse(text, offset = 0) {
  const events = [];
  let cut = text.indexOf("\n", offset);
  while (cut !== -1) {
    const line = text.slice(offset, cut).trim();
    offset = cut + 1;
    cut = text.indexOf("\n", offset);
    if (!line.startsWith("data: ")) continue;
    const payload = line.slice(6);
    if (payload === "[DONE]") continue;
    try {
      events.push(JSON.parse(payload));
    } catch (_) {
      // A torn or foreign line is not ours to fail on.
    }
  }
  return { events, offset };
}

// The server's refusal, if the error body carries one.
export function errorMessage(body, fallback) {
  try {
    const parsed = JSON.parse(String(body || "").trim());
    if (parsed?.error?.message) return parsed.error.message;
  } catch (_) {}
  return fallback;
}

export class AcBridge {
  constructor({
    transport,        // { start(bodyJson, headers) → bool, poll() → { text, done, error }, cancel() }
    token = "",
    model = DEFAULT_AC_MODEL,
    instructions = "",
    file = "",
    readPiece = () => "",
    writePiece = () => false,
    uuid = defaultUuid,
  }) {
    this.transport = transport;
    this.token = token;
    this.model = resolveAcModel(model);
    this.instructions = instructions;
    this.file = file;
    this.readPiece = readPiece;
    this.writePiece = writePiece;
    this.mint = uuid;
    this.sessionId = "";
    this.events = [];
    this.messages = [];
    this.turns = 0;
    this.turnId = null;
    this.rounds = 0;
    this.round = null;
    this.ready = false;
  }

  // Same shape as the Claude bridge, so the piece can hold either. There is
  // no child to spawn; the arguments are empty.
  launch(resume = "") {
    this.sessionId = resume || this.mint();
    this.messages = [];
    this.turnId = null;
    this.round = null;
    this.ready = false;
    return [];
  }

  handshake() {
    if (!this.token) {
      this.#emit({ type: "error", fatal: true, message: "the aesthetic engine needs this device linked to a handle: type `link` at the prompt" });
      return;
    }
    this.ready = true;
    this.#emit({ type: "ready", model: this.model, session: this.sessionId });
  }

  get busy() {
    return this.turnId !== null;
  }

  // No approvals: the one tool writes the session's own piece.
  get pendingApproval() {
    return null;
  }

  approve() {
    return false;
  }

  feed() {}

  take() {
    const out = this.events;
    this.events = [];
    return out;
  }

  say(text) {
    this.turnId = `turn-${++this.turns}`;
    this.rounds = 0;
    this.messages.push({ role: "user", content: text });
    this.#emit({ type: "turn", status: "started", id: this.turnId });
    this.#startRound();
  }

  interrupt() {
    if (!this.turnId) return false;
    this.transport.cancel();
    this.round = null;
    const id = this.turnId;
    this.turnId = null;
    this.#emit({ type: "turn", status: "interrupted", id });
    return true;
  }

  // Called once a frame while a turn is in flight.
  tick() {
    if (!this.round) return;
    const { text = "", done = false, error = "" } = this.transport.poll() || {};
    const round = this.round;
    const parsed = parseSse(text, round.offset);
    round.offset = parsed.offset;
    for (const event of parsed.events) this.#event(round, event);
    if (round.failed) return; // the turn already ended inside #event
    if (!done) return;
    if (error) {
      this.#fail(errorMessage(text, error));
      return;
    }
    this.#finishRound(round);
  }

  #startRound() {
    const source = this.readPiece() || "";
    const system = [{ type: "text", text: this.instructions }];
    if (source) system.push({ type: "text", text: `Current source of ${this.file}:\n\n${source}` });
    const body = {
      model: this.model,
      system,
      messages: this.messages,
      tools: [WRITE_PIECE],
      max_tokens: MAX_TOKENS,
    };
    this.round = { offset: 0, text: "", blocks: [], partials: new Map(), stop: "end_turn", failed: false };
    const ok = this.transport.start(JSON.stringify(body), { Authorization: `Bearer ${this.token}` });
    if (!ok) this.#fail("another request is already in flight on this device; try again in a moment");
  }

  #event(round, event) {
    if (event.type === "content_block_start") {
      const block = event.content_block;
      if (block?.type === "tool_use") round.partials.set(event.index, { id: block.id, name: block.name, json: "" });
    } else if (event.type === "content_block_delta") {
      const delta = event.delta;
      if (delta?.type === "text_delta" && delta.text) {
        round.text += delta.text;
        this.#emit({ type: "delta", text: delta.text });
      } else if (delta?.type === "input_json_delta") {
        const partial = round.partials.get(event.index);
        if (partial) partial.json += delta.partial_json || "";
      }
    } else if (event.type === "content_block_stop") {
      const partial = round.partials.get(event.index);
      if (partial) {
        let input = {};
        try { input = JSON.parse(partial.json || "{}"); } catch (_) {}
        round.blocks.push({ type: "tool_use", id: partial.id, name: partial.name, input });
        round.partials.delete(event.index);
      }
    } else if (event.type === "message_delta") {
      if (event.delta?.stop_reason) round.stop = event.delta.stop_reason;
    } else if (event.type === "error") {
      round.failed = true;
      this.#fail(event.error?.message || "inference error");
    }
  }

  #finishRound(round) {
    this.round = null;
    const assistant = [];
    if (round.text) assistant.push({ type: "text", text: round.text });
    for (const block of round.blocks) assistant.push(block);
    if (assistant.length) this.messages.push({ role: "assistant", content: assistant });

    if (round.stop !== "tool_use" || !round.blocks.length) {
      const id = this.turnId;
      this.turnId = null;
      this.#emit({ type: "turn", status: "completed", id });
      return;
    }

    const results = round.blocks.map((block) => this.#runTool(block));
    this.messages.push({ role: "user", content: results });
    // Bounded: a model that loops is a model spending someone's daily budget on a loop.
    if (++this.rounds >= MAX_ROUNDS) {
      this.#fail(`stopped after ${MAX_ROUNDS} tool rounds`);
      return;
    }
    this.#startRound();
  }

  #runTool(block) {
    const note = String(block.input?.note || "").trim();
    const label = note ? `${this.file} · ${note}` : this.file;
    if (block.name !== "write_piece") {
      this.#emit({ type: "tool", status: "failed", id: block.id, kind: "call", label: block.name, path: "" });
      return { type: "tool_result", tool_use_id: block.id, is_error: true, content: `No tool named ${block.name}. The only tool is write_piece.` };
    }
    const source = block.input?.source;
    if (typeof source !== "string" || !source.trim()) {
      this.#emit({ type: "tool", status: "failed", id: block.id, kind: "file", label, path: this.file });
      return { type: "tool_result", tool_use_id: block.id, is_error: true, content: "write_piece needs the complete source of the file." };
    }
    this.#emit({ type: "tool", status: "started", id: block.id, kind: "file", label, path: this.file });
    const ok = this.writePiece(source.endsWith("\n") ? source : `${source}\n`);
    if (!ok) {
      this.#emit({ type: "tool", status: "failed", id: block.id, kind: "file", label, path: this.file });
      return { type: "tool_result", tool_use_id: block.id, is_error: true, content: `Could not write ${this.file}.` };
    }
    this.#emit({ type: "tool", status: "completed", id: block.id, kind: "file", label, path: this.file });
    return { type: "tool_result", tool_use_id: block.id, content: "Saved. The person can run it on this machine now." };
  }

  #fail(message) {
    this.round = null;
    const id = this.turnId;
    this.turnId = null;
    this.#emit({ type: "turn", status: "failed", id, error: message });
  }

  #emit(event) {
    this.events.push(event);
  }
}
