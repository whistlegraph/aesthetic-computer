// aesel-bridge.mjs — the Claude engine bridge, for a piece.
//
// This is easel/src/claude-server.mjs with the process taken out. Easel's
// bridge owns a child process and an event emitter; a native piece has
// neither. What it has is a raw PTY that hands it whole lines once a frame
// (system.pty2.lines) and a write() for lines going the other way. So this
// module is the translator alone: feed it the lines Claude prints, it queues
// plain events for the piece to draw, and it hands back the lines to send.
//
// The protocol is Claude Code's headless stream-json —
// `claude --print --input-format stream-json --output-format stream-json` —
// and the two approval flags are load-bearing for the same reasons they are
// in Easel: `--permission-prompt-tool stdio` makes this process the one that
// answers permission prompts (without it every prompt is auto-denied and the
// model narrates that it lacks permission), and `--setting-sources ""` keeps
// the account's allow-lists, hooks and MCP servers out of a session where the
// person at the keyboard should be the only thing that can approve a command.
//
// Pure, so `node --test` can drive it with a scripted transcript. Nothing in
// here touches the runtime; the piece does the spawning, reading and writing.

export const DEFAULT_MODEL = "claude-opus-5";

// Tools whose work is a file change, and the input field naming the file.
const FILE_TOOLS = new Map([
  ["Write", "file_path"],
  ["Edit", "file_path"],
  ["MultiEdit", "file_path"],
  ["NotebookEdit", "notebook_path"],
]);

const COMMAND_TOOLS = new Set(["Bash", "BashOutput", "KillShell"]);

// Tools this surface has no room for: two that leave the machine on their
// own, and one that hides a whole second conversation behind a single line.
export const WITHHELD_TOOLS = ["WebFetch", "WebSearch", "Task"];

// A short second line for a tool that is neither a command nor a file change.
function toolDetail(input = {}) {
  for (const key of ["pattern", "query", "path", "file_path", "url", "description"]) {
    const value = input[key];
    if (typeof value === "string" && value.trim()) return value.trim();
  }
  return "";
}

// A v4 UUID from whatever random source the runtime has. QuickJS has no
// crypto module; Math.random is what a session id needs here — it names the
// conversation on this machine, it does not secure anything.
export function uuid(random = Math.random) {
  const hex = "0123456789abcdef";
  let out = "";
  for (let i = 0; i < 36; i++) {
    if (i === 8 || i === 13 || i === 18 || i === 23) out += "-";
    else if (i === 14) out += "4";
    else if (i === 19) out += hex[8 + Math.floor(random() * 4)];
    else out += hex[Math.floor(random() * 16)];
  }
  return out;
}

// Easel's pronounceable piece names, without node:crypto.
const CONSONANTS = "bdfgklmnprstvz";
const VOWELS = "aeiou";
export function randomSlug(random = Math.random) {
  const count = 2 + (random() < 0.5 ? 0 : 1);
  let name = "";
  for (let i = 0; i < count; i++) {
    name += CONSONANTS[Math.floor(random() * CONSONANTS.length)];
    name += VOWELS[Math.floor(random() * VOWELS.length)];
  }
  return name;
}

// The blank a session opens on. Flat colour, nothing else, so the first edit
// the agent makes is visible as a change rather than a replacement.
export function blankPiece(slug, stamp = new Date().toISOString().slice(0, 10)) {
  return [
    `// ${slug}, ${stamp}`,
    "// A blank Aesthetic Computer piece, on AC Native.",
    "",
    "function paint({ wipe }) {",
    "  wipe(70, 50, 100);",
    "}",
    "",
    "export { paint };",
    "",
  ].join("\n");
}

// What the agent is told about where it is. The native API is the web
// piece API's small cousin, and the difference is the whole point of saying
// anything: an agent that assumes the browser writes `fetch` and `document`
// into a piece that will run on a framebuffer.
export function instructionsFor({ handle = "", slug, file, screen = { width: 455, height: 256 }, writeTool = false }) {
  const account = handle
    ? `The user is @${handle}, signed in on this device.`
    : "The device has no handle; the user is anonymous.";
  return [
    "You are running inside Aesel on an Aesthetic Computer Native device: a laptop booted into AC Native OS (a small Linux, QuickJS, software framebuffer, no browser, no Node.js).",
    account,
    `This session's piece is ${file}. It already exists as a blank piece that paints a flat colour and nothing else. Edit that file unless the user asks for something else.`,
    "The user runs the piece on this machine with one key, so small frequent edits are better than one big rewrite.",
    "A native piece is an ES module exporting lifecycle functions: boot({ system, screen, params }), paint({ wipe, ink, box, line, circle, plot, write, screen, paintCount, sound, wifi, system }), act({ event, system, sound }), sim(), leave(). paint runs every frame at 60 fps; keep state in module variables.",
    `The screen is small: about ${screen.width}x${screen.height} logical pixels. Read screen.width and screen.height rather than assuming.`,
    'Graphics: wipe(r, g, b) clears; ink(r, g, b) sets the colour for what follows; box(x, y, w, h) fills, box(x, y, w, h, "outline") strokes; line(x1, y1, x2, y2); circle(x, y, r, filled); plot(x, y). write(text, { x, y, size, font }) with font "6x10" (glyphs 6 wide, 10 tall) or "unifont". There is no DOM, no fetch(), no npm, no canvas context.',
    'Input, in act: event.is("keyboard:down") with event.key ("a", "space", "enter", "backspace", "arrowup", ...), event.is("keyboard:down:shift") for modifiers, and event.is("touch"), event.is("draw"), event.is("lift") with event.x and event.y. Escape returns the user to the prompt on its own; do not handle it.',
    'Sound: sound.synth({ type: "sine" | "square" | "triangle" | "sawtooth", tone: hertz, duration: seconds, volume: 0..1, attack, decay }).',
    writeTool
      ? "You cannot read or list files here. The piece's current source is given to you in this prompt. Your only tool is write_piece: send the complete new source of the file every time, never a patch."
      : "The other files in /pieces are the reference for this API. Read chat.mjs or clock.mjs before guessing at a call; do not edit them.",
    "Do not write the piece's name onto the screen. Do not try to run the piece with node or a browser; the device runs it.",
  ].join("\n");
}

// The command line for the bridge child. `cwd` is where the child runs and
// where the piece lives; the session id is minted by the caller so the
// conversation has a name before its first turn.
export function launchArguments({ model = DEFAULT_MODEL, cwd, instructions = "", sessionId, resume = "" }) {
  const args = [
    "--print",
    "--input-format", "stream-json",
    "--output-format", "stream-json",
    "--include-partial-messages",
    "--verbose",
    "--model", model,
    "--permission-mode", "manual",
    "--permission-prompts", "host",
    "--permission-prompt-tool", "stdio",
    "--setting-sources", "",
    "--strict-mcp-config",
    "--disallowed-tools", ...WITHHELD_TOOLS,
    "--add-dir", cwd,
  ];
  if (instructions) args.push("--append-system-prompt", instructions);
  if (resume) args.push("--resume", resume);
  else args.push("--session-id", sessionId);
  return args;
}

// A line from the child may carry a spinner or an escape sequence ahead of
// its JSON when the CLI thought it had a terminal. Everything before the
// first brace is the CLI talking; the brace onward is addressed to us.
function splitLine(line) {
  const trimmed = line.trim();
  if (!trimmed) return null;
  const brace = trimmed.indexOf("{");
  if (brace < 0) return { log: trimmed };
  const json = trimmed.slice(brace);
  try {
    return { message: JSON.parse(json), log: brace > 0 ? trimmed.slice(0, brace).trim() : "" };
  } catch (error) {
    if (brace === 0) return { protocolError: `invalid engine message: ${error.message}` };
    return { log: trimmed };
  }
}

export class Bridge {
  constructor({ send, model = DEFAULT_MODEL, cwd, instructions = "", uuid: mint = uuid }) {
    this.send = send; // (line: string) => void — one JSON line, no newline
    this.model = model;
    this.cwd = cwd;
    this.instructions = instructions;
    this.mint = mint;
    this.sessionId = "";
    this.events = [];
    this.turns = 0;
    this.controls = 0;
    this.turnId = null;
    this.handshakeId = "";
    this.ready = false;
    this.announced = false;
    this.items = new Map();     // tool_use id → item, so a result completes its line
    this.approvals = new Map(); // request id → {tool, input, suggestions}
    this.messageId = "";
    this.textBlocks = new Set();
    this.streamedText = false;
  }

  // Arguments for the child, and a fresh session id. Call before spawning;
  // then, once the child is up, handshake().
  launch(resume = "") {
    this.sessionId = resume || this.mint();
    this.items.clear();
    this.approvals.clear();
    this.turnId = null;
    this.ready = false;
    this.announced = false;
    return launchArguments({
      model: this.model,
      cwd: this.cwd,
      instructions: this.instructions,
      sessionId: this.sessionId,
      resume,
    });
  }

  // The control handshake that registers this process as the host of the
  // permission channel. Its answer is the bridge saying it is ready.
  handshake() {
    this.handshakeId = `ac-${++this.controls}`;
    this.#send({ type: "control_request", request_id: this.handshakeId, request: { subtype: "initialize", hooks: {} } });
  }

  get busy() {
    return this.turnId !== null;
  }

  get pendingApproval() {
    for (const [id, approval] of this.approvals) return { id, ...approval };
    return null;
  }

  say(text) {
    this.turnId = `turn-${++this.turns}`;
    this.#send({
      type: "user",
      message: { role: "user", content: [{ type: "text", text }] },
      parent_tool_use_id: null,
      session_id: this.sessionId,
    });
    this.#emit({ type: "turn", status: "started", id: this.turnId });
  }

  interrupt() {
    if (!this.turnId) return false;
    this.#send({ type: "control_request", request_id: `ac-${++this.controls}`, request: { subtype: "interrupt" } });
    return true;
  }

  // y → accept once, a → accept for the session, n → decline. Accepting for
  // the session pins the rules Claude suggested for the call to this session
  // alone; nothing is written to a settings file.
  approve(id, decision) {
    const approval = this.approvals.get(id);
    if (!approval) return false;
    this.approvals.delete(id);
    let response;
    if (decision === "accept" || decision === "acceptForSession") {
      response = { behavior: "allow", updatedInput: approval.input };
      if (decision === "acceptForSession") response.updatedPermissions = sessionRules(approval);
    } else {
      response = { behavior: "deny", message: "Denied in Aesel." };
    }
    this.#send({ type: "control_response", response: { subtype: "success", request_id: id, response } });
    this.#emit({ type: "approval-resolved", id, decision });
    return true;
  }

  // Take the events queued since the last take.
  take() {
    const out = this.events;
    this.events = [];
    return out;
  }

  // One line from the child's stdout.
  feed(line) {
    const parts = splitLine(line);
    if (!parts) return;
    if (parts.log) this.#emit({ type: "log", line: parts.log });
    if (parts.protocolError) this.#emit({ type: "error", message: parts.protocolError });
    if (parts.message) this.#receive(parts.message);
  }

  #send(message) {
    this.send(JSON.stringify(message));
  }

  #emit(event) {
    this.events.push(event);
  }

  #receive(message) {
    switch (message.type) {
      case "control_response": return this.#controlResponse(message);
      case "system": return this.#system(message);
      case "stream_event": return this.#streamEvent(message.event || {});
      case "assistant": return this.#assistant(message);
      case "user": return this.#toolResults(message);
      case "control_request": return this.#controlRequest(message);
      case "result": return this.#result(message);
      default:
    }
  }

  #controlResponse(message) {
    const response = message.response || {};
    if (response.request_id !== this.handshakeId) return;
    if (response.subtype === "error") {
      this.#emit({ type: "error", message: response.error || "engine bridge refused the handshake", fatal: true });
      return;
    }
    this.ready = true;
    this.#emit({ type: "ready", model: this.model, session: this.sessionId });
  }

  // `system/init` opens each turn with what the CLI resolved the model and
  // session to. Say so once if the model differs.
  #system(message) {
    if (message.subtype !== "init" || this.announced) return;
    this.announced = true;
    if (message.session_id) this.sessionId = message.session_id;
    if (message.model && message.model !== this.model) {
      this.model = message.model;
      this.#emit({ type: "warning", message: `Running ${message.model}` });
    }
  }

  #streamEvent(event) {
    if (event.type === "message_start") {
      this.messageId = event.message?.id || `message-${this.turns}`;
      this.textBlocks.clear();
      this.streamedText = false;
      return;
    }
    if (event.type === "content_block_start" && event.content_block?.type === "text") {
      this.textBlocks.add(event.index);
      return;
    }
    if (event.type === "content_block_delta" && event.delta?.type === "text_delta") {
      if (!this.textBlocks.has(event.index)) return;
      this.streamedText = true;
      this.#emit({ type: "delta", text: event.delta.text });
    }
  }

  #assistant(message) {
    const content = message.message?.content || [];
    // A rate limit, a usage cap, or a refused model arrives as a synthetic
    // assistant message with no stream behind it; it is the only explanation
    // the session will get, since the turn that follows only says "interrupted".
    if (message.is_api_error_message) {
      const text = content.filter((b) => b.type === "text" && b.text).map((b) => b.text).join(" ").trim();
      this.#emit({ type: "error", message: text || "the engine refused the turn" });
      return;
    }
    for (const block of content) {
      if (block.type === "tool_use") {
        const item = toolItem(block);
        this.items.set(block.id, item);
        this.#emit({ type: "tool", status: "started", ...item });
      } else if (block.type === "text" && block.text && !this.streamedText) {
        // Text that never streamed still reaches the screen.
        this.#emit({ type: "message", text: block.text });
      }
    }
  }

  #toolResults(message) {
    for (const block of message.message?.content || []) {
      if (block.type !== "tool_result") continue;
      const item = this.items.get(block.tool_use_id);
      if (!item) continue;
      this.items.delete(block.tool_use_id);
      this.#emit({ type: "tool", status: block.is_error ? "failed" : "completed", ...item });
    }
  }

  #controlRequest(message) {
    const request = message.request || {};
    if (request.subtype !== "can_use_tool") {
      this.#send({
        type: "control_response",
        response: { subtype: "error", request_id: message.request_id, error: `Aesel does not support ${request.subtype} yet` },
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
      this.#emit({ type: "approval", id, kind: "file", label: request.input?.[field] || request.description || request.tool_name });
      return;
    }
    const label = COMMAND_TOOLS.has(request.tool_name)
      ? request.input?.command
      : `${request.tool_name}${request.description ? ` · ${request.description}` : ""}`;
    this.#emit({ type: "approval", id, kind: "command", label: label || request.tool_name });
  }

  #result(message) {
    const id = this.turnId || `turn-${this.turns}`;
    this.turnId = null;
    this.textBlocks.clear();
    const aborted = String(message.terminal_reason || "").startsWith("aborted");
    const status = aborted ? "interrupted" : message.is_error ? "failed" : "completed";
    const event = { type: "turn", status, id };
    if (status === "failed") event.error = String(message.result || message.terminal_reason || "turn failed");
    this.#emit(event);
  }
}

function sessionRules({ tool, suggestions }) {
  const rules = (suggestions || [])
    .filter((s) => s?.type === "addRules" || s?.type === "setMode")
    .map((s) => ({ ...s, destination: "session" }));
  if (rules.length) return rules;
  return [{ type: "addRules", rules: [{ toolName: tool }], behavior: "allow", destination: "session" }];
}

function toolItem(block) {
  const field = FILE_TOOLS.get(block.name);
  if (field) {
    const path = block.input?.[field] || "";
    return { id: block.id, kind: "file", label: path || block.name, path };
  }
  if (COMMAND_TOOLS.has(block.name)) {
    return { id: block.id, kind: "command", label: block.input?.command || block.name };
  }
  const detail = toolDetail(block.input);
  return { id: block.id, kind: "call", label: detail ? `${block.name} · ${detail}` : block.name };
}
