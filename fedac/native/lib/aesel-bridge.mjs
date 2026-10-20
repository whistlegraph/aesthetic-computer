// aesel-bridge.mjs — the Claude engine bridge, for a piece.
//
// This is aesel/src/claude-server.mjs with the process taken out. Easel's
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

// What the live feed calls a tool: its name, never its arguments, in the
// desktop's words (aesel/src/turn-activity.mjs), so both surfaces say the
// same thing while the machine works.
const TOOL_LABELS = {
  Bash: "running a command", BashOutput: "checking a process", KillShell: "stopping a process",
  Read: "reading files", Grep: "searching files", Glob: "finding files", LS: "listing files",
  Write: "writing files", Edit: "editing files", MultiEdit: "editing files", NotebookEdit: "editing files",
  TodoWrite: "planning", write_piece: "writing the piece",
};
export function toolLabel(name = "") {
  const short = String(name).split("__").pop();
  return TOOL_LABELS[short] || short.replace(/[^a-zA-Z0-9_-]/g, " ").replace(/[_-]+/g, " ").trim().slice(0, 60) || "using a tool";
}

// Tokens and dollars from either usage shape: the API's snake_case or the
// CLI's per-model camelCase. Cache buckets are separate from input, so they add.
export function measuredUsage(usage = {}) {
  const n = (v) => (Number.isFinite(v) && v >= 0 ? v : 0);
  const input = n(usage.input_tokens ?? usage.inputTokens);
  const output = n(usage.output_tokens ?? usage.outputTokens);
  const cache = n(usage.cache_read_input_tokens ?? usage.cacheReadInputTokens)
    + n(usage.cache_creation_input_tokens ?? usage.cacheCreationInputTokens);
  return { tokens: input + output + cache, usd: n(usage.costUSD ?? usage.cost) };
}

export const compact = (n) => (n >= 1e6 ? `${(n / 1e6).toFixed(1)}M` : n >= 1e3 ? `${(n / 1e3).toFixed(1)}k` : `${Math.round(n)}`);
export const dollars = (usd) => `$${usd < 0.01 ? usd.toFixed(4) : usd < 1 ? usd.toFixed(3) : usd.toFixed(2)}`;

// The receipt a turn leaves on the page: `2 tools · 14s · 3.1k tokens · $0.012`.
export function turnUsageText({ status = "completed", tools = 0, ms = 0, tokens = 0, usd = 0 }) {
  const seconds = Math.max(0, Math.round(ms / 1000));
  const elapsed = seconds >= 60 ? `${Math.floor(seconds / 60)}m ${seconds % 60}s` : `${seconds}s`;
  return [
    status === "interrupted" ? "stopped" : status === "failed" ? "failed" : "",
    `${tools} tool${tools === 1 ? "" : "s"}`,
    elapsed,
    tokens ? `${compact(tokens)} tokens` : "",
    usd > 0 ? dollars(usd) : "",
  ].filter(Boolean).join(" · ");
}

// The little markdown a reply is written in, read rather than shown — the
// desktop's markdown() (aesel/src/render.mjs). Markers come out; what they
// marked keeps its meaning as a tone. Offsets are on the cleaned text.
export function markdown(source) {
  const spans = [];
  let offset = 0;
  const out = String(source ?? "").split("\n").map((line) => {
    let text = line;
    let lineTone = "";
    const heading = /^\s{0,3}(#{1,6})\s+(.*)$/.exec(text);
    if (heading) { text = heading[2]; lineTone = "heading"; }
    text = text.replace(/^(\s*)[-*+]\s+/, "$1• ");
    const inline = [];
    let cleaned = "";
    let i = 0;
    while (i < text.length) {
      const rest = text.slice(i);
      let m;
      if ((m = /^`([^`\n]+)`/.exec(rest))) { inline.push({ start: cleaned.length, end: cleaned.length + m[1].length, tone: "code" }); cleaned += m[1]; i += m[0].length; continue; }
      if ((m = /^\*\*([^*\n]+)\*\*/.exec(rest))) { inline.push({ start: cleaned.length, end: cleaned.length + m[1].length, tone: "bold" }); cleaned += m[1]; i += m[0].length; continue; }
      if ((m = /^\[([^\]\n]+)\]\((https?:[^)\s]+)\)/.exec(rest))) { inline.push({ start: cleaned.length, end: cleaned.length + m[1].length, tone: "link" }); cleaned += m[1]; i += m[0].length; continue; }
      cleaned += text[i];
      i += 1;
    }
    if (lineTone) spans.push({ start: offset, end: offset + cleaned.length, tone: lineTone });
    else for (const span of inline) spans.push({ ...span, start: span.start + offset, end: span.end + offset });
    offset += cleaned.length + 1;
    return cleaned;
  });
  return { text: out.join("\n"), spans };
}

// Wrap to `cols`, word by word, hard-breaking a word longer than the row.
// Each row keeps its offset into `text`, so spans can be laid back on it.
export function wrapRows(text, cols) {
  const rows = [];
  let at = 0;
  for (const para of String(text).split("\n")) {
    let start = at;
    let end = at;
    const limit = at + para.length;
    while (end < limit) {
      if (end - start >= cols) {
        const space = text.lastIndexOf(" ", end);
        const cut = space > start ? space : end;
        rows.push({ start, text: text.slice(start, cut) });
        start = space > start ? cut + 1 : cut;
        end = start;
        continue;
      }
      end += 1;
    }
    rows.push({ start, text: text.slice(start, limit) });
    at = limit + 1;
  }
  return rows;
}

// The session's creature, as Slab draws one beside every agent on the Mac
// (slab/menubar-swift: SigilRenderer.seed / .name, ProxCreatureFrames). Same
// hash, same generator, so a session id names the same egg on both.
const M64 = (1n << 64n) - 1n;
export function creatureSeed(id) {
  let h = 0xcbf29ce484222325n;
  // UTF-8 bytes without TextEncoder, which QuickJS does not have.
  const bytes = encodeURIComponent(String(id).trim().toLowerCase())
    .replace(/%([0-9A-F]{2})|./g, (m, hex) => String.fromCharCode(hex ? parseInt(hex, 16) : m.charCodeAt(0)));
  for (const b of Array.from(bytes, (c) => c.charCodeAt(0))) {
    h ^= BigInt(b);
    h = (h * 0x100000001b3n) & M64;
  }
  return h === 0n ? 0x9e3779b97f4a7c15n : h;
}

function splitMix(seed) {
  let state = seed & M64;
  const next = () => {
    state = (state + 0x9e3779b97f4a7c15n) & M64;
    let z = state;
    z = ((z ^ (z >> 30n)) * 0xbf58476d1ce4e5b9n) & M64;
    z = ((z ^ (z >> 27n)) * 0x94d049bb133111ebn) & M64;
    return z ^ (z >> 31n);
  };
  return { next, int: (lo, hi) => lo + Number(next() % BigInt(hi - lo + 1)) };
}

// 3–6 letters, consonant first: "gop", "miva", "koziso".
export function petName(seed) {
  const rng = splitMix(seed ^ 0x5e5a11ed000bb1e5n);
  const sets = ["bdfgklmnprstvz", "aeiou"];
  let out = "";
  for (let i = 0, n = rng.int(3, 6); i < n; i++) out += sets[i % 2][rng.int(0, sets[i % 2].length - 1)];
  return out;
}

function hsv(h, s, v) {
  const i = Math.floor(h * 6) % 6, f = h * 6 - Math.floor(h * 6);
  const p = v * (1 - s), q = v * (1 - f * s), t = v * (1 - (1 - f) * s);
  return [[v, t, p], [q, v, p], [p, v, t], [p, q, v], [t, p, v], [v, p, q]][i].map((c) => Math.round(c * 255));
}

// The egg's look from its seed, in the renderer's own units: the shell is
// `width` wide and 1.02 tall, eyes sit at y 0.10, freckles on the shell.
export function creatureLook(seed) {
  const unit = (shift) => Number((seed >> BigInt(shift)) & 255n) / 255;
  const hue = Number(seed % 10000n) / 10000;
  return {
    shell: hsv(hue, 0.20 + unit(16) * 0.14, 1),
    accent: hsv((hue + 0.08) % 1, 0.52, 0.76),
    width: 0.72 + unit(8) * 0.12,
    eyeSpread: 0.22 + unit(24) * 0.07,
    blinkAt: 0.70 + unit(32) * 0.18,
    freckles: Array.from({ length: 7 }, (_, i) => ({
      x: (i % 2 === 0 ? -1 : 1) * (0.37 + unit((i * 7) % 48) * 0.15),
      y: 0.40 + unit((i * 5 + 4) % 48) * 0.32,
      r: 0.023 + unit((i * 3 + 9) % 48) * 0.025,
    })),
  };
}

// The colour a key flashes as it is typed: notepat's note for that key
// (pieces/notepat.mjs NOTE_TO_KEY, NOTE_COLORS), the sharps — the black keys —
// at notepat's lit-sharp shade. Keys that play no note flash nothing.
const NOTE_TO_KEY = {
  c: "c", "c#": "v", d: "d", "d#": "s", e: "e", f: "f", "f#": "w", g: "g", "g#": "r", a: "a", "a#": "q", b: "b",
  "+c": "h", "+c#": "t", "+d": "i", "+d#": "y", "+e": "j", "+f": "k", "+f#": "u", "+g": "l", "+g#": "o", "+a": "m", "+a#": "p", "+b": "n",
  "++c": ";", "++c#": "'", "-a#": "z", "-b": "x",
};
const KEY_TO_NOTE = Object.fromEntries(Object.entries(NOTE_TO_KEY).map(([note, key]) => [key, note]));
const NOTE_COLORS = {
  c: [255, 30, 30], "c#": [255, 80, 0], d: [255, 150, 0], "d#": [200, 200, 0], e: [230, 220, 0],
  f: [30, 200, 30], "f#": [0, 200, 180], g: [30, 100, 255], "g#": [80, 50, 255],
  a: [140, 30, 220], "a#": [200, 30, 150], b: [200, 50, 255],
};
export function keyColor(key) {
  const note = KEY_TO_NOTE[String(key).toLowerCase()];
  if (!note) return null;
  const base = note.replace(/^[+-]+/, "");
  const k = base.includes("#") ? 0.62 : 1;
  return NOTE_COLORS[base].map((c) => Math.floor(c * k));
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

// The piece being written, compiled inside Aesel for the corner preview.
// The runtime runs one module at a time, so the preview cannot be a second
// piece; instead the source becomes a function whose locals are the piece's
// top level and whose return value is its lifecycle. Exports are dropped
// (the names stay declared); an import has nothing to resolve against here,
// so a piece with one only runs full-screen. Returns { make } or { error }.
const LIFECYCLE = ["boot", "paint", "act", "sim", "leave"];
export function pieceFactory(source) {
  if (/^\s*import[\s{*"']/m.test(source)) return { error: "imports · tab runs it" };
  const body = source
    .replace(/\bexport\s*\{[^}]*\}\s*;?/g, "")
    .replace(/\bexport\s+default\s+/g, "const __default = ")
    .replace(/\bexport\s+(?=(async\s+)?function|const|let|var|class)/g, "");
  const tail = LIFECYCLE.map((n) => `${n}: typeof ${n} === "function" ? ${n} : null`).join(", ");
  try {
    return { make: new Function(`${body}\n;return { ${tail} };`) };
  } catch (err) {
    return { error: String(err && err.message || err) };
  }
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
    "Aesel shows the piece live in a small frame in its corner and reloads it on every save; one key runs it full-screen. Small frequent edits are better than one big rewrite. Keep the piece one self-contained file with no import statements, or the corner frame cannot run it.",
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

// The child's stdin may not be a terminal: with --print, Claude Code 2.1
// exits at once ("Input must be provided either through stdin or as a prompt
// argument when using --print") when stdin is a TTY, and a PTY is one, raw or
// not. So the child runs under a shell that puts a pipe in between — cat reads
// the PTY, Claude reads cat — and waits on Claude alone, so the shell's exit
// code is Claude's. The PTY stays Claude's stdout, which is what we read.
// Everything is in one -c script because pty2.spawn passes at most 30 argv.
export function pipedCommand(bin, args) {
  const quote = (s) => `'${String(s).replace(/'/g, "'\\''")}'`;
  return ["-c", `exec 3<&0; cat <&3 | ${[bin, ...args].map(quote).join(" ")} 3<&- & wait $!`];
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
    // Thinking streams nothing the page shows; it says so, so a long silence
    // reads as the model thinking rather than the bridge hanging.
    if (event.type === "content_block_start" && event.content_block?.type === "thinking") {
      this.#emit({ type: "phase", phase: "thinking" });
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
    // The CLI closes a turn with what it spent, per model that actually ran.
    const perModel = Object.values(message.modelUsage || {});
    const spent = (perModel.length ? perModel : message.usage ? [message.usage] : []).map(measuredUsage);
    if (spent.length) {
      this.#emit({
        type: "usage",
        tokens: spent.reduce((n, u) => n + u.tokens, 0),
        usd: spent.reduce((n, u) => n + u.usd, 0) || (Number.isFinite(message.total_cost_usd) ? message.total_cost_usd : 0),
      });
    }
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
    return { id: block.id, kind: "file", name: block.name, label: path || block.name, path };
  }
  if (COMMAND_TOOLS.has(block.name)) {
    return { id: block.id, kind: "command", name: block.name, label: block.input?.command || block.name };
  }
  const detail = toolDetail(block.input);
  return { id: block.id, kind: "call", name: block.name, label: detail ? `${block.name} · ${detail}` : block.name };
}
