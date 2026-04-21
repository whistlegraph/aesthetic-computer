// aa, 26.04.20
// Phone-side chat with AA — your remote Claude on the macbook,
// reached via help.aesthetic.computer. @jeffrey only.

const { floor, min, max } = Math;

const ENDPOINT_PROD = "https://help.aesthetic.computer";
const ENDPOINT_DEV = "https://help.aesthetic.computer"; // same; bridge enforces auth
const PROMPT_PLACEHOLDER = "ask aa…";

let endpoint = ENDPOINT_PROD;
let token = null;
let userHandle = null;
let userSub = null;

let isAdmin = false;
let authChecked = false;
let authError = null;

let log = []; // [{ role: "user"|"assistant"|"tool"|"system"|"error", text }]
let pending = false; // true while a response is streaming
let pendingText = ""; // partial assistant text being streamed
let abortCtrl = null;
let scrollY = 0; // pixels scrolled (positive = scrolled up to see older)

let input = null;
let sendBtn = null;
let resetBtn = null;

const PALETTE = {
  bg:        [10, 10, 14],
  bgLight:   [248, 246, 240],
  fg:        [232, 232, 240],
  fgLight:   [22, 22, 28],
  user:      [110, 200, 255],
  assistant: [220, 220, 230],
  tool:      [240, 200, 90],
  system:    [140, 140, 160],
  error:     [255, 100, 100],
  pending:   [180, 180, 90],
};

function colors(dark, kind) {
  if (kind === "bg") return dark ? PALETTE.bg : PALETTE.bgLight;
  if (kind === "fg") return dark ? PALETTE.fg : PALETTE.fgLight;
  return PALETTE[kind];
}

async function boot({ api, ui, screen, cursor, hud, handle, user, params }) {
  cursor("native");
  hud.labelBack();

  userHandle = handle();
  userSub = user?.sub || null;

  // Allow override: aa:dev → use a local bridge if you ever run one with public CORS
  if (params[0] === "dev") endpoint = ENDPOINT_DEV;

  log.push({ role: "system", text: "AA — your remote claude on the macbook." });

  if (!userSub) {
    authChecked = true;
    authError = "log in first";
    return;
  }

  try {
    token = await api.authorize();
    if (!token) {
      authError = "no token";
      authChecked = true;
      return;
    }
    // The bridge will reject if sub doesn't match ADMIN_SUB.
    // We probe /api/session to confirm before showing the input.
    const probe = await fetch(`${endpoint}/api/session`, {
      headers: { Authorization: `Bearer ${token}` },
    });
    if (probe.status === 200) {
      const data = await probe.json();
      isAdmin = true;
      authChecked = true;
      if (data.sessionId) {
        log.push({ role: "system", text: `resuming session ${data.sessionId.slice(0, 8)}…` });
      } else {
        log.push({ role: "system", text: "fresh session." });
      }
    } else if (probe.status === 403) {
      authError = "not admin (this piece is @jeffrey-only)";
      authChecked = true;
    } else {
      authError = `bridge said ${probe.status}`;
      authChecked = true;
    }
  } catch (err) {
    authError = `bridge unreachable: ${err.message}`;
    authChecked = true;
  }

  if (!isAdmin) return;

  input = new ui.TextInput(api, "", async (text) => {
    text = (text || "").trim();
    if (!text) return;
    if (text === "/reset") {
      await reset();
      input.text = "";
      return;
    }
    if (text === "/clear") {
      log = [{ role: "system", text: "cleared." }];
      input.text = "";
      return;
    }
    if (text === "/cancel" || text === "/stop") {
      if (abortCtrl) abortCtrl.abort();
      input.text = "";
      return;
    }
    await send(text);
    input.text = "";
  });
}

async function reset() {
  try {
    await fetch(`${endpoint}/api/reset`, {
      method: "POST",
      headers: { Authorization: `Bearer ${token}` },
    });
    log.push({ role: "system", text: "session reset." });
  } catch (err) {
    log.push({ role: "error", text: `reset failed: ${err.message}` });
  }
}

async function send(text) {
  if (pending) {
    log.push({ role: "system", text: "still thinking — type /cancel to stop." });
    return;
  }
  log.push({ role: "user", text });
  pending = true;
  pendingText = "";
  scrollY = 0; // jump to bottom on new turn
  abortCtrl = new AbortController();

  try {
    const res = await fetch(`${endpoint}/api/chat`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${token}`,
      },
      body: JSON.stringify({ message: text }),
      signal: abortCtrl.signal,
    });
    if (!res.ok) {
      const errText = await res.text().catch(() => `${res.status}`);
      log.push({ role: "error", text: `${res.status}: ${errText.slice(0, 200)}` });
      return;
    }
    await consumeSSE(res);
  } catch (err) {
    if (err.name === "AbortError") {
      log.push({ role: "system", text: "cancelled." });
    } else {
      log.push({ role: "error", text: err.message });
    }
  } finally {
    pending = false;
    pendingText = "";
    abortCtrl = null;
  }
}

async function consumeSSE(res) {
  const reader = res.body.getReader();
  const decoder = new TextDecoder();
  let buf = "";

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    buf += decoder.decode(value, { stream: true });
    let idx;
    while ((idx = buf.indexOf("\n\n")) !== -1) {
      const block = buf.slice(0, idx);
      buf = buf.slice(idx + 2);
      handleSSEBlock(block);
    }
  }
}

function handleSSEBlock(block) {
  let event = "message";
  let data = "";
  for (const line of block.split("\n")) {
    if (line.startsWith("event:")) event = line.slice(6).trim();
    else if (line.startsWith("data:")) data += line.slice(5).trim();
    else if (line.startsWith(":")) continue; // comment / heartbeat
  }
  if (!data) return;
  let payload;
  try { payload = JSON.parse(data); } catch { return; }

  if (event === "claude") handleClaudeEvent(payload);
  else if (event === "stderr") {
    // Surface only non-empty, non-warning stderr (claude prints noisy warnings)
    const t = (payload.text || "").trim();
    if (t && !t.startsWith("Warning:")) log.push({ role: "system", text: t });
  } else if (event === "error") {
    log.push({ role: "error", text: payload.message || "error" });
  } else if (event === "done") {
    if (pendingText) {
      log.push({ role: "assistant", text: pendingText });
      pendingText = "";
    }
  }
}

function handleClaudeEvent(ev) {
  // stream-json from `claude --print --output-format stream-json`
  if (ev.type === "assistant" && ev.message?.content) {
    for (const block of ev.message.content) {
      if (block.type === "text" && block.text) {
        // Accumulate; commit as one message on `done` to avoid duplicates
        pendingText += block.text;
      } else if (block.type === "tool_use") {
        const summary = `${block.name}${block.input ? " " + summarizeToolInput(block.input) : ""}`;
        log.push({ role: "tool", text: `→ ${summary}` });
      }
    }
  } else if (ev.type === "result") {
    // Final result event has the canonical answer; replace any partial.
    if (ev.result) {
      pendingText = ev.result;
    }
  }
}

function summarizeToolInput(input) {
  if (typeof input === "string") return input.slice(0, 80);
  // Pull the most likely useful field
  const k = input.command || input.file_path || input.path || input.pattern || input.query || input.url;
  if (k) return String(k).slice(0, 80);
  return JSON.stringify(input).slice(0, 80);
}

// ───────── paint ─────────

const PAD = 6;
const LINE_H = 12;
const HEADER_H = 18;
const INPUT_H = 64; // TextInput needs room for its prompt + gutter buttons

function wrap(text, charW, maxW) {
  const maxChars = max(8, floor(maxW / charW));
  const lines = [];
  for (const para of String(text).split("\n")) {
    if (!para) { lines.push(""); continue; }
    let i = 0;
    while (i < para.length) {
      lines.push(para.slice(i, i + maxChars));
      i += maxChars;
    }
  }
  return lines;
}

function paint($) {
  const { wipe, ink, write, screen, dark } = $;
  const w = screen.width;
  const h = screen.height;

  wipe(...colors(dark, "bg"));

  // Header
  ink(...colors(dark, "fg")).write("AA", { x: PAD, y: PAD });
  const status = pending ? "thinking…" : (isAdmin ? "ready" : (authError || "checking…"));
  const statusColor = pending ? PALETTE.pending : (isAdmin ? PALETTE.user : PALETTE.error);
  ink(...statusColor).write(status, { x: PAD + 32, y: PAD });

  if (userHandle) {
    const tag = `@${userHandle}`;
    ink(...PALETTE.system).write(tag, { x: w - tag.length * 6 - PAD, y: PAD });
  }

  // Divider
  ink(...PALETTE.system, 100).box(0, HEADER_H - 2, w, 1, "fill");

  // Scrollback area bounds
  const scrollTop = HEADER_H;
  const scrollBottom = h - INPUT_H - PAD;
  const scrollH = scrollBottom - scrollTop;
  const charW = 6;
  const innerW = w - PAD * 2;

  // Build flat list of {color, text} lines, in display order (top→bottom)
  const lines = [];
  for (const entry of log) {
    const color = colors(dark, "fg");
    let prefix = "";
    let textColor;
    if (entry.role === "user")        { prefix = "› "; textColor = PALETTE.user; }
    else if (entry.role === "assistant") { prefix = "  "; textColor = PALETTE.assistant; }
    else if (entry.role === "tool")   { prefix = "  "; textColor = PALETTE.tool; }
    else if (entry.role === "error")  { prefix = "! "; textColor = PALETTE.error; }
    else                              { prefix = "· "; textColor = PALETTE.system; }
    const wrapped = wrap(prefix + entry.text, charW, innerW);
    for (const ln of wrapped) lines.push({ text: ln, color: textColor });
    lines.push({ text: "", color: textColor }); // blank between entries
  }

  // Live partial assistant text
  if (pending && pendingText) {
    const wrapped = wrap("  " + pendingText + "▌", charW, innerW);
    for (const ln of wrapped) lines.push({ text: ln, color: PALETTE.assistant });
  } else if (pending && !pendingText) {
    lines.push({ text: "  …", color: PALETTE.pending });
  }

  // Render bottom-aligned with scroll offset
  const totalH = lines.length * LINE_H;
  const maxScroll = max(0, totalH - scrollH);
  scrollY = min(scrollY, maxScroll);
  const startY = scrollBottom - totalH + scrollY;

  for (let i = 0; i < lines.length; i++) {
    const y = startY + i * LINE_H;
    if (y < scrollTop - LINE_H || y > scrollBottom) continue;
    const ln = lines[i];
    if (!ln.text) continue;
    ink(...ln.color).write(ln.text, { x: PAD, y });
  }

  // Mask area below input zone
  ink(...colors(dark, "bg")).box(0, scrollBottom + 1, w, h - scrollBottom);

  // Input
  if (input) {
    const frameY = h - INPUT_H;
    ink(...PALETTE.system, 80).box(0, frameY, w, 1, "fill");
    input.paint($, false, { x: 0, y: frameY + 1, width: w, height: INPUT_H - 1 });
  } else if (authChecked && !isAdmin) {
    const msg = authError || "not authorized";
    ink(...PALETTE.error).write(msg, { center: "x", y: h - 16, screen });
  }

  if (pending) $.needsPaint();
}

function act({ api, event: e, screen }) {
  // Scroll the log when dragging in the scrollback area (not the input frame)
  if (e.is("draw") && e.dy) {
    const inInputFrame = e.y && e.y >= screen.height - INPUT_H;
    if (!inInputFrame) scrollY = max(0, scrollY + e.dy);
  }
  if (e.is("keyboard:down:pageup")) scrollY += LINE_H * 5;
  if (e.is("keyboard:down:pagedown")) scrollY = max(0, scrollY - LINE_H * 5);

  if (input) input.act(api);
}

function meta() {
  return {
    title: "AA",
    desc: "Talk to your macbook's claude from anywhere. @jeffrey only.",
  };
}

export { boot, paint, act, meta };
