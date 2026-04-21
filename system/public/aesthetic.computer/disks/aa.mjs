// aa, 26.04.20
// Phone-side chat with AA — your remote Claude on the macbook,
// reached via help.aesthetic.computer. @jeffrey only.

const { floor, max, min, abs } = Math;

const ENDPOINT = "https://help.aesthetic.computer";

// ───────── state ─────────
let token = null;
let userHandle = null;
let userSub = null;

let isAdmin = false;
let authChecked = false;
let authError = null;

// log entries: { role: "user"|"assistant"|"tool"|"toolResult"|"system"|"error", text }
let log = [];
let pending = false;
let pendingText = "";
let abortCtrl = null;

let input = null;

// ───────── scroll (chat.mjs convention: 0 = newest at bottom, +y = into older) ─────────
let scroll = 0;
let totalScrollHeight = 0;
let chatHeight = 0;
let layoutDirty = true;
let cachedLines = []; // [{ text, color }]
let lastScreenW = 0;

let scrollVelocity = 0;
let isFlinging = false;
let isDragging = false;
let dragStartPos = null;
const SCROLL_FRICTION = 0.92;
const SCROLL_MIN_VELOCITY = 0.5;
const BOUNCE_STIFFNESS = 0.15;
const BOUNCE_DAMPING = 0.7;
const MAX_OVERSCROLL = 60;
const DRAG_THRESHOLD = 5;

// ───────── layout constants ─────────
const PAD = 6;
const LINE_H = 12;
const HEADER_H = 18;
const INPUT_H = 64; // TextInput preview frame

const PALETTE = {
  bg:        [10, 10, 14],
  bgLight:   [248, 246, 240],
  fg:        [232, 232, 240],
  fgLight:   [22, 22, 28],
  divider:   [60, 60, 80, 120],
  user:      [110, 200, 255],
  assistant: [220, 220, 230],
  tool:      [240, 200, 90],
  toolResult:[150, 180, 110],
  system:    [140, 140, 160],
  error:     [255, 100, 100],
  pending:   [180, 180, 90],
};

function colorFor(role) {
  if (role === "user") return PALETTE.user;
  if (role === "assistant") return PALETTE.assistant;
  if (role === "tool") return PALETTE.tool;
  if (role === "toolResult") return PALETTE.toolResult;
  if (role === "error") return PALETTE.error;
  return PALETTE.system;
}

function prefixFor(role) {
  if (role === "user") return "› ";
  if (role === "tool") return "→ ";
  if (role === "toolResult") return "  ";
  if (role === "error") return "! ";
  if (role === "assistant") return "  ";
  return "· ";
}

// ───────── boot ─────────
async function boot({ api, ui, screen, cursor, hud, handle, user, params, send }) {
  cursor("native");
  hud.labelBack();

  userHandle = handle();
  userSub = user?.sub || null;

  log.push({ role: "system", text: "AA — your remote claude on the macbook." });
  layoutDirty = true;

  if (!userSub) {
    authError = "log in first";
    authChecked = true;
    return;
  }

  try {
    token = await api.authorize();
    if (!token) {
      authError = "no token";
      authChecked = true;
      return;
    }
    const probe = await fetch(`${ENDPOINT}/api/session`, {
      headers: { Authorization: `Bearer ${token}` },
    });
    if (probe.status === 200) {
      const data = await probe.json();
      isAdmin = true;
      authChecked = true;
      log.push({
        role: "system",
        text: data.sessionId
          ? `resuming session ${data.sessionId.slice(0, 8)}…`
          : "fresh session.",
      });
      layoutDirty = true;
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

  input = new ui.TextInput(
    api,
    "ask aa…",
    async (text) => {
      text = (text || "").replace(/\s+$/, "");
      if (!text) return;
      if (text === "/reset") {
        await reset();
      } else if (text === "/clear") {
        log = [{ role: "system", text: "cleared." }];
        layoutDirty = true;
      } else if (text === "/cancel" || text === "/stop") {
        if (abortCtrl) abortCtrl.abort();
      } else {
        await send(text);
      }
      input.text = "";
      input.showBlink = false;
      input.mute = true;
      send({ type: "keyboard:close" });
    },
    {
      scheme: {
        text: 255,
        background: [0, 180],
        block: 255,
        highlight: 0,
        guideline: [255, 128],
      },
      hideGutter: false,
      closeOnEmptyEnter: true,
    },
  );
}

// ───────── network ─────────
async function reset() {
  try {
    await fetch(`${ENDPOINT}/api/reset`, {
      method: "POST",
      headers: { Authorization: `Bearer ${token}` },
    });
    pushLog({ role: "system", text: "session reset." });
  } catch (err) {
    pushLog({ role: "error", text: `reset failed: ${err.message}` });
  }
}

async function send(text) {
  if (pending) {
    pushLog({ role: "system", text: "still thinking — type /cancel to stop." });
    return;
  }
  pushLog({ role: "user", text });
  pending = true;
  pendingText = "";
  scroll = 0; // jump to bottom on user turn
  scrollVelocity = 0;
  isFlinging = false;
  abortCtrl = new AbortController();

  try {
    const res = await fetch(`${ENDPOINT}/api/chat`, {
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
      pushLog({ role: "error", text: `${res.status}: ${errText.slice(0, 200)}` });
      return;
    }
    await consumeSSE(res);
  } catch (err) {
    if (err.name === "AbortError") {
      pushLog({ role: "system", text: "cancelled." });
    } else {
      pushLog({ role: "error", text: err.message });
    }
  } finally {
    pending = false;
    pendingText = "";
    abortCtrl = null;
    layoutDirty = true;
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
  }
  if (!data) return;
  let payload;
  try { payload = JSON.parse(data); } catch { return; }

  if (event === "claude") handleClaudeEvent(payload);
  else if (event === "stderr") {
    const t = (payload.text || "").trim();
    if (t && !t.startsWith("Warning:")) pushLog({ role: "system", text: t });
  } else if (event === "error") {
    pushLog({ role: "error", text: payload.message || "error" });
  } else if (event === "done") {
    if (pendingText) {
      pushLog({ role: "assistant", text: pendingText });
      pendingText = "";
    }
  }
  layoutDirty = true;
}

function handleClaudeEvent(ev) {
  if (ev.type === "assistant" && ev.message?.content) {
    for (const block of ev.message.content) {
      if (block.type === "text" && block.text) {
        pendingText += block.text;
      } else if (block.type === "tool_use") {
        const summary = `${block.name}${block.input ? " " + summarizeToolInput(block.input) : ""}`;
        pushLog({ role: "tool", text: summary });
      }
    }
  } else if (ev.type === "user" && ev.message?.content) {
    // Surface tool results from the user-role echo events
    for (const block of ev.message.content) {
      if (block.type === "tool_result") {
        const out = stringifyToolResult(block.content);
        if (out) pushLog({ role: "toolResult", text: truncate(out, 600) });
      }
    }
  } else if (ev.type === "result" && ev.result) {
    pendingText = ev.result;
  }
}

function summarizeToolInput(input) {
  if (typeof input === "string") return truncate(input, 80);
  const k = input.command || input.file_path || input.path || input.pattern || input.query || input.url;
  if (k) return truncate(String(k), 80);
  return truncate(JSON.stringify(input), 80);
}

function stringifyToolResult(content) {
  if (!content) return "";
  if (typeof content === "string") return content;
  if (Array.isArray(content)) {
    return content
      .map((c) => (typeof c === "string" ? c : c?.text || ""))
      .filter(Boolean)
      .join("\n");
  }
  return String(content);
}

function truncate(s, n) {
  s = String(s);
  return s.length > n ? s.slice(0, n - 1) + "…" : s;
}

function pushLog(entry) {
  log.push(entry);
  layoutDirty = true;
}

// ───────── layout ─────────
function rewrap(charW, innerW) {
  const maxChars = max(8, floor(innerW / charW));
  cachedLines = [];
  for (const entry of log) {
    const color = colorFor(entry.role);
    const prefix = prefixFor(entry.role);
    const lines = wrapText(prefix + entry.text, maxChars);
    for (const ln of lines) cachedLines.push({ text: ln, color });
    cachedLines.push({ text: "", color }); // gutter
  }
}

function wrapText(text, maxChars) {
  const out = [];
  for (const para of String(text).split("\n")) {
    if (!para) { out.push(""); continue; }
    let i = 0;
    while (i < para.length) {
      out.push(para.slice(i, i + maxChars));
      i += maxChars;
    }
  }
  return out;
}

// ───────── paint ─────────
function paint($) {
  const { wipe, ink, write, screen, dark, mask, unmask, needsPaint } = $;
  const w = screen.width;
  const h = screen.height;

  wipe(...(dark ? PALETTE.bg : PALETTE.bgLight));

  // Header
  const fg = dark ? PALETTE.fg : PALETTE.fgLight;
  ink(...fg).write("AA", { x: PAD, y: PAD });
  const status = pending ? "thinking…" : (isAdmin ? "ready" : (authError || "checking…"));
  const statusColor = pending ? PALETTE.pending : (isAdmin ? PALETTE.user : PALETTE.error);
  ink(...statusColor).write(status, { x: PAD + 32, y: PAD });

  if (userHandle) {
    const tag = `@${userHandle}`;
    ink(...PALETTE.system).write(tag, { x: w - tag.length * 6 - PAD, y: PAD });
  }
  ink(...PALETTE.divider).box(0, HEADER_H - 2, w, 1, "fill");

  // Scroll region bounds
  const scrollTop = HEADER_H;
  const scrollBottom = h - INPUT_H - PAD;
  chatHeight = scrollBottom - scrollTop;
  const charW = 6;
  const innerW = w - PAD * 2;

  // Re-layout when log/screen changes
  if (layoutDirty || w !== lastScreenW) {
    rewrap(charW, innerW);
    lastScreenW = w;
    layoutDirty = false;
  }

  // Append live partial assistant text (not cached — changes each frame)
  let liveLines = [];
  if (pending) {
    const tail = pendingText
      ? wrapText("  " + pendingText + "▌", floor(innerW / charW))
      : ["  …"];
    liveLines = tail.map((t) => ({
      text: t,
      color: pendingText ? PALETTE.assistant : PALETTE.pending,
    }));
  }

  const allLines = liveLines.length ? cachedLines.concat(liveLines) : cachedLines;
  totalScrollHeight = allLines.length * LINE_H;

  // Inertial scroll physics
  if (isFlinging) {
    const maxS = max(0, totalScrollHeight - chatHeight + 5);
    const outOfBounds = scroll < 0 || scroll > maxS;
    if (outOfBounds) {
      const target = scroll < 0 ? 0 : maxS;
      const displacement = scroll - target;
      scrollVelocity -= displacement * BOUNCE_STIFFNESS;
      scrollVelocity *= BOUNCE_DAMPING;
      scroll += scrollVelocity;
      if (abs(displacement) < 0.5 && abs(scrollVelocity) < SCROLL_MIN_VELOCITY) {
        scroll = target;
        scrollVelocity = 0;
        isFlinging = false;
      }
    } else {
      scrollVelocity *= SCROLL_FRICTION;
      scroll += scrollVelocity;
      if (scroll < 0 || scroll > maxS) scrollVelocity *= 0.5;
      if (abs(scrollVelocity) < SCROLL_MIN_VELOCITY) {
        scrollVelocity = 0;
        isFlinging = false;
      }
    }
    needsPaint();
  }

  // Mask scroll region so messages don't bleed into header or input
  mask({ x: 0, y: scrollTop, width: w, height: chatHeight });

  // Bottom-anchored render: bottom of last line sits at scrollBottom + scroll
  // (positive scroll → content slides down, revealing older above)
  const startY = scrollBottom - totalScrollHeight + scroll;
  for (let i = 0; i < allLines.length; i++) {
    const y = startY + i * LINE_H;
    if (y < scrollTop - LINE_H || y > scrollBottom) continue;
    const ln = allLines[i];
    if (!ln.text) continue;
    ink(...ln.color).write(ln.text, { x: PAD, y });
  }

  // Scrollbar (only if content overflows)
  const maxS = max(0, totalScrollHeight - chatHeight);
  if (maxS > 0) {
    const segH = max(8, (chatHeight / totalScrollHeight) * chatHeight);
    // scroll=0 → bar at bottom, scroll=maxS → bar at top
    const barY = scrollTop + (1 - scroll / maxS) * (chatHeight - segH);
    ink(...PALETTE.system, 140).box(w - 3, barY, 2, segH);
  }

  unmask();

  // Input frame
  if (input) {
    const frameY = h - INPUT_H;
    ink(...PALETTE.divider).box(0, frameY, w, 1, "fill");
    input.paint($, false, { x: 0, y: frameY + 1, width: w, height: INPUT_H - 1 });
  } else if (authChecked && !isAdmin) {
    const msg = authError || "not authorized";
    ink(...PALETTE.error).write(msg, { center: "x", y: h - 16, screen });
  }

  if (pending) needsPaint();
}

// ───────── act ─────────
function act({ api, event: e, screen }) {
  // Touch begin: capture for drag-vs-tap discrimination
  if (e.is("touch")) {
    dragStartPos = { x: e.x, y: e.y };
    isDragging = false;
    isFlinging = false;
    scrollVelocity = 0;
  }

  // Drag in scrollback area → scroll
  if (e.is("draw")) {
    const inInputFrame = e.y >= screen.height - INPUT_H;
    if (!inInputFrame) {
      if (dragStartPos && !isDragging) {
        const dx = abs(e.x - dragStartPos.x);
        const dy = abs(e.y - dragStartPos.y);
        if (dx > DRAG_THRESHOLD || dy > DRAG_THRESHOLD) isDragging = true;
      }
      if (isDragging) {
        // Drag down → see older (scroll +); drag up → see newer
        const dy = e.delta?.y ?? 0;
        scroll += dy;
        scrollVelocity = dy;
        boundScrollSoft();
      }
    }
  }

  // Lift: maybe start fling
  if (e.is("lift")) {
    if (isDragging && abs(scrollVelocity) > SCROLL_MIN_VELOCITY) {
      isFlinging = true;
    } else if (scroll < 0 || scroll > maxScroll()) {
      // Released into overscroll → bounce back
      isFlinging = true;
    }
    isDragging = false;
    dragStartPos = null;
  }

  // Wheel
  if (e.is("scroll")) {
    isFlinging = false;
    scroll += e.y; // positive wheel down → reveal older
    boundScroll();
  }

  // Keyboard scroll
  if (e.is("keyboard:down:pageup")) { scroll += LINE_H * 5; boundScroll(); }
  if (e.is("keyboard:down:pagedown")) { scroll -= LINE_H * 5; boundScroll(); }
  if (e.is("keyboard:down:home")) { scroll = maxScroll(); boundScroll(); }
  if (e.is("keyboard:down:end")) { scroll = 0; }

  // Reframe → re-layout
  if (e.is("reframed")) layoutDirty = true;

  if (input) input.act(api);
}

function boundScroll() {
  if (scroll < 0) scroll = 0;
  const m = maxScroll();
  if (scroll > m) scroll = m;
}

function boundScrollSoft() {
  const m = maxScroll();
  if (scroll < -MAX_OVERSCROLL) scroll = -MAX_OVERSCROLL;
  if (scroll > m + MAX_OVERSCROLL) scroll = m + MAX_OVERSCROLL;
}

function maxScroll() {
  return max(0, totalScrollHeight - chatHeight + 5);
}

function meta() {
  return {
    title: "AA",
    desc: "Talk to your macbook's claude from anywhere. @jeffrey only.",
  };
}

export { boot, paint, act, meta };
