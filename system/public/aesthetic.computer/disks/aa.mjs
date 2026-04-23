// aa, 26.04.20
// Phone-side conversation with AA — your remote Claude on the macbook,
// reached via help.aesthetic.computer. @jeffrey only.
//
// Built on chat.mjs (UI) but talks to a private SSE bridge instead of the
// chat-system server. Pattern lifted from laer-klokken.mjs.

import { Chat } from "../lib/chat.mjs";
import * as chat from "./chat.mjs";

const ENDPOINT = "https://help.aesthetic.computer";

let client;
let token = null;
let isAdmin = false;
let pending = false;
let abortCtrl = null;
let userHandleRef = null;
let hudRef = null;
let verbose = false;       // /verbose toggles rich telemetry
const queue = [];          // prompts stacked while pending

let msgCounter = 0;
const nextId = () => `aa-${Date.now()}-${msgCounter++}`;

// HUD state — visible indicator of admin / bridge status.
// Server still enforces auth on every call; this is a UX cue only.
function setStatus(state) {
  if (!hudRef) return;
  const map = {
    loading: ["aa", undefined],     // default color
    ok: ["aa", "lime"],             // admin, fresh session
    resumed: ["aa", "lime"],        // admin, resumed session
    forbidden: ["aa ✗", "red"],    // 403 not admin
    unauth: ["aa ?", "yellow"],    // 401 / no token
    down: ["aa !", "yellow"],      // bridge unreachable or odd status
  };
  const [label, color] = map[state] || map.loading;
  hudRef.label(label, color);
}

// Cool slate theme — distinct from chat's defaults so AA reads as a separate space.
const THEME = {
  background:        [12, 14, 22],
  chromeBg:          [20, 22, 32],
  lines:             [80, 90, 130, 96],
  scrollbar:         [120, 180, 240],
  messageText:       [232, 232, 240],
  messageBox:        [220, 220, 235],
  log:               [150, 200, 255],
  logHover:          [255, 240, 120],
  handle:            [120, 200, 255],
  handleHover:       [255, 240, 120],
  url:               [180, 220, 255],
  urlHover:          [255, 240, 120],
  prompt:            [200, 230, 255],
  promptContent:     [180, 220, 255],
  promptHover:       [255, 240, 120],
  promptContentHover:[255, 240, 120],
  painting:          [200, 200, 230],
  paintingHover:     [255, 240, 120],
  kidlisp:           [230, 180, 220],
  kidlispHover:      [255, 240, 120],
  timestamp:         [110, 120, 150],
  timestampHover:    [255, 240, 120],
  heart:             [220, 200, 240],
};

async function boot({ api, debug, send, hud, handle, user, authorize }) {
  client = new Chat(debug, send);
  // We feed messages in from the bridge — never connect to a chat server.
  // chat.mjs gates paint on `connecting`, so flip it false up front.
  client.system.connecting = false;

  hudRef = hud;
  setStatus("loading");
  userHandleRef = handle;

  if (!user?.sub) {
    setStatus("unauth");
    pushSystem("log in first to talk to aa.");
  } else {
    try {
      token = await authorize();
      if (!token) {
        setStatus("unauth");
        pushSystem("no auth0 token — refresh and retry.");
      } else {
        const probe = await fetch(`${ENDPOINT}/api/session`, {
          headers: { Authorization: `Bearer ${token}` },
        });
        if (probe.status === 200) {
          isAdmin = true;
          const data = await probe.json();
          if (data.sessionId) {
            setStatus("resumed");
            pushSystem(`resuming ${data.sessionId.slice(0, 8)}…`);
            loadHistory();
          } else {
            setStatus("ok");
            pushSystem("fresh session.");
          }
        } else if (probe.status === 403) {
          setStatus("forbidden");
          isAdmin = false;
          token = null;
          pushSystem("not admin — this piece is @jeffrey-only.");
        } else if (probe.status === 401) {
          setStatus("unauth");
          isAdmin = false;
          token = null;
          pushSystem("unauthorized — session expired, refresh.");
        } else {
          setStatus("down");
          pushSystem(`bridge said ${probe.status}.`);
        }
      }
    } catch (err) {
      setStatus("down");
      pushSystem(`bridge unreachable: ${err.message}`);
    }
  }

  await chat.boot(api, client.system, {
    submitHandler: async (text) => {
      if (!isAdmin || !token) { pushSystem("not authorized."); return; }
      if (text === "/reset") return reset();
      if (text === "/clear") {
        client.system.messages.length = 0;
        queue.length = 0;
        invalidate();
        return;
      }
      if (text === "/cancel" || text === "/stop") {
        if (abortCtrl) abortCtrl.abort();
        return;
      }
      if (text === "/verbose") {
        verbose = !verbose;
        pushSystem(`verbose ${verbose ? "on" : "off"}`);
        return;
      }
      if (text === "/queue") {
        pushSystem(queue.length ? `${queue.length} queued` : "queue empty");
        return;
      }
      if (text === "/pull") return manualPull();

      // Echo the prompt immediately so the transcript reflects submission order.
      const me = userHandleRef?.() || "@jeffrey";
      pushMessage(me, text);

      if (pending) {
        queue.push(text);
        pushSystem(`→ queued (${queue.length})`);
        return;
      }
      await sendToBridge(text);
    },
  });
}

function paint($) {
  chat.paint($, {
    otherChat: client.system,
    theme: THEME,
    // aa has no real chat presence — kill the news ticker + KPBJ radio.
    hideChrome: true,
    // Replace "N online" with a solo admin line.
    presenceOverride: currentPresenceLine(),
  });
}

function currentPresenceLine() {
  const raw = userHandleRef?.();
  const h = raw ? (raw.startsWith("@") ? raw : `@${raw}`) : "@jeffrey";
  if (isAdmin && token) return `${h} · ADMIN`;
  if (token === null && isAdmin === false) return `${h} · no access`;
  return `${h} · …`;
}

function act($) {
  chat.act($, client.system);
}

function sim($) {
  chat.sim($);
}

function leave() {
  if (abortCtrl) abortCtrl.abort();
}

function meta() {
  return {
    title: "AA",
    desc: "Talk to your macbook's claude from anywhere. @jeffrey only.",
  };
}

// ───────── message plumbing ─────────

function pushMessage(from, text, { sound = false } = {}) {
  const msg = { from, text, id: nextId(), sub: from };
  client.system.messages.push(msg);
  if (client.system.messages.length > 500) client.system.messages.shift();
  client.system.receiver?.(
    msg.id,
    sound ? "message" : "layout-only",
    sound ? msg : null,
    { layoutChanged: true },
  );
  return msg;
}

function pushSystem(text) {
  return pushMessage("aa", text);
}

function removeMessage(msg) {
  if (!msg) return;
  const idx = client.system.messages.indexOf(msg);
  if (idx !== -1) client.system.messages.splice(idx, 1);
  client.system.receiver?.(nextId(), "layout-only", null, { layoutChanged: true });
}

function invalidate() {
  client.system.receiver?.(nextId(), "layout-only", null, { layoutChanged: true });
}

async function reset() {
  try {
    await fetch(`${ENDPOINT}/api/reset`, {
      method: "POST",
      headers: { Authorization: `Bearer ${token}` },
    });
    queue.length = 0;
    pushSystem("session reset.");
  } catch (err) {
    pushSystem(`reset failed: ${err.message}`);
  }
}

async function manualPull() {
  try {
    const res = await fetch(`${ENDPOINT}/api/pull`, {
      method: "POST",
      headers: { Authorization: `Bearer ${token}` },
    });
    if (!res.ok) {
      pushSystem(`pull failed: ${res.status}`);
      return;
    }
    const data = await res.json();
    renderGitPull(data);
  } catch (err) {
    pushSystem(`pull failed: ${err.message}`);
  }
}

function renderGitPull(p) {
  if (!p) return;
  const ms = p.durationMs != null ? ` ${Math.max(1, Math.round(p.durationMs / 100) / 10)}s` : "";
  // "up to date" is the common case — keep it out of the transcript unless verbose.
  if (p.ok && p.summary === "up to date" && !verbose) return;
  const glyph = p.ok ? "⟲" : "⟲!";
  const head = `${glyph} pull ${p.summary}${ms}`;
  if (verbose && p.output) pushSystem(`${head}\n${p.output}`);
  else pushSystem(head);
}

function shorten(s, max = 80) {
  if (!s) return "";
  const t = String(s);
  return t.length > max ? t.slice(0, max - 1) + "…" : t;
}

function toolLabel(block) {
  const name = block.name || "?";
  const input = block.input || {};
  let arg = "";
  switch (name) {
    case "Read":
    case "Write":
    case "Edit":
    case "MultiEdit":
    case "NotebookEdit":
      arg = input.file_path || input.notebook_path || "";
      break;
    case "Bash":
      arg = input.command || "";
      break;
    case "Grep":
    case "Glob":
      arg = input.pattern || "";
      break;
    case "WebFetch":
    case "WebSearch":
      arg = input.url || input.query || "";
      break;
    case "TodoWrite":
      arg = `${input.todos?.length ?? 0} items`;
      break;
    case "Task":
      arg = input.description || input.subagent_type || "";
      break;
    default:
      // Generic fallback: first string-ish value in input
      for (const v of Object.values(input)) {
        if (typeof v === "string") { arg = v; break; }
      }
  }
  // Strip the common home prefix so paths stay readable on a phone.
  if (arg.startsWith("/Users/aesthetic/")) arg = arg.slice("/Users/aesthetic/".length);
  arg = shorten(arg.replace(/\s+/g, " ").trim(), 100);
  return arg ? `⚙ ${name} ${arg}` : `⚙ ${name}`;
}

async function loadHistory() {
  try {
    const res = await fetch(`${ENDPOINT}/api/history`, {
      headers: { Authorization: `Bearer ${token}` },
    });
    if (!res.ok) return;
    const data = await res.json();
    const events = Array.isArray(data?.events) ? data.events : [];
    let count = 0;
    for (const ev of events) count += renderClaudeEvent(ev);
    if (count) pushSystem(`loaded ${count} prior ${count === 1 ? "message" : "messages"}.`);
  } catch (err) {
    pushSystem(`history fetch failed: ${err.message}`);
  }
}

// Render one claude transcript event (used for history replay only — live
// streaming accumulates into pendingText in sendToBridge). Keeps the
// rendered history at "basic chat" level: user prompts ↔ assistant text.
// tool_use / tool_result / thinking blocks are intentionally skipped.
function renderClaudeEvent(ev) {
  if (!ev || typeof ev !== "object") return 0;
  let produced = 0;
  if (ev.type === "assistant" && ev.message?.content) {
    for (const b of ev.message.content) {
      if (b.type === "text" && b.text) {
        pushMessage("aa", b.text);
        produced++;
      }
    }
  } else if (ev.type === "user" && ev.message?.content) {
    const c = ev.message.content;
    const me = userHandleRef?.() || "@jeffrey";
    if (typeof c === "string") {
      pushMessage(me, c);
      produced++;
    } else if (Array.isArray(c)) {
      for (const b of c) {
        if (b.type === "text" && b.text) {
          pushMessage(me, b.text);
          produced++;
        }
      }
    }
  }
  return produced;
}

async function sendToBridge(text) {
  pending = true;
  abortCtrl = new AbortController();

  // Streamed reply: the aa bubble is created lazily on the first text chunk,
  // then mutated in place so the reply appears progressively. Each assistant
  // turn may emit multiple text blocks interleaved with tool_use blocks, so
  // we close the current bubble when a tool_use lands and start a fresh one
  // on the next text chunk.
  let streamMsg = null;
  let pendingText = "";
  const applyStream = () => {
    if (!streamMsg) streamMsg = pushMessage("aa", pendingText);
    else {
      streamMsg.text = pendingText;
      invalidate();
    }
  };
  const flushBubble = () => {
    if (streamMsg && !pendingText) removeMessage(streamMsg);
    streamMsg = null;
    pendingText = "";
  };

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
      const body = await res.text().catch(() => `${res.status}`);
      if (res.status === 401 || res.status === 403) {
        isAdmin = false;
        token = null;
        setStatus(res.status === 403 ? "forbidden" : "unauth");
      }
      pushSystem(`! ${res.status}: ${body.slice(0, 200)}`);
      return;
    }

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
        const evt = parseSSE(block);
        if (!evt) continue;

        if (evt.event === "git-pull") {
          renderGitPull(evt.data);
        } else if (evt.event === "claude") {
          const ev = evt.data;
          if (ev.type === "system" && ev.subtype === "init") {
            const parts = [];
            if (ev.model) parts.push(ev.model);
            if (ev.session_id) parts.push(ev.session_id.slice(0, 8));
            if (parts.length) pushSystem(`◌ ${parts.join(" · ")}`);
          } else if (ev.type === "assistant" && ev.message?.content) {
            for (const b of ev.message.content) {
              if (b.type === "text" && b.text) {
                pendingText += b.text;
                applyStream();
              } else if (b.type === "tool_use") {
                // Seal the current assistant bubble so the tool line lands
                // between text blocks rather than replacing them.
                if (streamMsg && pendingText) {
                  streamMsg.text = pendingText;
                  streamMsg = null;
                  pendingText = "";
                }
                pushSystem(toolLabel(b));
              } else if (b.type === "thinking" && verbose && b.thinking) {
                pushSystem(`… ${shorten(b.thinking.replace(/\s+/g, " "), 200)}`);
              }
            }
          } else if (ev.type === "user" && verbose && ev.message?.content) {
            const c = ev.message.content;
            const blocks = Array.isArray(c) ? c : [];
            for (const b of blocks) {
              if (b.type === "tool_result") {
                const body = typeof b.content === "string"
                  ? b.content
                  : Array.isArray(b.content)
                    ? b.content.filter((x) => x.type === "text").map((x) => x.text).join("\n")
                    : "";
                if (body) {
                  const preview = body.split("\n").slice(0, 3).join(" ");
                  pushSystem(`← ${shorten(preview, 180)}`);
                }
              }
            }
          } else if (ev.type === "result") {
            if (ev.result && !pendingText) {
              // Rare: result arrived with no prior streamed text — render it.
              pendingText = ev.result;
              applyStream();
            }
            const parts = [];
            if (ev.duration_ms != null) parts.push(`${(ev.duration_ms / 1000).toFixed(1)}s`);
            if (ev.total_cost_usd != null) parts.push(`$${Number(ev.total_cost_usd).toFixed(4)}`);
            const u = ev.usage || {};
            if (u.input_tokens) parts.push(`${u.input_tokens}in`);
            if (u.output_tokens) parts.push(`${u.output_tokens}out`);
            if (u.cache_read_input_tokens) parts.push(`${u.cache_read_input_tokens}cache`);
            if (ev.num_turns) parts.push(`${ev.num_turns}t`);
            if (parts.length) pushSystem(`✓ ${parts.join(" · ")}`);
          }
        } else if (evt.event === "error") {
          pushMessage("aa", `! ${evt.data.message || "error"}`);
        } else if (evt.event === "done") {
          if (streamMsg && pendingText) {
            streamMsg.text = pendingText;
            // Final refresh with sound cue — matches the old "message arrived" beep.
            client.system.receiver?.(
              streamMsg.id,
              "message",
              streamMsg,
              { layoutChanged: true },
            );
          } else if (streamMsg && !pendingText) {
            removeMessage(streamMsg);
          }
        }
        // stderr events ignored — bridge warnings are not user-facing
      }
    }
  } catch (err) {
    flushBubble();
    if (err.name === "AbortError") pushSystem("cancelled.");
    else pushSystem(`error: ${err.message}`);
  } finally {
    pending = false;
    abortCtrl = null;
    // Drain the next queued prompt if any. Defer a tick so the current turn's
    // "done" state renders before the next one starts streaming.
    if (queue.length > 0) {
      const next = queue.shift();
      setTimeout(() => { sendToBridge(next); }, 40);
    }
  }
}

// ───────── small helpers ─────────

function parseSSE(block) {
  let event = "message";
  let data = "";
  for (const line of block.split("\n")) {
    if (line.startsWith("event:")) event = line.slice(6).trim();
    else if (line.startsWith("data:")) data += line.slice(5).trim();
  }
  if (!data) return null;
  try { return { event, data: JSON.parse(data) }; } catch { return null; }
}

export { boot, paint, act, sim, leave, meta };
