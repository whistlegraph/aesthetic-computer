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
        invalidate();
        return;
      }
      if (text === "/cancel" || text === "/stop") {
        if (abortCtrl) abortCtrl.abort();
        return;
      }
      await sendToBridge(text);
    },
  });
}

function paint($) {
  chat.paint($, { otherChat: client.system, theme: THEME });
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
  // chat.mjs's receiver hook sets `messagesNeedLayout = true` whenever
  // extra.layoutChanged is set; type === "message" also plays the SFX.
  client.system.receiver?.(
    msg.id,
    sound ? "message" : "layout-only",
    sound ? msg : null,
    { layoutChanged: true },
  );
  return msg;
}

function pushSystem(text) {
  return pushMessage("log", text);
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
    pushSystem("session reset.");
  } catch (err) {
    pushSystem(`reset failed: ${err.message}`);
  }
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
    for (const ev of events) count += renderClaudeEvent(ev, { fromHistory: true });
    if (count) pushSystem(`loaded ${count} prior ${count === 1 ? "message" : "messages"}.`);
  } catch (err) {
    pushSystem(`history fetch failed: ${err.message}`);
  }
}

// Render one claude transcript event (from live stream or saved history)
// as zero or more chat messages. Returns the number of messages produced.
function renderClaudeEvent(ev, { fromHistory = false } = {}) {
  if (!ev || typeof ev !== "object") return 0;
  let produced = 0;
  if (ev.type === "assistant" && ev.message?.content) {
    for (const b of ev.message.content) {
      if (b.type === "text" && b.text) {
        pushMessage("@aa", b.text, { sound: !fromHistory });
        produced++;
      } else if (b.type === "tool_use") {
        pushMessage("log", `→ ${b.name} ${summarizeInput(b.input)}`);
        produced++;
      }
    }
  } else if (ev.type === "user" && ev.message?.content) {
    const c = ev.message.content;
    // History format: user content is usually a plain string (the prompt).
    if (typeof c === "string") {
      const me = userHandleRef?.() || "@jeffrey";
      pushMessage(me, c);
      produced++;
    } else if (Array.isArray(c)) {
      for (const b of c) {
        if (b.type === "tool_result") {
          const t = stringifyResult(b.content);
          if (t) { pushMessage("log", truncate(t, 600)); produced++; }
        } else if (b.type === "text" && b.text) {
          const me = userHandleRef?.() || "@jeffrey";
          pushMessage(me, b.text);
          produced++;
        }
      }
    }
  }
  return produced;
}

async function sendToBridge(text) {
  if (pending) { pushSystem("still thinking — type /cancel to stop."); return; }

  const me = userHandleRef?.() || "@jeffrey";
  pushMessage(me, text);
  pending = true;
  abortCtrl = new AbortController();

  let pendingText = "";

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

        if (evt.event === "claude") {
          const ev = evt.data;
          if (ev.type === "assistant" && ev.message?.content) {
            for (const b of ev.message.content) {
              if (b.type === "text" && b.text) {
                pendingText += b.text;
              } else if (b.type === "tool_use") {
                pushMessage("log", `→ ${b.name} ${summarizeInput(b.input)}`);
              }
            }
          } else if (ev.type === "user" && ev.message?.content) {
            for (const b of ev.message.content) {
              if (b.type === "tool_result") {
                const t = stringifyResult(b.content);
                if (t) pushMessage("log", truncate(t, 600));
              }
            }
          } else if (ev.type === "result" && ev.result) {
            pendingText = ev.result;
          }
        } else if (evt.event === "stderr") {
          const t = (evt.data.text || "").trim();
          if (t && !t.startsWith("Warning:")) pushMessage("log", t);
        } else if (evt.event === "error") {
          pushMessage("log", `! ${evt.data.message || "error"}`);
        } else if (evt.event === "done") {
          if (pendingText) {
            pushMessage("@aa", pendingText, { sound: true });
            pendingText = "";
          }
        }
      }
    }
  } catch (err) {
    if (err.name === "AbortError") pushSystem("cancelled.");
    else pushSystem(`error: ${err.message}`);
  } finally {
    pending = false;
    abortCtrl = null;
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

function summarizeInput(input) {
  if (!input) return "";
  if (typeof input === "string") return truncate(input, 80);
  const k = input.command || input.file_path || input.path || input.pattern || input.query || input.url;
  if (k) return truncate(String(k), 80);
  return truncate(JSON.stringify(input), 80);
}

function stringifyResult(content) {
  if (!content) return "";
  if (typeof content === "string") return content;
  if (Array.isArray(content)) {
    return content.map((c) => (typeof c === "string" ? c : c?.text || "")).filter(Boolean).join("\n");
  }
  return String(content);
}

function truncate(s, n) {
  s = String(s);
  return s.length > n ? s.slice(0, n - 1) + "…" : s;
}

export { boot, paint, act, sim, leave, meta };
