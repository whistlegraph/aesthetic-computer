// help, 26.04.21
// Public, sandboxed chatbot about aesthetic.computer. Rate-limited, stateless,
// backed by the same macbook bridge as aa but through /api/help/chat with a
// locked-down claude (haiku, read-only tools, path deny-list on secrets).
// Requires an Auth0 login AND a handle — small barrier to keep costs bounded.

import { Chat } from "../lib/chat.mjs";
import * as chat from "./chat.mjs";

const ENDPOINT = "https://help.aesthetic.computer";

let client;
let token = null;
let handleOk = false; // token + handle set → eligible to talk
let pending = false;
let abortCtrl = null;
let userHandleRef = null;
let hudRef = null;
let ratesLeft = null; // { hour, day } from last response, null until known
let currentState = "loading";

let msgCounter = 0;
const nextId = () => `help-${Date.now()}-${msgCounter++}`;

// HUD status — mirrors eligibility + most recent rate remaining.
// Server re-validates on every request; this is a UX cue only.
function setStatus(state) {
  currentState = state;
  if (!hudRef) return;
  const base = {
    loading:   ["help",    undefined],
    ok:        ["help",    "lime"],    // logged in + handle, ready
    busy:      ["help …",  "orange"],  // in-flight or over capacity
    unauth:    ["help ?",  "yellow"],  // 401 / no token
    nohandle:  ["help !@", "yellow"],  // logged in but handle unset
    ratelimit: ["help ⛔", "orange"],  // 429
    down:      ["help !",  "yellow"],  // bridge unreachable / odd status
  };
  let [label, color] = base[state] || base.loading;
  if (state === "ok" && ratesLeft) {
    label = `help ${ratesLeft.hour}·${ratesLeft.day}`;
  }
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
    pushSystem("log in to ask help anything.");
  } else {
    try {
      token = await authorize();
      if (!token) {
        setStatus("unauth");
        pushSystem("no auth0 token — refresh and retry.");
      } else if (!handle?.()) {
        // Token is good but no handle set. The server will reject with 403
        // anyway, but we catch it up-front for a clearer nudge.
        setStatus("nohandle");
        pushSystem("set a handle first — type `handle @yourname`.");
      } else {
        handleOk = true;
        setStatus("ok");
        pushSystem("ask about pieces, commands, kidlisp, or the codebase.");
      }
    } catch (err) {
      setStatus("down");
      pushSystem(`bridge unreachable: ${err.message}`);
    }
  }

  await chat.boot(api, client.system, {
    submitHandler: async (text) => {
      if (!handleOk || !token) { pushSystem("not eligible to ask yet."); return; }
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
  chat.paint($, {
    otherChat: client.system,
    theme: THEME,
    // help has no real chat presence — kill the news ticker + KPBJ radio.
    hideChrome: true,
    presenceOverride: currentPresenceLine(),
  });
}

function currentPresenceLine() {
  const raw = userHandleRef?.();
  const h = raw ? (raw.startsWith("@") ? raw : `@${raw}`) : null;
  if (handleOk && h) {
    if (ratesLeft) return `${h} · ${ratesLeft.hour} left this hour`;
    return `${h} · help is open`;
  }
  if (currentState === "nohandle") return "set a handle to ask";
  if (!token) return "log in to ask";
  return "…";
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
    title: "help",
    desc: "Public, rate-limited chatbot that answers questions about aesthetic.computer.",
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
  return pushMessage("help", text);
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

async function sendToBridge(text) {
  if (pending) { pushSystem("one at a time — /cancel to stop."); return; }

  const raw = userHandleRef?.();
  const me = raw ? (raw.startsWith("@") ? raw : `@${raw}`) : "anon";
  pushMessage(me, text);
  pending = true;
  abortCtrl = new AbortController();

  // Streamed reply: the help bubble is created lazily on the first text chunk,
  // then mutated in place so the reply appears progressively.
  let streamMsg = null;
  let pendingText = "";
  const applyStream = () => {
    if (!streamMsg) streamMsg = pushMessage("help", pendingText);
    else {
      streamMsg.text = pendingText;
      invalidate();
    }
  };

  try {
    const res = await fetch(`${ENDPOINT}/api/help/chat`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${token}`,
      },
      body: JSON.stringify({ message: text }),
      signal: abortCtrl.signal,
    });

    // Pull remaining-quota hints out of response headers (set on all success
    // and some error paths) so the HUD can show "help 18·47" etc.
    const hH = res.headers.get("X-Help-Remaining-Hour");
    const hD = res.headers.get("X-Help-Remaining-Day");
    if (hH !== null && hD !== null) {
      ratesLeft = { hour: parseInt(hH, 10), day: parseInt(hD, 10) };
      if (handleOk) setStatus("ok");
    }

    if (!res.ok) {
      const body = await res.text().catch(() => `${res.status}`);
      if (res.status === 401) {
        handleOk = false;
        token = null;
        setStatus("unauth");
      } else if (res.status === 403) {
        handleOk = false;
        setStatus("nohandle");
      } else if (res.status === 429) {
        setStatus("ratelimit");
      } else if (res.status === 503) {
        setStatus("busy");
      } else {
        setStatus("down");
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

        if (evt.event === "claude") {
          const ev = evt.data;
          if (ev.type === "assistant" && ev.message?.content) {
            for (const b of ev.message.content) {
              if (b.type === "text" && b.text) {
                pendingText += b.text;
                applyStream();
              }
              // tool_use / thinking / other blocks deliberately suppressed
            }
          } else if (ev.type === "result" && ev.result) {
            pendingText = ev.result;
            applyStream();
          }
          // tool_result (in user events) also suppressed
        } else if (evt.event === "error") {
          pushMessage("help", `! ${evt.data.message || "error"}`);
        } else if (evt.event === "done") {
          if (streamMsg && pendingText) {
            streamMsg.text = pendingText;
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
    if (streamMsg && !pendingText) removeMessage(streamMsg);
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

export { boot, paint, act, sim, leave, meta };
