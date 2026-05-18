// pp, 26.05.18
// Vibe-code AC pieces with claude, then publish them under YOUR handle.
//
// Whitelisted by Auth0 `sub` (not handle) on the help bridge. The agent is
// read-only on the repo + the AC publish MCP; when it publishes, the piece
// lands as @you/whatever through your own logged-in token. Drop the returned
// code into the "laer-klokken" room to share it.
//
// Sibling of aa.mjs (admin) — same chat.mjs UI + SSE bridge, different tier.

import { Chat } from "../lib/chat.mjs";
import * as chat from "./chat.mjs";

const ENDPOINT = "https://help.aesthetic.computer";

let client;
let token = null;
let allowed = false;       // sub is whitelisted for pp
let pending = false;
let abortCtrl = null;
let userHandleRef = null;
let hudRef = null;
let jumpRef = null;
let verbose = false;
let lastShare = null;      // last published code/url, for /clock hint
const queue = [];

let msgCounter = 0;
const nextId = () => `pp-${Date.now()}-${msgCounter++}`;

function setStatus(state) {
  if (!hudRef) return;
  const map = {
    loading:   ["pp", undefined],
    ok:        ["pp", "lime"],
    resumed:   ["pp", "lime"],
    forbidden: ["pp ✗", "red"],
    unauth:    ["pp ?", "yellow"],
    down:      ["pp !", "yellow"],
  };
  const [label, color] = map[state] || map.loading;
  hudRef.label(label, color);
}

// Maker theme — warm violet/lime, distinct from aa's cool slate.
const THEME = {
  background:        [18, 12, 24],
  chromeBg:          [28, 18, 36],
  lines:             [120, 90, 160, 96],
  scrollbar:         [180, 240, 120],
  messageText:       [240, 236, 248],
  messageBox:        [228, 220, 240],
  log:               [200, 255, 150],
  logHover:          [255, 240, 120],
  handle:            [210, 160, 255],
  handleHover:       [255, 240, 120],
  url:               [180, 255, 200],
  urlHover:          [255, 240, 120],
  prompt:            [220, 200, 255],
  promptContent:     [180, 255, 200],
  promptHover:       [255, 240, 120],
  promptContentHover:[255, 240, 120],
  painting:          [230, 200, 255],
  paintingHover:     [255, 240, 120],
  kidlisp:           [255, 170, 230],
  kidlispHover:      [255, 240, 120],
  timestamp:         [140, 110, 170],
  timestampHover:    [255, 240, 120],
  heart:             [255, 200, 240],
};

async function boot({ api, debug, send, hud, handle, user, authorize, jump }) {
  client = new Chat(debug, send);
  client.system.connecting = false;

  hudRef = hud;
  jumpRef = jump || api?.jump;
  setStatus("loading");
  userHandleRef = handle;

  if (debug) {
    (async () => {
      try {
        const res = await fetch("/api/version");
        if (!res.ok) return;
        const current = (await res.json()).deployed;
        while (true) {
          try {
            const r = await fetch(`/api/version?current=${current}`);
            if (!r.ok) break;
            const data = await r.json();
            if (data.changed !== false) { send?.({ type: "window:reload" }); break; }
          } catch { break; }
        }
      } catch {}
    })();
  }

  if (!user?.sub) {
    setStatus("unauth");
    pushSystem("log in first to vibe-code with pp.");
  } else {
    try {
      token = await authorize();
      if (!token) {
        setStatus("unauth");
        pushSystem("no auth0 token — refresh and retry.");
      } else {
        const probe = await fetch(`${ENDPOINT}/api/pp/whoami`, {
          headers: { Authorization: `Bearer ${token}` },
        });
        if (probe.status === 200) {
          allowed = true;
          const data = await probe.json();
          if (data.sessionId) {
            setStatus("resumed");
            pushSystem(`resuming ${data.sessionId.slice(0, 8)}… — say "continue" or /reset.`);
          } else {
            setStatus("ok");
            pushSystem(`hi ${data.handle || "@you"} — describe a piece and i'll build + publish it for you.`);
          }
        } else if (probe.status === 403) {
          setStatus("forbidden");
          allowed = false;
          token = null;
          pushSystem("not whitelisted for pp — ask @jeffrey to add your sub.");
        } else if (probe.status === 401) {
          setStatus("unauth");
          allowed = false;
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
      if (text === "/clock") return openClock();
      if (!allowed || !token) { pushSystem("not authorized."); return; }
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
      if (text === "/help" || text === "/?") {
        pushSystem("describe a piece → i build + publish it as @you. /clock opens laer-klokken to share. /reset new session · /cancel stop · /verbose tools.");
        return;
      }

      const me = userHandleRef?.() || "@you";
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
    hideChrome: true,
    presenceOverride: currentPresenceLine(),
  });
}

function currentPresenceLine() {
  const raw = userHandleRef?.();
  const h = raw ? (raw.startsWith("@") ? raw : `@${raw}`) : "@you";
  if (allowed && token) return `${h} · MAKER`;
  if (token === null && allowed === false) return `${h} · no access`;
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
    title: "PP",
    desc: "Vibe-code AC pieces with claude — published under your own handle.",
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
  return pushMessage("pp", text);
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
    await fetch(`${ENDPOINT}/api/pp/reset`, {
      method: "POST",
      headers: { Authorization: `Bearer ${token}` },
    });
    queue.length = 0;
    lastShare = null;
    pushSystem("session reset.");
  } catch (err) {
    pushSystem(`reset failed: ${err.message}`);
  }
}

function openClock() {
  if (lastShare) {
    pushSystem(`opening laer-klokken — paste ${lastShare} there to share it.`);
  } else {
    pushSystem("opening laer-klokken…");
  }
  jumpRef?.("laer-klokken");
}

// Surface a freshly published piece prominently + remember it for /clock.
const SHARE_RE =
  /(https?:\/\/(?:www\.)?(?:aesthetic\.computer|prompt\.ac)\/[^\s"')]+|@[a-z0-9_-]+\/[a-z0-9_-]+|\$[a-z0-9]{3,})/i;
function noteShare(text) {
  if (!text) return;
  const m = String(text).match(SHARE_RE);
  if (!m) return;
  const found = m[1];
  if (found === lastShare) return;
  lastShare = found;
  pushSystem(`✦ published → ${found}  ·  /clock to share in laer-klokken`);
}

function shorten(s, max = 80) {
  if (!s) return "";
  const t = String(s);
  return t.length > max ? t.slice(0, max - 1) + "…" : t;
}

function toolLabel(block) {
  let name = block.name || "?";
  const input = block.input || {};
  // Prettify AC MCP tools: mcp__aesthetic-computer__publish_kidlisp → publish kidlisp
  const mcp = name.match(/^mcp__[^_]+(?:-[^_]+)*__(.+)$/);
  if (mcp) name = mcp[1].replace(/_/g, " ");
  let arg = "";
  switch (block.name) {
    case "Read":
    case "Glob":
    case "Grep":
      arg = input.file_path || input.pattern || "";
      break;
    default:
      for (const v of Object.values(input)) {
        if (typeof v === "string") { arg = v; break; }
      }
  }
  arg = shorten(arg.replace(/\s+/g, " ").trim(), 90);
  return arg ? `⚙ ${name} ${arg}` : `⚙ ${name}`;
}

async function sendToBridge(text) {
  pending = true;
  abortCtrl = new AbortController();

  let streamMsg = null;
  let pendingText = "";
  const applyStream = () => {
    if (!streamMsg) streamMsg = pushMessage("pp", pendingText);
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
    const res = await fetch(`${ENDPOINT}/api/pp/chat`, {
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
        allowed = false;
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

        if (evt.event === "claude") {
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
                if (streamMsg && pendingText) {
                  streamMsg.text = pendingText;
                  noteShare(pendingText);
                  streamMsg = null;
                  pendingText = "";
                }
                pushSystem(toolLabel(b));
              } else if (b.type === "thinking" && verbose && b.thinking) {
                pushSystem(`… ${shorten(b.thinking.replace(/\s+/g, " "), 200)}`);
              }
            }
          } else if (ev.type === "user" && ev.message?.content) {
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
                  noteShare(body);
                  if (verbose) {
                    const preview = body.split("\n").slice(0, 3).join(" ");
                    pushSystem(`← ${shorten(preview, 180)}`);
                  }
                }
              }
            }
          } else if (ev.type === "result") {
            if (ev.result && !pendingText) {
              pendingText = ev.result;
              applyStream();
            }
            noteShare(ev.result);
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
          pushMessage("pp", `! ${evt.data.message || "error"}`);
        } else if (evt.event === "done") {
          if (streamMsg && pendingText) {
            streamMsg.text = pendingText;
            noteShare(pendingText);
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
      }
    }
  } catch (err) {
    flushBubble();
    if (err.name === "AbortError") pushSystem("cancelled.");
    else pushSystem(`error: ${err.message}`);
  } finally {
    pending = false;
    abortCtrl = null;
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
